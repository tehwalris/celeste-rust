//! A frame's SPOOL: the compressed blocks (out blocks, edge segments) the
//! wave makes long before the frame's end writes them, in a scratch file -
//! page cache the kernel can reclaim, not anonymous memory. Unlinked once
//! created: nothing is left behind, whatever ends the process.

use anyhow::{Context, Result};
use std::os::unix::fs::FileExt;
use std::sync::atomic::{AtomicU64, Ordering};

/// The scratch file, appended by any thread (each block at its own offset).
pub(crate) struct Spool {
    file: std::fs::File,
    end: AtomicU64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Spooled {
    at: u64,
    len: u32,
}

impl Spool {
    /// A spool at `path` (created, then unlinked).
    pub(crate) fn create(path: &std::path::Path) -> Result<Self> {
        let file = std::fs::File::options().read(true).write(true).create(true).truncate(true).open(path).with_context(|| path.display().to_string())?;
        std::fs::remove_file(path).with_context(|| path.display().to_string())?;
        Ok(Spool { file, end: AtomicU64::new(0) })
    }

    /// Append `b`; its place.
    pub(crate) fn put(&self, b: &[u8]) -> Result<Spooled> {
        let len = u32::try_from(b.len()).context("a spooled block past 4 GB")?;
        let at = self.end.fetch_add(b.len() as u64, Ordering::Relaxed);
        self.file.write_all_at(b, at).context("writing the frame's spool")?;
        Ok(Spooled { at, len })
    }

    pub(crate) fn read(&self, s: Spooled, buf: &mut Vec<u8>) -> Result<()> {
        buf.resize(s.len as usize, 0);
        self.file.read_exact_at(buf, s.at).context("reading the frame's spool")
    }

    #[cfg(test)]
    pub(crate) fn bytes(&self) -> u64 {
        self.end.load(Ordering::Relaxed)
    }
}

/// A block's compressed bytes: in memory, or spooled.
#[derive(Debug)]
pub(crate) enum Body {
    Mem(Vec<u8>),
    Spool(Spooled),
}

impl Body {
    pub(crate) fn len(&self) -> usize {
        match self {
            Body::Mem(v) => v.len(),
            Body::Spool(s) => s.len as usize,
        }
    }
    /// Into `spool`, if there is one.
    pub(crate) fn spill(self, spool: Option<&Spool>) -> Result<Body> {
        match (self, spool) {
            (Body::Mem(v), Some(sp)) => Ok(Body::Spool(sp.put(&v)?)),
            (b, _) => Ok(b),
        }
    }
    /// Its bytes: `f` on them (a spooled block read into `buf` first).
    pub(crate) fn with<R>(&self, spool: Option<&Spool>, buf: &mut Vec<u8>, f: impl FnOnce(&[u8]) -> R) -> Result<R> {
        match self {
            Body::Mem(v) => Ok(f(v)),
            Body::Spool(s) => {
                spool.context("a spooled block without its spool")?.read(*s, buf)?;
                Ok(f(buf))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spooled_blocks_read_back_from_threads() {
        let path = std::env::temp_dir().join(format!("spool-test-{}", std::process::id()));
        let sp = Spool::create(&path).unwrap();
        assert!(!path.exists(), "the spool is unlinked");
        let blocks: Vec<Vec<u8>> = (0..64u32).map(|k| (0..k * 37 + 1).map(|i| (i ^ k) as u8).collect()).collect();
        let bodies: Vec<Body> = crate::par::Pool::new(8).map(blocks.len(), |k| Body::Mem(blocks[k].clone()).spill(Some(&sp))).unwrap();
        assert_eq!(sp.bytes(), blocks.iter().map(|b| b.len() as u64).sum::<u64>());
        let mut buf = Vec::new();
        for (b, body) in blocks.iter().zip(&bodies) {
            assert_eq!(body.len(), b.len());
            assert!(body.with(Some(&sp), &mut buf, |x| x == &b[..]).unwrap());
        }
        assert!(Body::Spool(Spooled { at: 0, len: 1 }).with(None, &mut buf, |_| ()).is_err(), "no spool, no bytes");
    }
}
