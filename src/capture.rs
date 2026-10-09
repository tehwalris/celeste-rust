//! DIAGNOSTIC (`CELESTE_EMIT_CAPTURE=DIR`): one frame's kernel emissions and
//! the visited set at its start, for a standalone microbenchmark of the
//! dedup / frontier / edge pipeline. Not for merge.
//!
//! `DIR/w{n}.bin`: per worker thread, 48-byte little-endian records
//! `(src u64, key0 u64, key1 u64, shape u64, cell u32, xfer u32, flags u32, pad u32)`
//! in emission order; flags 1 = dropped by level -1 (key 0), 2 = a unit
//! starts (src = the unit's index, key0 = its block, key1 = its lanes).
//! `DIR/door.bin`: 40-byte records `(shape u64, cell u32, pad u32, key0, key1, id u64)`.

use std::cell::RefCell;
use std::io::Write;
use std::sync::atomic::{AtomicU32, Ordering};

static NEXT: AtomicU32 = AtomicU32::new(0);

thread_local! {
    static OUT: RefCell<Option<std::io::BufWriter<std::fs::File>>> = const { RefCell::new(None) };
    /// This thread's file number (`w{n}.bin`), once it has written.
    static FILE_N: std::cell::Cell<Option<u32>> = const { std::cell::Cell::new(None) };
}

pub fn dir() -> Option<&'static str> {
    static D: std::sync::OnceLock<Option<String>> = std::sync::OnceLock::new();
    D.get_or_init(|| std::env::var("CELESTE_EMIT_CAPTURE").ok()).as_deref()
}

#[allow(clippy::too_many_arguments)]
pub fn record(src: u64, key: (u64, u64), shape: u64, cell: u32, xfer: u32, flags: u32) {
    let Some(d) = dir() else { return };
    OUT.with(|o| {
        let mut o = o.borrow_mut();
        let w = o.get_or_insert_with(|| {
            let n = NEXT.fetch_add(1, Ordering::Relaxed);
            FILE_N.with(|f| f.set(Some(n)));
            std::fs::create_dir_all(d).expect("capture dir");
            std::io::BufWriter::with_capacity(8 << 20, std::fs::File::create(format!("{d}/w{n:03}.bin")).expect("capture file"))
        });
        let mut b = [0u8; 48];
        b[0..8].copy_from_slice(&src.to_le_bytes());
        b[8..16].copy_from_slice(&key.0.to_le_bytes());
        b[16..24].copy_from_slice(&key.1.to_le_bytes());
        b[24..32].copy_from_slice(&shape.to_le_bytes());
        b[32..36].copy_from_slice(&cell.to_le_bytes());
        b[36..40].copy_from_slice(&xfer.to_le_bytes());
        b[40..44].copy_from_slice(&flags.to_le_bytes());
        w.write_all(&b).expect("capture write");
    });
}

/// Flush this thread's writer (the end of a worker's wave).
pub fn flush() {
    OUT.with(|o| {
        if let Some(w) = o.borrow_mut().as_mut() {
            w.flush().expect("capture flush");
        }
    });
}

/// This worker's transfer table (`ForwardSink::xfer_tab`, ids are its
/// indexes): `DIR/x{n}.bin`, n = the thread's `w{n}.bin`, `encode_pair` each.
pub fn dump_xfers(tab: &[crate::search::arc_edges::Pair]) {
    let Some(d) = dir() else { return };
    let Some(n) = FILE_N.with(|f| f.get()) else { return };
    let mut buf = Vec::with_capacity(tab.len() * crate::search::arc_edges::PAIR_BYTES);
    for p in tab {
        crate::search::arc_edges::encode_pair(&mut buf, p);
    }
    std::fs::write(format!("{d}/x{n:03}.bin"), buf).expect("capture xfer table");
}
