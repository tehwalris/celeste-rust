use std::{fs, path::Path};

use anyhow::Result;

use crate::pico8_num::Pico8Num;

pub struct CartData {
    map_data: Vec<u8>,
    flag_data: Vec<u8>,
}

impl CartData {
    pub fn load<P>(base_path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let base_path = base_path.as_ref();
        Ok(CartData {
            map_data: Self::load_vec(&base_path.join("map-data.txt"), 8192)?,
            flag_data: Self::load_vec(&base_path.join("flag-data.txt"), 256)?,
        })
    }

    fn load_vec(path: &Path, expected_len: usize) -> Result<Vec<u8>> {
        let mut raw = fs::read_to_string(path)?;
        raw.retain(|c| !c.is_whitespace());
        let decoded = hex::decode(raw)?;
        if decoded.len() == expected_len {
            Ok(decoded)
        } else {
            Err(anyhow!(
                "Wrong length: got {}, expected {}",
                decoded.len(),
                expected_len
            ))
        }
    }

    pub fn mget(&self, x: Pico8Num, y: Pico8Num) -> Result<u8> {
        let x = Self::as_usize_below(x, "x", 128)?;
        let y = Self::as_usize_below(y, "y", 64)?;
        Ok(self.map_data[x + (y * 128)])
    }

    pub fn fget(&self, i: Pico8Num, b: Pico8Num) -> Result<bool> {
        let i = Self::as_usize_below(i, "i", self.flag_data.len())?;
        let b = Self::as_usize_below(b, "b", 8)?;
        Ok(self.flag_data[i] & (1 << b) != 0)
    }

    fn as_usize_below(v: Pico8Num, name: &str, high_exclusive: usize) -> Result<usize> {
        let v = v.as_i16().ok_or(anyhow!("{} is not an integer", name))?;
        if v >= 0 && (v as usize) < high_exclusive {
            Ok(v as usize)
        } else {
            Err(anyhow!("{} is out of range", name))
        }
    }
}
