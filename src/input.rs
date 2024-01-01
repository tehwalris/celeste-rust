use anyhow::Result;

#[derive(Debug, PartialEq, Eq)]
pub struct InputFlags {
    pub left: bool,
    pub right: bool,
    pub up: bool,
    pub down: bool,
    pub jump: bool,
    pub dash: bool,
}

impl InputFlags {
    pub fn none() -> InputFlags {
        InputFlags {
            left: false,
            right: false,
            up: false,
            down: false,
            jump: false,
            dash: false,
        }
    }

    pub fn from_tas_keycode(c: usize) -> Result<InputFlags> {
        if c < (1 << 6) {
            let bit = |i: usize| c & (1 << i) != 0;
            let result = InputFlags {
                left: bit(0),
                right: bit(1),
                up: bit(2),
                down: bit(3),
                jump: bit(4),
                dash: bit(5),
            };
            assert!(result.to_tas_keycode() == c);
            Ok(result)
        } else {
            Err(anyhow!("invalid keycode"))
        }
    }

    pub fn to_tas_keycode(&self) -> usize {
        let bit = |i: usize, v: bool| if v { 1 << i } else { 0 };
        bit(0, self.left)
            | bit(1, self.right)
            | bit(2, self.up)
            | bit(3, self.down)
            | bit(4, self.jump)
            | bit(5, self.dash)
    }

    pub fn iter() -> impl Iterator<Item = InputFlags> {
        (0..(1 << 6)).map(|c| Self::from_tas_keycode(c).unwrap())
    }
}
