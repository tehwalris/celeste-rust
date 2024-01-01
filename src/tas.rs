use crate::input::InputFlags;
use anyhow::Result;

struct CharParser<'a> {
    s: &'a [u8],
    i: usize,
}

impl<'a> CharParser<'a> {
    fn new(s: &'a str) -> Self {
        CharParser {
            s: s.as_bytes(),
            i: 0,
        }
    }

    fn try_take_char<F>(&mut self, is_match: F) -> Option<char>
    where
        F: Fn(char) -> bool,
    {
        if let Some(c) = self.s.get(self.i) {
            let c = *c as char;
            if is_match(c) {
                self.i += 1;
                return Some(c);
            }
        }
        None
    }

    fn done(&self) -> bool {
        self.i == self.s.len()
    }
}

pub fn parse_tas_string(s: &str) -> Result<Vec<InputFlags>> {
    let mut p = CharParser::new(s);

    p.try_take_char(|c| c == '[').ok_or(anyhow!("expected ["))?;
    p.try_take_char(|c| c == ']').ok_or(anyhow!("expected ]"))?;

    let mut inputs = Vec::new();
    loop {
        let mut digits = Vec::new();
        while let Some(digit) = p.try_take_char(|c| c.is_ascii_digit()) {
            digits.push(digit);
        }
        if digits.is_empty() {
            break;
        }

        p.try_take_char(|c| c == ',').ok_or(anyhow!("expected ,"))?;

        let keycode = digits.into_iter().collect::<String>().parse()?;
        inputs.push(InputFlags::from_tas_keycode(keycode)?)
    }

    while let Some(_) = p.try_take_char(|c| c.is_ascii_whitespace()) {}

    if p.done() {
        Ok(inputs)
    } else {
        Err(anyhow!("trailing characters"))
    }
}

#[cfg(test)]
mod tests {
    use crate::{input::InputFlags, tas::parse_tas_string};

    fn inp<F>(f: F) -> InputFlags
    where
        F: Fn(&mut InputFlags) -> (),
    {
        let mut flags = InputFlags::none();
        f(&mut flags);
        flags
    }

    #[test]
    fn test_parse_tas_string() {
        assert_eq!(parse_tas_string("[]").unwrap(), vec![]);
        assert_eq!(
            parse_tas_string("[]0,1,34,").unwrap(),
            vec![
                InputFlags::none(),
                inp(|v| v.left = true),
                inp(|v| {
                    v.right = true;
                    v.dash = true
                })
            ]
        );
        assert_eq!(
            parse_tas_string("[]2,     ").unwrap(),
            vec![inp(|v| v.right = true)],
        );
    }
}
