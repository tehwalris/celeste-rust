//! The one IR identifier newtype that survived the CFG's deletion:
//! `GlobalId` names closures in `Value` and the block bridge. It lives in
//! `celeste-core` so the interpreter and the bridge can name it without a
//! crate that no longer exists.

use serde::{Deserialize, Serialize};

/// A global (function) name. Stored by `Value::Closure` and the block
/// bridge's cell translation.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct GlobalId(String);

impl GlobalId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for GlobalId {
    fn from(s: String) -> Self {
        Self(s)
    }
}
