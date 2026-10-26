//! THE TOOLS that check, fingerprint and explain the core from outside it
//! (plans/architecture.md "Code layout"): each reads trees and records or
//! rebuilds through the core's entry points; none is a build option or a
//! field of a production type, and the core names none of them
//! (`conventions`).
#![warn(unreachable_pub)]
pub mod arcs;
pub mod census;
pub mod differential;
pub mod explain;
pub mod gate;
pub mod keys;
pub mod reference;
pub mod replay;
