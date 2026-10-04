//! Evaluating a traced graph at a point, so the trace can be CHECKED.
//!
//! The tracer's claim is that the graph it leaves behind computes what
//! running the program would have computed. That is checkable directly:
//! substitute concrete values for the input leaves, evaluate, and compare
//! against the same interpreter run with those same values concrete. This
//! module is the "evaluate" half.
//!
//! It delegates every operation to `domain::Concrete`, on purpose. An
//! evaluator that reimplemented pico-8 arithmetic would be a THIRD
//! definition of what the program means, and then the check would be
//! testing the evaluator as much as the tracer. What is left here is the
//! `Op` -> `Concrete` mapping, which is one line per op and nothing else.
//!
//! ## Why no recursion
//!
//! A node's operands are always created before it is, so operand ids are
//! strictly smaller. One backward pass marks what the root needs and one
//! forward pass evaluates it, which is linear and cannot blow the stack
//! on a graph whose depth is a frame's worth of arithmetic.

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

use crate::pico8_num::Pico8Num as P8;
use crate::transpile::graph::{Graph, NodeId, Op};

use super::domain::{Arith, Cmp, Concrete, Domain, Fun1, Fun2};
use super::iface::Conc;

/// What the leaves stand for.
pub struct Env<'a> {
    /// `Op::Cell(i)` - the frame's input slots, in `Iface` order.
    pub cells: &'a [Conc],
    pub cart: Option<Arc<CartData>>,
    pub cache: Option<Arc<CollisionCache>>,
}

fn num(c: Conc) -> Result<P8> {
    match c {
        Conc::Num(n) => Ok(n),
        Conc::Bool(b) => bail!("expected a number, got {}", b),
    }
}

fn boolean(c: Conc) -> Result<bool> {
    match c {
        Conc::Bool(b) => Ok(b),
        Conc::Num(n) => bail!("expected a boolean, got {:?}", n),
    }
}

pub fn eval(g: &Graph, root: NodeId, env: &Env) -> Result<Conc> {
    let n = root as usize + 1;
    let mut need = vec![false; n];
    need[root as usize] = true;
    for id in (0..n).rev() {
        if !need[id] {
            continue;
        }
        for a in &g.get(id as NodeId).args {
            need[*a as usize] = true;
        }
    }
    let val = run(g, &need, env, true)?;
    val[root as usize].ok_or_else(|| anyhow!("root {} was not evaluated", root))
}

/// One forward pass. Operands always have smaller ids, so this is linear
/// and needs no recursion. `strict` decides whether a node that cannot be
/// evaluated is an error or just a `None` that propagates.
fn run(g: &Graph, need: &[bool], env: &Env, strict: bool) -> Result<Vec<Option<Conc>>> {
    let n = need.len();
    let mut val: Vec<Option<Conc>> = vec![None; n];
    let mut c = Concrete;
    for id in 0..n {
        if !need[id] {
            continue;
        }
        let node = g.get(id as NodeId);
        let a = |i: usize| -> Result<Conc> {
            val[node.args[i] as usize]
                .ok_or_else(|| anyhow!("operand {} of node {} was not evaluated", i, id))
        };
        let one = || -> Result<Conc> {
        let v = match node.op {
            Op::Const(lo, hi) => {
                if lo != hi {
                    bail!("node {} is the interval [{}, {}], not a value", id, lo, hi);
                }
                Conc::Num(P8::from_raw(lo))
            }
            Op::ConstBool(b) => Conc::Bool(b),
            // Concrete: an unknown has no single value to return.
            Op::UnknownNum | Op::UnknownBool(_) => bail!("node {} is an unknown, not a value", id),
            // This evaluator is CONCRETE - one number per node - so a
            // span is a value only when it is degenerate, exactly as an
            // interval literal is. Same refusal, for the same reason:
            // there is no single number to return.
            Op::Span => {
                let (lo, hi) = (num(a(0)?)?, num(a(1)?)?);
                if lo != hi {
                    bail!("node {} is the interval [{:?}, {:?}], not a value", id, lo, hi);
                }
                Conc::Num(lo)
            }
            Op::Cell(i) => *env
                .cells
                .get(i as usize)
                .ok_or_else(|| anyhow!("no value for cell {}", i))?,
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem => {
                let op = match node.op {
                    Op::Add => Arith::Add,
                    Op::Sub => Arith::Sub,
                    Op::Mul => Arith::Mul,
                    Op::Div => Arith::Div,
                    _ => Arith::Rem,
                };
                Conc::Num(c.arith(op, &num(a(0)?)?, &num(a(1)?)?)?)
            }
            Op::Neg | Op::Abs | Op::Flr | Op::Sin => {
                let f = match node.op {
                    Op::Neg => Fun1::Neg,
                    Op::Abs => Fun1::Abs,
                    Op::Flr => Fun1::Flr,
                    _ => Fun1::Sin,
                };
                Conc::Num(c.fun1(f, &num(a(0)?)?)?)
            }
            Op::Min | Op::Max => {
                let f = if node.op == Op::Min { Fun2::Min } else { Fun2::Max };
                Conc::Num(c.fun2(f, &num(a(0)?)?, &num(a(1)?)?)?)
            }
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                let op = match node.op {
                    Op::Lt => Cmp::Lt,
                    Op::Le => Cmp::Le,
                    Op::Gt => Cmp::Gt,
                    Op::Ge => Cmp::Ge,
                    _ => Cmp::Eq,
                };
                Conc::Bool(c.compare(op, &num(a(0)?)?, &num(a(1)?)?)?)
            }
            Op::Not => Conc::Bool(!boolean(a(0)?)?),
            Op::And => Conc::Bool(boolean(a(0)?)? && boolean(a(1)?)?),
            Op::Or => Conc::Bool(boolean(a(0)?)? || boolean(a(1)?)?),
            Op::Sel => {
                if boolean(a(0)?)? {
                    a(1)?
                } else {
                    a(2)?
                }
            }
            // Every leaf was substituted with a value, so everything
            // reachable IS determined - which is what `Known` asks.
            Op::Known => Conc::Bool(true),
            // Concrete arithmetic wraps as PICO-8 does: one number, never an
            // interval whose ends wrapped apart.
            Op::NoWrap => Conc::Bool(true),
            Op::Mget => {
                let cart = env.cart.clone().ok_or_else(|| anyhow!("mget: no cart"))?;
                let t = cart.mget(num(a(0)?)?, num(a(1)?)?)?;
                Conc::Num(P8::from_i16(t as i16))
            }
            Op::TileFlagAt => {
                let cart = env.cart.clone().ok_or_else(|| anyhow!("tile_flag_at: no cart"))?;
                let cache = env
                    .cache
                    .clone()
                    .ok_or_else(|| anyhow!("tile_flag_at: no collision cache"))?;
                let gi = |v: P8| v.as_i16().ok_or_else(|| anyhow!("tile_flag_at: non-integer"));
                // Any flag: solid (0), ice (4). A flag no tile of the room
                // carries folds to false at trace time and never gets here.
                let r = cache.flag_at(
                    &cart,
                    gi(num(a(0)?)?)?,
                    gi(num(a(1)?)?)?,
                    gi(num(a(2)?)?)?,
                    gi(num(a(3)?)?)?,
                    gi(num(a(4)?)?)?,
                )?;
                Conc::Bool(r)
            }
            // A fork has no one value, and what resolving one leaves over an
            // interval is not concrete either. Resolve the configuration
            // first (`Graph::specialize_subset_into`): the buttons are forks
            // over a literal, and resolve to constants.
            Op::Split(_) | Op::SplitValid(_) | Op::SplitInt(_) | Op::IntFrag(_) | Op::SplitOk(_) | Op::Frag(_) | Op::FragOk(_)
            | Op::Lo | Op::Hi => {
                bail!("node {} is {:?}: resolve the fork configuration before evaluating", id, node.op)
            }
        };
        Ok(v)
        }();
        match one {
            Ok(v) => val[id] = Some(v),
            Err(e) if strict => return Err(e),
            Err(_) => val[id] = None,
        }
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_evaluates_what_the_symbolic_domain_built() {
        // Build the same expression twice - once over cells, once over
        // constants - and check the evaluator agrees with the folding the
        // domain did for free. That is the whole differential check in
        // miniature: same code, one side concrete.
        let mut d = super::super::domain::Symbolic::default();
        let three = P8::from_i16(3);
        let four = P8::from_i16(4);
        let (x, y) = (d.graph.leaf(Op::Cell(0)), d.graph.leaf(Op::Cell(1)));
        let sum = d.arith(Arith::Add, &x, &y).unwrap();
        let gt = d.compare(Cmp::Gt, &sum, &x).unwrap();
        let out = d.sel_num(&gt, &sum, &x);

        let cells = [Conc::Num(three), Conc::Num(four)];
        let env = Env { cells: &cells, cart: None, cache: None };
        assert_eq!(eval(&d.graph, out, &env).unwrap(), Conc::Num(P8::from_i16(7)));

        let (a, b) = (d.num(three), d.num(four));
        let s2 = d.arith(Arith::Add, &a, &b).unwrap();
        assert_eq!(d.as_const(&s2), Some(P8::from_i16(7)));
    }

    #[test]
    fn a_button_is_a_fork_evaluated_per_configuration() {
        let mut d = super::super::domain::Symbolic::default();
        let f = d.unknown_bool().unwrap();
        let (t, e) = (d.num(P8::from_i16(1)), d.num(P8::from_i16(0)));
        let out = d.sel_num(&f, &t, &e);
        let env = Env { cells: &[], cart: None, cache: None };
        assert!(eval(&d.graph, out, &env).is_err(), "an unresolved fork has no one value");
        for (c, want) in [(1u8, 1i16), (0, 0)] {
            let mut g = d.graph.like();
            let map = d.graph.specialize_subset_into(&[c], None, &mut g);
            assert_eq!(eval(&g, map[out as usize], &env).unwrap(), Conc::Num(P8::from_i16(want)));
        }
    }
}
