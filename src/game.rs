use crate::pico8_num::{constants, int, Pico8Num, Pico8Vec2};
use crate::player_flags::PlayerFlags;
use crate::room::Room;
use crate::sign::{SignVec2, SignVec2Map};
use crate::state_table::PosMap;
use anyhow::Result;
use std::cmp;

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
            Ok(InputFlags {
                left: bit(0),
                right: bit(1),
                up: bit(2),
                down: bit(3),
                jump: bit(4),
                dash: bit(5),
            })
        } else {
            Err(anyhow!("invalid keycode"))
        }
    }

    pub fn iter() -> impl Iterator<Item = InputFlags> {
        (0..(1 << 6)).map(|c| Self::from_tas_keycode(c).unwrap())
    }
}

#[derive(Clone)]
pub struct PlayerPosSpd {
    pub pos: Pico8Vec2,
    pub spd: Pico8Vec2,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct PosSpdFlags {
    pub is_solid_1_0: bool,
    pub is_solid_neg_1_0: bool,
    pub is_solid_0_1: bool,
    pub is_solid_neg_3_0: bool,
    pub is_solid_3_0: bool,
    pub y_more_than_128: bool,
    pub y_less_than_neg_4: bool,
    pub spikes_at: bool,
}

struct Pico8Hitbox {
    x: Pico8Num,
    y: Pico8Num,
    w: Pico8Num,
    h: Pico8Num,
}

const PLAYER_HITBOX: Pico8Hitbox = Pico8Hitbox {
    x: int(1),
    y: int(3),
    w: int(6),
    h: int(5),
};

pub const FREEZE_FRAME_COUNT: usize = 2;

pub enum PlayerUpdateResult {
    Die,
    KeepPlaying {
        freeze: bool,
        player_flags: PlayerFlags,
        spd: Pico8Vec2,
    },
    Win,
}

pub struct PlayerDrawResult {
    pub pos: Pico8Vec2,
    pub spd: Pico8Vec2,
}

fn clamp(val: Pico8Num, a: Pico8Num, b: Pico8Num) -> Pico8Num {
    cmp::max(a, cmp::min(b, val))
}

fn appr(val: Pico8Num, target: Pico8Num, amount: Pico8Num) -> Pico8Num {
    if val > target {
        cmp::max(val - amount, target)
    } else {
        cmp::min(val + amount, target)
    }
}

fn sign(v: Pico8Num) -> Pico8Num {
    int(if v > int(0) {
        1
    } else if v < int(0) {
        -1
    } else {
        0
    })
}

pub struct PlayerSolidSpikesCache {
    solid_at_data: PosMap<bool>,
    spikes_at_data: SignVec2Map<PosMap<bool>>,
}

impl PlayerSolidSpikesCache {
    pub fn calculate(room: &Room) -> Self {
        let mut solid_at_data = PosMap::new_wide();
        solid_at_data
            .fill(|x, y| Self::solid_at_uncached(&Pico8Vec2::from_i16s(x, y), room).unwrap());

        let spikes_at_data = SignVec2Map::new(|spd| {
            let mut spikes_at_inner_data = PosMap::new_wide();
            spikes_at_inner_data.fill(|x, y| {
                Self::spikes_at_uncached(&Pico8Vec2::from_i16s(x, y), spd, room).unwrap()
            });
            spikes_at_inner_data
        });

        Self {
            solid_at_data,
            spikes_at_data,
        }
    }

    fn solid_at(&self, pos_with_offset: &Pico8Vec2) -> Result<bool> {
        if let Some(&v) = self.solid_at_data.get(pos_with_offset.as_i16s_or_err()?) {
            Ok(v)
        } else {
            Err(anyhow!(
                "pos_with_offset out of range: {:?}",
                pos_with_offset
            ))
        }
    }

    fn spikes_at(&self, pos: &Pico8Vec2, spd: SignVec2) -> Result<bool> {
        if let Some(&v) = self.spikes_at_data.get(spd).get(pos.as_i16s_or_err()?) {
            Ok(v)
        } else {
            Err(anyhow!("pos out of range: {:?}", pos))
        }
    }

    fn solid_at_uncached(pos_with_offset: &Pico8Vec2, room: &Room) -> Result<bool> {
        // TODO This is only checking against the room, because there are no
        // active objects except the player at the moment.
        room.solid_at(
            pos_with_offset.x + PLAYER_HITBOX.x,
            pos_with_offset.y + PLAYER_HITBOX.y,
            PLAYER_HITBOX.w,
            PLAYER_HITBOX.h,
        )
    }

    fn spikes_at_uncached(pos: &Pico8Vec2, spd: SignVec2, room: &Room) -> Result<bool> {
        room.spikes_at(
            pos.x + PLAYER_HITBOX.x,
            pos.y + PLAYER_HITBOX.y,
            PLAYER_HITBOX.w,
            PLAYER_HITBOX.h,
            spd,
        )
    }
}

impl PlayerPosSpd {
    pub fn flags(&self, cache: &PlayerSolidSpikesCache) -> Result<PosSpdFlags> {
        let is_solid = |ox: i16, oy: i16| self.is_solid(&Pico8Vec2::from_i16s(ox, oy), cache);
        Ok(PosSpdFlags {
            is_solid_1_0: is_solid(1, 0)?,
            is_solid_neg_1_0: is_solid(-1, 0)?,
            is_solid_0_1: is_solid(0, 1)?,
            is_solid_neg_3_0: is_solid(-3, 0)?,
            is_solid_3_0: is_solid(3, 0)?,
            y_more_than_128: self.pos.y > int(128),
            y_less_than_neg_4: self.pos.y < int(-4),
            spikes_at: cache.spikes_at(&self.pos, SignVec2::from_pico8(&self.spd))?,
        })
    }

    fn is_solid(&self, offset: &Pico8Vec2, cache: &PlayerSolidSpikesCache) -> Result<bool> {
        // TODO This is only checking against the room, because there are no
        // active objects except the player at the moment.
        cache.solid_at(&Pico8Vec2 {
            x: self.pos.x + offset.x,
            y: self.pos.y + offset.y,
        })
    }
}

pub fn run_player_update(
    mut p: PlayerFlags,
    pos_spd_flags: &PosSpdFlags,
    input_flags: &InputFlags,
    mut spd: Pico8Vec2,
) -> PlayerUpdateResult {
    // TODO add support for double dash
    let max_djump = int(1);

    let mut freeze = false;

    let input = int(if input_flags.right {
        1
    } else if input_flags.left {
        -1
    } else {
        0
    });

    if pos_spd_flags.spikes_at {
        return PlayerUpdateResult::Die;
    }

    if pos_spd_flags.y_more_than_128 {
        return PlayerUpdateResult::Die;
    }

    let on_ground = pos_spd_flags.is_solid_0_1;

    let jump = input_flags.jump && !p.p_jump;
    p.p_jump = input_flags.jump;

    if jump {
        p.jbuffer = int(4);
    } else if p.jbuffer > int(0) {
        p.jbuffer = p.jbuffer - int(1);
    }

    let dash = input_flags.dash && !p.p_dash;
    p.p_dash = input_flags.dash;

    if on_ground {
        p.grace = int(6);
        if p.djump < max_djump {
            p.djump = max_djump;
        }
    } else if p.grace > int(0) {
        p.grace = p.grace - int(1);
    }

    if p.dash_time > int(0) {
        p.dash_time = p.dash_time - int(1);
        spd.x = appr(spd.x, p.dash_target.x, p.dash_accel.x);
        spd.y = appr(spd.y, p.dash_target.y, p.dash_accel.y);
    } else {
        let maxrun = int(1);
        let mut accel = constants::PICO8_NUM_0_6;
        let deccel = constants::PICO8_NUM_0_15;

        if !on_ground {
            accel = constants::PICO8_NUM_0_4;
        }

        if spd.x.abs() > maxrun {
            spd.x = appr(spd.x, sign(spd.x) * maxrun, deccel);
        } else {
            spd.x = appr(spd.x, input * maxrun, accel);
        }

        if spd.x != int(0) {
            p.flip_x = spd.x < int(0);
        }

        let mut maxfall = int(2);
        let mut gravity = constants::PICO8_NUM_0_21;

        if spd.y.abs() <= constants::PICO8_NUM_0_15 {
            gravity = gravity * constants::PICO8_NUM_0_5;
        }

        if input == int(1) && pos_spd_flags.is_solid_1_0
            || input == int(-1) && pos_spd_flags.is_solid_neg_1_0
        {
            maxfall = constants::PICO8_NUM_0_4;
        }

        if !on_ground {
            spd.y = appr(spd.y, maxfall, gravity);
        }

        if p.jbuffer > int(0) {
            if p.grace > int(0) {
                p.jbuffer = int(0);
                p.grace = int(0);
                spd.y = int(-2);
            } else {
                let wall_dir = int(if pos_spd_flags.is_solid_neg_3_0 {
                    -1
                } else if pos_spd_flags.is_solid_3_0 {
                    1
                } else {
                    0
                });
                if wall_dir != int(0) {
                    p.jbuffer = int(0);
                    spd.y = int(-2);
                    spd.x = -wall_dir * (maxrun + int(1));
                }
            }
        }

        let d_full = int(5);
        let d_half = d_full * constants::PICO8_NUM_0_70710678118;

        if p.djump > int(0) && dash {
            p.djump = p.djump - int(1);
            p.dash_time = int(4);
            let v_input = int(if input_flags.up {
                -1
            } else if input_flags.down {
                1
            } else {
                0
            });
            if input != int(0) {
                if v_input != int(0) {
                    spd.x = input * d_half;
                    spd.y = v_input * d_half;
                } else {
                    spd.x = input * d_full;
                    spd.y = int(0);
                }
            } else if v_input != int(0) {
                spd.x = int(0);
                spd.y = v_input * d_full;
            } else {
                spd.x = int(if p.flip_x { -1 } else { 1 });
                spd.y = int(0);
            }

            freeze = true;
            p.dash_target.x = int(2) * sign(spd.x);
            p.dash_target.y = int(2) * sign(spd.y);
            p.dash_accel.x = constants::PICO8_NUM_1_5;
            p.dash_accel.y = constants::PICO8_NUM_1_5;

            if spd.y < int(0) {
                p.dash_target.y = p.dash_target.y * constants::PICO8_NUM_0_75;
            }

            if spd.y != int(0) {
                p.dash_accel.x = p.dash_accel.x * constants::PICO8_NUM_0_70710678118;
            }
            if spd.x != int(0) {
                p.dash_accel.y = p.dash_accel.y * constants::PICO8_NUM_0_70710678118;
            }
        }
    }

    // TODO level_index check is missing (but maybe not relevant?)
    if pos_spd_flags.y_less_than_neg_4 {
        return PlayerUpdateResult::Win;
    }

    PlayerUpdateResult::KeepPlaying {
        freeze,
        player_flags: p,
        spd,
    }
}

pub fn run_player_draw(mut pos: Pico8Vec2, mut spd: Pico8Vec2) -> PlayerDrawResult {
    if pos.x < int(-1) || pos.x > int(121) {
        pos.x = clamp(pos.x, int(-1), int(121));
        spd.x = int(0);
    }

    PlayerDrawResult { pos, spd }
}

pub fn run_move_concrete(
    mut p: PlayerPosSpd,
    mut rem: Pico8Vec2,
    solid_spikes_cache: &PlayerSolidSpikesCache,
) -> Result<(PlayerPosSpd, Pico8Vec2)> {
    // TODO Some objects are not solid, but player is and that's the only object
    // that we currently have.
    let solids = true;

    rem.x = rem.x + p.spd.x;
    let amount = (rem.x + constants::PICO8_NUM_0_5).flr();
    rem.x = rem.x - amount;
    move_x(&mut p, &mut rem, solid_spikes_cache, solids, amount, int(0))?;

    rem.y = rem.y + p.spd.y;
    let amount = (rem.y + constants::PICO8_NUM_0_5).flr();
    rem.y = rem.y - amount;
    move_y(&mut p, &mut rem, solid_spikes_cache, solids, amount)?;

    Ok((p, rem))
}

pub const MIN_REM: Pico8Num = constants::PICO8_NUM_0_5.const_neg();
pub const MAX_REM: Pico8Num = constants::PICO8_NUM_0_5.next_smallest();
pub const ALL_DIFFERENT_REMS_FOR_MOVE: [Pico8Vec2; 4] = [
    Pico8Vec2 {
        x: MIN_REM,
        y: MIN_REM,
    },
    Pico8Vec2 {
        x: MIN_REM,
        y: MAX_REM,
    },
    Pico8Vec2 {
        x: MAX_REM,
        y: MIN_REM,
    },
    Pico8Vec2 {
        x: MAX_REM,
        y: MAX_REM,
    },
];

fn move_x(
    p: &mut PlayerPosSpd,
    rem: &mut Pico8Vec2,
    solid_spikes_cache: &PlayerSolidSpikesCache,
    solids: bool,
    amount: Pico8Num,
    start: Pico8Num,
) -> Result<()> {
    if solids {
        let step = sign(amount);
        for _ in start.as_i16_or_err()?..=amount.abs().as_i16_or_err()? {
            if !p.is_solid(&Pico8Vec2 { x: step, y: int(0) }, solid_spikes_cache)? {
                p.pos.x = p.pos.x + step;
            } else {
                p.spd.x = int(0);
                rem.x = int(0);
                break;
            }
        }
    } else {
        p.pos.x = p.pos.x + amount;
    }
    Ok(())
}

fn move_y(
    p: &mut PlayerPosSpd,
    rem: &mut Pico8Vec2,
    solid_spikes_cache: &PlayerSolidSpikesCache,
    solids: bool,
    amount: Pico8Num,
) -> Result<()> {
    if solids {
        let step = sign(amount);
        for _ in 0..=amount.abs().as_i16_or_err()? {
            if !p.is_solid(&Pico8Vec2 { x: int(0), y: step }, solid_spikes_cache)? {
                p.pos.y = p.pos.y + step;
            } else {
                p.spd.y = int(0);
                rem.y = int(0);
                break;
            }
        }
    } else {
        p.pos.y = p.pos.y + amount;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use std::{fs, path::PathBuf};

    use crate::{
        cart_data::CartData,
        game::{
            run_move_concrete, run_player_draw, run_player_update, InputFlags, PlayerPosSpd,
            PlayerSolidSpikesCache, PlayerUpdateResult, FREEZE_FRAME_COUNT,
        },
        pico8_num::{int, Pico8Num, Pico8Vec2},
        player_flags::PlayerFlags,
        room::Room,
        tas::parse_tas_string,
    };

    fn load_cart_data() -> Result<CartData> {
        CartData::load(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("cart"))
    }

    #[test]
    fn test_concrete_game() -> Result<()> {
        let cart_data = load_cart_data()?;
        let room = Room::from_position(&cart_data, int(1), int(0))?;

        let solid_spikes_cache = PlayerSolidSpikesCache::calculate(&room);

        let update = |player_pos_spd: &mut PlayerPosSpd,
                      player_flags: &mut PlayerFlags,
                      rem: &mut Pico8Vec2,
                      freeze: &mut Pico8Num,
                      input: &InputFlags|
         -> Result<bool> {
            if *freeze > int(0) {
                *freeze = *freeze - int(1);
                return Ok(false);
            }

            if player_pos_spd.spd.x != int(0) || player_pos_spd.spd.y != int(0) {
                let move_result =
                    run_move_concrete(player_pos_spd.clone(), rem.clone(), &solid_spikes_cache)?;
                *player_pos_spd = move_result.0;
                *rem = move_result.1;
            }

            let player_update_result = run_player_update(
                player_flags.clone(),
                &player_pos_spd.flags(&solid_spikes_cache)?,
                input,
                player_pos_spd.spd.clone(),
            );
            match player_update_result {
                PlayerUpdateResult::Die => panic!("unexpected death"),
                PlayerUpdateResult::KeepPlaying {
                    freeze: new_freeze,
                    player_flags: new_player_flags,
                    spd: new_spd,
                } => {
                    *freeze = int(if new_freeze {
                        FREEZE_FRAME_COUNT as i16
                    } else {
                        0
                    });
                    *player_flags = new_player_flags;
                    player_pos_spd.spd = new_spd;

                    Ok(false)
                }
                PlayerUpdateResult::Win => Ok(true),
            }
        };

        let draw = |player_pos_spd: &mut PlayerPosSpd, freeze: Pico8Num| -> Result<()> {
            if freeze > int(0) {
                return Ok(());
            }

            let draw_result =
                run_player_draw(player_pos_spd.pos.clone(), player_pos_spd.spd.clone());
            player_pos_spd.pos = draw_result.pos;
            player_pos_spd.spd = draw_result.spd;

            Ok(())
        };

        let tas = fs::read_to_string(
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tas/baseline/TAS2.tas"),
        )?;
        let tas = parse_tas_string(&tas)?;
        let required_frames = 75;

        let mut player_pos_spd = PlayerPosSpd {
            pos: room.spawn_pos(),
            spd: Pico8Vec2::zero(),
        };
        let mut player_flags = PlayerFlags::spawn(int(1));
        let mut rem = Pico8Vec2::zero();
        let mut freeze = int(0);
        let mut did_ever_win = false;

        // TODO double check that the order is: init, update, draw, update, draw, ...
        for (i, input) in tas.iter().enumerate() {
            let did_win = update(
                &mut player_pos_spd,
                &mut player_flags,
                &mut rem,
                &mut freeze,
                input,
            )?;
            did_ever_win = did_ever_win || did_win;
            assert!(did_win == (i >= required_frames));

            draw(&mut player_pos_spd, freeze)?;
        }

        assert!(did_ever_win);

        Ok(())
    }
}
