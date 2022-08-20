use crate::pico8_num::{constants, int, Pico8Num, Pico8Vec2};
use crate::player_flags::PlayerFlags;
use crate::room::Room;
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

#[derive(Clone)]
pub struct PlayerPosSpd {
    pub pos: Pico8Vec2,
    pub spd: Pico8Vec2,
}

#[derive(Debug)]
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

pub enum PlayerUpdateResult {
    Die,
    KeepPlaying {
        player_flags: PlayerFlags,
        spd: Pico8Vec2,
    },
    Win,
}

pub struct PlayerDrawResult {
    pos: Pico8Vec2,
    spd: Pico8Vec2,
}

pub struct MoveConcreteResult {
    pos_spd: PlayerPosSpd,
    rem: Pico8Vec2,
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

impl PlayerPosSpd {
    pub fn flags(&self, room: &Room) -> Result<PosSpdFlags> {
        let is_solid = |ox: i16, oy: i16| {
            self.is_solid(
                &Pico8Vec2 {
                    x: int(ox),
                    y: int(oy),
                },
                &room,
            )
        };
        Ok(PosSpdFlags {
            is_solid_1_0: is_solid(1, 0)?,
            is_solid_neg_1_0: is_solid(-1, 0)?,
            is_solid_0_1: is_solid(0, 1)?,
            is_solid_neg_3_0: is_solid(-3, 0)?,
            is_solid_3_0: is_solid(3, 0)?,
            y_more_than_128: self.pos.y > int(128),
            y_less_than_neg_4: self.pos.y < int(-4),
            spikes_at: room.spikes_at(
                self.pos.x + PLAYER_HITBOX.x,
                self.pos.y + PLAYER_HITBOX.y,
                PLAYER_HITBOX.w,
                PLAYER_HITBOX.h,
                &self.spd,
            )?,
        })
    }

    fn is_solid(&self, offset: &Pico8Vec2, room: &Room) -> Result<bool> {
        // TODO This is only checking against the room, because there are no
        // active objects except the player at the moment.
        room.solid_at(
            self.pos.x + PLAYER_HITBOX.x + offset.x,
            self.pos.y + PLAYER_HITBOX.y + offset.y,
            PLAYER_HITBOX.w,
            PLAYER_HITBOX.h,
        )
    }
}

fn run_player_update(
    mut p: PlayerFlags,
    pos_spd_flags: &PosSpdFlags,
    input_flags: &InputFlags,
    mut spd: Pico8Vec2,
) -> PlayerUpdateResult {
    // TODO remember to handle freeze in game update

    // TODO add support for double dash
    let max_djump = int(1);

    // TODO move this to the global update function
    // if player_flags.freeze > Pico8Num::from_i16(0) {
    //     player_flags.freeze = player_flags.freeze - Pico8Num::from_i16(1);
    //     return PlayerUpdateResult::Alive { player_flags, spd };
    // }

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

            p.freeze = int(2);
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
    room: &Room,
) -> Result<(PlayerPosSpd, Pico8Vec2)> {
    // TODO Some objects are not solid, but player is and that's the only object
    // that we currently have.
    let solids = true;

    rem.x = rem.x + p.spd.x;
    let amount = (rem.x + constants::PICO8_NUM_0_5).flr();
    rem.x = rem.x - amount;
    move_x(&mut p, &mut rem, room, solids, amount, int(0))?;

    rem.y = rem.y + p.spd.y;
    let amount = (rem.y + constants::PICO8_NUM_0_5).flr();
    rem.y = rem.y - amount;
    move_y(&mut p, &mut rem, room, solids, amount)?;

    Ok((p, rem))
}

fn move_x(
    p: &mut PlayerPosSpd,
    rem: &mut Pico8Vec2,
    room: &Room,
    solids: bool,
    amount: Pico8Num,
    start: Pico8Num,
) -> Result<()> {
    if solids {
        let step = sign(amount);
        for _ in start.as_i16_or_err()?..=amount.abs().as_i16_or_err()? {
            if !p.is_solid(&Pico8Vec2 { x: step, y: int(0) }, room)? {
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
    room: &Room,
    solids: bool,
    amount: Pico8Num,
) -> Result<()> {
    if solids {
        let step = sign(amount);
        for _ in 0..=amount.abs().as_i16_or_err()? {
            if !p.is_solid(&Pico8Vec2 { x: int(0), y: step }, room)? {
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
            PlayerUpdateResult,
        },
        pico8_num::{int, Pico8Vec2},
        player_flags::{self, PlayerFlags},
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

        let mut player_pos_spd = PlayerPosSpd {
            pos: room.spawn_pos(),
            spd: Pico8Vec2::zero(),
        };
        let mut player_flags = PlayerFlags::spawn(int(0), int(1));
        let mut rem = Pico8Vec2::zero();

        let tas = fs::read_to_string(
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tas/baseline/TAS2.tas"),
        )?;
        let tas = parse_tas_string(&tas)?;
        let required_frames = 75;

        let update = |player_pos_spd: &mut PlayerPosSpd,
                      player_flags: &mut PlayerFlags,
                      rem: &mut Pico8Vec2,
                      input: &InputFlags|
         -> Result<bool> {
            if player_flags.freeze > int(0) {
                player_flags.freeze = player_flags.freeze - int(1);
                return Ok(false);
            }

            if player_pos_spd.spd.x != int(0) || player_pos_spd.spd.y != int(0) {
                (*player_pos_spd, *rem) =
                    run_move_concrete(player_pos_spd.clone(), rem.clone(), &room)?;
            }

            let player_update_result = run_player_update(
                player_flags.clone(),
                &player_pos_spd.flags(&room)?,
                input,
                player_pos_spd.spd.clone(),
            );
            match player_update_result {
                PlayerUpdateResult::Die => panic!("unexpected death"),
                PlayerUpdateResult::KeepPlaying {
                    player_flags: new_player_flags,
                    spd: new_spd,
                } => {
                    *player_flags = new_player_flags;
                    player_pos_spd.spd = new_spd;

                    Ok(false)
                }
                PlayerUpdateResult::Win => Ok(true),
            }
        };

        let draw = |player_pos_spd: &mut PlayerPosSpd, player_flags: &PlayerFlags| -> Result<()> {
            if player_flags.freeze > int(0) {
                return Ok(());
            }

            let draw_result =
                run_player_draw(player_pos_spd.pos.clone(), player_pos_spd.spd.clone());
            player_pos_spd.pos = draw_result.pos;
            player_pos_spd.spd = draw_result.spd;

            Ok(())
        };

        let mut did_ever_win = false;

        // TODO double check that the order is: init, update, draw, update, draw, ...
        for (i, input) in tas.iter().enumerate() {
            let did_win = update(&mut player_pos_spd, &mut player_flags, &mut rem, input)?;
            did_ever_win = did_ever_win || did_win;
            assert!(did_win == (i >= required_frames));

            draw(&mut player_pos_spd, &player_flags)?;
        }

        assert!(did_ever_win);

        Ok(())
    }
}
