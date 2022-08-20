use crate::pico8_num::{constants, int, Pico8Num, Pico8Vec2};
use crate::player_flags::PlayerFlags;
use crate::room::Room;
use anyhow::Result;
use std::cmp;
use std::path::is_separator;

pub struct InputFlags {
    pub left: bool,
    pub right: bool,
    pub up: bool,
    pub down: bool,
    pub jump: bool,
    pub dash: bool,
}

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

fn clamp(val: Pico8Num, a: Pico8Num, b: Pico8Num) -> Pico8Num {
    cmp::max(a, cmp::min(b, val))
}

fn appr(val: Pico8Num, target: Pico8Num, amount: Pico8Num) -> Pico8Num {
    if val > target {
        cmp::max(val - amount, target)
    } else {
        cmp::max(val + amount, target)
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

impl PosSpdFlags {
    pub fn new(pos: &Pico8Vec2, spd: &Pico8Vec2, room: &Room) -> Result<PosSpdFlags> {
        let is_solid = |ox: i16, oy: i16| {
            Self::is_solid(
                &Pico8Vec2 {
                    x: int(ox),
                    y: int(oy),
                },
                &room,
                pos,
                &PLAYER_HITBOX,
            )
        };
        Ok(PosSpdFlags {
            is_solid_1_0: is_solid(1, 0)?,
            is_solid_neg_1_0: is_solid(-1, 0)?,
            is_solid_0_1: is_solid(0, 1)?,
            is_solid_neg_3_0: is_solid(-3, 0)?,
            is_solid_3_0: is_solid(3, 0)?,
            y_more_than_128: pos.y > int(128),
            y_less_than_neg_4: pos.y < int(-4),
            spikes_at: room.spikes_at(
                pos.x + PLAYER_HITBOX.x,
                pos.y + PLAYER_HITBOX.y,
                PLAYER_HITBOX.w,
                PLAYER_HITBOX.h,
                spd,
            )?,
        })
    }

    fn is_solid(
        offset: &Pico8Vec2,
        room: &Room,
        pos: &Pico8Vec2,
        hitbox: &Pico8Hitbox,
    ) -> Result<bool> {
        // TODO This is only checking against the room, because there are no
        // active objects except the player at the moment.
        room.solid_at(
            pos.x + hitbox.x + offset.x,
            pos.y + hitbox.y + offset.y,
            hitbox.w,
            hitbox.h,
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

        if jump {
            if p.grace > int(0) {
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

pub fn run_move(mut ox: )