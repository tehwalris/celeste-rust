use cart_data::CartData;
use pico8_num::{int, Pico8Vec2};
use player_flags::{PlayerFlagBitMatrix, PlayerFlagBitVec, PlayerFlags};
use room::Room;
use state_table::{Pico8Vec2Map, PosMap, StateTable};

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
#[macro_use(lazy_static)]
extern crate lazy_static;
extern crate hex;
extern crate regex;

mod cart_data;
mod game;
mod pico8_num;
mod player_flags;
mod room;
mod state_table;
mod tas;
mod transition_cache;

use anyhow::Result;

use crate::{
    game::{
        run_move_concrete, run_player_draw, PlayerDrawResult, ALL_DIFFERENT_REMS_FOR_MOVE, MAX_REM,
        MIN_REM,
    },
    transition_cache::TransitionCache,
};

fn set_initial_state(frame: &mut StateTable, room: &Room) -> Result<()> {
    let pos = room.spawn_pos();
    let spd = Pico8Vec2::zero();

    let mut player_flags = PlayerFlags::spawn(int(1));
    player_flags.adjust_before_compress();
    let compressed_player_flags = player_flags
        .compress()
        .ok_or(anyhow!("failed to compress initial player_flags"))?;

    let spd_map = frame.get_mut_or_insert_with(
        (pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?),
        Pico8Vec2Map::new,
    );
    let flag_vec = spd_map.get_mut_or_insert_with(spd, PlayerFlagBitVec::new);
    flag_vec.set(compressed_player_flags, true);

    Ok(())
}

fn add_reachable_to_dst_frame<'a>(
    src_frame: &StateTable,
    dst_frame_keep_playing: &'a mut StateTable,
    dst_frame_freeze: &'a mut StateTable,
    room: &Room,
    transition_cache: &mut TransitionCache,
) -> Result<()> {
    for (src_pos, src_spd_map) in src_frame.iter() {
        for (src_spd, src_player_flags) in src_spd_map.iter() {
            for rem in ALL_DIFFERENT_REMS_FOR_MOVE {
                let (post_move_pos_spd, post_move_rem) = run_move_concrete(
                    game::PlayerPosSpd {
                        pos: Pico8Vec2 {
                            x: int(src_pos.0),
                            y: int(src_pos.1),
                        },
                        spd: src_spd.clone(),
                    },
                    rem,
                    &room,
                )?;
                assert!(
                    post_move_rem.x >= MIN_REM
                        && post_move_rem.x <= MAX_REM
                        && post_move_rem.y >= MIN_REM
                        && post_move_rem.y <= MAX_REM
                );

                let transition =
                    transition_cache.get_or_calculate_transition(&post_move_pos_spd)?;
                for (matrices_by_spd, dst_frame) in [
                    (&transition.freeze, &mut *dst_frame_freeze),
                    (&transition.keep_playing, &mut *dst_frame_keep_playing),
                ] {
                    for (post_update_spd, matrix) in matrices_by_spd.iter() {
                        let dst_player_flags = matrix.calculate_reachable(src_player_flags);
                        let PlayerDrawResult {
                            pos: dst_pos,
                            spd: dst_spd,
                        } = run_player_draw(post_move_pos_spd.pos.clone(), post_update_spd.clone());

                        let dst_spd_map = dst_frame.get_mut_or_insert_with(
                            (dst_pos.x.as_i16_or_err()?, dst_pos.y.as_i16_or_err()?),
                            Pico8Vec2Map::new,
                        );
                        dst_spd_map
                            .get_mut_or_insert_with(dst_spd, PlayerFlagBitVec::new)
                            .mut_or(&dst_player_flags)
                    }
                }
            }
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;
    let room = Room::from_position(&cart_data, int(1), int(0))?;

    let mut transition_cache = TransitionCache::new(&room);

    let mut src_frame: StateTable = PosMap::new();
    set_initial_state(&mut src_frame, &room)?;

    let mut dst_frame_keep_playing: StateTable = PosMap::new();
    let mut dst_frame_freeze: StateTable = PosMap::new();

    add_reachable_to_dst_frame(
        &src_frame,
        &mut dst_frame_keep_playing,
        &mut &mut dst_frame_freeze,
        &room,
        &mut transition_cache,
    )?;

    println!("done");

    Ok(())
}
