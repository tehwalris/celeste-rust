use cart_data::CartData;
use pico8_num::{int, Pico8Vec2};
use player_flags::{PlayerFlagBitVec, PlayerFlags};
use room::Room;
use state_table::{Pico8Vec2Map, StateTable};

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
#[macro_use(lazy_static)]
extern crate lazy_static;
extern crate hex;
extern crate regex;
extern crate rustc_hash;

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
        run_move_concrete, run_player_draw, run_player_update, InputFlags, PlayerDrawResult,
        PlayerUpdateResult, ALL_DIFFERENT_REMS_FOR_MOVE, FREEZE_FRAME_COUNT, MAX_REM, MIN_REM,
    },
    player_flags::CompressedPlayerFlags,
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

fn add_reachable_to_dst_frame_cached<'a>(
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
                    (&transition.keep_playing, &mut *dst_frame_keep_playing),
                    (&transition.freeze, &mut *dst_frame_freeze),
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
                            .mut_or(&dst_player_flags);
                    }
                }
            }
        }
    }
    Ok(())
}

fn should_skip_run(player_flags: &PlayerFlags, input: &InputFlags) -> bool {
    // Left and right cancel out
    if input.left && input.right {
        return true;
    }

    // Up and down cancel out
    if input.up && input.down {
        return true;
    }

    // Jump will be ignored if it was pressed last frame
    if input.jump && player_flags.p_jump {
        return true;
    }

    // Dash will be ignored if it was pressed last frame
    if input.dash && player_flags.p_dash {
        return true;
    }

    false
}

fn should_skip_save(player_flags: &PlayerFlags) -> bool {
    // Can press jump later instead of buffering a jump
    // TODO double check that this is equivalent
    if player_flags.jbuffer == int(4) {
        return true;
    }
    assert!(player_flags.jbuffer == int(0));

    false
}

fn add_reachable_to_dst_frame_direct<'a>(
    src_frame: &StateTable,
    dst_frame_keep_playing: &'a mut StateTable,
    dst_frame_freeze: &'a mut StateTable,
    room: &Room,
) -> Result<()> {
    let mut potential_runs = 0;
    let mut actual_runs = 0;
    let mut potential_saves = 0;
    let mut actual_saves = 0;

    for (src_pos, src_spd_map) in src_frame.iter() {
        for (src_spd, src_player_flags_vec) in src_spd_map.iter() {
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

                let post_move_pos_spd_flags = post_move_pos_spd.flags(&room)?;

                for src_compressed_player_flags in CompressedPlayerFlags::iter() {
                    if !src_player_flags_vec.get(src_compressed_player_flags) {
                        continue;
                    }
                    let src_player_flags = src_compressed_player_flags.decompress();
                    for input in InputFlags::iter() {
                        potential_runs += 1;
                        if should_skip_run(&src_player_flags, &input) {
                            continue;
                        }
                        actual_runs += 1;

                        let update_result = run_player_update(
                            src_player_flags.clone(),
                            &post_move_pos_spd_flags,
                            &input,
                            post_move_pos_spd.spd.clone(),
                        );

                        if let PlayerUpdateResult::KeepPlaying {
                            freeze,
                            player_flags: mut dst_player_flags,
                            spd: post_update_spd,
                        } = update_result
                        {
                            potential_saves += 1;
                            if should_skip_save(&dst_player_flags) {
                                continue;
                            }
                            actual_saves += 1;

                            dst_player_flags.adjust_before_compress();
                            let dst_compressed_player_flags = dst_player_flags
                                .compress()
                                .ok_or(anyhow!("could not compress dst_player_flags"))?;

                            let PlayerDrawResult {
                                pos: dst_pos,
                                spd: dst_spd,
                            } = run_player_draw(
                                post_move_pos_spd.pos.clone(),
                                post_update_spd.clone(),
                            );

                            let dst_frame = if freeze {
                                &mut *dst_frame_freeze
                            } else {
                                &mut *dst_frame_keep_playing
                            };
                            let dst_spd_map = dst_frame.get_mut_or_insert_with(
                                (dst_pos.x.as_i16_or_err()?, dst_pos.y.as_i16_or_err()?),
                                Pico8Vec2Map::new,
                            );
                            dst_spd_map
                                .get_mut_or_insert_with(dst_spd.clone(), PlayerFlagBitVec::new)
                                .set(dst_compressed_player_flags, true);
                        }
                    }
                }
            }
        }
    }

    println!(
        "potential_runs: {}, actual_runs: {}, potential_saves: {}, actual_saves: {}",
        potential_runs, actual_runs, potential_saves, actual_saves
    );

    Ok(())
}

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;
    let room = Room::from_position(&cart_data, int(1), int(0))?;

    let mut transition_cache = TransitionCache::new(&room);

    let mut frames: Vec<Option<StateTable>> = (0..1 + FREEZE_FRAME_COUNT)
        .map(|_| Some(StateTable::new()))
        .collect();
    set_initial_state(frames[0].as_mut().unwrap(), &room)?;

    for i in 0..50 {
        println!("i {}", i);

        frames.push(Some(StateTable::new()));

        let future_frames = &mut frames[i..];
        let (src_frame, future_frames) = future_frames.split_first_mut().unwrap();
        let (dst_frame_keep_playing, future_frames) = future_frames.split_first_mut().unwrap();
        let dst_frame_freeze = &mut future_frames[FREEZE_FRAME_COUNT - 1];

        let src_frame = src_frame.as_ref().unwrap();
        let dst_frame_keep_playing = dst_frame_keep_playing.as_mut().unwrap();
        let dst_frame_freeze = dst_frame_freeze.as_mut().unwrap();

        let before_update = std::time::Instant::now();
        add_reachable_to_dst_frame_direct(
            src_frame,
            dst_frame_keep_playing,
            dst_frame_freeze,
            &room,
        )?;
        let update_elapsed = before_update.elapsed();

        println!("update_elapsed: {:?}", update_elapsed,);

        println!("src_frame stats:");
        src_frame.print_stats();
        println!("dst_frame_keep_playing stats:");
        dst_frame_keep_playing.print_stats();
        println!("dst_frame_freeze stats:");
        dst_frame_freeze.print_stats();
        println!("transition_cache stats:");
        transition_cache.print_stats();
    }

    println!("done");

    Ok(())
}
