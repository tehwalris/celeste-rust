use std::{
    ops::{Add, DerefMut},
    rc::Rc,
    sync::{Arc, Mutex},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use anyhow::Result;
use cart_data::CartData;
use image::{ImageBuffer, Luma};
use pico8_num::{int, Pico8Vec2};
use player_flags::PlayerFlags;
use room::Room;
use rustc_hash::{FxHashMap, FxHashSet};
use state_table::{PosMapRange, StateTable};
use work_queue::Queue;

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
#[macro_use(lazy_static)]
extern crate lazy_static;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

mod cart_data;
mod game;
mod pico8_num;
mod player_flags;
mod room;
mod sign;
mod state_table;
mod tas;

use crate::game::{
    run_move_concrete, run_player_draw, run_player_update, InputFlags, PlayerDrawResult,
    PlayerSolidSpikesCache, PlayerUpdateResult, ALL_DIFFERENT_REMS_FOR_MOVE, FREEZE_FRAME_COUNT,
    MAX_REM, MIN_REM,
};

fn set_initial_state(frame: &mut StateTable, room: &Room) -> Result<()> {
    let pos = room.spawn_pos();
    let spd = Pico8Vec2::zero();

    let mut player_flags = PlayerFlags::spawn(int(1));
    player_flags.adjust_before_compress();
    let compressed_player_flags = player_flags
        .compress()
        .ok_or(anyhow!("failed to compress initial player_flags"))?;

    let spd_player_flag_map = frame.get_mut_or_insert_with(
        (pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?),
        FxHashSet::default,
    );
    spd_player_flag_map.insert((spd, compressed_player_flags));

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

#[derive(Clone)]
struct AddReachableStats {
    potential_runs: usize,
    actual_runs: usize,
    potential_saves: usize,
    actual_saves: usize,
    elapsed: Option<Duration>,
    single_threaded_elapsed: Option<Duration>,
    thread_stats: Vec<AddReachableStats>,
}

impl AddReachableStats {
    fn new() -> Self {
        Self {
            potential_runs: 0,
            actual_runs: 0,
            potential_saves: 0,
            actual_saves: 0,
            elapsed: None,
            single_threaded_elapsed: None,
            thread_stats: Vec::new(),
        }
    }

    fn print(&self) {
        if let Some(elapsed) = self.elapsed {
            println!("elapsed: {:?}", elapsed,);
            println!(
                "  elapsed / potential_runs: {:?}, elapsed / actual_runs: {:?}",
                elapsed.div_f64(self.potential_runs as f64),
                elapsed.div_f64(self.actual_runs as f64),
            );
            println!(
                "  elapsed / potential_saves: {:?}, elapsed / actual_saves: {:?}",
                elapsed.div_f64(self.potential_saves as f64),
                elapsed.div_f64(self.actual_saves as f64),
            );
        }
        if let Some(single_threaded_elapsed) = self.single_threaded_elapsed {
            println!("single_threaded_elapsed: {:?}", single_threaded_elapsed);
        }
        println!(
            "potential_runs: {}, actual_runs: {}, potential_saves: {}, actual_saves: {}",
            self.potential_runs, self.actual_runs, self.potential_saves, self.actual_saves
        );
    }

    fn add(&mut self, other: &Self) {
        self.potential_runs += other.potential_runs;
        self.actual_runs += other.actual_runs;
        self.potential_saves += other.potential_saves;
        self.actual_saves += other.actual_saves;
        self.elapsed = None;
    }
}

fn add_reachable_to_dst_frame_direct_serial<'a>(
    src_frame: &StateTable,
    src_pos_filter: impl Fn((i16, i16)) -> bool,
    dst_frame_keep_playing: &'a mut StateTable,
    dst_frame_freeze: &'a mut StateTable,
    solid_spikes_cache: &PlayerSolidSpikesCache,
) -> Result<(bool, AddReachableStats)> {
    let mut stats = AddReachableStats::new();

    let mut did_win = false;

    let before_update = Instant::now();
    for (src_pos, src_spd_player_flags_set) in src_frame.iter() {
        if !src_pos_filter(src_pos) {
            continue;
        }

        for (src_spd, src_compressed_player_flags) in src_spd_player_flags_set.iter() {
            let src_player_flags = src_compressed_player_flags.decompress();

            let mut inputs_without_skipped = Vec::new();
            for input in InputFlags::iter() {
                stats.potential_runs += ALL_DIFFERENT_REMS_FOR_MOVE.len();
                if !should_skip_run(&src_player_flags, &input) {
                    inputs_without_skipped.push(input);
                }
            }

            let mut post_move_groups: FxHashMap<_, Vec<_>> = FxHashMap::default();
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
                    &solid_spikes_cache,
                )?;
                assert!(
                    post_move_rem.x >= MIN_REM
                        && post_move_rem.x <= MAX_REM
                        && post_move_rem.y >= MIN_REM
                        && post_move_rem.y <= MAX_REM
                );

                let post_move_pos_spd_flags = post_move_pos_spd.flags(&solid_spikes_cache)?;
                post_move_groups
                    .entry((post_move_pos_spd_flags, post_move_pos_spd.spd))
                    .or_default()
                    .push(post_move_pos_spd.pos);
            }

            for ((post_move_pos_spd_flags, post_move_spd), post_move_pos_vec) in post_move_groups {
                for input in &inputs_without_skipped {
                    stats.actual_runs += 1;

                    let update_result = run_player_update(
                        src_player_flags.clone(),
                        &post_move_pos_spd_flags,
                        &input,
                        post_move_spd.clone(),
                    );

                    if let PlayerUpdateResult::Win = update_result {
                        did_win = true;
                    }

                    if let PlayerUpdateResult::KeepPlaying {
                        freeze,
                        player_flags: mut dst_player_flags,
                        spd: post_update_spd,
                    } = update_result
                    {
                        stats.potential_saves += post_move_pos_vec.len();
                        if should_skip_save(&dst_player_flags) {
                            continue;
                        }

                        dst_player_flags.adjust_before_compress();
                        let dst_compressed_player_flags = dst_player_flags
                            .compress()
                            .ok_or(anyhow!("could not compress dst_player_flags"))?;

                        let dst_frame = if freeze {
                            &mut *dst_frame_freeze
                        } else {
                            &mut *dst_frame_keep_playing
                        };

                        for post_move_pos in &post_move_pos_vec {
                            stats.actual_saves += 1;

                            let PlayerDrawResult {
                                pos: dst_pos,
                                spd: dst_spd,
                            } = run_player_draw(post_move_pos.clone(), post_update_spd.clone());

                            let dst_spd_player_flags_set = dst_frame.get_mut_or_insert_with(
                                (dst_pos.x.as_i16_or_err()?, dst_pos.y.as_i16_or_err()?),
                                FxHashSet::default,
                            );
                            dst_spd_player_flags_set
                                .insert((dst_spd.clone(), dst_compressed_player_flags));
                        }
                    }
                }
            }
        }
    }
    stats.elapsed = Some(before_update.elapsed());

    Ok((did_win, stats))
}

fn add_reachable_to_dst_frame_direct_parallel<'a>(
    src_frame: Arc<StateTable>,
    dst_frame_keep_playing: &mut StateTable,
    dst_frame_freeze: &mut StateTable,
    solid_spikes_cache: Arc<PlayerSolidSpikesCache>,
) -> Result<(bool, AddReachableStats)> {
    let threads = 4;
    let local_queue_size = 8;
    let chunk_size = 4;

    let queue: Queue<PosMapRange> = Queue::new(threads, local_queue_size);

    for range in src_frame.range().chunks(chunk_size) {
        queue.push(range);
    }

    let before_spawn = Instant::now();
    let handles: Vec<_> = queue
        .local_queues()
        .map(|mut local_queue| {
            let src_frame = src_frame.clone();
            let solid_spikes_cache = solid_spikes_cache.clone();

            let handle = std::thread::spawn(
                move || -> Result<(StateTable, StateTable, bool, AddReachableStats)> {
                    let mut local_dst_frame_keep_playing = StateTable::new();
                    let mut local_dst_frame_freeze = StateTable::new();

                    let mut did_win = false;
                    let mut stats = AddReachableStats::new();

                    while let Some(range) = local_queue.pop() {
                        let (chunk_did_win, chunk_stats) =
                            add_reachable_to_dst_frame_direct_serial(
                                &src_frame,
                                |p| range.contains_pos(p),
                                &mut local_dst_frame_keep_playing,
                                &mut local_dst_frame_freeze,
                                &solid_spikes_cache,
                            )?;
                        did_win = did_win || chunk_did_win;
                        stats.add(&chunk_stats);
                    }

                    Ok((
                        local_dst_frame_keep_playing,
                        local_dst_frame_freeze,
                        did_win,
                        stats,
                    ))
                },
            );

            handle
        })
        .collect();

    let mut local_results = Vec::new();
    for handle in handles {
        local_results.push(handle.join().unwrap()?);
    }

    let mut did_win = false;
    let mut stats = AddReachableStats::new();
    let before_join = Instant::now();
    for (local_dst_frame_keep_playing, local_dst_frame_freeze, local_did_win, local_stats) in
        local_results
    {
        did_win = did_win || local_did_win;
        stats.add(&local_stats);
        stats.thread_stats.push(local_stats.clone());

        dst_frame_keep_playing.extend(local_dst_frame_keep_playing);
        dst_frame_freeze.extend(local_dst_frame_freeze);
    }

    stats.elapsed = Some(before_spawn.elapsed());
    stats.single_threaded_elapsed = Some(before_join.elapsed());

    Ok((did_win, stats))
}

fn save_debug_image(frame: &StateTable, frame_i: usize) -> Result<()> {
    let range = frame.range();

    let data: Vec<_> = range
        .clone()
        .into_positions()
        .map(|pos| {
            if let Some(stuff) = frame.get(pos) {
                stuff.len()
            } else {
                0
            }
        })
        .collect();
    let data_max = *data.iter().max().unwrap();

    let debug_img = ImageBuffer::<Luma<u8>, Vec<u8>>::from_raw(
        (range.max_x - range.min_x + 1) as u32,
        (range.max_y - range.min_y + 1) as u32,
        data.into_iter()
            .map(|v| (v as f32) / (data_max as f32) * 0.5 + if v == 0 { 0.0 } else { 0.5 })
            .map(|v| (v * 255.0) as u8)
            .collect(),
    )
    .ok_or(anyhow!("size mismatch"))?;
    debug_img.save(format!("output/frame-{}.png", frame_i))?;
    Ok(())
}

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;
    let room = Room::from_position(&cart_data, int(1), int(0))?;

    let solid_spikes_cache = Arc::new(PlayerSolidSpikesCache::calculate(&room));

    let mut frames: Vec<Option<Arc<StateTable>>> = (0..1 + FREEZE_FRAME_COUNT)
        .map(|_| Some(Arc::new(StateTable::new())))
        .collect();
    set_initial_state(
        &mut Arc::get_mut(frames[0].as_mut().unwrap()).unwrap(),
        &room,
    )?;

    for i in 0..1000 {
        println!("i {}", i);

        if i > 0 {
            frames[i - 1] = None;
        }

        frames.push(Some(Arc::new(StateTable::new())));

        let future_frames = &mut frames[i..];
        let (src_frame, future_frames) = future_frames.split_first_mut().unwrap();
        let (dst_frame_keep_playing, future_frames) = future_frames.split_first_mut().unwrap();
        let dst_frame_freeze = &mut future_frames[FREEZE_FRAME_COUNT - 1];

        let src_frame = src_frame.as_ref().unwrap();
        let dst_frame_keep_playing = dst_frame_keep_playing.as_mut().unwrap();
        let dst_frame_freeze = dst_frame_freeze.as_mut().unwrap();

        let (did_win, add_reachable_stats) = add_reachable_to_dst_frame_direct_parallel(
            Arc::clone(src_frame),
            Arc::get_mut(dst_frame_keep_playing).unwrap(),
            Arc::get_mut(dst_frame_freeze).unwrap(),
            Arc::clone(&solid_spikes_cache),
        )?;

        add_reachable_stats.print();
        for (i, local_stats) in add_reachable_stats.thread_stats.iter().enumerate() {
            print!("  thread {}: ", i);
            local_stats.print();
        }
        println!("did_win: {:?}", did_win);
        println!("src_frame stats:");
        src_frame.print_stats();
        println!("dst_frame_keep_playing stats:");
        dst_frame_keep_playing.print_stats();
        println!("dst_frame_freeze stats:");
        dst_frame_freeze.print_stats();

        save_debug_image(&src_frame, i)?;

        println!();
    }

    println!("done");

    Ok(())
}
