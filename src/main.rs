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
use player_flags::{CompressedPlayerFlags, PlayerFlags};
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

use crate::{
    game::{
        run_move_concrete, run_player_draw, run_player_update, InputFlags, PlayerDrawResult,
        PlayerPosSpd, PlayerSolidSpikesCache, PlayerUpdateResult, ALL_DIFFERENT_REMS_FOR_MOVE,
        FREEZE_FRAME_COUNT, MAX_REM, MIN_REM,
    },
    pico8_num::Pico8Num,
};

fn set_initial_state(frame: &mut StateTable, room: &Room) -> Result<()> {
    let pos = room.spawn_pos();
    let spd = Pico8Vec2::zero();

    let mut player_flags = PlayerFlags::spawn(int(1));
    player_flags.adjust_before_compress();
    let compressed_player_flags = player_flags
        .compress()
        .ok_or(anyhow!("failed to compress initial player_flags"))?;

    let spd_player_flag_set = frame.get_mut_or_insert_with(
        (pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?),
        FxHashSet::default,
    );
    spd_player_flag_set.insert((spd, compressed_player_flags));

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

fn mark_in_frame(
    frame: &mut StateTable,
    pos: &Pico8Vec2,
    spd: Pico8Vec2,
    player_flags: CompressedPlayerFlags,
) -> Result<()> {
    let player_flags_set = frame.get_mut_or_insert_with(
        (pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?),
        FxHashSet::default,
    );
    player_flags_set.insert((spd.clone(), player_flags));
    Ok(())
}

fn has_in_frame(
    frame: &StateTable,
    pos: &Pico8Vec2,
    spd: Pico8Vec2,
    player_flags: CompressedPlayerFlags,
) -> Result<bool> {
    if let Some(player_flags_set) = frame.get((pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?)) {
        Ok(player_flags_set.contains(&(spd.clone(), player_flags)))
    } else {
        Ok(false)
    }
}

fn add_reachable_to_dst_frame_direct_serial<'a>(
    src_frame: &StateTable,
    src_frame_mark_exists: Option<&mut StateTable>,
    src_frame_mark_win: Option<&mut StateTable>,
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

                    if matches!(update_result, PlayerUpdateResult::Win)
                        || post_move_pos_vec
                            .iter()
                            .any(|pos| is_extra_win_state(pos.as_i16s_or_err().unwrap()))
                    {
                        did_win = true;
                        if let Some(&mut ref mut src_frame_mark_win) = src_frame_mark_win {
                            mark_in_frame(
                                src_frame_mark_win,
                                &Pico8Vec2::from_i16s(src_pos.0, src_pos.1),
                                src_spd.clone(),
                                src_compressed_player_flags.clone(),
                            )?;
                        }
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

                            if let Some(&mut ref mut src_frame_mark_exists) = src_frame_mark_exists
                            {
                                if has_in_frame(
                                    dst_frame,
                                    &dst_pos,
                                    dst_spd,
                                    dst_compressed_player_flags,
                                )? {
                                    mark_in_frame(
                                        &mut *src_frame_mark_exists,
                                        &Pico8Vec2::from_i16s(src_pos.0, src_pos.1),
                                        src_spd.clone(),
                                        src_compressed_player_flags.clone(),
                                    )?;
                                }
                            } else {
                                mark_in_frame(
                                    dst_frame,
                                    &dst_pos,
                                    dst_spd,
                                    dst_compressed_player_flags,
                                )?;
                            }
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
    src_frame_mark_exists: Option<&mut StateTable>,
    src_frame_mark_win: Option<&mut StateTable>,
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

    let src_frame_mark_exists_is_some = src_frame_mark_exists.is_some();
    let src_frame_mark_win_is_some = src_frame_mark_win.is_some();

    let before_spawn = Instant::now();
    let handles: Vec<_> = queue
        .local_queues()
        .map(|mut local_queue| {
            let src_frame = src_frame.clone();
            let solid_spikes_cache = solid_spikes_cache.clone();

            let mut local_src_frame_mark_exists = if src_frame_mark_exists_is_some { Some(StateTable::new()) } else { None };
            let mut local_src_frame_mark_win = if src_frame_mark_win_is_some { Some(StateTable::new()) } else { None };
            let mut local_dst_frame_keep_playing = if src_frame_mark_exists_is_some { dst_frame_keep_playing.clone() } else { StateTable::new() };
            let mut local_dst_frame_freeze = if src_frame_mark_exists_is_some { dst_frame_freeze.clone() } else { StateTable::new() };

            let handle = std::thread::spawn(
                move || -> Result<(Option<StateTable>, Option<StateTable>, StateTable, StateTable, bool, AddReachableStats)> {
                    let mut did_win = false;
                    let mut stats = AddReachableStats::new();

                    while let Some(range) = local_queue.pop() {
                        let (chunk_did_win, chunk_stats) =
                            add_reachable_to_dst_frame_direct_serial(
                                &src_frame,
                                local_src_frame_mark_exists.as_mut(),
                                local_src_frame_mark_win.as_mut(),
                                |p| range.contains_pos(p),
                                &mut local_dst_frame_keep_playing,
                                &mut local_dst_frame_freeze,
                                &solid_spikes_cache,
                            )?;
                        did_win = did_win || chunk_did_win;
                        stats.add(&chunk_stats);
                    }

                    Ok((
                        local_src_frame_mark_exists,
                        local_src_frame_mark_win,
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
    for (
        local_src_frame_mark_exists,
        local_src_frame_mark_win,
        local_dst_frame_keep_playing,
        local_dst_frame_freeze,
        local_did_win,
        local_stats,
    ) in local_results
    {
        did_win = did_win || local_did_win;
        stats.add(&local_stats);
        stats.thread_stats.push(local_stats.clone());

        if let Some(&mut ref mut src_frame_mark_exists) = src_frame_mark_exists {
            src_frame_mark_exists.extend(local_src_frame_mark_exists.unwrap());
        }
        if let Some(&mut ref mut src_frame_mark_win) = src_frame_mark_win {
            src_frame_mark_win.extend(local_src_frame_mark_win.unwrap());
        }
        dst_frame_keep_playing.extend(local_dst_frame_keep_playing);
        dst_frame_freeze.extend(local_dst_frame_freeze);
    }

    stats.elapsed = Some(before_spawn.elapsed());
    stats.single_threaded_elapsed = Some(before_join.elapsed());

    Ok((did_win, stats))
}

fn save_debug_image(frame: &StateTable, name: &str) -> Result<()> {
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
    debug_img.save(format!("output/{}.png", name))?;
    Ok(())
}

fn is_extra_win_state(pos: (i16, i16)) -> bool {
    false
}

fn guided_brute_force(
    room: &Room,
    solid_spikes_cache: &PlayerSolidSpikesCache,
    frames: &Vec<StateTable>,
) -> Result<Option<Vec<InputFlags>>> {
    #[derive(Clone, Hash, PartialEq, Eq)]
    struct State {
        player_pos_spd: PlayerPosSpd,
        player_flags: PlayerFlags,
        rem: Pico8Vec2,
        freeze: Pico8Num,
    }

    let update = |s: &mut State, input: &InputFlags| -> Result<bool> {
        if s.freeze > int(0) {
            s.freeze = s.freeze - int(1);
            return Ok(false);
        }

        if s.player_pos_spd.spd.x != int(0) || s.player_pos_spd.spd.y != int(0) {
            let move_result =
                run_move_concrete(s.player_pos_spd.clone(), s.rem.clone(), &solid_spikes_cache)?;
            s.player_pos_spd = move_result.0;
            s.rem = move_result.1;
        }

        let player_update_result = run_player_update(
            s.player_flags.clone(),
            &s.player_pos_spd.flags(&solid_spikes_cache)?,
            input,
            s.player_pos_spd.spd.clone(),
        );
        match player_update_result {
            PlayerUpdateResult::Die => panic!("unexpected death"),
            PlayerUpdateResult::KeepPlaying {
                freeze: new_freeze,
                player_flags: new_player_flags,
                spd: new_spd,
            } => {
                s.freeze = int(if new_freeze {
                    FREEZE_FRAME_COUNT as i16
                } else {
                    0
                });
                s.player_flags = new_player_flags;
                s.player_pos_spd.spd = new_spd;

                Ok(false)
            }
            PlayerUpdateResult::Win => Ok(true),
        }
    };

    let draw = |s: &mut State| {
        if s.freeze > int(0) {
            return;
        }

        let draw_result =
            run_player_draw(s.player_pos_spd.pos.clone(), s.player_pos_spd.spd.clone());
        s.player_pos_spd.pos = draw_result.pos;
        s.player_pos_spd.spd = draw_result.spd;
    };

    let mut states = Vec::new();
    let mut inputs: Vec<InputFlags> = Vec::new();
    let mut queue: Vec<(usize, Option<InputFlags>, State)> = vec![(
        0,
        None,
        State {
            player_pos_spd: PlayerPosSpd {
                pos: room.spawn_pos(),
                spd: Pico8Vec2::zero(),
            },
            player_flags: PlayerFlags::spawn(int(1)),
            rem: Pico8Vec2::zero(),
            freeze: int(0),
        },
    )];

    let mut highest_frame_i = 0;

    while let Some((frame_i, last_input, last_state)) = queue.pop() {
        while inputs.len() + if last_input.is_some() { 1 } else { 0 } > frame_i {
            states.pop().unwrap();
            inputs.pop().unwrap();
        }
        if let Some(input) = last_input {
            inputs.push(input);
        }
        states.push(last_state.clone());
        assert!(inputs.len() == frame_i);
        assert!(inputs.len() + 1 == states.len());

        if frame_i > highest_frame_i {
            println!("brute force reached frame {}", highest_frame_i);
            highest_frame_i = frame_i;
        }

        if last_state.freeze.as_i16_or_err()? > 0 {
            queue.push((
                frame_i + 1,
                Some(InputFlags::none()),
                State {
                    freeze: last_state.freeze - int(1),
                    ..last_state
                },
            ));
            continue;
        }

        let mut next_states = FxHashSet::default();
        for input in InputFlags::iter() {
            if should_skip_run(&last_state.player_flags, &input) {
                continue;
            }

            let mut next_state = last_state.clone();
            let did_win = update(&mut next_state, &input)?;
            if did_win || is_extra_win_state(next_state.player_pos_spd.pos.as_i16s_or_err()?) {
                inputs.push(input);
                return Ok(Some(inputs));
            }
            draw(&mut next_state);

            if should_skip_save(&next_state.player_flags) {
                continue;
            }

            let mut player_flags_for_lookup = next_state.player_flags.clone();
            player_flags_for_lookup.adjust_before_compress();
            if !has_in_frame(
                &frames[frame_i + 1 + (next_state.freeze.as_i16_or_err()? as usize)],
                &next_state.player_pos_spd.pos,
                next_state.player_pos_spd.spd.clone(),
                player_flags_for_lookup.compress().unwrap(),
            )? {
                continue;
            }

            let is_new = next_states.insert(next_state.clone());
            if is_new {
                queue.push((frame_i + 1, Some(input), next_state))
            }
        }
    }

    Ok(None)
}

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;
    let room = Room::from_position(&cart_data, int(1), int(0))?;

    let solid_spikes_cache = Arc::new(PlayerSolidSpikesCache::calculate(&room));

    let mut forward_frames: Vec<Option<Arc<StateTable>>> = (0..=FREEZE_FRAME_COUNT)
        .map(|_| Some(Arc::new(StateTable::new())))
        .collect();
    let mut forward_frames_new: Vec<Option<Arc<StateTable>>> = (0..=FREEZE_FRAME_COUNT)
        .map(|_| Some(Arc::new(StateTable::new())))
        .collect();
    for frames in [&mut forward_frames, &mut forward_frames_new] {
        set_initial_state(
            &mut Arc::get_mut(frames[0].as_mut().unwrap()).unwrap(),
            &room,
        )?;
    }

    let fast_forward_pass = room.is_world_still();
    let mut seen_states = StateTable::new();
    let mut seen_win_states = StateTable::new();

    for forward_i in 0..1000 {
        let frame_name = format!("forward-{}", forward_i);
        println!("{}", frame_name);

        // HACK You need a lot of RAM or swap to run this without deleting frames from memory.
        // if forward_i > 0 {
        //     forward_frames[forward_i - 1] = None;
        // }
        if forward_i > 0 {
            forward_frames_new[forward_i - 1] = None;
        }

        forward_frames.push(Some(Arc::new(StateTable::new())));
        forward_frames_new.push(Some(Arc::new(StateTable::new())));

        let mut src_frame_mark_win = StateTable::new();
        {
            let future_frames = &mut forward_frames[forward_i..];
            let (src_frame, future_frames) = future_frames.split_first_mut().unwrap();
            let (dst_frame_keep_playing, future_frames) = future_frames.split_first_mut().unwrap();
            let dst_frame_freeze = &mut future_frames[FREEZE_FRAME_COUNT - 1];

            let src_frame = src_frame.as_ref().unwrap();
            let dst_frame_keep_playing = dst_frame_keep_playing.as_mut().unwrap();
            let dst_frame_freeze = dst_frame_freeze.as_mut().unwrap();

            let future_frames = &mut forward_frames_new[forward_i..];
            let (src_frame_new, future_frames) = future_frames.split_first_mut().unwrap();
            let (dst_frame_keep_playing_new, future_frames) =
                future_frames.split_first_mut().unwrap();
            let dst_frame_freeze_new = &mut future_frames[FREEZE_FRAME_COUNT - 1];

            let src_frame_new = src_frame_new.as_ref().unwrap();
            let dst_frame_keep_playing_new = dst_frame_keep_playing_new.as_mut().unwrap();
            let dst_frame_freeze_new = dst_frame_freeze_new.as_mut().unwrap();

            let (did_win, add_reachable_stats) = add_reachable_to_dst_frame_direct_parallel(
                Arc::clone(if fast_forward_pass {
                    src_frame_new
                } else {
                    src_frame
                }),
                None,
                Some(&mut src_frame_mark_win),
                Arc::get_mut(dst_frame_keep_playing).unwrap(),
                Arc::get_mut(dst_frame_freeze).unwrap(),
                Arc::clone(&solid_spikes_cache),
            )?;
            assert!(src_frame_mark_win.is_empty() != did_win);

            add_reachable_stats.print();
            for (i, local_stats) in add_reachable_stats.thread_stats.iter().enumerate() {
                print!("  thread {}: ", i);
                local_stats.print();
            }
            println!("did_win: {:?}", did_win);

            let before_fast_forward_pass_extras = Instant::now();
            if fast_forward_pass {
                seen_states.extend(src_frame_new.as_ref().clone());
                seen_win_states.extend(src_frame_mark_win.clone());
                src_frame_mark_win = seen_win_states.clone();

                for (full_frame, new_frame) in [
                    (
                        Arc::get_mut(dst_frame_keep_playing).unwrap(),
                        Arc::get_mut(dst_frame_keep_playing_new).unwrap(),
                    ),
                    (
                        Arc::get_mut(dst_frame_freeze).unwrap(),
                        Arc::get_mut(dst_frame_freeze_new).unwrap(),
                    ),
                ] {
                    for (pos, spd_player_flag_set) in full_frame.iter() {
                        for (spd, compressed_player_flags) in spd_player_flag_set {
                            if !has_in_frame(
                                &seen_states,
                                &Pico8Vec2::from_i16s(pos.0, pos.1),
                                spd.clone(),
                                *compressed_player_flags,
                            )? {
                                mark_in_frame(
                                    new_frame,
                                    &Pico8Vec2::from_i16s(pos.0, pos.1),
                                    spd.clone(),
                                    *compressed_player_flags,
                                )?;
                            }
                        }
                    }

                    full_frame.extend(seen_states.clone());
                }
            }
            let fast_forward_pass_extras_elapsed = before_fast_forward_pass_extras.elapsed();

            println!(
                "fast_forward_pass_extras_elapsed: {:?}",
                fast_forward_pass_extras_elapsed
            );
            println!("src_frame_new stats:");
            src_frame_new.print_stats();
            println!("dst_frame_keep_playing_new stats:");
            dst_frame_keep_playing_new.print_stats();
            println!("dst_frame_freeze_new stats:");
            dst_frame_freeze_new.print_stats();
            save_debug_image(&src_frame_new, &format!("{} src_frame_new", frame_name))?;
            println!("src_frame stats:");
            src_frame.print_stats();
            println!("dst_frame_keep_playing stats:");
            dst_frame_keep_playing.print_stats();
            println!("dst_frame_freeze stats:");
            dst_frame_freeze.print_stats();
            save_debug_image(&src_frame, &format!("{} src_frame", frame_name))?;
            println!();
        }

        if src_frame_mark_win.is_empty() {
            continue;
        }

        {
            let mut backward_frames: Vec<_> = (0..=forward_i + 1 + FREEZE_FRAME_COUNT)
                .map(|_| StateTable::new())
                .collect();
            backward_frames[forward_i] = src_frame_mark_win;

            for backward_i in (0..forward_i).rev() {
                let frame_name = format!("forward-{} backward-{}", forward_i, backward_i);
                println!("{}", frame_name);

                let src_frame = forward_frames[backward_i].as_ref().unwrap();

                let future_frames = &mut backward_frames[backward_i..];
                let (src_frame_mark_exists, future_frames) =
                    future_frames.split_first_mut().unwrap();
                let (dst_frame_keep_playing, future_frames) =
                    future_frames.split_first_mut().unwrap();
                let dst_frame_freeze = &mut future_frames[FREEZE_FRAME_COUNT - 1];

                let (_, add_reachable_stats) = add_reachable_to_dst_frame_direct_parallel(
                    Arc::clone(src_frame),
                    Some(src_frame_mark_exists),
                    None,
                    dst_frame_keep_playing,
                    dst_frame_freeze,
                    Arc::clone(&solid_spikes_cache),
                )?;

                add_reachable_stats.print();
                for (i, local_stats) in add_reachable_stats.thread_stats.iter().enumerate() {
                    print!("  thread {}: ", i);
                    local_stats.print();
                }
                println!("src_frame stats:");
                src_frame.print_stats();
                println!("src_frame_mark_exists stats:");
                src_frame_mark_exists.print_stats();
                println!("dst_frame_keep_playing stats:");
                dst_frame_keep_playing.print_stats();
                println!("dst_frame_freeze stats:");
                dst_frame_freeze.print_stats();
                println!();

                save_debug_image(&src_frame_mark_exists, &frame_name)?;
            }

            assert!(!backward_frames[0].is_empty());

            if let Some(winning_inputs) =
                guided_brute_force(&room, &solid_spikes_cache, &backward_frames)?
            {
                println!("won in frame {}!", forward_i);
                println!(
                    "winning_inputs: {}",
                    winning_inputs
                        .iter()
                        .map(|input| input.to_tas_keycode().to_string() + ",")
                        .collect::<String>()
                );
                return Ok(());
            } else {
                println!("wining in frame {} is not possible", forward_i);
                println!();
            }
        }
    }

    Ok(())
}
