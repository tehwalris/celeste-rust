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

fn set_initial_state(frame: &mut StateTable, room: &Room) -> Result<()> {
    let pos = room.spawn_pos();
    let spd = Pico8Vec2::zero();

    let mut player_flags = PlayerFlags::spawn(int(1));
    player_flags.adjust_before_compress();
    let compressed_player_flags = player_flags
        .compress()
        .ok_or(anyhow!("failed to compress initial player_flags"))?;

    let spd_map = frame
        .get_mut_or_insert_with((pos.x.as_i16_or_err()?, pos.y.as_i16_or_err()?), || {
            Pico8Vec2Map::new()
        });
    let flag_vec = spd_map.get_mut_or_insert_with(spd, || PlayerFlagBitVec::new());
    flag_vec.set(compressed_player_flags, true);

    Ok(())
}

fn add_reachable_to_dst_frame(
    src_frame: &StateTable,
    dst_frame: &mut StateTable,
    room: &Room,
) -> Result<()> {
    for (src_pos, src_spd_map) in src_frame.iter() {
        for (src_spd, src_player_flags) in src_spd_map.iter() {
            // TODO freeze will probably need special handling
        }
    }

    todo!()
}

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;
    let room = Room::from_position(&cart_data, int(1), int(0))?;

    let mut src_frame: StateTable = PosMap::new();
    set_initial_state(&mut src_frame, &room)?;

    let mut dst_frame: StateTable = PosMap::new();

    add_reachable_to_dst_frame(&src_frame, &mut dst_frame, &room)?;

    println!("done");

    Ok(())
}
