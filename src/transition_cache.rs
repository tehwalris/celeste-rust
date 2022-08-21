use anyhow::Result;
use std::collections::HashMap;

use crate::{
    game::{run_player_update, InputFlags, PlayerPosSpd, PlayerUpdateResult, PosSpdFlags},
    pico8_num::Pico8Vec2,
    player_flags::{CompressedPlayerFlags, PlayerFlagBitMatrix},
    room::Room,
    state_table::Pico8Vec2Map,
    transition_cache,
};

#[derive(Hash, PartialEq, Eq)]
struct TransitionCacheKey {
    pos_spd_flags: PosSpdFlags,
    spd: Pico8Vec2,
}

pub struct Transition {
    pub keep_playing: Pico8Vec2Map<PlayerFlagBitMatrix>,
    pub freeze: Pico8Vec2Map<PlayerFlagBitMatrix>,
}

pub struct TransitionCache<'a> {
    room: &'a Room<'a>,
    matrices: HashMap<TransitionCacheKey, Transition>,
}

impl<'a> TransitionCache<'a> {
    pub fn new(room: &'a Room) -> Self {
        Self {
            room,
            matrices: HashMap::new(),
        }
    }

    fn make_key(&self, pos_spd: &PlayerPosSpd) -> Result<TransitionCacheKey> {
        Ok(TransitionCacheKey {
            pos_spd_flags: pos_spd.flags(self.room)?,
            spd: pos_spd.spd.clone(),
        })
    }

    pub fn get_or_calculate_transition(&mut self, pos_spd: &PlayerPosSpd) -> Result<&Transition> {
        // TODO HACK
        self.matrices = HashMap::new();

        let key = self.make_key(pos_spd)?;
        let transition = self
            .matrices
            .entry(key)
            // TODO ideally don't unwrap here
            .or_insert_with(|| Self::calculate_transition(pos_spd, &self.room).unwrap());
        Ok(transition)
    }

    fn calculate_transition(src_pos_spd: &PlayerPosSpd, room: &Room) -> Result<Transition> {
        let mut occupancy = 0;

        let src_pos_spd_flags = src_pos_spd.flags(room)?;

        let mut transition = Transition {
            keep_playing: Pico8Vec2Map::new(),
            freeze: Pico8Vec2Map::new(),
        };
        for src_compressed_player_flags in CompressedPlayerFlags::iter() {
            let src_player_flags = src_compressed_player_flags.decompress();
            for input_flags in InputFlags::iter() {
                let result = run_player_update(
                    src_player_flags.clone(),
                    &src_pos_spd_flags,
                    &input_flags,
                    src_pos_spd.spd.clone(),
                );
                if let PlayerUpdateResult::KeepPlaying {
                    freeze,
                    player_flags: mut dst_player_flags,
                    spd: dst_spd,
                } = result
                {
                    dst_player_flags.adjust_before_compress();
                    let dst_compressed_player_flags = dst_player_flags
                        .compress()
                        .ok_or(anyhow!("could not compress dst_player_flags"))?;
                    let dst_spd_map = if freeze {
                        &mut transition.freeze
                    } else {
                        &mut transition.keep_playing
                    };
                    let matrix =
                        dst_spd_map.get_mut_or_insert_with(dst_spd, || PlayerFlagBitMatrix::new());
                    matrix.set(
                        src_compressed_player_flags,
                        dst_compressed_player_flags,
                        true,
                    );
                    occupancy += 1;
                }
            }
        }

        println!("calculate_transition occupancy {}", occupancy);

        Ok(transition)
    }

    pub fn print_stats(&self) {
        println!(
            "transitions: {}, matrices: {}",
            self.matrices.len(),
            self.matrices
                .iter()
                .flat_map(|(_, v)| [&v.freeze, &v.keep_playing])
                .map(|v| v.len())
                .sum::<usize>()
        );
    }
}
