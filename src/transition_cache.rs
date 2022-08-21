use anyhow::Result;
use std::collections::HashMap;

use crate::{
    game::{run_player_update, InputFlags, PlayerPosSpd, PlayerUpdateResult, PosSpdFlags},
    pico8_num::Pico8Vec2,
    player_flags::{CompressedPlayerFlags, PlayerFlagBitMatrix},
    room::Room,
};

#[derive(Hash, PartialEq, Eq)]
struct TransitionCacheKey {
    pos_spd_flags: PosSpdFlags,
    spd: Pico8Vec2,
}

pub struct Transition {
    pub keep_playing: PlayerFlagBitMatrix,
    pub freeze: PlayerFlagBitMatrix,
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
        let key = self.make_key(pos_spd)?;
        let transition = self
            .matrices
            .entry(key)
            // TODO ideally don't unwrap here
            .or_insert_with(|| Self::calculate_transition(pos_spd, &self.room).unwrap());
        Ok(transition)
    }

    fn calculate_transition(src_pos_spd: &PlayerPosSpd, room: &Room) -> Result<Transition> {
        let src_pos_spd_flags = src_pos_spd.flags(room)?;

        let transition = Transition {
            keep_playing: PlayerFlagBitMatrix::new(),
            freeze: PlayerFlagBitMatrix::new(),
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
                    player_flags: dst_player_flags,
                    spd: dst_spd,
                } = result
                {}
            }
        }

        Ok(transition)
    }
}
