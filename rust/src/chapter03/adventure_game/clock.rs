use crate::chapter03::adventure_game::objects::thing::is_thing;
use crate::chapter03::adventure_game::{dynamic_type::Obj, generic_procedures::CLOCK_TICK};
use crate::chapter03::generic_procedures::GenericResult;
use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct Clock {
    current_time: u64,
    things: Vec<Obj>,
}

impl Clock {
    pub fn new() -> Self {
        Clock {
            current_time: 0,
            things: vec![],
        }
    }

    pub fn tick(&mut self) -> GenericResult {
        self.current_time += 1;
        for thing in &self.things {
            CLOCK_TICK(&[&**thing])?;
        }
        Ok(None)
    }

    pub fn register(&mut self, thing: Obj) {
        self.things.push(thing);
    }
}
