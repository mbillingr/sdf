use std::cell::RefCell;
use std::collections::HashMap;

use crate::chapter03::adventure_game::dynamic_type;
use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::DebugAny;
use std::fmt::Debug;

pub trait Properties {
    fn set_property(&self, key: &'static str, value: Obj);

    fn set_raw_property<T: 'static + std::fmt::Debug>(&self, key: &'static str, value: T) {
        self.set_property(key, dynamic_type::obj(value))
    }

    fn get_property(&self, key: &'static str) -> Option<Obj>;

    fn has_property(&self, key: &'static str) -> bool {
        self.get_property(key).is_some()
    }
}

impl Properties for dyn DebugAny {
    fn set_property(&self, key: &'static str, value: Obj) {
        dynamic_type::as_table(self)
            .expect("not a table")
            .set_property(key, value)
    }

    fn get_property(&self, key: &'static str) -> Option<Obj> {
        dynamic_type::as_table(self)
            .expect("not a table")
            .get_property(key)
    }
}

#[derive(Debug, Default)]
pub struct Table {
    properties: RefCell<HashMap<&'static str, Obj>>,
}

impl Table {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Properties for Table {
    fn set_property(&self, key: &'static str, value: Obj) {
        self.properties.borrow_mut().insert(key, value);
    }

    fn get_property(&self, key: &'static str) -> Option<Obj> {
        self.properties.borrow().get(key).cloned()
    }
}
