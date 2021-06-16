//! Allright!
//! Generic procedures are kinda weird to implement in Rust, but the result is somewhat OK-ish.
//! Using the type system to implement a hierarchy of structures is painful. Maybe implementing
//! these types as runtime entities would have been a better approach.
//! I won't pursue this further in Rust.
//! Implementing multiple dispatch and property types in Python could be useful. I'll try that next.
//! I wonder how all this would fare in Julia with its first-class multiple-dispatch...

use crate::chapter03::generic_procedures::{
    define_generic_procedure_handler, make_generic_procedure_constructor, match_args,
    SimpleDispatchStore,
};
use crate::chapter03::type_structure::{Type, TypeHierarchy};
use rand::{thread_rng, Rng};
use std::any::Any;
use std::cell::RefCell;
use std::sync::Arc;

pub type Obj = Arc<dyn Any>;
pub type Ref<'a> = &'a dyn Any;

fn obj<T: Any>(x: T) -> Obj {
    Arc::new(x)
}

pub trait Properties {
    fn set_property<T>(&self, key: &'static str, value: T);

    fn has_property(&self, key: &'static str) -> bool;

    fn mutate_property_value<T>(&self, key: &'static str, func: impl FnOnce(&mut T));
}

impl Properties for dyn Any {
    fn set_property<T>(&self, key: &'static str, value: T) {
        as_table(self)
            .expect("not a table")
            .set_property(key, value)
    }

    fn has_property(&self, key: &'static str) -> bool {
        as_table(self).expect("not a table").has_property(key)
    }

    fn mutate_property_value<T>(&self, key: &'static str, func: impl FnOnce(&mut T)) {
        as_table(self)
            .expect("not a table")
            .mutate_property_value(key, func)
    }
}

struct Table {}

impl Table {
    fn new() -> Self {
        unimplemented!()
    }
}

impl Properties for Table {
    fn set_property<T>(&self, key: &'static str, value: T) {
        unimplemented!()
    }

    fn has_property(&self, key: &'static str) -> bool {
        unimplemented!()
    }

    fn mutate_property_value<T>(&self, key: &'static str, func: impl FnOnce(&mut T)) {
        unimplemented!()
    }
}

fn as_table(obj: &dyn Any) -> Option<&Table> {
    obj.downcast_ref::<Table>()
}

fn is_object(obj: &dyn Any) -> bool {
    as_table(obj)
        .filter(|table| table.has_property("name"))
        .filter(|table| table.has_property("description"))
        .is_some()
}

fn make_object(name: impl ToString) -> Obj {
    let name = name.to_string();
    let table = Table::new();
    table.set_property("name", name.clone());
    table.set_property("description", name);
    return obj(table);
}

fn is_container(obj: &dyn Any) -> bool {
    is_object(obj) && obj.has_property("things")
}

fn make_container(name: impl ToString) -> Obj {
    let obj = make_object(name);
    as_table(&obj)
        .unwrap()
        .set_property("things", Vec::<Obj>::new());
    return obj;
}

fn is_thing(obj: &dyn Any) -> bool {
    is_object(obj)
        && as_table(obj)
            .filter(|table| table.has_property("location"))
            .is_some()
}

fn make_thing(name: impl ToString, location: Obj) -> Obj {
    let obj = make_object(name);
    as_table(&obj).unwrap().set_property("location", location);
    return obj;
}

fn is_exit(obj: &dyn Any) -> bool {
    is_object(obj)
        && as_table(obj)
            .filter(|table| table.has_property("from"))
            .filter(|table| table.has_property("to"))
            .filter(|table| table.has_property("direction"))
            .is_some()
}

fn make_exit(from: Obj, to: Obj, direction: Direction) -> Obj {
    let obj = make_object(String::new());
    as_table(&obj).unwrap().set_property("from", from);
    as_table(&obj).unwrap().set_property("to", to);
    as_table(&obj).unwrap().set_property("direction", direction);
    return obj;
}

fn is_place(obj: &dyn Any) -> bool {
    is_container(obj)
        && as_table(obj)
            .filter(|table| table.has_property("vistas"))
            .filter(|table| table.has_property("exits"))
            .is_some()
}

fn make_place(name: impl ToString) -> Obj {
    let obj = make_container(name);
    obj.set_property("vistas", Vec::<Obj>::new());
    obj.set_property("exits", Vec::<Obj>::new());
    return obj;
}

fn add_exit(place: &Obj, exit: Obj) {
    assert!(is_place(place));
    assert!(is_exit(&exit));
    place.mutate_property_value("exits", move |exits: &mut Vec<Obj>| exits.push(exit))
}

fn is_mobile_thing(obj: &dyn Any) -> bool {
    is_thing(obj)
        && as_table(obj)
            .filter(|table| table.has_property("origin"))
            .is_some()
}

fn make_mobile_thing(name: impl ToString, location: Obj) -> Obj {
    let obj = make_thing(name, location.clone());
    as_table(&obj).unwrap().set_property("origin", location);
    return obj;
}

fn is_person(obj: &dyn Any) -> bool {
    is_mobile_thing(obj)
        && as_table(obj)
            .filter(|table| table.has_property("health"))
            .filter(|table| table.has_property("bag"))
            .is_some()
}

fn make_person(name: impl ToString, location: Obj) -> Obj {
    let person = make_mobile_thing(name, location.clone());
    as_table(&person).unwrap().set_property("health", 3);
    as_table(&person)
        .unwrap()
        .set_property("bag", make_bag("my bag".to_string(), obj(false)));
    return person;
}

fn is_bag(obj: &dyn Any) -> bool {
    is_container(obj)
        && as_table(obj)
            .filter(|table| table.has_property("holder"))
            .is_some()
}

fn make_bag(name: impl ToString, holder: Obj) -> Obj {
    let obj = make_container(name);
    as_table(&obj).unwrap().set_property("holder", holder);
    return obj;
}

#[derive(Debug, Copy, Clone)]
pub enum Direction {
    North,
    South,
    East,
    West,
    In,
    Out,
    Up,
    Down,
    Skew,
}

#[derive(Debug, Clone)]
pub struct Screen;

fn random_bias(x: i64) -> f64 {
    1.0 / thread_rng().gen_range(1..=x) as f64
}

fn connect(place1: &Obj, d1: Direction, d2: Direction, place2: &Obj) {
    assert!(is_place(place1));
    assert!(is_place(place2));
    let exit12 = make_exit(place1.clone(), place2.clone(), d1);
    let exit21 = make_exit(place2.clone(), place1.clone(), d2);
    add_exit(place1, exit12);
    add_exit(place2, exit21);
}

fn connect_one_way(place1: &Obj, d1: Direction, place2: &Obj) {
    assert!(is_place(place1));
    assert!(is_place(place2));
    let exit12 = make_exit(place1.clone(), place2.clone(), d1);
    add_exit(place1, exit12);
}

fn move_internal(thing: &Obj, _from: &Obj, to: Obj) {
    thing.set_property("location", to);
}

#[test]
pub fn main() {
    use Direction::*;

    let lobby = make_place("Lobby");
    let restroom = make_place("Restroom");
    let infinite_corridor = make_place("Infinite Corridor");

    connect(&lobby, East, West, &restroom);
    connect_one_way(&lobby, West, &infinite_corridor);
    connect(&infinite_corridor, North, South, &infinite_corridor);

    let generic_procedure = make_generic_procedure_constructor(SimpleDispatchStore::new);

    let generic_move = generic_procedure("generic-move", None);

    define_generic_procedure_handler(
        &generic_move,
        match_args(&[is_person, is_place, is_place, is_person]),
        |args| {
            /*let person = args[0].upcast::<Person>().unwrap();
            let from = args[1].upcast::<Place>().unwrap();
            let to = args[2].upcast::<Place>().unwrap();
            let _actor = args[3].upcast::<Person>().unwrap();

            move_internal(person.upcast::<Thing>().unwrap(), from, to);
            Ok(Arc::new(())) // argh!*/
            unimplemented!()
        },
    );

    /*let grendel: Obj<dyn Type> = obj(Troll::new(
        "Grendel",
        lobby.clone(),
        3,
        random_bias(3),
        random_bias(3),
    ));

    // try to move Grendel into the infinite corridor

    generic_move(&[&*grendel, &lobby, &infinite_corridor, &*grendel])
        .map_err(|e| e.to_string())
        .unwrap();

    assert_eq!(
        grendel
            .upcast::<Troll>()
            .unwrap()
            .location()
            .borrow()
            .name(),
        "Infinite Corridor"
    );*/
}
