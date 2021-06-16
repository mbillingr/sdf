//! Allright!
//! Generic procedures are kinda weird to implement in Rust, but the result is somewhat OK-ish.
//! Using the type system to implement a hierarchy of structures is painful. Maybe implementing
//! these types as runtime entities would have been a better approach.
//! I won't pursue this further in Rust.
//! Implementing multiple dispatch and property types in Python could be useful. I'll try that next.
//! I wonder how all this would fare in Julia with its first-class multiple-dispatch...

use crate::chapter03::adventure_game::objects::mobile_thing::is_mobile_thing;
use crate::chapter03::adventure_game::objects::place::is_place;
use dynamic_type::Obj;
use objects::{exit, place};
use rand::{thread_rng, Rng};

mod dynamic_type;
mod generic_procedures;
mod objects;
mod property_table;

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

fn random_bias(x: i64) -> f64 {
    1.0 / thread_rng().gen_range(1..=x) as f64
}

fn connect(place1: &Obj, d1: Direction, d2: Direction, place2: &Obj) {
    println!("{:?}", place1);
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    let exit21 = exit::make_exit(place2.clone(), place1.clone(), d2);
    place::add_exit(place1, exit12);
    place::add_exit(place2, exit21);
}

fn connect_one_way(place1: &Obj, d1: Direction, place2: &Obj) {
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    place::add_exit(place1, exit12);
}

fn move_internal(mobile_thing: &Obj, from: &Obj, to: &Obj) {
    assert!(is_mobile_thing(&**mobile_thing));
    assert!(is_place(&**from));
    assert!(is_place(&**to));
    /*leave_place(mobile_thing);
    remove_thing(from, mobile_thing);
    mobile_thing.set_property("location", to.clone());
    add_thing(to, mobile_thing);
    enter_place(mobile_thing);*/
    unimplemented!()
}

#[test]
pub fn main() {
    use Direction::*;

    objects::install_generic_procedure_handlers();

    let lobby = place::make_place("Lobby");
    let restroom = place::make_place("Restroom");
    let infinite_corridor = place::make_place("Infinite Corridor");

    connect(&lobby, East, West, &restroom);
    connect_one_way(&lobby, West, &infinite_corridor);
    connect(&infinite_corridor, North, South, &infinite_corridor);

    //let generic_procedure = make_generic_procedure_constructor(SimpleDispatchStore::new);

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
