//! Allright!
//! Generic procedures are kinda weird to implement in Rust, but the result is somewhat OK-ish.
//! Using the type system to implement a hierarchy of structures is painful. Maybe implementing
//! these types as runtime entities would have been a better approach.
//! I won't pursue this further in Rust.
//! Implementing multiple dispatch and property types in Python could be useful. I'll try that next.
//! I wonder how all this would fare in Julia with its first-class multiple-dispatch...

use crate::chapter03::adventure_game::objects::avatar::{look_around, make_avatar};
use crate::chapter03::adventure_game::objects::mobile_thing::is_mobile_thing;
use crate::chapter03::adventure_game::objects::place::is_place;
use crate::chapter03::adventure_game::objects::screen::make_screen;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::GenericResult;
use dynamic_type::Obj;
use objects::{exit, place};
use rand::{thread_rng, Rng};

pub mod dynamic_type;
pub mod generic_procedures;
pub mod objects;
pub mod property_table;

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

pub fn random_bias(x: i64) -> f64 {
    1.0 / thread_rng().gen_range(1..=x) as f64
}

pub fn connect(place1: &Obj, d1: Direction, d2: Direction, place2: &Obj) {
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    let exit21 = exit::make_exit(place2.clone(), place1.clone(), d2);
    place::add_exit(place1, exit12);
    place::add_exit(place2, exit21);
}

pub fn connect_one_way(place1: &Obj, d1: Direction, place2: &Obj) {
    assert!(place::is_place(&**place1));
    assert!(place::is_place(&**place2));
    let exit12 = exit::make_exit(place1.clone(), place2.clone(), d1);
    place::add_exit(place1, exit12);
}

pub fn move_internal(mobile_thing: &Obj, from: &Obj, to: &Obj) {
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

pub struct AdventureGame {
    my_avatar: Obj,
}

impl AdventureGame {
    pub fn new(my_name: &str) -> Self {
        let all_places = create_world();
        let start_location = all_places
            .iter()
            .find(|place| {
                let p = place.get_property("name").unwrap();
                p.downcast_ref::<String>().unwrap() == "Lobby"
            })
            .cloned()
            .unwrap();
        let screen = make_screen();
        Self {
            my_avatar: make_avatar(my_name, start_location, screen),
        }
    }

    pub fn repl(&self) -> GenericResult {
        self.whats_here()?;
        unimplemented!()
    }

    pub fn whats_here(&self) -> GenericResult {
        look_around(&self.my_avatar)
    }
}

fn create_world() -> Vec<Obj> {
    use Direction::*;

    let lobby = place::make_place("Lobby");
    let restroom = place::make_place("Restroom");
    let infinite_corridor = place::make_place("Infinite Corridor");

    connect(&lobby, East, West, &restroom);
    connect_one_way(&lobby, West, &infinite_corridor);
    connect(&infinite_corridor, North, South, &infinite_corridor);

    vec![lobby, restroom, infinite_corridor]
}

#[test]
pub fn main() {
    use crate::chapter03::adventure_game::dynamic_type::obj;
    use crate::chapter03::adventure_game::generic_procedures::SEND_MESSAGE;
    use crate::chapter03::adventure_game::objects::place::make_place;

    objects::install_generic_procedure_handlers();

    let place = make_place("WORLD");

    let screen = make_screen();
    SEND_MESSAGE(&[&vec![obj("hello"), place], &*screen])
        .map_err(|e| e.to_string())
        .unwrap();

    let game = AdventureGame::new("Hotzenplotz");
    game.repl().map_err(|s| s.to_string()).unwrap();

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
