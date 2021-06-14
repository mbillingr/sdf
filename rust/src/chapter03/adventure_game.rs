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
use std::cell::RefCell;
use std::sync::Arc;

pub type Obj<T> = Box<T>;

fn obj<T>(x: T) -> Obj<T> {
    Box::new(x)
}

type_hierarchy! {
    (Object [name: String,]
        (Place [vistas: RefCell<Vec<Place>>,
                exits: RefCell<Vec<Exit>>,]
        )
        (Exit [from: Place, to: Place, direction: Direction,])
        (Thing [location: RefCell<Place>,]
            (MobileThing [origin: Place,]
                (Person [health: i64,]
                    (AutonomousAgent [restlessness: f64,]
                        (Student [])
                        (HouseMaster [irritability: f64,])
                        (Troll [hunger: f64,])
                    )
                    (Avatar [screen: Screen,])
                )
            )
        )
    )
}

impl Object {
    pub fn new(name: impl ToString) -> Self {
        Object {
            name: name.to_string(),
        }
    }
}

impl Place {
    pub fn new(name: impl ToString) -> Self {
        Place {
            parent: Object::new(name),
            exits: RefCell::new(vec![]),
            vistas: RefCell::new(vec![]),
        }
    }
}

impl Exit {
    pub fn new(from: Place, to: Place, direction: Direction) -> Self {
        Exit {
            parent: Object::new(""),
            from,
            to,
            direction,
        }
    }
}

impl Thing {
    pub fn new(name: impl ToString, location: Place) -> Self {
        Thing {
            parent: Object::new(name),
            location: RefCell::new(location),
        }
    }
}

impl MobileThing {
    pub fn new(name: impl ToString, location: Place) -> Self {
        MobileThing {
            parent: Thing::new(name, location.clone()),
            origin: location,
        }
    }
}

impl Person {
    pub fn new(name: impl ToString, location: Place, health: i64) -> Self {
        Person {
            parent: MobileThing::new(name, location.clone()),
            health,
        }
    }
}

impl AutonomousAgent {
    pub fn new(name: impl ToString, location: Place, health: i64, restlessness: f64) -> Self {
        AutonomousAgent {
            parent: Person::new(name, location, health),
            restlessness,
        }
    }
}

impl Troll {
    pub fn new(
        name: impl ToString,
        location: Place,
        health: i64,
        restlessness: f64,
        hunger: f64,
    ) -> Self {
        Troll {
            parent: AutonomousAgent::new(name, location, health, restlessness),
            hunger,
        }
    }
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

fn connect(place1: &Place, d1: Direction, d2: Direction, place2: &Place) {
    let exit12 = Exit::new(place1.clone(), place2.clone(), d1);
    let exit21 = Exit::new(place2.clone(), place1.clone(), d2);
    place1.exits.borrow_mut().push(exit12);
    place2.exits.borrow_mut().push(exit21);
}

fn connect_one_way(place1: &Place, d1: Direction, place2: &Place) {
    let exit12 = Exit::new(place1.clone(), place2.clone(), d1);
    place1.exits.borrow_mut().push(exit12);
}

fn move_internal(thing: &Thing, _from: &Place, to: &Place) {
    *thing.location.borrow_mut() = to.clone();
}

fn is_person(obj: &dyn Type) -> bool {
    obj.is_a::<Person>()
}

fn is_place(obj: &dyn Type) -> bool {
    obj.is_a::<Place>()
}

pub fn main() {
    use Direction::*;

    let lobby = Place::new("Lobby");
    let restroom = Place::new("Restroom");
    let infinite_corridor = Place::new("Infinite Corridor");

    connect(&lobby, East, West, &restroom);
    connect_one_way(&lobby, West, &infinite_corridor);
    connect(&infinite_corridor, North, South, &infinite_corridor);

    let generic_procedure = make_generic_procedure_constructor(SimpleDispatchStore::new);

    let generic_move = generic_procedure("generic-move", None);

    define_generic_procedure_handler(
        &generic_move,
        match_args(&[is_person, is_place, is_place, is_person]),
        |args| {
            let person = args[0].upcast::<Person>().unwrap();
            let from = args[1].upcast::<Place>().unwrap();
            let to = args[2].upcast::<Place>().unwrap();
            let _actor = args[3].upcast::<Person>().unwrap();

            move_internal(person.upcast::<Thing>().unwrap(), from, to);
            Ok(Arc::new(())) // argh!
        },
    );

    let grendel: Obj<dyn Type> = obj(Troll::new(
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
    );
}
