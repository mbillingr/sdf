use crate::chapter03::adventure_game::objects::mobile_thing::is_mobile_thing;
use crate::chapter03::adventure_game::objects::place::{
    get_exits, get_vistas, people_in_place, things_in_place,
};
use crate::chapter03::adventure_game::objects::thing::get_location;
use crate::chapter03::adventure_game::objects::{bag, mobile_thing};
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_person(obj: &dyn DebugAny) -> bool {
    mobile_thing::is_mobile_thing(obj) && obj.has_property("health") && obj.has_property("bag")
}

pub fn make_person(name: impl ToString, location: Obj) -> Obj {
    let person = mobile_thing::make_mobile_thing(name, location.clone());
    person.set_raw_property("health", 3);
    person.set_property(
        "bag",
        bag::make_bag("my bag".to_string(), dynamic_type::obj(false)),
    );
    return person;
}

pub fn get_bag(person: &Obj) -> Obj {
    assert!(is_person(&**person));
    person.get_property("bag").unwrap()
}

pub fn things_here(person: &Obj) -> Vec<Obj> {
    assert!(is_person(&**person));
    things_in_place(&get_location(person))
}

pub fn people_here(person: &Obj) -> Vec<Obj> {
    assert!(is_person(&**person));
    let mut people = people_in_place(&get_location(person));
    let me = people.iter().position(|p| p == person).unwrap();
    people.remove(me);
    people
}

pub fn vistas_here(person: &Obj) -> Vec<Obj> {
    assert!(is_person(&**person));
    get_vistas(&get_location(person))
}

pub fn exits_here(person: &Obj) -> Vec<Obj> {
    assert!(is_person(&**person));
    get_exits(&get_location(person))
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_person, is_mobile_thing);
}
