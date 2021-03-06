use crate::chapter03::adventure_game::objects::container::add_thing;
use crate::chapter03::adventure_game::objects::object;
use crate::chapter03::adventure_game::objects::object::is_object;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_thing(obj: &dyn DebugAny) -> bool {
    object::is_object(obj) && obj.has_property("location")
}

pub fn make_thing(name: impl ToString, location: Obj) -> Obj {
    let obj = object::make_object(name);
    add_thing(&location, obj.clone());
    obj.set_property("location", location);
    return obj;
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_thing, is_object);
}

pub fn get_location(thing: &Obj) -> Obj {
    assert!(is_thing(&**thing));
    thing.get_property("location").unwrap()
}
