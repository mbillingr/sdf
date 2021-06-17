use crate::chapter03::adventure_game::dynamic_type;
use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::mobile_thing::is_mobile_thing;
use crate::chapter03::adventure_game::objects::{bag, mobile_thing};
use crate::chapter03::adventure_game::property_table::Properties;
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

pub fn install_generic_procedure_handlers() {
    declare_superset(is_person, is_mobile_thing);
}
