use crate::chapter03::adventure_game::objects::object;
use crate::chapter03::adventure_game::objects::object::is_object;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_container(obj: &dyn DebugAny) -> bool {
    object::is_object(obj) && obj.has_property("things")
}

pub fn make_container(name: impl ToString) -> Obj {
    let obj = object::make_object(name);
    obj.set_raw_property("things", Vec::<Obj>::new());
    return obj;
}

pub fn get_things(container: &Obj) -> Vec<Obj> {
    assert!(is_container(&**container));
    let things = container.get_property("things").unwrap();
    let things_typed = things.downcast_ref::<Vec<Obj>>().unwrap();
    things_typed.clone()
}

pub fn add_thing(container: &Obj, thing: Obj) {
    let mut things = get_things(container);
    things.push(thing);
    container.set_raw_property("things", things);
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_container, is_object);
}
