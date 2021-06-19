use crate::chapter03::adventure_game::clock::Clock;
use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::objects::person::is_person;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_autonomous_agent(obj: &dyn DebugAny) -> bool {
    person::is_person(obj)
        && obj.has_property("restlessness")
        && obj.has_property("acquisitiveness")
}

pub fn make_autonomous_agent(
    name: impl ToString,
    location: Obj,
    restlessness: f32,
    acquisitiveness: f32,
    clock: &mut Clock,
) -> Obj {
    let obj = person::make_person(name, location);
    obj.set_raw_property("restlessness", restlessness);
    obj.set_raw_property("acquisitiveness", acquisitiveness);

    clock.register(obj.clone());

    obj
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_autonomous_agent, is_person);
}
