use crate::chapter03::adventure_game::clock::Clock;
use crate::chapter03::adventure_game::objects::autonomous_agent;
use crate::chapter03::adventure_game::objects::autonomous_agent::is_autonomous_agent;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_troll(obj: &dyn DebugAny) -> bool {
    autonomous_agent::is_autonomous_agent(obj) && obj.has_property("hunger")
}

pub fn make_troll(
    name: impl ToString,
    place: Obj,
    restlessness: f64,
    hunger: f64,
    clock: &mut Clock,
) -> Obj {
    let obj = autonomous_agent::make_autonomous_agent(name, place, restlessness, 1.0 / 10.0, clock);
    obj.set_raw_property("hunger", hunger);
    obj
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_troll, is_autonomous_agent);
}
