use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::autonomous_agent;
use crate::chapter03::adventure_game::objects::autonomous_agent::is_autonomous_agent;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_student(obj: &dyn DebugAny) -> bool {
    autonomous_agent::is_autonomous_agent(obj) && obj.has_property("student-flag")
}

pub fn make_student(
    name: impl ToString,
    home: Obj,
    restlessness: f32,
    acquisitiveness: f32,
) -> Obj {
    let obj = autonomous_agent::make_autonomous_agent(name, home, restlessness, acquisitiveness);
    obj.set_raw_property("student-flag", true);
    obj
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_student, is_autonomous_agent);
}
