use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::autonomous_agent;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::DebugAny;

pub fn is_house_master(obj: &dyn DebugAny) -> bool {
    autonomous_agent::is_autonomous_agent(obj) && obj.has_property("irritability")
}

pub fn make_house_master(
    name: impl ToString,
    home: Obj,
    restlessness: f32,
    irritability: f32,
) -> Obj {
    let obj = autonomous_agent::make_autonomous_agent(name, home, restlessness, 1.0 / 10.0);
    obj.set_raw_property("irritability", irritability);
    obj
}
