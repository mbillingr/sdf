use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::container;
use crate::chapter03::adventure_game::objects::container::is_container;
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;

pub fn is_bag(obj: &dyn DebugAny) -> bool {
    container::is_container(obj) && obj.has_property("holder")
}

pub fn make_bag(name: impl ToString, holder: Obj) -> Obj {
    let obj = container::make_container(name);
    obj.set_property("holder", holder);
    obj
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_bag, is_container);
}
