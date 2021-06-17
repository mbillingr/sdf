use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::exit::is_exit;
use crate::chapter03::adventure_game::objects::{container, exit};
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::DebugAny;
use std::cell::RefCell;

type MutableVec = RefCell<Vec<Obj>>;

pub fn is_place(obj: &dyn DebugAny) -> bool {
    container::is_container(obj) && obj.has_property("vistas") && obj.has_property("exits")
}

pub fn make_place(name: impl ToString) -> Obj {
    let obj = container::make_container(name);
    obj.set_raw_property("vistas", MutableVec::new(vec![]));
    obj.set_raw_property("exits", MutableVec::new(vec![]));
    return obj;
}

pub fn add_exit(place: &Obj, exit: Obj) {
    assert!(is_place(&**place));
    assert!(exit::is_exit(&*exit));
    place
        .get_property("exits")
        .unwrap()
        .downcast_ref::<MutableVec>()
        .unwrap()
        .borrow_mut()
        .push(exit);
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_place, is_exit);
}
