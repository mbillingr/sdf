use crate::chapter03::adventure_game::dynamic_type::Obj;
use crate::chapter03::adventure_game::objects::container::get_things;
use crate::chapter03::adventure_game::objects::exit::is_exit;
use crate::chapter03::adventure_game::objects::person::is_person;
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

pub fn get_vistas(place: &Obj) -> Vec<Obj> {
    assert!(is_place(&**place));
    let vistas = place.get_property("vistas").unwrap();
    let vistas_typed = vistas.downcast_ref::<MutableVec>().unwrap();
    let vistas_cloned = (*vistas_typed.borrow()).clone();
    vistas_cloned
}

pub fn add_vista(place: &Obj, vista: Obj) {
    assert!(is_place(&**place));
    place
        .get_property("vistas")
        .unwrap()
        .downcast_ref::<MutableVec>()
        .unwrap()
        .borrow_mut()
        .push(vista);
}

pub fn get_exits(place: &Obj) -> Vec<Obj> {
    assert!(is_place(&**place));
    let exits = place.get_property("exits").unwrap();
    let exits_typed = exits.downcast_ref::<MutableVec>().unwrap();
    let exits_cloned = (*exits_typed.borrow()).clone();
    exits_cloned
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

pub fn things_in_place(place: &Obj) -> Vec<Obj> {
    assert!(is_place(&**place));
    get_things(place)
        .into_iter()
        .filter(|thing| !is_person(&**thing))
        .collect()
}

pub fn people_in_place(place: &Obj) -> Vec<Obj> {
    assert!(is_place(&**place));
    get_things(place)
        .into_iter()
        .filter(|thing| is_person(&**thing))
        .collect()
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_place, is_exit);
}
