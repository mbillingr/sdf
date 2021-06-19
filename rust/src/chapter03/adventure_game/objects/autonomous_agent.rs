use crate::chapter03::adventure_game::clock::Clock;
use crate::chapter03::adventure_game::generic_procedures::CLOCK_TICK;
use crate::chapter03::adventure_game::objects::person;
use crate::chapter03::adventure_game::objects::person::{exits_here, is_person};
use crate::chapter03::adventure_game::property_table::Properties;
use crate::chapter03::adventure_game::{flip_coin, random_choice};
use crate::chapter03::dynamic_type::Obj;
use crate::chapter03::generic_procedures::predicate::declare_superset;
use crate::chapter03::generic_procedures::{
    define_generic_procedure_handler, match_args, GenericResult,
};
use crate::chapter03::DebugAny;

pub fn is_autonomous_agent(obj: &dyn DebugAny) -> bool {
    person::is_person(obj)
        && obj.has_property("restlessness")
        && obj.has_property("acquisitiveness")
}

pub fn make_autonomous_agent(
    name: impl ToString,
    location: Obj,
    restlessness: f64,
    acquisitiveness: f64,
    clock: &mut Clock,
) -> Obj {
    let obj = person::make_person(name, location);
    obj.set_raw_property("restlessness", restlessness);
    obj.set_raw_property("acquisitiveness", acquisitiveness);

    clock.register(obj.clone());

    obj
}

pub fn move_and_take_stuff(agent: &dyn DebugAny) -> GenericResult {
    assert!(is_autonomous_agent(agent));
    if flip_coin(agent.downcast_property("restlessness").unwrap()) {
        move_somewhere(agent)?;
    }
    if flip_coin(agent.downcast_property("acquisitiveness").unwrap()) {
        take_something(agent)?;
    }
    Ok(None)
}

pub fn move_somewhere(agent: &dyn DebugAny) -> GenericResult {
    assert!(is_autonomous_agent(agent));
    //let exit = random_choice(exits_here(agent));
    unimplemented!()
}

pub fn take_something(agent: &dyn DebugAny) -> GenericResult {
    assert!(is_autonomous_agent(agent));
    unimplemented!()
}

pub fn install_generic_procedure_handlers() {
    declare_superset(is_autonomous_agent, is_person);

    define_generic_procedure_handler(&CLOCK_TICK, match_args(&[is_autonomous_agent]), |args| {
        move_and_take_stuff(args[0])
    });
}
