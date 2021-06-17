pub mod autonomous_agent;
pub mod avatar;
pub mod bag;
pub mod container;
pub mod exit;
pub mod house_master;
pub mod mobile_thing;
pub mod object;
pub mod person;
pub mod place;
pub mod screen;
pub mod student;
pub mod thing;
pub mod troll;

pub fn install_generic_procedure_handlers() {
    autonomous_agent::install_generic_procedure_handlers();
    avatar::install_generic_procedure_handlers();
    bag::install_generic_procedure_handlers();
    container::install_generic_procedure_handlers();
    exit::install_generic_procedure_handlers();
    house_master::install_generic_procedure_handlers();
    mobile_thing::install_generic_procedure_handlers();
    object::install_generic_procedure_handlers();
    person::install_generic_procedure_handlers();
    place::install_generic_procedure_handlers();
    screen::install_generic_procedure_handlers();
    student::install_generic_procedure_handlers();
    thing::install_generic_procedure_handlers();
    troll::install_generic_procedure_handlers();
}
