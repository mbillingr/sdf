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
pub mod student;
pub mod thing;
pub mod troll;

pub fn install_generic_procedure_handlers() {
    mobile_thing::install_generic_procedure_handlers()
}
