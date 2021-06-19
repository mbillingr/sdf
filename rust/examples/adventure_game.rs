use sdf::chapter03::adventure_game::{generic_procedures, objects, AdventureGame};

fn main() {
    generic_procedures::install_generic_procedure_handlers();
    objects::install_generic_procedure_handlers();

    let mut game = AdventureGame::new("Hotzenplotz");
    game.repl();
}
