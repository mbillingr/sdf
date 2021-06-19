use sdf::chapter03::adventure_game::{objects, AdventureGame};

fn main() {
    objects::install_generic_procedure_handlers();

    let mut game = AdventureGame::new("Hotzenplotz");
    game.repl();
}
