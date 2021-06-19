use sdf::chapter03::adventure_game::{objects, AdventureGame};

fn main() {
    objects::install_generic_procedure_handlers();

    let game = AdventureGame::new("Hotzenplotz");
    game.repl().map_err(|s| s.to_string()).unwrap();
}
