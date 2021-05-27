use std::sync::Arc;

use crate::board_game::factored::board_game_domain_model::{
    AggregateRule, Direction, EvolutionRule, Game, Movable, PMoveCollection, PartialMove, Piece,
};

#[derive(Clone)]
pub struct Checkers {
    evolution_rules: Vec<EvolutionRule<Self>>,
    aggregate_rules: Vec<AggregateRule<Self>>,
}

impl Game for Checkers {
    type Players = Color;
    type PieceKind = CheckersPiece;

    const BOARD_MIN_ROW: i16 = 1;
    const BOARD_MAX_ROW: i16 = 8;
    const BOARD_MIN_COL: i16 = 1;
    const BOARD_MAX_COL: i16 = 8;

    fn start_player() -> Self::Players {
        Color::White
    }

    fn evolution_rules(&self) -> &[EvolutionRule<Self>] {
        &self.evolution_rules
    }
    fn aggregate_rules(&self) -> &[AggregateRule<Self>] {
        &self.aggregate_rules
    }
}

impl Checkers {
    pub fn new() -> Self {
        Checkers {
            evolution_rules: vec![Arc::new(Self::simple_move), Arc::new(Self::jump)],
            aggregate_rules: vec![Arc::new(Self::coronation), Arc::new(Self::require_jumps)],
        }
    }

    fn simple_move(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        if pmove.is_empty() {
            Self::get_simple_moves(pmove)
        } else {
            vec![]
        }
    }

    fn get_simple_moves(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        pmove
            .current_piece()
            .possible_directions()
            .into_iter()
            .filter_map(|direction| {
                let landing = pmove.compute_new_position(direction, 1);
                let board = pmove.current_board();
                if board.is_position_on_board(landing) && board.is_position_unoccupied(landing) {
                    Some(pmove.clone().new_piece_position(landing).finish_move())
                } else {
                    None
                }
            })
            .collect()
    }

    fn jump(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        let jumps = Self::get_jumps(pmove.clone());

        if !jumps.is_empty() {
            jumps
        } else if pmove.is_empty() {
            return vec![];
        } else {
            vec![pmove.finish_move()]
        }
    }

    fn get_jumps(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        pmove
            .current_piece()
            .possible_directions()
            .into_iter()
            .filter_map(|direction| {
                let possible_jump = pmove.compute_new_position(direction, 1);
                let landing = pmove.compute_new_position(direction, 2);
                let board = pmove.current_board();
                if board.is_position_on_board(landing)
                    && board.is_position_unoccupied(landing)
                    && board.is_position_occupied_by_opponent(possible_jump)
                {
                    Some(
                        pmove
                            .clone()
                            .new_piece_position(landing)
                            .capture_piece_at(possible_jump),
                    )
                } else {
                    None
                }
            })
            .collect()
    }

    fn coronation(pmoves: PMoveCollection<Self>) -> PMoveCollection<Self> {
        pmoves
            .into_iter()
            .map(|pmove| {
                let piece = pmove.current_piece();
                if Self::should_be_crowned(&piece) {
                    pmove.update_piece(Self::crown_piece)
                } else {
                    pmove
                }
            })
            .collect()
    }

    fn should_be_crowned(piece: &Piece<Self>) -> bool {
        piece.coords().row == Self::BOARD_MAX_ROW
    }

    fn crown_piece(piece: &Piece<Self>) -> Piece<Self> {
        piece.new_kind(CheckersPiece::King)
    }

    fn require_jumps(pmoves: PMoveCollection<Self>) -> PMoveCollection<Self> {
        let jumps: PMoveCollection<_> = pmoves
            .iter()
            .cloned()
            .filter(PartialMove::does_capture_pieces)
            .collect();
        if jumps.is_empty() {
            pmoves
        } else {
            jumps
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    White,
    Black,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CheckersPiece {
    Normal,
    King,
}

impl Movable for CheckersPiece {
    fn possible_directions(&self) -> Vec<Direction> {
        match self {
            CheckersPiece::Normal => vec![Direction::new(1, -1), Direction::new(1, 1)],
            CheckersPiece::King => vec![
                Direction::new(1, -1),
                Direction::new(1, 1),
                Direction::new(-1, -1),
                Direction::new(-1, 1),
            ],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board_game::factored::board_game_domain_model::{Board, Coords, Game, Piece};
    use crate::board_game::factored::checkers::Color::*;
    use crate::board_game::factored::checkers::{Checkers, CheckersPiece};

    #[test]
    fn empty_board_produces_no_moves() {
        let board = Board::new();
        let moves = Checkers::new().execute_game_rules(&board);
        assert!(moves.is_empty());
    }

    #[test]
    fn immovable_piece_produces_no_moves() {
        let board =
            Board::new().insert_piece(Piece::new(Coords::new(8, 8), CheckersPiece::Normal, White));
        let moves = Checkers::new().execute_game_rules(&board);
        assert!(moves.is_empty());
    }

    #[test]
    fn produce_normal_simple_moves() {
        let board =
            Board::new().insert_piece(Piece::new(Coords::new(3, 3), CheckersPiece::Normal, White));
        let moves = Checkers::new().execute_game_rules(&board);
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn produce_king_simple_moves() {
        let board =
            Board::new().insert_piece(Piece::new(Coords::new(3, 3), CheckersPiece::King, White));
        let moves = Checkers::new().execute_game_rules(&board);
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn produce_jump() {
        let piece_coords = Coords::new(1, 1);
        let jumped_coords = Coords::new(2, 2);
        let landing_coords = Coords::new(3, 3);
        let board = Board::new()
            .insert_piece(Piece::new(piece_coords, CheckersPiece::Normal, White))
            .insert_piece(Piece::new(jumped_coords, CheckersPiece::Normal, Black));
        let moves = Checkers::new().execute_game_rules(&board);
        assert_eq!(moves.len(), 1);
        assert_eq!(moves[0].current_board().get(jumped_coords), None);
        assert_eq!(
            moves[0].current_board().get(landing_coords),
            Some(&Piece::new(landing_coords, CheckersPiece::Normal, White))
        );
    }

    #[test]
    fn crown_a_piece_that_moves_into_the_front_line() {
        let piece_coords = Coords::new(7, 1);
        let move_coords = Coords::new(8, 2);
        let board =
            Board::new().insert_piece(Piece::new(piece_coords, CheckersPiece::Normal, White));
        let moves = Checkers::new().execute_game_rules(&board);
        assert_eq!(moves.len(), 1);
        assert_eq!(
            moves[0].current_board().get(move_coords),
            Some(&Piece::new(move_coords, CheckersPiece::King, White))
        );
    }

    #[test]
    fn enforce_jump() {
        let piece_coords = Coords::new(3, 3);
        let jumped_coords = Coords::new(4, 4);
        let landing_coords = Coords::new(5, 5);
        let board = Board::new()
            .insert_piece(Piece::new(piece_coords, CheckersPiece::Normal, White))
            .insert_piece(Piece::new(jumped_coords, CheckersPiece::Normal, Black));
        let moves = Checkers::new().execute_game_rules(&board);
        assert_eq!(moves.len(), 1);
        assert_eq!(moves[0].current_board().get(jumped_coords), None);
        assert_eq!(
            moves[0].current_board().get(landing_coords),
            Some(&Piece::new(landing_coords, CheckersPiece::Normal, White))
        );
    }
}
