use crate::board_game::factored::board_game_domain_model::{
    AggregateRule, Direction, EvolutionRule, Game, Movable, PMoveCollection, PartialMove,
};
use std::fmt;
use std::sync::Arc;

#[derive(Clone)]
pub struct Chess {
    evolution_rules: Vec<EvolutionRule<Self>>,
    aggregate_rules: Vec<AggregateRule<Self>>,
}

impl Game for Chess {
    type Players = Color;
    type PieceKind = ChessPiece;

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

impl Chess {
    pub fn new() -> Self {
        Chess {
            evolution_rules: vec![Arc::new(Self::rook_move)],
            aggregate_rules: vec![],
        }
    }

    fn rook_move(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        assert_eq!(pmove.current_piece().kind(), &ChessPiece::Rook);
        Chess::multidirectional_move(pmove)
    }

    fn multidirectional_move(pmove: PartialMove<Chess>) -> Vec<PartialMove<Chess>> {
        pmove
            .current_piece()
            .possible_directions()
            .into_iter()
            .flat_map(|direction| Self::directional_move(pmove.clone(), direction))
            .collect()
    }

    fn directional_move(
        pmove: PartialMove<Self>,
        direction: Direction,
    ) -> impl Iterator<Item = PartialMove<Self>> {
        let board = pmove.current_board();
        let mut stop_moving = false;
        (1..)
            .map(move |distance| {
                if stop_moving {
                    return None;
                }

                let landing = pmove.compute_new_position(direction, distance);

                if !board.is_position_on_board(landing)
                    || board.is_position_occupied_by_self(landing)
                {
                    return None;
                }

                let mut new_move = pmove.clone().new_piece_position(landing);

                if board.is_position_occupied_by_opponent(landing) {
                    stop_moving = true;
                    new_move = new_move.capture_piece_at(landing);
                }

                Some(new_move.finish_move())
            })
            .take_while(Option::is_some)
            .filter_map(|x| x)
    }
}

impl fmt::Debug for Chess {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Chess")
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    White,
    Black,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ChessPiece {
    Rook,
}

impl Movable for ChessPiece {
    fn possible_directions(&self) -> Vec<Direction> {
        match self {
            ChessPiece::Rook => vec![
                Direction::new(0, 1),
                Direction::new(0, -1),
                Direction::new(1, 0),
                Direction::new(-1, 0),
            ],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board_game::factored::board_game_domain_model::{Board, Coords, Piece};
    use Color::*;

    #[test]
    fn directional_move_does_not_leave_the_board() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let pmove = PartialMove::initial(Board::new(), piece);

        let moves: Vec<_> = Chess::directional_move(pmove, Direction::new(-1, 1)).collect();

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn directional_move_visits_all_positions_in_direction() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let pmove = PartialMove::initial(Board::new(), piece);

        let moves: Vec<_> = Chess::directional_move(pmove, Direction::new(1, 1)).collect();

        assert_eq!(moves.len(), 7);
    }

    #[test]
    fn directional_move_does_not_move_over_owned_pieces() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Rook, White));
        let pmove = PartialMove::initial(board, piece);

        let moves: Vec<_> = Chess::directional_move(pmove, Direction::new(0, 1)).collect();

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn directional_move_captures_opponent_pieces() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Rook, Black));
        let pmove = PartialMove::initial(board, piece);

        let moves: Vec<_> = Chess::directional_move(pmove, Direction::new(0, 1)).collect();

        assert_eq!(moves.len(), 1);
        assert!(moves[0].does_capture_pieces());
        assert_eq!(moves[0].current_piece().coords(), Coords::new(1, 2));
    }

    #[test]
    fn multidirectional_move_visits_all_directions_of_piece() {
        let piece = Piece::new(Coords::new(4, 4), ChessPiece::Rook, White);
        let board = Board::new().insert_piece(piece.clone());
        let pmove = PartialMove::initial(board, piece);

        let moves = Chess::multidirectional_move(pmove);

        assert_eq!(moves.len(), 14);
        let moved_coords: Vec<_> = moves
            .iter()
            .map(|pmove| pmove.current_piece().coords())
            .collect();
        assert!(moved_coords.contains(&Coords::new(1, 4)));
        assert!(moved_coords.contains(&Coords::new(8, 4)));
        assert!(moved_coords.contains(&Coords::new(4, 1)));
        assert!(moved_coords.contains(&Coords::new(4, 8)));
    }

    #[test]
    fn rook_moves_horizontally_or_vertically() {
        let directions = ChessPiece::Rook.possible_directions();
        assert_eq!(directions.len(), 4);
        assert!(directions.contains(&Direction::new(1, 0)));
        assert!(directions.contains(&Direction::new(-1, 0)));
        assert!(directions.contains(&Direction::new(0, 1)));
        assert!(directions.contains(&Direction::new(0, -1)));
    }
}
