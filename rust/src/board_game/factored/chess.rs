use crate::board_game::factored::board_game_domain_model::{
    AggregateRule, Direction, EvolutionRule, Game, Movable, PMoveCollection, PartialMove,
    PositionInfo,
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
            evolution_rules: vec![Arc::new(Self::move_dispatch)],
            aggregate_rules: vec![],
        }
    }

    fn move_dispatch(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        match pmove.current_piece().kind() {
            ChessPiece::Rook | ChessPiece::Bishop | ChessPiece::Queen => {
                Self::multidirectional_move(pmove)
            }
            ChessPiece::Knight => Self::simple_move(pmove),
        }
    }

    fn simple_move(pmove: PartialMove<Chess>) -> PMoveCollection<Self> {
        pmove
            .current_piece()
            .possible_directions()
            .into_iter()
            .filter_map(|direction| {
                let landing = pmove.compute_new_position(direction, 1);
                let board = pmove.current_board();

                if !board.is_position_on_board(landing) {
                    return None;
                }

                let mut new_move = pmove.clone().new_piece_position(landing);

                match board.position_info(landing) {
                    PositionInfo::OccupiedBySelf => return None,
                    PositionInfo::Unoccupied => {}
                    PositionInfo::OccupiedByOpponent => {
                        new_move = new_move.capture_piece_at(landing)
                    }
                }

                Some(new_move.finish_move())
            })
            .collect()
    }

    fn multidirectional_move(pmove: PartialMove<Chess>) -> PMoveCollection<Self> {
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
    Knight,
    Bishop,
    Queen,
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
            ChessPiece::Knight => vec![
                Direction::new(2, 1),
                Direction::new(2, -1),
                Direction::new(1, 2),
                Direction::new(-1, 2),
                Direction::new(-2, 1),
                Direction::new(-2, -1),
                Direction::new(1, -2),
                Direction::new(-1, -2),
            ],
            ChessPiece::Bishop => vec![
                Direction::new(1, 1),
                Direction::new(1, -1),
                Direction::new(-1, 1),
                Direction::new(-1, -1),
            ],
            ChessPiece::Queen => vec![
                Direction::new(1, 1),
                Direction::new(1, -1),
                Direction::new(-1, 1),
                Direction::new(-1, -1),
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
    fn simple_move_visits_all_piece_directions() {
        let piece = Piece::new(Coords::new(5, 5), ChessPiece::Rook, White);
        let pmove = PartialMove::initial(Board::new(), piece);

        let moves = Chess::simple_move(pmove);

        assert_eq!(moves.len(), 4);
        let moved_coords: Vec<_> = moves
            .iter()
            .map(|pmove| pmove.current_piece().coords())
            .collect();
        assert!(moved_coords.contains(&Coords::new(5, 6)));
        assert!(moved_coords.contains(&Coords::new(5, 4)));
        assert!(moved_coords.contains(&Coords::new(4, 5)));
        assert!(moved_coords.contains(&Coords::new(6, 5)));
    }

    #[test]
    fn simple_move_does_not_leave_the_board() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let pmove = PartialMove::initial(Board::new(), piece);

        let moves = Chess::simple_move(pmove);

        assert_eq!(moves.len(), 2);
        let moved_coords: Vec<_> = moves
            .iter()
            .map(|pmove| pmove.current_piece().coords())
            .collect();
        assert!(!moved_coords.contains(&Coords::new(1, 0)));
        assert!(!moved_coords.contains(&Coords::new(0, 1)));
    }

    #[test]
    fn simple_move_does_not_move_over_owned_pieces() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Rook, White))
            .insert_piece(Piece::new(Coords::new(2, 1), ChessPiece::Rook, White));
        let pmove = PartialMove::initial(board, piece);

        let moves = Chess::simple_move(pmove);

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn simple_move_captures_opponent_pieces() {
        let piece = Piece::new(Coords::new(1, 1), ChessPiece::Rook, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(2, 1), ChessPiece::Rook, White))
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Rook, Black));
        let pmove = PartialMove::initial(board, piece);

        let moves = Chess::simple_move(pmove);

        assert_eq!(moves.len(), 1);
        assert!(moves[0].does_capture_pieces());
        assert_eq!(moves[0].current_piece().coords(), Coords::new(1, 2));
    }

    #[test]
    fn pieces_make_only_appropriate_moves() {
        let rook = Piece::new(Coords::new(2, 3), ChessPiece::Rook, White);
        let knight = Piece::new(Coords::new(1, 1), ChessPiece::Knight, White);
        let board = Board::new()
            .insert_piece(rook.clone())
            .insert_piece(knight.clone());

        let moves = Chess::new().execute_game_rules(&board);

        assert_eq!(moves.len(), 14 + 1) // all moves valid for the rook, one for the knight
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

    #[test]
    fn knight_jumps_in_8_possible_directions() {
        let directions = ChessPiece::Knight.possible_directions();
        assert_eq!(directions.len(), 8);
        assert!(directions.contains(&Direction::new(2, 1)));
        assert!(directions.contains(&Direction::new(2, -1)));
        assert!(directions.contains(&Direction::new(-2, 1)));
        assert!(directions.contains(&Direction::new(-2, -1)));
        assert!(directions.contains(&Direction::new(1, 2)));
        assert!(directions.contains(&Direction::new(-1, 2)));
        assert!(directions.contains(&Direction::new(1, -2)));
        assert!(directions.contains(&Direction::new(-1, -2)));
    }

    #[test]
    fn bishop_moves_diagonally() {
        let directions = ChessPiece::Bishop.possible_directions();
        assert_eq!(directions.len(), 4);
        assert!(directions.contains(&Direction::new(1, 1)));
        assert!(directions.contains(&Direction::new(-1, 1)));
        assert!(directions.contains(&Direction::new(1, -1)));
        assert!(directions.contains(&Direction::new(-1, -1)));
    }

    #[test]
    fn queen_moves_diagonally_horizontally_or_vertically() {
        let directions = ChessPiece::Queen.possible_directions();
        assert_eq!(directions.len(), 8);
        assert!(directions.contains(&Direction::new(1, 1)));
        assert!(directions.contains(&Direction::new(-1, 1)));
        assert!(directions.contains(&Direction::new(1, -1)));
        assert!(directions.contains(&Direction::new(-1, -1)));
        assert!(directions.contains(&Direction::new(1, 0)));
        assert!(directions.contains(&Direction::new(-1, 0)));
        assert!(directions.contains(&Direction::new(0, 1)));
        assert!(directions.contains(&Direction::new(0, -1)));
    }
}
