use crate::board_game::factored::board_game_domain_model::{
    AggregateRule, Board, Coords, Direction, EvolutionRule, Game, Movable, PMoveCollection,
    PartialMove, Piece, PositionInfo,
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
            evolution_rules: vec![Arc::new(Self::move_dispatch), Arc::new(Self::castling)],
            aggregate_rules: vec![
                Arc::new(Self::promotion),
                Arc::new(Self::mark_moved),
                Arc::new(Self::forbid_self_check),
            ],
        }
    }

    pub fn new_checker_check() -> Self {
        Chess {
            evolution_rules: vec![Arc::new(Self::move_dispatch), Arc::new(Self::castling)],
            aggregate_rules: vec![Arc::new(Self::promotion)],
        }
    }

    fn move_dispatch(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        match pmove.current_piece().kind() {
            ChessPiece::UnmovedRook | ChessPiece::Rook | ChessPiece::Bishop | ChessPiece::Queen => {
                Self::multidirectional_move(pmove)
            }
            ChessPiece::Knight | ChessPiece::UnmovedKing | ChessPiece::King => {
                Self::simple_move(pmove)
            }
            ChessPiece::Pawn => Self::pawn_move(pmove),
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

    fn pawn_move(pmove: PartialMove<Chess>) -> PMoveCollection<Self> {
        let board = pmove.current_board();

        let mut moves = PMoveCollection::new();

        let normal_landing = pmove.compute_new_position(Direction::new(1, 0), 1);
        if board.is_position_unoccupied(normal_landing) {
            moves.push(
                pmove
                    .clone()
                    .new_piece_position(normal_landing)
                    .finish_move(),
            );
        }

        if pmove.current_piece().coords().row == 2 {
            let extra_landing = pmove.compute_new_position(Direction::new(1, 0), 2);
            if board.is_position_unoccupied(extra_landing) {
                moves.push(
                    pmove
                        .clone()
                        .new_piece_position(extra_landing)
                        .finish_move(),
                );
            }
        }

        for capture_direction in vec![Direction::new(1, -1), Direction::new(1, 1)] {
            let landing = pmove.compute_new_position(capture_direction, 1);
            if board.is_position_occupied_by_opponent(landing) {
                moves.push(
                    pmove
                        .clone()
                        .new_piece_position(landing)
                        .capture_piece_at(landing)
                        .finish_move(),
                );
            }
        }

        moves
    }

    fn promotion(pmoves: PMoveCollection<Self>) -> PMoveCollection<Self> {
        pmoves
            .into_iter()
            .flat_map(|pmove| {
                let piece = pmove.current_piece();
                if let Some(new_kinds) = Self::should_be_promoted(&piece) {
                    new_kinds
                        .into_iter()
                        .map(|new_kind| pmove.clone().update_piece(|p| p.new_kind(new_kind)))
                        .collect()
                } else {
                    vec![pmove]
                }
            })
            .collect()
    }

    fn should_be_promoted(piece: &Piece<Self>) -> Option<Vec<ChessPiece>> {
        if let ChessPiece::Pawn = piece.kind() {
            if piece.coords().row == Self::BOARD_MAX_ROW {
                return Some(vec![
                    ChessPiece::Queen,
                    ChessPiece::Rook,
                    ChessPiece::Bishop,
                    ChessPiece::Knight,
                ]);
            }
        }
        None
    }

    fn forbid_self_check(pmoves: PMoveCollection<Self>) -> PMoveCollection<Self> {
        pmoves
            .into_iter()
            .filter(|pmove| !Self::current_king_checked(&pmove.current_board()))
            .collect()
    }

    fn current_king_checked(board: &Board<Self>) -> bool {
        let board = board.new_player(board.current_player().next());
        let opponent_moves = Self::new_checker_check().execute_game_rules(&board);
        let res = opponent_moves
            .iter()
            .flat_map(|pmove| pmove.captured_pieces())
            .any(|piece| piece.kind() == &ChessPiece::King);
        res
    }

    fn castling(pmove: PartialMove<Self>) -> PMoveCollection<Self> {
        if pmove.current_piece().kind() == &ChessPiece::UnmovedKing {
            let board = pmove.current_board();
            let mut castle_moves = PMoveCollection::new();
            for (rook_pos, direction) in vec![(Coords::new(1, 1), -1), (Coords::new(1, 8), 1)] {
                if let Some(rook) = board
                    .get(rook_pos)
                    .filter(|piece| piece.kind() == &ChessPiece::UnmovedRook)
                {
                    let direction = Direction::new(0, direction);
                    let king_pos = pmove.current_piece().coords();
                    let king_out_pos = king_pos + direction * 2;
                    let rook_out_pos = king_pos + direction;
                    castle_moves.push(
                        pmove
                            .clone()
                            .update_piece(|king| king.new_kind(ChessPiece::King))
                            .new_piece_position(king_out_pos)
                            .move_another_piece(rook.clone())
                            .update_piece(|rook| rook.new_kind(ChessPiece::Rook))
                            .new_piece_position(rook_out_pos)
                            .finish_move(),
                    )
                }
            }
            castle_moves
        } else {
            PMoveCollection::new()
        }
    }

    fn mark_moved(pmoves: PMoveCollection<Self>) -> PMoveCollection<Self> {
        pmoves
            .into_iter()
            .map(|pmove| {
                let piece = pmove.current_piece();
                match piece.kind() {
                    ChessPiece::UnmovedRook => {
                        pmove.update_piece(|piece| piece.new_kind(ChessPiece::Rook))
                    }
                    ChessPiece::UnmovedKing => {
                        pmove.update_piece(|piece| piece.new_kind(ChessPiece::King))
                    }
                    _ => pmove,
                }
            })
            .collect()
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

impl Color {
    pub fn next(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ChessPiece {
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
    Pawn,
    UnmovedKing,
    UnmovedRook,
}

impl Movable for ChessPiece {
    fn possible_directions(&self) -> Vec<Direction> {
        match self {
            ChessPiece::Rook | ChessPiece::UnmovedRook => vec![
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
            ChessPiece::Queen | ChessPiece::King | ChessPiece::UnmovedKing => vec![
                Direction::new(1, 1),
                Direction::new(1, -1),
                Direction::new(-1, 1),
                Direction::new(-1, -1),
                Direction::new(0, 1),
                Direction::new(0, -1),
                Direction::new(1, 0),
                Direction::new(-1, 0),
            ],
            ChessPiece::Pawn => vec![], // pawns have special movement logic
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

    #[test]
    fn king_moves_diagonally_horizontally_or_vertically() {
        let directions = ChessPiece::King.possible_directions();
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

    #[test]
    fn pawn_moves_one_row_forward() {
        let piece = Piece::new(Coords::new(5, 5), ChessPiece::Pawn, White);
        let pmove = PartialMove::initial(Board::new(), piece.clone());

        let moves = Chess::pawn_move(pmove);

        assert_eq!(moves.len(), 1);
        assert_eq!(moves[0].current_piece().coords(), Coords::new(6, 5));
    }

    #[test]
    fn pawns_cant_move_over_own_pieces() {
        let piece = Piece::new(Coords::new(5, 5), ChessPiece::Pawn, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(6, 5), ChessPiece::Rook, White));
        let pmove = PartialMove::initial(board, piece.clone());

        let moves = Chess::pawn_move(pmove);

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn pawns_cant_move_over_opponent_pieces() {
        let piece = Piece::new(Coords::new(5, 5), ChessPiece::Pawn, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(6, 5), ChessPiece::Rook, Black));
        let pmove = PartialMove::initial(board, piece.clone());

        let moves = Chess::pawn_move(pmove);

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn pawns_can_capture_opponent_pieces() {
        let piece = Piece::new(Coords::new(5, 5), ChessPiece::Pawn, White);
        let board = Board::new()
            .insert_piece(piece.clone())
            .insert_piece(Piece::new(Coords::new(6, 4), ChessPiece::Rook, Black))
            .insert_piece(Piece::new(Coords::new(6, 6), ChessPiece::Rook, Black));
        let pmove = PartialMove::initial(board, piece.clone());

        let moves = Chess::pawn_move(pmove);

        assert_eq!(moves.len(), 3);
    }

    #[test]
    fn pawn_can_initially_move_two_rows_forward() {
        let piece = Piece::new(Coords::new(2, 5), ChessPiece::Pawn, White);
        let pmove = PartialMove::initial(Board::new(), piece.clone());

        let moves = Chess::pawn_move(pmove);

        assert_eq!(moves.len(), 2);
        assert_eq!(moves[0].current_piece().coords(), Coords::new(3, 5));
        assert_eq!(moves[1].current_piece().coords(), Coords::new(4, 5));
    }

    #[test]
    fn promote_pawns_moving_into_opponent_home() {
        let piece = Piece::new(Coords::new(7, 5), ChessPiece::Pawn, White);
        let board = Board::new().insert_piece(piece.clone());

        let moves = Chess::new().execute_game_rules(&board);

        assert_eq!(moves.len(), 4);
        let final_pieces: Vec<_> = moves
            .iter()
            .map(|pmove| *pmove.current_piece().kind())
            .collect();
        assert!(final_pieces.contains(&ChessPiece::Queen));
        assert!(final_pieces.contains(&ChessPiece::Rook));
        assert!(final_pieces.contains(&ChessPiece::Bishop));
        assert!(final_pieces.contains(&ChessPiece::Knight));
    }

    #[test]
    fn moves_may_not_end_in_own_king_being_checked() {
        let board = Board::new()
            .insert_piece(Piece::new(Coords::new(1, 1), ChessPiece::King, White))
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Queen, Black));
        let moves = Chess::new().execute_game_rules(&board);

        // only valid move is to capture the opponent queen
        assert_eq!(moves.len(), 1);
        assert!(moves[0].does_capture_pieces());
        assert_eq!(moves[0].current_piece().coords(), Coords::new(1, 2));
    }

    #[test]
    fn no_moves_possible_in_check_mate() {
        let board = Board::new()
            .insert_piece(Piece::new(Coords::new(1, 1), ChessPiece::King, White))
            .insert_piece(Piece::new(Coords::new(2, 1), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 2), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(1, 3), ChessPiece::Queen, Black));
        let moves = Chess::new().execute_game_rules(&board);

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn number_of_possible_opening_moves() {
        let board = Board::new()
            .insert_piece(Piece::new(Coords::new(2, 1), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 2), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 3), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 4), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 5), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 6), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 7), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(2, 8), ChessPiece::Pawn, White))
            .insert_piece(Piece::new(Coords::new(1, 1), ChessPiece::Rook, White))
            .insert_piece(Piece::new(Coords::new(1, 2), ChessPiece::Knight, White))
            .insert_piece(Piece::new(Coords::new(1, 3), ChessPiece::Bishop, White))
            .insert_piece(Piece::new(Coords::new(1, 4), ChessPiece::Queen, White))
            .insert_piece(Piece::new(Coords::new(1, 5), ChessPiece::King, White))
            .insert_piece(Piece::new(Coords::new(1, 6), ChessPiece::Bishop, White))
            .insert_piece(Piece::new(Coords::new(1, 7), ChessPiece::Knight, White))
            .insert_piece(Piece::new(Coords::new(1, 8), ChessPiece::Rook, White));
        let moves = Chess::new().execute_game_rules(&board);

        assert_eq!(moves.len(), 20);
    }

    #[test]
    fn castling_is_not_allowed_if_king_or_rook_have_moved() {
        let king = Piece::new(Coords::new(1, 5), ChessPiece::King, White);
        let board = Board::new()
            .insert_piece(king.clone())
            .insert_piece(Piece::new(Coords::new(1, 1), ChessPiece::Rook, White))
            .insert_piece(Piece::new(Coords::new(1, 8), ChessPiece::Rook, White));
        let pmove = PartialMove::initial(board, king);
        let moves = Chess::castling(pmove);

        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn castling_is_allowed_if_king_or_rook_have_not_moved() {
        let king = Piece::new(Coords::new(1, 5), ChessPiece::UnmovedKing, White);
        let board = Board::new()
            .insert_piece(king.clone())
            .insert_piece(Piece::new(
                Coords::new(1, 1),
                ChessPiece::UnmovedRook,
                White,
            ))
            .insert_piece(Piece::new(
                Coords::new(1, 8),
                ChessPiece::UnmovedRook,
                White,
            ));
        let pmove = PartialMove::initial(board, king);
        let moves = Chess::castling(pmove);

        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn do_the_small_white_castle() {
        let king = Piece::new(Coords::new(1, 5), ChessPiece::UnmovedKing, White);
        let rook = Piece::new(Coords::new(1, 8), ChessPiece::UnmovedRook, White);
        let board = Board::new().insert_piece(king.clone()).insert_piece(rook);
        let pmove = PartialMove::initial(board, king);
        let moves = Chess::castling(pmove);

        assert_eq!(moves.len(), 1);
        assert_eq!(
            moves[0]
                .current_board()
                .get(Coords::new(1, 7))
                .map(Piece::kind),
            Some(&ChessPiece::King)
        );
        assert_eq!(
            moves[0]
                .current_board()
                .get(Coords::new(1, 6))
                .map(Piece::kind),
            Some(&ChessPiece::Rook)
        );
    }

    #[test]
    fn do_the_big_white_castle() {
        let king = Piece::new(Coords::new(1, 5), ChessPiece::UnmovedKing, White);
        let rook = Piece::new(Coords::new(1, 1), ChessPiece::UnmovedRook, White);
        let board = Board::new().insert_piece(king.clone()).insert_piece(rook);
        let pmove = PartialMove::initial(board, king);
        let moves = Chess::castling(pmove);

        assert_eq!(moves.len(), 1);
        assert_eq!(
            moves[0]
                .current_board()
                .get(Coords::new(1, 3))
                .map(Piece::kind),
            Some(&ChessPiece::King)
        );
        assert_eq!(
            moves[0]
                .current_board()
                .get(Coords::new(1, 4))
                .map(Piece::kind),
            Some(&ChessPiece::Rook)
        );
    }

    #[test]
    fn unmoved_king_becomes_king_when_moving() {
        let king = Piece::new(Coords::new(1, 1), ChessPiece::UnmovedKing, White);
        let board = Board::new().insert_piece(king.clone());
        let pmove = PartialMove::initial(board, king);
        let moves = Chess::mark_moved(vec![pmove]);

        assert_eq!(moves.len(), 1);
        assert_eq!(moves[0].current_piece().kind(), &ChessPiece::King);
    }
}
