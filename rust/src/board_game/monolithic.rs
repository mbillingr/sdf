use domain_model::{Board, Coords, Direction, Piece, PositionInfo};

fn try_step<B: Board, P: Path<B>>(
    piece: &B::Piece,
    board: &B,
    direction: Direction,
    path: &P,
) -> Option<P> {
    let new_coords = piece.coords() + direction;

    if !board.is_position_on_board(new_coords) {
        return None;
    }

    match board.position_info(new_coords) {
        PositionInfo::Unoccupied => {
            if path.contains_jumps() {
                return None;
            }
            Some(path.append(Step::make_simple_move(new_coords, piece, board)))
        }
        PositionInfo::OccupiedByOpponent => {
            let landing = new_coords + direction;
            if !(board.is_position_on_board(landing) && board.is_position_unoccupied(landing)) {
                return None;
            }
            Some(path.append(Step::make_jump(landing, new_coords, piece, board)))
        }
        PositionInfo::OccupiedBySelf => None,
    }
}

#[derive(Debug)]
enum Step<B: Board> {
    Move(Coords, Coords, B),
    Jump(Coords, Coords, Coords, B),
}

impl<B: Board> PartialEq for Step<B> {
    fn eq(&self, other: &Self) -> bool {
        use Step::*;
        match (self, other) {
            (Move(a_from, a_to, _), Move(b_from, b_to, _)) => a_from == b_from && a_to == b_to,
            (Jump(a_from, a_over, a_to, _), Jump(b_from, b_over, b_to, _)) => {
                a_from == b_from && a_to == b_to && a_over == b_over
            }
            _ => false,
        }
    }
}

impl<B: Board> Step<B> {
    /// Gets the piece after step is taken.
    fn to(&self) -> &B::Piece {
        match self {
            Step::Move(_, to, board) | Step::Jump(_, _, to, board) => board
                .get(*to)
                .expect("could not find a piece on the board at the step's target position"),
        }
    }

    /// Gets the board after step is taken.
    fn board(&self) -> &B {
        match self {
            Step::Move(_, _, board) | Step::Jump(_, _, _, board) => board,
        }
    }

    fn is_jump(&self) -> bool {
        match self {
            Step::Jump(_, _, _, _) => true,
            _ => false,
        }
    }

    /// Create a step that moves piece to new_coords on board.
    fn make_simple_move(new_coords: Coords, piece: &B::Piece, board: &B) -> Self {
        let new_board = board.move_piece(piece.coords(), new_coords).unwrap();
        Step::Move(piece.coords(), new_coords, new_board)
    }

    /// Create a step that moves piece to new_coords
    /// and removes the opponent's piece at jumped_coords
    fn make_jump(new_coords: Coords, jumped_coords: Coords, piece: &B::Piece, board: &B) -> Self {
        let new_board = board
            .move_piece(piece.coords(), new_coords)
            .unwrap()
            .remove_piece_at(jumped_coords)
            .unwrap();
        Step::Jump(piece.coords(), jumped_coords, new_coords, new_board)
    }

    /// Create a step that replaces old_piece with new_piece on board
    fn replace_piece(new_piece: B::Piece, old_piece: &B::Piece, board: &B) -> Self {
        unimplemented!()
    }
}

trait Path<B: Board> {
    fn append(&self, step: Step<B>) -> Self;
    fn contains_jumps(&self) -> bool;
}

mod domain_model {
    pub trait Board: Clone {
        type Piece: Piece;
        type Pieces: Iterator<Item = Self::Piece>;

        /// Get a list of pieces belonging to the current player.
        fn current_pieces(&self) -> Self::Pieces;

        /// Test if the given coords specify a position on the board.
        fn is_position_on_board(&self, coords: Coords) -> bool;

        /// Return a reference to the piece  at the position specified by coords.
        fn get(&self, coords: Coords) -> Option<&Self::Piece>;

        /// Describe what occupies the position in coords {
        fn position_info(&self, coords: Coords) -> PositionInfo;

        /// Equivalent to position_info returning Unoccupied
        fn is_position_unoccupied(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::Unoccupied)
        }

        /// Equivalent to position_info returning OccupiedBySelf
        fn is_position_occupied_by_self(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::OccupiedBySelf)
        }

        /// Equivalent to position_info returning OccupiedByOpponent
        fn is_position_occupied_by_opponent(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::OccupiedByOpponent)
        }

        fn move_piece(&self, old_coords: Coords, new_coords: Coords) -> Option<Self>;
        fn remove_piece_at(&self, coords: Coords) -> Option<Self>;
    }

    pub trait Piece {
        type Directions: Iterator<Item = Direction>;

        /// Get the coordinates of the piece.
        fn coords(&self) -> Coords;

        /// Test if piece should be crowned.
        fn should_be_crowned(&self) -> bool;

        /// Get a new identical piece except that it is a king.
        fn crown(&self) -> Self;

        /// Get a list of directions that piece may consider for a move.
        fn possible_directions(&self) -> Self::Directions;

        fn move_to(&self, coords: Coords) -> Self;
    }

    pub enum PositionInfo {
        Unoccupied,
        OccupiedBySelf,
        OccupiedByOpponent,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Coords {
        pub row: i16,
        pub col: i16,
    }

    impl Coords {
        pub fn new(row: i16, col: i16) -> Self {
            Self { row, col }
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Offset {
        pub row_step: i16,
        pub col_step: i16,
    }

    impl Offset {
        pub fn new(row: i16, col: i16) -> Self {
            Self {
                row_step: row,
                col_step: col,
            }
        }
    }

    pub type Direction = Offset;

    impl std::ops::Add<Offset> for Coords {
        type Output = Coords;
        fn add(self, ofs: Offset) -> Self::Output {
            Coords {
                row: self.row + ofs.row_step,
                col: self.col + ofs.col_step,
            }
        }
    }

    impl std::ops::Add<Coords> for Offset {
        type Output = Coords;
        fn add(self, pos: Coords) -> Self::Output {
            Coords {
                row: self.row_step + pos.row,
                col: self.col_step + pos.col,
            }
        }
    }

    impl std::ops::Sub<Coords> for Coords {
        type Output = Offset;
        fn sub(self, rhs: Coords) -> Self::Output {
            Offset {
                row_step: self.row - rhs.row,
                col_step: self.col - rhs.col,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::list::{ListView, SharedList};
    use Color::*;

    const CHECKERS_BOARD_SIZE: i16 = 8;

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Color {
        White,
        Black,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    struct CheckersPiece {
        coords: Coords,
        color: Color,
    }

    impl Piece for CheckersPiece {
        type Directions = std::array::IntoIter<Direction, 4>;
        fn coords(&self) -> Coords {
            self.coords
        }
        fn should_be_crowned(&self) -> bool {
            unimplemented!()
        }
        fn crown(&self) -> Self {
            unimplemented!()
        }
        fn possible_directions(&self) -> Self::Directions {
            unimplemented!()
        }
        fn move_to(&self, coords: Coords) -> Self {
            CheckersPiece {
                coords,
                color: self.color,
            }
        }
    }

    impl CheckersPiece {
        pub fn new(coords: Coords, color: Color) -> Self {
            Self { coords, color }
        }
    }

    #[derive(Debug, Clone)]
    struct CheckersBoard {
        current_player: Color,
        pieces: Vec<CheckersPiece>,
    }

    impl Board for CheckersBoard {
        type Piece = CheckersPiece;
        type Pieces = std::vec::IntoIter<CheckersPiece>;
        fn current_pieces(&self) -> Self::Pieces {
            unimplemented!()
        }
        fn is_position_on_board(&self, coords: Coords) -> bool {
            coords.row > 0
                && coords.col > 0
                && coords.row <= CHECKERS_BOARD_SIZE
                && coords.col <= CHECKERS_BOARD_SIZE
        }
        fn get(&self, coords: Coords) -> Option<&Self::Piece> {
            self.pieces.iter().find(|piece| piece.coords() == coords)
        }
        fn position_info(&self, coords: Coords) -> PositionInfo {
            if let Some(piece) = self.get(coords) {
                if piece.color == self.current_player {
                    PositionInfo::OccupiedBySelf
                } else {
                    PositionInfo::OccupiedByOpponent
                }
            } else {
                PositionInfo::Unoccupied
            }
        }

        fn move_piece(&self, old_coords: Coords, new_coords: Coords) -> Option<Self> {
            let idx = self
                .pieces
                .iter()
                .map(Self::Piece::coords)
                .position(|coords| coords == old_coords)?;
            let mut new_board = self.clone();
            new_board.pieces[idx] = new_board.pieces[idx].move_to(new_coords);
            Some(new_board)
        }

        fn remove_piece_at(&self, coords: Coords) -> Option<Self> {
            let idx = self
                .pieces
                .iter()
                .map(Self::Piece::coords)
                .position(|c| c == coords)?;
            let mut new_board = self.clone();
            new_board.pieces.swap_remove(idx);
            Some(new_board)
        }
    }

    impl CheckersBoard {
        pub fn new() -> Self {
            CheckersBoard {
                current_player: Color::White,
                pieces: vec![],
            }
        }

        fn with_piece(mut self, piece: CheckersPiece) -> Self {
            self.pieces.push(piece);
            self
        }
    }

    impl Path<CheckersBoard> for SharedList<Step<CheckersBoard>> {
        fn append(&self, step: Step<CheckersBoard>) -> Self {
            ListView::append(self, step)
        }

        fn contains_jumps(&self) -> bool {
            match self {
                SharedList::Empty => false,
                SharedList::Cons(entry) => entry.0.is_jump() || entry.1.contains_jumps(),
            }
        }
    }

    #[test]
    fn move_step() {
        let old_coords = Coords::new(3, 3);
        let new_coords = Coords::new(4, 2);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(old_coords, Black));
        let piece = board.get(old_coords).unwrap();

        let step = Step::make_simple_move(new_coords, piece, &board);

        let expected_piece = CheckersPiece::new(new_coords, Black);

        assert_eq!(step.to(), &expected_piece);
        assert!(step.board().is_position_unoccupied(old_coords));
        assert_eq!(step.board().get(new_coords), Some(&expected_piece));
    }

    #[test]
    fn jump_step() {
        let old_coords = Coords::new(3, 3);
        let jump_coords = Coords::new(4, 2);
        let new_coords = Coords::new(5, 1);
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(old_coords, Black))
            .with_piece(CheckersPiece::new(jump_coords, White));
        let piece = board.get(old_coords).unwrap();

        let step = Step::make_jump(new_coords, jump_coords, piece, &board);

        let expected_piece = CheckersPiece::new(new_coords, Black);

        assert_eq!(step.to(), &expected_piece);
        assert!(step.board().is_position_unoccupied(old_coords));
        assert!(step.board().is_position_unoccupied(jump_coords));
        assert_eq!(step.board().get(new_coords), Some(&expected_piece));
    }

    #[test]
    fn try_step_fails_if_new_position_is_not_on_board() {
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(Coords::new(1, 1), White));
        let piece = board.get(Coords::new(1, 1)).unwrap();
        let direction = Direction::new(-1, -1);
        let path = SharedList::Empty;
        assert_eq!(try_step(piece, &board, direction, &path), None);
    }

    #[test]
    fn try_step_fails_if_new_position_is_occupied_by_self() {
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(Coords::new(1, 1), White))
            .with_piece(CheckersPiece::new(Coords::new(2, 2), White));
        let piece = board.get(Coords::new(1, 1)).unwrap();
        let direction = Direction::new(1, 1);
        let path = SharedList::Empty;
        assert_eq!(try_step(piece, &board, direction, &path), None);
    }

    #[test]
    fn try_step_fails_if_new_position_free_but_path_contains_jumps() {
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(Coords::new(3, 3), White))
            .with_piece(CheckersPiece::new(Coords::new(2, 2), Black));
        let piece = board.get(Coords::new(3, 3)).unwrap();
        let direction = Direction::new(1, 1);
        let path = SharedList::new(Step::make_jump(
            Coords::new(3, 3),
            Coords::new(2, 2),
            piece,
            &board,
        ));
        assert_eq!(try_step(piece, &board, direction, &path), None);
    }

    #[test]
    fn try_step_succeeds_if_new_position_free() {
        let coords = Coords::new(1, 1);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(coords, White));
        let piece = board.get(coords).unwrap();
        let direction = Direction::new(1, 1);
        let path = SharedList::Empty;
        let expected = SharedList::new(Step::make_simple_move(coords + direction, piece, &board));
        assert_eq!(try_step(piece, &board, direction, &path), Some(expected));
    }

    #[test]
    fn try_step_fails_if_opponent_on_new_position_and_jump_is_off_board() {
        let piece_coords = Coords::new(2, 2);
        let target_coords = Coords::new(1, 1);
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(piece_coords, White))
            .with_piece(CheckersPiece::new(target_coords, Black));

        let piece = board.get(piece_coords).unwrap();
        let direction = target_coords - piece_coords;
        let path = SharedList::Empty;

        assert_eq!(try_step(piece, &board, direction, &path), None);
    }

    #[test]
    fn try_step_fails_if_opponent_on_new_position_and_jump_landing_is_occupied() {
        let piece_coords = Coords::new(1, 1);
        let target_coords = Coords::new(2, 2);
        let landing_coords = Coords::new(3, 3);
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(piece_coords, White))
            .with_piece(CheckersPiece::new(target_coords, Black))
            .with_piece(CheckersPiece::new(landing_coords, Black));

        let piece = board.get(piece_coords).unwrap();
        let direction = target_coords - piece_coords;
        let path = SharedList::Empty;

        assert_eq!(try_step(piece, &board, direction, &path), None);
    }

    #[test]
    fn try_step_succeeds_if_opponent_on_new_position_and_jump_landing_is_free() {
        let piece_coords = Coords::new(1, 1);
        let target_coords = Coords::new(2, 2);
        let landing_coords = Coords::new(3, 3);
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(piece_coords, White))
            .with_piece(CheckersPiece::new(target_coords, Black));

        let piece = board.get(piece_coords).unwrap();
        let direction = target_coords - piece_coords;
        let path = SharedList::Empty;

        let expected = SharedList::new(Step::make_jump(
            landing_coords,
            target_coords,
            piece,
            &board,
        ));
        assert_eq!(try_step(piece, &board, direction, &path), Some(expected));
    }

    #[test]
    fn compute_next_steps_returns_none_for_piece_that_cant_move() {
        let piece_coords = Coords::new(CHECKERS_BOARD_SIZE, CHECKERS_BOARD_SIZE);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(piece_coords, White));
        let piece = board.get(piece_coords).unwrap();
        let path = SharedList::Empty;

        assert_eq!(compute_next_steps(piece, board, path), None);
    }
}
