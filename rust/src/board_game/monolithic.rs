use domain_model::{Board, Coords, Direction, Piece, PositionInfo};

pub fn generate_moves<'a, B: Board, P: 'a + Path<B>>(
    board: &'a B,
) -> Box<dyn 'a + Iterator<Item = P>> {
    Box::new(crown_kings(mandate_jumps(
        board
            .current_pieces()
            .iter()
            .flat_map(move |piece| evolve_paths(piece, board)),
    )))
}

fn crown_kings<'a, B: Board, P: 'a + Path<B>>(
    paths: impl 'a + Iterator<Item = P>,
) -> impl 'a + Iterator<Item = P> {
    paths.map(|path| {
        let piece = path.last_step().unwrap().to();
        if piece.should_be_crowned() {
            path.append(Step::replace_piece(
                piece.crown(),
                piece,
                path.last_step().unwrap().board(),
            ))
        } else {
            path
        }
    })
}

fn mandate_jumps<'a, B: Board, P: 'a + Path<B>>(
    paths: impl 'a + Iterator<Item = P>,
) -> impl 'a + Iterator<Item = P> {
    let paths: Vec<_> = paths.collect();
    let jumps: Vec<_> = paths
        .iter()
        .filter(|p| p.contains_jumps())
        .cloned()
        .collect();
    if jumps.is_empty() {
        paths.into_iter()
    } else {
        jumps.into_iter()
    }
}

fn evolve_paths<'a, B: Board, P: 'a + Path<B>>(
    piece: &'a B::Piece,
    board: &'a B,
) -> Box<dyn Iterator<Item = P> + 'a> {
    let mut paths = vec![];
    let mut jumps = vec![];
    for path in compute_next_steps::<B, P>(piece, board, Path::empty()) {
        if path.contains_jumps() {
            jumps.push(path);
        } else {
            paths.push(path);
        }
    }

    if jumps.is_empty() {
        Box::new(paths.into_iter())
    } else {
        Box::new(evolve_jumps(jumps.into_iter()))
    }
}

fn evolve_jumps<'a, B: Board, P: 'a + Path<B>>(
    paths: impl 'a + Iterator<Item = P>,
) -> impl 'a + Iterator<Item = P> {
    paths.flat_map(|path| {
        let step = path.last_step().unwrap();
        let paths: Vec<_> = compute_next_steps(step.to(), step.board(), path.clone()).collect();
        if paths.is_empty() {
            vec![path]
        } else {
            evolve_jumps(paths.into_iter()).collect()
        }
        .into_iter()
    })
}

fn compute_next_steps<'a, B: Board, P: 'a + Path<B>>(
    piece: &'a B::Piece,
    board: &'a B,
    path: P,
) -> impl Iterator<Item = P> + 'a {
    piece
        .possible_directions()
        .filter_map(move |direction| try_step(piece, board, direction, path.clone()))
}

fn try_step<B: Board, P: Path<B>>(
    piece: &B::Piece,
    board: &B,
    direction: Direction,
    path: P,
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

#[derive(Debug, PartialEq)]
pub enum Step<B: Board> {
    Move(Coords, Coords, B),
    Jump(Coords, Coords, Coords, B),
    Replace(Coords, B),
}

impl<B: Board> Step<B> {
    /// Gets the piece after step is taken.
    fn to(&self) -> &B::Piece {
        match self {
            Step::Move(_, to, board) | Step::Jump(_, _, to, board) | Step::Replace(to, board) => {
                board
                    .get(*to)
                    .expect("could not find a piece on the board at the step's target position")
            }
        }
    }

    /// Gets the board after step is taken.
    fn board(&self) -> &B {
        match self {
            Step::Move(_, _, board) | Step::Jump(_, _, _, board) | Step::Replace(_, board) => board,
        }
    }

    pub fn is_jump(&self) -> bool {
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
        assert_eq!(new_piece.coords(), old_piece.coords());
        let new_board = board
            .remove_piece_at(old_piece.coords())
            .unwrap()
            .insert_piece(new_piece);
        Step::Replace(old_piece.coords(), new_board)
    }
}

pub trait Path<B: Board>: Clone {
    fn empty() -> Self;
    fn append(&self, step: Step<B>) -> Self;
    fn last_step(&self) -> Option<&Step<B>>;
    fn contains_jumps(&self) -> bool;
}

mod domain_model {
    pub trait Board: Clone {
        type Piece: Piece;

        /// Get a list of pieces belonging to the current player.
        fn current_pieces(&self) -> &[Self::Piece];

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

        fn insert_piece(&self, piece: Self::Piece) -> Self;
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
        crowned: bool,
    }

    impl Piece for CheckersPiece {
        type Directions = std::iter::Take<std::array::IntoIter<Direction, 4>>;
        fn coords(&self) -> Coords {
            self.coords
        }
        fn should_be_crowned(&self) -> bool {
            self.coords.row == CHECKERS_BOARD_SIZE
        }
        fn crown(&self) -> Self {
            let mut new_king = self.clone();
            new_king.crowned = true;
            new_king
        }
        fn possible_directions(&self) -> Self::Directions {
            let n = if self.crowned { 4 } else { 2 };
            std::array::IntoIter::new([
                Direction::new(1, 1),
                Direction::new(1, -1),
                Direction::new(-1, 1),
                Direction::new(-1, -1),
            ])
            .take(n)
        }
        fn move_to(&self, coords: Coords) -> Self {
            CheckersPiece {
                coords,
                color: self.color,
                crowned: self.crowned,
            }
        }
    }

    impl CheckersPiece {
        pub fn new(coords: Coords, color: Color) -> Self {
            Self {
                coords,
                color,
                crowned: false,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    struct CheckersBoard {
        current_player: Color,
        pieces: Vec<CheckersPiece>,
    }

    impl Board for CheckersBoard {
        type Piece = CheckersPiece;
        fn current_pieces(&self) -> &[Self::Piece] {
            &self.pieces
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

        fn insert_piece(&self, piece: Self::Piece) -> Self {
            let mut new_board = self.clone();
            new_board.pieces.push(piece);
            new_board
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
        fn empty() -> Self {
            SharedList::Empty
        }

        fn append(&self, step: Step<CheckersBoard>) -> Self {
            ListView::append(self, step)
        }

        fn last_step(&self) -> Option<&Step<CheckersBoard>> {
            ListView::head(self)
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
        assert_eq!(try_step(piece, &board, direction, path), None);
    }

    #[test]
    fn try_step_fails_if_new_position_is_occupied_by_self() {
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(Coords::new(1, 1), White))
            .with_piece(CheckersPiece::new(Coords::new(2, 2), White));
        let piece = board.get(Coords::new(1, 1)).unwrap();
        let direction = Direction::new(1, 1);
        let path = SharedList::Empty;
        assert_eq!(try_step(piece, &board, direction, path), None);
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
        assert_eq!(try_step(piece, &board, direction, path), None);
    }

    #[test]
    fn try_step_succeeds_if_new_position_free() {
        let coords = Coords::new(1, 1);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(coords, White));
        let piece = board.get(coords).unwrap();
        let direction = Direction::new(1, 1);
        let path = SharedList::Empty;
        let expected = SharedList::new(Step::make_simple_move(coords + direction, piece, &board));
        assert_eq!(try_step(piece, &board, direction, path), Some(expected));
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

        assert_eq!(try_step(piece, &board, direction, path), None);
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

        assert_eq!(try_step(piece, &board, direction, path), None);
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
        assert_eq!(try_step(piece, &board, direction, path), Some(expected));
    }

    #[test]
    fn compute_next_steps_returns_none_for_piece_that_cant_move() {
        let piece_coords = Coords::new(CHECKERS_BOARD_SIZE, CHECKERS_BOARD_SIZE);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(piece_coords, White));
        let piece = board.get(piece_coords).unwrap();
        let path = SharedList::Empty;

        assert_eq!(compute_next_steps(piece, &board, path).next(), None);
    }

    #[test]
    fn compute_next_steps_returns_two_moves_for_a_free_piece() {
        let piece_coords = Coords::new(3, 3);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(piece_coords, White));
        let piece = board.get(piece_coords).unwrap();
        let path = SharedList::Empty;

        let moves: Vec<_> = compute_next_steps(piece, &board, path).collect();

        let move_left_coords = Coords::new(4, 2);
        let move_right_coords = Coords::new(4, 4);

        assert_eq!(moves.len(), 2);
        assert!(moves.contains(&SharedList::new(Step::make_simple_move(
            move_left_coords,
            piece,
            &board
        ))));
        assert!(moves.contains(&SharedList::new(Step::make_simple_move(
            move_right_coords,
            piece,
            &board
        ))));
    }

    #[test]
    fn evolve_paths_allows_all_paths_if_none_is_a_jump() {
        let piece_coords = Coords::new(3, 3);
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(piece_coords, White));
        let piece = board.get(piece_coords).unwrap();

        let moves: Vec<_> = evolve_paths::<_, SharedList<_>>(piece, &board).collect();
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn evolve_paths_forces_jump() {
        let piece_coords = Coords::new(3, 3);
        let jumped_coords = Coords::new(4, 4);
        let landing_coords = Coords::new(5, 5);
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(piece_coords, White))
            .with_piece(CheckersPiece::new(jumped_coords, Black));
        let piece = board.get(piece_coords).unwrap();

        let moves: Vec<_> = evolve_paths::<_, SharedList<_>>(piece, &board).collect();
        assert_eq!(
            moves,
            vec![SharedList::new(Step::make_jump(
                landing_coords,
                jumped_coords,
                piece,
                &board
            ))]
        );
    }

    #[test]
    fn evolve_jump_computes_chain_of_jumps() {
        let piece_coords = Coords::new(3, 3);
        let jump1_coords = Coords::new(4, 4);
        let land1_coords = Coords::new(5, 5);
        let jump2_coords = Coords::new(6, 6);
        let land2_coords = Coords::new(7, 7);

        let board0 = CheckersBoard::new()
            .with_piece(CheckersPiece::new(piece_coords, White))
            .with_piece(CheckersPiece::new(jump1_coords, Black))
            .with_piece(CheckersPiece::new(jump2_coords, Black));
        let piece0 = board0.get(piece_coords).unwrap();

        let board1 = CheckersBoard::new()
            .with_piece(CheckersPiece::new(land1_coords, White))
            .with_piece(CheckersPiece::new(jump2_coords, Black));
        let piece1 = board1.get(land1_coords).unwrap();

        let expected_path = Path::append(
            &SharedList::new(Step::make_jump(land1_coords, jump1_coords, piece0, &board0)),
            Step::make_jump(land2_coords, jump2_coords, piece1, &board1),
        );

        let moves: Vec<_> = evolve_paths::<_, SharedList<_>>(piece0, &board0).collect();
        assert_eq!(moves, vec![expected_path]);
    }

    #[test]
    fn generate_moves_no_move_possible() {
        let board = CheckersBoard::new();
        let moves: Vec<SharedList<_>> = generate_moves(&board).collect();
        assert_eq!(moves, vec![]);
    }

    #[test]
    fn generate_moves_for_one_piece() {
        let board = CheckersBoard::new().with_piece(CheckersPiece::new(Coords::new(3, 3), White));

        let moves: Vec<SharedList<_>> = generate_moves(&board).collect();
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn generate_moves_for_two_pieces() {
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(Coords::new(3, 3), White))
            .with_piece(CheckersPiece::new(Coords::new(3, 5), White));

        let moves: Vec<SharedList<_>> = generate_moves(&board).collect();
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn generate_constrained_moves_for_two_pieces() {
        let board = CheckersBoard::new()
            .with_piece(CheckersPiece::new(Coords::new(3, 3), White))
            .with_piece(CheckersPiece::new(Coords::new(2, 4), White));

        let moves: Vec<SharedList<_>> = generate_moves(&board).collect();
        assert_eq!(moves.len(), 3);
    }

    #[test]
    fn crown_pieces_that_move_to_home_row() {
        let start_coords = Coords::new(CHECKERS_BOARD_SIZE - 1, 1);
        let crown_coords = Coords::new(CHECKERS_BOARD_SIZE, 2);
        let board0 = CheckersBoard::new().with_piece(CheckersPiece::new(start_coords, White));
        let piece = board0.get(start_coords).unwrap();

        let board1 = CheckersBoard::new().with_piece(CheckersPiece::new(crown_coords, White));
        let moved_piece = board1.get(crown_coords).unwrap();
        let crowned_piece = moved_piece.crown();

        let expected_path = Path::append(
            &SharedList::new(Step::make_simple_move(crown_coords, piece, &board0)),
            Step::replace_piece(crowned_piece, moved_piece, &board1),
        );

        let moves: Vec<SharedList<_>> = generate_moves(&board0).collect();
        assert_eq!(moves, vec![expected_path]);
    }
}
