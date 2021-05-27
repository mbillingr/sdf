use crate::board_game::factored::domain_model::{Game, PartialMove};
use std::sync::Arc;

mod domain_model {
    use std::collections::HashSet;
    use std::fmt::Debug;
    use std::sync::Arc;

    pub type PMoveCollection<G> = Vec<PartialMove<G>>;
    pub type EvolutionRule<G> = Arc<dyn Fn(PartialMove<G>) -> PMoveCollection<G>>;
    pub type AggregateRule<G> = Arc<dyn Fn(Vec<PartialMove<G>>) -> PMoveCollection<G>>;

    fn compose_rules<G: Game>(f: AggregateRule<G>, g: AggregateRule<G>) -> AggregateRule<G> {
        Arc::new(move |pmoves| f(g(pmoves)))
    }

    pub trait Movable {
        fn possible_directions(&self) -> Vec<Direction>;
    }

    pub trait Game: 'static + Clone {
        type Players: Debug + Clone + PartialEq;
        type PieceKind: Debug + Clone + Movable + PartialEq;

        const BOARD_MIN_ROW: i16;
        const BOARD_MAX_ROW: i16;
        const BOARD_MIN_COL: i16;
        const BOARD_MAX_COL: i16;

        fn start_player() -> Self::Players;
        fn evolution_rules(&self) -> &[EvolutionRule<Self>];
        fn aggregate_rules(&self) -> &[AggregateRule<Self>];

        fn execute_game_rules(&self, board: &Board<Self>) -> PMoveCollection<Self> {
            Self::execute_rules(
                self.initial_pmoves(board),
                self.evolution_rules(),
                self.aggregate_rules(),
            )
        }

        fn initial_pmoves(&self, board: &Board<Self>) -> PMoveCollection<Self> {
            board
                .current_pieces()
                .map(|piece| PartialMove::initial(board.clone(), piece))
                .collect()
        }

        fn execute_rules(
            initial_pmoves: PMoveCollection<Self>,
            evolution_rules: &[EvolutionRule<Self>],
            aggregate_rules: &[AggregateRule<Self>],
        ) -> PMoveCollection<Self> {
            let agg: AggregateRule<_> = aggregate_rules
                .iter()
                .cloned()
                .fold(Arc::new(|x| x), compose_rules);
            agg(initial_pmoves
                .into_iter()
                .flat_map(|pmove| Self::evolve_pmove(pmove, evolution_rules))
                .collect())
        }

        fn evolve_pmove(
            pmove: PartialMove<Self>,
            evolution_rules: &[EvolutionRule<Self>],
        ) -> PMoveCollection<Self> {
            evolution_rules
                .iter()
                .flat_map(|rule| rule(pmove.clone()))
                .flat_map(|new_pmove| {
                    if new_pmove.is_finished() {
                        vec![new_pmove]
                    } else {
                        Self::evolve_pmove(new_pmove, evolution_rules)
                    }
                })
                .collect()
        }
    }

    #[derive(Clone)]
    pub struct Board<G: Game> {
        current_player: G::Players,
        pieces: Vec<Piece<G>>,
    }

    impl<G: Game> Board<G> {
        pub fn new() -> Self {
            Board {
                current_player: G::start_player(),
                pieces: vec![],
            }
        }

        /// Get a list of pieces belonging to the current player.
        fn current_pieces(&self) -> impl Iterator<Item = Piece<G>> + '_ {
            let current_player = self.current_player.clone();
            self.pieces
                .iter()
                .filter(move |piece| piece.owner == current_player)
                .cloned()
        }

        /// Test if the given coords specify a position on the board.
        pub fn is_position_on_board(&self, coords: Coords) -> bool {
            coords.row >= G::BOARD_MIN_ROW
                && coords.row <= G::BOARD_MAX_ROW
                && coords.col >= G::BOARD_MIN_COL
                && coords.col <= G::BOARD_MAX_COL
        }

        /// Return a reference to the piece at the position specified by coords.
        pub fn get(&self, coords: Coords) -> Option<&Piece<G>> {
            self.pieces.iter().find(|piece| piece.coords() == coords)
        }

        /// Return a mutable reference to the piece at the position specified by coords.
        pub fn get_mut(&mut self, coords: Coords) -> Option<&mut Piece<G>> {
            self.pieces
                .iter_mut()
                .find(|piece| piece.coords() == coords)
        }

        /// Describe what occupies the position in coords {
        pub fn position_info(&self, coords: Coords) -> PositionInfo {
            if let Some(piece) = self.get(coords) {
                if piece.owner == self.current_player {
                    PositionInfo::OccupiedBySelf
                } else {
                    PositionInfo::OccupiedByOpponent
                }
            } else {
                PositionInfo::Unoccupied
            }
        }

        /// Equivalent to position_info returning Unoccupied
        pub fn is_position_unoccupied(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::Unoccupied)
        }

        /// Equivalent to position_info returning OccupiedBySelf
        pub fn is_position_occupied_by_self(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::OccupiedBySelf)
        }

        /// Equivalent to position_info returning OccupiedByOpponent
        pub fn is_position_occupied_by_opponent(&self, coords: Coords) -> bool {
            matches!(self.position_info(coords), PositionInfo::OccupiedByOpponent)
        }

        pub fn insert_piece(&self, piece: Piece<G>) -> Self {
            let mut new_board = self.clone();
            new_board.pieces.push(piece);
            new_board
        }

        fn move_piece(&self, old_coords: Coords, new_coords: Coords) -> Option<Self> {
            Some(Board {
                current_player: self.current_player.clone(),
                pieces: self
                    .pieces
                    .iter()
                    .map(|piece| {
                        if piece.coords() == old_coords {
                            piece.move_to(new_coords)
                        } else {
                            piece.clone()
                        }
                    })
                    .collect(),
            })
        }

        fn remove_piece_at(&self, coords: Coords) -> Option<Self> {
            Some(Board {
                current_player: self.current_player.clone(),
                pieces: self
                    .pieces
                    .iter()
                    .filter(|piece| piece.coords() != coords)
                    .cloned()
                    .collect(),
            })
        }
    }

    impl<G: Game> PartialEq for Board<G> {
        fn eq(&self, other: &Self) -> bool {
            self.current_player == other.current_player && self.pieces == other.pieces
        }
    }

    impl<G: Game> std::fmt::Debug for Board<G> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "Board{{current_player={:?}, pieces={:?}}}",
                self.current_player, self.pieces
            )
        }
    }

    #[derive(Copy, Clone)]
    pub struct Piece<G: Game> {
        coords: Coords,
        owner: G::Players,
        kind: G::PieceKind,
    }

    impl<G: Game> PartialEq for Piece<G> {
        fn eq(&self, other: &Self) -> bool {
            self.coords == other.coords && self.owner == other.owner && self.kind == other.kind
        }
    }

    impl<G: Game> Piece<G> {
        pub fn new(coords: Coords, kind: G::PieceKind, owner: G::Players) -> Self {
            Piece {
                coords,
                owner,
                kind,
            }
        }

        /// Get the coordinates of the piece.
        fn coords(&self) -> Coords {
            self.coords
        }

        /// Get the type of the piece.
        fn kind(&self) -> &G::PieceKind {
            &self.kind
        }

        /// Get a new identical piece except that it has given type.
        fn new_kind(&self, kind: G::PieceKind) -> Self {
            Piece {
                coords: self.coords,
                owner: self.owner.clone(),
                kind,
            }
        }

        /// Get a list of directions that piece may consider for a move.
        pub fn possible_directions(&self) -> Vec<Direction> {
            self.kind.possible_directions()
        }

        fn move_to(&self, coords: Coords) -> Self {
            Piece {
                coords: coords,
                kind: self.kind.clone(),
                owner: self.owner.clone(),
            }
        }
    }

    impl<G: Game> std::fmt::Debug for Piece<G> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "<{:?} {:?} @ {:?}", self.owner, self.kind, self.coords)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum PartialMove<G: Game> {
        Initial(Board<G>, Piece<G>),
        Move(Coords, Coords, Box<Self>),
        Capture(Coords, Box<Self>),
        Finished(Box<Self>),
    }

    impl<G: Game> PartialMove<G> {
        pub fn initial(board: Board<G>, piece: Piece<G>) -> Self {
            PartialMove::Initial(board, piece)
        }

        pub fn is_empty(&self) -> bool {
            match self {
                PartialMove::Initial(_, _) => true,
                PartialMove::Move(_, _, _) | PartialMove::Capture(_, _) => false,
                PartialMove::Finished(pmove) => pmove.is_empty(),
            }
        }

        pub fn is_finished(&self) -> bool {
            match self {
                PartialMove::Initial(_, _) => false,
                PartialMove::Move(_, _, _) | PartialMove::Capture(_, _) => false,
                PartialMove::Finished(_) => true,
            }
        }

        pub fn current_board(&self) -> Board<G> {
            self.build_board()
        }

        fn build_board(&self) -> Board<G> {
            match self {
                PartialMove::Initial(board, _) => board.clone(),
                PartialMove::Move(old_coords, new_coords, parent) => parent
                    .build_board()
                    .move_piece(*old_coords, *new_coords)
                    .unwrap(),
                PartialMove::Capture(coords, parent) => {
                    parent.build_board().remove_piece_at(*coords).unwrap()
                }
                PartialMove::Finished(pmove) => pmove.build_board(),
            }
        }

        pub fn current_piece(&self) -> Piece<G> {
            match self {
                PartialMove::Initial(_, piece) => piece.clone(),
                PartialMove::Move(_, coords, parent) => parent.current_piece().move_to(*coords),
                PartialMove::Capture(_, parent) => parent.current_piece(),
                PartialMove::Finished(parent) => parent.current_piece(),
            }
        }

        pub fn new_piece_position(self, coords: Coords) -> Self {
            let old_coords = self.current_piece().coords;
            PartialMove::Move(old_coords, coords, Box::new(self))
        }

        pub fn update_piece(&self, proc: impl Fn(&Piece<G>) -> Piece<G>) -> Self {
            unimplemented!()
        }

        pub fn finish_move(self) -> Self {
            PartialMove::Finished(Box::new(self))
        }

        pub fn capture_piece_at(self, coords: Coords) -> Self {
            PartialMove::Capture(coords, Box::new(self))
        }

        pub fn does_capture_pieces(&self) -> bool {
            unimplemented!()
        }

        pub fn compute_new_position(&self, direction: Direction, distance: i16) -> Coords {
            self.current_piece().coords + direction * distance
        }
    }

    struct Change<G: Game> {
        board: Board<G>,
        piece: Piece<G>,
        flags: Flags,
    }

    impl<G: Game> Change<G> {
        fn new(board: Board<G>, piece: Piece<G>, flags: Flags) -> Self {
            Change {
                board,
                piece,
                flags,
            }
        }

        fn board(&self) -> &Board<G> {
            &self.board
        }

        fn piece(&self) -> &Piece<G> {
            &self.piece
        }

        fn flags(&self) -> &Flags {
            &self.flags
        }
    }

    pub struct Flags(HashSet<&'static str>);

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

    impl std::ops::Mul<i16> for Direction {
        type Output = Direction;
        fn mul(self, distance: i16) -> Self::Output {
            Direction {
                row_step: self.row_step * distance,
                col_step: self.col_step * distance,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::board_game::factored::domain_model::{
        AggregateRule, Board, Coords, Direction, EvolutionRule, Movable, PMoveCollection, Piece,
    };

    #[derive(Debug, Copy, Clone, PartialEq)]
    struct TestGame;
    impl Game for TestGame {
        type Players = i8;
        type PieceKind = i8;

        const BOARD_MIN_ROW: i16 = 0;
        const BOARD_MAX_ROW: i16 = 9;
        const BOARD_MIN_COL: i16 = 0;
        const BOARD_MAX_COL: i16 = 9;

        fn start_player() -> Self::Players {
            0
        }

        fn evolution_rules(&self) -> &[EvolutionRule<Self>] {
            unimplemented!()
        }

        fn aggregate_rules(&self) -> &[AggregateRule<Self>] {
            unimplemented!()
        }
    }

    impl TestGame {
        fn new_board() -> Board<Self> {
            Board::new()
        }
    }

    impl Movable for i8 {
        fn possible_directions(&self) -> Vec<Direction> {
            unimplemented!()
        }
    }

    #[test]
    fn execute_rules_no_moves_no_rules() {
        let resulting_moves = TestGame::execute_rules(vec![], &[], &[]);
        assert_eq!(resulting_moves, vec![]);
    }

    #[test]
    fn execute_rules_one_move_trivial_rules() {
        let piece = Piece::new(Coords::new(0, 0), 0, 0);
        let board = TestGame::new_board().insert_piece(piece);

        fn finish(pmove: PartialMove<TestGame>) -> PMoveCollection<TestGame> {
            vec![pmove.finish_move()]
        }

        let resulting_moves = TestGame::execute_rules(
            vec![PartialMove::initial(board, piece)],
            &[Arc::new(finish)],
            &[],
        );
        assert_eq!(resulting_moves.len(), 1);
    }
}

mod test_checkers {
    use crate::board_game::factored::domain_model::{
        AggregateRule, Board, Coords, Direction, EvolutionRule, Game, Movable, PMoveCollection,
        PartialMove, Piece,
    };
    use std::sync::Arc;
    use Color::*;

    #[derive(Clone)]
    struct Checkers {
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
            White
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
                aggregate_rules: vec![],
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
                    if board.is_position_on_board(landing) && board.is_position_unoccupied(landing)
                    {
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
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Color {
        White,
        Black,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum CheckersPiece {
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
}
