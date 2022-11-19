namespace TicTacToe {

    pub type alias Position = Int32

    pub enum Piece with Eq {
        case X
        case O
    }

    instance ToString[Piece] {
        pub def toString(x: Piece): String = match x {
            case X => "X"
            case O => "O"
        }
    }

    instance FromString[Piece] {
        pub def fromString(x: String): Option[Piece] = {
            match String.toLowerCase(String.trim(x)) {
                case "x" => Some(X)
                case "o" => Some(O)
                case _   => None
            }
        }
    }

    pub def otherPiece(p: Piece): Piece = match p {
        case X => O
        case O => X
    }

    def ps(p: Option[Piece]): String = match p {
        case Some(ps) => "${ps}"
        case None    => " "
    }

    pub type alias Board = (
        (Option[Piece], Option[Piece], Option[Piece]),
        (Option[Piece], Option[Piece], Option[Piece]),
        (Option[Piece], Option[Piece], Option[Piece])
    )

    pub def boardString(b: Board): String = {
        let ((b1, b2, b3), (b4, b5, b6), (b7, b8, b9)) = b;
        let lines = " ${ps(b1)} | ${ps(b2)} | ${ps(b3)} " ::
            "---+---+---" ::
            " ${ps(b4)} | ${ps(b5)} | ${ps(b6)} " ::
            "---+---+---" ::
            " ${ps(b7)} | ${ps(b8)} | ${ps(b9)} " ::
            Nil;
        String.unlines(lines)
    }

    pub def empty(): Board = (
        (None, None, None),
        (None, None, None),
        (None, None, None)
    )

    pub def available(i: Position, b: Board): Bool = {
        let ((b1, b2, b3), (b4, b5, b6), (b7, b8, b9)) = b;
        match i {
            case 1 => b1 == None
            case 2 => b2 == None
            case 3 => b3 == None
            case 4 => b4 == None
            case 5 => b5 == None
            case 6 => b6 == None
            case 7 => b7 == None
            case 8 => b8 == None
            case 9 => b9 == None
            case _ => false
        }
    }

    /// Will ignore placements on occupied places
    pub def placePiece(p: Piece, i: Position, b: Board): Board = {
        let ((b1, b2, b3), (b4, b5, b6), (b7, b8, b9)) = b;
        def place(bb) = bb |> Option.getWithDefault(p) |> Some;
        match i {
            case 1 => ((place(b1), b2, b3), (b4, b5, b6), (b7, b8, b9))
            case 2 => ((b1, place(b2), b3), (b4, b5, b6), (b7, b8, b9))
            case 3 => ((b1, b2, place(b3)), (b4, b5, b6), (b7, b8, b9))
            case 4 => ((b1, b2, b3), (place(b4), b5, b6), (b7, b8, b9))
            case 5 => ((b1, b2, b3), (b4, place(b5), b6), (b7, b8, b9))
            case 6 => ((b1, b2, b3), (b4, b5, place(b6)), (b7, b8, b9))
            case 7 => ((b1, b2, b3), (b4, b5, b6), (place(b7), b8, b9))
            case 8 => ((b1, b2, b3), (b4, b5, b6), (b7, place(b8), b9))
            case 9 => ((b1, b2, b3), (b4, b5, b6), (b7, b8, place(b9)))
            case _ => b
        }
    }

    pub def positionFromString(s: String): Option[Position] = {
        use String.{trim, toLowerCase, isMatch};
        def pairPattern(s1, s2, input) =
            isMatch(regex = "(${s1} *${s2}|${s2} *${s1})", input);
        match trim(toLowerCase(s)) {
            case s1 if isMatch(regex = "+?[1-9]", s1) => Int32.fromString(s1)
            case "up" => Some(2)
            case "down" => Some(8)
            case "left" => Some(4)
            case "right" => Some(6)
            case s1 if pairPattern("up", "right", s1) => Some(3)
            case s1 if pairPattern("up", "left", s1) => Some(1)
            case s1 if pairPattern("down", "right", s1) => Some(9)
            case s1 if pairPattern("down", "left", s1) => Some(7)
            case "top" => Some(2)
            case s1 if pairPattern("top", "right", s1) => Some(3)
            case s1 if pairPattern("top", "left", s1) => Some(1)
            case s1 if isMatch(regex = "(mid|middle)", s1) => Some(5)
            case s1 if isMatch(regex = "(bot|bottom)", s1) => Some(8)
            case s1 if pairPattern("(bot|bottom)", "right", s1) => Some(9)
            case s1 if pairPattern("(bot|bottom)", "left", s1) => Some(7)
            case s1 if isMatch(regex = "(n|north)", s1) => Some(2)
            case s1 if isMatch(regex = "(e|east)", s1) => Some(6)
            case s1 if isMatch(regex = "(s|south)", s1) => Some(8)
            case s1 if isMatch(regex = "(w|west)", s1) => Some(4)
            case s1 if pairPattern("(n|north)", "(e|east)", s1) => Some(3)
            case s1 if pairPattern("(n|north)", "(w|west)", s1) => Some(1)
            case s1 if pairPattern("(s|south)", "(e|east)", s1) => Some(9)
            case s1 if pairPattern("(s|south)", "(w|west)", s1) => Some(7)
            case _ => None
        }
    }

    pub enum Winner {
        case PieceWin(Piece)
        case NoneWin
        case BothWin
        case Stuck
    }

    pub def boardState(b: Board): Winner = {
        let xWin = match b {
            case ((Some(X), _, _), (Some(X), _, _), (Some(X), _, _)) => true
            case ((_, Some(X), _), (_, Some(X), _), (_, Some(X), _)) => true
            case ((_, _, Some(X)), (_, _, Some(X)), (_, _, Some(X))) => true
            case ((Some(X), Some(X), Some(X)), (_, _, _), (_, _, _)) => true
            case ((_, _, _), (Some(X), Some(X), Some(X)), (_, _, _)) => true
            case ((_, _, _), (_, _, _), (Some(X), Some(X), Some(X))) => true
            case ((Some(X), _, _), (_, Some(X), _), (_, _, Some(X))) => true
            case ((_, _, Some(X)), (_, Some(X), _), (Some(X), _, _)) => true
            case _ => false
        };
        let oWin = match b {
            case ((Some(O), _, _), (Some(O), _, _), (Some(O), _, _)) => true
            case ((_, Some(O), _), (_, Some(O), _), (_, Some(O), _)) => true
            case ((_, _, Some(O)), (_, _, Some(O)), (_, _, Some(O))) => true
            case ((Some(O), Some(O), Some(O)), (_, _, _), (_, _, _)) => true
            case ((_, _, _), (Some(O), Some(O), Some(O)), (_, _, _)) => true
            case ((_, _, _), (_, _, _), (Some(O), Some(O), Some(O))) => true
            case ((Some(O), _, _), (_, Some(O), _), (_, _, Some(O))) => true
            case ((_, _, Some(O)), (_, Some(O), _), (Some(O), _, _)) => true
            case _ => false
        };
        let stuck = match b {
            case (
                (Some(_), Some(_), Some(_)),
                (Some(_), Some(_), Some(_)),
                (Some(_), Some(_), Some(_))
                ) => true
            case _ => false
        };
        match (xWin, oWin) {
            case (true, true) => BothWin
            case (true, false) => PieceWin(X)
            case (false, true) => PieceWin(O)
            case _ if stuck => Stuck
            case _ => NoneWin
        }
    }
}
