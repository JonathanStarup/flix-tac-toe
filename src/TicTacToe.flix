namespace TicTacToe {

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

    pub def available(i: Int32, b: Board): Bool = {
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
    pub def placePiece(p: Piece, i: Int32, b: Board): Board = {
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
