namespace Cli/TicTacToe {
    use Cli.Res
    use SmartTerminal.choice
    use SmartTerminal.enterToX
    use SmartTerminal.Output
    use SmartTerminal.StateOutput
    use SmartTerminal.statePrompt
    use SmartTerminal/Formatting.multiLine
    use SmartTerminal/Formatting.oneLine
    use SmartTerminal/Parsing.wordMatch
    use SmartTerminal/Util.emph
    use SmartTerminal/Util.flatten
    use SmartTerminal/Util.resStateOutput
    use TicTacToe.available
    use TicTacToe.boardState
    use TicTacToe.boardString
    use TicTacToe.otherPiece
    use TicTacToe.Piece
    use TicTacToe.placePiece
    use TicTacToe.positionFromString
    use TicTacToe.Winner

    pub def play(ansi: Bool): Res \ IO = {
        use Result.flatMap;
        let* _ = intro(ansi);
        let* starting = choice(
            ansi,
            oneLine(),
            startHint = "",
            (
                ("X", () -> Output.Return(Piece.X)) ::
                ("O", () -> Output.Return(Piece.O)) :: Nil
            ),
            delim = "/",
            "Which player should start?"
        );
        playWith(ansi, starting)
    }

    def intro(ansi: Bool): Res \ IO = {
        use String.{lineSeparator => ls};
        let b = emph(ansi);
        enterToX(ansi, multiLine(), x = "play", List.join("",
            "${b("TicTacToe")}:" ::
            ls() :: ls() ::
            "X and O play against eachother turnwise on a three-by-three" ::
            " board in which the objective is to get" ::
            " ${b("three pieces in a row")}." ::
            " This can be both vertically, horizontally, or diagonally." ::
            ls() :: ls() ::
            "Input can be given in natural language (e.g. 'top left'," ::
            " 'north', etc.), or numbers 1-9 read top to bottom," ::
            " left to right." ::
            Nil
        ))
    }

    def playWith(ansi: Bool, starting: Piece): Res \ IO = {
        use String.{lineSeparator => ls};
        let break = ls() + ls();
        let quitOrElse = wordMatch(
            "(exit|quit|forfeit|back)",
            () -> _ -> StateOutput.Return(Ok()) as \ IO
        );
        let parser = quitOrElse(
            positionFromString >> (opt -> c -> match opt {
                case Some(p) if available(p, c.b) =>
                    let b1 = placePiece(c.p, p, c.b);
                    let ending = enterToX(ansi, multiLine(), x = "exit") >>
                        Result.map(() -> StateOutput.Return()) >>
                        resStateOutput;
                    match boardState(b1) {
                        case Winner.NoneWin =>
                            let c1 = {p = otherPiece(c.p), b = b1};
                            StateOutput.Retry("", c1)
                        case Winner.PieceWin(winner) =>
                            ending("${winner} has won!${break}${boardString(b1)}")
                        case Winner.BothWin =>
                            ending("Both players.. win?${break}${boardString(b1)}")
                        case Winner.Stuck =>
                            ending("A stalemate!${break}${boardString(b1)}")
                    }
                case None =>
                    StateOutput.Retry("Sorry, didn't understand that", c)
                case Some(_) =>
                    StateOutput.Retry("That position is already occupied", c)
            })
        );
        def rep(c) = "${c.p} to play${break}${boardString(c.b)}";
        statePrompt(
            ansi,
            multiLine(),
            startHint = "",
            {p = starting, b = TicTacToe.empty()},
            parser,
            rep
        ) |> flatten
    }

}
