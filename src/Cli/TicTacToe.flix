namespace Cli/TicTacToe {
use Cli.Res
use Foldable.join
use Functor.map
use Option.getWithDefault
use Result.flatten
use SmartTerminal.choice
use SmartTerminal.enterToX
use SmartTerminal.Output
use SmartTerminal.StateOutput
use SmartTerminal.statePrompt
use SmartTerminal/Formatting.multiLine
use SmartTerminal/Formatting.oneLine
use SmartTerminal/Parsing.wordMatch
use SmartTerminal/Strings.parBreak
use SmartTerminal/Util.emph
use SmartTerminal/Util.resStateOutput
use TicTacToe.availablePosition
use TicTacToe.boardState
use TicTacToe.boardString
use TicTacToe.placePiece
use TicTacToe.Position
use TicTacToe.positionFromString
use TicTacToe.Winner
use TicTacToe/Piece.{other => otherPiece}
use TicTacToe/Piece.Piece

pub def play(ansi: Bool): Res[Unit] \ IO = {
    use Result.flatMap;
    let* _ = intro(ansi);
    let* starting = startingPiece(ansi);
    playWithPiece(ansi, starting)
}

def intro(ansi: Bool): Res[Unit] \ IO = {
    let b = emph(ansi);
    let text = join("",
        "${b("TicTacToe")}:" ::
        parBreak() ::
        "X and O play against eachother turnwise on a three-by-three" ::
        " board in which the objective is to get" ::
        " ${b("three pieces in a row")}." ::
        " This can be both vertically, horizontally, or diagonally." ::
        parBreak() ::
        "Input can be given in natural language (e.g. 'top left'," ::
        " 'north', etc.), or numbers 1-9 read top to bottom," ::
        " left to right." ::
        Nil
    );
    enterToX(ansi, multiLine(), x = "play", text)
}

def startingPiece(ansi: Bool): Res[Piece] \ IO = {
    let choices =
        ("X", () -> Output.Return(Piece.X)) ::
        ("O", () -> Output.Return(Piece.O)) :: Nil;
    let text = "Which player should start?";
    choice(ansi, oneLine(), startHint = "", choices, delim = "/", text)
}

def playWithPiece(ansi: Bool, starting: Piece): Res[Unit] \ IO = {
    let quitOrElse = wordMatch(
        "(exit|quit|forfeit|back)",
        () -> _ -> StateOutput.Return(Ok()) as \ IO
    );
    let exitPrompt = enterToX(ansi, multiLine(), x = "exit") >>
        StateOutput.Return;
    def playMove(config, pos: Position) = {
        if (availablePosition(pos, config.b)) {
            let b1 = placePiece(config.p, pos, config.b);
            match boardState(b1) {
                case Winner.NoneWin =>
                    let c1 = {p = otherPiece(config.p), b = b1};
                    StateOutput.Retry("", c1)
                case Winner.PieceWin(winner) =>
                    exitPrompt("${winner} has won!${parBreak()}${boardString(b1)}")
                case Winner.BothWin =>
                    exitPrompt("Both players.. win?${parBreak()}${boardString(b1)}")
                case Winner.Stuck =>
                    exitPrompt("A stalemate!${parBreak()}${boardString(b1)}")
            }
        } else {
            StateOutput.Retry("That position is already occupied", config)
        }
    };
    def parser(input, config) = {
        input |>
            positionFromString |>
            map(playMove(config)) |>
            getWithDefault(
                StateOutput.Retry("Sorry, didn't understand that", config)
            )
    };
    def rep(c) = "${c.p} to play${parBreak()}${boardString(c.b)}";
    let initConfig = {p = starting, b = TicTacToe.empty()};
    statePrompt(
        ansi, multiLine(), startHint = "", initConfig, quitOrElse(parser), rep
    ) |> flatten
}
}
