use String.{lineSeparator => newline}

def titleCard(): String =
    "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String =
    "Type out any command in the box"

def main(): Unit \ IO = {
    Terminal.clearScreenAndReset();
    println(titleCard());
    println(introduction());
    let initConfig = ();
    Menu.menuLoop(initConfig, mainMenu())
}

def mainMenu(): Menu.Menu[config] = Menu.menu("exit",
    {
        name = "play",
        action = Menu.menuAction(playMenu())
    } ::
    Nil
)

def playMenu(): Menu.Menu[config] = Menu.menu("back",
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.consoleAction(config -> {
            discard ttt2pGame(use TicTacToe.Piece; Piece.X);
            config
        })
    } ::
    Nil
)

def ttt2pGame(starting: TicTacToe.Piece): Option[TicTacToe.Piece] \ IO = region r {
    use TicTacToe.Winner;
    let board = ref TicTacToe.empty() @ r;
    let next = ref starting @ r;
    def loop() = match tttMove(deref next, deref board) {
        case Some(board1) =>
            board := board1;
            next := TicTacToe.otherPiece(deref next);
            match TicTacToe.boardState(deref board) {
                case Winner.PieceWin(p) =>
                    Menu.outputMessage(TicTacToe.boardString(deref board));
                    println("${p} has won! Enter to continue");
                    Some(p)
                case Winner.NoneWin =>
                    loop()
                case Winner.BothWin =>
                    Menu.outputMessage(TicTacToe.boardString(deref board));
                    println("Both seem to have won... Enter to continue");
                    None
                case Winner.Stuck =>
                    Menu.outputMessage(TicTacToe.boardString(deref board));
                    println("It's a draw! Enter to continue");
                    None
            }
        case None =>
            None
    };
    loop()
}

def tttMove(toPlay: TicTacToe.Piece, b: TicTacToe.Board): Option[TicTacToe.Board] \ IO = {
    use TicTacToe.boardString;
    use TicTacToe.available;
    use TicTacToe.placePiece;

    Menu.outputMessage(boardString(b));
    def parse(input) = {
        let i = match String.toLowerCase(input) {
            case "1"      => Some(1)
            case "2"      => Some(2)
            case "up"     => Some(2)
            case "top"    => Some(2)
            case "3"      => Some(3)
            case "4"      => Some(4)
            case "left"   => Some(4)
            case "5"      => Some(5)
            case "middle" => Some(5)
            case "6"      => Some(6)
            case "right"  => Some(6)
            case "7"      => Some(7)
            case "8"      => Some(8)
            case "down"   => Some(8)
            case "bottom" => Some(8)
            case "9"      => Some(9)
            case _        => None
        };
        i |> Option.flatMap(j -> if (available(j, b)) Some(j) else None)
    };
    let i = Menu.prompt(parse, "cancel");
    i |> Option.map(j -> placePiece(toPlay, j, b))
}
