namespace Cli {
use Foldable.join
use SmartTerminal.ansiPrompt
use SmartTerminal.enterToX
use SmartTerminal.menu
use SmartTerminal.StateOutput
use SmartTerminal/Formatting.multiLine
use SmartTerminal/Formatting.oneLine
use SmartTerminal/Strings.parBreak

pub type alias Res[t] = Result[t, String]

pub def run(): Result[Unit, String] \ IO = {
    use Result.flatMap;
    let* ansi = ansiPrompt();
    let* _ = intro(ansi);
    gameMenu(ansi)
}

def intro(ansi: Bool): Res[Unit] \ IO = {
    let text = join("")(
        "A box of games to play." ::
        parBreak() ::
        "Generally you can write exit and other synonyms to quit." ::
        Nil
    );
    enterToX(ansi, multiLine(), x = "continue", text)
}

def gameMenu(ansi: Bool): Res[Unit] \ IO = {
    let options =
        ("Tic Tac Toe", _ -> StateOutput.Retry("", Cli/TicTacToe.play(ansi))) ::
        ("exit", _ -> StateOutput.Return() as \ IO) ::
        Nil;
    menu(
        ansi,
        multiLine(),
        startHint = "",
        Ok(),
        options,
        delim = " | ",
        _ -> ""
    )
}
}
