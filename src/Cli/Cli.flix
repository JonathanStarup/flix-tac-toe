namespace Cli {
    use SmartTerminal.ansiPrompt
    use SmartTerminal.enterToX
    use SmartTerminal.menu
    use SmartTerminal.StateOutput
    use SmartTerminal/Formatting.multiLine
    use SmartTerminal/Formatting.oneLine

    pub type alias Res = Result[Unit, String]

    pub def run(): Result[Unit, String] \ IO = {
        use Result.flatMap;
        use String.{lineSeparator => ls};
        let* ansi = ansiPrompt();
        let* _ = enterToX(
            ansi,
            multiLine(),
            x = "continue",
            List.join("",
                "A box of games to play." ::
                ls() :: ls() ::
                "Generally you can write exit and other synonyms to quit." ::
                Nil
            )
        );
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
