namespace SmartTerminal/Examples {
    use Functor.map
    use Result.flatten
    use SmartTerminal.ansiPrompt
    use SmartTerminal.choice
    use SmartTerminal.enterToX
    use SmartTerminal.menu
    use SmartTerminal.Output
    use SmartTerminal.prompt
    use SmartTerminal.StateOutput
    use SmartTerminal.yesNo
    use SmartTerminal/Formatting.multiLine
    use SmartTerminal/Formatting.oneLine
    use SmartTerminal/Parsing.wordMatch
    use SmartTerminal/Util.crash
    use SmartTerminal/Util.mapOutput
    use SmartTerminal/Util.mapStateOutput
    use SmartTerminal/Util.optOutput
    use SmartTerminal/Util.optStateOutput
    use SmartTerminal/Util.resOutput
    use SmartTerminal/Util.resStateOutput

    pub def fileName(): Result[String, String] \ IO = {
        use Result.flatMap;

        let* ansi = ansiPrompt();
        let text =
            "Write the file name you'd like to save as (including extension).";
        def parser(s0) = match String.trim(s0) {
            case s if String.isMatch(regex = "[A-Za-z0-9]+(\\.[A-Za-z]+)+", s) =>
                Output.Return(s)
            case _ =>
                Output.Retry("Try something like 'something123.txt'")
        };
        prompt(ansi, multiLine(), startHint = "", parser, text)
    }

    pub def nestedPrompt(): Unit \ IO = {
        let ansi = crash(ansiPrompt());
        def helpPrompt() = crash(prompt(
            ansi,
            multiLine(),
            startHint = "",
            s0 -> match s0 {
                case "" =>
                    Output.Retry("I'm waiting...")
                case s if not String.isMatch(regex = "[A-Za-z]*", s) =>
                    Output.Retry("I said letters buddy.")
                case s if not (String.length(s) >= 20) =>
                    Output.Retry("That doesn't look like 20 to me.")
                case _ =>
                    crash(enterToX(
                        ansi,
                        multiLine(),
                        x = "continue",
                        "Alright, those are pretty cool. The best candy is blue jeans."
                    ));
                    Output.Return()
            },
            "Okay, if you really want help I will need at least 20 cool letters"
        ));

        discard prompt(
            ansi,
            multiLine(),
            startHint = "",
            s0 -> match String.toLowerCase(String.trim(s0)) {
                case "blue jeans" =>
                    if (ansi) Terminal.clearScreenAndReset() else ();
                    println("You are entirely correct! Well done.");
                    Output.Return()
                case "help" =>
                    helpPrompt();
                    Output.Retry("")
                case "" =>
                    Output.Retry("You gotta type something man.")
                case _ =>
                    Output.Retry("Incorrect, stupid!")
            },
            "What is the best candy?\n(try 'help' for help)"
        )
    }

    pub def areYouSure(): Unit \ IO = {
        let ansi = crash(ansiPrompt());
        def areYouSurePrompt(lang) = {
            let sure = crash(yesNo(
                ansi,
                oneLine(),
                "Are you really sure it's ${lang}?"
            ));
            if (sure) Output.Return() else Output.Retry("")
        };
        def option(name) = (name, () -> areYouSurePrompt(name));
        discard choice(
            ansi,
            multiLine(),
            startHint = "",
            map(option, List.sort(
                "Flix" :: "Scala" :: "Haskell" :: "Java" ::
                "Sml" :: Nil
            )),
            delim = " | ",
            "What is your favorite programming language?"
        )
    }

    pub def resultThreading1(): Result[Int32, String] \ IO = {
        use Result.flatMap;

        let hint = "integer";
        let* ansi = ansiPrompt();
        def intParser(s) = match Int32.fromString(String.trim(s)) {
            case Some(v) =>
                let res = yesNo(
                    ansi,
                    oneLine(),
                    "Are you committed to ${v}?"
                );
                res |>
                    Result.map(b ->
                        if (b) Output.Return(v) else Output.Retry(hint)
                    ) |>
                    resOutput
            case None => Output.Retry(hint)
        };
        def intPrompt(text) = {
            flatten(prompt(ansi, oneLine(), startHint = hint, intParser, text))
        };
        let* _ = enterToX(
            ansi,
            multiLine(),
            x = "start",
            "You supply one number, then another, and I will add them together"
        );
        let* x = intPrompt("The first number");
        let* y = intPrompt("The second number");
        let sum = x + y;
        let* _ = enterToX(ansi, oneLine(), x = "exit", "${x} + ${y} = ${sum}");
        Ok(sum)
    }

    pub def calculator(): Unit \ IO = {
        let ansi = crash(ansiPrompt());
        def intParser(s) = match Int32.fromString(String.trim(s)) {
            case Some(v) => Output.Return(v)
            case None => Output.Retry("Must be an integer")
        };
        def intPrompt(msg) = {
            crash(prompt(
                ansi,
                oneLine(),
                startHint = "Must be an integer",
                intParser,
                msg))
        };

        discard menu(
            ansi,
            multiLine(),
            startHint = "",
            0,
            ("Add", n -> {
                StateOutput.Retry("", n + intPrompt("Write a number to add to ${n}"))
            }) ::
                ("Subtract", n -> {
                    StateOutput.Retry("", n - intPrompt("Write a number to subtract from ${n}"))
                }) ::
                ("Multiply", n -> {
                    StateOutput.Retry("", n * intPrompt("Write a number to multiply with ${n}"))
                }) ::
                ("Divide", n -> {
                    StateOutput.Retry("", n / intPrompt("Write a number to divide ${n} with"))
                }) ::
                ("Nothing", n -> {
                    StateOutput.Return(println("Your final number was ${n: Int32}, good choice."))
                }) ::
                Nil,
            delim = " | ",
            n -> "What Do you want to do with your number: ${n}"
        )
    }

    /// WIP
    pub def settingsMenu(): Result[String, String] \ IO = {
        use Result.flatMap;

        let* ansi = ansiPrompt();

        let changeA = ("A", settings -> {
            let res = prompt(
                ansi,
                oneLine(),
                startHint = "integer",
                String.trim >>
                    Int32.fromString >>
                    optOutput("integer"),
                "Change A from ${settings.a}"
            );
            res |>
                Result.map(n -> StateOutput.Retry("", {a = n | settings})) |>
                resStateOutput
        });
        let changeB = ("B", settings -> {
            def threeLetter(s) = {
                if (String.isMatch(regex = "[A-Za-z][A-Za-z][A-Za-z]", s))
                    Some(s)
                else None
            };
            let res = prompt(
                ansi,
                oneLine(),
                startHint = "must be 3 letters",
                String.trim >>
                    threeLetter >>
                    optOutput("must be 3 letters"),
                "Change B from ${settings.b}"
            );
            res |>
                Result.map(n -> StateOutput.Retry("", {b = n | settings})) |>
                resStateOutput
        });
        let changeC = ("C", settings -> {
            def boolFromString(s) = match s {
                case "true" => Some(true)
                case "false" => Some(false)
                case _ => None
            };
            let res = prompt(
                ansi,
                oneLine(),
                startHint = "true/false",
                String.trim >>
                    boolFromString >>
                    optOutput("true/false"),
                "Change C from ${settings.c}"
            );
            res |>
                Result.map(b -> StateOutput.Retry("", {c = b | settings})) |>
                resStateOutput
        });
        let quit = ("quit", settings -> StateOutput.Return(Ok(settings)) as \ IO);

        let finalSettings = flatten(menu(
            ansi,
            multiLine(),
            startHint = "",
            {a = 0, b = "aAa", c = false},
            changeA :: changeB :: changeC :: quit :: Nil,
            delim = " | ",
            settings -> String.unlines(
                "Current settings:" ::
                "  A = ${settings.a}" ::
                "  B = ${settings.b}" ::
                "  C = ${settings.c}" ::
                Nil
            )
        ));
        finalSettings |> Result.map(config ->
            "a = ${config.a}\nb = ${config.b}\nc = ${config.c}"
        )
    }
}
