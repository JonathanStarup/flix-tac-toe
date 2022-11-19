namespace SmartTerminal/Examples {
    use SmartTerminal.choice
    use SmartTerminal.enterToX
    use SmartTerminal.menu
    use SmartTerminal.prompt
    use SmartTerminal.yesNo
    use SmartTerminal/Formatting.multiLine
    use SmartTerminal/Formatting.oneLine
    use SmartTerminal/Parsing.wordMatch
    use SmartTerminal/Util.crash
    use SmartTerminal/Util.flatten
    use SmartTerminal/Util.mapOutput
    use SmartTerminal/Util.mapStateOutput
    use SmartTerminal/Util.optOutput
    use SmartTerminal/Util.optStateOutput
    use SmartTerminal/Util.resOutput
    use SmartTerminal/Util.resStateOutput

    pub def ansiPrompt(): Result[Bool, String] \ IO = {
        yesNo(
            false,
            oneLine(),
            "Do you want to enable ANSI use?"
        )
    }

    pub def fileName(): Result[String, String] \ IO = {
        use Result.flatMap;
        use SmartTerminal.Output.{Return, Retry};

        let* ansi = ansiPrompt();
        prompt(
            ansi,
            multiLine(),
            startHint = "",
            s0 -> match String.trim(s0) {
                case s if String.isMatch(regex = "[A-Za-z0-9]+(\\.[A-Za-z]+)+", s) =>
                    Return(s)
                case _ => Retry("Try something like 'something123.txt'")
            },
            "Write the file name you'd like to save as (including extension)."
        )
    }

    pub def nestedPrompt(): Unit \ IO = {
        use SmartTerminal.Output.{Return, Retry};

        let ansi = crash(ansiPrompt());
        def helpPrompt() = crash(prompt(
            ansi,
            multiLine(),
            startHint = "",
            s0 -> match s0 {
                case "" =>
                    Retry("I'm waiting...")
                case s if not String.isMatch(regex = "[A-Za-z]*", s) =>
                    Retry("I said letters buddy.")
                case s if not (String.length(s) >= 20) =>
                    Retry("That doesn't look like 20 to me.")
                case _ =>
                    crash(enterToX(
                        ansi,
                        multiLine(),
                        x = "continue",
                        "Alright, those are pretty cool. The best candy is blue jeans."
                    ));
                    Return()
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
                    Return()
                case "help" =>
                    helpPrompt();
                    Retry("")
                case "" =>
                    Retry("You gotta type something man.")
                case _ => Retry("Incorrect, stupid!")
            },
            "What is the best candy?\n(try 'help' for help)"
        )
    }

    pub def areYouSure(): Unit \ IO = {
        use SmartTerminal.Output.{Return, Retry};

        let ansi = crash(ansiPrompt());
        def areYouSurePrompt(lang) = {
            let sure = crash(yesNo(
                ansi,
                oneLine(),
                "Are you really sure it's ${lang}?"
            ));
            if (sure) Return() else Retry("")
        };
        def option(name) = (name, () -> areYouSurePrompt(name));
        discard choice(
            ansi,
            multiLine(),
            startHint = "",
            List.map(option, List.sort(
                "Flix" :: "Scala" :: "Haskell" :: "Java" ::
                "Sml" :: Nil
            )),
            delim = " | ",
            "What is your favorite programming language?"
        )
    }

    pub def resultThreading1(): Result[Int32, String] \ IO = {
        use SmartTerminal.Output.{Return, Retry};
        use Result.flatMap;

        let hint = "integer";
        let* ansi = ansiPrompt();
        def intParser(s) = match Int32.fromString(String.trim(s)) {
            case Some(v) =>
                let res = {
                    let* ans = yesNo(
                        ansi,
                        oneLine(),
                        "Are you committed to ${v}?"
                    );
                    if (ans) Ok(Return(v)) else Ok(Retry(hint))
                };
                resOutput(res)
            case None => Retry(hint)
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
        use SmartTerminal.StateOutput.{Return, Retry};
        use SmartTerminal.Output.{Return => OReturn, Retry => ORetry};

        let ansi = crash(ansiPrompt());
        def intParser(s) = match Int32.fromString(String.trim(s)) {
            case Some(v) => OReturn(v)
            case None => ORetry("Must be an integer")
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
                Retry("", n + intPrompt("Write a number to add to ${n}"))
            }) ::
                ("Subtract", n -> {
                    Retry("", n - intPrompt("Write a number to subtract from ${n}"))
                }) ::
                ("Multiply", n -> {
                    Retry("", n * intPrompt("Write a number to multiply with ${n}"))
                }) ::
                ("Divide", n -> {
                    Retry("", n / intPrompt("Write a number to divide ${n} with"))
                }) ::
                ("Nothing", n -> {
                    Return(println("Your final number was ${n: Int32}, good choice."))
                }) ::
                Nil,
            delim = " | ",
            n -> "What Do you want to do with your number: ${n}"
        )
    }

    /// WIP
    pub def settingsMenu(): Result[String, String] \ IO = {
        use SmartTerminal.StateOutput.{Return, Retry};
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
                Result.map(n -> Retry("", {a = n | settings})) |>
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
                Result.map(n -> Retry("", {b = n | settings})) |>
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
                Result.map(b -> Retry("", {c = b | settings})) |>
                resStateOutput
        });
        let quit = ("quit", settings -> Return(Ok(settings)) as \ IO);

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
        finalSettings |> Result.map(stringify)
    }
}
