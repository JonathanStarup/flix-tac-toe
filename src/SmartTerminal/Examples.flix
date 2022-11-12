namespace SmartTerminal/Examples {
    use SmartTerminal.choice
    use SmartTerminal.enterToContinue
    use SmartTerminal.prompt
    use SmartTerminal.yesNo
    use SmartTerminal/Util.crash
    use SmartTerminal/Util.resOutput
    use SmartTerminal/Util.flatten

    def ansi(): Result[Bool, String] \ IO = {
        use SmartTerminal.Layout.{OneLine, MultiLine};
        yesNo(
            false,
            OneLine,
            "Do you want to enable ANSI?"
        )
    }

    pub def fileName(): Unit \ IO = {
        use SmartTerminal.Output.{Return, Retry};
        use SmartTerminal.Layout.{OneLine, MultiLine};

        let ansi = crash(ansi());
        discard prompt(
            ansi,
            MultiLine,
            "",
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
        use SmartTerminal.Layout.{OneLine, MultiLine};

        let ansi = crash(ansi());
        def helpPrompt() = crash(prompt(
            ansi,
            MultiLine,
            "",
            s0 -> match s0 {
                case "" =>
                    Retry("I'm waiting...")
                case s if not String.isMatch(regex = "[A-Za-z]*", s) =>
                    Retry("I said letters buddy.")
                case s if not (String.length(s) >= 20) =>
                    Retry("That doesn't look like 20 to me.")
                case _ =>
                    crash(enterToContinue(
                        ansi,
                        MultiLine,
                        "Alright, those are pretty cool. The best candy is blue jeans."
                    ));
                    Return()
            },
            "Okay, if you really want help I will need at least 20 cool letters"
        ));

        discard prompt(
            ansi,
            MultiLine,
            "",
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
        use SmartTerminal.Layout.{OneLine, MultiLine};

        let ansi = crash(ansi());
        def areYouSurePrompt(lang) = {
            let sure = crash(yesNo(
                ansi,
                OneLine,
                "Are you really sure it's ${lang}?"
            ));
            if (sure) Return() else Retry("")
        };
        def option(name) = (name, () -> areYouSurePrompt(name));
        discard choice(
            ansi,
            MultiLine,
            List.map(option, List.sort(
                "Flix" :: "Scala" :: "Haskell" :: "Java" ::
                "Sml" :: Nil
            )),
            "What is your favorite programming language?"
        )
    }

    pub def resultThreading1(): Result[Int32, String] \ IO = {
        use SmartTerminal.Output.{Return, Retry};
        use SmartTerminal.Layout.{OneLine, MultiLine};
        use Result.flatMap;

        let hint = "integer";
        let* ansi = ansi();
        def intParser(s) = match Int32.fromString(String.trim(s)) {
            case Some(v) =>
                let res = {
                    let* ans = yesNo(
                        ansi,
                        OneLine,
                        "Are you committed to ${v}?"
                    );
                    if (ans) Ok(Return(v)) else Ok(Retry(hint))
                };
                resOutput(res)
            case None => Retry(hint)
        };
        def intPrompt(text) = {
            flatten(prompt(ansi, OneLine, hint, intParser, text))
        };
        let* _ = enterToContinue(
            ansi,
            MultiLine,
            "You supply one number, then another, and I will add them together"
        );
        let* x = intPrompt("The first number");
        let* y = intPrompt("The second number");
        let sum = x + y;
        let* _ = enterToContinue(ansi, OneLine, "${x} + ${y} = ${sum}");
        Ok(sum)
    }
}

