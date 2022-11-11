namespace SmartTerminal {

    ///
    /// A smart terminal output.
    /// * `Return(t: t)` stops looping the terminal action and returns `t`.
    /// * `Retry(msg: String)` loops the terminal action with the additional
    ///     message `s` below the original information.
    ///
    pub enum Output[t] {
        case Return(t)
        case Retry(String)
    }

    ///
    /// Prints the given `text` and then prompts the console for input, which is
    /// given to the `parser`, which will perhaps restart the process with the
    /// additional message of the `Retry` value.
    ///
    /// `text = ""` does not print an empty line
    ///
    pub def prompt(
        ansi: Bool,
        text: String,
        parser: String -> Output[t] \ ef,
        startHint: String
    ): Result[t, String] \ {IO, ef} = {
        /// `extraMsg = ""` does not print an empty line
        def inner(extraMsg: String): Result[t, String] \ {IO, ef} = {
            if (ansi) Terminal.clearScreenAndReset() else ();
            if (text != "") println(text) else ();
            println(line());
            if (extraMsg != "") println(extraMsg) else ();
            print("> ");
            let inputRes = Console.readLine();
            println("");
            match inputRes {
                case Err(msg) =>
                    Err(msg)
                case Ok(input) =>
                    match parser(input) {
                        case Return(output) =>
                            Ok(output)
                        case Retry(msg) =>
                            inner(msg)
                    }
            }
        };
        inner(startHint)
    }

    /// Unsafely unpacks result types.
    @Unsafe
    pub def crash(r: Result[a, b]): a with ToString[b] = match r {
        case Ok(v) => v
        case Err(msg) => bug!("!!Crash!! ${msg}")
    }

    /// Appends `"<Press enter to continue>"` after the `text`.
    pub def obliviousPrompt(
        ansi: Bool,
        text: String
    ): Result[Unit, String] \ IO = {
        let newParagraph = String.repeat(2, String.lineSeparator());
        prompt(
            ansi,
            "${text}${newParagraph}<Press enter to continue>",
            _ -> Return(),
            ""
        )
    }

    /// Uses trim and is case-insensitive
    pub def selectionPrompt(
        ansi: Bool,
        text: String,
        items: foldable[(String, Unit -> Output[t] \ ef)]
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use String.{trim, toLowerCase};
        let helpMsg = List.join(" | ", List.map(fst, Foldable.toList(items)));
        let parser = s0 -> {
            let s1 = trim(toLowerCase(s0));
            match Foldable.findLeft(match (s, _) -> trim(toLowerCase(s)) == s1, items) {
                case None => Retry(helpMsg)
                case Some((_, action)) => match action() {
                        case Return(v) => Return(v)
                        case Retry("") => Retry(helpMsg)
                        case Retry(msg) =>
                            let break = String.repeat(2, String.lineSeparator());
                            Retry("${msg}${break}${helpMsg}")
                    }
            }
        };
        prompt(ansi, text, parser, helpMsg)
    }

    def line(): String = {
        let barLength = 5;
        String.repeat(barLength, "-")
    }

    namespace Examples {
        use SmartTerminal.prompt
        use SmartTerminal.crash
        use SmartTerminal.obliviousPrompt
        use SmartTerminal.selectionPrompt

        def ansi(): Bool = true

        pub def fileName(): String \ IO = {
            use SmartTerminal.Output.{Return, Retry};
            crash(prompt(
                ansi(),
                "Write the file name you'd like to save as (including extension).",
                s0 -> match String.trim(s0) {
                    case s if String.isMatch(regex = "[A-Za-z0-9]+(\\.[A-Za-z]+)+", s) =>
                        Return(s)
                    case _ => Retry("Try something like 'something123.txt'")
                },
                ""
            ))
        }

        pub def yesNo(): Unit \ IO = {
            use SmartTerminal.Output.{Return, Retry};
            let coolTimes = 13;
            crash(selectionPrompt(
                ansi(),
                "Do you want to print 'cool'${coolTimes} times?",
                (
                    "y",
                    () -> {
                        if (ansi()) Terminal.clearScreenAndReset() else ();
                        List.range(0, coolTimes) |>
                            List.foreach(_ -> println("cool"));
                        Return()
                    }
                ) :: (
                    "n",
                    upcast () -> Return()
                ) ::
                Nil
            ))
        }

        pub def nestedPrompt(): Unit \ IO = {
            use SmartTerminal.Output.{Return, Retry};

            def helpPrompt() = crash(prompt(
                ansi(),
                "Okay, if you really want help I will need at least 20 cool letters",
                s0 -> match s0 {
                    case s if not String.isMatch(regex = "[A-Za-z]*", s) =>
                        Retry("I said letters buddy.")
                    case s if not (String.length(s) >= 20) =>
                        Retry("That doesn't look like 20 to me.")
                    case _ =>
                        crash(obliviousPrompt(
                            ansi(),
                            "Alright, those are pretty cool. The best candy is blue jeans."
                        ));
                        Return()
                },
                ""
            ));

            crash(prompt(
                ansi(),
                "What is the best candy?\n(try 'help' for help)",
                s0 -> match String.toLowerCase(String.trim(s0)) {
                    case "blue jeans" =>
                        if (ansi()) Terminal.clearScreenAndReset() else ();
                        println("You are entirely correct! Well done.");
                        Return()
                    case "help" =>
                        helpPrompt();
                        Retry("")
                    case "" =>
                        Retry("You gotta type something man.")
                    case _ => Retry("Incorrect, stupid!")
                },
                ""
            ))
        }

        pub def areYouSure(): Unit \ IO = {
            use SmartTerminal.Output.{Return, Retry};
            def areYouSurePrompt(choice) = crash(selectionPrompt(
                ansi(),
                "Are you really sure it's ${choice}?",
                ("y", () -> Return(Return())) ::
                ("n", () -> Return(Retry(""))) ::
                Nil
            ));
            def option(name) = (name, () -> areYouSurePrompt(name));
            crash(selectionPrompt(
                ansi(),
                "What is your favorite programming language?",
                List.map(option, List.sort(
                    "Flix" :: "Scala" :: "Haskell" :: "Java" ::
                    "Sml" :: Nil
                ))
            ))
        }

    }
}
