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
        text: String,
        parser: String -> Output[t] \ ef
    ): Result[t, String] \ {IO, ef} = {
        let barLength = 5;
        def printBar() = println(String.repeat(barLength, "-"));

        /// `extraMsg = ""` does not print an empty line
        def inner(extraMsg: String): Result[t, String] \ {IO, ef} = {
            if (text != "") println(text) else ();
            printBar();
            if (extraMsg != "") println(extraMsg) else ();
            print("> ");
            let inputRes = getInput();
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
        inner("")
    }

    /// Unsafely unpacks result types.
    @Unsafe
    pub def crash(r: Result[a, b]): a with ToString[b] = match r {
        case Ok(v) => v
        case Err(msg) => bug!("!!Crash!! ${msg}")
    }

    /// Appends `"<Press enter to continue>"` after the `text`.
    pub def obliviousPrompt(
        text: String
    ): Result[Unit, String] \ IO = {
        let newParagraph = String.repeat(2, String.lineSeparator());
        prompt("${text}${newParagraph}<Press enter to continue>", _ -> Return())
    }

    /// TODO
    pub def selection(): Unit \ IO =
        ??? as \ IO

    def getInput(): Result[String, String] \ IO = {
        match Console.readLine() {
            case None =>
                Err("Terminal input error, perhaps EOF or terminated terminal.")
            case Some(input) =>
                Ok(input)
        }
    }

    namespace Examples {
        use SmartTerminal.prompt
        use SmartTerminal.crash
        use SmartTerminal.obliviousPrompt

        pub def fileName(): String \ IO = {
            use SmartTerminal.Output.{Return, Retry};
            crash(prompt(
                "Write the file name you'd like to save as (including extension).",
                s0 -> match String.trim(s0) {
                    case s if String.isMatch(regex = "[A-Za-z0-9]+(\\.[A-Za-z]+)+", s) =>
                        Return(s)
                    case _ => Retry("Try something like 'something123.txt'")
                }
            ))
        }

        pub def yesNo(): Unit \ IO = {
            use SmartTerminal.Output.{Return, Retry};
            let coolTimes = 13;
            crash(prompt(
                "Do you want to print 'cool'${coolTimes} times? (y/n)",
                s0 -> match String.trim(s0) {
                    case "y" =>
                        List.range(0, coolTimes) |>
                            List.foreach(_ -> println("cool"));
                        Return()
                    case "n" =>
                        Return()
                    case _ =>
                        Retry("")
                }
            ))
        }

        pub def nestedPrompt(): Unit \ IO = {
            use SmartTerminal.Output.{Return, Retry};

            def helpPrompt() = crash(prompt(
                "Okay, if you really want help I will need at least 20 cool letters",
                s0 -> match s0 {
                    case s if not String.isMatch(regex = "[A-Za-z]*", s) =>
                        Retry("I said letters buddy.")
                    case s if not (String.length(s) >= 20) =>
                        Retry("That doesn't look like 20 to me.")
                    case _ =>
                        crash(obliviousPrompt(
                            "Alright, those are pretty cool. The best candy is blue jeans."
                        ));
                        Return()
                }
            ));

            crash(prompt(
                "What is the best candy?\n(try 'help' for help)",
                s0 -> match String.toLowerCase(String.trim(s0)) {
                    case "blue jeans" =>
                        println("You are entirely correct! Well done.");
                        Return()
                    case "help" =>
                        helpPrompt();
                        Retry("")
                    case "" =>
                        Retry("You gotta type something man.")
                    case _ => Retry("Incorrect, stupid!")
                }
            ))
        }

    }
}
