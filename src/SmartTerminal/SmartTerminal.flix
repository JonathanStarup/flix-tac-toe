namespace SmartTerminal {

    ///
    /// Smart terminal parser output.
    /// * `Return(t: t)` stops looping the terminal action and returns `t`.
    /// * `Retry(msg: String)` loops the terminal action with the additional
    ///     message `s` below the original information.
    ///
    pub enum Output[t] {
        case Return(t)
        case Retry(String)
    }

    ///
    /// Defines the layout of the `prompt` method.
    ///
    pub enum Layout {
        case OneLine, MultiLine
    }

    ///
    /// Prints the given `text` and then prompts the console for input, which is
    /// given to the `parser`, which will perhaps restart the process with the
    /// additional message of the `Retry` value.
    ///
    /// In the two following layouts, `startHint` is replaced by the message in
    /// `Retry` if that is returned by the parser:
    ///
    /// ---
    /// The `MultiLine` layout:
    /// ```
    /// <text>
    /// -----
    /// <startHint>
    /// >
    /// ```
    /// ---
    /// The `OneLine` layout:
    /// ```
    /// <text> (<StartHint>):
    /// ```
    ///
    pub def prompt(
        ansi: Bool,
        layout: Layout,
        startHint: String,
        parser: String -> Output[t] \ ef,
        text: String
    ): Result[t, String] \ {IO, ef} = {
        let bar = String.repeat(5, "-");
        /// `extraMsg = ""` does not print an empty line
        def inner(extraMsg: String): Result[t, String] \ {IO, ef} = {
            if (ansi) Terminal.clearScreenAndReset() else ();
            match layout {
                case OneLine =>
                    if (text != "") print(text) else ();
                    if (extraMsg != "") print(" (${extraMsg})") else ();
                    print(": ")
                case MultiLine =>
                    if (text != "") println(text) else ();
                    println(bar);
                    if (extraMsg != "") println(extraMsg) else ();
                    print("> ")
            };
            let inputRes = Console.readLine();
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

    pub def enterToContinue(
        ansi: Bool,
        layout: Layout,
        text: String
    ): Result[Unit, String] \ IO = {
        prompt(
            ansi,
            layout,
            "Press enter to continue",
            _ -> Return(),
            text
        )
    }

    pub def yesNo(
        ansi: Bool,
        layout: Layout,
        text: String
    ): Result[Bool, String] \ IO = {
        choice(
            ansi,
            layout,
            ("Y", () -> Return(true)) :: ("N", () -> Return(false)) :: Nil,
            text
        )
    }

    /// Uses trim and is case-insensitive.
    /// If `items` functions return `Retry`, the string is prepended to a
    /// list of the items
    pub def choice(
        ansi: Bool,
        layout: Layout,
        items: foldable[(String, Unit -> Output[t] \ ef)],
        text: String
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use String.{trim, toLowerCase};

        let helpMsg = List.join(" | ", List.map(fst, Foldable.toList(items)));
        let parser = s0 -> {
            let s1 = trim(toLowerCase(s0));
            def cmp(s) = trim(toLowerCase(s)) == s1;
            match Foldable.findLeft(fst >> cmp, items) {
                case None => Retry(helpMsg)
                case Some((_, action)) => match action() {
                    case Return(v)  => Return(v)
                    case Retry(msg) => Retry("${msg}${helpMsg}")
                }
            }
        };
        prompt(ansi, layout, helpMsg, parser, text)
    }
}
