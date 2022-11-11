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
        text: String,
        parser: String -> Output[t] \ ef,
        startHint: String
    ): Result[t, String] \ {IO, ef} = {
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
                    println(line());
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

    /// Unsafely unpacks result types.
    @Unsafe
    pub def crash(r: Result[a, b]): a with ToString[b] = match r {
        case Ok(v) => v
        case Err(msg) => bug!("!!Crash!! ${msg}")
    }

    pub def enterToContinue(
        ansi: Bool,
        layout: Layout,
        text: String
    ): Result[Unit, String] \ IO = {
        prompt(
            ansi,
            layout,
            text,
            _ -> Return(),
            "Press enter to continue"
        )
    }

    /// Uses trim and is case-insensitive.
    /// If `items` functions return `Retry`, the string is prepended to a
    /// list of the items
    pub def selectionPrompt(
        ansi: Bool,
        layout: Layout,
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
                        case Return(v)  => Return(v)
                        case Retry("")  => Retry(helpMsg)
                        case Retry(msg) => Retry("${msg}${helpMsg}")
                    }
            }
        };
        prompt(ansi, layout, text, parser, helpMsg)
    }

    def line(): String = {
        let barLength = 5;
        String.repeat(barLength, "-")
    }
}
