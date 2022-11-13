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
                        case Output.Return(output) =>
                            Ok(output)
                        case Output.Retry(msg) =>
                            inner(msg)
                    }
            }
        };
        inner(startHint)
    }

    pub enum StateOutput[t, s] {
        case Return(t)
        case Retry(String, s)
    }

    pub def menu(
        ansi: Bool,
        layout: Layout,
        startHint: String,
        config: config,
        items: foldable[(String, config -> StateOutput[t, config] \ ef)],
        text: config -> String
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use Result.flatMap;

        let itemList = Foldable.toList(items);
        def adjustPair(conf) = match (item, f) -> {
            (item, () -> Output.Return(f(conf)))
        };
        def inner(hint, conf) = {
            let* output = choice(
                ansi,
                layout,
                hint,
                List.map(adjustPair(conf),
                itemList),
                text(conf)
            );
            match output {
                case StateOutput.Return(v) => Ok(v)
                case StateOutput.Retry(msg, conf1) => inner(msg, conf1)
            }
        };
        inner(startHint, config)
    }

    // -------------------------------------------------------------------------
    // Shorthand Defs ----------------------------------------------------------
    // -------------------------------------------------------------------------

    pub def enterToContinue(
        ansi: Bool,
        layout: Layout,
        text: String
    ): Result[Unit, String] \ IO = {
        prompt(
            ansi,
            layout,
            "Press enter to continue",
            _ -> Output.Return(),
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
            "",
            ("y", () -> Output.Return(true)) ::
                ("n", () -> Output.Return(false)) ::
                Nil,
            text
        )
    }

    ///
    /// Uses trim and is case-insensitive.
    /// If `items` functions return `Retry`, the string is prepended to a
    /// list of the items
    ///
    /// hint: use `upcast` if you combine pure `items` with impure ones.
    ///
    pub def choice(
        ansi: Bool,
        layout: Layout,
        startHint: String,
        items: foldable[(String, Unit -> Output[t] \ ef)],
        text: String
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use String.{trim, toLowerCase};

        let helpMsg = List.join(" | ", List.map(fst, Foldable.toList(items)));
        let parser = s0 -> {
            let s1 = trim(toLowerCase(s0));
            let choiceMatch = fst >> trim >> toLowerCase >> Eq.eq(s1);
            match Foldable.findLeft(choiceMatch, items) {
                case None => Output.Retry(helpMsg)
                case Some((_, action)) => match action() {
                    case Output.Return(v)  => Output.Return(v)
                    case Output.Retry(msg) => Output.Retry("${msg}${helpMsg}")
                }
            }
        };
        prompt(ansi, layout, "${startHint}${helpMsg}", parser, text)
    }
}
