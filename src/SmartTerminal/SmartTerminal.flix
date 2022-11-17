namespace SmartTerminal {
    use SmartTerminal/Formatting.Formatter

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
    /// Prints the given `text` and then prompts the console for input, which is
    /// given to the `parser`, which will perhaps restart the process with the
    /// additional message of the `Retry` value.
    ///
    /// In the two following layouts, `startHint` is replaced by the message in
    /// `Retry` if that is returned by the parser:
    ///
    pub def prompt(
        ansi: Bool,
        formatter: Formatter,
        startHint: String,
        parser: String -> Output[t] \ ef,
        text: String
    ): Result[t, String] \ {IO, ef} = {
        use Result.flatMap;

        if (ansi) Terminal.clearScreenAndReset() else ();
        print(formatter({text = text, info = startHint}));
        let* line = Console.readLine();
        let parserOutput = parser(line);
        match parserOutput {
            case Output.Return(output) =>
                Ok(output)
            case Output.Retry(msg) =>
                prompt(ansi, formatter, msg, parser, text)
        }
    }

    pub enum StateOutput[t, s] {
        case Return(t)
        case Retry(String, s)
    }

    pub def statePrompt(
        ansi: Bool,
        formatter: Formatter,
        startHint: String,
        config: config,
        parser: (String, config) -> StateOutput[t, config] \ ef1,
        text: config -> String \ ef2
    ): Result[t, String] \ {IO, ef1, ef2} = {
        use Result.flatMap;

        if (ansi) Terminal.clearScreenAndReset() else ();
        print(formatter({text = text(config), info = startHint}));
        let* line = Console.readLine();
        let parserOutput = parser(line, config);
        match parserOutput {
            case StateOutput.Return(output) =>
                Ok(output)
            case StateOutput.Retry(msg, config1) =>
                statePrompt(ansi, formatter, msg, config1, parser, text)
        }
    }

    namespace Formatting {
        use StringBuilder.append!;
        use StringBuilder.appendLine!;
        use StringBuilder.toString;

        pub type alias Formatter = {text = String, info = String} -> String

        pub def oneLine(): Formatter = input -> region r {
            let sb = new StringBuilder(r);
            if (input.text != "") append!(input.text, sb) else ();
            if (input.info != "") append!(" (${input.info})", sb) else ();
            append!(": ", sb);
            toString(sb)
        }

        pub def multiLine(): Formatter = input -> region r {
            let sb = new StringBuilder(r);
            if (input.text != "") appendLine!(input.text, sb) else ();
            appendLine!("-----", sb);
            if (input.info != "") appendLine!(input.info, sb) else ();
            append!("> ", sb);
            toString(sb)
        }
    }

    namespace Parsing {

        pub def wordMatch(
            w: String,
            ifWord: Unit -> t \ ef1,
            otherwise: String -> t \ ef2
        ): String -> t \ {ef1, ef2} = s0 -> {
            if (s0 == w) {
                ifWord()
            } else {
                otherwise(s0)
            }
        }

        pub def choice(
            items: foldable[(String, Unit -> t \ ef1)],
            ifNot: Unit -> t \ ef2
        ): String -> t \ {ef1, ef2} with Foldable[foldable] = {
            s0 -> {
                match Foldable.findLeft(fst >> Eq.eq(s0), items) {
                    case None => ifNot()
                    case Some((_, action)) => action()
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // Shorthand Defs ----------------------------------------------------------
    // -------------------------------------------------------------------------

    pub def enterToContinue(
        ansi: Bool,
        formatter: Formatter,
        text: String
    ): Result[Unit, String] \ IO = {
        prompt(
            ansi,
            formatter,
            "Press enter to continue",
            _ -> Output.Return(),
            text
        )
    }

    pub def yesNo(
        ansi: Bool,
        formatter: Formatter,
        text: String
    ): Result[Bool, String] \ IO = choice(
        ansi,
        formatter,
        "",
        ("y", () -> Output.Return(true)) ::
            ("n", () -> Output.Return(false)) ::
            Nil,
        "/",
        text
    )

    ///
    /// Uses trim and is case-insensitive.
    /// If `items` functions return `Retry`, the string is prepended to a
    /// list of the items
    ///
    /// hint: use `upcast` if you combine pure `items` with impure ones.
    ///
    pub def choice(
        ansi: Bool,
        formatter: Formatter,
        startHint: String,
        items: foldable[(String, Unit -> Output[t] \ ef)],
        delim: String,
        text: String
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use Foldable.join;
        use Functor.map;
        use SmartTerminal/Parsing.choice;
        use String.{trim, toLowerCase};

        let itemList = Foldable.toList(items);
        let helpMsg = join(delim, map(fst, itemList));
        let items1 = itemList |> map(match (s, a) -> (trim(toLowerCase(s)), a));
        let parser =
            trim >>
            toLowerCase >>
            choice(items1, () -> Output.Retry(helpMsg));
        prompt(ansi, formatter, "${startHint}${helpMsg}", parser, text)
    }

    pub def menu(
        ansi: Bool,
        formatter: Formatter,
        startHint: String,
        config: config,
        items: foldable[(String, config -> StateOutput[t, config] \ ef)],
        delim: String,
        text: config -> String
    ): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
        use Foldable.join;
        use Functor.map;
        use Result.flatMap;
        use SmartTerminal/Parsing.choice;
        use String.{trim, toLowerCase};
        use SmartTerminal.StateOutput.{Return, Retry};

        let itemList = Foldable.toList(items);
        let helpMsg = join(delim, map(fst, itemList));
        let items1 = itemList |> map(match (s, a) -> (
            trim(toLowerCase(s)),
            () -> conf -> match a(conf) {
                case Return(v) => Return(v)
                case Retry(msg, conf1) => Retry("${msg}${helpMsg}", conf1)
            }
        ));
        let retry = () -> (conf -> StateOutput.Retry(helpMsg, conf) as \ ef);
        let parser =
            trim >>
            toLowerCase >>
            choice(items1, retry);
        statePrompt(
            ansi,
            formatter,
            "${startHint}${helpMsg}",
            config,
            parser,
            text
        )
    }
}
