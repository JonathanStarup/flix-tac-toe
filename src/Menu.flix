namespace Menu {
    use Applicative.point
    use Foldable.findLeft
    use Foldable.foreach
    use Foldable.join
    use Functor.map
    use Monad.flatMap
    use Option.getWithDefault
    use String.{lineSeparator => newline}

    ///
    /// A list of items that modify a `config` value.
    ///
    pub type alias Menu[config] = (List[MenuItem[config]], String)

    /// `(items, return)`
    pub def menu(return: String, items: List[MenuItem[config]]): Menu[config] =
        (items, return)

    ///
    /// An named item that can modify a `config` value.
    /// An item is invoked by its name verbatim.
    ///
    pub type alias MenuItem[config] = {name = String, action = Action[config]}

    ///
    /// An action on a `config` value.
    ///
    pub opaque enum Action[config] {
        // The boolean value indicates console effects
        case Action(config -> config \ IO, Bool)
        case ReturnAction(config -> config \ IO, Bool)
    }

    /// An action.
    pub def action(f: config -> config \ ef): Action[config] = {
        Action(f as config -> config \ IO, false)
    }

    /// An action with console printing.
    pub def consoleAction(f: config -> config \ ef): Action[config] = {
        Action(f as config -> config \ IO, true)
    }

    // An action, exiting the menu.
    pub def returnAction(f: config -> config \ ef): Action[config] = {
        ReturnAction(f as config -> config \ IO, false)
    }

    // An action, exiting the menu, with console printing.
    pub def consoleReturnAction(f: config -> config \ ef): Action[config] = {
        ReturnAction(f as config -> config \ IO, true)
    }

    /// An action that prints `"${feature} in development"`.
    pub def wip(feature: String): Action[config] = consoleAction(config -> {
        outputMessage("${feature} in development");
        config
    })

    pub def menuAction(menu: Menu[config]): Action[config] = {
        consoleAction(config -> menuLoop(config, menu))
    }

    pub def menuLoop(
        config: config,
        menu: Menu[config]
    ): config \ IO = {
        def loop(c) = {
            menuLoop(c, menu)
        };
        Terminal.saveCursor();
        def reset() = {
            Terminal.restoreCursor();
            Terminal.clearScreenAfterCursor()
        };
        outputLine(menuString(padding = 3, menu));

        match getMenuInput(menu) {
            case None =>
                reset();
                loop(config)
            case Some(Exit) =>
                reset();
                config
            case Some(Item(item)) => match item.action {
                case ReturnAction(action, console) =>
                    if (console) reset() else ();
                    let config1 = action(config);
                    if (not console) reset() else ();
                    config1
                case Action(action, console) =>
                    if (console) reset() else ();
                    let config1 = action(config);
                    if (not console) reset() else ();
                    loop(config1)
            }
        }
    }

    pub def outputMessage(msg: String): Unit \ IO = {
        Terminal.clearScreenAndReset();
        outputLine(msg)
    }

    /// Assumes the cursor is on a new line
    pub def prompt(parser: String -> Option[a], cancel: String): Option[a] \ IO = {
        def header() = {
            outputLine("------------");
            outputLine("")
        };
        Terminal.saveCursor();
        header();
        def inner() = match getInput() {
            case None => inner()
            case Some(line) if line == cancel =>
                None
            case Some(line) =>
                match parser(String.trim(line)) {
                    case Some(output) =>
                        Terminal.restoreCursor();
                        Terminal.clearScreenAfterCursor();
                        Some(output)
                    case None =>
                        Terminal.restoreCursor();
                        Terminal.clearScreenAfterCursor();
                        header();
                        inner()
                }
        };
        inner()
    }

    // -------------------------------------------------------------------------
    // Private Defs
    // -------------------------------------------------------------------------

    def output(x: a): Unit \ IO with ToString[a] =
        print(x)

    def outputLine(x: a): Unit \ IO with ToString[a] =
        println(x)

    def getMenuInput(menu: Menu[config]): Option[MatchResult[config]] \ IO = {
        flatMap(matchOnMenu(menu), getInput())
    }

    def getInput(): Option[String] \ IO = {
        output("> ");
        Console.readLine()
    }

    def menuString(padding: {padding = Int32}, menu: Menu[config]): String = {
        let (items, return) = menu;
        let pad = String.repeat(padding.padding, " ");
        let itemNames = map(i -> i.name, items);
        let names = itemNames `List.append` (return :: Nil);
        let menuLine = join(pad, names);
        let boxLines = boxLines(padding, point(menuLine));
        join(newline(), boxLines)
    }

    def boxLines(
        padding: {padding = Int32},
        lines: List[String]
    ): List[String] = {
        let pad = String.repeat(padding.padding, " ");
        // this is an upper bound if colors are present.
        let maxLine = lines |>
            map(String.length) |>
            List.maximum |>
            getWithDefault(0);
        let barLength = maxLine + 2 * padding.padding + 1;
        let barredLines = lines |> map(line -> "|${pad}${line}");
        let bar = "|${String.repeat(barLength - 1, "-")}";
        bar :: (barredLines `List.append` (point(bar)))
    }

    /// Return `true` if `input` is a prefix of `against`.
    def matchString(
        input: {input = String},
        against: {against = String}
    ): Bool = {
        String.isMatch(regex = "${input.input}.*", against.against)
    }

    /// Starts from 1 char and gets longer and longer.
    def prefixes(r: Region[r], s: String): Iterator[String, r] \ r = region r1 {
        let sb = new StringBuilder(r1);
        let pfs = new MutList(r1);
        foreach(c <- String.iterator(r, s)) {
            StringBuilder.append!(c, sb);
            MutList.push!(StringBuilder.toString(sb), pfs)
        };
        MutList.iterator((pfs as MutList[String, r]))
    }

    /// `against` has `s` removed.
    def _minUniquePrefix(
        s: String,
        against: List[String]
    ): Option[String] = region r {
        prefixes(r, s) |>
            Iterator.filterL(prefix -> {
                ???
            }) |>
            Iterator.next
    }

    enum MatchResult[config] {
        case Exit
        case Item(MenuItem[config])
    }

    def matchOnMenu(
        menu: Menu[config],
        s: String
    ): Option[MatchResult[config]] = {
        let (items, return) = menu;
        let menuItems = items |> map(i -> (i.name, Item(i)));
        let withReturn = menuItems `List.append` ((return, Exit) :: Nil);
        let matches = withReturn |>
            List.filter(match (is, _) -> matchString(input = s, against = is));
        match matches {
            case (_, res) :: Nil => Some(res)
            case _ => None
        }
    }

}
