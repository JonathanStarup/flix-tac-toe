namespace TA {
    pub def play(): Unit \ IO = {
        match characterSetup() {
            case Err(msg) =>
                println("ERR: ${msg}")
            case Ok(None) =>
                println("exited")
            case Ok(Some(c)) =>
                let data = {player = c};
                story(data);
                println("character:");
                println("  name: ${c.name}");
                println("  profession: ${c.prof}")
        }
    }

    def consoleWidth(): Int32 = 50

    type alias Data = {
        player = {
            name = String,
            prof = Profession
        }
    }

    namespace Constants {
        pub def city(): String = "Nuet Benig"
    }

    def story(data: Data): Unit \ IO = {
        enterToContinue(
            List.join(" ",
                "You are the ${emph(data.player.prof)} named" ::
                "${emph(data.player.name)} in the place" ::
                "${emph(TA/Constants.city())}." :: Nil
            ) ::
            Nil
        );
        enterToContinue(
            "Another being walks up to you." ::
            quotes("Are you okay? You look kind of messed up.") ::
            Nil
        )
    }

    enum Profession with Eq, ToString {
        case Constructor
        case Learner
        case PeoplePerson
    }

    def professions(): Nel[Profession] = {
        Nel.Nel(Constructor, Learner :: PeoplePerson :: Nil)
    }

    def characterSetup(): Result[Option[{name = String, prof = Profession}], String] \ IO = {
        let nameRes = taPrompt(
            "Whats your name?" ::
            Nil,
            s -> match s {
                case _ if String.length(s) < 3 =>
                    Err("Name must be at least three characters.")
                case _ if String.length(s) > 10 =>
                    Err("Name must not be longer than ten characters.")
                case _ if not String.isMatch(regex = "[A-Za-z]+", s) =>
                    Err("Name can only be one word with letters.")
                case _ if not String.isMatch(regex = "[A-Z][a-z]+", s) =>
                    Err("Name must be properly capitalized")
                case _ => Ok(s)
            },
            "A one word name using letters"
        ) |> map(n  -> {name = n});
        nameRes |> flatMap(data -> taSelection(
            "${data.name}... Nice name." ::
            " What's your profession?" ::
            Nil,
            Nel.map(ToString.toString, professions()) |> Nel.toList,
            "Type out one of the professions"
        ) |> map(profS -> match profS {
            case "Constructor" => {+prof = Constructor | data}
            case "Learner" => {+prof = Learner | data}
            case "PeoplePerson" => {+prof = PeoplePerson | data}
            case _ => ?unreachable
        }))
    }

    def flatMap(
        f: a -> Result[Option[b], err] \ ef,
        x: Result[Option[a], err]
    ): Result[Option[b], err] \ ef with ToString[err] = {
        match x {
            case Ok(Some(a)) => f(a)
            case Ok(None) => Ok(None)
            case Err(msg) => Err(msg)
        }
    }

    def map(
        f: a -> b \ ef,
        x: Result[Option[a], err]
    ): Result[Option[b], err] \ ef with ToString[err] = {
        match x {
            case Ok(Some(a)) => Ok(Some(f(a)))
            case Ok(None) => Ok(None)
            case Err(msg) => Err(msg)
        }
    }

    def enterToContinue(text: List[String]): Unit \ IO = {
        discard prompt(
            text,
            consoleWidth(),
            _ -> Ok(()),
            "",
            Set.empty(),
            Set.empty()
        )
    }

    def taPrompt(
        text: List[String],
        parser: String -> Result[a, String],
        helpMsg: String
    ): Result[Option[a], String] \ IO = prompt(
        text,
        consoleWidth(),
        parser,
        helpMsg,
        Set#{"help"},
        Set#{"cancel"}
    )

    def taSelection(
        text: List[String],
        items: List[String],
        helpMsg: String
    ): Result[Option[String], String] \ IO = selection(
        text,
        items,
        consoleWidth(),
        helpMsg,
        Set#{"help"},
        Set#{"cancel"}
    )

    def getInput(): Result[String, String] \ IO = {
        print("> ");
        match Console.readLine() {
            case None => Err("Terminal input error, perhaps EOF")
            case Some(input) => Ok(input)
        }
    }

    /// Ok(None) means a cancel cmd.
    /// Errors should not be reattempted.
    def prompt(
        text: List[String],
        width: Int32,
        parser: String -> Result[a, String],
        helpMsg: String,
        help: Set[String],
        cancel: Set[String]
    ): Result[Option[a], String] \ IO = {
        let w0 = Int32.max(1, width);
        Terminal.clearScreenAndReset();
        println(clampParagraphs(text, w0));
        println(String.repeat(w0, "-"));
        Terminal.saveCursor();
        println("");
        def inner() = match getInput() {
            case Err(msg) =>
                Err(msg)
            case Ok(input) if Set.exists(Eq.eq(input), cancel) =>
                Ok(None)
            case Ok(input) if Set.exists(Eq.eq(input), help) =>
                Terminal.restoreCursor();
                Terminal.clearScreenAfterCursor();
                println(clampText(helpMsg, w0));
                inner()
            case Ok(input) =>
                match parser(String.trim(input)) {
                    case Ok(output) =>
                        Ok(Some(output))
                    case Err(msg) =>
                        Terminal.restoreCursor();
                        Terminal.clearScreenAfterCursor();
                        println(clampText(msg, w0));
                        inner()
                }
        };
        inner()
    }

    def selection(
        text: List[String],
        items: List[String],
        width: Int32,
        helpMsg: String,
        help: Set[String],
        cancel: Set[String]
    ): Result[Option[String], String] \ IO = {
        let w0 = Int32.max(1, width);
        let line = String.repeat(w0-1, "-");
        let delim = " | ";
        Terminal.clearScreenAndReset();
        println(clampParagraphs(text, w0));

        println("+${line}");
        let itemLines = clampText(List.join(delim, items), w0-2) |>
            String.lines;
        println(itemLines |> List.map(l -> "+ ${l}") |> List.join(String.lineSeparator()));
        println("+${line}");

        Terminal.saveCursor();
        println("");
        def inner() = match getInput() {
            case Err(msg) =>
                Err(msg)
            case Ok(input) if Set.exists(Eq.eq(input), cancel) =>
                Ok(None)
            case Ok(input) if Set.exists(Eq.eq(input), help) =>
                Terminal.restoreCursor();
                Terminal.clearScreenAfterCursor();
                println(clampText(helpMsg, w0));
                inner()
            case Ok(input) =>
                let trimmed = String.trim(input);
                match List.find(Eq.eq(trimmed), items) {
                    case Some(output) =>
                        Ok(Some(output))
                    case None =>
                        Terminal.restoreCursor();
                        Terminal.clearScreenAfterCursor();
                        println(clampText("Type in any item from the list", w0));
                        inner()
                }
        };
        inner()
    }

    def clampParagraphs(ps: List[String], width: Int32): String = {
        let break = String.repeat(2, String.lineSeparator());
        ps |> List.map(p -> clampText(p, width)) |> List.join(break)
    }

    def clampText(text: String, width: Int32): String = region r {
        let words = String.words(text);

        let first = ref true @ r;
        let sb = ref new StringBuilder(r) @ r;
        let lines = new MutList(r);
        foreach(word <- List.iterator(r, words)) {
            def tooBig() =
                StringBuilder.length(deref sb) +
                String.length(word) +
                String.length(" ")
                > width;
            if (deref first) {
                first := false;
                StringBuilder.append!(word, deref sb)
            } else if (tooBig()) {
                MutList.push!(StringBuilder.toString(deref sb), lines);
                sb := new StringBuilder(r);
                StringBuilder.append!(word, deref sb)
            } else {
                StringBuilder.append!(" ", deref sb);
                StringBuilder.append!(word, deref sb)
            }
        };
        if (not List.isEmpty(words)) {
            MutList.push!(StringBuilder.toString(deref sb), lines)
        } else ();
        MutList.join(String.lineSeparator(), lines)
    }

    def emph(s: a): String with ToString[a] = Console.bold(s)

    def quotes(s: a): String with ToString[a] = "\"${s}\""
}
