def play(): Unit \ IO = {
    match characterSetup() {
        case Err(msg) =>
            println("ERR: ${msg}")
        case Ok(None) =>
            println("exited")
        case Ok(Some(c)) =>
            println("character:");
            println("  name: ${c.name}");
            println("  profession: ${c.prof}")
    }
}
enum Profession with Eq, ToString {
    case Smith
    case Student
}

def professions(): Nel[Profession] = {
    Nel.Nel(Smith, Student :: Nil)
}

def characterSetup(): Result[Option[{name = String, prof = Profession}], String] \ IO = {
    let nameRes = taPrompt(
        "Whats your name?",
        s -> match s {
            case _ if String.length(s) < 3 =>
                Err("Name must be at least three characters.")
            case _ if String.length(s) > 10 =>
                Err("Name must not be longer than ten characters.")
            case _ if not String.isMatch(regex = "[A-Za-z]+", s) =>
                Err("Name can only be one word with letters.")
            case _ => Ok(String.toLowerCase(s))
        },
        "A one word name using letters"
    ) |> map(n  -> {name = n});
    nameRes |> flatMap(data -> taSelection(
        "What's your profession?",
        Nel.map(ToString.toString, professions()) |> Nel.toList,
        "Type out one of the professions"
    ) |> map(profS -> match profS {
        case "Smith" => {+prof = Smith | data}
        case "Student" => {+prof = Student | data}
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

def taPrompt(
    text: String,
    parser: String -> Result[a, String],
    helpMsg: String
): Result[Option[a], String] \ IO = prompt(
    text,
    parser,
    helpMsg,
    Set#{"help"},
    Set#{"cancel"}
)

def taSelection(
    text: String,
    items: List[String],
    helpMsg: String
): Result[Option[String], String] \ IO = selection(
    text,
    items,
    40,
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
pub def prompt(
    text: String,
    parser: String -> Result[a, String],
    helpMsg: String,
    help: Set[String],
    cancel: Set[String]
): Result[Option[a], String] \ IO = {
    Terminal.clearScreenAndReset();
    println(text);
    println("------------");
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
            println(helpMsg);
            inner()
        case Ok(input) =>
            match parser(String.trim(input)) {
                case Ok(output) =>
                    Ok(Some(output))
                case Err(msg) =>
                    Terminal.restoreCursor();
                    Terminal.clearScreenAfterCursor();
                    println(msg);
                    inner()
            }
    };
    inner()
}

def selection(
    text: String,
    items: List[String],
    width: Int32,
    helpMsg: String,
    help: Set[String],
    cancel: Set[String]
): Result[Option[String], String] \ IO = region r {

    let maxWord =
        List.map(String.length, items) |>
        List.maximum |>
        Option.getWithDefault(0);
    let fixedWidth = Int32.max(
        maxWord+2 /* `+ ` */,
        width /* `+` */
    );
    let line = String.repeat(fixedWidth-1, "-");
    let delim = " | ";
    Terminal.clearScreenAndReset();
    println(text);
    println("+${line}");

    let first = ref true @ r;
    let sb = ref new StringBuilder(r) @ r;
    StringBuilder.append!("+ ", deref sb);
    foreach(word <- List.iterator(r, items)) {
        def tooBig() =
            StringBuilder.length(deref sb) +
            String.length(word) +
            String.length(delim)
            > fixedWidth;
        if (deref first) {
            first := false;
            StringBuilder.append!(word, deref sb)
        } else if (tooBig()) {
            println(StringBuilder.toString(deref sb));
            sb := new StringBuilder(r);
            StringBuilder.append!("+ ", deref sb);
            StringBuilder.append!(word, deref sb)
        } else {
            StringBuilder.append!(delim, deref sb);
            StringBuilder.append!(word, deref sb)
        }
    };
    println(StringBuilder.toString(deref sb));
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
            println(helpMsg);
            inner()
        case Ok(input) =>
            let trimmed = String.trim(input);
            match List.find(Eq.eq(trimmed), items) {
                case Some(output) =>
                    Ok(Some(output))
                case None =>
                    Terminal.restoreCursor();
                    Terminal.clearScreenAfterCursor();
                    println("Type in any item from the list");
                    inner()
            }
    };
    inner()
}
