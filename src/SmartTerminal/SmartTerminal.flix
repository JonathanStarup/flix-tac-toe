namespace SmartTerminal {
use Foldable.join
use Functor.map
use SmartTerminal/Formatting.Formatter
use SmartTerminal/Strings.parBreak

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
    startHint: {startHint = String},
    parser: String -> Output[t] \ ef,
    text: String
): Result[t, String] \ {IO, ef} = {
    use Result.flatMap;

    if (ansi) Terminal.clearScreenAndReset() else ();
    print(formatter({text = text, info = startHint.startHint}));
    let* line = Console.readLine();
    let parserOutput = parser(line);
    match parserOutput {
        case Output.Return(output) =>
            Ok(output)
        case Output.Retry(msg) =>
            prompt(ansi, formatter, startHint = msg, parser, text)
    }
}

pub enum StateOutput[t, s] {
    case Return(t)
    case Retry(String, s)
}

pub def statePrompt(
    ansi: Bool,
    formatter: Formatter,
    startHint: {startHint = String},
    config: config,
    parser: (String, config) -> StateOutput[t, config] \ ef1,
    text: config -> String \ ef2
): Result[t, String] \ {IO, ef1, ef2} = {
    use Result.flatMap;

    if (ansi) Terminal.clearScreenAndReset() else ();
    print(formatter({text = text(config), info = startHint.startHint}));
    let* line = Console.readLine();
    let parserOutput = parser(line, config);
    match parserOutput {
        case StateOutput.Return(output) =>
            Ok(output)
        case StateOutput.Retry(msg, config1) =>
            statePrompt(ansi, formatter, startHint = msg, config1, parser, text)
    }
}

// -------------------------------------------------------------------------
// Shorthand Defs ----------------------------------------------------------
// -------------------------------------------------------------------------

/// `"Press enter to ${x}"`
pub def enterToX(
    ansi: Bool,
    formatter: Formatter,
    x: {x = String},
    text: String
): Result[Unit, String] \ IO = {
    prompt(
        ansi,
        formatter,
        startHint = "Press enter to ${x.x}",
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
    startHint = "",
    ("y", () -> Output.Return(true)) ::
        ("n", () -> Output.Return(false)) ::
        Nil,
    delim = "/",
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
    startHint: {startHint = String},
    items: foldable[(String, Unit -> Output[t] \ ef)],
    delim: {delim = String},
    text: String
): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
    use SmartTerminal/Parsing.choice;
    use String.{trim, toLowerCase};

    let itemList = Foldable.toList(items);
    let helpMsg = join(delim.delim, map(fst, itemList));
    let items1 = itemList |> map(match (s, a) -> (trim(toLowerCase(s)), a));
    let parser =
        trim >>
        toLowerCase >>
        choice(items1, () -> Output.Retry(helpMsg));
    prompt(
        ansi,
        formatter,
        startHint = "${startHint.startHint}${helpMsg}",
        parser,
        text
    )
}

pub def menu(
    ansi: Bool,
    formatter: Formatter,
    startHint: {startHint = String},
    config: config,
    items: foldable[(String, config -> StateOutput[t, config] \ ef)],
    delim: {delim = String},
    text: config -> String
): Result[t, String] \ {IO, ef} with Foldable[foldable] = {
    use Result.flatMap;
    use SmartTerminal/Parsing.choice;
    use String.{trim, toLowerCase};

    let itemList = Foldable.toList(items);
    let helpMsg = join(delim.delim, map(fst, itemList));
    let items1 = itemList |> map(match (s, a) -> (
        trim(toLowerCase(s)),
        () -> conf -> match a(conf) {
            case StateOutput.Return(v) =>
                StateOutput.Return(v)
            case StateOutput.Retry(msg, conf1) =>
                StateOutput.Retry("${msg}${helpMsg}", conf1)
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
        startHint = "${startHint.startHint}${helpMsg}",
        config,
        parser,
        text
    )
}

pub def ansiPrompt(): Result[Bool, String] \ IO = {
    yesNo(
        false,
        SmartTerminal/Formatting.multiLine(), join("",
            "Do you want to enable ANSI?" ::
            parBreak() ::
            "if the folliwing text is bold, then ANSI works:" ::
            " ${Console.bold("bold")}" :: Nil
        )
    )
}
}
