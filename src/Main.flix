use String.{lineSeparator => newline}

enum Action {
    case Exit
    case Action(Unit -> Unit \ IO)
}

type alias MenuItem = {name = String, action = Action}

def wip(feature: String): Unit -> Unit \ IO =
    () -> println("${feature} WIP")

def menuItems(): List[MenuItem] =
    {name = "play", action = Action(wip("play"))} ::
    {name = "menu", action = Action(wip("menu"))} ::
    {name = "quit", action = Exit} ::
    Nil

def matchMenuItem(s: String, item: MenuItem): Bool = {
    s == item.name
}

def matchOnItems(s: String, items: List[MenuItem]): Option[MenuItem] = {
    List.findLeft(matchMenuItem(s), items)
}

def titleCard(): String = "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String = "Type out any command in the box"

def boxLines(padding: {padding = Int32}, lines: List[String]): List[String] = {
    let pad = String.repeat(padding.padding, " ");
    // this is an upper bound if colors are present.
    let maxLine = lines |>
        List.map(String.length) |>
        List.maximum |>
        Option.getWithDefault(0);
    let barLength = maxLine + 2 * padding.padding + 1;
    let barredLines = lines |> List.map(line -> "|${pad}${line}");
    let bar = "|${String.repeat(barLength - 1, "-")}";
    bar :: (barredLines `List.append` (List.point(bar)))
}

def menuString(items: List[String]): String = {
    let menuLine = List.join("  ", items);
    let boxLines = boxLines(padding = 2, List.point(menuLine));
    newline() + List.join(newline(), boxLines)
}

def getInput(): Option[String] \ IO = {
    print("> ");
    Console.readLine()
}

def main(): Unit \ IO = {
    println(titleCard());
    println(introduction());
    mainLoop(menuItems())
}

def mainLoop(menuItems: List[MenuItem]): Unit \ IO = {
    def loop() = mainLoop(menuItems);
    println(menuString(menuItems |> List.map(item -> item.name)));
    match getInput() {
        case None =>
            loop()
        case Some(input) =>
            match matchOnItems(input, menuItems) {
                case None => loop()
                case Some(item) =>
                    match item.action {
                        case Exit => ()
                        case Action(action) =>
                            action();
                            loop()
                    }
            }
    }

}
