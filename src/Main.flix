use String.{lineSeparator => newline}

enum Action {
    case Action(Unit -> Unit \ IO)
    case Exit
}

def runAction(a: Action, postAction: Unit -> Unit \ ef): Unit \ {IO, ef} = match a {
    case Action(action) => action(); postAction()
    case Exit => ()
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

def matchOnItems(items: List[MenuItem], s: String): Option[MenuItem] = {
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

def menuString(menu: List[MenuItem]): String = {
    let menuLine = List.join("  ", List.map(i -> i.name, menu));
    let boxLines = boxLines(padding = 2, List.point(menuLine));
    newline() + List.join(newline(), boxLines)
}

def getInput(): Option[String] \ IO = {
    print("> ");
    Console.readLine()
}

def getMenuInput(menu: List[MenuItem]): Option[MenuItem] \ IO = {
    getInput() |>
        Option.flatMap(matchOnItems(menu))
}

def main(): Unit \ IO = {
    println(titleCard());
    println(introduction());
    mainLoop(menuItems())
}

def mainLoop(menu: List[MenuItem]): Unit \ IO = {
    def loop() = mainLoop(menu);

    println(menuString(menu));

    let optItem = getInput() |>
        Option.flatMap(matchOnItems(menu));
    match optItem {
        case None =>
            loop()
        case Some(item) =>
            runAction(item.action, loop)
    }

}
