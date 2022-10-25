use Applicative.point
use Foldable.findLeft
use Foldable.join
use Functor.map
use Monad.flatMap
use Option.getWithDefault
use String.{lineSeparator => newline}

enum Action {
    case Action(Unit -> Unit \ IO)
    case Return
}

def runAction(a: Action, postAction: Unit -> Unit \ ef): Unit \ {IO, ef} = match a {
    case Action(action) => action(); postAction()
    case Return => ()
}

type alias Menu = List[MenuItem]

type alias MenuItem = {name = String, action = Action}

def wip(feature: String): Action =
    Action(() -> println("WIP(${feature})"))

def addReturnItem(name: String, m: Menu): Menu =
    m `List.append` ({name = name, action = Return} :: Nil)

def playMenu(): Menu = addReturnItem("back",
    {name = "1 player", action = wip("1 player")} ::
    {name = "2 player", action = wip("2 player")} ::
    Nil
)

def mainMenu(): Menu = addReturnItem("quit",
    {name = "play", action = Action(() -> menuLoop(playMenu()))} ::
    {name = "menu", action = wip("menu")} ::
    Nil
)

def matchMenuItem(s: String, item: MenuItem): Bool = {
    s == item.name
}

def matchOnItems(items: Menu, s: String): Option[MenuItem] = {
    findLeft(matchMenuItem(s), items)
}

def titleCard(): String = "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String = "Type out any command in the box"

def boxLines(padding: {padding = Int32}, lines: List[String]): List[String] = {
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

def menuString(padding: {padding = Int32}, menu: Menu): String = {
    let pad = String.repeat(padding.padding, " ");
    let menuLine = join(pad, map(i -> i.name, menu));
    let boxLines = boxLines(padding, point(menuLine));
    newline() + join(newline(), boxLines)
}

def getInput(): Option[String] \ IO = {
    print("> ");
    Console.readLine()
}

def getMenuInput(menu: Menu): Option[MenuItem] \ IO = {
    flatMap(matchOnItems(menu), getInput())
}

def main(): Unit \ IO = {
    println(titleCard());
    println(introduction());
    menuLoop(mainMenu())
}

def menuLoop(menu: Menu): Unit \ IO = {
    def loop() = menuLoop(menu);

    println(menuString(padding = 3, menu));

    match getMenuInput(menu) {
        case None =>
            loop()
        case Some(item) =>
            runAction(item.action, loop)
    }

}
