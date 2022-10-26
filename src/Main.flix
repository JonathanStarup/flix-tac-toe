use Applicative.point
use Foldable.findLeft
use Foldable.join
use Functor.map
use Monad.flatMap
use Option.getWithDefault
use String.{lineSeparator => newline}

def titleCard(): String = "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String = "Type out any command in the box"

def main(): Unit \ IO = {
    println(titleCard());
    println(introduction());
    Menu.menuLoop(mainMenu())
}

def mainMenu(): Menu.Menu = Menu.addReturnItem("quit",
    {
        name = "play",
        action = Menu.menuAction(playMenu())
    } :: {
        name = "settings",
        action = Menu.menuAction(settingsMenu())
    } ::
    Nil
)

def playMenu(): Menu.Menu = Menu.addReturnItem("back",
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.wip("2 player")
    } ::
    Nil
)

def settingsMenu(): Menu.Menu = Menu.addReturnItem("back",
    {
        name = "colors",
        action = Menu.wip("colors")
    } ::
    Nil
)

namespace Menu {

    pub type alias Menu = List[MenuItem]

    pub type alias MenuItem = {name = String, action = Action}

    pub enum Action {
        case Action(Unit -> Unit \ IO)
        case Return
    }

    pub def addReturnItem(name: String, m: Menu): Menu =
        m `List.append` ({name = name, action = Return} :: Nil)

    pub def wip(feature: String): Action =
        Action(() -> println("${feature} in development"))

    pub def menuAction(menu: Menu): Action = {
        Action(() -> menuLoop(menu))
    }

    pub def menuLoop(menu: Menu): Unit \ IO = {
        def loop() = menuLoop(menu);

        println(menuString(padding = 3, menu));

        match getMenuInput(menu) {
            case None =>
                loop()
            case Some(item) =>
                runAction(item.action, loop)
        }
    }

    def getMenuInput(menu: Menu): Option[MenuItem] \ IO = {
        flatMap(matchOnItems(menu), getInput())
    }

    def getInput(): Option[String] \ IO = {
        print("> ");
        Console.readLine()
    }

    def menuString(padding: {padding = Int32}, menu: Menu): String = {
        let pad = String.repeat(padding.padding, " ");
        let menuLine = join(pad, map(i -> i.name, menu));
        let boxLines = boxLines(padding, point(menuLine));
        newline() + join(newline(), boxLines)
    }

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

    def matchMenuItem(s: String, item: MenuItem): Bool = {
        s == item.name
    }

    def matchOnItems(items: Menu, s: String): Option[MenuItem] = {
        findLeft(matchMenuItem(s), items)
    }

    def runAction(a: Action, postAction: Unit -> Unit \ ef): Unit \ {IO, ef} = match a {
        case Action(action) => action(); postAction()
        case Return => ()
    }

}
