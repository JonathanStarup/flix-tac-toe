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
    let initConfig = ();
    Menu.menuLoop(mainMenu(), initConfig)
}

def mainMenu(): Menu.Menu[Unit] = Menu.addReturnItem("quit",
    {
        name = "play",
        action = Menu.menuAction(playMenu())
    } :: {
        name = "settings",
        action = Menu.menuAction(settingsMenu())
    } ::
    Nil
)

def playMenu(): Menu.Menu[Unit] = Menu.addReturnItem("back",
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.wip("2 player")
    } ::
    Nil
)

def settingsMenu(): Menu.Menu[Unit] = Menu.addReturnItem("back",
    {
        name = "colors",
        action = Menu.wip("colors")
    } ::
    Nil
)

namespace Menu {

    pub type alias Menu[config] = List[MenuItem[config]]

    pub type alias MenuItem[config] = {name = String, action = Action[config]}

    pub enum Action[config] {
        case Action(config -> config \ IO)
        case Return
    }

    pub def pureAction(f: config -> config): Action[config] = {
        Action(upcast(f))
    }

    pub def addReturnItem(name: String, m: Menu[config]): Menu[config] =
        m `List.append` ({name = name, action = Return} :: Nil)

    pub def wip(feature: String): Action[config] =
        Action(config -> {
            println("${feature} in development");
            config
        })

    pub def menuAction(menu: Menu[config]): Action[config] = {
        Action(config -> menuLoop(menu, config))
    }

    pub def menuLoop(menu: Menu[config], config: config): config \ IO = {
        let loop = menuLoop(menu);

        println(menuString(padding = 3, menu));

        match getMenuInput(menu) {
            case None =>
                loop(config)
            case Some(item) => match item.action {
                case Return => config
                case Action(action) =>
                    loop(action(config))
            }
        }
    }

    def getMenuInput(menu: Menu[config]): Option[MenuItem[config]] \ IO = {
        flatMap(matchOnItems(menu), getInput())
    }

    def getInput(): Option[String] \ IO = {
        print("> ");
        Console.readLine()
    }

    def menuString(padding: {padding = Int32}, menu: Menu[config]): String = {
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

    def matchMenuItem(s: String, item: MenuItem[config]): Bool = {
        s == item.name
    }

    def matchOnItems(items: Menu[config], s: String): Option[MenuItem[config]] = {
        findLeft(matchMenuItem(s), items)
    }

}
