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

def mainMenu(): Menu.Menu[Unit] =
    {
        name = "play",
        action = Menu.menuAction(playMenu())
    } :: {
        name = "settings",
        action = Menu.menuAction(settingsMenu())
    } ::
        Menu.returnItem("quit")
    ::
    Nil

def playMenu(): Menu.Menu[Unit] =
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.wip("2 player")
    } ::
        Menu.returnItem("back")
    ::
    Nil

def settingsMenu(): Menu.Menu[Unit] =
    {
        name = "colors",
        action = Menu.wip("colors")
    } ::
        Menu.returnItem("back")
    ::
    Nil

namespace Menu {

    // TODO: add a return item structurally
    ///
    /// A list of items that modify a `config` value.
    ///
    pub type alias Menu[config] = List[MenuItem[config]]

    ///
    /// An named item that can modify a `config` value.
    /// An item is invoked by its name verbatim.
    ///
    pub type alias MenuItem[config] = {name = String, action = Action[config]}

    ///
    /// An action on a `config` value. A `Return` action means that the
    /// contextual menu should close.
    ///
    pub enum Action[config] {
        case Action(config -> config \ IO)
        case Return
    }

    /// `Action(upcast(f))`
    pub def pureAction(f: config -> config): Action[config] = {
        Action(upcast(f))
    }

    /// `{name = name, action = Return}`
    pub def returnItem(name: String): MenuItem[config] =
        {name = name, action = Return}

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
