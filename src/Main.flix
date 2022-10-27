use Applicative.point
use Foldable.findLeft
use Foldable.foreach
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
    let initConfig = {colors = false};
    discard Menu.menuLoop(None, initConfig, mainMenu())
}

def mainMenu(): Menu.Menu[{colors = Bool | r}] = Menu.menu("exit",
    {
        name = "play",
        action = Menu.menuAction(playMenu())
    } :: {
        name = "settings",
        action = Menu.menuAction(settingsMenu())
    } ::
    Nil
)

def playMenu(): Menu.Menu[{colors = Bool | r}] = Menu.menu("back",
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.wip("2 player")
    } ::
    Nil
)

def settingsMenu(): Menu.Menu[{colors = Bool | r}] = Menu.menu("back",
    {
        name = "colors",
        action = Menu.action(config -> {
            Menu.menuLoop(Some("colors = ${config.colors}"), config, Menu.menu("cancel",
                {
                    name = "true",
                    action = Menu.returnAction(c -> {colors = true | c})
                } :: {
                    name = "false",
                    action = Menu.returnAction(c -> {colors = false | c})
                } ::
                Nil
            ))
        })
    } ::
    Nil
)

namespace Menu {

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
        case Action(config -> config \ IO)
        case ReturnAction(config -> config \ IO)
    }

    /// Some action.
    pub def action(f: config -> config \ ef): Action[config] = {
        Action(f as config -> config \ IO)
    }

    pub def returnAction(f: config -> config \ ef): Action[config] = {
        ReturnAction(f as config -> config \ IO)
    }

    /// The action to exit the menu.
    pub def return(): Action[config] = {
        ReturnAction(config -> config as \ IO)
    }

    /// An action that prints `"${feature} in development"`.
    pub def wip(feature: String): Action[config] = action(config -> {
        println("${feature} in development");
        config
    })

    /// `action(config -> menuLoop(None, config, menu))`
    pub def menuAction(menu: Menu[config]): Action[config] = {
        action(config -> menuLoop(None, config, menu))
    }

    /// `action(config -> menuLoop(Some(msg), config, menu))`
    pub def menuActionMsg(msg: String, menu: Menu[config]): Action[config] = {
        action(config -> menuLoop(Some(msg), config, menu))
    }

    pub def menuLoop(msg: Option[String], config: config, menu: Menu[config]): config \ IO = {
        def loop(c) = menuLoop(msg, c, menu);

        msg |> foreach(s -> println("|${String.repeat(3, " ")}${s}"));
        println(menuString(padding = 3, menu));

        match getMenuInput(menu) {
            case None => loop(config)
            case Some(Exit) => config
            case Some(Item(item)) => match item.action {
                case ReturnAction(action) => action(config)
                case Action(action) => loop(action(config))
            }

        }
    }

    def getMenuInput(menu: Menu[config]): Option[MatchResult[config]] \ IO = {
        flatMap(matchOnMenu(menu), getInput())
    }

    def getInput(): Option[String] \ IO = {
        print("> ");
        Console.readLine()
    }

    def menuString(padding: {padding = Int32}, menu: Menu[config]): String = {
        let (items, return) = menu;
        let pad = String.repeat(padding.padding, " ");
        let names = map(i -> i.name, items) `List.append` (return :: Nil);
        let menuLine = join(pad, names);
        let boxLines = boxLines(padding, point(menuLine));
        join(newline(), boxLines)
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

    def matchString(input: {input = String}, against: {against = String}): Bool = {
        input.input == against.against
    }

    def matchOnItem(s: String, item: MenuItem[config]): Bool = {
        matchString(input = s, against = item.name)
    }

    enum MatchResult[config] {
        case Exit
        case Item(MenuItem[config])
    }

    def matchOnMenu(menu: Menu[config], s: String): Option[MatchResult[config]] = {
        let (items, return) = menu;
        // check return before items to avoid being stuck.
        if (matchString(input = s, against = return)) {
            Some(Exit)
        } else {
            findLeft(matchOnItem(s), items) |> map(Item)
        }
    }

}
