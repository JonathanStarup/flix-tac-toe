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
        action = config -> {
            Menu.menuLoop(Some("colors = ${config.colors}"), config, Menu.menu("cancel",
                {
                    name = "true",
                    action = Menu.pureAction(c -> {colors = true | c})
                } :: {
                    name = "false",
                    action = Menu.pureAction(c -> {colors = false | c})
                } ::
                Nil
            ))
        }
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

    // TODO: add terminal actions
    ///
    /// An action on a `config` value.
    ///
    pub type alias Action[config] = config -> config \ IO

    /// `upcast(f)`
    pub def pureAction(f: config -> config): Action[config] = {
        upcast(f)
    }

    /// An action that prints `"${feature} in development"`.
    pub def wip(feature: String): Action[config] = config -> {
            println("${feature} in development");
            config
        }

    /// `config -> menuLoop(None, config, menu)`
    pub def menuAction(menu: Menu[config]): Action[config] = {
        config -> menuLoop(None, config, menu)
    }

    /// `config -> menuLoop(Some(msg), config, menu)`
    pub def menuActionMsg(msg: String, menu: Menu[config]): Action[config] = {
        config -> menuLoop(Some(msg), config, menu)
    }

    pub def menuLoop(msg: Option[String], config: config, menu: Menu[config]): config \ IO = {
        def loop(c) = menuLoop(msg, c, menu);

        msg |> foreach(s -> println("|${String.repeat(3, " ")}${s}"));
        println(menuString(padding = 3, menu));

        match getMenuInput(menu) {
            case None => loop(config)
            case Some(Return) => config
            case Some(Item(item)) => loop((item.action)(config))
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
        case Return
        case Item(MenuItem[config])
    }

    def matchOnMenu(menu: Menu[config], s: String): Option[MatchResult[config]] = {
        let (items, return) = menu;
        // check return before items to avoid being stuck.
        if (matchString(input = s, against = return)) {
            Some(Return)
        } else {
            findLeft(matchOnItem(s), items) |> map(Item)
        }
    }

}
