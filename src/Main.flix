use String.{lineSeparator => newline}

def titleCard(): String =
    "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String =
    "Type out any command in the box"

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
        action = Menu.menuActionMsg(settingsString, settingsMenu())
    } :: {
        name = "external settings",
        action = Menu.wip("external settings")
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

def settingsString(config: {colors = Bool | r}): String = {
    String.unlines(
        "Current Settings:" ::
        "  colors = ${config.colors}"
        :: Nil
    )
}

def settingsMenu(): Menu.Menu[{colors = Bool | r}] = Menu.menu("back",
    {
        name = "colors",
        action = Menu.menuActionMsg(
            c -> "set colors from ${c.colors} to:",
            colorMenu())
    } ::
    Nil
)

def colorMenu(): Menu.Menu[{colors = Bool | r}] = Menu.menu("cancel",
    {
        name = "true",
        action = Menu.returnAction(c -> {colors = true | c})
    } :: {
        name = "false",
        action = Menu.returnAction(c -> {colors = false | c})
    } ::
    Nil
)
