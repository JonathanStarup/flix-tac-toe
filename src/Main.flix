use String.{lineSeparator => newline}

def titleCard(): String =
    "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String =
    "Type out any command in the box"

def main(): Unit \ IO = {
    println(titleCard());
    println(introduction());
    let initConfig = {ansi = false};
    discard Menu.menuLoop(
        None,
        initConfig,
        mainMenu(),
        c1 -> c1.ansi
    )
}

def mainMenu(): Menu.Menu[{ansi = Bool | r}] = Menu.menu("exit",
    {
        name = "play",
        action = Menu.consoleAction(c ->
            Menu.menuLoop(
                None,
                c,
                playMenu(),
                c1 -> c1.ansi
            )
        )
    } :: {
        name = "settings",
        action = Menu.consoleAction(c ->
            Menu.menuLoop(
                Some(settingsString),
                c,
                settingsMenu(),
                c1 -> c1.ansi
            )
        )
    } ::
    Nil
)

def playMenu(): Menu.Menu[{ansi = Bool | r}] = Menu.menu("back",
    {
        name = "1 player",
        action = Menu.wip("1 player")
    } :: {
        name = "2 player",
        action = Menu.wip("2 player")
    } ::
    Nil
)

def settingsString(config: {ansi = Bool | r}): String = {
    String.unlines(
        "Current Settings:" ::
        "  ansi = ${config.ansi}"
        :: Nil
    )
}

def settingsMenu(): Menu.Menu[{ansi = Bool | r}] = Menu.menu("back",
    {
        name = "ansi",
        action = Menu.consoleAction(c -> Menu.menuLoop(
            Some(c1 -> "set ansi from ${c1.ansi} to:"),
            c,
            ansiSettingMenu(),
            c1 -> c1.ansi
        ))
    } ::
    Nil
)

def ansiSettingMenu(): Menu.Menu[{ansi = Bool | r}] = Menu.menu("cancel",
    {
        name = "true",
        action = Menu.returnAction(
            c1 -> {ansi = true | c1}
        )
    } :: {
        name = "false",
        action = Menu.returnAction(
            c1 -> {ansi = false | c1}
        )
    } ::
    Nil
)
