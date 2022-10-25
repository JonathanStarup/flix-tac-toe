use String.{lineSeparator => newline}

def menuItems(): List[String] = "play" :: "menu" :: "quit" :: Nil

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

    println(menuString(menuItems()));
    discard getInput()
}
