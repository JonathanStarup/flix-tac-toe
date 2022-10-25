use String.{lineSeparator => newline}

def menuItems(): List[String] = "play" :: "menu" :: Nil

def titleCard(): String = "${newline()}      ~ Tic ~ Tac ~ Toe ~${newline()}"

def introduction(): String = "Type out any command in the box"

def boxLines(barWidth: {barWidth = Int32}, padding: {padding = Int32}, lines: List[String]): List[String] = {
    let pad = String.repeat(padding.padding, " ");
    let barredLines = lines |> List.map(line -> "|${pad}${line}");
    let bar = "|${String.repeat(barWidth.barWidth - 1, "-")}";
    bar :: (barredLines `List.append` (List.point(bar)))
}

def menuString(items: List[String]): String = {
    let menuLine = List.join("  ", items);
    let extraWidth = 10;
    let barWidth = String.length(menuLine) + extraWidth;
    let boxLines = boxLines(barWidth = barWidth, padding = 2, List.point(menuLine));
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
