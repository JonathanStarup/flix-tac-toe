pub def main(): Unit \ IO = match Cli.run() {
    case Ok() => ()
    case Err(msg) =>
        println();
        println("An error occurred:");
        println(msg)
}
