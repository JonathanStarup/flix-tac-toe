namespace SmartTerminal/Formatting {
use StringBuilder.append!
use StringBuilder.appendLine!
use StringBuilder.toString

pub type alias Formatter = {text = String, info = String} -> String

pub def oneLine(): Formatter = input -> region r {
    let sb = new StringBuilder(r);
    if (input.text != "") append!(input.text, sb) else ();
    if (input.info != "") append!(" (${input.info})", sb) else ();
    append!(": ", sb);
    toString(sb)
}

pub def multiLine(): Formatter = input -> region r {
    let sb = new StringBuilder(r);
    if (input.text != "") appendLine!(input.text, sb) else ();
    appendLine!("-----", sb);
    if (input.info != "") appendLine!(input.info, sb) else ();
    append!("> ", sb);
    toString(sb)
}
}
