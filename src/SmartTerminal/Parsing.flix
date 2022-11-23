namespace SmartTerminal/Parsing {

pub def wordMatch(
    regex: String,
    ifWord: Unit -> t \ ef1,
    otherwise: String -> t \ ef2
): String -> t \ {ef1, ef2} = s0 -> {
    if (String.isMatch(regex = regex, s0)) {
        ifWord()
    } else {
        otherwise(s0)
    }
}

pub def choice(
    items: foldable[(String, Unit -> t \ ef1)],
    ifNot: Unit -> t \ ef2
): String -> t \ {ef1, ef2} with Foldable[foldable] = {
    s0 -> {
        match Foldable.findLeft(fst >> Eq.eq(s0), items) {
            case None => ifNot()
            case Some((_, action)) => action()
        }
    }
}
}
