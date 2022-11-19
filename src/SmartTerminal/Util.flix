namespace SmartTerminal/Util {
    use SmartTerminal.Output
    use SmartTerminal.StateOutput

    ///
    /// Unsafely unpacks result types.
    ///
    @Unsafe
    pub def crash(r: Result[a, b]): a with ToString[b] = match r {
        case Ok(v)    => v
        case Err(msg) => bug!("SmartTerminal.crash found error: ${msg}")
    }

    pub def mapOutput(f: a -> b \ ef, o: Output[a]): Output[b] \ ef = {
        use SmartTerminal.Output.{Return, Retry};
        match o {
            case Return(v) => Return(f(v))
            case Retry(msg) => Retry(msg)
        }
    }

    pub def mapStateOutput(f: a -> b \ ef, o: StateOutput[a, s]): StateOutput[b, s] \ ef = {
        use SmartTerminal.StateOutput.{Return, Retry};
        match o {
            case Return(v) => Return(f(v))
            case Retry(msg, s) => Retry(msg, s)
        }
    }

    /// Uses bold if `ansi` is true, otherwise returns the argument.
    pub def emph(ansi: Bool, s: String): String = match ansi {
        case true => Console.bold(s)
        case false => s
    }

    ///
    /// Extracts an output from a result. Useful for nested prompts.
    ///
    pub def resOutput(r: Result[Output[t], String]): Output[Result[t, String]] = {
        use SmartTerminal.Output.{Return, Retry};
        match r {
            case Ok(Return(v))  => Return(Ok(v))
            case Ok(Retry(msg)) => Retry(msg)
            case Err(msg)       => Return(Err(msg))
        }
    }

    ///
    /// Extracts a state output from a result. Useful for nested menus.
    ///
    pub def resStateOutput(r: Result[StateOutput[t, s], String]): StateOutput[Result[t, String], s] = {
        use SmartTerminal.StateOutput.{Return, Retry};
        match r {
            case Ok(Return(v))     => Return(Ok(v))
            case Ok(Retry(msg, s)) => Retry(msg, s)
            case Err(msg)          => Return(Err(msg))
        }
    }

    pub def optOutput(hint: String, o: Option[t]): Output[t] = {
        use SmartTerminal.Output.{Return, Retry};
        match o {
            case Some(v) => Return(v)
            case None    => Retry(hint)
        }
    }

    pub def optStateOutput(hint: String, s: s, o: Option[t]): StateOutput[t, s] = {
        use SmartTerminal.StateOutput.{Return, Retry};
        match o {
            case Some(v) => Return(v)
            case None    => Retry(hint, s)
        }
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    pub def flatten(r: Result[Result[t, e], e]): Result[t, e] = match r {
        case Ok(v)  => v
        case Err(e) => Err(e)
    }
}
