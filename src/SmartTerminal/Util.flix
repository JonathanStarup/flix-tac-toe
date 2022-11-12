namespace SmartTerminal/Util {
    use SmartTerminal.Output

    ///
    /// Unsafely unpacks result types.
    ///
    @Unsafe
    pub def crash(r: Result[a, b]): a with ToString[b] = match r {
        case Ok(v) => v
        case Err(msg) => bug!("SmartTerminal.crash found error: ${msg}")
    }

    ///
    /// Extracts an output from a result. Useful for nested prompts.
    ///
    pub def resOutput(r: Result[Output[t], String]): Output[Result[t, String]] = {
        use SmartTerminal.Output.{Return, Retry};
        match r {
            case Ok(Return(v)) => Return(Ok(v))
            case Ok(Retry(msg)) => Retry(msg)
            case Err(msg) => Return(Err(msg))
        }
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `r`.
    ///
    pub def flatten(r: Result[Result[t, e], e]): Result[t, e] = match r {
        case Ok(v) => v
        case Err(e) => Err(e)
    }
}
