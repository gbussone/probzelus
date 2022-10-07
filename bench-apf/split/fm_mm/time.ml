type time = Mtime_clock.counter

let start () = Mtime_clock.counter ()

let time start = Mtime.Span.to_ms (Mtime_clock.count start)
