
type mavg = {
    mutable historicAvg:  float;    (* Number of events in this period *)
    mutable lastUpdateTS: float;    (* Last update time stamp *)
    mutable unprocessedEvents: int;
                        (* Number of events not counted in historicAvg *)
            period:       int;      (* 300 *)
    }

let mavg_new smoothing_window =
    { historicAvg = 0.0;
      period = smoothing_window;
      unprocessedEvents = 0;
      lastUpdateTS = Unix.gettimeofday (); }

let bump_rate m now events =
    let elapsed = now -. m.lastUpdateTS in
    if elapsed < 0.09 then
        m.unprocessedEvents <- m.unprocessedEvents + events
    else begin
        m.historicAvg <-
            (exp(-1.0 /. float_of_int(m.period))
                    *. (m.historicAvg -. float_of_int(m.unprocessedEvents))
             +. float_of_int(m.unprocessedEvents))
            *. exp((1.0 -. elapsed) /. float_of_int(m.period));
        m.unprocessedEvents <- events;
        m.lastUpdateTS <- now;
    end

let bump_value m now value =
    if m.unprocessedEvents = 0 then begin
        m.historicAvg <- value;
        m.unprocessedEvents <- 1;
        m.lastUpdateTS <- now
    end else begin
        let e = exp((m.lastUpdateTS -. now) /. float_of_int(m.period)) in
        let a = 1.0 -. e in
        m.historicAvg <- (a *. value) +. (e *. m.historicAvg);
        m.lastUpdateTS <- now
    end

let period m = m.period

let value_average m = m.historicAvg

let rate_average m =
    m.historicAvg *. float_of_int(m.period)

let current_rate_average m now =
    bump_rate m now 0;
    m.historicAvg *. float_of_int(m.period)

