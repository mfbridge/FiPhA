# peak


identify.peak.events = function(dt, t, x, sd.peak.n = 3, sd.peak.width = 1, prefix="peak", ...) {

    n = round(sd.peak.width / dt[, shift(get(t), n = -1) - get(t)][[1]], digits = 0)

    .x = as.vector(dt[, get(x)])

    .xs = as.vector(dt[, shift(get(x), n = round(-n/2))])
    .mean = frollmean(.xs, n, na.rm = T)
    .sd = frollapply(.xs, n, \(...) { sd(..., na.rm = T) })

    .signal = as.numeric(.x > .mean + sd.peak.n * .sd)



    event = data.table(x = .x, rollmean = .mean, rollsd = .sd, signal = .signal)

    event[, test := rollmean + sd.peak.n * rollsd]
    event[is.na(signal), signal := 0]

    # identify all state transitions (i.e. 0->1 and 1->0)
    indices = which(event[, (is.na(shift(signal, n = 1)) & (signal == 1)) |
                        ((shift(signal, n = 1) == 0) & (signal == 1)) |
                        ((shift(signal, n = 1) == 1) & (signal == 0)) |
                        (is.na(shift(signal, n = -1)) & (signal == 1))])
    assert_that(length(indices) %% 2 == 0)

    el = data.table(start = numeric(), end = numeric())
    if (length(indices) > 0) {
        for (i in 1:length(indices)) {
            if (i %% 2 == 0) {
                el = rbindlist(list(el, data.table(start = dt[, get(t)][indices[i-1]], end = dt[, shift(get(t), n = 1)][indices[i]])))
            }
        }
    }

    el[, length := max(end - start, dt[2, get(t)] - dt[1, get(t)]), by = 1:nrow(el)] # minimum length of one time step at each event
    el[, end := start + length] # recalculate `end` as it may have been equal to `start` if the event was a single frame

    el
}
