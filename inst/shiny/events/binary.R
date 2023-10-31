identify.binary.events = function(dt, t, x, inverted = F) {
    suppressWarnings({
        signal = as.numeric(dt[, get(x)])
    })
    signal[which(is.na(signal))] = 0
    signal[which(is.null(signal))] = 0
    assert_that(length(unique(signal)) == 2, msg = "more than just 0 and 1 in selected binary event variable")

    if (inverted) {
        signal = !signal
    }

    # identify all state transitions (i.e. 0->1 and 1->0)
    indices = which((is.na(shift(signal, n = 1)) & (signal == 1)) |
                        ((shift(signal, n = 1) == 0) & (signal == 1)) |
                        ((shift(signal, n = 1) == 1) & (signal == 0)) |
                        (is.na(shift(signal, n = -1)) & (signal == 1)))
    assert_that(length(indices) %% 2 == 0, msg = "issue with state transition indices")

    # and parse into pairs to define our list of events
    el = data.table(start = numeric(), end = numeric())
    for (i in 1:length(indices)) {
        if (i %% 2 == 0) {
            el = rbindlist(list(el, data.table(start = dt[, get(t)][indices[i-1]], end = dt[, shift(get(t), n = 1)][indices[i]])))
        }
    }

    el[, length := max(end - start, dt[2, get(t)] - dt[1, get(t)]), by = 1:nrow(el)] # minimum length of one time step at each event
    el[, end := start + length] # recalculate `end` as it may have been equal to `start` if the event was a single frame

    el
}
