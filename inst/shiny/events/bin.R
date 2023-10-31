
identify.bin.events = function(dt, t, length = 15, start = 0) {
    i = 1
    bin.start = start
    el = data.table()
    while(bin.start < max(dt[, get(t)])) {
        el = rbindlist(list(el, data.table(start = bin.start, end = bin.start + length)))
        bin.start = bin.start + length
        i = i + 1
    }

    el[, length := max(end - start, dt[2, get(t)] - dt[1, get(t)]), by = 1:nrow(el)] # minimum length of one time step at each event
    el[, end := start + length] # recalculate `end` as it may have been equal to `start` if the event was a single frame

    el
}
