testValidFilters = function() {
    .ft = as.data.table(excel_to_R(input$events_filters))
    if (nrow(.ft) == 0) return(T)

    .valid = logical(nrow(.ft))
    for (n in 1:nrow(.ft)) {
        if (.ft[n, rule] %in% c("drop first N events", "drop last N events", "keep first N events", "keep last N events"))  {
            .valid[n] = .ft[n, as.integer(value)] >= 0

        } else if (.ft[n, rule] == "shift all events by X (sec)") {
            .valid[n] = !is.na(.ft[n, as.numeric(value)])

        } else if (.ft[n, rule] %in% c("pad to minimum X (sec)", "limit at maximum X (sec)", "drop if shorter than X (sec)", "drop if longer than X (sec)", "aggregate if within X (sec)", "include events before X (sec)", "include events after X (sec)")) {
            .valid[n] = .ft[n, as.numeric(value)] >= 0

        } else if (.ft[n, rule] == "aggregate if overlapping") {
            .valid[n] = T # this has no parameter

        } else if (.ft[n, rule] == "keep if N within X (sec)") {

            # ensure first argument evaluates to an integer
            .args = strsplit(.ft[n, value], ",")[[1]]

            if (length(.args) == 2) {
                .test1 = !is.na(as.integer(.args[1])) & as.integer(.args[1]) == as.numeric(.args[1])
                .test2 = !is.na(as.numeric(.args[2]))
                .valid[n] = .test1 & .test2
            } else {
                .valid[n] = F
            }

        } else if (.ft[n, rule] == "") {
            .valid[n] = T # this will be ignored
        }
    }
    all(.valid)
}

filter.events = function(.events, .filters) {

    # fixed.offset = NULL, drop.first = NULL, drop.last = NULL, keep.first = NULL, keep.last = NULL,
    #                      drop.shorter.than = NULL, drop.longer.than = NULL, pad.minimum = NULL, limit.maximum = NULL,
    #                      aggregate.within = NULL, drop.within = NULL, aggregate.overlap = NULL,
    #                      keep.n.within.x = NULL) {

    events = as.data.table(.events)
    events[, length := end - start]

    if (nrow(events) > 0) {
        #for (i in names(as.list(match.call()))) {
        for (n in 1:nrow(.filters)) {
            i = .filters[n, rule]

            if (i %in% c("fixed.offset", "drop.first", "drop.shorter.than", "drop.longer.than", "pad.minimum",
                "limit.maximum", "aggregate.within", "drop.within", "drop.last", "keep.first", "keep.last",
                "aggregate.overlap", "keep.n.within.x", "include.before", "include.after")) {

                if (i == "fixed.offset") {
                    fixed.offset = as.numeric(.filters[n, value])
                    # offset all events by a fixed amount of time
                    for (j in 1:nrow(events)) {
                        events[j, start := start + fixed.offset]
                        events[j, end := end + fixed.offset]
                    }

                } else if (i == "drop.first") {
                    drop.first = as.numeric(.filters[n, value])

                    # drop first N events
                    if (drop.first < nrow(events)) {
                        drop = c(rep(T, drop.first), rep(F, nrow(events) - drop.first))
                        events = events[!drop]
                    }

                } else if (i == "drop.last") {
                    drop.last = as.numeric(.filters[n, value])

                    # drop last N events
                    if (drop.last < nrow(events)) {
                        drop = c(rep(F, nrow(events) - drop.last), rep(T, drop.last))
                        events = events[!drop]
                    }

                } else if (i == "keep.first") {
                    keep.first = as.numeric(.filters[n, value])

                    # keep first N events
                    if (keep.first < nrow(events)) {
                        drop = c(rep(F, keep.first), rep(T, nrow(events) - keep.first))
                        events = events[!drop]
                    }


                } else if (i == "keep.last") {
                    keep.last = as.numeric(.filters[n, value])

                    # keep last N events
                    if (keep.last < nrow(events)) {
                        drop = c(rep(T, nrow(events) - keep.last), rep(F, keep.last))
                        events = events[!drop]
                    }

                } else if (i == "include.before") {
                    include.before = as.numeric(.filters[n, value])
                    drop = ifelse(events[, end] < include.before, F, T)
                    events = events[!drop]

                } else if (i == "include.after") {
                    include.after = as.numeric(.filters[n, value])
                    drop = ifelse(events[, start] >= include.after, F, T)
                    events = events[!drop]

                } else if (i == "drop.shorter.than") {
                    drop.shorter.than = as.numeric(.filters[n, value])

                    # drop events shorter than T
                    drop = events[, (end - start) >= drop.shorter.than]
                    events = events[drop]

                } else if (i == "drop.longer.than") {
                    drop.longer.than = as.numeric(.filters[n, value])

                    # drop events longer than T
                    drop = events[, (end - start) <= drop.longer.than]
                    events = events[drop]

                } else if (i == "pad.minimum") {
                    pad.minimum = as.numeric(.filters[n, value])

                    # pad event length to a minimum of T
                    events[, end := start + ifelse(length <= pad.minimum, pad.minimum, length)]

                } else if (i == "limit.maximum") {
                    limit.maximum = as.numeric(.filters[n, value])

                    # limit maximum event length to T
                    events[, end := start + ifelse(length >= limit.maximum, limit.maximum, length)]

                } else if (i == "aggregate.overlap") {
                    # aggregate events occurring inside of the previous one
                    if (nrow(events) > 1) {
                        #print(events)
                        for (j in nrow(events):2) {
                            if ((events[j - 1, start] < events[j, start]) & (events[j, start] <= events[j - 1, end])) {
                                events[j - 1, `:=`(end = events[j, end], length = events[j, end] - start)]
                                events = events[-j]
                            }
                        }
                    }

                } else if (i == "drop.within") {
                    drop.within = as.numeric(.filters[n, value])

                    # drop an event if it occurs within T seconds of any previous
                    drop = rep(F, nrow(events))

                    if (nrow(events) > 1) {
                        for (j in 1:(nrow(events) - 1)) {
                            if (!drop[j]) {
                                for (k in (j+1):nrow(events)) {
                                    if (events[k, start] < events[j, end] + drop.within) {
                                        drop[k] = T
                                    }
                                }
                            }
                        }
                    }
                    events = events[!drop]

                } else if (i == "aggregate.within") {
                    # aggregate overlapping events if at least within T seconds
                    aggregate.within = as.numeric(.filters[n, value])

                    if (nrow(events) > 1) {
                        new.events = events[.0,]

                        # probably safe to assume `events` is sorted on $start already
                        event.start = events[1, start]
                        .s = events[1, start]
                        for (j in 1:nrow(events)) {
                            if (events[j, start] - .s <= aggregate.within) {
                                # event j starts close enough to event j-1, keep going
                                .s = events[j, end]
                            } else {
                                # event j is too far away from current event to keep going, add to list
                                new.events = rbindlist(list(new.events, data.table(start = event.start, end = events[j-1, end], length = events[j-1, end] - event.start)))
                                .s = events[j, end]
                                event.start = events[j, start]
                            }

                            #print(new.events)
                        }
                        # add final event

                        events = rbindlist(list(new.events, data.table(start = event.start, end = events[nrow(events), end], length = events[nrow(events), end] - event.start)))
                    }

                } else if (i == "keep.n.within.x") {
                    .args = strsplit(.filters[n, value], ",")[[1]]
                    .test1 = !is.na(as.integer(.args[1])) & as.integer(.args[1]) == as.numeric(.args[1])
                    .test2 = !is.na(as.numeric(.args[2]))

                    assert_that((length(.args) == 2) & .test1 & .test2, msg = "keep.n.within.x argument format should be: integer,numeric")

                    min.n = as.integer(.args[1])
                    span.x = as.numeric(.args[2])

                    # keep only if 'min.n' occur within 'span.x' seconds of each other
                    drop = rep(F, nrow(events))

                    if (nrow(events) > 1) {
                        for (j in 1:nrow(events)) {
                            e.e = (events[j, end] + span.x)
                            e.s = (events[j, start] - span.x)
                            drop[j] = events[(end >= e.s) & (start <= e.e), .N] < min.n
                        }
                    }

                    events = events[!drop]
                }
            }

            events[, length := end - start]
            printf("%s %f\n", i, events[, .N])
        }
    }

    events
}
