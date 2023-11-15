
extract_event_datasets = function(events, .intervals, series.name = "") {
    datasets = list()

    for (f in input$events_dataset2) { #names(data$series)) {
        datasets[[f]] = list()

        for (s in series.name) { #names(data$series[[f]])) {

            datasets[[f]][[s]] = list()
            intervals = .intervals #as.data.table(excel_to_R(input$events_intervals)) #as.data.table(data$series[[f]][[s]]$intervals)
            intervals[, `:=`(start = as.numeric(start), end = as.numeric(end))]
            #intervals[, `:=`(start = as.numeric(start), end = as.numeric(start))]
            #View(intervals)

            for (e in 1:nrow(events)) {

                s.dataset = data$raw[[f]]
                s.time = data$meta[[f]]$time
                .epsilon = (2/3)*(s.dataset[2, get(s.time)] - s.dataset[1, get(s.time)])

                min.time = Inf
                max.time = -Inf


                #print(events[e,])
                # figure out bounds of event data (min/max referred to by an interval)
                for (i in 1:nrow(intervals)) {
                    if (intervals[i, reference] == "event signal") {
                        # just highlight the event itself
                        min.time = min(min.time, events[e, start])
                        max.time = max(max.time, events[e, end])

                    } else if (intervals[i, reference] == "before event") {
                        # an interval that occurs relative to event t_0
                        min.time = min(min.time, events[e, start] + intervals[i, start])
                        max.time = max(max.time, events[e, start] + intervals[i, end])


                    } else if (intervals[i, reference] == "before event, non-overlapping") {
                        # occurs before, but clip if overlapping with previous event
                        if (e > 1) {
                            min.time = min(min.time, max(datasets[[f]][[s]][[e-1]][, max(get(s.time), na.rm = T)], events[e, start] + intervals[i, start]))
                        } else {
                            min.time = min(min.time, events[e, start] + intervals[i, start])
                        }
                        max.time = max(max.time, events[e, start] + intervals[i, end])

                    } else if (intervals[i, reference] == "after event") {
                        # an interval that occurs relative to event t_max
                        min.time = min(min.time, events[e, end] + intervals[i, start])
                        max.time = max(max.time, events[e, end] + intervals[i, end])


                    } else if (intervals[i, reference] == "within event") {
                        # an interval that only occurs during an event
                        min.time = min(min.time, events[e, start] + intervals[i, start])
                        max.time = max(max.time, min(events[e, end], events[e, start] + intervals[i, end]))

                    }


                }

                event.dataset = data$raw[[f]][get(s.time) - min.time >= -.epsilon & get(s.time) - max.time < .epsilon, ]
                event.dataset[, `(event time)` := get(s.time) - events[e, start]]
                event.dataset[, `(interval)` := "" ]
                event.dataset[, `(interval time)` := numeric()]
                #event.dataset[, `(event index)` := round((get(s.time) - events[e, start]) / .epsilon)]
                #print(dim(event.dataset)/25)


                #print(intervals)
                # assign intervals to event.dataset
                for (i in 1:nrow(intervals)) {
                    i.start = NA
                    i.end = NA

                    if (intervals[i, reference] == "event signal") {
                        i.start = events[e, start]
                        i.end = events[e, end]

                    } else if (intervals[i, reference] == "before event") {
                        i.start = events[e, start] + intervals[i, start]
                        i.end = events[e, start] + intervals[i, end]

                    } else if (intervals[i, reference] == "before event, non-overlapping") {
                        if (e > 1) {
                            i.start = max(datasets[[f]][[s]][[e-1]][, max(get(s.time), na.rm = T)], events[e, start] + intervals[i, start])
                        } else {
                            i.start = events[e, start] + intervals[i, start]
                        }
                        i.end = events[e, start] + intervals[i, end]

                    } else if (intervals[i, reference] == "after event") {
                        i.start = events[e, end] + intervals[i, start]
                        i.end = events[e, end] + intervals[i, end]
                    }

                    # adjust start/end to beginning/end of datasets if they are exceeded
                    i.start = max(i.start, s.dataset[, min(get(s.time), na.rm = T)])
                    i.end = min(i.end, s.dataset[, max(get(s.time), na.rm = T)])


                    event.dataset[(get(s.time) - i.start) >= -.epsilon & (get(s.time) - i.end) < .epsilon,
                        `:=`(`(interval)` = intervals[i, name], `(interval time)` = get(s.time) - i.start)] ## assign interval

                    #printf("%s %f %d\n", intervals[i, name], i.end - i.start, event.dataset[`(interval)` == intervals[i, name], .N])

                }

                for (r in data$series[[f]][[s]]$responses) {
                    if (data$series[[f]][[s]]$normalization$type == "none") {
                        # do nothing

                    } else if (data$series[[f]][[s]]$normalization$type == "z" & event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, .N] > 0) {
                        fun = ifelse(data$series[[f]][[s]]$normalization$summary == "mean", mean, median)
                        reference = event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, .(mu = fun(get(r), na.rm = T), sig = sd(get(r), na.rm = T))]
                        event.dataset[, (r) := (get(r) - reference$mu) / reference$sig]

                    } else if (data$series[[f]][[s]]$normalization$type == "%" & event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, .N] > 0) {
                        fun = ifelse(data$series[[f]][[s]]$normalization$summary == "mean", mean, median)
                        reference = event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, .(mu = fun(get(r), na.rm = T), sig = sd(get(r), na.rm = T))]
                        event.dataset[, (r) := (get(r) - reference$mu) / reference$mu * 100]

                    } else if (data$series[[f]][[s]]$normalization$type == "r" & event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, .N] > 0) {
                        # robust z-score from pMAT is a z-score with medians referenced to the %dF/F transformed baseline values and using the median absolute deviation

                        event = event.dataset[, get(r)]
                        baseline = event.dataset[`(interval)` == data$series[[f]][[s]]$normalization$reference, get(r)]

                        dff.baseline = (baseline - mean(baseline)) / mean(baseline) * 100
                        dff.event = (event - mean(event)) / mean(event) * 100

                        mad = median(abs(dff.baseline - median(dff.baseline)))

                        event.dataset[, (r) := (dff.event - median(dff.baseline)) / mad]


                    }

                }

                datasets[[f]][[s]][[e]] = copy(event.dataset)
            }


        }


    }

    datasets
}

# observeEvent(input$events_dataset, {
#     updatePickerInput(session, "events_binary_variable", choices = getCommonDatasetVariables(input$events_dataset), selected = input$events_binary_variable)
#     updatePickerInput(session, "events_peakwindow_variable", choices = getCommonDatasetVariables(input$events_dataset), selected = input$events_peakwindow_variable)
# })



# update pre-generated event series name --------------------------------------------------------------------------

getEventsPlaceholderName = function() {

}

observeEvent(c(input$events_type, input$events_binary_variable, input$events_binned_start, input$events_binned_length, input$events_peakwindow_variable,
    input$events_peakwindow_width, input$events_peakwindow_n, input$events_fixed_list, input$events_conditions), {
    if (input$events_name == "") {
        if (input$events_type == "binary") {
            updateTextInput(session, "events_name", placeholder = sprintf("binary, %s", input$events_binary_variable))

        } else if (input$events_type == "binaryinv") {
            updateTextInput(session, "events_name", placeholder = sprintf("binary (inverted), %s", input$events_binary_variable))

        } else if (input$events_type == "binned") {
            updateTextInput(session, "events_name", placeholder = sprintf("binned every %0.3f sec, t0=%0.3f", input$events_binned_length, input$events_binned_start))

        } else if (input$events_type == "peak") {
            updateTextInput(session, "events_name", placeholder = sprintf("peak, win=%0.3fs n.sd=%0.3f, %s", input$events_peak_variable, input$events_peakwindow_width, input$events_peakwindow_n ))

        } else if (input$events_type == "list") {
            updateTextInput(session, "events_name", placeholder = sprintf("list of %d events", nrow(excel_to_R(input$events_fixed_list))))
        }
    }
})


# toggle event type options ---------------------------------------------------------------------------------------


observeEvent(input$events_type, {
    #browser()
    toggleEventTypeOptions()
})

toggleEventTypeOptions = function() {
    req(input$events_type)
    if (input$events_type %in% c("binary", "binaryinv")) {
        shinyjs::show("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "binned") {
        shinyjs::hide("events_binary_")
        shinyjs::show("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "scorepeak") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::show("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "peak") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::show("events_peakwindow_")
        shinyjs::hide("events_list_")


    } else if (input$events_type == "list") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::show("events_list_")


    }
}
