cba.temp = reactiveValues()

evaluate_events = function() {
    events = NULL # list()
    if (length(input$events_dataset2) > 0) {
        f = input$events_dataset2

        # default naming schemes
        name.str = ""
        if (input$events_type == "binary") {
            name.str = sprintf("binary, %s", input$events_binary_variable)

        } else if (input$events_type == "binaryinv") {
            name.str = sprintf("binary (inverted), %s", input$events_binary_variable)

        } else if (input$events_type == "binned") {
            name.str = sprintf("binned every %0.3f sec, t0=%0.3f", input$events_binned_length, input$events_binned_start)

        } else if (input$events_type == "peak") {
            name.str = sprintf("peak, win=%0.3fs n.sd=%0.3f, %s", input$events_peak_variable, input$events_peakwindow_width, input$events_peakwindow_n )

        } else if (input$events_type == "list") {
            name.str = sprintf("list of %d events", nrow(excel_to_R(input$events_fixed_list)))

        } else if (input$events_type == "cba") {

            name.str = "%start% to %end%, %type%"
        }

        new.name = ifelse(nchar(input$events_name) > 0, input$events_name, name.str)


        if (input$events_type == "binary") {
            events = identify.binary.events(data$raw[[f]], data$meta[[f]]$time, input$events_binary_variable)
            events$type = new.name

        } else if (input$events_type == "binaryinv") {
            events = identify.binary.events(data$raw[[f]], data$meta[[f]]$time, input$events_binary_variable, inverted = T)
            events$type = new.name

        } else if (input$events_type == "peak") {
            events = identify.peak.events(data$raw[[f]], data$meta[[f]]$time, input$events_peakwindow_variable, input$events_peakwindow_n, input$events_peakwindow_width)
            events$type = new.name

        } else if (input$events_type == "binned") {
            events = identify.bin.events(data$raw[[f]], data$meta[[f]]$time, input$events_binned_length, input$events_binned_start)
            events$type = new.name

        } else if (input$events_type == "list") {
            events = identify.fixed.events(data$raw[[f]], data$meta[[f]]$time, getFixedEventTimes())
            events$type = new.name

        } else if (input$events_type == "cba") {
            wins = as.data.table(excel_to_R(input$events_list_cba_windows))

            events = data.table()

            if (nrow(wins) == 0) {
                # one big window
                for (etype in input$events_list_cba_types) {
                    event.times = cba.temp$event.data[type == etype,]

                    window.name = new.name
                    window.name = str_replace_all(window.name, "\\%start\\%", "t=0")
                    window.name = str_replace_all(window.name, "\\%end\\%", sprintf("t=%0.3f", data$raw[[f]][nrow(data$raw[[f]]), get(data$meta[[f]]$time)]))
                    window.name = str_replace_all(window.name, "\\%type\\%", etype)

                    events = rbindlist(list(events, data.table(identify.fixed.events(data$raw[[f]], data$meta[[f]]$time, event.times[, .(start, end)], type = window.name))))
                }

            } else {
                # multiple windows
                for (i in 1:nrow(wins)) {
                    browser()
                    if (wins[i, Start] != "" & wins[i, End] != "") {
                        win.start.event = wins[i, Start]
                        win.end.event = wins[i, End]

                        if (!is.na(as.numeric(wins[i, t0])) & !is.na(as.numeric(wins[i, tf]))) {
                            win.start.time = cba.temp$event.data[type == win.start.event, min(start)] + as.numeric(wins[i, t0])
                            win.end.time = cba.temp$event.data[type == win.start.event, min(start)] + as.numeric(wins[i, tf])
                        } else {
                            win.start.time = cba.temp$event.data[type == win.start.event, min(start)]
                            win.end.time = cba.temp$event.data[type == win.end.event, max(end)]
                        }
                        for (etype in input$events_list_cba_types) {
                            event.times = cba.temp$event.data[type == etype & (win.start.time < start) & (end < win.end.time),]

                            window.name = new.name
                            window.name = str_replace_all(window.name, "\\%start\\%", sprintf("%s", win.start.event))#, win.start.time))
                            window.name = str_replace_all(window.name, "\\%end\\%", sprintf("%s", win.end.event))#, win.start.time))
                            window.name = str_replace_all(window.name, "\\%type\\%", etype)

                            events = rbindlist(list(events, data.table(identify.fixed.events(data$raw[[f]], data$meta[[f]]$time, event.times[, .(start, end)], type = window.name))))
                        }
                    }
                }
            }
        }
    }

    #print(events)

    events
}

condition_events = function(events) {
    # TODO: make the necessary arguments types to condition.events() less convoluted
    # (i.e. the first argument, data$raw[f] needs to be a list of one data.table indexed by filename, but events is already indexed by that filename
    f = input$events_dataset2
    if (nchar(input$events_conditions) > 0 & testConditionValid()) {
        condition.events(data$raw[f],
            events,
            setNames(as.list(data$meta[[f]]$time), f),
            input$events_conditions,
            if.all = input$events_conditions_obs == "all")[[f]]
    } else {
        events
    }
}

filter_events = function(events) {
    # TODO:
    filter.list = as.data.table(excel_to_R(input$events_filters))
    if (nrow(filter.list) > 0) {
            filter.list = filter.list[!(rule == "") | !(value == "" & rule != "aggregate if overlapping"),]

            # replace filters with argument names
            filter.list[rule == "shift all events by X (sec)", rule := "fixed.offset"]
            filter.list[rule == "drop first N events", rule := "drop.first"]
            filter.list[rule == "drop last N events", rule := "drop.last"]
            filter.list[rule == "keep first N events", rule := "keep.first"]
            filter.list[rule == "keep last N events", rule := "keep.last"]
            filter.list[rule == "pad to minimum X (sec)", rule := "pad.minimum"]
            filter.list[rule == "limit at maximum X (sec)", rule := "limit.maximum"]
            filter.list[rule == "drop if shorter than X (sec)", rule := "drop.shorter.than"]
            filter.list[rule == "drop if longer than X (sec)", rule := "drop.longer.than"]
            filter.list[rule == "aggregate if within X (sec)", rule := "aggregate.within"]
            filter.list[rule == "aggregate if overlapping", rule := "aggregate.overlap"]
            filter.list[rule == "keep if N within X (sec)", rule := "keep.n.within.x"]
            filter.list[rule == "include events before X (sec)", rule := "include.before"]
            filter.list[rule == "include events after X (sec)", rule := "include.after"]

            #filters = split(as.numeric(filter.list$value), filter.list$rule)
            #filters = split(filter.list$value, filter.list$rule)

            return(do.call(filter.events, list(.events = events, .filters = filter.list)))
    } else {
        return(events)
    }
}
# update event preview --------------------------------------------------------------------------------------------


# refresh when changing anything
observeEvent(c(input$events_dataset2, input$events_variables, input$events_type, input$events_binary_variable, input$events_binned_start, input$events_binned_length, input$events_peakwindow_variable,
    input$events_peakwindow_width, input$events_peakwindow_n, input$events_fixed_list, input$events_conditions, input$events_conditions_obs, input$events_filters, #input$events_intervals,
    input$events_norm, input$events_norm_baseline, input$events_norm_function,
    input$events_list_cba_types), {

    req(input$events_dataset2, input$events_type, input$events_variables[[1]])


    if (input$events_type %in% c("binary", "binaryinv")) {
        req(input$events_binary_variable)
    } else if (input$events_type == "binned") {
        req(input$events_binned_start, input$events_binned_length)
    } else if (input$events_type == "peak") {
        req(input$events_peakwindow_variable, input$events_peakwindow_width, input$events_peakwindow_n)
    } else if (input$events_type == "list") {
        #req(input$events_fixed_list)
    } else if (input$events_type == "cba") {
        req(cba.temp$event.data)
    }

    .state = ""
    tryCatch({
        withProgress({

            incProgress(1/3, "identifying...")
            events = evaluate_events()
            .state = paste0(.state, sprintf(" - found <b>%d</b> event(s)", nrow(events)), collapse = "")

            output$events_desc = renderUI(HTML(.state))

            incProgress(1/3, "conditioning...")
            conditioned = condition_events(events)

            if (testConditionValid()) {
                .state = paste0(.state, sprintf("\n - <b>%d</b> met condition(s)", nrow(conditioned)), collapse = "")
                output$events_desc = renderUI(HTML(.state))
            }

            incProgress(1/3, "filtering...")
            filtered = filter_events(conditioned)
            filtered[, n := .I]
            #View(filtered)

            if (testValidFilters()) {
                .state = paste0(.state, sprintf("\n - <b>%d</b> remain after filtering", nrow(filtered)), collapse = "")
                output$events_desc = renderUI(HTML(.state))
            }

            if (nrow(filtered) > 0) {
                # update preview plot

                subi = 1:nrow(data$raw[[input$events_dataset2]])

                if (length(subi) > 10000) {
                    subi = seq(1, length(subi), floor(length(subi) / 10000))
                }

                output$events_add_preview = renderPlotly({
                    ggplotly({
                        min.max = data$raw[[input$events_dataset2]][, .(.min=min(get(input$events_variables[[1]])), .max=max(get(input$events_variables[[1]])))]

                        plot.data = data$raw[[input$events_dataset2]][subi, ][, .(x = get(data$meta[[input$events_dataset2]]$time), y = get(input$events_variables[[1]]))]

                        #browser()

                        .g = ggplot(plot.data, aes(x = x, y = y))

                        if ("type" %in% colnames(filtered)) {
                            .g = .g + geom_rect(aes(xmin = start, xmax = end, ymin = min.max$.min, ymax = min.max$.max, fill = type), data = filtered, alpha = 0.5, inherit.aes = F)
                        } else {
                            .g = .g + geom_rect(aes(xmin = start, xmax = end, ymin = min.max$.min, ymax = min.max$.max, fill = factor(n)), data = filtered, alpha = 0.5, inherit.aes = F)
                        }

                        .g = .g +
                            scale_fill_manual(values = rep(viridis_pal(option = "turbo")(10), length.out = nrow(filtered))) +
                            geom_path(size = 0.2) +
                            theme_minimal(9) +
                            theme(axis.text.x = element_blank(), panel.spacing.x = unit(0, "pt"), panel.spacing.y = unit(18, "pt"), legend.position = "none") +
                            labs(x = NULL, y = NULL)
                    }, tooltip = "text") %>%
                        config() %>%
                        layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))

                })
            } else {
                output$events_add_preview = plotlyMessage("No events to visualize.")
            }

        }, value = 0, message = "processing events...")
    }, error = function(e) {
        output$events_desc = renderUI(HTML(toString(e)))
        #print(e)
    }, warning = function(e) {
        output$events_desc = renderUI(HTML(toString(e)))
        #print(e)
    }, finally = function() {

    })

})


# ui def ----------------------------------------------------------------------------------------------------------



ui.tags = tagList(
    layout_columns(col_widths = c(5, 7, 5),
        pickerInput("events_dataset2", "Dataset", choices = c(), multiple = F, width = "100%"),
        textInput("events_name", "Name", "", width = "100%", placeholder = "New Event Series"),
        virtualSelectInput("events_tags", "Tags", choices = c(), showValueAsTags = T, search = T, allowNewOption = T, width = "100%", multiple = T)
    ),

    fluidRow(
        column(5, pickerInput("events_type", tags$b("Type"), width = "100%",
                               choices = c("binary"="binary",
                                           "binary (inverted)"="binaryinv",
                                           "binned"="binned",
                                           "peak (moving window)"="peak",
                                           "timestamps"="list",
                                           "caltech behavior annontation"="cba")),
            div(id = "events_binary_",
                pickerInput("events_binary_variable", tags$b("Binary Variable"), width = "100%",
                            choices = c("var1", "var2", "var3"))
            ),
            hidden(div(id = "events_binned_",
                numericInput("events_binned_start", tags$b("Starting Time (s)"), value = 0, min = -Inf, max = Inf, step = 0.001),
                numericInput("events_binned_length", tags$b("Bin Length (s)"), value = 360, min = 0, max = Inf, step = 0.001)
            )),
            # div(id = "events_scorepeak",
            #     pickerInput("events_scorepeak_type", "Function", choices = c("Type 1", "Type 2", "Type 3")),
            #     numericInput("events_scorepeak_maxima_w", "Maxima Window Size (odd)", value = 3, min = 3, max = Inf, step = 2),
            #     numericInput("events_scorepeak_peak_w", "Peak Function Window Size (odd)", value = 3, min = 3, max = Inf, step = 2)
            # ),
            hidden(div(id = "events_peakwindow_",
                pickerInput("events_peakwindow_variable", tags$b("Peak Variable"), choices = c()),
                fluidRow(
                    column(6, numericInput("events_peakwindow_width", tags$b("Window (sec)"), value = 5, min = 0, max = Inf, step = 0.001)),
                    column(6, numericInput("events_peakwindow_n", tags$b("Limit (#SD)"), value = 6, min = 0, max = Inf, step = 0.001))
                )
            )),
            hidden(div(id = "events_list_",
                excelOutput("events_fixed_list", height = "auto")
            )),
            hidden(div(id = "events_list_cba_", #style = "overflow-y: scroll; max-height: 20rem; min-height: 20rem;",
                fluidRow(
                    column(6, numericInput("events_list_cba_framerate", label = "Framerate", value = 25, min = 1)),
                    column(6, numericInput("events_list_cba_start", label = "Initial Timestamp", value = 1 / 25))
                ),

                tags$label("Event file (*.txt)"), tags$br(), actionButton("events_list_cba_browse", label = "Browse..."),
                tags$br(),
                tags$label("Windows (optional)"), tags$br(), excelOutput("events_list_cba_windows", height = "auto"), tags$br(),
                virtualSelectInput("events_list_cba_types", "Types", c("one", "two", "other"), optionHeight="22px", multiple = T, allowNewOption = F, hideClearButton = F, disableSelectAll = F),
                #tags$label("Event Listing"), tags$br(),
                #excelOutput("events_list_cba_list", height = "auto")
            ))
         ),
         #column(3,# style="border-left: 1px solid #e0e0e0; border-right: 1px solid #e0e0e0;",
                #actionButton("events_conditions_test", "Check Syntax", class="btn btn-warning action-button btn-sm", style="float:right;")
         #),
         column(7,
            tags$label("Filters"),
            excelOutput("events_filters", height = "auto"),
            fluidRow(
                column(8,textAreaInput("events_conditions", "Logical conditions (R expression)", width = "100%", rows = 1)),
                column(4,pickerInput("events_conditions_obs", "Logical criteria", width = "100%", choices = c("any observation meets criteria"="any", "all observations meet criteria"="all")))
            )
         )
    ),

    #tags$hr(),

    fluidRow(
        column(5,
            pickerInput("events_variables", tags$b("Response Variables"), width = "100%", multiple = F, choices = c("Abc", "Def")),
            tags$pre(style="font-size: 0.6rem;", htmlOutput("events_desc"))
        ),
        column(7,
            fluidRow(
                column(4, pickerInput("events_norm", "Normalization", choices = c("z-score"="z", "percent departure"="%", "robust-z"="r", "none"))),
                column(4, pickerInput("events_norm_baseline", "Reference Interval", choices = unique(default$intervals$name), selected = "baseline")),
                column(4, pickerInput("events_norm_function", "Summary Function", choices = c("mean", "median", "max", "min")))
            ),

            tags$b(tags$label("Intervals")),
            excelOutput("events_intervals", height = "auto")
        )
    ),

    fluidRow(
        column(12, plotlyOutput("events_add_preview", height = "200px"))
    )
)

observeEvent(input$events_new_finish, {
    withProgress({

        incProgress(1/4, detail = "identifying events")
        events = evaluate_events()

        conditioned = condition_events(events)

        filtered = filter_events(conditioned)

        #browser()

        for (dstype in unique(filtered$type)) {
            type.events = filtered[type == dstype, ]

            type.name = dstype

            data$series[[input$events_dataset2]][[type.name]] = list()

            data$series[[input$events_dataset2]][[type.name]]$tags = input$events_tags

            if (input$events_type == "binary") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "binary", variable = input$events_binary_variable)
            else if (input$events_type == "binaryinv") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "binaryinv", variable = input$events_binary_variable)
            else if (input$events_type == "binned") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "binned", start = input$events_binned_start, length = input$events_binned_length)
            else if (input$events_type == "peak") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "peakwindow", width = input$events_peakwindow_width, n = input$events_peakwindow_n, variable = input$events_peakwindow_variable)
            else if (input$events_type == "list") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "list", events = as.data.table(excel_to_R(input$events_fixed_list)))
            else if (input$events_type == "cba") data$series[[input$events_dataset2]][[type.name]]$signal = list(type = "cba", events = cba.temp$event.data, windows = as.data.table(excel_to_R(input$events_list_cba_windows)))

            data$series[[input$events_dataset2]][[type.name]]$conditions = input$events_conditions
            data$series[[input$events_dataset2]][[type.name]]$filters = as.data.table(excel_to_R(input$events_filters))
            data$series[[input$events_dataset2]][[type.name]]$responses = input$events_variables

            data$series[[input$events_dataset2]][[type.name]]$normalization = list()
            if (input$events_norm != "none") {
                data$series[[input$events_dataset2]][[type.name]]$normalization$type = input$events_norm
                data$series[[input$events_dataset2]][[type.name]]$normalization$reference = input$events_norm_baseline
                data$series[[input$events_dataset2]][[type.name]]$normalization$summary = input$events_norm_function
            } else {
                data$series[[input$events_dataset2]][[type.name]]$normalization$type = "none"
            }

            int = as.data.table(excel_to_R(input$events_intervals))
            if (nrow(int) == 0) int = copy(as.data.table(default$intervals)) # pick default if empty

            int$start = as.numeric(int$start)
            int$end = as.numeric(int$end)

            data$series[[input$events_dataset2]][[type.name]]$intervals = int

            if (!is.list(data$events[[input$events_dataset2]])) data$events[[input$events_dataset2]] = list()


            # TODO: simplify this as it's a little kludged together
            tryCatch({
                    incProgress(1/4, detail = sprintf("collecting event datasets - %s", type.name))
                    data$events[[input$events_dataset2]][[type.name]] = extract_event_datasets(type.events, int, type.name)[[input$events_dataset2]][[type.name]]
            }, error = function(e) {
                print(e)
            })
        }


    }, value = 0, message = sprintf("%s", input$events_dataset2), detail = "creating new series...")


    updatePickerInput(session, "heatmap_dataset", choices = names(data$series))
    updatePickerInput(session, "heatmap2_dataset", choices = names(data$series))
    updatePickerInput(session, "lag_dataset", choices = names(data$series))
    updatePickerInput(session, "power_dataset", choices = names(data$series))
    updatePickerInput(session, "summary_dataset", choices = names(data$series))

    updatePickerInput(session, "events_series", choices = names(data$series[[input$events_dataset2]]))
    # updatePickerInput(session, "power_series", choices = names(data$series[[input$events_dataset2]]))
    # updatePickerInput(session, "summary_series", choices = names(data$series[[input$events_dataset2]]))
    #removeModal()

    #View(data$series)
    #View(data$events)
})

output$events_list_cba_windows = renderExcel(
    excelTable(
        data = data.table(start = character(1), end = character(1), t0 = NA, tf = NA),
        columns = data.frame(title = c("Start", "End", "t0", "tf"), type = c("dropdown", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(c("one", "two"), c("one", "two"), NA, NA))),
        allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = F
    )
)
output$events_list_cba_list = renderExcel(
    excelTable(
        data = data.table(start = numeric(1), end = numeric(1), type = character(1)),
        columns = data.frame(title = c("Start (s)", "End (s)", "Type"), type = c("number", "number", "text"), width = c(600, 600, 200), source = I(list(NA, NA, NA))),
        allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = F
    )
)

output$events_intervals = renderExcel(excelTable(default$intervals, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("text", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(NA, c("before event", "event signal", "after event", "within event", "before event, non-overlapping"), NA, NA))),
))

output$events_fixed_list = renderExcel(excelTable(default$fixed_list, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("number", "number"), title = c("start", "end"))))

output$events_filters = renderExcel(excelTable(default$filters, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("dropdown", "number"), width = c(400, 100), source = I(list(c("drop first N events", "drop last N events", "keep first N events", "keep last N events", "shift all events by X (sec)", "pad to minimum X (sec)", "limit at maximum X (sec)", "drop if shorter than X (sec)", "drop if longer than X (sec)", "aggregate if within X (sec)", "aggregate if overlapping", "keep if N within X (sec)", "include events before X (sec)", "include events after X (sec)", "drop.within"), NA)), title = c("rule", "value"))))

observeEvent(input$events_dataset2, {
    common.vars = getCommonDatasetVariables(input$events_dataset2)
    updatePickerInput(session, "events_binary_variable", choices = c(common.vars), selected = input$events_binary_variable)
    updatePickerInput(session, "events_peakwindow_variable", choices = c(common.vars), selected = input$events_peakwindow_variable)
    updatePickerInput(session, "events_variables", choices = c(common.vars), selected = input$events_variables)

    tag.choices = unique(unlist(lapply(data$meta, \(d) return(d$tags))))
    tags.selected = ifelse(is.null(data$meta[[input$events_dataset2]]$tags), character(0), data$meta[[input$events_dataset2]]$tags)
    updateVirtualSelect("events_tags", choices = tag.choices, selected = tags.selected)
})

observeEvent(input$events_new, {
    showModal(
        modalDialog(title = "New Event Series", size = "l", footer = tagList(modalButton("Close"), actionButton("events_new_finish", "Create")), ui.tags)
    )

    # tags

    updatePickerInput(session, "events_dataset2", choices = names(data$meta), selected = input$events_dataset)

    toggleEventTypeOptions()
    updatePickerInput(session, "events_type", selected = "binary")
    if (input$events_type %in% c("binary", "binaryinv")) updatePickerInput(session, "events_binary_variable")
    else if (input$events_type == "binned") {
        updateNumericInput(session, "events_binned_start", value = 0)
        updateNumericInput(session, "events_binned_length", value = 360)
    } else if (input$events_type == "peak") {
        updatePickerInput(session, "events_peakwindow_variable", selected = character())
        updateNumericInput(session, "events_peakwindow_width", value = 15)
        updateNumericInput(session, "events_peakwindow_n", value = 6)
    } else if (input$events_type == "list") {
        output$events_fixed_list = renderExcel(excelTable(data = default$fixed_list))
    }
    updateTextInput(session, "events_conditions", value = "")
    output$events_filters = renderExcel(excelTable(data = default$filter_list))
    updatePickerInput(session, "events_variables", selected = character())
    updatePickerInput(session, "events_norm", selected = "z")
    #if (input$events_norm != "none") {
    updatePickerInput(session, "events_norm_baseline", selected = "baseline")
    updatePickerInput(session, "events_norm_function", selected = "mean")
    #}

    # re-populate default table values
    output$events_intervals = renderExcel(excelTable(default$intervals, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("text", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(NA, c("before event", "event signal", "after event", "within event", "before event, non-overlapping"), NA, NA))),
    ))

    output$events_fixed_list = renderExcel(excelTable(default$fixed_list, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("number", "number"), title = c("start", "end"))))

    output$events_filters = renderExcel(excelTable(default$filters, allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T, columns = data.frame(type = c("dropdown", "number"), width = c(400, 100), source = I(list(c("drop first N events", "drop last N events", "keep first N events", "keep last N events", "shift all events by X (sec)", "pad to minimum X (sec)", "limit at maximum X (sec)", "drop if shorter than X (sec)", "drop if longer than X (sec)", "aggregate if within X (sec)", "aggregate if overlapping", "keep if N within X (sec)", "include events before X (sec)", "include events after X (sec)"), NA)), title = c("rule", "value"))))
})

observeEvent(input$events_edit, {

})

observeEvent(input$events_delete, {
    req(input$events_dataset, input$events_series)

    data$series[[input$events_dataset]][[input$events_series]] = NULL
    data$events[[input$events_dataset]][[input$events_series]] = NULL

    updatePickerInput(session, "events_series", choices = names(data$series[[input$events_dataset]]))
    updatePickerInput(session, "power_series", choices = names(data$series[[input$events_dataset]]))
    updatePickerInput(session, "summary_series", choices = names(data$series[[input$events_dataset]]))
})



observeEvent(input$events_list_cba_browse, {
    f = rstudioapi::selectFile(
        caption = "Select behavior event file (*.txt)",
        label = "Select",
        path = rstudioapi::getActiveProject(),
        filter = "TXT files (*.txt)",
        existing = T
    )

    if (!is.null(f)) {
        # parse caltech behavior annotation file to find the appropriate line numbers
        first.line = read_lines(f, skip = 0, skip_empty_rows = F, n_max = 1)
        assert_that(first.line == "Caltech Behavior Annotator - Annotation File")

        # find size of list of events
        config.header = read_lines(f, skip = 2, skip_empty_rows = F, n_max = 1)
        assert_that(config.header == "Configuration file:")
        config.lines = c()
        for (i in 1:100) {
            this.line = read_lines(f, skip = 3 + i - 1, skip_empty_rows = F, n_max = 1)
            if (this.line == "") { break }
        }
        config.data = as.data.table(read_delim(f, delim = " ", skip = 3, col_names = c("type", "symbol"), col_types = "c",  n_max = i-1, skip_empty_rows = F))
        config.data = config.data

        # read event data
        table.header = read_lines(f, skip = 3 + i, skip_empty_rows = F, n_max = 1)
        assert_that(table.header == "S1: start    end     type")
        event.data =  as.data.table(read_table(f, skip = 3 + i + 2, col_names = c("start", "end", "type"), col_types = "nnc"))[, `:=`(start = start / input$events_list_cba_framerate + input$events_list_cba_start, end = end / input$events_list_cba_framerate + input$events_list_cba_start)]
        #View(event.data)

        cba.temp$event.data = event.data


        type.vals = unique(event.data$type)

        output$events_list_cba_windows = renderExcel(
            excelTable(
                data = data.table(start = character(1), end = character(1), t0 = NA, tf = NA),
                columns = data.frame(title = c("Start", "End", "t0", "tf"), type = c("dropdown", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(type.vals, type.vals, NA, NA))),
                allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T
            )
        )

        updateVirtualSelect("events_list_cba_types", choices = type.vals, selected = type.vals)

        showNotification(sprintf("Loaded %d events from %s", nrow(event.data), f), type = "message")
    }
})
