
observeEvent(input$data_caltech_append, {
    showModal(
        modalDialog(title = "Caltech Annotation Append", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_caltech_append_apply", "Apply")),

            fluidRow(
                column(6, numericInput("data_list_cba_framerate", label = "Framerate", value = 25, min = 1)),
                column(6, numericInput("data_list_cba_start", label = "Initial Timestamp", value = 1 / 25))
            ),

            tags$label("Event file (*.txt)"), tags$br(), actionButton("data_list_cba_browse", label = "Browse..."),
            tags$br(),
            tags$label("Windows (optional)"), tags$br(), excelOutput("data_list_cba_windows", height = "auto"), tags$br(),
            textInput("data_list_cba_column", label = "Output Column", value = "colname"),
            virtualSelectInput("data_list_cba_types", "Types", c("one", "two", "other"), optionHeight="22px", multiple = T, allowNewOption = F, hideClearButton = F, disableSelectAll = F)
        )
    )
})


output$data_list_cba_windows = renderExcel(
    excelTable(
        data = data.table(start = character(1), end = character(1), t0 = NA, tf = NA),
        columns = data.frame(title = c("Start", "End", "t0", "tf"), type = c("dropdown", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(c("one", "two"), c("one", "two"), NA, NA))),
        allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = F
    )
)
output$data_list_cba_list = renderExcel(
    excelTable(
        data = data.table(start = numeric(1), end = numeric(1), type = character(1)),
        columns = data.frame(title = c("Start (s)", "End (s)", "Type"), type = c("number", "number", "text"), width = c(600, 600, 200), source = I(list(NA, NA, NA))),
        allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = F
    )
)

cba.temp = reactiveValues(event.data = data.table())

observeEvent(input$data_list_cba_browse, {
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
        event.data =  as.data.table(read_table(f, skip = 3 + i + 2, col_names = c("start", "end", "type"), col_types = "nnc"))[, `:=`(start = start / input$data_list_cba_framerate + input$data_list_cba_start, end = end / input$data_list_cba_framerate + input$data_list_cba_start)]
        #View(event.data)

        cba.temp$event.data = event.data


        type.vals = unique(event.data$type)

        output$data_list_cba_windows = renderExcel(
            excelTable(
                data = data.table(start = character(1), end = character(1), t0 = NA, tf = NA),
                columns = data.frame(title = c("Start", "End", "t0", "tf"), type = c("dropdown", "dropdown", "number", "number"), width = c(600, 600, 200, 200), source = I(list(type.vals, type.vals, NA, NA))),
                allowInsertColumn = F, allowDeleteColumn = F, allowRenameColumn = F, rowDrag = T
            )
        )

        updateVirtualSelect("data_list_cba_types", choices = type.vals, selected = type.vals)

        showNotification(sprintf("Loaded %d events from %s", nrow(event.data), f), type = "message")
    }
})

observeEvent(input$data_caltech_append_apply, {

    wins = as.data.table(excel_to_R(input$data_list_cba_windows))

    events = data.table()

    new.name = input$data_list_cba_column

    if (nrow(wins) == 0) {
        # one big window
        for (etype in input$data_list_cba_types) {
            event.times = cba.temp$event.data[type == etype,]

            window.name = etype

            events = rbindlist(list(events, data.table(identify.fixed.events(data$raw[[input$data_dataset]], data$meta[[input$data_dataset]]$time, event.times[, .(start, end)], type = window.name))))
        }

    } else {
        # multiple windows
        for (i in 1:nrow(wins)) {
            #browser()
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
                for (etype in input$data_list_cba_types) {
                    event.times = cba.temp$event.data[type == etype & (win.start.time < start) & (end < win.end.time),]

                    window.name = etype

                    events = rbindlist(list(events, data.table(identify.fixed.events(data$raw[[input$data_dataset]], data$meta[[input$data_dataset]]$time, event.times[, .(start, end)], type = window.name))))
                }
            }
        }
    }

    data$raw[[input$data_dataset]][, (new.name) := 0]

    for (e in 1:nrow(events)) {
        data$raw[[input$data_dataset]][ events[e, start] <= get(data$meta[[input$data_dataset]]$time) & get(data$meta[[input$data_dataset]]$time) < events[e, end] , (new.name) := 1]
    }

        updateCurrentVariableSelections()
        removeModal()
})








observeEvent(input$data_custom_finish, {
    tryCatch({
        start = proc.time()

        .dt = copy(data$raw[[input$data_dataset]])
        .env = env({ dt = .dt })
        .exp = parse(text = input$data_custom_code)
        .res = eval(.exp, envir = .env)
        data$raw[[input$data_dataset]] = .env$dt

        updateCurrentVariableSelections()

        end = proc.time()

        removeModal()
    }, error = \(e) {
        output$data_custom_log = renderText(toString(e))
    }, warning = \(e) {
        output$data_custom_log = renderText(toString(e))
    })
})

observeEvent(input$data_custom_test, {
    tryCatch({
        start = proc.time()

        .dt = copy(data$raw[[input$data_dataset]])
        .env = env({ dt = .dt })
        .exp = parse(text = input$data_custom_code)
        .out = capture.output(eval(.exp, envir = .env))

        end = proc.time()

        output$data_custom_log = renderText(sprintf("script successfully executed in %0.3f seconds\n\n%s", (end[[3]] - start[[3]]), paste0(.out, collapse="\n")))

    }, error = \(e) {
        output$data_custom_log = renderText(toString(e))
    }, warning = \(e) {
        output$data_custom_log = renderText(toString(e))
    })
})

observeEvent(input$data_custom, {
    output$data_custom_log = renderText("Errors and warnings will appear here")

    showModal(
        modalDialog(title = "Custom R Script", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_custom_test", "Test"), actionButton("data_custom_finish", "Finish")),
            fluidRow(
                column(12, tags$small("The selected dataset can be referenced as `dt`, which is a data.frame/data.table object. Any changes will replace the original data."), tags$br(),
                       textAreaInput("data_custom_code", NULL, width = "100%", rows = 20))
            ),
            fluidRow(
                column(12, style="overflow-y: scroll; max-height: 250px;", tags$label("Log"), tags$br(), verbatimTextOutput("data_custom_log"))
            )
        )
    )
})




observeEvent(input$data_rescale, {
    showModal(
        modalDialog(title = "linear rescaling", size = "l", fade = F, footer = tagList(modalButton("Close"), actionButton("data_rescale_action", "Rescale")),
            "Apply a linear transformation to a variable to scale it to the magnitude of another.",
            fluidRow(
                column(6, selectInput("data_rescale_input", label = "Variable to scale", multiple = F, width = "100%", choices = c())),
                column(6, selectInput("data_rescale_ref", label = "Reference variable", multiple = F, width = "100%", choices = c()))
            )
        )
    )

    if (input$data_dataset %in% names(data$raw)) {
        variables = names(data$raw[[input$data_dataset]])
        updateSelectInput(session, "data_rescale_input", choices = variables)
        updateSelectInput(session, "data_rescale_ref", choices = variables)
    }
})

observeEvent(input$data_rescale_action, {
    req(input$data_dataset %in% names(data$raw))

    withProgress({
        x.in = data$raw[[input$data_dataset]][, get(input$data_rescale_input)]
        x.ref = data$raw[[input$data_dataset]][, get(input$data_rescale_ref)]

        lin.fit = lm(x.ref ~ x.in)

        intercept = lin.fit$coefficients[[1]]
        slope = lin.fit$coefficients[[2]]

        new.x.in = x.in * slope + intercept

        data$raw[[input$data_dataset]][, (input$data_rescale_input) := new.x.in]
    }, message = "Rescaling...")
})
