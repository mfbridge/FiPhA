
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
