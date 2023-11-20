observeEvent(input$data_norm, {
    showModal(
        modalDialog(title = "Normalize dataset", size = "l", fade = F, footer = tagList(modalButton("Close"), actionButton("data_norm_action", "Apply")),
            virtualSelectInput("data_norm_dataset", "Dataset", choices = names(data$meta), selected = input$data_dataset, multiple = F, width = "100%"),
            tags$hr(),
            fluidRow(
                column(3,
                    div(
                        virtualSelectInput("data_norm_type", "Normalization", choices = c("z score", "robust z", "percent change", "delta f over f", "percent delta f over f"), selected = "z score", multiple = F, width = "100%"),
                        # virtualSelectInput("data_norm_summary", "Summary Function", choices = c("mean", "median"), selected = "mean", multiple = F, width = "100%")
                    )),
                column(2, numericInput("data_norm_min", "Min", value = 0, min = 0, step = 0.001)),
                column(5, sliderInput("data_norm_ref", "Reference Interval (sec)", min = 0, max = 1, value = c(0, 1), step = 0.001, width = "100%"), style = "z-index: 1;"),
                column(2, numericInput("data_norm_max", "Max", value = 1, min = 0, step = 0.001)),
            ),
            fluidRow(
                column(12,
                    virtualSelectInput("data_norm_variables", "Variables", choices = names(data$raw[[input$data_dataset]]), selected = c(), multiple = T, width = "100%")
                )
            )
        )
    )

    req(input$data_dataset %in% names(data$meta))
    updateVirtualSelect("data_norm_dataset", choices = names(data$meta), selected = input$data_dataset)
    t = data$meta[[input$data_dataset]]$time
    limits = data$raw[[input$data_dataset]][, list(min.t = min(get(t), na.rm = T), max.t = max(get(t), na.rm = T))]
    updateSliderInput(session, "data_norm_ref", min = limits$min.t, max = limits$max.t, value = c(limits$min.t, limits$max.t))
    updateNumericInput(session, "data_norm_min", value = limits$min.t)
    updateNumericInput(session, "data_norm_max", value = limits$max.t)
})

observeEvent(input$data_norm_type, {
    if (input$data_norm_type == "percent change") {
        shinyjs::disable("data_norm_min")
        shinyjs::disable("data_norm_max")
        shinyjs::disable("data_norm_ref")
    } else {
        shinyjs::enable("data_norm_min")
        shinyjs::enable("data_norm_max")
        shinyjs::enable("data_norm_ref")
    }
})

observeEvent(input$data_norm_action, {
    req(input$data_norm_dataset %in% names(data$raw))

    withProgress({

        for (v in input$data_norm_variables) {
            new.name = sprintf("%s (%s)", v, input$data_norm_type)
            incProgress(amount = 1, detail = new.name)

            t = data$meta[[input$data_norm_dataset]]$time

            if (input$data_norm_type == "z score") {
                #browser()
                r = data$raw[[input$data_norm_dataset]][get(t) < input$data_norm_max & input$data_norm_min <= get(t), get(v)]
                data$raw[[input$data_norm_dataset]][, (new.name) := zscore(get(v), r = r)]

            } else if (input$data_norm_type == "robust z") {
                r = data$raw[[input$data_norm_dataset]][get(t) < input$data_norm_max & input$data_norm_min <= get(t), get(v)]
                data$raw[[input$data_norm_dataset]][, (new.name) := robustz(get(v), r = r)]

            } else if (input$data_norm_type == "percent change") {
                data$raw[[input$data_norm_dataset]][, (new.name) := pctchg(get(v))]

            } else if (input$data_norm_type == "delta f over f") {
                r = data$raw[[input$data_norm_dataset]][get(t) < input$data_norm_max & input$data_norm_min <= get(t), get(v)]
                data$raw[[input$data_norm_dataset]][, (new.name) := deltaf(get(v), r = r)]

            } else if (input$data_norm_type == "percent delta f over f") {
                r = data$raw[[input$data_norm_dataset]][get(t) < input$data_norm_max & input$data_norm_min <= get(t), get(v)]
                data$raw[[input$data_norm_dataset]][, (new.name) := pctdff(get(v), r = r)]
            }
        }

    }, min = 0, max = length(input$data_norm_variables), message = "Normalizing...")

    updateCurrentVariableSelections()

})

observeEvent(input$data_norm_dataset, {
    req(input$data_norm_dataset %in% names(data$raw))

    updateVirtualSelect("data_norm_variables", choices = names(data$raw[[input$data_norm_dataset]]), selected = input$data_norm_variables)

    t = data$meta[[input$data_norm_dataset]]$time
    limits = data$raw[[input$data_norm_dataset]][, list(min.t = min(get(t), na.rm = T), max.t = max(get(t), na.rm = T))]
    updateNumericInput(session, "data_norm_min", value = limits$min.t)
    updateNumericInput(session, "data_norm_max", value = limits$max.t)
    updateSliderInput(session, "data_norm_ref", min = limits$min.t, max = limits$max.t, value = c(limits$min.t, limits$max.t))
})

observeEvent(input$data_norm_min, {
    if (input$data_norm_ref[[1]] != input$data_norm_min) {
        updateSliderInput(session, "data_norm_ref", value = c(input$data_norm_min, input$data_norm_ref[[2]]))
    }
})

observeEvent(input$data_norm_max, {
    if (input$data_norm_ref[[2]] != input$data_norm_max) {
        updateSliderInput(session, "data_norm_ref", value = c(input$data_norm_ref[[1]], input$data_norm_max))
    }
})

observeEvent(input$data_norm_ref, {
    if (input$data_norm_ref[[1]] != input$data_norm_min) {
        updateNumericInput(session, "data_norm_min", value = input$data_norm_ref[[1]])
    }

    if (input$data_norm_ref[[2]] != input$data_norm_max) {
        updateNumericInput(session, "data_norm_max", value = input$data_norm_ref[[2]])
    }
})
