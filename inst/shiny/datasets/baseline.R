
observeEvent(input$data_baseline_model, {
    if (input$data_baseline_model == "exp") {
        shinyjs::hide("data_baseline_linear")
        shinyjs::show("data_baseline_exp")
    } else if (input$data_baseline_model == "lin") {
        shinyjs::show("data_baseline_linear")
        shinyjs::hide("data_baseline_exp")
    }
})

observeEvent(input$data_baseline_params, {
    .inputs = c("data_baseline_lin_slope", "data_baseline_lin_intercept", "data_baseline_exp_alpha", "data_baseline_exp_beta", "data_baseline_exp_theta")
    if (input$data_baseline_params == "least squares") {
        lapply(.inputs, disable)
    } else {
        lapply(.inputs, enable)
    }
})

runFits = function() {
    plot.data = data.table()
    models = list()

    withProgress({
        for (v in input$data_baseline_var) {

            incProgress(amount = 1, detail = v)

            .X = data$raw[[input$data_dataset]][, get(data$meta[[input$data_dataset]]$time)]
            .Y = data$raw[[input$data_dataset]][, get(v)]

            Yhat = NULL
            model = NULL

            if (input$data_baseline_model == "exp") {
                #browser()
                models[[v]] = list(converged = F)

                try({

                model = nlsr(Y ~ alpha * exp(beta * X) + theta,
                    start = c(alpha = 100.0, beta = -0.01, theta = 1000.0),
                    data = data.frame(X = .X, Y = .Y),
                    control = nlsr.control(list(femax = 1000, jemax = 500))
                )

                    models[[v]]$converged = model$convInfo$isConv

                if (models[[v]]$converged) {
                    Yhat = predict(model, list(X = .X))

                    #print(AIC(model))
                    #print(summary(model))
                    print(coefficients(model))
                    #View(model)
                    #browser()

                    updateNumericInput(session, "data_baseline_exp_alpha", value = coefficients(model)[["alpha"]])
                    updateNumericInput(session, "data_baseline_exp_beta", value = coefficients(model)[["beta"]])
                    updateNumericInput(session, "data_baseline_exp_theta", value = coefficients(model)[["theta"]])

                    models[[v]]$type = "exponential"
                    models[[v]]$a = coefficients(model)[["alpha"]]
                    models[[v]]$b = coefficients(model)[["beta"]]
                    models[[v]]$c = coefficients(model)[["theta"]]
                }

                })

            } else if (input$data_baseline_model == "biexp") {
                # bi-exponential (sum of two)

                models[[v]] = list(converged = F)

                try({
                    model = nlsr(Y ~ A1 * exp(B1 * X) + A2 * exp(B2 * X),
                        start = c(A1 = 1.0, B1 = -1.0, A2 = -1.0, B2 = -1.0),
                        data = data.frame(X = .X, Y = .Y),
                        control = nlsr.control(list(femax = 1000, jemax = 500))
                    )
                    print(summary(model))

                    models[[v]]$converged = model$convInfo$isConv

                    if (models[[v]]$converged) {
                        Yhat = predict(model, list(X = .X))
                        print(coefficients(model))

                        models[[v]]$type = "biexp"
                        models[[v]]$A1= coefficients(model)[["A1"]]
                        models[[v]]$B1 = coefficients(model)[["B1"]]
                        models[[v]]$A2= coefficients(model)[["A2"]]
                        models[[v]]$B2 = coefficients(model)[["B2"]]
                    }
                })

            } else if (input$data_baseline_model == "biexp-c") {

                models[[v]] = list(converged = F)

                try({
                    model = nlsr(Y ~ A1 * exp(B1 * X) + A2 * exp(B2 * X) + C,
                        start = c(A1 = 1.0, B1 = -1.0, A2 = -1.0, B2 = -1.0, C = 1000),
                        data = data.frame(X = .X, Y = .Y),
                        control = nlsr.control(list(femax = 1000, jemax = 500))
                    )
                    print(summary(model))

                    models[[v]]$converged = model$convInfo$isConv

                    if (models[[v]]$converged) {
                        Yhat = predict(model, list(X = .X))
                        print(coefficients(model))

                        models[[v]]$type = "biexp-c"
                        models[[v]]$A1= coefficients(model)[["A1"]]
                        models[[v]]$B1 = coefficients(model)[["B1"]]
                        models[[v]]$A2= coefficients(model)[["A2"]]
                        models[[v]]$B2 = coefficients(model)[["B2"]]
                        models[[v]]$C = coefficients(model)[["C"]]
                    }
                })


            } else if (input$data_baseline_model == "lin") {
                models[[v]] = list(converged = T) # assume

                model = lm(Y ~ X, data = data.frame(X = .X, Y = .Y), na.action = "na.omit")
                Yhat = predict(model, list(X = .X))

                print(summary(model))

                updateNumericInput(session, "data_baseline_lin_slope", value = coefficients(model)[[2]])
                updateNumericInput(session, "data_baseline_lin_intercept", value = coefficients(model)[[1]])

                models[[v]]$type = "linear"
                models[[v]]$m = coefficients(model)[[2]]
                models[[v]]$b = coefficients(model)[[1]]
            }

            if (!is.null(Yhat)) {
                plot.data = rbindlist(list(plot.data, data.table(x = .X, y = .Y, yhat = Yhat, ydiff = .Y - Yhat, var = v, type = models[[v]]$type)))
            }

        }
    }, message = "Fitting models...", min = 0, max = length(input$data_baseline_var))

    list(data = plot.data, models = models)
}

observeEvent(c(input$data_baseline_var_open, input$data_baseline_var, input$data_baseline_model, input$data_baseline_params, input$data_baseline_linear_slope, input$data_baseline_linear_intercept, input$data_baseline_exp_alpha, input$data_baseline_exp_beta, input$data_baseline_exp_theta), {
    # re-fit model and update plot on any changes

    req(input$data_baseline_var_open == F)
    req(input$data_dataset, input$data_baseline_var)
    req(input$changed != "plotly_afterplot-A")

    plot.data = runFits()

    output$data_baseline_diag = renderUI({
        string = c("")

        nlen = max(str_length(names(plot.data$models)))

        for (n in names(plot.data$models)) {
            if (plot.data$models[[n]]$converged) {
                lls=list()
                for (nn in names(plot.data$models[[n]])) {
                    if (!(nn %in% c("converged", "type"))) {
                        lls = append(lls, sprintf("<span style='color: #b0b0b0; display: inline;'>%3s=</span>%9.3g", nn, plot.data$models[[n]][nn]))
                    }
                }
                string = append(string, sprintf("<span style='color: #20a020; text-align: left; white-space: pre;'><b>%s</b></span><span style='white-space: pre;'>%s</span><br/>", str_pad(n, nlen), paste0(lls, collapse = "  ")))
            } else {
                string = append(string, sprintf("<span style='color: #c02020; text-align: left;'><span style='white-space: pre;'><b>%s</b></span> failed to converge.</span><br/>", str_pad(n, nlen)))
            }
        }

        HTML(paste0(string, collapse = ""))
    })

    req(!is.null(plot.data))


    subi = 1:length(plot.data$data$x)
    if (length(plot.data$data$x) > 10000) {
        subi = seq(1, length(plot.data$data$x), floor(length(plot.data$data$x) / 10000))
    }
    plot.data$data = plot.data$data[subi, ]

    # generate plot
    output$data_baseline_plot = renderPlotly({
        .gg = ggplotly({
            ggplot(mapping = aes(x = x, y = y)) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$ydiff, var = plot.data$data$var, type="Model Residuals"), size = 0.2) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$y, var = plot.data$data$var, type="Fitted Model"), size = 0.2) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$yhat, var = plot.data$data$var, type="Fitted Model"), size = 0.3, color = "red") +
                facet_wrap2(~ var + type, ncol = 2, scales = "free_y") +
                theme_minimal() + theme(axis.text.x = element_blank(), panel.spacing.x = unit(0, "pt"), panel.spacing.y = unit(12, "pt")) + labs(x = NULL, y = NULL)

        }) %>% config() %>%
            layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto")) %>%
            toWebGL2()

        .gg
    })
})

observeEvent(input$data_baseline_finish, {
    req(input$data_dataset, input$data_baseline_var)

    fitted.data = runFits()

    req(fitted.data$data)

    for (.v in unique(fitted.data$data$v)) {
        .m = fitted.data$models[[.v]]

        if ("no_offset" %in% input$data_baseline_options) {
            if (.m$type == "exponential") {
                offset = .m$c
            } else if (.m$type == "linear") {
                offset = .m$b
            } else if (.m$type == "biexp-c") {
                offset = .m$C
            }
            if (.m$type %in% c("exponential", "linear", "biexp-c")) {
                data$raw[[input$data_dataset]][, (sprintf("%s%s", .v, input$data_baseline_suffix)) := fitted.data$data[var == .v, ydiff] + offset] # add offset back
            } else {
                data$raw[[input$data_dataset]][, (sprintf("%s%s", .v, input$data_baseline_suffix)) := fitted.data$data[var == .v, ydiff]]
            }

        } else {
            data$raw[[input$data_dataset]][, (sprintf("%s%s", .v, input$data_baseline_suffix)) := fitted.data$data[var == .v, ydiff]]
        }

        if ("model" %in% input$data_baseline_options) {
            data$raw[[input$data_dataset]][, (sprintf("%s%s [model]", .v, input$data_baseline_suffix)) := fitted.data$data[var == .v, yhat]]
        }
    }

    for (f in names(fitted.data$models)) {
        data$models[[input$data_dataset]][[f]] = fitted.data$models[[f]]
        #print(fitted.data$models[[f]])
    }

    removeModal()

    updateCurrentVariableSelections()
})

observeEvent(input$data_baseline, {

    variables = names(data$raw[[input$data_dataset]])

    updatePickerInput(session, "data_baseline_var", choices = variables)

    output$data_baseline_plot = plotlyMessage("Select variable(s) to de-trend.")

    showModal(
        modalDialog(title = "De-trend variable(s)", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_baseline_finish", "Finish")),
            fluidRow(
                column(4, pickerInput("data_baseline_var", "Input Variable(s)", c(), width = "100%", multiple = T)),
                column(4, textInput("data_baseline_suffix", "Output Suffix", " (corrected)")),
                column(4, pickerInput("data_baseline_model", "Model", choices = c(
                    "y = Mx + B (linear)"="lin",
                    "y = Ae^Bx + C (exp+c)"="exp",
                    "y = A1e^B1x + A2e^B2x (biexp)"="biexp",
                    "y = A1e^B1x + A2e^B2x + C (biexp+c)"="biexp-c"
                ), width = "100%"))
            ),
            fluidRow(
                column(8,
                    tags$label("Model Diagnostics"), tags$br(),
                    div(style = "line-height: 0.75rem; font-size: 0.75rem; text-align: left; overflow: scroll; text-wrap: nowrap; font-family: monospace; max-height: 10rem; padding: 0.5rem",
                        uiOutput("data_baseline_diag", inline = T)
                    )
                ),
                column(4, style = "line-height: 0.5rem; font-size: 0.5rem;",
                    tags$label("Options"), tags$br(), tags$br(), prettyCheckboxGroup("data_baseline_options", NULL, choices = c("keep constant term in correction"="no_offset", "generate model value column"="model"), selected = c("no_offset")))
            ),
            plotlyOutput("data_baseline_plot", height = "400px") %>% withSpinner()

        )
    )

    shinyjs::hide("data_baseline_linear")
    shinyjs::show("data_baseline_exp")
})





