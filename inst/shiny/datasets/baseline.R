
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
                if (input$data_baseline_params == "least squares") {
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


                } else {
                    alpha = input$data_baseline_exp_alpha
                    beta = input$data_baseline_exp_beta
                    theta = input$data_baseline_exp_theta

                    # using user-specified parameters instead
                    Yhat = alpha * exp(beta * .X) + theta

                    models[[v]] = list(type = "exponential", a = alpha, b = beta, c = theta)
                }

            } else if (input$data_baseline_model == "lin") {
                if (input$data_baseline_params == "least squares") {
                    models[[v]] = list(converged = T) # assume

                    model = lm(Y ~ X, data = data.frame(X = .X, Y = .Y), na.action = "na.omit")
                    Yhat = predict(model, list(X = .X))

                    print(summary(model))

                    updateNumericInput(session, "data_baseline_lin_slope", value = coefficients(model)[[2]])
                    updateNumericInput(session, "data_baseline_lin_intercept", value = coefficients(model)[[1]])

                    models[[v]]$type = "linear"
                    models[[v]]$m = coefficients(model)[[2]]
                    models[[v]]$b = coefficients(model)[[1]]

                } else {
                    slope = input$data_baseline_lin_slope
                    intercept = input$data_baseline_lin_intercept

                    Yhat = slope * .X + intercept

                    models[[v]] = list(type = "linear", m = slope, b = intercept)
                }
            }

            if (!is.null(Yhat)) {
                plot.data = rbindlist(list(plot.data, data.table(x = .X, y = .Y, yhat = Yhat, ydiff = .Y - Yhat, var = v, type = models[[v]]$type)))
            }

        }
    }, message = "Fitting models...", min = 0, max = length(input$data_baseline_var))

    list(data = plot.data, models = models)
}

observeEvent(c(input$data_baseline_var, input$data_baseline_model, input$data_baseline_params, input$data_baseline_linear_slope, input$data_baseline_linear_intercept, input$data_baseline_exp_alpha, input$data_baseline_exp_beta, input$data_baseline_exp_theta), {
    # re-fit model and update plot on any changes

    req(input$data_dataset, input$data_baseline_var)
    req(input$changed != "plotly_afterplot-A")

    plot.data = NULL

    if (input$changed %in% c("data_baseline_linear_slope", "data_baseline_linear_intercept", "data_baseline_exp_alpha", "data_baseline_exp_beta", "data_baseline_exp_theta")) {
        if (input$data_baseline_params == "least squares") {
            # avoid duplicate re-fits when parameters are updated while least squares is selected

        } else if (input$data_baseline_params == "manual") {
            plot.data = runFits()
        }
    } else {
       plot.data = runFits()
    }

    req(!is.null(plot.data))

    # generate plot
    output$data_baseline_plot = renderPlotly({
        .gg = ggplotly({
            ggplot(mapping = aes(x = x, y = y)) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$ydiff, var = plot.data$data$var, type="Model Residuals"), size = 0.2) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$y, var = plot.data$data$var, type="Fitted Model"), size = 0.2) +
                geom_path(data = data.table(x = plot.data$data$x, y = plot.data$data$yhat, var = plot.data$data$var, type="Fitted Model"), size = 0.3, color = "red") +
                facet_wrap2(~ var + type, ncol = 2, scales = "free_y") +
                theme_minimal() + theme(axis.text.x = element_blank(), panel.spacing.x = unit(0, "pt"), panel.spacing.y = unit(18, "pt")) + labs(x = NULL, y = NULL)

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
            } else {
                offset = .m$b
            }
            data$raw[[input$data_dataset]][, (sprintf("%s%s", .v, input$data_baseline_suffix)) := fitted.data$data[var == .v, ydiff] + offset] # add offset back

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
                column(4, pickerInput("data_baseline_model", "Model", choices = c("exponential, y=a*exp(bx)+c"="exp", "linear, y=mx+b"="lin"), width = "100%"))
            ),
            fluidRow(
                column(2, pickerInput("data_baseline_params", "Estimation", c("auto"="least squares", "manual"), selected = "least squares"),
                ),
                column(6,
                    fluidRow(id = "data_baseline_linear",
                        column(6, disabled(numericInput("data_baseline_lin_slope", "slope (m)", NA, min = -Inf, max = Inf, step = 1e-6, width = "100%"))),
                        column(6, disabled(numericInput("data_baseline_lin_intercept", "intercept (b)", NA, min = -Inf, max = Inf, step = 1e-6, width = "100%")))
                    ),

                    fluidRow(id = "data_baseline_exp",
                        column(4, disabled(numericInput("data_baseline_exp_alpha", HTML("scale (a)"), NA, min = -Inf, max = Inf, step = 1e-6, width = "100%"))),
                        column(4, disabled(numericInput("data_baseline_exp_beta", HTML("exponent (b)"), NA, min = -Inf, max = Inf, step = 1e-6, width = "100%"))),
                        column(4, disabled(numericInput("data_baseline_exp_theta", HTML("offset (c)"), NA, min = -Inf, max = Inf, step = 1e-6, width = "100%")))
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





