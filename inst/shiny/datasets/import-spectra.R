# import-spectra.R

spectra.reference = reactiveValues()
spectra.preview = reactiveValues()

shinyFileChoose(input, "data_import_spectrometry_file", root=root.dirs, filetypes=c("txt"))
shinyFileChoose(input, "data_import_spectrometry_reference", root=root.dirs, filetypes=c("csv"))

observeEvent(input$data_spectra_preview_file, {
    if (is.integer(input$data_import_spectrometry_file)) {

    } else {
        withProgress({
            fi = parseFilePaths(root=root.dirs, selection = input$data_import_spectrometry_file)

            preview.file = as.data.table(fi)[name == input$data_spectra_preview_file, datapath]

            # ext= txt
            wavelengths = as.character(read_tsv(preview.file,
               col_names = F,
               n_max = 1,
               na = default$missing_values,
               skip = input$data_spectra_header_row - 1,
               show_col_types = F))

            wl = as.numeric(wavelengths[3:length(wavelengths)])
            # updateNumericInput(session, "data_spectra_minimum", value = min(wl))
            # updateNumericInput(session, "data_spectra_maximum", value = max(wl))

            # only ever previewing the first file
            preview.data = as.data.table(read_tsv(preview.file, col_names = F, n_max = 1, na = default$missing_values, skip = input$data_spectra_data_row, show_col_types = F))

            ys = melt(preview.data, measure.vars = 3:(2+length(wl)))

            spectra.preview$data.series = ys$value
            spectra.preview$wavelengths = wl
        }, message = "creating preview data...")
    }
})

observeEvent(c(input$data_import_spectrometry_file, input$data_spectra_header_row, input$data_spectra_data_row), {
    #
    if (is.integer(input$data_import_spectrometry_file)) {

    } else {
        withProgress({
            fi = parseFilePaths(root=root.dirs, selection = input$data_import_spectrometry_file)
            if (nrow(fi) == 1) {
                output$data_import_spectrometry_filename = renderText(fi$datapath)
            } else {
                output$data_import_spectrometry_filename = renderText(sprintf("%0.0f files selected", nrow(fi)))
            }
            #browser()
            fit = as.data.table(fi)
            updatePickerInput(session, "data_spectra_preview_file", choices = fit[, name], selected = fit[1, name])

        }, message = "parsing header information...")
    }
})

observeEvent(c(input$data_spectra_minimum, input$data_spectra_maximum), {
    updateSliderInput(session, "data_spectra_range1", min = input$data_spectra_minimum, max = input$data_spectra_maximum, value = input$data_spectra_range1)
    updateSliderInput(session, "data_spectra_range2", min = input$data_spectra_minimum, max = input$data_spectra_maximum, value = input$data_spectra_range2)

})

observe({
    req(!is.integer(input$data_import_spectrometry_files))
    req(spectra.preview$wavelengths, spectra.preview$data.series)

    output$data_spectra_preview = renderPlot({
        .data = data.table(x = spectra.preview$wavelengths, y = spectra.preview$data.series)
        #print(.data)

        .ribbon1 = .data[input$data_spectra_range1[[1]] <= x & x <= input$data_spectra_range1[[2]], .(ymin = min(y, 0), ymax = max(y, 0)), by = x]
        .ribbon2 = .data[input$data_spectra_range2[[1]] <= x & x <= input$data_spectra_range2[[2]], .(ymin = min(y, 0), ymax = max(y, 0)), by = x]

        if (nrow(.data) > 0 & nrow(.ribbon1) > 0 & nrow(.ribbon2) > 0) {
            ggplot(.data, aes(x = x, y = y)) +
                theme_minimal(11) +
                #theme_prism(base_size = 11) +
                labs(x = "Wavelength (nm)", y = NULL) +
                coord_cartesian(expand = F, xlim = c(input$data_spectra_minimum, input$data_spectra_maximum)) +
                geom_vline(aes(xintercept = input$data_spectra_range1[[1]]), color = "#77dd77") +
                geom_vline(aes(xintercept = input$data_spectra_range1[[2]]), color = "#77dd77") +
                geom_vline(aes(xintercept = input$data_spectra_range2[[1]]), color = "#ff6961") +
                geom_vline(aes(xintercept = input$data_spectra_range2[[2]]), color = "#ff6961") +
                geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), .ribbon1, inherit.aes = F, fill = "#77dd77", alpha = 0.5) +
                geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), .ribbon2, inherit.aes = F, fill = "#ff6961", alpha = 0.5) +
                geom_path(size = 0.25) +
                theme(axis.text.y = element_blank())
        } else {

        }
    })
})

observe({
    req(!is.integer(input$data_import_spectrometry_files))
    req(!is.integer(input$data_import_spectrometry_reference))
    req(spectra.preview$wavelengths, spectra.preview$data.series)

    output$data_spectra_linear_preview = renderPlotly({

        .data = data.table(x = spectra.preview$wavelengths, y = spectra.preview$data.series)
        .data2 = data.table(y = spectra.preview$data.series)
        .data3 = data.table(x = numeric(), y = numeric(), name = character())

        pf = y ~ NULL
        for (i in 2:(ncol(spectra.reference$dataset))) {
            col.name = colnames(spectra.reference$dataset)[[i]]

            pf = update.formula(pf, as.formula(sprintf("~ . + `%s`", col.name)))

            ref.x = spectra.reference$dataset[, 1, with = F]
            ref.y = spectra.reference$dataset[, i, with = F] # ??
            if (length(spectra.preview$wavelengths) != length(ref.x)) {
                #browser()
                int.ref = approx(x = ref.x[[1]], y = ref.y[[1]], xout = spectra.preview$wavelengths, yleft = 0, yright = 0)
                ref.y = data.table()[, (col.name) := int.ref$y]
            } else {

            }

            #browser()
            .data2 = cbind(.data2, ref.y)
        }

        .fit = lm(pf, .data2)
        .coef = coefficients(.fit)

        for (i in 2:(ncol(spectra.reference$dataset))) {
            col.name = colnames(spectra.reference$dataset)[[i]]

            .data3 = rbindlist(list(.data3,
                data.table(x = spectra.reference$dataset[, 1, with = F],
                            y = .coef[[i]] * spectra.reference$dataset[, i, with = F],
                            name = col.name)
                ),
                use.names = F)
        }

        ggplotly(ggplot(.data, aes(x = x, y = y)) +
            geom_path(size = 0.2) +
            theme_minimal(11) +
            labs(x = "Wavelength (nm)", y = NULL) +
            coord_cartesian(expand = F) +
            geom_path(aes(x = x, y = y, color = name), .data3, size = 0.4, inherit.aes = F, alpha = 0.85) +
            scale_color_viridis_d(begin = 0.2, end = 0.8, option = "turbo") +
            theme(axis.text.y = element_blank())
        )
    })
})

observeEvent(input$data_import_spectrometry_reference, {
    if (is.integer(input$data_import_spectrometry_reference)) {

    } else {
        fi = parseFilePaths(root=root.dirs, selection = input$data_import_spectrometry_reference)
        output$data_import_spectrometry_reference_filename = renderText(fi$datapath)
        spectra.reference$dataset = as.data.table(read_csv(fi$datapath, na = default$missing_values))
    }
})

observeEvent(input$data_spectra_range1_lower, {
    if (input$data_spectra_range1[[1]] != input$data_spectra_range1_lower)
        updateSliderInput(session, "data_spectra_range1", value = c(input$data_spectra_range1_lower, input$data_spectra_range1[[2]]))
})

observeEvent(input$data_spectra_range1_upper, {
    if (input$data_spectra_range1[[2]] != input$data_spectra_range1_upper)
        updateSliderInput(session, "data_spectra_range1", value = c(input$data_spectra_range1[[1]], input$data_spectra_range1_upper))
})

observeEvent(input$data_spectra_range2_lower, {

    if (input$data_spectra_range2[[1]] != input$data_spectra_range2_lower)
        updateSliderInput(session, "data_spectra_range2", value = c(input$data_spectra_range2_lower, input$data_spectra_range2[[2]]))
})

observeEvent(input$data_spectra_range2_upper, {
    if (input$data_spectra_range2[[2]] != input$data_spectra_range2_upper)
        updateSliderInput(session, "data_spectra_range2", value = c(input$data_spectra_range2[[1]], input$data_spectra_range2_upper))
})

observeEvent(input$data_spectra_range1, {
    req(input$data_spectra_range1_lower, input$data_spectra_range1_upper)

    if (input$data_spectra_range1[[1]] != input$data_spectra_range1_lower)
        updateNumericInput(session, "data_spectra_range1_lower", value = input$data_spectra_range1[[1]])

    if (input$data_spectra_range1[[2]] != input$data_spectra_range1_upper)
        updateNumericInput(session, "data_spectra_range1_upper", value = input$data_spectra_range1[[2]])
})

observeEvent(input$data_spectra_range2, {
    req(input$data_spectra_range2_lower, input$data_spectra_range2_upper)

    if (input$data_spectra_range2[[1]] != input$data_spectra_range2_lower)
        updateNumericInput(session, "data_spectra_range2_lower", value = input$data_spectra_range2[[1]])

    if (input$data_spectra_range2[[2]] != input$data_spectra_range2_upper)
        updateNumericInput(session, "data_spectra_range2_upper", value = input$data_spectra_range2[[2]])
})

observeEvent(input$data_spectra_method, {
    if (input$data_spectra_method == "2-color") {
        shinyjs::show("spectra_auc")
        shinyjs::hide("spectra_linear")
    } else if (input$data_spectra_method == "linear") {
        shinyjs::hide("spectra_auc")
        shinyjs::show("spectra_linear")
    }
})

observeEvent(input$data_new_spectra, {
    spectra.preview$data.series = NULL
    spectra.preview$wavelengths = NULL
    output$data_spectra_preview = renderPlot({})
    output$data_import_spectrometry_filename = renderText("select one or more tab-delimited spectrometer recording files (*.txt)")
    output$data_import_spectrometry_reference_filename = renderText("select a file that defines all reference spectra (*.csv)")
    showModal(
        modalDialog(
            fluidRow(
                tags$label("Spectrometer Data File", style="margin-bottom: 0.5rem;"),
                column(2, shinyFilesButton("data_import_spectrometry_file", label = "Browse...", title = "", multiple = T, style = "display: block-inline;")),
                column(10, verbatimTextOutput("data_import_spectrometry_filename"))
            ),
            fluidRow(
                column(3, numericInput("data_spectra_header_row", "Wavelength Row (#)", min = 1, value = default$import_spectra_header_row)),
                column(3, numericInput("data_spectra_data_row", "Data Row (#)", min = 1, value = default$import_spectra_data_row)),
                column(3, numericInput("data_spectra_frequency", "Frequency (Hz)", min = 1, value = default$import_spectra_frequency)),
                column(3, pickerInput("data_spectra_method", "Method", c("summary statistic"="2-color", "linear unmixing"="linear"), width = "100%"))
            ),

            fluidRow(
                column(12, pickerInput("data_spectra_preview_file", label = "Preview File", choices = c(), width = "100%"))
            ),

            tags$hr(style = "margin-top: 0rem; margin-bottom: 0.5rem;"),

            div(id = "spectra_auc",
                fluidRow(
                    column(4, pickerInput("data_spectra_ratio", "Create Ratio", c("color 1 / color 2", "color 2 / color 1", "none"), width = "100%")),
                    column(4, pickerInput("data_spectra_trend", "Detrending Model", c("none", "linear", "exponential"), width = "100%")),
                    column(4, pickerInput("data_spectra_function", "Summary Function", choices = c("auc", "mean", "median"), width = "100%"))
                ),
                fluidRow(
                    column(2, numericInput("data_spectra_minimum", "Minimum", 400, step = 0.001)),
                    column(8, plotOutput("data_spectra_preview", height = "200px")),
                    column(2, numericInput("data_spectra_maximum", "Maximum", 800, step = 0.001))
                ),
                fluidRow(
                    column(2, numericInput("data_spectra_range1_lower", tags$span("Lower", style="color: #77dd77;"), 500, step = 0.001)),
                    column(8, sliderInput("data_spectra_range1", tags$span("Color #1", style="color: #77dd77;"), width = "100%", value = c(500, 550), min = 400, max = 800, step=0.001, dragRange = T)),
                    column(2, numericInput("data_spectra_range1_upper", tags$span("Upper", style="color: #77dd77;"), 550, step = 0.001))
                ),
                fluidRow(
                    column(2, numericInput("data_spectra_range2_lower", tags$span("Lower", style="color: #ff6961;"), 600, step = 0.001)),
                    column(8, sliderInput("data_spectra_range2", tags$span("Color #2", style="color: #ff6961;"), width = "100%", value = c(600, 650), min = 400, max = 800, step=0.001, dragRange = T)),
                    column(2, numericInput("data_spectra_range2_upper", tags$span("Upper", style="color: #ff6961;"), 650, step = 0.001))
                ),
            ),

            div(id = "spectra_linear",
                fluidRow(
                    tags$label("Spectra Reference File", style = "margin-bottom: 0.5rem;"),
                    column(2, shinyFilesButton("data_import_spectrometry_reference", label = "Browse...", title = "", multiple = F, style = "display: block-inline;")),
                    column(10, verbatimTextOutput("data_import_spectrometry_reference_filename"))
                ),
                fluidRow(
                    column(12, plotlyOutput("data_spectra_linear_preview", height = "200px")),

                )
            ),

            size = "l",
            footer = tagList(modalButton("Cancel"), actionButton("data_import_spectrometry_action", "Import")))
    )
    shinyjs::show("spectra_auc")
    shinyjs::hide("spectra_linear")
})

observeEvent(input$data_import_spectrometry_action, {
    if (is.integer(input$data_import_spectrometry_file)) {

    } else {

        files = parseFilePaths(root=root.dirs, selection = input$data_import_spectrometry_file)

        # going to assume all files imported at the same time are formatted exactly the same so just use the first one's header
        wavelength.c = as.data.table(read_tsv(files[1, ]$datapath, col_names = F, n_max = 1, na = default$missing_values, skip = input$data_spectra_header_row - 1, show_col_types = F))
        wavelength.n = melt(wavelength.c, measure.vars = 3:ncol(wavelength.c)) # could this be faster?

        if (input$data_spectra_method == "2-color") {
            wavelength.1 = which(input$data_spectra_range1[[1]] <= wavelength.n$value & wavelength.n$value <= input$data_spectra_range1[[2]])
            wavelength.2 = which(input$data_spectra_range2[[1]] <= wavelength.n$value & wavelength.n$value <= input$data_spectra_range2[[2]])

            withProgress({
                for (f in 1:nrow(files)) {
                    incProgress(1/3 * (1 / nrow(files)), message = "importing data...", detail = files[f, ]$name)
                    file.data = read_tsv(files[f, ]$datapath, col_names = F, na = default$missing_values, skip = input$data_spectra_data_row, show_col_types = F)

                    incProgress(1/3 * (1 / nrow(files)), message = "processing data...", detail = files[f, ]$name)
                    color.1.data = file.data[, wavelength.1]
                    color.2.data = file.data[, wavelength.2]

                    if (input$data_spectra_function == "mean") {
                        color.1 = apply(color.1.data, 1, mean)
                        color.2 = apply(color.2.data, 1, mean)
                    } else if (input$data_spectra_function == "median") {
                        color.1 = apply(color.1.data, 1, median)
                        color.2 = apply(color.2.data, 1, median)
                    } else if (input$data_spectra_function == "auc") {
                        color.1 = apply(color.1.data, 1, auc, t = wavelength.n[wavelength.1, ]$value) #dt = wavelength.n[2, value] - wavelength.n[1, value])
                        color.2 = apply(color.2.data, 1, auc, t = wavelength.n[wavelength.2, ]$value) #dt = wavelength.n[2, value] - wavelength.n[1, value]) # FYI delta for these wavelengths varies slightly across the spectrum
                    }

                    # create raw dataset
                    incProgress(1/3 * (1 / nrow(files)), message = "creating dataset...", detail = files[f, ]$name)
                    data$raw[[files[f,]$name]] = data.table(color.1, color.2)
                    data$raw[[files[f,]$name]][, `(time)` := (.I - 1) / input$data_spectra_frequency]

                    data$models[[files[f,]$name]] = list()

                    data$analysis[[files[f,]$name]] = list()

                    # create a new metadata entry named as the imported filename
                    data$meta = append(data$meta,
                                       setNames(list(list(file = files[f,]$name,
                                                          path = normalizePath(files[f,]$datapath),
                                                          time = "(time)")),
                                                files[f,]$name))

                    # de-trend if necessary
                    if (input$data_spectra_trend == "linear") {
                        incProgress(0, message = "applying linear correction...")

                        .X = data$raw[[files[f,]$name]][, `(time)`]

                        model.1 = lm(Y ~ X, data = data.frame(X = .X, Y = color.1), na.action = "na.omit")
                        model.2 = lm(Y ~ X, data = data.frame(X = .X, Y = color.2), na.action = "na.omit")

                        yhat.1 = predict(model.1, list(X = .X))
                        yhat.2 = predict(model.2, list(X = .X))

                        data$raw[[files[f,]$name]][, color.1 := color.1 - yhat.1 + coefficients(model.1)[[1]]]
                        data$raw[[files[f,]$name]][, color.2 := color.2 - yhat.2 + coefficients(model.2)[[1]]]

                        data$models[[files[f,]$name]][["color.1"]] = list(type = "linear", m = coefficients(model.1)[[2]], b = coefficients(model.1)[[1]])
                        data$models[[files[f,]$name]][["color.2"]] = list(type = "linear", m = coefficients(model.2)[[2]], b = coefficients(model.2)[[1]])

                    } else if (input$data_spectra_trend == "exponential") {
                        incProgress(0, message = "applying exponential correction...")

                        .X = data$raw[[files[f,]$name]][, `(time)`]

                        tryCatch({
                            model.1 = nlsr(Y ~ alpha * exp(beta * X) + theta, start = c(alpha = 100.0, beta = -0.01, theta = 1000.0), data = data.frame(X = .X, Y = color.1), na.action = "na.omit")
                            model.2 = nlsr(Y ~ alpha * exp(beta * X) + theta, start = c(alpha = 100.0, beta = -0.01, theta = 1000.0), data = data.frame(X = .X, Y = color.2), na.action = "na.omit")

                            yhat.1 = predict(model.1, list(X = .X))
                            yhat.2 = predict(model.2, list(X = .X))

                            data$raw[[files[f,]$name]][, color.1 := color.1 - yhat.1 + coefficients(model.1)[["theta"]]]
                            data$raw[[files[f,]$name]][, color.2 := color.2 - yhat.2 + coefficients(model.2)[["theta"]]]

                            data$models[[files[f,]$name]][["color.1"]] = list(type = "exponential", a = coefficients(model.1)[["alpha"]], b = coefficients(model.1)[["beta"]], c = coefficients(model.1)[["theta"]])
                            data$models[[files[f,]$name]][["color.2"]] = list(type = "exponential", a = coefficients(model.2)[["alpha"]], b = coefficients(model.2)[["beta"]], c = coefficients(model.2)[["theta"]])

                        }, error = function(e) {
                            print(e)
                            showNotification(sprintf("Couldn't find exponential parameters for %s, try manually specifying them under Transform -> de-trend.", files[f,]$name), duration = NULL)
                        }, warning = function(w) {
                            print(w)
                            showNotification(sprintf("Couldn't find exponential parameters for %s, try manually specifying them under Transform -> de-trend.", files[f,]$name), duration = NULL)
                        })

                    } else {
                        # nothing
                    }

                    # create a ratio of the two colors
                    if (input$data_spectra_ratio == "color 2 / color 1") {
                        data$raw[[files[f,]$name]][, ratio := color.2/color.1]
                    } else if (input$data_spectra_ratio == "color 1 / color 2") {
                        data$raw[[files[f,]$name]][, ratio := color.1/color.2]
                    } else {

                    }
                }

            }, value = 0)

        } else if (input$data_spectra_method == "linear") {
            withProgress({

                for (f in 1:nrow(files)) {
                    incProgress(0, message = files[f, ]$name, detail = "importing dataset...")
                    file.data = read_tsv(files[f, ]$datapath, col_names = F, na = default$missing_values, skip = input$data_spectra_data_row, show_col_types = F)

                    ref.data = data.table()
                    pf = y ~ NULL
                    # create formula
                    for (i in 2:(ncol(spectra.reference$dataset))) {
                        col.name = colnames(spectra.reference$dataset)[[i]]
                        pf = update.formula(pf, as.formula(sprintf("~ . + `%s`", col.name)))

                        ref.x = spectra.reference$dataset[, 1, with = F]
                        ref.y = spectra.reference$dataset[, i, with = F]
                        if (length(spectra.preview$wavelengths) != length(ref.x)) {
                            int.ref = approx(x = ref.x[[1]], y = ref.y[[1]], xout = spectra.preview$wavelengths, yleft = 0, yright = 0)
                            ref.y = data.table()[, (col.name) := int.ref$y]
                        } else {

                        }
                        ref.data = cbind(ref.data, ref.y) #spectra.reference$dataset[, i, with = F])
                    }

                    incProgress(0, detail = "fitting models...")

                    .start = Sys.time()

                    .ret = apply(file.data, 1, \(row, reference, lm.formula) {
                        .fit = lm(lm.formula, cbind(data.frame(y = as.numeric(row[3:length(row)])), reference))
                        .coef = coefficients(.fit)
                        append(.coef, c(`(r-squared)` = summary(.fit)$r.squared))
                    }, reference = ref.data, lm.formula = pf)

                    printf("%s took %0.3f seconds\n", files[f,]$name, Sys.time() - .start)

                    data$raw[[files[f,]$name]] = as.data.table(t(.ret))

                    data$raw[[files[f,]$name]][, `(time)` := (.I - 1) / input$data_spectra_frequency]

                    data$analysis[[files[f,]$name]] = list()

                    # create a new metadata entry named as the imported filename
                    data$meta = append(data$meta,
                                       setNames(list(list(file = files[f,]$name,
                                                          path = normalizePath(files[f,]$datapath),
                                                          time = "(time)")),
                                                files[f,]$name))

                    incProgress(1 / nrow(files))
                }
            }, value = 0)
        }

        removeModal()
        refreshDatasetChoices()
    }
})
