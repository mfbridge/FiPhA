

shinyFileChoose(input, "data_unaligned_file", root=c(`Working Directory`='.', getVolumes()()), filetypes=c("csv", "xlsx"))

observeEvent(input$data_unaligned_time_unit, {
    if (input$data_unaligned_time_unit == "freq") {
        shinyjs::hide("data_unaligned_time")
        shinyjs::show("data_unaligned_frequency")
        shinyjs::show("data_unaligned_start")
    } else if (input$data_unaligned_time_unit == "var") {
        shinyjs::show("data_unaligned_time")
        shinyjs::hide("data_unaligned_frequency")
        shinyjs::hide("data_unaligned_start")
    }
})


observeEvent(input$data_unaligned_file, {
    if (is.integer(input$data_unaligned_file)) {
        # nothing selected
    } else {
        fileinfo = parseFilePaths(root=c(`Working Directory`='.', getVolumes()()), selection = input$data_unaligned_file)
        output$data_unaligned_filename = renderText({ sprintf("%s", paste0(fileinfo$name, collapse=", ")) })

        tryCatch({
            withProgress({
                for (f in 1:nrow(fileinfo)) {
                    # import each selected file

                    ext = regmatches(fileinfo[f,]$name, regexpr("([a-zA-Z0-9]+)$", fileinfo[f,]$name))

                    if (tolower(ext) == "csv") {
                        # read header separately
                        header = as.character(read_csv(fileinfo[f,]$datapath,
                                                       col_names = F,
                                                       n_max = 1,
                                                       na = default$missing_values,
                                                       skip = input$data_unaligned_header_row - 1,
                                                       show_col_types = F))

                    } else if (tolower(ext) == "xlsx") {
                        # read header
                        header = as.character(read_xlsx(fileinfo[f,]$datapath,
                                                        na = default$missing_values,
                                                        sheet = input$data_new_sheet,
                                                        col_names = F,
                                                        range = cell_limits(c(input$data_unaligned_header_row, 1), c(input$data_unaligned_header_row, NA))))
                    }
                }

                updatePickerInput(session, "data_unaligned_time", choices = header)
                updatePickerInput(session, "data_unaligned_vars", choices = header, selected = header)
            }, message = "Reading header...")
        }, error = \(e) {
            output$data_unaligned_log = renderText(as.character(e))
        }, warning = \(e) {
            output$data_unaligned_log = renderText(as.character(e))
        })
    }
})

observeEvent(input$data_unaligned_finish, {
if (is.integer(input$data_unaligned_file)) {
        # nothing selected
    } else {
        fileinfo = parseFilePaths(root=c(`Working Directory`='.', getVolumes()()), selection = input$data_unaligned_file)
        output$data_unaligned_filename = renderText({ sprintf("%s", paste0(fileinfo$name, collapse=", ")) })

        tryCatch({
            withProgress({

                for (f in 1:nrow(fileinfo)) {

                    Bi.list = c()
                    # import selected file

                    ext = regmatches(fileinfo[f,]$name, regexpr("([a-zA-Z0-9]+)$", fileinfo[f,]$name))

                    if (tolower(ext) == "csv") {
                        # read header separately
                        header = as.character(read_csv(fileinfo[f,]$datapath,
                                                       col_names = F,
                                                       n_max = 1,
                                                       na = default$missing_values,
                                                       skip = input$data_unaligned_header_row - 1,
                                                       show_col_types = F))

                        this.raw = as.data.table(read_csv(fileinfo[f,]$datapath, col_names = header, na = default$missing_values, skip = input$data_unaligned_data_row - 1, show_col_types = F))

                    } else if (tolower(ext) == "xlsx") {
                        # read header
                        header = as.character(read_xlsx(fileinfo[f,]$datapath,
                                                        na = default$missing_values,
                                                        sheet = input$data_unaligned_sheet,
                                                        col_names = F,
                                                        range = cell_limits(c(input$data_unaligned_header_row, 1), c(input$data_unaligned_header_row, NA))))

                        this.raw = as.data.table(read_xlsx(fileinfo[f,]$datapath, na = default$missing_values, sheet = input$data_unaligned_sheet, col_names = header, range = cell_limits(c(input$data_unaligned_data_row, NA), c(NA, NA))))
                    }

                    if (input$data_unaligned_time_unit == "freq") {
                        this.raw[, `(time)` := input$data_unaligned_start + (.I - 1) / input$data_unaligned_frequency]
                        this.raw.var = "(time)"
                    } else {
                        this.raw.var = input$data_unaligned_time
                    }

                    # align with currently active dataset (optimized to only work on vectors)
                    A = data$raw[[input$data_unaligned_dataset]]
                    At = data$meta[[input$data_unaligned_dataset]]$time
                    B = this.raw
                    Bt = this.raw.var

                    x = A[, get(At)]
                    y = B[, get(Bt)]

                    len.x = length(x)
                    len.y = length(y)

                    r = numeric(length(x))

                    j = 1
                    for (i in 1:len.x) {
                        while (j < len.y) {
                            if (is.numeric(y[j+1]) & is.numeric(x[i])) {
                                if (abs(y[j+1] - x[i]) < abs(y[j] - x[i])) {
                                    j = j + 1
                                } else {
                                    break
                                }
                            } else {
                                break
                            }
                        }
                        if (j <= len.y) {
                            r[i] = j
                        } else {
                            r[i] = NA # past end of file
                        }
                        if (i %% 1000 == 0) incProgress(1000/len.x)
                    }

                    data$raw[[input$data_unaligned_dataset]] = cbind(data$raw[[input$data_unaligned_dataset]], B[r, input$data_unaligned_vars, with = F])

                    # update some things that are currently showing lists of variables
                    variables = names(data$raw[[input$data_dataset]])
                    updatePickerInput(session, "data_plot_x", choices = variables, selected = input$data_plot_x)
                    updatePickerInput(session, "data_plot_y", choices = variables, selected = input$data_plot_y)
                    updatePickerInput(session, "data_time", choices = variables, selected = input$data_time)

                }
            }, message = "Aligning...", value = 0)

            #removeModal()
        }, error = \(e) {
            output$data_unaligned_log = renderText(as.character(e))
        }, warning = \(e) {
            output$data_unaligned_log = renderText(as.character(e))
        })
    }
})


observeEvent(input$data_unaligned, {
    output$data_unaligned_filename = renderText("Select an *.xlsx/*.csv file")
    output$data_unaligned_log = renderText("")

    updatePickerInput(session, "data_unaligned_dataset", choices = names(data$meta))

    showModal(
        modalDialog(title = "Append (un)aligned dataset", size = "l", fade = F, footer = tagList(modalButton("Close"), actionButton("data_unaligned_finish", "Append")),
            pickerInput("data_unaligned_dataset", NULL, choices = c()),
            verbatimTextOutput("data_unaligned_filename"),
            fluidRow(
                column(3, shinyFilesButton("data_unaligned_file", label = "Browse...", title = "", multiple = F)),
                column(9, pickerInput("data_unaligned_vars", NULL, c(), width = "100%", multiple = T, options = list(`actions-box`=T, title = "Variables to import")))
            ),
            tags$hr(),
            fluidRow(
                column(3, numericInput("data_unaligned_header_row", "Header Row #", min = 1, value = default$unaligned_header_row)),
                column(3, numericInput("data_unaligned_data_row", "Data Row #", min = 1, value = default$unaligned_data_row)),
                column(3, pickerInput("data_unaligned_time_unit", NULL, c("frequency (hz)"="freq", "time variable"="var"), selected = "var"),
                       numericInput("data_unaligned_frequency", NULL, min = 1, value = default$unaligned_frequency),
                       numericInput("data_unaligned_start", HTML("t<sub>0</sub> ="), min = -Inf, max = Inf, value = 0, step = 0.001),
                       pickerInput("data_unaligned_time", NULL, choices = list("NA"=list()), multiple = F, options=list(`max-options`=1))),
                column(3, numericInput("data_unaligned_sheet", "Sheet #", min = 1, value = 1))
            ),
            verbatimTextOutput("data_unaligned_log")
        )
    )
})
