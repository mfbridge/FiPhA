# import-tabular.R

shinyFileChoose(input, "data_new_xlcsv_file", root=c(directories, `Working Directory`='.', getVolumes()()), filetypes=c("csv", "xlsx"))

observeEvent(input$data_new_xlcsv, {
    output$data_new_log = renderText({ "Ready to import" })
    output$data_new_xlcsv_name = renderText({ "select one or more similarly-formatted *.xlsx/*.csv files" })

    showModal(
        modalDialog(title = "Import a tabular dataset", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_new_xlcsv_finish", "Import")),
            fluidRow(
                column(2, shinyFilesButton("data_new_xlcsv_file", label = "Browse...", title = "", multiple = T)),
                column(10, verbatimTextOutput("data_new_xlcsv_name"))
            ),
            fluidRow(
                column(3, numericInput("data_new_header_row", "Header Row #", min = 1, value = default$import_header_row)),
                column(3, numericInput("data_new_data_row", "Data Row #", min = 1, value = default$import_data_row)),
                column(3, numericInput("data_new_frequency", "Frequency (Hz)", min = 1, value = default$import_frequency)),
                column(3, numericInput("data_new_sheet", "Sheet #", min = 1, value = 1))
            ),
            verbatimTextOutput("data_new_log")
        )
    )
})

observeEvent(input$data_new_xlcsv_file, {
    if (is.integer(input$data_new_xlcsv_file)) {
        # nothing selected
    } else {
        fileinfo = parseFilePaths(root=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_new_xlcsv_file)
        output$data_new_xlcsv_name = renderText({ sprintf("%s", paste0(fileinfo$name, collapse=", ")) })
    }
})

observeEvent(input$data_new_xlcsv_finish, {
    if (is.integer(input$data_new_xlcsv_file)) {
        # nothing selected
    } else {
        fileinfo = parseFilePaths(root=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_new_xlcsv_file)

        tryCatch({
            withProgress({
                for (f in 1:nrow(fileinfo)) {
                    # import each selected file

                    incProgress(0, message = fileinfo[f,]$name)

                    ext = regmatches(fileinfo[f,]$name, regexpr("([a-zA-Z0-9]+)$", fileinfo[f,]$name))

                    if (tolower(ext) == "csv") {
                        # read header separately
                        header = as.character(read_csv(fileinfo[f,]$datapath,
                                                       col_names = F,
                                                       n_max = 1,
                                                       na = default$missing_values,
                                                       skip = input$data_new_header_row - 1,
                                                       show_col_types = F))

                        suppressWarnings({
                            data$raw[[fileinfo[f,]$name]] = as.data.table(read_csv(fileinfo[f,]$datapath,
                                                                                   col_names = header,
                                                                                   na = default$missing_values,
                                                                                   skip = input$data_new_data_row,
                                                                                   show_col_types = F))
                        })

                    } else if (tolower(ext) == "xlsx") {
                        # read header
                        header = as.character(read_xlsx(fileinfo[f,]$datapath,
                                                        na = default$missing_values,
                                                        sheet = input$data_new_sheet,
                                                        col_names = F,
                                                        range = cell_limits(c(input$data_new_header_row, 1), c(input$data_new_header_row, NA))))

                        data$raw[[fileinfo[f,]$name]] = as.data.table(read_xlsx(fileinfo[f,]$datapath,
                                                                                na = default$missing_values,
                                                                                sheet = input$data_new_sheet,
                                                                                col_names = header,
                                                                                range = cell_limits(c(input$data_new_data_row, NA), c(NA, NA))))
                    }

                    data$raw[[fileinfo[f,]$name]][, `(time)` := (.I - 1) / input$data_new_frequency]

                    data$analysis[[fileinfo[f,]$name]] = list()

                    # create a new metadata entry named as the imported filename
                    data$meta = append(data$meta,
                                       setNames(list(list(file = fileinfo[f,]$name,
                                                          path = normalizePath(fileinfo[f,]$datapath),
                                                          time = "(time)")),
                                                fileinfo[f,]$name))

                    incProgress(amount = 1/nrow(fileinfo))
                }
            })
        }, error = \(e) {
            output$data_new_log = renderText(as.character(e))
        }, warning = \(w) {
            output$data_new_log = renderText(as.character(w))
        }, finally = \() {
        })
        removeModal()
        updatePickerInput(session, "data_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "heatmap_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "heatmap2_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "events_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "power_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "lag_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
        updatePickerInput(session, "summary_dataset", choices = names(data$meta), selected = fileinfo[f,]$name)
    }
})
