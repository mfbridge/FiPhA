# import-rds.R

shinyFileChoose(input, "data_import_file_r", root=c(directories, `Working Directory`='.', getVolumes()()), filetypes=c("rds"))

observeEvent(input$data_rds, {
    output$data_import_r_filename = renderText("select a *.Rds file from a previous session")
    showModal(
        modalDialog(title = "Previous Session",
            fluidRow(
                column(2, shinyFilesButton("data_import_file_r", label = "Browse...", title = "", multiple = F)),
                column(10, verbatimTextOutput("data_import_r_filename"))
            ),
            size = "l",
            footer = tagList(modalButton("Cancel"), actionButton("data_import_action_r", "Import")))
    )
})

observeEvent(input$data_import_file_r, {
    if (is.integer(input$data_import_file_r)) {

    } else {
        fi = parseFilePaths(root=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_import_file_r)
        output$data_import_r_filename = renderText(fi$datapath)
    }
})


observeEvent(input$data_import_action_r, {
    if (is.integer(input$data_import_file_r)) {

    } else {
        fi = parseFilePaths(root=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_import_file_r)

        obj = readRDS(fi$datapath)

        for (n in names(obj)) {
            if (n == "raw") {
                for (j in names(obj$raw)) {
                    obj$raw[[j]] = copy(obj$raw[[j]]) # loaded from disk, need to set .internal.selfref ptr somehow
                }
            }
            data[[n]] = obj[[n]]
        }
    }
    removeModal()
    updatePickerInput(session, "data_dataset", choices = names(data$meta))
    updatePickerInput(session, "heatmap_dataset", choices = names(data$meta))
    updatePickerInput(session, "heatmap2_dataset", choices = names(data$meta))
    updatePickerInput(session, "events_dataset", choices = names(data$meta))
    updatePickerInput(session, "power_dataset", choices = names(data$meta))
    updatePickerInput(session, "lag_dataset", choices = names(data$meta))
    updatePickerInput(session, "summary_dataset", choices = names(data$meta))
})
