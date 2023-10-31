# shiny stuff for saving a dataset in the data tab

shinyFileSave(input, "data_save", root=c(`Working Directory`='.', getVolumes()()), filetypes=c("csv", "xlsx"))
shinyFileSave(input, "data_saveall", root=c(`Working Directory`='.', getVolumes()()), filetypes=c("xlsx"))

observeEvent(input$data_save, {
    if (is.integer(input$data_save)) {
        # nothing selected
    } else {
        fileinfo = parseSavePath(root=c(`Working Directory`='.', getVolumes()()), selection = input$data_save)
        name = input$data_dataset

        if (str_length(name) > 0) {
            withProgress({
                if (fileinfo$type == "Excel Workbook") {
                    write_xlsx(data$raw[[name]], path = fileinfo$datapath, col_names = T, format_headers = F)
                } else {
                    write_csv(data$raw[[name]], path = fileinfo$datapath, na = "")
                }
            }, message = "Saving...", detail = input$data_dataset)
        }
    }
})


observeEvent(input$data_saveall, {
    if (is.integer(input$data_saveall)) {
        # nothing selected
    } else {
        fileinfo = parseSavePath(root=c(`Working Directory`='.', getVolumes()()), selection = input$data_saveall)

        withProgress({

            out.data = list()
            out.data[["Datasets"]] = data.table()
            out.data[["Models"]] = getAllModelStrings()

            for (f in names(data$raw)) {
                out.data[["Datasets"]] = rbindlist(list(out.data[["Datasets"]], data.table(name = f, path = data$meta[[f]]$path, filename = data$meta[[f]]$file)))
                out.data[[f]] = copy(data$raw[[f]])
            }

            write_xlsx(out.data, path = fileinfo$datapath, col_names = T, format_headers = F)
        }, message = "Exporting...")

    }
})
