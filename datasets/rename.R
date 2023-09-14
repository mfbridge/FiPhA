
observeEvent(input$data_rename_finish, {
    # rename selected variable
    ds.name = input$data_rename_current

    if (str_length(input$data_rename_var_name) > 0) {
        #this should work, but as it turns out only if the internal structure of the DT object wasn't loaded from a disk (see truelength() / alloc.col())
        #data$raw[[input$data_rename_current]][, (input$data_rename_var_name) := get(input$data_rename_var)]

        data$raw[[input$data_rename_current]] = data$raw[[input$data_rename_current]][, (input$data_rename_var_name) := get(input$data_rename_var)]
        data$raw[[input$data_rename_current]][, (input$data_rename_var) := NULL]
    }

    # rename a dataset's list item in all the internal structures
    if (str_length(input$data_rename_new_name) > 0) {
        for (d in names(data)) {
            #data[[d]][input$data_rename_current] = setNames(data[[d]][input$data_rename_current], input$data_rename_new_name)

            data[[d]][input$data_rename_new_name] = data[[d]][input$data_rename_current]
            data[[d]][input$data_rename_current] = NULL
        }

        updatePickerInput(session, "data_dataset", choices = names(data$meta), selected = input$data_rename_new_name)
        ds.name = input$data_rename_new_name
    }

    variables = names(data$raw[[ds.name]])
    updatePickerInput(session, "data_plot_x", choices = variables, selected = input$data_plot_x)
    updatePickerInput(session, "data_plot_y", choices = variables, selected = input$data_plot_y)
    updatePickerInput(session, "data_time", choices = variables, selected = input$data_time)

    removeModal()
})

observeEvent(input$data_rename, {
    updateTextInput(session, "data_rename_new_name", value = "")
    updateTextInput(session, "data_rename_var_name", value = "")
    updateTextInput(session, "data_rename_current", value = input$data_dataset)
    updatePickerInput(session, "data_rename_var", choices = names(data$raw[[input$data_dataset]]))

    showModal(
        modalDialog(title = "Rename...", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_rename_finish", "Finish")),
            fluidRow(
                column(6, disabled(textInput("data_rename_current", "Active Dataset", "", width = "100%"))),
                column(6, textInput("data_rename_new_name", "New Name", "", width = "100%"))
            ),
            tags$hr(),
            fluidRow(
                column(6, pickerInput("data_rename_var", "Variable", c(), width = "100%")),
                column(6, textInput("data_rename_var_name", "New Name", "", width = "100%"))
            )
        )
    )
})

## append

observeEvent(input$data_append, {
    updatePickerInput(session, "data_append_src", choices = names(data$raw))
    updatePickerInput(session, "data_append_dest", choices = names(data$raw))

    showModal(
        modalDialog(title = "Rename...", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_append_finish", "Finish")),
            fluidRow(
                column(4, pickerInput("data_append_src", "Source Dataset", c(), width = "100%")),
                column(4, pickerInput("data_append_dest", "Destination Dataset", c(), width = "100%")),
                column(4, numericInput("data_append_freq", "New Time Frequency (Hz)", 25, min = 0, step = 1))
            )
        )
    )
})

observeEvent(input$data_append_finish, {
    data$raw[[input$data_append_dest]] = rbindlist(list(data$raw[[input$data_append_dest]], data$raw[[input$data_append_src]]))
    data$raw[[input$data_append_dest]][, (data$meta[[input$data_append_dest]]$time) := (.I - 1) / input$data_append_freq]

    removeModal()
})
