
observeEvent(input$data_ratio_finish, {
    data$raw[[input$data_dataset]][, (input$data_ratio_name) := get(input$data_ratio_num) / get(input$data_ratio_den)]

    updateCurrentVariableSelections()
    removeModal()
})

observeEvent(input$data_ratio, {
    updatePickerInput(session, "data_ratio_num", choices = names(data$raw[[input$data_dataset]]))
    updatePickerInput(session, "data_ratio_den", choices = names(data$raw[[input$data_dataset]]))

    showModal(
        modalDialog(title = "Ratio...", size = "l", fade = F, footer = tagList(modalButton("Cancel"), actionButton("data_ratio_finish", "Finish")),
            fluidRow(
                column(4, pickerInput("data_ratio_num", "Numerator", choices = c(), width = "100%")),
                column(4, pickerInput("data_ratio_den", "Denominator", choices = c(), width = "100%")),
                column(4, textInput("data_ratio_name", "Name", value = "Ratio"))
            )
        )
    )
})

