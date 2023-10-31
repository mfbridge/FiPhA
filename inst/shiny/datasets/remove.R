
observeEvent(input$data_remove_finish, {
    if (str_length(input$data_dataset) > 0) {
        for (d in names(data)) {
            #data[[d]][input$data_rename_current] = setNames(data[[d]][input$data_rename_current], input$data_rename_new_name)
            
            data[[d]][input$data_dataset] = NULL
        }
        updatePickerInput(session, "data_dataset", selected = NULL, choices = names(data$meta))
        updateCurrentVariableSelections()
        removeModal()
    }
})

observeEvent(input$data_remove, {
    if (str_length(input$data_dataset) > 0) {
        showModal(modalDialog(
            sprintf("Removing the dataset: %s", input$data_dataset), title = "Are you sure?", size = "l", fade = F, footer = tagList(modalButton("No"), actionButton("data_remove_finish", "Yes"))))
    }
})
