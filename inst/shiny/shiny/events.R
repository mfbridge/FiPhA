
updateEventsDatasetPicker = function() {
    updatePickerInput(session, "events_dataset", choices = names(data$meta), selected = input$events_dataset)
    updatePickerInput(session, "heatmap_dataset", choices = names(data$meta))
    updatePickerInput(session, "lag_dataset", choices = names(data$meta))
}




observeEvent(input$events_dataset, {
    updatePickerInput(session, "events_series", choices = names(data$series[[input$events_dataset]]), selected = input$events_series)
})
