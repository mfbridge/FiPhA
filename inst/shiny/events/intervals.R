
getIntervalNames = function() {
    intervals = setNames(as.data.table(excel_to_R(input$events_intervals)), names(default$intervals))
    return(intervals$name)
}

getIntervals = function() {
    setNames(as.data.table(excel_to_R(input$intervals)), names(default$intervals))
}

observeEvent(input$events_intervals, {
    updatePickerInput(session, "events_norm_baseline", choices = getIntervalNames(), selected = input$events_norm_baseline)
})

observeEvent(input$events_dataset, {
    updatePickerInput(session, "events_variables", choices = getCommonDatasetVariables(input$events_dataset), selected = input$events_variables)

})

# show/hide norm options if relevant
observeEvent(input$events_norm, {
    if (input$events_norm %in% c("z", "%", "r")) {
        shinyjs::show("events_norm_baseline")
        if (input$events_norm %in% c("z", "%")) {
            shinyjs::show("events_norm_function")
        } else {
            shinyjs::hide("events_norm_function")
        }
    } else {
        shinyjs::hide("events_norm_baseline")
        shinyjs::hide("events_norm_function")
    }
})
