

# observeEvent(input$events_dataset, {
#     updatePickerInput(session, "events_binary_variable", choices = getCommonDatasetVariables(input$events_dataset), selected = input$events_binary_variable)
#     updatePickerInput(session, "events_peakwindow_variable", choices = getCommonDatasetVariables(input$events_dataset), selected = input$events_peakwindow_variable)
# })



# update pre-generated event series name --------------------------------------------------------------------------

getEventsPlaceholderName = function() {

}

observeEvent(c(input$events_type, input$events_binary_variable, input$events_binned_start, input$events_binned_length, input$events_peakwindow_variable,
    input$events_peakwindow_width, input$events_peakwindow_n, input$events_fixed_list, input$events_conditions), {
    if (input$events_name == "") {
        if (input$events_type == "binary") {
            updateTextInput(session, "events_name", placeholder = sprintf("binary, %s", input$events_binary_variable))

        } else if (input$events_type == "binaryinv") {
            updateTextInput(session, "events_name", placeholder = sprintf("binary (inverted), %s", input$events_binary_variable))

        } else if (input$events_type == "binned") {
            updateTextInput(session, "events_name", placeholder = sprintf("binned every %0.3f sec, t0=%0.3f", input$events_binned_length, input$events_binned_start))

        } else if (input$events_type == "peak") {
            updateTextInput(session, "events_name", placeholder = sprintf("peak, win=%0.3fs n.sd=%0.3f, %s", input$events_peak_variable, input$events_peakwindow_width, input$events_peakwindow_n ))

        } else if (input$events_type == "list") {
            updateTextInput(session, "events_name", placeholder = sprintf("list of %d events", nrow(excel_to_R(input$events_fixed_list))))
        }
    }
})


# toggle event type options ---------------------------------------------------------------------------------------


observeEvent(input$events_type, {
    #browser()
    toggleEventTypeOptions()
})

toggleEventTypeOptions = function() {
    req(input$events_type)
    if (input$events_type %in% c("binary", "binaryinv")) {
        shinyjs::show("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "binned") {
        shinyjs::hide("events_binary_")
        shinyjs::show("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "scorepeak") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::show("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::hide("events_list_")

    } else if (input$events_type == "peak") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::show("events_peakwindow_")
        shinyjs::hide("events_list_")


    } else if (input$events_type == "list") {
        shinyjs::hide("events_binary_")
        shinyjs::hide("events_binned_")
        shinyjs::hide("events_scorepeak_")
        shinyjs::hide("events_peakwindow_")
        shinyjs::show("events_list_")


    }
}
