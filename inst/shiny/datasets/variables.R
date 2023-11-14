
refreshDatasetChoices = function () {
    datasets = names(data$meta)

    updatePickerInput(session, "data_dataset", choices = datasets, selected = datasets[length(datasets)])
    updatePickerInput(session, "heatmap_dataset", choices = datasets)
    updatePickerInput(session, "heatmap2_dataset", choices = datasets)
    updatePickerInput(session, "events_dataset", choices = datasets)
    updatePickerInput(session, "power_dataset", choices = datasets)
    updatePickerInput(session, "lag_dataset", choices = datasets)
    updatePickerInput(session, "summary_dataset", choices = datasets)
}

updateCurrentVariableSelections = function () {
    variables = names(data$raw[[input$data_dataset]])

    updatePickerInput(session, "data_signal_var", choices = variables, selected = input$data_signal_var)
    updatePickerInput(session, "data_corr_var2", choices = variables, selected = input$data_corr_var2)
    updateVirtualSelect("data_plot_x", choices = variables, selected = input$data_plot_x)
    updateVirtualSelect("data_plot_y", choices = variables, selected = input$data_plot_y)
    updatePickerInput(session, "data_time", choices = variables, selected = data$meta[[input$data_dataset]]$time)
    updateEventsDatasetPicker()
}

observeEvent(input$data_dataset, {
    if (input$data_dataset %in% names(data$meta)) {
        variables = names(data$raw[[input$data_dataset]])
        updateVirtualSelect("data_plot_x", choices = variables, selected = data$meta[[input$data_dataset]]$time)
        updateVirtualSelect("data_plot_y", choices = variables, selected = input$data_plot_y)
        updateTextInput(session, "data_source", value = data$meta[[input$data_dataset]]$path)
        updatePickerInput(session, "data_time", choices = variables, selected = data$meta[[input$data_dataset]]$time)
        updateEventsDatasetPicker()
    }
})

observeEvent(input$data_time, {
    if (input$data_dataset %in% names(data$meta)) {
        data$meta[[input$data_dataset]]$time = input$data_time
    }
})

getAllModelStrings = function() {
    strings = data.table()

    for (f in names(data$models)) {
        if (length(data$models[[f]]) > 0) {
            for (i in 1:length(data$models[[f]])) {
                name = names(data$models[[f]])[i]
                v = data$models[[f]][[i]]

                if (v$type == "exponential") {
                    strings = rbindlist(list(strings, data.table(file = f, variable = name, model = sprintf("exponential, y = a*exp(b*x)+c, where a = %0.6f, b = %0.6f, c = %0.6f", v$a, v$b, v$c))))
                } else if (v$type == "linear") {
                    strings = rbindlist(list(strings, data.table(file = f, variable = name, model = sprintf("linear, y = m*x+b, where m = %0.6f, b = %0.6f", v$m, v$b))))
                }
            }
        }
    }

    strings
}

# print any models applied
output$data_models = renderText({
    string = ""

    if (length(data$models[[input$data_dataset]]) > 0) {
        for (i in 1:length(data$models[[input$data_dataset]])) {
            name = names(data$models[[input$data_dataset]])[i]
            v = data$models[[input$data_dataset]][[i]]
            if (v$type == "exponential") {
                string = sprintf("%s<b>%s</b><br/> - model: exponential, y = a*exp(b*x)+c<br/> - parameters: a = %0.6f, b = %0.6f, c = %0.6f<br/><br/>", string, name, v$a, v$b, v$c)
            } else if (v$type == "linear") {
                string = sprintf("%s<b>%s</b><br/> - model: linear, y = m*x+b<br/> - parameters: m = %0.6f, b = %0.6f<br/><br/>", string, name, v$m, v$b)
            }
        }
    }

    HTML(paste0("<pre>",string,"</pre>"))
})
