# subsetting


observeEvent(input$data_subset, {
    showModal(
        modalDialog(
            title = "Subset",
            footer = tagList(modalButton("Close"), actionButton("data_subset_action", "Subset")),
            pickerInput("data_subset_dataset", "Dataset", c(), "", multiple = F, options = list(), width="100%"),

            fluidRow(
                column(2,
                    pickerInput("data_subset_type", "By", choices = c("row", "time"), selected = "row")
                ),
                column(10,
                    sliderInput("data_subset_range", "Row (#)", value = c(0, 0), min = 0, max = 0, width = "100%")
                ),
            )
        )
    )
    updatePickerInput(session, "data_subset_dataset", choices = names(data$meta), selected = names(data$meta)[[1]])
})

observeEvent(c(input$data_subset_dataset, input$data_subset_type), {
    req(input$data_subset_dataset %in% names(data$raw))
    nr = nrow(data$raw[[input$data_subset_dataset]])
    min.t = min(data$raw[[input$data_subset_dataset]][, get(data$meta[[input$data_subset_dataset]]$time)])
    max.t = max(data$raw[[input$data_subset_dataset]][, get(data$meta[[input$data_subset_dataset]]$time)])

    if (input$data_subset_type == "row") {
        updateSliderInput(session, "data_subset_range", label = "Row (#)", value = c(1, nr), min = 1, max = nr, step = 1)
    } else {
        updateSliderInput(session, "data_subset_range", label = "Timestamp (s)", value = c(min.t, max.t), min = min.t, max = max.t, step = 0.001)
    }
})

observeEvent(input$data_subset_action, {
    req(input$data_subset_dataset %in% names(data$raw))

    withProgress({
        if (input$data_subset_type == "row") {
            data$raw[[input$data_subset_dataset]] = data$raw[[input$data_subset_dataset]][input$data_subset_range[[1]]:input$data_subset_range[[2]], ]

            min.t = min(data$raw[[input$data_subset_dataset]][, get(data$meta[[input$data_subset_dataset]]$time)])
            data$raw[[input$data_subset_dataset]][, (data$meta[[input$data_subset_dataset]]$time) := get(data$meta[[input$data_subset_dataset]]$time) - min.t]
        } else {
            data$raw[[input$data_subset_dataset]] =
                data$raw[[input$data_subset_dataset]][(get(data$meta[[input$data_subset_dataset]]$time) < input$data_subset_range[[2]]) & ((get(data$meta[[input$data_subset_dataset]]$time) >= input$data_subset_range[[1]])), ]
            min.t = min(data$raw[[input$data_subset_dataset]][, get(data$meta[[input$data_subset_dataset]]$time)])
            data$raw[[input$data_subset_dataset]][, (data$meta[[input$data_subset_dataset]]$time) := get(data$meta[[input$data_subset_dataset]]$time) - min.t]
        }
    }, message = "Subsetting...")
})
