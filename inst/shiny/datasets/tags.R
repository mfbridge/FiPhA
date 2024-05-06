
observeEvent(input$data_tags, {
    showModal(modalDialog(title = "Tag Editor",

        virtualSelectInput("data_tags_dataset", "Dataset", choices = c(), width = "100%"),
        virtualSelectInput("data_tags_values", "Tags", choices = c(), showValueAsTags = T, search = T, allowNewOption = T, width = "100%", multiple = T),

    ))

    updateVirtualSelect("data_tags_dataset", choices = names(data$meta), selected = input$data_dataset)
})

observeEvent(input$data_tags_dataset, {
    req(input$data_tags_dataset)
    tag.choices = unique(unlist(lapply(data$meta, \(d) return(d$tags))))

    if (input$data_tags_dataset %in% names(data$meta)) {
        tags.selected = ifelse(is.null(data$meta[[input$data_tags_dataset]]$tags), character(0), data$meta[[input$data_tags_dataset]]$tags)
        updateVirtualSelect("data_tags_values", choices = tag.choices, selected = tags.selected)
        print(tags.selected)
    } else {
        updateVirtualSelect("data_tags_values", choices = tag.choices, selected = NULL)
    }
})

observeEvent(input$data_tags_values, {
    req(input$data_tags_dataset)

    if (input$data_tags_dataset %in% names(data$meta)) {
        data$meta[[input$data_tags_dataset]]$tags = input$data_tags_values
        print(input$data_tags_values)
    }
})
