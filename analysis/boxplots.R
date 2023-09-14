 output$summary_plot = renderPlot({
    dataset = data.table()
    values = data.table()

    req(input$summary_series)
    req(length(input$summary_series) > 0)

    #browser()

    #for (f in names(input$summary_series)) { #for (f in input$summary_dataset) {
        #browser()
        for (sel in input$summary_series) { #for (s in input$summary_series) { #for (s in names(data$series[[f]])) {

            f = str_split(sel, "!!")[[1]][[1]]
            s = str_split(sel, "!!")[[1]][[2]]

            .s = data$series[[f]][[s]]
            for (e in 1:length(data$events[[f]][[s]])) {
                .ed = data$events[[f]][[s]][[e]]
                dataset = rbindlist(list(dataset, .ed[, .(f = f, s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]))

                for (.i in .s$intervals$name) {
                    .ss = .ed[`(interval)` == .i, .(f = f, s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]

                        auc.value = 0
                    if (input$summary_function == "auc" & nrow(.ss) > 2) {


                        if (input$summary_auc_type == "full") {
                            weights = c(1, rep(2, nrow(.ss) - 2), 1)
                            .auc = .ss[, Y] * weights * ((.ss[2, X] - .ss[1, X]) / 2)
                            auc.value = sum(.auc, na.rm = T)

                        } else if (input$summary_auc_type == "range") {
                            subset = .ss[X > input$summary_auc_range[[1]] & X < input$summary_auc_range[[2]], ]
                            weights = c(1, rep(2, nrow(subset) - 2), 1)
                            .auc = subset[, Y] * ((subset[2, X] - subset[1,X]) / 2)
                            auc.value = sum(.auc, na.rm = T)
                        }

                    }

                        values = rbindlist(list(values, data.table(f = f, s = s, e = e, i = .i, value = auc.value)))

                }
            }
        }
    #}

    if (input$summary_function == "mean") {
        values = dataset[, .(value = mean(Y, na.rm=T)), by = .(f, s, e, i)]

    } else if (input$summary_function == "median") {
        values = dataset[, .(value = median(Y, na.rm=T)), by = .(f, s, e, i)]
    }

    #for (f in input$summary_dataset) {

    for (sel in input$summary_series) { #for (s in input$summary_series) { #for (s in names(data$series[[f]])) {
        f = str_split(sel, "!!")[[1]][[1]]
        s = str_split(sel, "!!")[[1]][[2]]
        .w = which(values$f == f)
        data$analysis[[f]]$summary = values[.w, ]
    }


    # output$summary_data = renderExcel({
    #     excelTable(data = values)
    # })

     #ggplotly({
         .gg = ggplot(values, aes(x = i, y = value, fill = s)) +
            geom_boxplot(size = 0.25, outlier.shape = NA, position = position_dodge(width = 1)) +
            geom_point(size = 0.5, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 1)) +
            (get(paste0("theme_", input$summary_theme)))(base_size = input$summary_font_size) +
            theme(legend.position = "bottom", text = element_text(size = input$summary_font_size, family = input$summary_font_family)) +
            labs(x = "Interval")

    if (input$summary_function == "mean") { .gg = .gg + labs(y = "Mean") }
    else if (input$summary_function == "median") { .gg = .gg + labs(y = "Median") }
    else { .gg = .gg + labs(y = "AUC") }

         if (input$summary_palette == "gradient2") {
            #.gg = .gg + scale_fill_stepsn(colors = c(input$summary_from, input$summary_to), guide = guide_colorsteps(title = NULL))
            .gg = .gg + scale_fill_manual(values = colorRampPalette(c(input$summary_from, input$summary_to))(values[, length(unique(s))]))
         } else if (input$summary_palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
            .gg = .gg + scale_fill_viridis_d(option = input$summary_palette, begin = 0.1, end = 0.9)
         }

         .gg
     #})  %>% config() %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(yaxis = list(tickmode = "auto"))) %>%
     #    toWebGL2()

    # this was not a plotly plot for reasons
 })


observe({
    req(length(names(data$series)) > 0)

#observeEvent(input$summary_dataset, {
    #updatePickerInput(session, "summary_series", choices = names(data$series[[input$summary_dataset]]))
    series.list = lapply(setNames(names(data$series), names(data$series)), \(f) {
        if (length(names(data$series[[f]])) > 0) {
            setNames(paste0(f, "!!", names(data$series[[f]])), names(data$series[[f]]))
        } else {
            NULL
        }
    })

    updateVirtualSelect("summary_series", choices = series.list, selected = list())
})

 observeEvent(input$summary_auc_type, {
     if (input$summary_auc_type == "range") {
        .min = Inf
        .max = -Inf
        #for (s in input$summary_series) {
        for (sel in input$summary_series) { #for (s in input$summary_series) { #for (s in names(data$series[[f]])) {
            f = str_split(sel, "!!")[[1]][[1]]
            s = str_split(sel, "!!")[[1]][[2]]
            .s = data$series[[f]][[s]]
            for (e in 1:length(data$events[[f]][[s]])) {
                .ed = data$events[[f]][[s]][[e]]
                .max = max(.max, .ed[, max(`(event time)`, na.rm = T)])
                .min = min(.min, .ed[, min(`(event time)`, na.rm = T)])
            }
        }
        updateSliderInput(session, "summary_auc_range", min = .min, max = .max, value = c(.min, .max))
     }
 })
