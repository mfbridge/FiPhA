 output$heatmap2_plot = renderPlotly({
    req(input$heatmap2_dataset)
    req(input$heatmap2_series)

    dataset = data.table()
    aucs = data.table()

    for (f in input$heatmap2_dataset) {
        for (s in input$heatmap2_series) { #for (s in names(data$series[[f]])) {
            .s = data$series[[f]][[s]]
            for (e in 1:length(data$events[[f]][[s]])) {
                .ed = data$events[[f]][[s]][[e]]

                dataset = rbindlist(list(dataset, .ed[, .(f = f, s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]))
            }
        }
    }
    dataset[, Xstr := as.numeric(sprintf("%0.3f", X))]

    if (input$traces_sdse == "SD") {
        dataset[, `:=`(meanY = mean(Y, na.rm = T),
            upperY = mean(Y, na.rm = T) + input$heatmap2_meansd_n * sd(Y, na.rm = T),
            lowerY = mean(Y, na.rm = T) - input$heatmap2_meansd_n * sd(Y, na.rm = T)), by = .(f, s, Xstr)]
    } else {
        dataset[, `:=`(meanY = mean(Y, na.rm = T),
            upperY = mean(Y, na.rm = T) + input$heatmap2_meansd_n * sd(Y, na.rm = T) / sqrt(.N),
            lowerY = mean(Y, na.rm = T) - input$heatmap2_meansd_n * sd(Y, na.rm = T) / sqrt(.N)), by = .(f, s, Xstr)]
    }

    dataset = dataset[!is.na(Y)&!is.na(upperY)&!is.na(lowerY),]
     ggplotly({
         .gg = ggplot(dataset, aes(x = X, y = meanY, color = f)) +
             geom_hline(yintercept = 0, linetype = "solid", size = 0.2) +
             geom_vline(xintercept = 0, linetype = "dotted", size = 0.2)

         # if (input$heatmap2_palette == "gradient2") {
         #    .gg = .gg + scale_fill_gradientn(colors = c(input$heatmap2_from, input$heatmap2_to), guide = guide_colorsteps(title = NULL))
         # } else {
         #    .gg = .gg + scale_fill_viridis_c(option = input$heatmap2_palette, guide = guide_colorsteps(title = NULL))
         # }
         #

        if (input$heatmap2_meansd & input$heatmap2_shade_intervals) {
            .gg = .gg +
                geom_ribbon(aes(x = X, ymax = upperY, ymin=lowerY, fill = interaction(f, s, i)), alpha = input$heatmap2_y_alpha, inherit.aes = F)# +
                #getPaletteFill(input$heatmap2_shade_palette, input$heatmap2_shade_from, input$heatmap2_shade_to)
        } else if (input$heatmap2_meansd & !input$heatmap2_shade_intervals) {
            .gg = .gg +
                geom_ribbon(aes(x = X, ymax = upperY, ymin=lowerY, fill = interaction(f, s)), inherit.aes = F) #+
                # scale_fill_gradient(low = "#404040", high = input$heatmap2_meansd_area)
        }

        .gg = .gg + geom_line(size = input$heatmap2_y_size, color = input$heatmap2_meansd_line, alpha = input$heatmap2_y_alpha) +
            (get(paste0("theme_", input$heatmap2_theme)))(base_size = input$heatmap2_font_size) +
            labs(x = "Event Time", y = NULL) +
            theme(legend.position = "bottom", text = element_text(size = input$heatmap2_font_size, family = input$heatmap2_font_family))


         .gg
     })  %>% config() %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))# %>% toWebGL2()
 })

 observeEvent(input$heatmap2_dataset, {
    updatePickerInput(session, "heatmap2_series", choices = names(data$series[[input$heatmap2_dataset]]))
 })

 observeEvent(input$heatmap2_auc_type, {
     if (input$heatmap2_auc_type == "range") {
        .min = Inf
        .max = -Inf
        for (s in input$heatmap2_series) {
            .s = data$series[[input$heatmap2_dataset]][[s]]
            for (e in 1:length(data$events[[input$heatmap2_dataset]][[s]])) {
                .ed = data$events[[input$heatmap2_dataset]][[s]][[e]]
                .max = max(.max, .ed[, max(`(event time)`, na.rm = T)])
                .min = min(.min, .ed[, min(`(event time)`, na.rm = T)])
            }
        }
        updateSliderInput(session, "heatmap_auc_range", min = .min, max = .max, value = c(.min, .max))
     }
 })
