 output$heatmap_plot = renderPlotly({
    req(input$heatmap_dataset, input$heatmap_series)

     .heatmap$plot_object = NULL

    dataset = data.table()
    aucs = data.table()

    for (s in input$heatmap_series) {
        .s = data$series[[input$heatmap_dataset]][[s]]
        for (e in 1:length(data$events[[input$heatmap_dataset]][[s]])) {
            .ed = data$events[[input$heatmap_dataset]][[s]][[e]]

            dataset = rbindlist(list(dataset, .ed[, .(e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]))

            if (nrow(.ed)>2) {
                if (input$heatmap_auc_type == "full") {
                    weights = c(1, rep(2, nrow(.ed) - 2), 1)
                    .auc = .ed[, get(.s$responses[[1]])] * weights * ((.ed[2, `(event time)`] - .ed[1, `(event time)`]) / 2)

                } else if (input$heatmap_auc_type == "range") {
                    subset = .ed[`(event time)` > input$heatmap_auc_range[[1]] & `(event time)` < input$heatmap_auc_range[[2]], ]

                    weights = c(1, rep(2, nrow(subset) - 2), 1)
                    .auc = subset[, get(.s$responses[[1]])] * ((subset[2, `(event time)`] - subset[1, `(event time)`]) / 2)
                }
                auc.value = sum(.auc, na.rm = T)
                aucs = rbindlist(list(aucs, data.table(e = e, auc = auc.value)))
            }
        }
    }
    dataset[, Xstr := as.numeric(sprintf("%0.6f", X))]

    if (input$heatmap_sort == "order") {
        dataset[ , e := factor(e, levels = 1:length(data$events[[input$heatmap_dataset]][[input$heatmap_series]]), ordered = T)]


    } else if (input$heatmap_sort == "length") {
        len = dataset[, .(.max = max(X)), by = .(e)]
        dataset[, e := factor(e, levels = len[order(.max), e], ordered = T)]

    } else if (input$heatmap_sort == "auc") {
        dataset[, e := factor(e, levels = aucs[order(auc), e], ordered = T)]
    }


     .gg = ggplot(dataset, aes(x = Xstr, y = e, fill = Y)) +
        geom_tile(aes(color = Y), lwd = 0.5) +
        (get(paste0("theme_", input$heatmap_theme)))(base_size = input$heatmap_font_size) +
        labs(x = "Event Time", y = NULL) +
        theme(legend.position = "bottom", text = element_text(size = input$heatmap_font_size, family = input$heatmap_font_family)) + scale_color_continuous(guide = guide_none())

     if (input$heatmap_palette == "gradient2") {
        .gg = .gg + scale_fill_gradientn(colors = c(input$heatmap_from, input$heatmap_to), guide = guide_colorbar(title = NULL))
        .gg = .gg + scale_color_gradientn(colors = c(input$heatmap_from, input$heatmap_to), guide = guide_colorbar(title = NULL))
     } else if (input$heatmap_palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
        .gg = .gg + scale_fill_viridis_c(option = input$heatmap_palette, guide = guide_colorbar(title = NULL))
        .gg = .gg + scale_color_viridis_c(option = input$heatmap_palette, guide = guide_colorbar(title = NULL))
     }

     .heatmap$plot_object = .gg

     output$heatmap_plot_static = renderPlot(.gg)

     ggplotly(.gg)  %>% config() %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))# %>% toWebGL2()
 })

.heatmap = reactiveValues()

observeEvent(plotly::event_data("plotly_relayout"), {
    b = plotly::event_data("plotly_relayout")
    req(length(b) > 0) # need at least some bounds to be updated
    req(.heatmap$plot_object)

    if (all(c("xaxis.range[0]", "xaxis.range[1]") %in% names(b))) {
        .heatmap$plot_object = .heatmap$plot_object + coord_cartesian(xlim = c(b$`xaxis.range[0]`, b$`xaxis.range[1]`))
    }

    if (all(c("yaxis.range[0]", "yaxis.range[1]") %in% names(b))) {
        .heatmap$plot_object = .heatmap$plot_object + coord_cartesian(ylim = c(b$`yaxis.range[0]`, b$`yaxis.range[1]`))
    }

    output$heatmap_plot_static = renderPlot(.heatmap$plot_object)
})

 observeEvent(input$heatmap_dataset, {
     updatePickerInput(session, "heatmap_series", choices = names(data$events[[input$heatmap_dataset]]))
 })

 observeEvent(input$heatmap_auc_type, {
     if (input$heatmap_auc_type == "range") {
        .min = Inf
        .max = -Inf
        for (s in input$heatmap_series) {
            .s = data$series[[input$heatmap_dataset]][[s]]
            for (e in 1:length(data$events[[input$heatmap_dataset]][[s]])) {
                .ed = data$events[[input$heatmap_dataset]][[s]][[e]]
                .max = max(.max, .ed[, max(`(event time)`, na.rm = T)])
                .min = min(.min, .ed[, min(`(event time)`, na.rm = T)])
            }
        }
        updateSliderInput(session, "heatmap_auc_range", min = .min, max = .max, value = c(.min, .max))
     }
 })
