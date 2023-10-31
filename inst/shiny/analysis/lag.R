# lag autocorrelation

output$lag_plot = renderPlot({
    req(input$lag_dataset, input$lag_series)

    dataset = data.table()
    values = data.table()
    refs = data.table()

    for (s in input$lag_series) {
        .s = data$series[[input$lag_dataset]][[s]]
        ref.val = acf(data$raw[[input$lag_dataset]][, get(.s$responses[[1]])], lag.max = 1, plot = F, na.action = na.omit)$acf[[2,1,1]]
        refs = rbindlist(list(refs, data.table(s = s, acf1 = ref.val)))

        for (e in 1:length(data$events[[input$lag_dataset]][[s]])) {
            .ed = data$events[[input$lag_dataset]][[s]][[e]][, .(s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]

            if (!all(is.na(.ed$Y)) & nrow(.ed) > 1) {
                values = rbindlist(list(values, data.table(s = s, e = e, acf1 = acf(.ed$Y, lag.max = 1, plot = F, na.action = na.omit)$acf[[2,1,1]])))
            } else {
                printf("excluded event %d from %s due to incomplete dataset\n", e, s)
            }
        }
    }

    values[, s := factor(s)]

    data$analysis[[input$lag_dataset]]$acf = values

     #ggplotly({
         .gg = ggplot(values, aes(x = s, y = acf1, fill = s)) +
            geom_boxplot(size = 0.25, outlier.shape = NA, position = position_dodge(width = 1)) +
            geom_point(size = 0.5, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 1)) +
            #geom_point(size = 0.5, position = position_jitter(width = 0.1)) +
            #geom_hline(aes(yintercept = acf1, color = s, linetype = "dotted"), refs) +
            (get(paste0("theme_", input$lag_theme)))(base_size = input$lag_font_size) +
            theme(legend.position = "bottom", text = element_text(size = input$lag_font_size, family = input$lag_font_family))

         if (input$lag_palette == "gradient2") {
            #.gg = .gg + scale_fill_stepsn(colors = c(input$lag_from, input$lag_to), guide = guide_colorsteps(title = NULL))
            .gg = .gg + scale_fill_manual(values = colorRampPalette(c(input$lag_from, input$lag_to))(values[, length(unique(s))]))
         } else if (input$lag_palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
            .gg = .gg + scale_fill_viridis_d(option = input$lag_palette, begin = 0.1, end = 0.9)
         }

         .gg
     #})  %>% config() %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(yaxis = list(tickmode = "auto"))) %>% toWebGL2()
})

observeEvent(input$lag_dataset, {
    updatePickerInput(session, "lag_series", choices = names(data$events[[input$lag_dataset]]))
})

observeEvent(input$lag_series, {

})
