# power density spectrum

# lag autocorrelation

output$power_plot = renderPlotly({
    req(input$power_dataset, input$power_series)

    dataset = data.table()
    values = data.table()
    refs = data.table()

    # for (s in input$lag_series) {
    #     .s = data$series[[input$lag_dataset]][[s]]
    #     ref.val = acf(data$raw[[input$lag_dataset]][, get(.s$responses[[1]])], lag.max = 1, plot = F, na.action = na.omit)$acf[[2,1,1]]
    #     refs = rbindlist(list(refs, data.table(s = s, acf1 = ref.val)))
    #
    #     for (e in 1:length(data$events[[input$lag_dataset]][[s]])) {
    #         .ed = data$events[[input$lag_dataset]][[s]][[e]][, .(s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]
    #
    #         if (!all(is.na(.ed$Y)) & nrow(.ed) > 1) {
    #             values = rbindlist(list(values, data.table(s = s, e = e, acf1 = acf(.ed$Y, lag.max = 1, plot = F, na.action = na.omit)$acf[[2,1,1]])))
    #         } else {
    #             printf("excluded event %d from %s due to incomplete dataset\n", e, s)
    #         }
    #     }
    # }


    for (s in input$power_series) {
        .s = data$series[[input$power_dataset]][[s]]

        .eps = 1 / (data$raw[[input$power_dataset]][2, `(time)`] - data$raw[[input$power_dataset]][1, `(time)`])

        ref = spectrum(data$raw[[input$power_dataset]][, get(.s$responses[[1]])], plot = F)
        refs = rbindlist(list(refs, data.table(s = s, X = ref$freq * .eps, Y = ref$spec)))

        for (e in 1:length(data$events[[input$power_dataset]][[s]])) {
            .ed = data$events[[input$power_dataset]][[s]][[e]][, .(s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]

            if (nrow(.ed) > 1) {

                tryCatch({
                    spec = spectrum(.ed[, Y], plot = F)
                    values = rbindlist(list(values, data.table(s = s, e = e, X = spec$freq * .eps, Y = spec$spec)))
                }, error = \(err) {
                    printf("error calling spectrum() on event %d of %s\n", e, s)
                })
            }
        }
    }

    values[, s := factor(s)]
    refs[, s := factor(s)]

    data$analysis[[input$power_dataset]]$power = values

     ggplotly({
         .gg = ggplot(values, aes(x = X, y = Y, color = s, group = e)) +
            geom_line(aes(x = X, y = Y), refs, size = 0.4, alpha = 0.2,  color = "black", inherit.aes = F) +
            geom_line(size = 0.3, alpha = 0.9) +

            (get(paste0("theme_", input$power_theme)))(base_size = input$power_font_size) +
            theme(legend.position = "bottom", text = element_text(size = input$power_font_size, family = input$power_font_family))

            if (input$power_log10_x) .gg = .gg + scale_x_log10()
            if (input$power_log10_y) .gg = .gg + scale_y_log10()

         .gg
     })  %>% config() %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25), xaxis = list(yaxis = list(tickmode = "auto"))) %>% toWebGL2()
})

observeEvent(input$power_dataset, {
    updatePickerInput(session, "power_series", choices = names(data$series[[input$power_dataset]]))
})

observeEvent(input$power_series, {

})
