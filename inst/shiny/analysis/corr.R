
# virtualSelectInput("summary_corr_events", "Dataset/Event(s)", choices = c(), multiple = T, optionHeight = "24rem", width = "100%"),
# virtualSelectInput("summary_corr_x", "X Variable", choices = c(), multiple = T, placeholder = "X", optionHeight = "24rem", width = "100%"),
# virtualSelectInput("summary_corr_y", "Y Variable", choices = c(), multiple = T, placeholder = "Y", optionHeight = "24rem", width = "100%"),
# numericInput("summary_corr_window", "Window (sec)", 30, min = 0, step = 0.01),
# numericInput("summary_corr_resolution", "Res. (sec)", 30, min = 0, step = 0.01),
# numericInput("summary_corr_maxlag", "Max Lag (N)", 0, min = 0, step = 1)
# plotlyOutput("summary_corr_plot", fill = T)

acorr = reactiveValues(
    common.variables = c(),
    plot.data = data.table()
)

observe({
    req(length(names(data$series)) > 0)

    value.list = list()
    series.list = lapply(setNames(names(data$series), names(data$series)), \(f) {
        if (length(names(data$series[[f]])) > 0) {
            raw.values = paste0(f, "!!", names(data$series[[f]]))
            raw.values = setNames(raw.values, names(data$series[[f]]))
            value.list = append(value.list, raw.values)
        } else {
            NULL
        }
    })


    updateVirtualSelect("summary_corr_events", choices = series.list, selected = NULL)
})

observeEvent(input$summary_corr_events, {

    variable.list = list()
    for (ds in input$summary_corr_events) {
        d = str_split(ds, "!!")[[1]][[1]]
        s = str_split(ds, "!!")[[1]][[2]]

        if (length(data$events[[d]][[s]])>0) {
            variable.list = append(variable.list, list(as.list(colnames(data$events[[d]][[s]][[1]]))))
        }
    }
    valid.variables = Reduce(intersect, variable.list)

    #browser()

    updateVirtualSelect("summary_corr_x", choices = valid.variables, selected = input$summary_corr_x)
    updateVirtualSelect("summary_corr_y", choices = valid.variables, selected = input$summary_corr_y)
})

observeEvent(c(input$summary_corr_events, input$summary_corr_x, input$summary_corr_y, input$summary_corr_window, input$summary_corr_resolution, input$summary_corr_maxlag), {
    req(input$summary_corr_x)
    req(input$summary_corr_y)
    req(input$summary_corr_events)

    corr.data = data.table()

    shiny::withProgress({
        for (ds in input$summary_corr_events) {
            d = str_split(ds, "!!")[[1]][[1]]
            s = str_split(ds, "!!")[[1]][[2]]

            if (length(data$events[[d]][[s]])>0) {
                for (i in 1:length(data$events[[d]][[s]])) {
                    shiny::setProgress(value = i / length(data$events[[d]][[s]]), message = d, detail = s)

                    X = data$events[[d]][[s]][[i]][, get(input$summary_corr_x)]
                    X = X - shift(X, n = 1)
                    Y = data$events[[d]][[s]][[i]][, get(input$summary_corr_y)]
                    Y = Y - shift(Y, n = 1)
                    t = data$events[[d]][[s]][[i]][, `(event time)`]
                    t.var = "(event time)"
                    min.t = min(t)
                    max.t = max(t)
                    n.bins = ceiling((max.t - min.t) / input$summary_corr_window)
                    t0 = min.t
                    est.dt = t[2] - t[1]
                    .dt = data.table()
                    while (t0 < max.t & input$summary_corr_resolution > 0) {
                        subset.x = data$events[[d]][[s]][[i]][(t0 <= get(t.var)) & (get(t.var) < t0 + input$summary_corr_window), get(input$summary_corr_x)]
                        subset.y = data$events[[d]][[s]][[i]][(t0 <= get(t.var)) & (get(t.var) < t0 + input$summary_corr_window), get(input$summary_corr_y)]

                        if (any(c(length(subset.x), length(subset.y)) < 0.5 * input$summary_corr_window / est.dt)) {
                            .dt = rbindlist(list(.dt, data.table(t = t0, correlation = NA)))
                        } else {
                            .dt = rbindlist(list(.dt, data.table(t = t0, correlation = cor(subset.x, subset.y, method = "pearson")**2)))
                        }

                        t0 = t0 + input$summary_corr_resolution
                    }

                    corr.data = rbindlist(list(corr.data, data.table(.dt, `[Event]`=i,`[Series]`=s,`[Dataset]`=d)))
                }
            }
        }
    })

    output$summary_corr_plot = renderPlot({
        ggplot(corr.data, aes(x = t, y = correlation, group = `[Event]`)) +
            geom_line(linewidth = 0.5, alpha = 0.8) +
            facet_grid(. ~ `[Series]`) +
            theme_minimal(16) +
            labs(x = "Event Time, s", y = bquote(R^2))

    })

    output$summary_corr_table = renderTable({
        corr.data[, .(mean = mean(correlation, na.rm = T), min = min(correlation, na.rm = T), q25 = quantile(correlation, 0.25, na.rm = T), median = median(correlation, na.rm = T), q75 = quantile(correlation, 0.75, na.rm = T), max = max(correlation, na.rm = T)), by = c("[Dataset]", "[Series]")]
    })

    acorr$plot.data = corr.data
})

output$summary_corr_download = downloadHandler("event correlations.csv", \(f) {
    write_csv(acorr$plot.data, f)
})
