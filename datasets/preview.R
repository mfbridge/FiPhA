observeEvent(input$data_dataset, {
    updatePickerInput(session, "data_signal_var", choices = names(data$raw[[input$data_dataset]]))
})

signal.values = reactiveValues()

output$data_signal_dl_lag = downloadHandler(filename = \() { "lag autocorrelation.xlsx" } , content = \(file) { write_xlsx(signal.values$lag, file) })
output$data_signal_dl_pow = downloadHandler(filename = \() { "power density spectrum.xlsx" } , content = \(file) { write_xlsx(signal.values$pow, file) })


output$data_spectro_plot = renderPlotly({
    if (input$data_dataset %in% names(data$meta)) {
        if (input$data_signal_var %in% names(data$raw[[input$data_dataset]])) {
            .f = input$data_dataset

            spec = specgram(x = data$raw[[.f]][, get(input$data_signal_var)], n = as.numeric(input$data_power_size), fs = 1 / (data$raw[[.f]][2, `(time)`] - data$raw[[.f]][1, `(time)`]), overlap = 0)

            P = t(abs(spec$S))
            P = P / max(P)
            P = 20 * log10(P)

            .dt = data.table(y = rep(spec$f, each = nrow(P)), x = rep(spec$t, ncol(P)), z = as.vector(P))

            .gg = ggplot(.dt, aes(x = x, y = y, fill = z)) +
                geom_raster() +
                scale_fill_viridis_c(option = "magma", guide = guide_colorbar(title = "Power (dB)", barheight = 12, barwidth = 0.5)) +
                theme_bw(base_size = 9) +
                theme() +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                labs(x = "Time (s)", y = "Frequency (Hz)")

            .ggplotly = subplot(ggplotly(.gg), nrows = 1, titleX = T, titleY = T, margin = 0.05) %>%
                config() %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
                       xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))
        }
    }
})

output$data_lag_plot = renderPlotly({
    if (input$data_dataset %in% names(data$meta)) {
        if (input$data_signal_var %in% names(data$raw[[input$data_dataset]])) {
            .f = input$data_dataset
            try({
                .dt1 = acf(data$raw[[.f]][, get(input$data_signal_var)], lag.max = input$data_lag_max, plot = F)$acf[,,1]
            })

            signal.values$lag = data.table(Dataset = input$data_dataset, Lag = (1:length(.dt1)) - 1, Autocorrelation = .dt1)

            .gg1 = ggplot(data.table(x = (1:length(.dt1)) - 1, y = .dt1), aes(x = x, y = y)) +
                geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
                geom_line(size = 0.2, color = "#a02010") +
                theme_bw(base_size = 9) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                labs(x = "Lag", y = "Lag-n Autocorrelation")

            .ggplotly = subplot(ggplotly(.gg1), nrows = 1, titleX = T, titleY = T, margin = 0.05) %>%
                config() %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
                       xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))
        }
    }
})

output$data_power_plot = renderPlotly({
    if (input$data_dataset %in% names(data$meta)) {
        if (input$data_signal_var %in% names(data$raw[[input$data_dataset]])) {
            .f = input$data_dataset
            try({
                .dt2 = gsignal::pwelch(data$raw[[.f]][, get(input$data_signal_var)], detrend="none", fs = 1 / (data$raw[[.f]][2, `(time)`] - data$raw[[.f]][1, `(time)`]))
            })

            signal.values$pow = data.table(Dataset = input$data_dataset, Frequency = .dt2$freq, Power = gsignal::pow2db(.dt2$spec))

            .gg2 = ggplot(data.table(x = .dt2$freq, y = gsignal::pow2db(.dt2$spec)), aes(x = x, y = y)) +
                geom_line(size = 0.2, color = "#1020a0") +
                theme_bw(base_size = 9) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                labs(x = "Frequency (Hz)", y = "Power (dB)")

            .ggplotly = subplot(ggplotly(.gg2), nrows = 1, titleX = T, titleY = T, margin = 0.05) %>%
                config() %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
                       xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))
        }
    }
})

output$data_plot = renderPlotly({
    if (input$data_dataset %in% names(data$meta)) {
        name = input$data_dataset
        use.y2 = length(input$data_plot_y) > 1

        y.range = NULL

        if (!is.null(input$data_plot_x) & (length(input$data_plot_y) > 0)) {
            if (use.y2) {
                .dt = data$raw[[name]][, .(X = get(input$data_plot_x), Y = get(input$data_plot_y[1]), Y2 = get(input$data_plot_y[2]))]
            } else {
                .dt = data$raw[[name]][, .(X = get(input$data_plot_x), Y = get(input$data_plot_y[1]))]
            }

            if (input$data_scale_y_y2) {
                y.100 = .dt[!is.na(Y), Y][[1]]
                if (use.y2) {
                    y2.100 = .dt[!is.na(Y2), Y2][[1]]

                    .dt[, Y2 := 100 * (Y2 / y2.100)]
                    .dt[, Y := 100 * (Y / y.100)]
                } else {
                    .dt[, Y := 100 * (Y / y.100)]
                }
            }

            .preview$ds = NULL

            .gg = ggplot(.dt, aes(x = X)) +
                geom_path(aes(y = Y, color = input$data_plot_y[1]), size = input$data_y_size) +
                (get(paste0("theme_", input$data_theme)))(base_size = input$data_font_size) +
                labs(x = ifelse(input$data_plot_x == "(time)", "Time", input$data_plot_x), y = NULL) + #paste0(input$data_plot_y, collapse = ", ")) +
                scale_color_manual(values = c(input$data_y_color, input$data_y2_color), guide = guide_legend(title = NULL)) +
                theme(legend.position = "bottom", text = element_text(size = input$data_font_size, family = input$data_font_family))

            .ggplotly = ggplotly(.gg) %>%
                config(toImageButtonOptions = list(format= 'svg')) %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = 1, xref = "container", yref = "container"),
                       xaxis = list(title = ifelse(input$data_plot_x == "(time)", "Time", input$data_plot_x), tickmode = "auto"), yaxis = list(title = input$data_plot_y[1], tickmode = "auto"))

            if (use.y2) {
                .ggplotly = .ggplotly %>% add_trace(x = .dt[, X], y = .dt[, Y2], name=input$data_plot_y[2], yaxis= ifelse(input$data_scale_y_y2, "y", "y2"), mode="lines", type = "scatter",
                        line=list(color = input$data_y2_color, width = input$data_y2_size*2))

                if (!input$data_scale_y_y2) {
                    .ggplotly = .ggplotly %>%
                        layout(margin = list(t = 0, b = 0, l = 80, r = 80),
                            yaxis2 = list(title = list(text = input$data_plot_y[2], font = list(color = input$data_y2_color)), side = "right", overlaying = "y", anchor = "free", position = 1, tickfont = list(family = input$data_font_family, size = input$data_font_size, color = input$data_y2_color))) #%>%
                        #layout(annotations = list(x = 0, y = -0.1, showarrow=F, font=list(size = 9, color = "red"), text = "Note: Plotting two traces with different scales on the same graph may be misleading.", xref = "paper", yref = "paper"))
                }
            }

            if (input$data_scale_y_y2) {
                .ggplotly = .ggplotly %>% layout(yaxis = list(ticksuffix= " %"))
            }

            fig = .ggplotly %>% toWebGL2() %>% plotly_build() %>% event_register("plotly_relayout")


            ds = downsampler$new(figure = fig,
                aggregator = nth_pnt_aggregator2$new(),
                n_out = 10000)

            .preview$ds = ds

            ds$figure
        }
    } else {
        plotlyMessage("Select an imported dataset and variable(s) to plot.", F)
    }
})

.preview = reactiveValues()

observeEvent(plotly::event_data("plotly_relayout"), {
    b = plotly::event_data("plotly_relayout")
    req(length(b) > 0) # need at least some bounds to be updated
    req(.preview$ds)
    updatePlotlyH(session, "plot", plotly::event_data("plotly_relayout"), .preview$ds)
})


nth_pnt_aggregator2 <- R6::R6Class(
  "nth_pnt_aggregator2",
  inherit = null_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position, accepted_datatype
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      idx <- seq(1, length(x), max(1, ceiling(length(x) / n_out)))
      return(list(x = x[idx], y = y[idx]))
    }
  )
)
