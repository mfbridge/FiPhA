observeEvent(input$data_dataset, {
    updatePickerInput(session, "data_signal_var", choices = names(data$raw[[input$data_dataset]]))
    updatePickerInput(session, "data_corr_var2", choices = names(data$raw[[input$data_dataset]]))
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
                config(toImageButtonOptions = list(filename = "preview", format = input$plotly_format, height = input$plotly_height, width = input$plotly_width)) %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
                       xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))
        }
    }
})


observeEvent(input$data_corr_append, {
    .dt = data.table()
    min.t = data$raw[[input$data_dataset]][, min(get(data$meta[[input$data_dataset]]$time))]
    max.t = data$raw[[input$data_dataset]][, max(get(data$meta[[input$data_dataset]]$time))]
    n.bins = ceiling((max.t - min.t) / input$data_corr_window)
    t0 = min.t
    t = data$raw[[input$data_dataset]][, get(data$meta[[input$data_dataset]]$time)]

    est.dt = mean(na.omit(data$raw[[input$data_dataset]][, (get(data$meta[[input$data_dataset]]$time) - shift(get(data$meta[[input$data_dataset]]$time), n = 1))]))

    while (t0 < max.t & input$data_corr_resolution > 0) {
        subset.x = data$raw[[input$data_dataset]][(t0 <= get(data$meta[[input$data_dataset]]$time)) & (get(data$meta[[input$data_dataset]]$time) < t0 + input$data_corr_window), get(input$data_signal_var)]
        subset.y = data$raw[[input$data_dataset]][(t0 <= get(data$meta[[input$data_dataset]]$time)) & (get(data$meta[[input$data_dataset]]$time) < t0 + input$data_corr_window), get(input$data_corr_var2)]

        if (any(c(length(subset.x), length(subset.y)) < 0.5 * input$data_corr_window / est.dt)) {

            .dt = rbindlist(list(.dt, data.table(t = t0, correlation = NA)))
        } else {
            .dt = rbindlist(list(.dt, data.table(t = t0, correlation = cor(subset.x, subset.y, method = "pearson"))))
        }

        t0 = t0 + input$data_corr_resolution
    }

    # align with currently active dataset (optimized to only work on vectors)
    A = data$raw[[input$data_dataset]]
    At = data$meta[[input$data_dataset]]$time
    B = .dt
    Bt = "t"

    x = A[, get(At)]
    y = B[, get(Bt)]

    len.x = length(x)
    len.y = length(y)

    r = numeric(length(x))

    withProgress({
        j = 1
        for (i in 1:len.x) {
            while (j < len.y) {
                if (is.numeric(y[j+1]) & is.numeric(x[i])) {
                    if (abs(y[j+1] - x[i]) < abs(y[j] - x[i])) {
                        j = j + 1
                    } else {
                        break
                    }
                } else {
                    break
                }
            }
            if (j <= len.y) {
                r[i] = j
            } else {
                r[i] = NA # past end of file
            }
            if (i %% 10 == 0) incProgress(amount = floor(len.x / 10))
        }
    }, min = 0, max = len.x, message = "Appending...")

    #browser()
    data$raw[[input$data_dataset]][, (sprintf("corr(`%s`, `%s`, %0.0f s)", input$data_signal_var, input$data_corr_var2, input$data_corr_window)) := B[r, "correlation", with = F]]

    updateCurrentVariableSelections()
})

output$data_corr_plot = renderPlotly({
    if (input$data_dataset %in% names(data$meta)) {
        if (input$data_signal_var %in% names(data$raw[[input$data_dataset]]) & input$data_corr_var2 %in% names(data$raw[[input$data_dataset]])) {
            .f = input$data_dataset

            .dt = data.table()
            min.t = data$raw[[.f]][, min(get(data$meta[[input$data_dataset]]$time))]
            max.t = data$raw[[.f]][, max(get(data$meta[[input$data_dataset]]$time))]
            n.bins = ceiling((max.t - min.t) / input$data_corr_window)
            t0 = min.t
            t = data$raw[[.f]][, get(data$meta[[input$data_dataset]]$time)]
            while (t0 < max.t & input$data_corr_resolution > 0) {
                subset.x = data$raw[[.f]][(t0 <= get(data$meta[[input$data_dataset]]$time)) & (get(data$meta[[input$data_dataset]]$time) < t0 + input$data_corr_window), get(input$data_signal_var)]
                subset.y = data$raw[[.f]][(t0 <= get(data$meta[[input$data_dataset]]$time)) & (get(data$meta[[input$data_dataset]]$time) < t0 + input$data_corr_window), get(input$data_corr_var2)]

                .dt = rbindlist(list(.dt, data.table(t = t0, correlation = xcov(subset.x, subset.y, maxlag = 0, scale = "coeff")$C[[1]])))

                t0 = t0 + input$data_corr_resolution
            }


            .gg = ggplot(.dt, aes(x = t, y = correlation)) +
                geom_line(size = 0.2, color = "#f08040") +
                theme_bw(base_size = 9) +
                scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
                labs(x = "Time (s)", y = "Correlation Coefficient")

            .ggplotly = subplot(ggplotly(.gg), nrows = 1, titleX = T, titleY = T, margin = 0.05) %>%
                config(toImageButtonOptions = list(filename = "preview", format = input$plotly_format, height = input$plotly_height, width = input$plotly_width)) %>%
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

            tt = data$raw[[.f]][, .(t = get(data$meta[[.f]]$time))][, .(Dt = t - shift(t))][!is.na(Dt), mean(Dt)]

            plot.data = data.table(
                x = tt * ((1:length(.dt1)) - 1),
                y = .dt1
            )

            plot.data2 = data.table(
                x = tt * (-(1:length(.dt1) - 1)),
                y = .dt1
            )


            .gg1 = ggplot(rbindlist(list(plot.data, plot.data2)), aes(x = x, y = y)) +
                geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
                geom_line(size = 0.2, color = "#a02010") +
                theme_bw(base_size = 9) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                labs(x = "Lag (sec)", y = "Lag Autocorrelation")

            .ggplotly = subplot(ggplotly(.gg1), nrows = 1, titleX = T, titleY = T, margin = 0.05) %>%
                config(toImageButtonOptions = list(filename = "preview", format = input$plotly_format, height = input$plotly_height, width = input$plotly_width)) %>%
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
                config(toImageButtonOptions = list(filename = "preview", format = input$plotly_format, height = input$plotly_height, width = input$plotly_width)) %>%
                layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
                       xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto"))
        }
    }
})

binary_periods = function(x, t, inverted = F) {
    suppressWarnings({
        s = as.numeric(x)
    })

    s[which(is.na(s))] = 0
    s[which(is.null(s))] = 0

    assert_true(length(unique(s)) == 2)

    if (inverted) s = !s

    prev = shift(s, n=1, fill=0)
    nextv = shift(s, n=-1, fill=0)

    starts = which(s == 1 & prev == 0)
    ends = which(s == 1 & nextv == 0) + 1

    assert_true(length(starts) == length(ends))

    end_idx = pmin(ends-1, length(s))

    return(data.table(start = t[starts], end = t[end_idx]))

}

binary2shapes = function(dt, t, y, color, alpha) {
    shapes = list()
    intervals = binary_periods(dt[, get(..t)], dt[, get(y)])

    for (p in 1:nrow(intervals)) {
        shapes[[p]] = list(
            type = "vrect",
            fillcolor = color, opacity = alpha,
            line = list(color = color, opacity = alpha, width = 0),
            x0 = intervals[p, start], x1 = intervals[p, end],
            y0 = 0, y1 = 1.0,
            xref = "x", yref = "y domain",
            layer = "below"
        )
    }

    return(shapes)
}


output$data_plot = renderPlotly({
    req(input$data_plot_x, input$data_plot_y, input$data_dataset %in% names(data$meta), input$data_plot_y_open == F)

    #withProgress({
        #setProgress( message = "Collecting data...", value = 0.25)

        shade.binary = "shade_binary" %in% input$data_plot_options
        color.ticks = "y2_color" %in% input$data_plot_options

        .name = input$data_dataset # input$tree_selected[[1]]$data$name
        #.cols = names(data$raw[[.name]]) # data_columns(state$active, .name)
        .time = input$data_plot_x # data_time(state$active, .name)
        .data = copy(data$raw[[.name]]) # copy(data_table(state$active, .name))
        .preview$dt = cbind(.data, data.table(.empty = rep(Inf, nrow(.data))))

        #setProgress(message = "Building plot...", value = 0.75)

        shapes = list()

        pal1index = 1
        pal2index = 1

        is.binary = lapply(input$data_plot_y, \(c) (length(unique(.data[, get(c)])) == 2) | (length(unique(.data[, get(c)])) == 3 & sum(is.na(unique(.data[, get(c)])) == 1)))

        if (input$data_scale_y_y2) {
            for (.yi in 1:length(input$data_plot_y)) {
                var = input$data_plot_y[[.yi]]
                if (!is.binary[[.yi]]) {
                    .preview$dt[, (var) := (get(var) - first(get(var), na_rm = T)) / first(get(var), na_rm = T)]
                }
            }
        }

        figure = plot_ly(
            data = .preview$dt,
            x = ~ get(input$data_plot_x),
            y = ~ get(input$data_plot_y[[1]]),
            name = input$data_plot_y[[1]],
            yaxis = "y",
            type = "scatter",
            mode = "lines",
            visible = ifelse(shade.binary, !is.binary[[1]], T),
            line = list(color = palette_index(input$pal1, pal1index, viridis.max = length(input$data_plot_y)), width = input$data_plot_line_width)
        )

        # user might have selected a binary variable first
        if (shade.binary & is.binary[[1]]) {
            figure = figure |>
                add_trace(x = ~get(input$data_plot_x), y = ~.empty,
                    type = "scatter", mode = "markers",
                    marker = list(color = palette_index(input$pal2, pal2index, viridis.max = length(input$data_plot_y)), symbol = "square"),
                    name = input$data_plot_y[[1]], inherit = F)
            shapes = append(shapes, binary2shapes(.preview$dt, input$data_plot_y[[1]], .time, palette_index(input$pal2, pal2index, viridis.max = length(input$data_plot_y)), input$pal2alpha))
            pal2index = pal2index + 1
        } else {
            pal1index = pal1index + 1
        }

        if ("title" %in% input$data_plot_options) {
        figure = figure |> layout(
            title = list(
                text = sprintf("<b>%s</b>", .name),
                font = list(size = input$data_font_size * 1.5, family = input$data_font_family)
            ),
            margin = list(
                t = 50
            )
        )
        }

        wx = ifelse("y2" %in% input$data_plot_options, input$data_plot_y2space, 0)
        #if ("x" %in% input$elements) {
            figure = figure |> layout(
                xaxis = list(tickmode = "auto", nticks = 15, domain = c(0, 1.0 - wx * (length(input$data_plot_y)-sum(unlist(is.binary))-1)),
                    tickfont = list(size = input$data_font_size),
                    tickcolor = "#000000",
                    title = ifelse(input$data_plot_x == "(time)", "<b>Time, sec</b>", sprintf("<b>%s</b>",input$data_plot_x)),
                    titlefont = list(size = input$data_font_size, family = input$data_font_family),
                    showgrid = "x_grid" %in% input$data_plot_options
                )
            )
        # } else {
        #     figure = figure |> layout(xaxis = list(visible = F))
        # }

        #if ("y" %in% input$elements) {
            figure = figure |> layout(
                yaxis = list(tickmode = "auto", nticks = 15, visible = ifelse(shade.binary & is.binary[[1]], F, T),
                    tickfont = list(size = 12),
                    tickcolor = "#000000",
                    title = sprintf("<b>%s</b>", input$data_plot_y[[1]]), automargin = T,
                    titlefont = list(size = input$data_font_size, family = input$data_font_family),
                    #range = c(0.25, 0.75),
                    showgrid = "y_grid" %in% input$data_plot_options
                )
            )
        # } else {
        #     figure = figure |> layout(yaxis = list(visible = F))
        # }

        # base figure
        figure = figure |>
            config(responsive = T, toImageButtonOptions = list(format = input$plotly_format, filename = .name, height = 720, width = 1280, scale = 1)) |>
            layout(
                legend = list(itemsizing = "constant", orientation = "h", xanchor = "center", yanchor = "center", x = 0.5 - 0.075 * (length(input$data_plot_y)-sum(unlist(is.binary))-1)/2, xref = "container", font = list(size = 12))
                # font = list(
                #     family = state$pref("plot_font")
                # )
            )

        if (input$data_scale_y_y2) {
            figure = figure |> layout(yaxis = list(tickformat = ".1%"))
        }

        binary.count = Reduce(sum, is.binary, 0)

        # programmatically add other traces
        if (length(input$data_plot_y) > 1) {
            figure = figure %>% layout(margin = list(r = 0))

            idx = 2
            for (y2i in 2:length(input$data_plot_y)) {
                if (shade.binary & is.binary[[idx]]) {
                    # create shape list and append for a binary variable
                    figure = figure |>
                        add_trace(x = ~get(input$data_plot_x), y = ~.empty,
                            type = "scatter", mode = "markers",
                            marker = list(color = palette_index(input$pal2, pal2index, viridis.max = binary.count), symbol = "square"),
                            name = input$data_plot_y[[y2i]], inherit = F)
                    shapes = append(shapes, binary2shapes(.preview$dt, input$data_plot_y[[y2i]], .time,
                                palette_index(input$pal2, pal2index, viridis.max = binary.count), input$data_plot_area_opacity))
                    pal2index = pal2index + 1


                } else {

                    # build parameters for a function call for a regular trace
                    call.par = list(
                        p = figure,
                        x = .preview$dt[, get(input$data_plot_x)],
                        y = .preview$dt[, get(input$data_plot_y[[y2i]])],
                        yaxis = paste0("y", idx),
                        name = input$data_plot_y[[y2i]],
                        visible = T,
                        type = "scatter", mode = "lines", line = list(width = input$data_plot_line_width, color = palette_index(input$pal1, pal1index, viridis.max = length(input$data_plot_y)))
                    )

                    # make add_trace() call
                    figure = do.call(add_trace, call.par)

                    # are all of these necessary?
                    # TODO: tickmode = "sync" is only in plotly.js versions 2.18+, but the current plotly CRAN package still uses 2.11
                    wx = ifelse("y2" %in% input$data_plot_options, input$data_plot_y2space, 0)


                    args = setNames(
                        list(
                            figure,
                            list(tickmode = "sync", nticks = 15, side = "right", overlaying = "y",
                                tickfont = list(size = input$data_font_size, family = input$data_font_family),
                                color = ifelse(color.ticks, palette_index(input$pal1, pal1index, viridis.max = length(input$data_plot_y)), "#000000"),
                                tickcolor = ifelse(color.ticks, palette_index(input$pal1, pal1index, viridis.max = length(input$data_plot_y)), "#000000"),
                                title = sprintf("<b>%s</b>", input$data_plot_y[[y2i]]), automargin = T,
                                titlefont = list(size = input$data_font_size),
                                showgrid = "y2_grid" %in% input$data_plot_options,
                                position = 1.0 - wx*(pal1index-1), anchor = "free",
                                visible = "y2" %in% input$data_plot_options
                            ) # scaleanchor = "y" toggle?
                        ),
                        c("p", paste0("yaxis", idx)))

                    if (input$data_scale_y_y2) {
                        args[[2]]$tickformat = ".1%"
                        View(args)
                    }

                    if ("ignore_binary_axes" %in% input$data_plot_options) {
                        if (!is.binary[[idx]]) {
                            figure = do.call(layout, args)
                        } else {
                            #figure = do.call(layout, args)
                        }
                    } else {
                        figure = do.call(layout, args)
                    }
                    pal1index = pal1index + 1
                }


                idx = idx + 1
            }
        }

        figure = figure |> layout(shapes = shapes) |> layout(font = list(family = "Arial"))

        # convert to webgl element if necessary
        # faster drawing for large number of points, but plotly still complains about not rendering in RStudio even though it does
        #if (state$pref("plot_webgl")) {
            figure = figure |> toWebGL()
        #}

            #browser()

        figure = figure |> plotly_build() %>% event_register("plotly_relayout")

        # resume interaction events after building plot (avoids warning messages about unregistered plotly events)
        #observe_doubleclick$resume()
        #observe_relayout$resume()

        #figure$x$data[[1]]$name = input$y[[1]]

        .preview$ds = downsampler$new(figure = figure,
            aggregator = nth_pnt_aggregator2$new(),
            n_out = 1e5
        )
    #})

    .preview$ds$figure

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
      interleave_gaps, coef_gap, NA_position
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
