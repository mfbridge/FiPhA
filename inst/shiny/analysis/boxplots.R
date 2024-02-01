observeEvent(input$summary_model_type, {
    if (input$summary_model_type == "one") {
        output$summary_model_info = renderUI("")
    } else if (input$summary_model_type == "two") {
        output$summary_model_info = renderUI(HTML("<b>Note</b>: Two fixed effects must be specified."))
    } else if (input$summary_model_type == "rep") {
        output$summary_model_info = renderUI(HTML("<b>Note</b>: At least one random effect must be specified."))
    }
})

observeEvent(input$summary_model_ss, {
    if (input$summary_model_ss == "I") {
        output$summary_ss_info = renderUI("Type I sums of squares are not usually of interest.")
    } else if (input$summary_model_ss == "II") {
        output$summary_ss_info = renderUI("Type II sums of squares have better statistical power than type III when interactions are not significant. Group means are weighted by their sample size.")
    } else if (input$summary_model_ss == "III") {
        output$summary_ss_info = renderUI("Type III sums of squares are appropriate under most circumstances. Without an interaction term, they are equivalent to type II. Group sizes are assumed to be equal.")
    }
})

# TODO: add comparison lines to plots
add_test_pvalues = function(plot, pvals = data.table(from = c(), to = c(), p = c())) {
    shapes = list()
    for (p in 1:nrow(pvals)) {
        shapes = append(shapes, list(
            type = "line",
            xref = "x",
            yref = "paper",
            x0 = pvals[p, from],
            y0 = (p-1)*0.01,
            x1 = pvals[p, from],
            y1 = (p)*0.01,
            size = 1.0
        ))
        shapes = append(shapes, list(
            type = "line",
            xref = "x",
            yref = "paper",
            x0 = pvals[p, to],
            y0 = 1.0+(p-1)*0.01,
            x1 = pvals[p, to],
            y1 = 1.0+(p)*0.01,
            size = 1.0
        ))
        shapes = append(shapes, list(
            type = "line",
            xref = "x",
            yref = "paper",
            x0 = pvals[p, from],
            y0 = 1.0+(p)*0.01,
            x1 = pvals[p, to],
            y1 = 1.0+(p)*0.01,
            size = 1.0
        ))
    }
    plot = plot %>% layout(shapes = shapes)
}

output$summary_pkg = DT::renderDT({
    p = c("stats", "lme4", "car")

    v = data.table(Name = p, Version = Map(packageVersion, p))

    DT::datatable(v, options = list(
            dom = 't'
        )

    )
})

observe({
    if (input$summary_model_type %in% c("one", "two")) {
        shinyjs::hide("summary_interaction_terms")
        shinyjs::hide("summary_random_effects")
    } else {
        shinyjs::show("summary_interaction_terms")
        shinyjs::show("summary_random_effects")
    }
})

# auto-update plot variables to something sensible when modifying model
observeEvent(c(input$summary_series, input$summary_fixed_effects, input$interaction_terms, input$random_effects, input$summary_model_type), {
    req(length(input$summary_fixed_effects) > 0)

    fixed = input$summary_fixed_effects
    ints = c()
    rand = input$summary_random_effects

    # calculate interaction terms
    for (a in 1:length(fixed)) {
        for (b in a:length(fixed)) {
            if (a != b) {
                ints = append(ints, c(sprintf("`%s`:`%s`", fixed[[a]], fixed[[b]])))
            }
        }
    }
    updateVirtualSelect("summary_interaction_terms", choices = ints, selected = NULL)

    if (input$summary_model_type == "one" & length(fixed) == 1) {
        updateVirtualSelect("summary_plot_color", selected = NULL)
        updateVirtualSelect("summary_plot_facet", selected = NULL)
    }
    if (input$summary_model_type == "two" & length(fixed) == 2) {
        updateVirtualSelect("summary_plot_color", selected = fixed[[2]])
        updateVirtualSelect("summary_plot_facet", selected = NULL)
    }
    if (input$summary_model_type == "rep" & length(fixed) > 0 & length(rand) > 0) {
        req(length(rand) > 0)

        if (length(fixed) == 2) {
            updateVirtualSelect("summary_plot_color", selected = fixed[[2]])
            updateVirtualSelect("summary_plot_facet", selected = rand[[1]])

        } else if (length(rand) == 1) {
            updateVirtualSelect("summary_plot_color", selected = rand[[1]])
            updateVirtualSelect("summary_plot_facet", selected = NULL)
        }
    }
})


output$summary_boxplot = renderPlotly({
    dataset = data.table()
    values = data.table()

    req(input$summary_series)
    req(length(input$summary_series) > 0)

    if (input$summary_function == "mean") fun = mean
    else if (input$summary_function == "median") fun = median
    else if (input$summary_function == "auc") fun = auc

    for (sel in input$summary_series) {

        f = str_split(sel, "!!")[[1]][[1]]
        s = str_split(sel, "!!")[[1]][[2]]

        .s = data$series[[f]][[s]]
        #browser()
        for (e in 1:length(data$events[[f]][[s]])) {
            .ed = data$events[[f]][[s]][[e]]

            for (.i in .s$intervals$name) {
                .ss = .ed[`(interval)` == .i, .(f = f, s = s, e = e, i = `(interval)`, X = `(event time)`, Y = get(.s$responses[[1]]))]

                .is = data.table(
                    Dataset = as.factor(f),
                    Series = as.factor(s),
                    `Event #` = as.factor(e),
                    Interval = as.factor(.i),
                    `Length (s)` = max(.ss$X) - min(.ss$X) + (.ss$X[[2]]-.ss$X[[1]])
                )

                if (input$summary_function == "mean") {
                    values = rbindlist(list(values, data.table(.is, value = mean(.ss$Y, na.rm = T))))
                } else if (input$summary_function == "median") {
                    values = rbindlist(list(values, data.table(.is, value = median(.ss$Y, na.rm = T))))
                } else if (input$summary_function == "auc") {
                    values = rbindlist(list(values, data.table(.is, value = auc(.ss$Y, .ss$X, na.rm = T))))
                }
            }
        }
    }

    output$summary_download_csv = downloadHandler(
        filename = sprintf("%s interval summaries.csv", input$summary_function),
        content = \(file) {
            write_csv(values, file, na = "")
        }
    )

    output$summary_download_xlsx = downloadHandler(
        filename = sprintf("%s interval summaries.xlsx", input$summary_function),
        content = \(file) {
            write_xlsx(values, file, col_names = T, format_headers = T)
        }
    )

    model = NULL
    error = NULL
    fixed = input$summary_fixed_effects
    ints = input$summary_interaction_terms
    rand = input$summary_random_effects

    # one, two-way, and RM anova are just special cases of more general linear mixed models
    if (input$summary_model_type == "one") {
        req(length(input$summary_fixed_effects) == 1)
        fixed = fixed[c(1)]
        ints = c()
        rand = c()

    } else if (input$summary_model_type == "two") {
        req(length(input$summary_fixed_effects) == 2)
        fixed = fixed[c(1, 2)]
        ints = ints
        rand = c()

    } else if (input$summary_model_type == "rep") {
        # no limits on terms
        req(length(fixed) > 0)
        req(length(rand) > 0)
    }

    if (length(fixed) > 0) {
        ms = sprintf("value ~ %s", paste(paste0("`", fixed, "`"), collapse = "+"))

        if (length(ints) > 0) {
            ms = sprintf("%s + %s", ms, paste(ints, collapse = "+"))
        }

        if (length(rand) > 0) {
            rand = sprintf("1|`%s`", rand)
            ms = sprintf("%s + %s", ms, paste(rand, collapse = "+"))
        }

        mf = as.formula(ms)

        tryCatch({
            if (input$summary_model_type %in% c("one", "two")) {
                model = lm(mf, data = values)

                output$summary_qqplot = renderPlot({
                    .aes = aes(sample = value)
                    if (length(input$summary_plot_facet) == 1) {
                        .aes$colour = aes(color = get(input$summary_plot_facet))[[1]]
                        .aes$fill = aes(fill = get(input$summary_plot_facet))[[1]]
                        .aes$group = aes(group = get(input$summary_plot_facet))[[1]]
                    }

                    # QQ Plot
                    .gg = ggplot(values, .aes) +
                        stat_qq_band(alpha = 0.1, bandType = input$summary_qq_bands) +
                        stat_qq_line(linewidth = 1, alpha = 0.5) +
                        stat_qq_point(size = 1, shape = 15, alpha = 0.9) +
                        scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                        scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                        theme_minimal(11) +
                        theme(legend.position = "bottom", plot.title.position = "panel", plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
                        labs(x = "Theoretical Quantile", y = "Sample Quantile", title = "Quantile-Quantile Plot")

                    .gg
                })

                output$summary_residual_vfit = renderPlot({
                    .gg = ggplot(data = data.table(x = fitted(model), y = residuals(model)), aes(x = x, y = y)) +
                        geom_hline(yintercept = 0, linewidth = 1, alpha = 0.5) +
                        geom_point(size = 1, shape = 15, alpha = 0.9) +
                        theme_minimal(11) +
                        theme(legend.position = "bottom", plot.title.position = "panel", plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
                        labs(x = "Fitted Value", y = "Residual", title = "Residuals vs Fitted Values")

                    #if (length(values[, unique(get(fixed[[1]]))]) > 2) {
                        .gg = .gg +  stat_smooth(method = "loess", fill = NA, linewidth = 1, alpha = 0.5, color = "red")
                    #}

                    .gg
                })


                output$summary_residual_dist = renderPlot({
                    .gg = ggplot(data = data.table(x = residuals(model)), aes(x = x)) +
                        geom_histogram(fill = "black") +
                        theme_minimal(11) +
                        theme(legend.position = "bottom", plot.title.position = "panel", plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
                        labs(x = "Residual Value", y = "Count", title = "Residual Distribution")

                    .gg
                })

            } else if (input$summary_model_type %in% c("rep")) {
                model = lme4::lmer(mf, data = values, REML = "reml" %in% input$summary_options)
            }
        }, warning = \(w) {
            error = toString(w)

        }, error = \(e) {
            output$summary_model = renderPrint({
                print(e)
            })
        })

        if (!is.null(model)) {
            output$summary_summary = renderText({
                    str = ""
                    str = paste0(str, sprintf("Model: %s\n", deparse1(mf)))
                    str = paste0(str, sprintf("\nAIC: %f\n", AIC(model)))
                    if (input$summary_model_type == "rep") str = paste0(str, " * see \u00b9 in model options when comparing this value\n")

                    str
            })

            output$summary_model = renderPrint({
                    print(summary(model))
            })

            output$summary_anova = renderPrint({
                if (input$summary_model_ss == "I") {
                    print(anova(model))

                } else {
                    print(car::Anova(model, type = input$summary_model_ss))
                }
            })
        }


        X = fixed[[1]]

        .gg = NULL
        .plots = list()

        if (input$summary_model_type %in% c("one", "two", "rep")) {
            if (!is.null(input$summary_plot_color)) {
                base.colors = viridis::viridis_pal(option = input$summary_plot_palette)(length(unique(values[, get(input$summary_plot_color)])))
            }

            facets = c()

            if (length(input$summary_plot_facet) == 1) {
                facets = unique(values[, get(input$summary_plot_facet)])
            } else {
                facets = c(NA)
            }

            for (f in 1:length(facets)) {
                if (!is.na(facets[[f]])) {
                    .values = values[get(input$summary_plot_facet) == facets[[f]], ]
                } else {
                    .values = values
                }

                .plot = plot_ly(.values)

                # boxplot trace options
                trace.opts = list(
                    p = .plot,
                    type = "box",
                    x = ~get(X),
                    y = ~value,
                    line = list(color = "#000000ff", width = 1.5),
                    jitter = 0.25,
                    boxmean = T,
                    boxpoints = "all",
                    pointpos = 0,
                    marker = list(size = 5, symbol = 'square', line = list(width = 1, color = '#00000080')),
                    text = .values[, sprintf("<b>Dataset</b>: %s\n<b>Series</b>: %s\n<b>Event</b>: %d\n<b>Interval</b>: %s", Dataset, Series, `Event #`, Interval)]
                )

                if (!is.na(facets[[f]])) {
                    trace.opts$name = facets[[f]]
                }

                if (!is.null(input$summary_plot_color)) {
                    trace.opts$color = ~get(input$summary_plot_color)
                    trace.opts$colors = base.colors

                } else {
                    trace.opts$fillcolor = "#00000080"
                    trace.opts$marker$color = "#000000ff"
                }

                .plot = do.call(add_trace, trace.opts)

                if (length(input$summary_plot_facet) == 1) {
                    .plot = .plot %>% add_annotations(x = 0.5, y = 1, xanchor = "center", yanchor = "bottom", xref = "paper", yref = "paper", showarrow = F, text = facets[[f]])
                }

                # layout options
                layout.opts = list(
                    p = .plot,
                    boxgap = 0.25,
                    boxgroupgap = 0,
                    xaxis = list(showgrid = T, minor = list(showgrid = T)),
                    yaxis = list(showgrid = T, minor = list(showgrid = T), tickformat = ".1f")
                )

                # move axis labels to main plot
                if (length(input$summary_plot_facet) == 0) {
                    layout.opts$xaxis$title = X
                    if (input$summary_function == "mean") layout.opts$yaxis$title = "Mean"
                    else if (input$summary_function == "median") layout.opts$yaxis$title = "Median"
                    else if (input$summary_function == "auc") layout.opts$yaxis$title = "AUC"
                } else {
                    layout.opts$xaxis$title = ""
                    layout.opts$yaxis$title = ""
                }

                if ("dodgex" %in% input$summary_plot_options) {
                    layout.opts$boxmode = "group"
                }

                if (!is.null(input$summary_plot_color)) {
                    if ("hzlegend" %in% input$summary_plot_options)
                        layout.opts$legend = list(title = list(text=sprintf("<b>%s</b>", input$summary_plot_color)), orientation = 'h', y = -0.25, x = 0.5, xanchor = 'center', yanchor = 'top')
                    else
                        layout.opts$legend = list(title = list(text=sprintf("<b>%s</b>", input$summary_plot_color)))

                    layout.opts$showlegend = "legend" %in% input$summary_plot_options
                } else {
                    if (length(input$summary_plot_facet) == 1) {
                        layout.opts$legend = list(title = list(text=sprintf("<b>%s</b>", input$summary_plot_facet)), orientation = 'h', y = -0.25, x = 0.5, xanchor = 'center', yanchor = 'top')
                    }
                }

                .plot = do.call(layout, layout.opts)

                .plots[[length(.plots)+1]] = .plot
            }
        }

        if ("hzfacet" %in% input$summary_plot_options) {
            .plots$nrows = 1
        } else {
            .plots$nrows = length(facets)
        }

        .plots$shareY = T
        .plots$shareX = T

        if (length(input$summary_plot_facet) == 1 & !("hzfacet" %in% input$summary_plot_options)) {
            .plots$margin = c(0, 0, 0.05, 0)
        }

        .final = do.call(subplot, .plots)

        if (length(input$summary_plot_facet) == 1) {
            .title = ""
            if (input$summary_function == "mean") .title = "Mean"
            else if (input$summary_function == "median") .title = "Median"
            else if (input$summary_function == "auc") .title = "AUC"

            .final = .final %>%
                layout(margin = list(l = 100, b = 50)) %>%
                add_annotations(text = .title, xref = "paper", yref = "paper", x = -0.075, y = 0.5, xanchor = "right", yanchor = "center", showarrow = F, textangle = -90, font = list(size = 14)) %>%
                add_annotations(text = X, xref = "paper", yref = "paper", x = 0.5, y = -0.15, xanchor = "center", yanchor = "top", showarrow = F, font = list(size = 14))
        }

        .final
    }
 })

observe({
    req(length(names(data$series)) > 0)

    # afaik virtualselects' selected values are just vectors and not a structured list like the choices it can be given, so need to encode the dataset somehow
    value.list = c()
    series.list = lapply(setNames(names(data$series), names(data$series)), \(f) {
        if (length(names(data$series[[f]])) > 0) {
            raw.values = paste0(f, "!!", names(data$series[[f]]))
            setNames(raw.values, names(data$series[[f]]))
            value.list = append(value.list, raw.values)
        } else {
            NULL
        }
    })

    updateVirtualSelect("summary_series", choices = series.list, selected = value.list)
})
