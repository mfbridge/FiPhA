# events selected series preview

output$events_preview = renderPlotly({
    # prepare datasets
    req(input$events_series, input$events_dataset)

    interval.names = c()
    dataset = data.table()
    minmax.sdt = data.table()

    for (s in input$events_series) {
        .s = data$series[[input$events_dataset]][[s]]
        interval.names = append(interval.names, .s$intervals$name)
        for (e in 1:length(data$events[[input$events_dataset]][[s]])) {
            .ed = data$events[[input$events_dataset]][[s]][[e]]
            if (nrow(.ed) > 0) {
                if (input$events_series_x == "event time") {
                    dataset = rbindlist(list(dataset, .ed[, .(e = e, s = s, i = `(interval)`, X = `(event time)`, Xe = `(event time)`, Y = get(.s$responses[[1]]))]))
                } else {
                    dataset = rbindlist(list(dataset, .ed[, .(e = e, s = s, i = `(interval)`, X = `(interval time)`, Xe = `(event time)`, Y = get(.s$responses[[1]]))]))
                }
            }


        }
    }
    dataset[, Xstr := sprintf("%0.3f", X)]
    dataset$i.f = factor(dataset$i, levels = unique(interval.names), ordered = T)

    rfdt = data.table()

    if (input$events_risefall) {
        for (es in input$events_series) {
            .dt = dataset[s == es, ]
            #browser()
            try({

            f = lm(Y ~ ns(Xe, df = input$events_risefall_nsdf), .dt)
            XX = seq(min(.dt$Xe), max(.dt$Xe), .dt$Xe[2]-.dt$Xe[1])
            Ys = predict(f, data.table(Xe = XX))
            peaks = gsignal::findpeaks(Ys, DoubleSided = T, MinPeakHeight = input$events_risefall_peakheight, MinPeakWidth = input$events_risefall_peakwidth)
            #if (length(peaks$loc) > 6) {
                maxpk = max(Ys) # one of peaks obviously should be the global max
                maxi = peaks$loc[peaks$pks==maxpk]
                maxpeaki = which(peaks$pks==maxpk)
                maxpk.x = XX[maxi]

                # find which local minima are next to our global max
                left.minima = peaks$loc[1]
                right.minima = peaks$loc[length(peaks$loc)]
                for (ii in (maxpeaki+1):(length(peaks$pks)-1)) {
                    # find first local maxima
                    if (peaks$pks[ii-1] > peaks$pks[ii] & peaks$pks[ii] < peaks$pks[ii+1]) {
                        right.minima = peaks$loc[ii]
                        break
                    }
                }
                for (ii in (maxpeaki-1):2) {
                    if (peaks$pks[ii-1] > peaks$pks[ii] & peaks$pks[ii] < peaks$pks[ii+1]) {
                        left.minima = peaks$loc[ii]
                        break
                    }
                }
                #print(peaks)
                rise.est = maxpk.x - XX[left.minima]
                fall.est = XX[right.minima] - maxpk.x

                rfdt = rbindlist(list(rfdt, data.table(s = es, x = XX, y = Ys)))

                minmax.sdt = rbindlist(list(minmax.sdt, data.table(
                    `Series` = es,
                    `Left Minima` = XX[left.minima],
                    `Peak` = maxpk.x,
                    `Right Minima` = XX[right.minima],
                    `Rise Time` = rise.est,
                    `Fall Time` = fall.est
                )), fill = T)

            })
            #} else {
            #    showNotification("Could not find any peaks with specified parameters.", type = "error")
            #}
        }

    }

    if (input$events_meansd) {
        if (input$events_series_x == "event time") {
            mean.sd = dataset[, .(x=mean(X), mu = mean(Y, na.rm = T), sig = sd(Y, na.rm = T)), by = .(s, Xstr)] # rounding error is a thing sometimes
        } else {
            mean.sd = dataset[, .(x=mean(X), mu = mean(Y, na.rm = T), sig = sd(Y, na.rm = T)), by = .(s, i.f, Xstr)] # rounding error is a thing sometimes
        }
        mean.sd[, lower := mu - input$events_meansd_n * sig]
        mean.sd[, upper := mu + input$events_meansd_n * sig]
        mean.sd = mean.sd[order(x),]
        mean.sd = mean.sd[!is.na(mu)&!is.na(upper)&!is.na(lower),]

        #browser()
    }

    output$events_minmax_summary = renderTable({
        minmax.sdt
    })

    ggplotly({
        .gg = ggplot(dataset, aes(x = X, y = Y, color = i, group = e)) +
            #facet_grid(s ~ .) +
            #(get(paste0("theme_", input$events_theme)))(base_size = input$events_font_size) +
            theme_minimal(base_size = input$events_font_size) +
            labs(x = input$events_title_xaxis, y = input$events_title_yaxis) +
            theme(panel.spacing = unit(0, "cm")) +
            coord_cartesian(expand = F)


        if (input$events_series_x == "event time") {
            .gg = .gg + facet_grid(s ~ .)
        } else if (input$events_series_x == "interval time") {
            .gg = .gg + facet_grid(s ~ i.f)
        }


        if (input$events_show_xaxis) {
            .gg = .gg + geom_hline(yintercept = 0, linetype = "solid", size = 0.2, color = "#000000")
        }

        if (input$events_show_yaxis) {
            .gg = .gg + geom_vline(xintercept = 0, linetype = "dotted", size = 0.2, color = "#000000")
        }

        if (input$events_meansd) {
            # put the ribbon in the background
            .gg = .gg + geom_ribbon(aes(x = x, ymax = upper, ymin = lower), data = mean.sd, fill = input$events_meansd_area, inherit.aes = F)
        }

        # draw lines for each interval
        .gg = .gg + geom_line(alpha = input$events_y_alpha, size = input$events_y_size)
        if (input$events_interval_palette == "gradient2") {
            .gg = .gg + scale_color_manual(values = colorRampPalette(c(input$events_interval_from, input$events_interval_to))(dataset[, length(unique(i))]))
        } else if (input$events_interval_palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
            .gg = .gg + scale_color_viridis_d(begin = 0.1, end = 0.9)
        }

        if (input$events_risefall) {
            if (input$events_series_x == "event time") {
                .gg = .gg + geom_line(aes(x = x, y = y, group = s), rfdt, linewidth = 0.75, alpha = 0.5, inherit.aes = F)
                .gg = .gg + geom_vline(aes(xintercept = x, group = s), data = data.table(x = c(minmax.sdt$`Left Minima`, minmax.sdt$`Peak`, minmax.sdt$`Right Minima`), s = minmax.sdt$Series), linetype = "dotted", linewidth = 0.5)
            }
        }

        if (input$events_meansd) {
            # draw the mean line last
            if (input$events_series_x == "event time") {
                .gg = .gg + geom_line(aes(x = x, y = mu), data = mean.sd, color = input$events_meansd_line, size = input$events_meansd_size, inherit.aes = F)
            } else {
                .gg = .gg + geom_line(aes(x = x, color = i, y = mu), data = mean.sd, color = input$events_meansd_line, size = input$events_meansd_size, inherit.aes = F)
            }
        }

        .gg
    }) %>%
        config(toImageButtonOptions = list(filename = "preview", format = input$plotly_format, height = input$plotly_height, width = input$plotly_width)) %>%
        layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.25),
            xaxis = list(tickmode = "auto"), yaxis = list(tickmode = "auto")) %>%
        toWebGL2()
})
