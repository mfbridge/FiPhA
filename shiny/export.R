# export.R


shinyFileSave(input, "export_r", root=c(`Working Directory`='.', getVolumes()()), filetypes=c("rds"))
shinyFileSave(input, "export_excel", root=c(`Working Directory`='.', getVolumes()()), filetypes=c("xlsx"))

observeEvent(input$export_format, {
    shinyjs::hide("export_excel_opts")
    shinyjs::hide("export_r_opts")

    if (input$export_format == "excel") shinyjs::show("export_excel_opts")
    if (input$export_format == "r") shinyjs::show("export_r_opts")
})

observeEvent(input$export_r, {
    if (is.integer(input$export_r)) {
        # nothing selected
    } else {
        fileinfo = parseSavePath(root=c(`Working Directory`='.', getVolumes()()), selection = input$export_r)

        withProgress({
            obj = list()
            # need to explictly save values from the `data` object, not in a Shiny reactive context
            for (x in names(data)) { obj[[x]] = isolate(data[[x]]) }
            write_rds(obj, fileinfo$datapath)
        }, message = "Saving...")
    }
})

observeEvent(input$export_excel, {
    if (is.integer(input$export_excel)) {
        # nothing selected
    } else {
        fileinfo = parseSavePath(root=c(`Working Directory`='.', getVolumes()()), selection = input$export_excel)

        withProgress({

            out.data = list()

            # save metadata info
            out.data[["Datasets"]] = data.table()

            out.data[["Models"]] = getAllModelStrings()

            for (f in names(data$events)) {
                out.data[["Datasets"]] = rbindlist(list(out.data[["Datasets"]], data.table(name = f, path = data$meta[[f]]$path, filename = data$meta[[f]]$file)))
            }

            # Export Lists of Events on a separate page
            # out.data[["Events"]] = data.table()


            if ("acf" %in% input$export_excel_tabs) out.data[["ACF"]] = data.table()
            if ("sum" %in% input$export_excel_tabs) out.data[["Summary"]] = data.table()
            if ("power" %in% input$export_excel_tabs) out.data[["Power Density"]] = data.table()

            for (f in names(data$events)) {
                # for (s in names(data$events[[f]])) {
                #     events = data$events[[f]][[s]]
                #     t = data$meta[[f]]$time
                #     for (e in 1:length(events)) {
                #         out.data[["Events"]] = rbindlist(list(out.data[["Events"]], data.table(
                #             dataset = f,
                #             event = s,
                #             index = e,
                #             start = events[[e]][, min(get(t))],
                #             end = events[[e]][, max(get(t))],
                #             length = events[[e]][, max(get(t)) - min(get(t))]
                #         )))
                #     }
                # }

                if ("acf" %in% input$export_excel_tabs & !is.null(data$analysis[[f]]$acf)) {
                    out.data[["ACF"]] = rbindlist(list(out.data[["ACF"]], data.table(f = f, data$analysis[[f]]$acf)))
                }

                if ("sum" %in% input$export_excel_tabs & !is.null(data$analysis[[f]]$summary)) {
                    out.data[["Summary"]] = rbindlist(list(out.data[["Summary"]], data.table(f = f, data$analysis[[f]]$summary)))
                }


                if ("power" %in% input$export_excel_tabs & !is.null(data$analysis[[f]]$power)) {
                    out.data[["Power Density"]] = rbindlist(list(out.data[["Power Density"]], data.table(f = f, data$analysis[[f]]$power[, .(hz = X, amplitude = Y)])))
                }
            }

            #for (f in data$filenames) {
            # for (f in names(data$events)) {
            #     for (s in names(data$events[[f]])) {
            #         f.dt = data$events[[f]][[s]]
            #         f.dt[, `:=`(i = .I, filename = f)]
            #         out.data[["Events"]] = rbindlist(list(out.data[["Events"]], f.dt))
            #     }
            # }

            #for (f in data$filenames) {
            for (f in names(data$events)) {
                if (!("tab_int" %in% input$export_excel_opts_list)) {
                    # separate by series, file
                    for (s in names(data$events[[f]])) {
                        tab.name = sprintf("%s|%s", s, f)

                        .nrows = 0
                        for (e in 1:length(data$events[[f]][[s]])) {
                            .nrows = max(.nrows, nrow(data$events[[f]][[s]][[e]]))
                        }


                        if ("int_align" %in% input$export_excel_opts_list) {
                            ir = data.table()
                            # figure out max interval lengths
                            for (e in 1:length(data$events[[f]][[s]])) {
                                ir = rbindlist(list(ir, data$events[[f]][[s]][[e]][, .(int = `(interval)`, len = .SD[, .N]), by = .(int = `(interval)`)]))
                            }
                            int.max = ir[, .(ml = max(len)), by = .(int)]
                            int.rows = data.table()
                            #for (i in int.max[, unique(int
                            for (i in data$series[[f]][[s]]$intervals[, unique(name)]) {
                                int.rows = rbindlist(list(int.rows, data.table(interval = i, int.row = seq(1, int.max[int == i, ml]))))
                            }
                            .nrows = nrow(int.rows)
                        }

                        if ("event_align" %in% input$export_excel_opts_list) {
                            largest.time = 0
                            largest.rows = data.table(event.time = character(), e.row = numeric())
                            for (e in 1:length(data$events[[f]][[s]])) {
                                temp.rows = data$events[[f]][[s]][[e]][, .(event.time = sprintf("%0.6f", `(event time)`))]
                                largest.rows = merge.data.table(largest.rows, temp.rows, by = c("event.time"), all = T, sort = F)
                                largest.rows = largest.rows[, .(event.time)]
                            }
                            largest.rows$et.num = as.numeric(largest.rows$event.time)
                            largest.rows = largest.rows[order(et.num),]
                            .nrows = nrow(largest.rows)
                        }

                        out.data[[tab.name]] = data.table(Dataset = f, Name = s, Row = 1:.nrows)
                        out.data[[tab.name]] = cbind(out.data[[tab.name]], data.table(` ` = rep("", .nrows))) # add a space

                        for (e in 1:length(data$events[[f]][[s]])) {
                            fv.data = data$events[[f]][[s]][[e]][order(`(event time)`), .(
                                etime = `(event time)`,
                                itime = `(interval time)`,
                                ftime = get(data$meta[[f]]$time),
                                value = get(data$series[[f]][[s]]$responses[[1]]),
                                interval = `(interval)`,
                                fvint = `(interval)`,
                                int.row = .SD[, .I]), by = .(int = `(interval)`)]

                            fv.data[, row := .I]

                            if ("event_align" %in% input$export_excel_opts_list) {
                                #le.rows = largest.event[order(`(event time)`), .(event.time = sprintf("%0.6f", `(event time)`))]
                                fv.data[, event.time := sprintf("%0.6f", etime)]

                                fv.aligned = merge.data.table(largest.rows, fv.data, by = c("event.time"), all = T, sort = F) # sort ok?
                                fv.data = fv.aligned
                            }

                            if ("int_align" %in% input$export_excel_opts_list) {
                                fv.data = merge.data.table(int.rows, fv.data, by = c("interval", "int.row"), all = T, sort = F)
                                #fv.data$interval = fv.data$fvint # fix for weird stuff?
                            }

                            if ("pad_rev" %in% input$export_excel_opts_list) {
                                #for each interval in fve.data, count the NAs at the end and shift to beginning of interval because by default we're left-aligned
                                #for (i in unique(fv.data$interval)) {
                                for (i in data$series[[f]][[s]]$intervals[, unique(name)]) {
                                    i.rows = fv.data[interval == i, ]
                                    na.rows = i.rows[is.na(etime), ]
                                    reg.rows = i.rows[!is.na(etime), ]

                                    na.n = na.rows[, .N]
                                    total.n = i.rows[, .N]

                                    min.i = min(i.rows[, row], na.rm = T)
                                    max.i = max(i.rows[, row], na.rm = T)

                                    # rearrange
                                    if (na.n > 0) {
                                        fv.data[min.i:(min.i+na.n-1), `:=`(etime = na.rows$etime,
                                            itime=na.rows$itime,
                                            ftime=na.rows$ftime,
                                            interval=na.rows$interval,
                                            value=na.rows$value,
                                            row=na.rows$row,
                                            int.row=na.rows$int.row,
                                            fvint=na.rows$fvint)]

                                        fv.data[(min.i+na.n):(min.i+na.n-1+reg.rows[,.N]), `:=`(etime = reg.rows[, etime],
                                            itime=reg.rows$itime,
                                            ftime=reg.rows$ftime,
                                            interval=reg.rows$interval,
                                            value=reg.rows$value,
                                            row=reg.rows$row,
                                            int.row=reg.rows$int.row,
                                            fvint=reg.rows$fvint)]
                                    }
                                }

                                fv.data$interval = fv.data$fvint
                            }

                            #printf("%f %f\n", nrow(fv.data), nrow(out.data[[tab.name]]))

                            if ("event_interval" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval", e)) := fv.data[, interval]]
                            }

                            if ("event_interval_single" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval", e)) := fv.data[, ifelse(shift(interval, n=1) != interval | is.na(shift(interval, n=1)), interval, "")]]
                            }

                            if ("event_interval_time" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval Time", e)) := round(fv.data[, itime], 3)]
                            }

                            if ("event_file" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, File Time", e)) := round(fv.data[, ftime], 3)]
                            }

                            if ("event_event" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Event Time", e)) := round(fv.data[, etime], 3)]
                            }

                            fve.data = fv.data

                            if ("event_var" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]][1:nrow(fve.data), (sprintf("%0.0f, %s", e, data$series[[f]][[s]]$responses[[1]])) := fve.data[, value]]
                            } else {
                                out.data[[tab.name]][1:nrow(fve.data), (sprintf("%0.0f", e)) := fve.data[, value]]
                            }

                            if ("eventspace" %in% input$export_excel_opts_list) {
                                out.data[[tab.name]] = cbind(out.data[[tab.name]], data.table(` ` = rep("", .nrows)))
                            }
                        }

                        incProgress(1 / length(data$filenames))
                    }
                } else {
                    # separate by interval, series, file
                    for (s in names(data$events[[f]])) {
                        for (i in data$series[[f]][[s]]$intervals$name) {

                            tab.name = sprintf("%s|%s|%s", i, s, f)

                            .nrows = 0
                            for (e in 1:length(data$events[[f]][[s]])) {
                                .nrows = max(.nrows, nrow(data$events[[f]][[s]][[e]][`(interval)` == i,]))
                            }

                            out.data[[tab.name]] = data.table(Dataset = f, Name = s, Row = 1:.nrows, `Interval Time`=0)
                            out.data[[tab.name]] = cbind(out.data[[tab.name]], data.table(` ` = rep("", .nrows))) # add a space

                            for (e in 1:length(data$events[[f]][[s]])) {
                                fv.data = data$events[[f]][[s]][[e]][order(`(event time)`), .(etime = `(event time)`, itime = `(interval time)`, ftime = get(data$meta[[f]]$time),  interval = `(interval)`)]
                                fv.data = fv.data[interval == i, ]
                                out.data[[tab.name]][1:nrow(fv.data), `Interval Time` := fv.data$itime]

                                if ("event_interval" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval", e)) := fv.data[, interval]]
                                }

                                if ("event_interval_single" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval", e)) := fv.data[, ifelse(shift(interval, n=1) != interval | is.na(shift(interval, n=1)), interval, "")]]
                                }

                                if ("event_interval_time" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Interval Time", e)) := round(fv.data[, itime], 3)]
                                }

                                if ("event_file" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, File Time", e)) := round(fv.data[, ftime], 3)]
                                }


                                if ("event_event" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fv.data), (sprintf("%0.0f, Event Time", e)) := round(fv.data[, etime], 3)]
                                }

                                fve.data = data$events[[f]][[s]][[e]][order(`(event time)`), .(time = `(interval time)`, interval = `(interval)`, value = get(data$series[[f]][[s]]$responses[[1]]))]
                                fve.data = fve.data[interval == i,]

                                if ("event_var" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]][1:nrow(fve.data), (sprintf("%0.0f, %s", e, data$series[[f]][[s]]$responses[[1]])) := fve.data[, value]]
                                } else {
                                    out.data[[tab.name]][1:nrow(fve.data), (sprintf("%0.0f", e)) := fve.data[, value]]
                                }


                                if ("eventspace" %in% input$export_excel_opts_list) {
                                    out.data[[tab.name]] = cbind(out.data[[tab.name]], data.table(` ` = rep("", .nrows)))
                                }
                            }
                        }

                        incProgress(1 / length(data$filenames))
                    }
                }
            }

            try({
                write_xlsx(out.data, fileinfo$datapath, col_names = T, format_headers = T)
            })

        }, message = "Saving...")
    }
})
