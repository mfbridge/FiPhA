# downsampling

observeEvent(input$data_down, {
    updatePickerInput(session, "data_down_dataset", choices = names(data$meta), selected = "")
    showModal(
        modalDialog(
            footer = tagList(modalButton("Close"), actionButton("data_down_action", "Process")),
            pickerInput("data_down_dataset", "Dataset", c(), "", multiple = F, options = list(), width="100%"),

            fluidRow(
                column(2,
                    disabled(pickerInput("data_down_filter_type", "Filter Type", choices = c("lowpass")))
                ),
                column(4,
                    pickerInput("data_down_filter_vars", "Filter Columns(s)", choices = c(), multiple = T)
                ),
                column(3,
                    numericInput("data_down_filter_order", "Filter Order (n)", value = 8, min = 1, step = 1)
                ),
                column(3,
                    numericInput("data_down_filter_freq", "Filter Frequency (Hz)", value = 10, min = 0, step = 0.1)
                )
            ),
            fluidRow(
                column(3,
                    disabled(numericInput("data_down_infreq", label = "Old Frequency (Hz)", value = NULL, min = 0, max = Inf, width = "100%"))
                ),
                column(2,
                    disabled(numericInput("data_down_inrows", label = "# Rows", value = NULL, min = 0, max = Inf, width = "100%"))
                ),
                column(2, style="text-align: center;",
                    HTML("<h2>&rarr;</h2>")
                ),
                column(3,
                    numericInput("data_down_outfreq", label = "New Frequency (Hz)", value = NULL, min = 0, max = Inf, width = "100%")
                ),
                column(2,
                    disabled(numericInput("data_down_outrows", label = "est. # Rows", value = NULL, min = 0, max = Inf, width = "100%"))
                ),
            )
        )
    )
})

observeEvent(input$data_down_dataset, {
    .d = data$raw[[input$data_down_dataset]]
    .t = data$meta[[input$data_down_dataset]]$time
    old.dt = .d[2, get(.t)] - .d[1, get(.t)]
    new.dt = 1 / input$data_down_outfreq
    updateNumericInput(session, "data_down_outfreq", max = 1 / old.dt) # don't allow upsampling
    updateNumericInput(session, "data_down_outrows", value = ((.d[, .N]-1) * old.dt / new.dt))
    updateNumericInput(session, "data_down_infreq", value = 1 / old.dt)
    updateNumericInput(session, "data_down_inrows", value = .d[, .N])
    updatePickerInput(session, "data_down_filter_vars", choices = names(data$raw[[input$data_down_dataset]]))
})

observeEvent(input$data_down_outfreq, {
    req(input$data_down_dataset %in% names(data$meta))

    .d = data$raw[[input$data_down_dataset]]
    .t = data$meta[[input$data_down_dataset]]$time

    old.dt = .d[2, get(.t)] - .d[1, get(.t)]
    new.dt = 1 / input$data_down_outfreq
    updateNumericInput(session, "data_down_outrows", value = ((.d[, .N]-1) * old.dt / new.dt))
})

observeEvent(input$data_down_action, {
    req(input$data_down_dataset %in% names(data$meta))

    .d = data$raw[[input$data_down_dataset]]
    .t = data$meta[[input$data_down_dataset]]$time
    old.dt = .d[2, get(.t)] - .d[1, get(.t)]

    # apply a filter to selected variables prior to downsampling (helps with aliasing) if vars selected
    if (length(input$data_down_filter_vars) > 0) {
        withProgress({

            for (v in input$data_down_filter_vars) {
                incProgress(1 / length(input$data_down_filter_vars), detail = "v")
                fs = 1 / old.dt
                h = fir1(input$data_down_filter_order, input$data_down_filter_freq / (fs / 2), "low")
                x = .d[, get(v)]
                new.x = gsignal::filter(h, x)
                #.d[, (v) := new.x]
                data$raw[[input$data_down_dataset]][, (v) := new.x]
            }


        }, value = 0, message = "Applying filter...")
    }

    # only downsample if frequency is set
    if (req(input$data_down_outfreq)) {
        new.dt = 1 / input$data_down_outfreq
        old.t = .d[, get(.t)]
        new.t = seq(0, (.d[, .N]-1) * old.dt, new.dt)

        len.old = length(old.t)
        len.new = length(new.t)

        withProgress({
            # this is basically the alignment code
            v = numeric(len.new)
            r = numeric(len.new)

            j = 1
            for (i in 1:len.new) {
                prev.j = j
                while (j <= len.old) {
                    if (j != len.old & (abs(old.t[j+1] - new.t[i]) < abs(old.t[j] - new.t[i]))) {
                        j = j + 1
                    } else {
                        break
                    }
                }
                r[i] = j
                if (i %% 1000 == 0) incProgress(1000 / len.new)
            }

            data$raw[[input$data_down_dataset]] = data$raw[[input$data_down_dataset]][r, ]
            data$raw[[input$data_down_dataset]][, (.t) := new.t] # and finally reassign time to new spacing
        }, value = 0, message = "Downsampling...", detail = input$data_down_dataset)
    }

    # refresh displayed values?
    updatePickerInput(session, "data_down_dataset", choices = names(data$meta), selected = input$data_down_dataset)
    updateNumericInput(session, "data_down_outrows", value = data$raw[[input$data_down_dataset]][, .N])
    updateNumericInput(session, "data_down_infreq", value = 1 / new.dt)
    updateNumericInput(session, "data_down_outfreq", max = 1 / new.dt) # don't allow upsampling
    updateNumericInput(session, "data_down_inrows", value = data$raw[[input$data_down_dataset]][, .N])

    updatePickerInput(session, "data_down_filter_vars", choices = names(data$raw[[input$data_down_dataset]]))
})
