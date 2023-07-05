# tab panel for data page layout


data.ui = nav("Datasets",
    fluidRow(
        column(6,
               pickerInput("data_dataset", NULL, choices = c(), multiple = F,  width = "100%", options = list(title="Import a new dataset to begin"))),

        column(6, style="text-align: center;",
               dropdownButton(
                   actionLink("data_new_xlcsv", "tabular dataset (*.xlsx, *.csv)"),
                   actionLink("data_new_spectra", HTML("spectrometer recording (*.txt)")),
                   actionLink("data_tdt", "fiber photometry gizmo (*.tsq/*.tev)"),
                   actionLink("data_rds", "previous session (*.Rds)"),
                   #actionLink("data_new_spectra", "dataset from raw spectra (*.???)"),
                   circle = F, label = "Import", size = "xs", inline = T, status = "info"
               ),

               dropdownButton(
                   actionLink("data_unaligned", "join/align dataset (*.xlsx, *.csv)"),
                   actionLink("data_append", "append dataset rows"),
                   actionLink("data_rename", "rename a variable/dataset"),
                   tags$hr(style="margin: 0.4rem;"),
                   actionLink("data_down", "signal processing"),
                   actionLink("data_ratio", "create ratio"),
                   actionLink("data_baseline", "de-trend variable(s)"),
                   actionLink("data_custom", "custom R script"),
                   tags$hr(style="margin: 0.4rem;"),
                   actionLink("data_rescale", "linear scaling"),
                   circle = F, label = "Transform", size = "xs", inline = T, status = "info"
               ),
                dropdownButton(
                    shinySaveLink("data_save", "selected (*.xlsx, *.csv)", "Save selected dataset", filetype = list("Excel Workbook"=c("xlsx"), "CSV File"=c("csv"))),
                    shinySaveLink("data_saveall", "export all (*.xlsx)", "Export all datasets", filetype = list("Excel Workbook"=c("xlsx"))),
                    circle = F, label = "Save", size = "xs", inline = T, status = "warning"
                ),

                actionButton("data_remove", "Remove", class="btn btn-danger action-button btn-xs")
        )
    ),

    tabsetPanel(
        tabPanel("Preview",
                 tags$br(),
                 fluidRow(
                     column(5, pickerInput("data_plot_x", NULL, c(), width = "100%", options = list(title="X Axis Variable"))),
                     column(5, pickerInput("data_plot_y", NULL, c(), width = "100%", multiple = T, options = list(`max-options`=2, title="Y Axis Variable(s)"))),
                     column(2, dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
                                        plotOptions("data_plot_opts", options = list(
                                            list(type="title", text="Plot Options"),
                                            list(type="font-size+family", size.id="data_font_size", family.id="data_font_family", text = "Font", size.value = 14, family.value = "Open Sans"),
                                            list(type="theme", id = "data_theme", text = "Theme"),
                                            list(type="hr"),
                                            list(type="line-size+color", size.id="data_y_size", color.id="data_y_color", text="Y Size & Color", size.value=0.15, color.value="#000000"),
                                            list(type="line-size+color", size.id="data_y2_size", color.id="data_y2_color", text="Y2 Size & Color", size.value=0.15, color.value="#ff0000"),
                                            list(type="hr"),
                                            list(type="checkbox", id = "data_scale_y_y2", text = "plot percent change", checked = F))
                                    )
                            )
                     )
                 ),
                 fluidRow(
                     column(12, plotlyOutput("data_plot", height = "640px") %>% withSpinner())
                 )
        ),

        tabPanel("Signal Analysis",
            fluidRow(
                column(12,
                    fluidRow(
                        column(4,
                            pickerInput("data_signal_var", "dataset variable", c(), width = "100%", options = list(title="Variable"))
                        ),
                        column(4,
                            numericInput("data_lag_max", "max lag n", width = "100%", value = 10000, min = 1, step = 1),
                        ),
                        column(4,
                            pickerInput("data_power_size", "spectrogram window size", choices = c(32, 64, 128, 256, 512, 1024, 2048, 8192), selected = 256, width = "100%")
                        )
                    ),

                    tabsetPanel(
                        tabPanel("lag autocorrelation",


                            fluidRow(column(12, plotlyOutput("data_lag_plot", height = "600px") %>% withSpinner())),
                            downloadLink("data_signal_dl_lag", label = tagList(icon("download"), "Lag-autocorrelation data (xlsx)"))
                        ),
                        tabPanel("power density spectrum",

                            fluidRow(column(12, plotlyOutput("data_power_plot", height = "600px") %>% withSpinner())),
                            downloadLink("data_signal_dl_pow", label = tagList(icon("download"), "Power density spectrum data (xlsx)"))
                        ),
                        tabPanel("spectrogram",
                            fluidRow(column(12, plotlyOutput("data_spectro_plot", height = "600px") %>% withSpinner())),
                        )
                    )
                )
            )
        ),

        tabPanel("Metadata",
                 tags$br(),
                 fluidRow(
                     column(8, disabled(textAreaInput("data_source", "Source File", width = "100%", rows = 3))),
                     column(4, pickerInput("data_time", "Time Variable", c(), width = "100%"))
                 ),
                 fluidRow(
                     column(12, tags$label("Models"), tags$br(), htmlOutput("data_models"))
                 )
        )
    )
)
