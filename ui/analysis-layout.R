
analysis.ui = navbarMenu("Analysis",
    "Visualizations",

    tabPanel("dataset traces",
        fluidRow(
            column(5, pickerInput("heatmap2_dataset", NULL, c(), width = "100%", multiple = T, options = list(title="Select dataset(s)", `actions-box`=T))),
            column(4, pickerInput("heatmap2_series", NULL, c(), width = "100%", options = list(title = "Event series"), multiple = T)),
            #column(2, pickerInput("heatmap2_sort", NULL, choices = c("sort by length"="length", "sort by auc"="auc", "sort by order"="order"))),
            column(3,
                fluidRow(
                    # column(6,
                    #     dropdown(size = "xs", status = "warning", icon = icon("chart-area"), right = T,
                    #         tags$label(style="font-weight: bold; text-align: center; width: 100%;", "AUC Options"),
                    #         pickerInput("heatmap2_auc_type", NULL, c("integrate over full event"="full", "integrate over specified range"="range")),
                    #         sliderInput("heatmap2_auc_range", HTML("t<sub>event</sub> Range"), min = 0, max = 0, value = c(0, 0), dragRange = T, step = 0.001))
                    # ),
                    column(12,
                        dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
                            plotOptions("heatmap2_options", options = list(
                                                list(type="title", text="Plot Options"),
                                                list(type="font-size+family", size.id="heatmap2_font_size", family.id="heatmap2_font_family", text = "Font", size.value = 9, family.value = "Open Sans"),
                                                list(type="theme", id = "heatmap2_theme", text = "Theme"),
                                                list(type="hr"),
                                                list(type="palette", text="Palette", palette.id="heatmap2_palette", from.id = "heatmap2_from", to.id = "heatmap2_to", from.color = "#000000", to.color = "#ffffff"),
                                                list(type="hr"),

                                                list(type="line-size+alpha", size.id="heatmap2_y_size", text="Y Size / Alpha", size.value=0.3, alpha.value = 1, alpha.id = "heatmap2_y_alpha"),
                                                list(type="mean+se", show.id="heatmap2_meansd", n.id = "heatmap2_meansd_n", ss.id = "heatmap_meansd_ss", n.value = 3, n.f = "traces_sdse"),
                                                list(type="line-size+alpha", size.id="heatmap2_meansd_size", text="Mean Size / Alpha", size.value=0.3, alpha.value = 1, alpha.id = "heatmap2_meansd_alpha"),
                                                list(type="line-color", id = "heatmap2_meansd_line", value = "#000000", text = "Mean Color"),
                                                list(type="checkbox", id = "heatmap2_shade_intervals", value = T, text = "Shade Area By Intervals"),

                                                list(type="palette", text="Palette", palette.id="heatmap2_shade_palette", from.id = "heatmap2_shade_from", to.id = "heatmap2_shade_to", from.color = "#000000", to.color = "#ffffff"),
                                                list(type="line-color", id = "heatmap2_meansd_area", value = "#1259ff", text = "SD Color")
                                        )
                        ))
                    )
                )
            )
        ),
        fluidRow(
          column(12, plotlyOutput("heatmap2_plot", height = "800px") %>% withSpinner()),
        )
    ),

    tabPanel("event heatmaps",
        fluidRow(
            column(4, pickerInput("heatmap_dataset", NULL, c(), width = "100%", options = list(title="Select a dataset"))),
            column(3, pickerInput("heatmap_series", NULL, c(), width = "100%", options = list(title = "Select an event series"))),
            column(2, pickerInput("heatmap_sort", NULL, choices = c("sort by length"="length", "sort by auc"="auc", "sort by order"="order"))),
            column(3,
                fluidRow(
                    column(6,
                        dropdown(size = "xs", status = "warning", icon = icon("chart-area"), right = T,
                            tags$label(style="font-weight: bold; text-align: center; width: 100%;", "AUC Options"),
                            pickerInput("heatmap_auc_type", NULL, c("integrate over full event"="full", "integrate over specified range"="range")),
                            sliderInput("heatmap_auc_range", HTML("t<sub>event</sub> Range"), min = 0, max = 0, value = c(0, 0), dragRange = T, step = 0.001))
                    ),
                    column(6,
                        dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
                            plotOptions("heatmap_options", options = list(
                                                list(type="title", text="Plot Options"),
                                                list(type="font-size+family", size.id="heatmap_font_size", family.id="heatmap_font_family", text = "Font", size.value = 14, family.value = "Open Sans"),
                                                list(type="theme", id = "heatmap_theme", text = "Theme"),
                                                list(type="hr"),
                                                list(type="palette", text="Palette", palette.id="heatmap_palette", from.id = "heatmap_from", to.id = "heatmap_to", from.color = "#000000", to.color = "#ffffff")
                                        )
                        ))
                    )
                )
            )
        ),
        fluidRow(
          column(12, plotlyOutput("heatmap_plot", height = "800px") %>% withSpinner()),
        ),
        fluidRow(
          column(12, plotOutput("heatmap_plot_static", height = "800px") %>% withSpinner()),
        )
    ),

    # "---",
    # "Signal Processing",
    #
    # tabPanel("lag-1 autocorrelation",
    #     fluidRow(
    #         column(4, pickerInput("lag_dataset", NULL, c(), width = "100%", options = list(title="Select a dataset"))),
    #         column(4, pickerInput("lag_series", NULL, choices = c(), multiple = T)),
    #         column(4,
    #             fluidRow(
    #                 column(12,
    #                     dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
    #                         plotOptions("lag_options", options = list(
    #                                             list(type="title", text="Plot Options"),
    #                                             list(type="font-size+family", size.id="lag_font_size", family.id="lag_font_family", text = "Font", size.value = 14, family.value = "Open Sans"),
    #                                             list(type="theme", id = "lag_theme", text = "Theme"),
    #                                             list(type="hr"),
    #                                             list(type="palette", text="Palette", palette.id="lag_palette", from.id = "lag_from", to.id = "lag_to", from.color = "#000000", to.color = "#ffffff")
    #                                     )
    #                     ))
    #                 )
    #             )
    #         )
    #     ),
    #     fluidRow(
    #         column(12,
    #             tabsetPanel(
    #                 tabPanel("Plot", plotOutput("lag_plot", height = "600px") %>% withSpinner())
    #             )
    #         )
    #     ),
    # ),
    #
    # tabPanel("power density spectrum",
    #     fluidRow(
    #         column(4, pickerInput("power_dataset", NULL, c(), width = "100%", options = list(title="Select a dataset"))),
    #         column(4, pickerInput("power_series", NULL, choices = c(), multiple = T)),
    #         column(4,
    #             fluidRow(
    #                 column(12,
    #                     dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
    #                         plotOptions("power_options", options = list(
    #                                             list(type="title", text="Plot Options"),
    #                                             list(type="font-size+family", size.id="power_font_size", family.id="power_font_family", text = "Font", size.value = 14, family.value = "Open Sans"),
    #                                             list(type="theme", id = "power_theme", text = "Theme"),
    #                                             list(type="hr"),
    #                                             list(type="checkbox", id = "power_log10_x", text="log10 X Axis"),
    #                                             list(type="checkbox", id = "power_log10_y", text="log10 Y Axis")
    #                                             #list(type="palette", text="Palette", palette.id="lag_palette", from.id = "lag_from", to.id = "lag_to", from.color = "#000000", to.color = "#ffffff")
    #                                     )
    #                     ))
    #                 )
    #             )
    #         )
    #     ),
    #     fluidRow(
    #         column(12,
    #             tabsetPanel(
    #                 tabPanel("Plot", plotlyOutput("power_plot", height = "800px") %>% withSpinner())
    #             )
    #         )
    #     ),
    # ),

    "---",
    "Statistics",


    tabPanel("interval summaries",
        fluidRow(
            #column(4, pickerInput("summary_dataset", NULL, c(), width = "100%", multiple = T, options = list(title="Select a dataset", `actions-box`=T))),
            column(8, virtualSelectInput("summary_series", NULL, choices = c(), multiple = T, allowNewOption = F, autoSelectFirstOption = F, optionHeight = "24rem", width = "100%")),
            column(4,
                fluidRow(
                    column(6,
                        dropdown(size = "xs", status = "warning", icon = icon("chart-area"), right = T,
                            pickerInput("summary_function", NULL, choices = c("mean", "median", "auc")),
                            tags$label(style="font-weight: bold; text-align: center; width: 100%;", "AUC Options"),
                            pickerInput("summary_auc_type", NULL, c("integrate over full interval"="full", "integrate over specified range"="range")),
                            sliderInput("summary_auc_range", HTML("t<sub>interval</sub> Range"), min = 0, max = 0, value = c(0, 0), dragRange = T, step = 0.001))
                    ),
                    column(6,
                        dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
                            plotOptions("summary_options", options = list(
                                                list(type="title", text="Plot Options"),
                                                list(type="font-size+family", size.id="summary_font_size", family.id="summary_font_family", text = "Font", size.value = 14, family.value = "Open Sans"),
                                                list(type="theme", id = "summary_theme", text = "Theme"),
                                                list(type="hr"),
                                                list(type="palette", text="Palette", palette.id="summary_palette", from.id = "summary_from", to.id = "summary_to", from.color = "#404040", to.color = "#a0a0a0")
                                        )
                        ))
                    )
                )
            )
        ),
        fluidRow(
            column(12,
                tabsetPanel(
                    tabPanel("Plot", plotOutput("summary_plot", height = "720px") %>% withSpinner())
                    #tabPanel("Data", excelOutput("summary_data", width = "100%", height = "100%"))
                )
            )
        ),
        # tags$br(),
        # tabsetPanel(
        #     tabPanel("Summary",
        #         tags$h6("Assumptions"),
        #         tags$ul(
        #             tags$li("Summary values were not significantly different from that a normal distribution (Shapiro-Wilk p=0.5, Kolmogorov-Smirnov p=0.5)."),
        #             tags$li("Variances among interval groups did not significantly depart from one another (Levene's test p=0.5).")
        #         ),
        #         tags$h6("ANOVA Results"),
        #         tags$ul(
        #             tags$li("Intervals were not significantly different from one another.")
        #         )
        #     ),
        #     tabPanel("Distribution"),
        #     tabPanel("Variance"),
        #     tabPanel("ANOVA")
        # )
    ),

    #tabPanel("analysis of variance")
)
