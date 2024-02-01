
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

    "---",
    "Statistics",


    tabPanel("interval summaries",
        layout_columns(col_widths = 12, row_heights = c(7, 5),
            layout_columns(col_widths = c(4, 8),
                card(
                    card_header(
                        layout_columns(class = "m-0", col_widths = c(10, 2),
                            "Model",
                            div(class = "text-end",
                                dropMenu(actionLink("summary_model_menu", label = NULL, icon = icon("gears"), class = "text-dark"), padding = "0px", theme="light-border", placement = "bottom",
                                    virtualSelectInput("summary_function", "Summary Function", choices = c("mean", "median", "auc"), selected = "mean"),
                                    virtualSelectInput("summary_model_ss", "ANOVA Sums of Squares", selected = "III", choices = c("Type I"="I", "Type II"="II", "Type III" ="III"), width = "100%"),
                                    tags$br(),
                                    div(style = "text-align: left; font-size: 0.75rem;",uiOutput("summary_ss_info")),
                                    tags$br(),tags$br(),
                                    checkboxGroupInput("summary_options", "lme4 Options",
                                        choices = list(
                                            "Use REML instead of ML\u00b9"="reml"), selected = c("drop", "reml"), width = "100%"),

                                    tags$br(),
                                    div(style = "font-size: 0.75rem; text-align: left;",
                                        HTML("<sup>1</sup> REML (restricted maximum likelihood) estimates are unbiased, but models with different fixed effects cannot be directly compared using log-likelihood methods such as AIC (Akaike Information Criterion).")
                                    )
                                )
                            )
                        )
                    ),

                    virtualSelectInput("summary_model_type", NULL, selected = NULL, placeholder = "Model Type", width = "100%", choices = c("One-Way ANOVA"="one", "Two-Way ANOVA"="two", "Repeated Measures ANOVA"="rep")),
                    div(style = "font-size: 0.75rem;", uiOutput("summary_model_info") ),
                    virtualSelectInput("summary_series", "Data", choices = c(), multiple = T, placeholder = "Series", optionHeight = "24rem", width = "100%"),
                    virtualSelectInput("summary_fixed_effects", "Fixed Effect(s)", choices = c("Dataset", "Series", "Event #", "Interval"), selected = "Interval", multiple = T, placeholder = "Main/Fixed Effect(s)", optionHeight = "24rem", width = "100%"),
                    virtualSelectInput("summary_interaction_terms", "Interaction(s)", choices = c(), multiple = T, placeholder = "Interaction(s)", optionHeight = "24rem", width = "100%"),
                    virtualSelectInput("summary_random_effects", "Random Effect(s)" , choices = c("Dataset", "Series", "Event #", "Interval"), selected = "Event #", multiple = T, placeholder = "Random Effect(s)", optionHeight = "24rem", width = "100%"),

                ),
                card(
                    card_header(
                        layout_columns(class = "m-0", col_widths = c(10, 2),
                            "Plot",
                            div(class = "text-end",
                                dropMenu(actionLink("test", label = NULL, icon = icon("gears"), class = "text-dark"), padding = "0px", theme="light-border", placement = "bottom-end",
                                    div(style = "text-align: left;",
                                        checkboxGroupInput("summary_plot_options", label = "Options", choices = c("dodged groupings"="dodgex", "horizontal facets"="hzfacet", "horizontal legend"="hzlegend", "visible color legend"="legend"), selected = c("hzfacet", "hzlegend", "dodgex", "legend")),
                                    virtualSelectInput("summary_plot_palette", "Palette", choices = c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"), selected = "turbo", multiple = F, placeholder = "Palette", optionHeight = "24rem", width = "100%")
                                    )
                                )
                            )
                        )
                    ),
                    layout_columns(col_widths = c(6, 6),
                        virtualSelectInput("summary_plot_color", NULL, choices = c("Dataset", "Series", "Event #", "Interval"), multiple = T, maxValues = 1, placeholder = "color", optionHeight = "24rem", width = "100%"),
                        virtualSelectInput("summary_plot_facet", NULL, choices = c("Dataset", "Series", "Event #", "Interval"), multiple = T, maxValues = 1, placeholder = "facet", optionHeight = "24rem", width = "100%")
                    ),
                    plotlyOutput("summary_boxplot", fill = T)
                )
            ),

            navset_card_tab(
                nav_panel("Summary",
                    layout_columns(col_widths = c(6, 6),
                        card(fill = T,
                            card_body(class = "p-0",
                                div(style = "font-size: 0.75rem;" ,
                                    #tags$pre(style = "padding: 0;",
                                        verbatimTextOutput("summary_summary"),
                                    #),
                                    #tags$pre(style = "padding: 0;",
                                        verbatimTextOutput("summary_anova")
                                    #)
                                )
                            )
                        ),
                        card(fill = T,
                            card_body(class = "p-0",
                                div(style = "font-size: 0.75rem;",
                                    verbatimTextOutput("summary_model")
                                )
                            )

                        )
                    ),
                ),

                nav_panel("Diagnostics",
                    layout_columns(col_widths = c(4, 4, 4),
                        plotOutput("summary_qqplot", fill = T),
                        plotOutput("summary_residual_vfit", fill = T),
                        plotOutput("summary_residual_dist", fill = T)
                    )
                ),

                nav_menu("Data",
                    nav_item(downloadLink("summary_download_csv", label = "save as *.csv")),
                    nav_item(downloadLink("summary_download_xlsx", label = "save as *.xlsx"))
                ),

                nav_panel("Packages",
                    layout_columns(col_widths = c(6, 6),
                        tags$small("The currently installed versions of the packages lme4 (for mixed models), base stats (for one-/two-way models), and car (for type II & III sums of squares). "),
                        DT::DTOutput("summary_pkg")
                    )
                )
            )
        )

    )
)
