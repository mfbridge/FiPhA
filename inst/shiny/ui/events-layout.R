
events.ui = nav("Events",
    fluidRow(
        column(6,
            pickerInput("events_dataset", NULL, multiple = F, width = "100%", choices = c(), choicesOpt = list(subtext = c()), options = list(title="Select a dataset to add a new event series"))
        ),

        column(6, style="text-align: center;",
            actionButton("events_new", "Add", class="btn btn-primary action-button btn-xs"),
            #actionButton("events_edit", "Edit", class = "btn btn-success action-button btn-xs"),
            actionButton("events_delete", "Delete", class = "btn btn-error action-button btn-xs")
        )
    ),

    tabsetPanel(
        tabPanel("Preview",
                 tags$br(),
                 fluidRow(
                     #column(5, pickerInput("events_preview_x", NULL, c(), width = "100%", options = list(title="X Axis Variable"))),
                     column(6, pickerInput("events_series", NULL, multiple = T, choices = c(), choicesOpt = list())),
                     column(2, pickerInput("events_series_x", NULL, multiple = F, choices = c("event time", "interval time"), selected = "event time")),
                     #column(5, pickerInput("events_preview_y", NULL, c("One", "two", "33"), width = "100%", multiple = T, options = list(`max-options`=2, title="Y Axis Variable(s)"))),
                     column(2, offset=2, dropdown(size = "xs", status = "primary", icon = icon("cogs"), right = T,
                                        plotOptions("events_options", options = list(
                                            list(type="title", text="Plot Options"),
                                            list(type="font-size+family", size.id="events_font_size", family.id="events_font_Family", text = "Font", size.value = 14, family.value = "Open Sans"),
                                            list(type="theme", id = "events_theme", text = "Theme"),
                                            list(type="checkbox+text", text = "Highlight X Axis", checkbox.id = "events_show_xaxis", checked = T, text.id = "events_title_xaxis", value = "Time"),
                                            list(type="checkbox+text", text = "Highlight Y Axis", checkbox.id = "events_show_yaxis", checked = T, text.id = "events_title_yaxis", value = ""),
                                            list(type="hr"),
                                            list(type="title", text="Line Options"),
                                            list(type="line-size+alpha", size.id="events_y_size", text="Size + Alpha", size.value=0.15, alpha.value = 0.5, alpha.id = "events_y_alpha"),
                                            list(type="palette", text="Palette", palette.id="events_interval_palette",
                                                from.id = "events_interval_from", to.id = "events_interval_to", from.color = "#3f3f3f", to.color = "#a8326d"),
                                            list(type="hr"),
                                            list(type="title", text="Mean +/- SD Options"),
                                            list(type="mean+sd", show.id="events_meansd", n.id = "events_meansd_n", ss.id = "events_meansd_ss", n.value = 3),
                                            list(type="line-size", id="events_meansd_size", text="Line Size", value=0.4),
                                            list(type="line-color", id = "events_meansd_line", value = "#000000", text = "Line Color"),
                                            list(type="line-color", id = "events_meansd_area", value = "#1259ff80", text = "Area Color")
                                        )
                                    )
                            )
                     )
                 ),
                 fluidRow(
                     column(12, plotlyOutput("events_preview", height = "600px") %>% withSpinner()),
                 ))
    ),

)
