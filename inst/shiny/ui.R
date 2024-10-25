 # shiny ui layout

# simple ui generator for dropdown plot options
source("ui/plotOptions.R")

# source("ui/help-layout.R", local = T)
source("ui/data-layout.R", local = T)
source("ui/events-layout.R", local = T)
# source("ui/intervals-layout.R", local = T)
source("ui/analysis-layout.R", local = T)
# source("ui/export-layout.R", local = T)

ui = tagList(useShinyjs(),
    tags$head(
        # keep track of which event was fired
        tags$script(
            "$(document).on('shiny:inputchanged', function(event) {
                if (event.name != 'changed') {
                    Shiny.setInputValue('changed', event.name);
                }
            });"
        ),

        # css styling
        tags$link(rel = "stylesheet", type="text/css", href="style.css")
    ),

    page_navbar(title = HTML(sprintf("FiPhA v%s", packageVersion("FiPhA"))), theme = bs_theme(bootswatch = "yeti", font_scale = 1), inverse = F,
        data.ui,
        events.ui,
        analysis.ui,
        tabPanel("Export",
            fluidRow(
                column(4, pickerInput("export_format", "Format", choices = c("Excel Workbook (.xlsx)"="excel", "R object (.rds)"="r"), selected = "excel", multiple = F)),
                column(8,
                    div(id = "export_excel_opts",
                        tags$strong("Exporting as an Excel Workbook"), tags$br(),
                        "Creates an Excel Workbook with a tab for each processed dataset's event data.", tags$br(),
                        prettyCheckboxGroup("export_excel_tabs", "Export Analysis Data", choices = c("interval summaries"="sum")),
                        prettyCheckboxGroup("export_excel_opts_list", "Formatting Options",
                            choices = c("Include variable on event labels"="event_var",
                                        "Pad events with empty columns"="eventspace",
                                        "Add interval name next to each event (all observations)"="event_interval",
                                        "Add interval name next to each event (first observation only)"="event_interval_single",
                                        "Add interval time next to each event"="event_interval_time",
                                        "Add file time next to each event"="event_file",
                                        "Add event time next to each event"="event_event",
                                        "Align intervals across events"="int_align",
                                        "Align events by event time"="event_align",
                                        "Create separate tabs for each interval"="tab_int"),
                                        #"Reverse alignment of padded intervals"="pad_rev")
                            selected = ""),
                        shinySaveButton("export_excel", "Save as...", "Save as...", filetype = list(`.xlsx file`="xlsx"))
                    ),
                    div(id = "export_r_opts",
                        tags$strong("Exporting as an R Object"), tags$br(),
                        "Saves the current session data to an R object file (.rds) that can be imported again or viewed in R/Rstudio.", tags$br(),
                        shinySaveButton("export_r", "Save as...", "Save as...", filetype = list(`.rds file`="rds"))
                    )
                )
            ),
            tags$h5("Plotly Options"),
            fluidRow(
                column(4, virtualSelectInput("plotly_format", "Output image format", choices = c("png", "svg", "jpeg"), selected = "png")),
                column(4, numericInput("plotly_width", "Image width (px)", value = 1280, min = 0, step = 1)),
                column(4, numericInput("plotly_height", "Image height (px)", value = 640, min = 0, step = 1))
            )
        )

    )
)
