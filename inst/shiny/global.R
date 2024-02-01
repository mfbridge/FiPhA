directories = c(

)


default = list(
    # values to interpret as missing when importing csv/xlsx files
    missing_values = c("", ".", "-", "NA"),

    # fonts to choose from in plots
    fonts = c("Arial", "Open Sans", "Times New Roman", "Courier New",  "Garamond", "Verdana", "Georgia", "Comic Sans MS"),

    # guesses for initial parameter values for nls2
    # exp_model_start_params = data.frame(
    #     alpha = c(10, 1, 100, 200, 4000, 1, 1),
    #     beta = c(1, 1, -10, -0.1, -0.001, -0.1, 0.00001),
    #     theta = c(0, 1, 1, 0, 1, 1, -100)
    # ),
    # exp_model_start_params = data.frame(
    #     alpha = c(0, 1e4),
    #     beta = c(-1, -0.001),
    #     theta = c(-10000, 10000)
    # ),

    # data import defaults
    import_header_row = 1,
    import_data_row = 2,
    import_frequency = 25,

    import_spectra_header_row = 17,
    import_spectra_data_row = 18,
    import_spectra_frequency = 25,

    # align import defaults (35/37 for EthoVision output)
    unaligned_header_row = 35,
    unaligned_data_row = 37,
    unaligned_frequency = 25,

    # default interval definition; simple baseline of 5 s prior to event
    intervals = data.frame(
        name=c("baseline", "event"),
        reference=c("before event", "event signal"),
        start=c(-5, NA),
        end=c(0, NA)
    ),

    # list of fixed event times
    fixed_list = data.frame(
        start = c(NA),
        end = c(NA)
    ),

    filters = data.frame(
        rule = c(NA),
        value = c(NA)
    )
)

# load imports for shiny app
packages = c(
    "assertthat",
    "bit64",
    "bslib",
    "checkmate",
    "cli",
    "data.table",
    "excelR",
    "ggh4x",
    "ggplot2",
    "ggprism",
    "ggrepel",
    "gsignal",
    "lubridate",
    #"lme4", # loading the lme4 namespace conflicts with some other packages that are used
    "nlsr",
    "patchwork",
    "plotly",
    "qqplotr",
    "R.utils",
    "readr",
    "readxl",
    "scales",
    "shiny",
    "shinycssloaders",
    "shinyFiles",
    "shinyHugePlot",
    "shinyjs",
    "shinyWidgets",
    "stringr",
    "writexl",
    "devtools",
    "jsonlite",
    "FiPhA"
)

for (p in packages) {
    if (!require(p, character.only = T)) {
        library(p, character.only = T)
    }
}

root.dirs = c(directories, `Home`=path.expand("~"), getVolumes()())

options(shiny.fullstacktrace = T)
options(spinner.type = 8, spinner.color = "#000000")

# helper functions ------------------------------------------------------------------------------------------------

# strip some attributes from some plotly objects to avoid annoying warnings
toWebGL2 = function(gg, type = NULL) {
    gg$x$data = lapply(gg$x$data, \(x) { x$hoveron = NULL; x })
    toWebGL(gg)
}

# shortcut for generating a plotly plot that just prints a centered string
plotlyMessage = function(text, render = T) {
    .plot = plotly_empty() %>%
            layout(title = list(text = text, yref = "paper", y = 0.5)) %>%
            config() %>%
            toWebGL2()
    if (render) {
        renderPlotly(.plot)
    } else {
        .plot
    }
}
