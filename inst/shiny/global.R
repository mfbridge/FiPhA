directories = c(

)

default = list(
    # values to interpret as missing when importing csv/xlsx files
    missing_values = c("", ".", "-", "NA"),

    # fonts to choose from in plots
    fonts = c("Arial", "Open Sans", "Times New Roman", "Courier New",  "Garamond", "Verdana", "Georgia", "Comic Sans MS"),

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
        name=c("baseline", "event","post"),
        reference=c("before event", "event signal","after event"),
        start=c(-5, NA, 0),
        end=c(0, NA, 5)
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
    "splines",
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

options(shiny.maxRequestSize = 1024^3)
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

palettes = list()
palettes = append(palettes, ggprism::ggprism_data$colour_palettes)
palettes = palettes[!(names(palettes) %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"))]
palettes = append(palettes, list(
    viridis = viridis::viridis_pal(option = "viridis")(6),
    magma = viridis::viridis_pal(option = "magma")(6),
    plasma = viridis::viridis_pal(option = "plasma")(6),
    inferno = viridis::viridis_pal(option = "inferno")(6),
    cividis = viridis::viridis_pal(option = "cividis")(6),
    mako = viridis::viridis_pal(option = "mako")(6),
    rocket = viridis::viridis_pal(option = "rocket")(6),
    turbo = viridis::viridis_pal(option = "turbo")(6)
))
palettes = append(palettes, list(
    viridis_reverse = viridis::viridis_pal(option = "viridis", direction = -1)(6),
    magma_reverse = viridis::viridis_pal(option = "magma", direction = -1)(6),
    plasma_reverse = viridis::viridis_pal(option = "plasma", direction = -1)(6),
    inferno_reverse = viridis::viridis_pal(option = "inferno", direction = -1)(6),
    cividis_reverse = viridis::viridis_pal(option = "cividis", direction = -1)(6),
    mako_reverse = viridis::viridis_pal(option = "mako", direction = -1)(6),
    rocket_reverse = viridis::viridis_pal(option = "rocket", direction = -1)(6),
    turbo_reverse = viridis::viridis_pal(option = "turbo", direction = -1)(6)
))


palette_index = function(pal, n, viridis.max = 6) {
    if (is.null(pal)) return("#ff00ff")

    if (length(pal) > 1) {
        return(pal[1 + n %% length(pal)])
    } else if (pal %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
        return(viridis::viridis_pal(begin = 0, end = 1, direction = 1, option = pal)(viridis.max)[n])

    } else if (pal %in% c("viridis_reverse", "magma_reverse", "plasma_reverse", "inferno_reverse", "cividis_reverse", "mako_reverse", "rocket_reverse", "turbo_reverse")) {
        return(viridis::viridis_pal(begin = 0, end = 1, direction = -1, option = str_replace(pal, "_reverse", ""))(viridis.max)[n])

    } else {
        if (pal %in% names(palettes)) {
            idx = 1 + (n-1) %% length(palettes[[pal]])
            return(palettes[[pal]][idx]) # cycle through colors
        } else {
            return("#ff00ff")
        }
    }
}
