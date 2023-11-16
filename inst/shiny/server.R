# Shiny server definitions

server = function(input, output, session) {
    # markdown help documentation
    #observe_helpers(help_dir = "help", withMathJax = T)

    data = reactiveValues(
        # metadata
        meta = list(),

        # data
        raw = list(),

        # model history
        models = list(),

        # event series configuration
        series = list(),

        # parsed events
        events = list(),

        # analysis tables (acf, means, etc.)
        analysis = list()
    )

    # TODO: figure out a nice way to keep track of last used options between opening different instances of the same dialogs
    last = reactiveValues()

    # Collect and reduce available variables to the common ones between multiple datasets
    getCommonDatasetVariables = function(x) {
        varlists = list()
        for (r in x) {
            varlists = append(varlists, list(colnames(data$raw[[r]])))
        }
        Reduce(intersect, varlists)
    }

    # data tab
    source("datasets/import.R", local = T)
    source("datasets/variables.R", local = T)
    source("datasets/preview.R", local = T)
    source("datasets/align.R", local = T)
    source("datasets/subset.R", local = T)
    source("datasets/rename.R", local = T)
    source("datasets/baseline.R", local = T)
    source("datasets/save.R", local = T)
    source("datasets/ratio.R", local = T)
    source("datasets/custom.R", local = T)
    source("datasets/remove.R", local = T)
    source("datasets/down.R", local = T)

    # events tab
    source("events/new.R", local = T)
    source("events/conditions.R", local = T)
    source("events/filters.R", local = T)
    source("events/preview.R", local = T)
    source("events/intervals.R", local = T)
    source("events/fixed.R", local = T)
    source("events/binary.R", local = T)
    source("events/peak.R", local = T)
    source("events/bin.R", local = T)
    source("events/signals.R", local = T)

    # TODO: Get rid of this
    source("shiny/events.R", local = T)

    # analysis tabs
    source("shiny/analysis.R", local = T)
    source("analysis/heatmap.R", local = T)
    source("analysis/traces.R", local = T)
    source("analysis/boxplots.R", local = T)
    source("analysis/lag.R", local = T)
    source("analysis/power.R", local = T)

    # export tab
    source("shiny/export.R", local = T)

    # check for updates
    src = devtools::package_info("FiPhA", dependencies = F)$source

    if (src == "local") {
        cli_inform(sprintf("Skipping check for updates, FiPhA was installed from a local source."))

    } else {
        local.hash = stringr::str_match(src, "^Github \\(mfbridge\\/FiPhA@(.*)\\)$")[[1, 2]]
        if (src == "local") local.hash = "local"

        json = jsonlite::fromJSON("https://api.github.com/repos/mfbridge/FiPhA/commits/pkg")

        remote.hash = json$sha
        remote.date = json$commit$author$date

        if (local.hash != remote.hash) {
            show_toast(title = "Update Available!",
                HTML(sprintf("A new update is available to the development branch, use the RStudio addin menu to upgrade.<br/><span style='color: #d0d0d0;'>(current: %s; remote: %s, dated %s)</span>",
                    stringr::str_sub(local.hash, 1, 8),
                    stringr::str_sub(remote.hash, 1, 8),
                    remote.date)
                ), timer = 30000)
        }
    }
}
