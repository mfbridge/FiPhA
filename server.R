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
}
