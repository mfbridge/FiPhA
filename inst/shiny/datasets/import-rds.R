# import-rds.R

observeEvent(input$data_rds, {
    f = rstudioapi::selectFile(
        caption = "Select R Object File (*.rds)",
        label = "Select",
        path = rstudioapi::getActiveProject(),
        filter = "RDS Files (*.rds)",
        existing = T
    )

    if (!is.null(f)) {
        f = normalizePath(f)

        if (file.exists(f)) {
            obj = readRDS(f)
            setCurrentSession(obj)
        } else {
            cli::cli_alert_danger(sprintf("specified file %s does not exist", cli::col_red(f)))
        }
    }

    refreshDatasetChoices()
})

observeEvent(input$data_sample, {
    obj = readRDS(system.file("extdata/FIPHA_SAMPLE_DATASET.rds", package = "FiPhA"))

    setCurrentSession(obj)

    refreshDatasetChoices()
})
