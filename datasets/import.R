# datasets/import.R

source("datasets/import-tabular.R", local = T)
source("datasets/import-spectra.R", local = T)
source("datasets/import-rds.R", local = T)
source("datasets/import-tdt.R", local = T)

refreshDatasetChoices = function () {
    updatePickerInput(session, "data_dataset", choices = names(data$meta))
    updatePickerInput(session, "heatmap_dataset", choices = names(data$meta))
    updatePickerInput(session, "heatmap2_dataset", choices = names(data$meta))
    updatePickerInput(session, "events_dataset", choices = names(data$meta))
    updatePickerInput(session, "power_dataset", choices = names(data$meta))
    updatePickerInput(session, "lag_dataset", choices = names(data$meta))
    updatePickerInput(session, "summary_dataset", choices = names(data$meta))
}
