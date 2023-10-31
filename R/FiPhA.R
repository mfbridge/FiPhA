#' Start FiPhA's Shiny Application
#'
#' @return
#' @export
#'
FiPhA = function() {
    shiny::runApp(system.file("shiny/", package = "FiPhA"))
}
