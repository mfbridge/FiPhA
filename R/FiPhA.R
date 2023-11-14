#' Start FiPhA's Shiny Application
#'
#' @return
#' @export
#'
FiPhA = function() {
    shiny::runApp(system.file("shiny/", package = "FiPhA"))
}

#' Update FiPhA from its GitHub repo
#'
#' @param force.update upgrade even if already the latest version
#' @param upgrade.dependencies upgrade dependencies if available (may need to be compiled from source)
#'
#' @return
#' @export
#'
update_FiPhA = function(force.update = F, upgrade.dependencies = F) {
    src = devtools::package_info("FiPhA", dependencies = F)$source

    if (!force.update & src == "local") {
        cli::cli_inform(sprintf("Skipping update, FiPhA was installed from a local source. Enter `FiPhA::%s` in the console to upgrade anyway.", cli::col_br_white('update_FiPhA(force = T)')))

    } else {
        local.hash = stringr::str_match(src, "^Github \\(mfbridge\\/FiPhA@(.*)\\)$")[[1, 2]]
        if (src == "local") local.hash = "local"

        json = jsonlite::fromJSON("https://api.github.com/repos/mfbridge/FiPhA/commits/pkg")
        remote.hash = json$sha
        remote.date = json$commit$author$date

        cli::cli_inform(sprintf("Currently installed FiPhA commit hash is %s, latest available is %s (dated %s).",
            cli::col_green(stringr::str_sub(local.hash, 1, 8)),
            cli::col_br_green(stringr::str_sub(remote.hash, 1, 8)),
            cli::col_yellow(remote.date)
            ))

        if (force.update | local.hash != remote.hash) {
            unloadNamespace("FiPhA")
            devtools::install_github('mfbridge/FiPhA@pkg', force = T, upgrade = upgrade.dependencies)
        } else if (local.hash == remote.hash) {
            cli::cli_inform(sprintf("Skipping update, the latest version is already installed. Enter `FiPhA::%s` in the console to upgrade anyway.", cli::col_br_white('update_FiPhA(force = T)')))
        }
    }
}

