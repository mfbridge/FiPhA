#' Zscore
#'
#' @param x values
#' @param r baseline values (x)
#' @param fun summary function (mean)
#'
#' @return standard z score
#' @export
#'
zscore = function(x, r = x, fun = mean) {
    R = fun(r, na.rm = T)
    (x - R) / sd(r, na.rm = T)
}

#' Robust Z
#'
#' @param x values
#' @param r baseline values
#' @param fun summary function (mean)
#'
#' @return zscore-like values based on medians
#' @export
#'
robustz = function(x, r, fun = mean) {
    R = pctdff(r, r)
    (pctdff(x, x) - median(R)) / mad(R, constant = 1)
}

#' Percent change
#'
#' @param x values
#'
#' @return values as a percent of the first x
#' @export
pctchg = function(x) {
    (x - first(x)) / first(x) * 100
}

#' Percent Delta F over F
#'
#' @param x values
#' @param r baseline values
#' @param fun summary function (mean)
#'
#' @return delta f over f expressed as a percent
#' @export
#'
pctdff = function(x, r, fun = mean) {
    deltaf(x, r, fun = fun) * 100
}

#' Delta F over F
#'
#' @param x values
#' @param r baseline values
#' @param fun summary function (mean)
#'
#' @return percent departure from baseline
#' @export
#'
deltaf = function(x, r, fun = mean) {
    R = fun(r, na.rm = T)
    (x - R) / R
}
