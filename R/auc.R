
#' Area Under Curve
#'
#' @description
#' Estimates the area under a given curve using the trapezoidal rule, which is equivalent to the average of a left and right Riemann sum.
#'
#' @param x data values
#' @param t time values (x axis)
#' @param dt optional data.table
#' @param na.rm remove missing values
#'
#' @return total area bounded by the points (t, x)
#'
#' @export
#'
auc = function(x, t = NULL, dt = NULL, na.rm = T) {
    if (!is.null(dt)) {
        weights = c(1, rep(2, length(x) - 2), 1)
        return(sum(x * (weights * dt) / 2, na.rm = na.rm))
    } else {
        L = shift(t, n = -1, fill = t[length(t)]) - t
        R = t - shift(t, n = 1, fill = t[1])
        return(sum(x * (L + R) / 2, na.rm = na.rm))
    }
}
