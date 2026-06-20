#' ecp wrapper
#'
#' The ecp package provides a non-parametric way to detect changepoints. Unlike
#' the changepoint package, it does not assume raw data to have any formal
#' distribution. This wrapper function wraps two functions from the ecp package,
#' i.e., \code{e.divisive()} and \code{e.agglo()}. Users can use either function
#' by switching the \code{algorithm} argument. Before using the wrapper function,
#' seed should be set for the sake of reproducibility.
#'
#' @param data A numeric vector (for univariate) or matrix/data.frame (for
#'   multivariate).
#' @param algorithm Either \code{divisive} or \code{agglo}. \code{divisive} is
#'   the default.
#' @param min_size Minimum number of observations between change points. By
#'   default is 2. This argument is only applied when \code{algorithm =
#'   "divisive"}.
#' @param seed Optional. A seed for reproducibility of the stochastic
#'   permutation test.
#' @param ... Extra arguments to pass on either from \code{e.divisive()} or
#'   \code{e.agglo()}.
#'
#' @return A tibble includes which point(s) is/are the changepoint along with
#'   raw changepoint value corresponding to that changepoint. Changepoint
#'   locations follow the \code{ecp} package convention: the first index of the
#'   right segment. When no changepoint is found, an empty tibble is returned
#'   (0 rows).
#' @import tibble
#' @import ecp
#' @import Rdpack
#' @references
#' \insertRef{james2013ecp}{ggchangepoint}
#' @export
#'
#' @examples
#' set.seed(2022)
#' ecp_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ecp_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#'
ecp_wrapper <- function(data,
                        algorithm = "divisive",
                        min_size = 2,
                        seed = NULL,
                        ...){

  algorithm <- match.arg(algorithm, c("divisive", "agglo"))

  if (!is.numeric(data) && !is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a numeric vector, matrix, or data.frame.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  estimates <- if (algorithm == "divisive") {
    ecp::e.divisive(as.matrix(data), min.size = min_size, ...)$estimates
  } else {
    ecp::e.agglo(as.matrix(data), ...)$estimates
  }

  cp <- estimates[2:(length(estimates) - 1)]

  if (length(cp) == 0) {
    return(tibble::tibble(cp = integer(), cp_value = numeric()))
  }

  if (is.vector(data) || is.numeric(data)) {
    cp_value <- data[cp]
  } else {
    cp_value <- rep(NA_real_, length(cp))
  }

  tibble::tibble(cp = cp, cp_value = cp_value)
}



#' Plot for the ecp package
#'
#' The plot for changepoints detected by the ecp package is a line plot for the
#' raw data and the vertical lines representing each changepoint. The x-axis is
#' the row number of the raw data in the original data vector. The plot inherits
#' ggplot2, meaning users can add ggplot2 functions on top the changepoint plot
#' for customization.
#'
#' @inheritParams ecp_wrapper
#' @inheritParams ggcptplot
#'
#' @return A line plot with data points along with the vertical lines
#'   representing changepoints.
#' @export
#'
#' @examples
#' ggecpplot(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ggecpplot(c(rnorm(100,0,1),rnorm(100,10,1)))
#'
ggecpplot <- function(data,
                      algorithm = "divisive",
                      min_size = 2,
                      ...,
                      cptline_alpha = 1,
                      cptline_color = "blue",
                      cptline_type = "solid",
                      cptline_linewidth = 0.5,
                      cptline_size = lifecycle::deprecated(),
                      index = NULL,
                      show_points = NULL,
                      show_line = TRUE){

  if (lifecycle::is_present(cptline_size)) {
    lifecycle::deprecate_soft("0.2.0", "ggecpplot(cptline_size)", "ggecpplot(cptline_linewidth)")
    cptline_linewidth <- cptline_size
  }

  if (is.null(show_points)) {
    show_points <- if (is.vector(data)) length(data) <= 500 else nrow(data) <= 500
  }

  result <- ecp_wrapper(data, algorithm, min_size, ...)
  ggcptplot_internal(data, result, cptline_alpha, cptline_color,
                     cptline_type, cptline_linewidth, index,
                     show_points, show_line)
}
