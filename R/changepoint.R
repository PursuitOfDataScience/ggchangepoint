#' Changepoint wrapper
#'
#' This function wraps a number of \code{cpt} functions from the changepoint
#' package and the \code{cpt.np()} function from the changepoint.np package.
#' It is handy that users can use this function to get the same changepoint
#' results as these functions output individually. Moreover, it returns a tibble
#' that inherits the tidyverse style. Functions from the changepoint package do
#' require data normality assumption by default, yet changepoint.np is a
#' non-parametric way to detect changepoints and let data speak by itself.
#' If user sets \code{change_in} as \code{np} (or \code{cpt_np}), a seed should
#' be set before using the function for the sake of reproducibility. For more
#' details on the changepoint and changepoint.np packages, please refer to
#' their documentation.
#'
#' @param data A numeric vector.
#' @param change_in Choice of \code{mean_var}, \code{mean}, \code{var}, and
#'   \code{np} (or \code{cpt_np} for backward compatibility). Each choice
#'   corresponds to \code{cpt.meanvar()}, \code{cpt.mean()}, \code{cpt.var()}
#'   and \code{cpt.np()} respectively. The default is \code{mean_var}.
#' @param cp_method A wide range of choices (i.e., \code{AMOC}, \code{PELT},
#'   \code{SegNeigh} or \code{BinSeg}). Please note when \code{change_in} is
#'   \code{np} or \code{cpt_np}, \code{PELT} is the only option.
#' @param ... Extra arguments for each \code{cpt} function mentioned in the
#'   \code{change_in} section.
#'
#' @return A tibble including which point(s) is/are the changepoint along with
#'   raw changepoint value corresponding to that changepoint. Changepoint
#'   locations follow the convention of the \code{changepoint} package: the
#'   last index of the left segment.
#' @import changepoint
#' @import changepoint.np
#' @import tibble
#' @import Rdpack
#' @references
#' \insertRef{killick2014changepoint}{ggchangepoint}
#' @export
#'
#' @examples
#' set.seed(2022)
#' cpt_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#' cpt_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#'
cpt_wrapper <- function(data,
                        change_in = "mean_var",
                        cp_method = "PELT",
                        ...){

  change_in <- match.arg(change_in, c("mean_var", "mean", "var", "np", "cpt_np"))

  if (!is.numeric(data)) {
    stop("`data` must be numeric.", call. = FALSE)
  }
  data <- as.numeric(data)
  if (anyNA(data) || any(!is.finite(data))) {
    stop("`data` must be finite (no NA/NaN/Inf).", call. = FALSE)
  }
  if (length(data) < 3) {
    stop("`data` must have at least 3 observations.", call. = FALSE)
  }

  cp_method <- match.arg(cp_method, c("AMOC", "PELT", "SegNeigh", "BinSeg"))

  cpt_fun <- switch(change_in,
    mean_var = changepoint::cpt.meanvar,
    mean     = changepoint::cpt.mean,
    var      = changepoint::cpt.var,
    np       = changepoint.np::cpt.np,
    cpt_np   = changepoint.np::cpt.np
  )

  fit <- cpt_fun(data, method = cp_method, ...)
  cp <- changepoint::cpts(fit)

  tibble::tibble(cp = cp, cp_value = data[cp])
}


#' Plot for the changepoint package
#'
#' The plot for changepoints detected by the changepoint package is a line plot
#' for the raw data and the vertical lines representing each changepoint. The
#' x-axis is the row number of the raw data in the original data vector. The
#' plot inherits ggplot2, meaning users can add ggplot2 functions on top the
#' changepoint plot for customization.
#'
#' @inheritParams  cpt_wrapper
#' @param cptline_alpha The value of alpha for the vertical changepoint line(s),
#'   default is 1, meaning no transparency.
#' @param cptline_color The color for the vertical changepoint line(s), default
#'   is \code{blue}.
#' @param cptline_type The linetype for the vertical changepoint line(s),
#'   default is \code{solid}.
#' @param cptline_linewidth The linewidth for the vertical changepoint line(s),
#'   default is \code{0.5}.
#' @param cptline_size Deprecated. Use \code{cptline_linewidth} instead.
#' @param index Optional. A vector of x-axis labels (e.g. dates) of the same
#'   length as \code{data}.
#' @param show_points Logical. Whether to draw data points. Defaults to
#'   \code{TRUE} when \code{length(data) <= 500}, \code{FALSE} otherwise.
#' @param show_line Logical. Whether to draw the line. Defaults to \code{TRUE}.
#'
#' @return A line plot with data points along with the vertical lines
#'   representing changepoints.
#' @export
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @examples
#' ggcptplot(c(rnorm(100,0,1),rnorm(100,0,10)))
#' ggcptplot(c(rnorm(100,0,1),rnorm(100,10,1)))
#'
ggcptplot <- function(data,
                      change_in = "mean_var",
                      cp_method = "PELT",
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
    lifecycle::deprecate_soft("0.2.0", "ggcptplot(cptline_size)", "ggcptplot(cptline_linewidth)")
    cptline_linewidth <- cptline_size
  }

  if (is.null(show_points)) {
    show_points <- length(data) <= 500
  }

  result <- cpt_wrapper(data, change_in, cp_method, ...)
  ggcptplot_internal(data, result, cptline_alpha, cptline_color,
                     cptline_type, cptline_linewidth, index,
                     show_points, show_line)
}
