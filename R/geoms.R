#' Changepoint vertical rules geom
#'
#' Draws vertical lines at changepoint locations. Mimics \code{geom_vline}
#' but designed to work with the tidy changepoint data frames returned by
#' the package. Can be used as a standalone layer:
#' \code{geom_changepoint(data = cp_df, aes(xintercept = cp))}.
#'
#' @param mapping Set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   Requires \code{xintercept}.
#' @param data A data frame with changepoint information.
#' @param ... Other arguments passed to \code{geom_vline}.
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#' @inheritParams ggplot2::geom_vline
#'
#' @return A ggplot layer.
#' @export
geom_changepoint <- function(mapping = NULL, data = NULL, ...,
                             na.rm = FALSE, show.legend = NA) {
  ggplot2::geom_vline(
    mapping = mapping,
    data = data,
    ...,
    na.rm = na.rm,
    show.legend = show.legend
  )
}

#' Changepoint segment level geom
#'
#' Draws horizontal segments representing the estimated level of each segment
#' between changepoints. Typically used with data from \code{augment()}.
#'
#' @param mapping Aesthetic mappings. Requires \code{x}, \code{xend}, \code{y},
#'   \code{yend}.
#' @param data A data frame with segment information.
#' @param ... Other arguments passed to \code{geom_segment}.
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#' @inheritParams ggplot2::geom_segment
#'
#' @return A ggplot layer.
#' @export
geom_cpt_segment <- function(mapping = NULL, data = NULL, ...,
                             na.rm = FALSE, show.legend = NA) {
  ggplot2::geom_segment(
    mapping = mapping,
    data = data,
    ...,
    na.rm = na.rm,
    show.legend = show.legend
  )
}

#' Changepoint confidence interval geom
#'
#' Draws horizontal whiskers for changepoint-location confidence
#' intervals (e.g. from MOSUM, stepR, strucchange, segmented).
#'
#' @param mapping Aesthetic mappings. Requires \code{x}, \code{xmin}, \code{xmax},
#'   and \code{y}.
#' @param data A data frame with CI information.
#' @param ... Other arguments passed to \code{geom_errorbarh}.
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#' @inheritParams ggplot2::geom_errorbarh
#'
#' @return A ggplot layer.
#' @export
geom_cpt_ci <- function(mapping = NULL, data = NULL, ...,
                        na.rm = FALSE, show.legend = NA) {
  ggplot2::geom_errorbarh(
    mapping = mapping,
    data = data,
    ...,
    na.rm = na.rm,
    show.legend = show.legend
  )
}

#' Changepoint detection stat
#'
#' Runs changepoint detection inside the ggplot pipeline. Useful for
#' quick exploration: \code{ggplot(df, aes(t, y)) + geom_line() +
#' stat_changepoint(method = "pelt")}. Draws vertical lines at detected
#' changepoint locations.
#'
#' @param mapping Aesthetic mappings.
#' @param data A data frame.
#' @param geom The geometric object to use (default: "vline").
#' @param position Position adjustment.
#' @param ... Other arguments passed to the geom.
#' @param method Detection method (passed to \code{cpt_detect}).
#' @param change_in What to detect change in (passed to \code{cpt_detect}).
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#' @inheritParams ggplot2::layer
#'
#' @return A ggplot layer.
#' @export
stat_changepoint <- function(mapping = NULL, data = NULL,
                             geom = "vline", position = "identity",
                             ...,
                             method = "pelt",
                             change_in = "mean",
                             na.rm = FALSE,
                             show.legend = NA) {
  ggplot2::layer(
    stat = StatChangepoint,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      method = method,
      change_in = change_in,
      na.rm = na.rm,
      ...
    )
  )
}

StatChangepoint <- ggplot2::ggproto("StatChangepoint", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, method = "pelt", change_in = "mean") {
    result <- cpt_detect(data$y, method = method, change_in = change_in)
    cp <- result$changepoints
    if (nrow(cp) == 0) {
      return(data.frame())
    }
    data.frame(
      xintercept = cp$cp
    )
  }
)
