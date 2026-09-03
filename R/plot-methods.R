#' Base plot() methods for ggchangepoint result objects
#'
#' Every result class in the package has an \code{autoplot()} method, but
#' \code{plot()} is the reflex most users reach for first. Without a method,
#' \code{plot()} on a list-shaped result falls through to
#' \code{\link[graphics]{plot.default}} and fails with \code{'x' is a list, but
#' does not have components 'x' and 'y'} -- a message that names neither this
#' package nor \code{autoplot()}. These methods delegate to the corresponding
#' \code{autoplot()} method so that \code{plot(result)} draws the intended
#' figure.
#'
#' The plot is drawn as a side effect and the \code{ggplot} object is returned
#' invisibly, so \code{plot()} works inside a loop or a function while
#' \code{p <- plot(result)} still gives you the object to add layers to.
#'
#' @param x A result object created by one of the package's \code{cpt_*()}
#'   functions.
#' @param ... Passed to the corresponding \code{autoplot()} method.
#' @return The \code{ggplot} object, invisibly. Called for the side effect of
#'   drawing the plot.
#' @seealso \code{\link{autoplot.ggcpt}}; \code{\link{ggcpt_methods}} for the
#'   \code{ggcpt} class itself.
#' @name ggcpt_plot_methods
#' @examples
#' set.seed(2024)
#' x <- c(stats::rnorm(60), stats::rnorm(60, 4))
#'
#' plot(cpt_select(x, method = "pelt", criterion = "bic", k_max = 4))
#' plot(cpt_crops(x, pen_min = 1, pen_max = 30))
#'
#' # the object is still available to build on
#' p <- plot(cpt_stability(x, method = "pelt", B = 20, seed = 1))
#' p + ggplot2::labs(title = "Bootstrap stability")
NULL

# One helper so all fourteen methods agree on what plot() does: dispatch on
# autoplot() (not a hard-coded autoplot.<class>, which would bypass a
# subclass's own method), draw, and return the object invisibly.
plot_via_autoplot <- function(x, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_selection <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_stability <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_sensitivity <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_influence <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_batch <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_benchmark <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_consensus <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_monitor <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_delay <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_path <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_power <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_events <- function(x, ...) plot_via_autoplot(x, ...)

#' @rdname ggcpt_plot_methods
#' @exportS3Method base::plot
plot.ggcpt_label_curve <- function(x, ...) plot_via_autoplot(x, ...)
