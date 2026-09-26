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
#'
#' @return A ggplot layer.
#' @export
#' @family ggplot2 layers
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' cp <- data.frame(cp = cpt_detect(d$y, method = "pelt")$changepoints$cp)
#' ggplot(d, aes(t, y)) + geom_line() +
#'   geom_changepoint(aes(xintercept = cp), data = cp, colour = "blue")
geom_changepoint <- function(mapping = NULL, data = NULL, ...,
                             na.rm = FALSE, show.legend = NA) {
  dots <- list(...)
  # geom_vline()'s shorthand: a fixed `xintercept` makes its own data, as
  # the layer this used to delegate to did.
  if (!is.null(dots$xintercept)) {
    data <- data.frame(xintercept = dots$xintercept)
    mapping <- ggplot2::aes(xintercept = xintercept)
    dots$xintercept <- NULL
    show.legend <- FALSE
  }
  la <- layer_args(dots, FALSE)
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomChangepoint, position = "identity",
    show.legend = show.legend, inherit.aes = la$inherit,
    key_glyph = la$key_glyph, params = c(list(na.rm = na.rm), la$params)
  )
}

#' ggproto objects for the changepoint layers
#'
#' The layers are real \pkg{ggplot2} extensions: each \code{geom_*()} is a
#' layer built on one of these, with its own default aesthetics, required
#' aesthetics and legend glyph, so they take part in scales, guides and the
#' position system like any geom, and can be extended with
#' \code{ggplot2::ggproto()}. Until 0.6.0 every layer was a thin wrapper
#' around a stock geom and borrowed its glyph.
#' \describe{
#'   \item{\code{GeomChangepoint}}{a vertical rule at each \code{xintercept}
#'     (\code{\link{geom_changepoint}()}); key: a vertical rule.}
#'   \item{\code{GeomCptSegment}}{a segment level
#'     (\code{\link{geom_cpt_segment}()}).}
#'   \item{\code{GeomCptCi}}{a horizontal interval with caps
#'     (\code{\link{geom_cpt_ci}()}); key: the same interval.}
#'   \item{\code{GeomCptRegion}}{a band from \code{xmin} to \code{xmax},
#'     full height unless \code{ymin}/\code{ymax} are mapped
#'     (\code{\link{geom_cpt_region}()}); key: a shaded band.}
#'   \item{\code{GeomCptLabel}}{a labelled interval behind the series
#'     (\code{\link{geom_cpt_label}()}).}
#'   \item{\code{GeomCptEvent}}{a dotted rule with its label
#'     (\code{\link{geom_cpt_event}()}); key: a dotted rule with a flag.}
#'   \item{\code{StatChangepoint}, \code{StatCptRegion}}{detection inside the
#'     layer (\code{\link{stat_changepoint}()},
#'     \code{\link{stat_cpt_region}()}).}
#' }
#' @name ggchangepoint-ggproto
#' @aliases GeomChangepoint GeomCptSegment GeomCptCi GeomCptRegion
#'   GeomCptLabel GeomCptEvent StatChangepoint StatCptRegion
#' @format ggproto objects.
#' @family ggplot2 layers
#' @examples
#' library(ggplot2)
#' class(GeomChangepoint)
#' GeomCptRegion$default_aes
NULL

#' @export
GeomChangepoint <- ggplot2::ggproto("GeomChangepoint", ggplot2::GeomVline,
  draw_key = draw_key_cpt_rule
)

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
#'
#' @return A ggplot layer.
#' @export
#' @family ggplot2 layers
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(50), rnorm(50, 4)), method = "pelt")
#' ggplot(fit$data, aes(index, value)) + geom_line(colour = "grey70") +
#'   geom_cpt_segment(aes(x = start, xend = end, y = param_estimate,
#'                        yend = param_estimate),
#'                    data = fit$segments, colour = "blue", linewidth = 1)
geom_cpt_segment <- function(mapping = NULL, data = NULL, ...,
                             na.rm = FALSE, show.legend = NA) {
  la <- layer_args(list(...), TRUE)
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomCptSegment, position = "identity",
    show.legend = show.legend, inherit.aes = la$inherit,
    key_glyph = la$key_glyph, params = c(list(na.rm = na.rm), la$params)
  )
}

#' @export
GeomCptSegment <- ggplot2::ggproto("GeomCptSegment", ggplot2::GeomSegment)

#' Changepoint confidence interval geom
#'
#' Draws horizontal whiskers for changepoint-location confidence
#' intervals (e.g. from MOSUM, stepR, strucchange, segmented).
#'
#' @param mapping Aesthetic mappings. Requires \code{y} (the height at which
#'   to draw the whisker) together with \code{xmin} and \code{xmax}. An
#'   \code{x} aesthetic is accepted but not needed: the layer is a
#'   horizontal error bar, so the interval is given by \code{xmin}/\code{xmax}
#'   and the changepoint itself is usually marked with a separate point layer,
#'   as \code{autoplot(show_ci = TRUE)} does.
#' @param data A data frame with CI information.
#' @param ... Other arguments passed to \code{geom_errorbar} (with
#'   \code{orientation = "y"}; \code{geom_errorbarh()} is deprecated).
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#'
#' @return A ggplot layer.
#' @export
#' @family ggplot2 layers
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' ci <- data.frame(xmin = 45, xmax = 56, y = 0)
#' ggplot(d, aes(t, y)) + geom_line() +
#'   geom_cpt_ci(aes(xmin = xmin, xmax = xmax, y = y), data = ci,
#'               inherit.aes = FALSE, width = 0.4, colour = "blue")
geom_cpt_ci <- function(mapping = NULL, data = NULL, ...,
                        na.rm = FALSE, show.legend = NA) {
  # A horizontal error bar: GeomErrorbar with orientation = "y", which is
  # how ggplot2 draws one since geom_errorbarh() was deprecated in 3.5.0.
  la <- layer_args(list(...), TRUE)
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomCptCi, position = "identity",
    show.legend = show.legend, inherit.aes = la$inherit,
    key_glyph = la$key_glyph,
    params = c(list(orientation = "y", na.rm = na.rm), la$params)
  )
}

#' @export
GeomCptCi <- ggplot2::ggproto("GeomCptCi", ggplot2::GeomErrorbar,
  draw_key = draw_key_cpt_ci
)

#' Changepoint detection stat
#'
#' Runs changepoint detection inside the ggplot pipeline. Useful for
#' quick exploration: \code{ggplot(df, aes(t, y)) + geom_line() +
#' stat_changepoint(method = "pelt")}. Draws vertical lines at detected
#' changepoint locations.
#'
#' @param mapping Aesthetic mappings.
#' @param data A data frame.
#' @param geom The geometric object to use (default: \code{"vline"}). The stat
#'   computes a single \code{xintercept} per changepoint and drops
#'   \code{x}/\code{y}, so \code{"vline"} is the geom that fits. A geom
#'   needing \code{x}/\code{y} (\code{"point"}, and \code{"rug"}, which
#'   consumes \code{x}/\code{y} rather than \code{xintercept}) errors for
#'   that reason. \code{inherit.aes} is fixed at \code{TRUE} here: the stat
#'   re-detects on the plot's own data, so the panel's \code{x}/\code{y}
#'   mapping is what it reads.
#' @param position Position adjustment.
#' @param ... Other arguments passed to the geom.
#' @param method Detection method (passed to \code{cpt_detect}).
#' @param change_in What to detect change in (passed to \code{cpt_detect}).
#' @param na.rm If \code{FALSE}, missing values are removed.
#' @param show.legend Whether to show legend.
#'
#' @return A ggplot layer.
#' @export
#' @family ggplot2 layers
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' ggplot(d, aes(t, y)) + geom_line() +
#'   stat_changepoint(method = "pelt", colour = "blue")
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

#' @export
StatChangepoint <- ggplot2::ggproto("StatChangepoint", ggplot2::Stat,
  required_aes = c("x", "y"),
  dropped_aes = c("x", "y"),
  compute_group = function(data, scales, method = "pelt", change_in = "mean") {
    # Detection assumes a series ordered in x; layer data arrives in row
    # order, which need not be x order.
    ord <- order(data$x)
    y_sorted <- data$y[ord]
    x_sorted <- data$x[ord]

    result <- cpt_detect(y_sorted, method = method, change_in = change_in)
    cp <- result$changepoints
    if (nrow(cp) == 0) {
      return(data.frame())
    }
    # Map index positions to actual x-axis values
    cp_idx <- cp$cp
    cp_idx <- cp_idx[cp_idx >= 1 & cp_idx <= length(x_sorted)]
    data.frame(
      xintercept = x_sorted[cp_idx]
    )
  }
)
