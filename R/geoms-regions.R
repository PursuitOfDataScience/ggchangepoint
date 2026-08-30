# ---------------------------------------------------------------------------
# 0.5.0 layers: the objects the package could not draw before.
#
#   geom_cpt_region()  a changepoint that IS an interval (NSP significance
#                      regions) -- a shaded band, not a rule
#   geom_cpt_label()   a labelled region of the series (supervised detection)
#   geom_cpt_event()   a real-world event annotated onto the series
# ---------------------------------------------------------------------------

# Internal: does a mapping carry this aesthetic? `[[` with exact = TRUE, not
# `$`: a mapping is a list, and `$x` partially matches `xintercept`.
#' @noRd
aes_has <- function(mapping, nm) {
  !is.null(mapping) && !is.null(mapping[[nm, exact = TRUE]])
}

# Internal: the full-panel height, supplied as fixed PARAMETERS rather than
# as default aesthetics. Passing `ymin`/`ymax` through `aes()` would mean
# merging two mapping objects and re-attaching the class by hand, which
# reaches into ggplot2's internals; passing them as parameters is the
# documented way to fix an aesthetic and renders identically. They must be
# omitted when the caller maps them, or ggplot2 rejects the duplicate.
#' @noRd
full_height_params <- function(mapping) {
  out <- list()
  if (!aes_has(mapping, "ymin")) out$ymin <- -Inf
  if (!aes_has(mapping, "ymax")) out$ymax <- Inf
  out
}

#' Significance region geom
#'
#' Draws a changepoint that is an \emph{interval}: a vertical band spanning
#' \code{xmin} to \code{xmax} and the full height of the panel. This is the
#' display Narrowest Significance Pursuit needs
#' (\code{\link{nsp_wrapper}()}): NSP returns intervals each of which
#' contains at least one changepoint at a prescribed \emph{global}
#' significance level, which is neither a point estimate nor a confidence
#' interval around one. \code{autoplot()} adds this layer automatically for
#' a result that carries regions.
#'
#' @param mapping Aesthetic mappings. Requires \code{xmin} and \code{xmax};
#'   \code{ymin}/\code{ymax} default to the panel extent, so they need not be
#'   supplied.
#' @param data A data frame of regions, e.g. \code{\link{cpt_regions}()}
#'   output.
#' @param ... Other arguments passed to \code{ggplot2::geom_rect()}.
#' @param alpha Fill transparency. Defaults to \code{0.2} — light enough that
#'   the series stays readable through overlapping bands.
#' @param fill Band fill colour. Defaults to \code{"steelblue"}.
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#' @param show.legend Whether to show a legend.
#' @param inherit.aes Whether to inherit the plot's aesthetics. Defaults to
#'   \code{FALSE}: a region frame has its own columns and nothing to do with
#'   the series' \code{x}/\code{y}.
#'
#' @return A ggplot layer.
#' @seealso \code{\link{nsp_wrapper}()}, \code{\link{cpt_regions}()}.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' regions <- data.frame(xmin = 44, xmax = 57)
#' ggplot(d, aes(t, y)) +
#'   geom_cpt_region(aes(xmin = xmin, xmax = xmax), data = regions) +
#'   geom_line()
#' @family ggplot2 layers
geom_cpt_region <- function(mapping = NULL, data = NULL, ..., alpha = 0.2,
                            fill = "steelblue", na.rm = FALSE,
                            show.legend = NA, inherit.aes = FALSE) {
  do.call(ggplot2::geom_rect, c(
    list(mapping = mapping, data = data, alpha = alpha, fill = fill,
         na.rm = na.rm, show.legend = show.legend,
         inherit.aes = inherit.aes),
    full_height_params(mapping), list(...)
  ))
}

#' Changepoint label geom
#'
#' Draws labelled regions behind a series — the central object of supervised
#' changepoint detection (Hocking et al.), where an expert marks intervals as
#' containing a change or not and the penalty is learned from those labels.
#' Fill defaults to the label's \code{change} status, so a plot of labels
#' reads as a picture of what the expert asserted; map \code{fill} to the
#' \code{status} column of \code{\link{cpt_label_error}()} instead to get the
#' correct / false-positive / false-negative display.
#'
#' @param mapping Aesthetic mappings. Requires \code{xmin} and \code{xmax};
#'   \code{ymin}/\code{ymax} default to the panel extent. \code{fill} is
#'   commonly mapped to \code{change} or \code{status}.
#' @param data A data frame of labels, e.g. \code{\link{cpt_labels}()} output.
#' @param ... Other arguments passed to \code{ggplot2::geom_rect()}.
#' @param alpha Fill transparency. Defaults to \code{0.25}.
#' @param colour Border colour. Defaults to \code{NA} (no border).
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#' @param show.legend Whether to show a legend.
#' @param inherit.aes Whether to inherit the plot's aesthetics. Defaults to
#'   \code{FALSE}.
#'
#' @return A ggplot layer.
#' @seealso \code{\link{cpt_labels}()}, \code{\link{cpt_label_error}()},
#'   \code{\link{scale_fill_cpt_label}()}.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' labs <- cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
#' ggplot(d, aes(t, y)) +
#'   geom_cpt_label(aes(xmin = start, xmax = end, fill = change),
#'                  data = labs) +
#'   geom_line() +
#'   scale_fill_cpt_label()
#' @family ggplot2 layers
geom_cpt_label <- function(mapping = NULL, data = NULL, ..., alpha = 0.25,
                           colour = NA, na.rm = FALSE, show.legend = NA,
                           inherit.aes = FALSE) {
  do.call(ggplot2::geom_rect, c(
    list(mapping = mapping, data = data, alpha = alpha, colour = colour,
         na.rm = na.rm, show.legend = show.legend,
         inherit.aes = inherit.aes),
    full_height_params(mapping), list(...)
  ))
}

#' Colour scales for changepoint labels and label errors
#'
#' A colour-vision-deficiency-safe fill scale covering both vocabularies the
#' supervised-detection displays use: the label's assertion
#' (\code{change} / \code{no_change} / \code{one_change}) and the outcome of
#' scoring a segmentation against it (\code{correct} / \code{false_positive}
#' / \code{false_negative}). Unknown values fall back to grey rather than
#' erroring, so a partially-scored frame still plots.
#'
#' @param ... Passed to \code{ggplot2::scale_fill_manual()} /
#'   \code{ggplot2::scale_colour_manual()}.
#' @param na.value Fill for values outside the vocabulary.
#' @return A ggplot2 scale.
#' @export
#' @family accessibility scales
#' @examples
#' library(ggplot2)
#' labs <- cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
#' ggplot(labs, aes(xmin = start, xmax = end, ymin = 0, ymax = 1,
#'                  fill = change)) +
#'   geom_rect() + scale_fill_cpt_label()
#' # the same palette covers correct / false_positive / false_negative
#' class(scale_colour_cpt_label())
scale_fill_cpt_label <- function(..., na.value = "grey70") {
  ggplot2::scale_fill_manual(values = cpt_label_palette(),
                             na.value = na.value, ...)
}

#' @rdname scale_fill_cpt_label
#' @export
scale_colour_cpt_label <- function(..., na.value = "grey70") {
  ggplot2::scale_colour_manual(values = cpt_label_palette(),
                               na.value = na.value, ...)
}

# Internal: the shared label/status palette. Okabe-Ito hues, which are
# distinguishable under the three common forms of colour-vision deficiency;
# the displays that use it also vary linetype or alpha so colour is never
# the only channel.
#' @noRd
cpt_label_palette <- function() {
  c(
    change         = "#0072B2",  # blue
    no_change      = "#999999",  # grey
    one_change     = "#56B4E9",  # light blue
    correct        = "#009E73",  # bluish green
    false_positive = "#E69F00",  # orange
    false_negative = "#D55E00",  # vermillion
    TP             = "#009E73",
    FP             = "#E69F00",
    FN             = "#D55E00"
  )
}

#' Event annotation geom
#'
#' Marks known real-world events on a changepoint plot: a vertical rule plus
#' a text label. "Changepoint at index 147" is not a finding; "changepoint at
#' 2020-03-11, matching the WHO pandemic declaration" is.
#' \code{\link{cpt_annotate_events}()} produces the frame this layer expects
#' and, crucially, also reports the changepoints no event explains and the
#' events no changepoint found.
#'
#' @param mapping Aesthetic mappings. Requires \code{xintercept} for the rule
#'   and \code{label} for the text.
#' @param data A data frame of events (see
#'   \code{\link{cpt_annotate_events}()}).
#' @param ... Other arguments passed to the text layer.
#' @param colour Rule and text colour. Defaults to \code{"grey30"}.
#' @param linetype Rule linetype. Defaults to \code{"dotted"}.
#' @param angle Text angle in degrees. Defaults to \code{90} — event labels
#'   are usually longer than the space between events.
#' @param size Text size. Defaults to \code{3}.
#' @param vjust,hjust Text justification.
#' @param repel Use \pkg{ggrepel} to keep labels from overlapping? Defaults
#'   to \code{TRUE} when the package is installed. Requires \code{y} to be
#'   mapped as well (ggrepel places text, it cannot infer a height).
#' @param inherit.aes Whether the label layer inherits the plot's
#'   aesthetics. Defaults to \code{FALSE}: an event table has its own
#'   columns, and inheriting the series' \code{x}/\code{y} mapping would
#'   look for columns that are not there.
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#'
#' @return A list of two ggplot layers (a rule and a label), which can be
#'   added to a plot exactly like a single layer.
#' @seealso \code{\link{cpt_annotate_events}()}.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
#' ev <- data.frame(x = 50, label = "policy change")
#' ggplot(d, aes(t, y)) + geom_line() +
#'   geom_cpt_event(aes(xintercept = x, label = label), data = ev,
#'                  repel = FALSE)
#' @family ggplot2 layers
geom_cpt_event <- function(mapping = NULL, data = NULL, ...,
                           colour = "grey30", linetype = "dotted",
                           angle = 90, size = 3, vjust = -0.4, hjust = 0,
                           repel = NULL, inherit.aes = FALSE,
                           na.rm = FALSE) {
  if (is.null(mapping)) {
    stop("`geom_cpt_event()` needs a mapping with at least `xintercept`.",
         call. = FALSE)
  }
  has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
  if (is.null(repel)) repel <- has_ggrepel && aes_has(mapping, "y")
  validate_flag(repel, "repel")
  if (isTRUE(repel) && !has_ggrepel) {
    stop("`repel = TRUE` needs the ggrepel package. ",
         "Install it with install.packages('ggrepel'), or pass ",
         "`repel = FALSE`.", call. = FALSE)
  }

  rule_map <- mapping[intersect(names(mapping), c("xintercept", "colour",
                                                  "color", "linetype",
                                                  "linewidth", "alpha"))]
  class(rule_map) <- class(mapping)
  rule <- ggplot2::geom_vline(
    mapping = rule_map, data = data,
    colour = colour, linetype = linetype, na.rm = na.rm
  )

  # The text layer needs an x, and events arrive with `xintercept`; alias it
  # rather than making the caller map the same column twice.
  #
  # `[[` with exact = TRUE, not `$`: a mapping is a list, and `$x` partially
  # matches `xintercept`, so the guard below silently decided `x` was
  # already mapped and the text layer went out with no x at all.
  text_map <- mapping
  if (!aes_has(text_map, "x") && aes_has(text_map, "xintercept")) {
    text_map[["x"]] <- text_map[["xintercept", exact = TRUE]]
  }
  text_map[["xintercept"]] <- NULL
  if (!aes_has(text_map, "y")) text_map[["y"]] <- ggplot2::aes(y = -Inf)$y
  class(text_map) <- class(mapping)

  text_layer <- if (isTRUE(repel)) {
    ggrepel::geom_text_repel(
      mapping = text_map, data = data, colour = colour, size = size,
      angle = angle, na.rm = na.rm, inherit.aes = inherit.aes, ...
    )
  } else {
    ggplot2::geom_text(
      mapping = text_map, data = data, colour = colour, size = size,
      angle = angle, vjust = vjust, hjust = hjust, na.rm = na.rm,
      inherit.aes = inherit.aes, ...
    )
  }

  list(rule, text_layer)
}
