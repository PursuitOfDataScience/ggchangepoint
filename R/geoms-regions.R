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

# Internal: the fixed styling parameters a layer passes, minus any the caller
# has MAPPED. In ggplot2 a fixed parameter silently beats a mapping, so a
# layer that always passes `fill = "steelblue"` cannot be filled by a column:
# `geom_cpt_region(aes(xmin, xmax, fill = level))` drew every band
# steelblue, `geom_cpt_label(aes(..., colour = change))` drew no borders,
# and geom_cpt_event() coloured its rules by `kind` while its labels stayed
# grey30. The styling arguments are passed only when not mapped. `colour` and `color` are one aesthetic.
#' @noRd
unmapped_params <- function(mapping, params) {
  keep <- vapply(names(params), function(nm) {
    alias <- if (nm %in% c("colour", "color")) c("colour", "color") else nm
    !any(vapply(alias, function(a) aes_has(mapping, a), logical(1)))
  }, logical(1))
  params[keep]
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
#' @param alpha Fill transparency. Defaults to \code{0.2}: light enough that
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
  la <- layer_args(list(...), inherit.aes)
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomCptRegion, position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    key_glyph = la$key_glyph,
    params = c(list(na.rm = na.rm),
               unmapped_params(mapping, list(alpha = alpha, fill = fill)),
               la$params)
  )
}

#' @export
GeomCptRegion <- ggplot2::ggproto("GeomCptRegion", ggplot2::GeomRect,
  required_aes = c("xmin", "xmax"),
  # Declared, or a mapped height is dropped as an "unknown aesthetic".
  optional_aes = c("ymin", "ymax"),
  default_aes = ggplot2::aes(colour = NA, fill = "steelblue", linewidth = 0.5,
                             linetype = 1, alpha = 0.2),
  # Full panel height unless the caller maps a height: a region is an
  # interval in time, and -Inf/Inf are drawn to the panel's edges without
  # entering the y scale.
  setup_data = function(self, data, params) {
    if (is.null(data$ymin)) data$ymin <- -Inf
    if (is.null(data$ymax)) data$ymax <- Inf
    ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_data(data, params)
  },
  draw_key = draw_key_cpt_region
)

#' Changepoint label geom
#'
#' Draws labelled regions behind a series: the central object of supervised
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
  la <- layer_args(list(...), inherit.aes)
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity",
    geom = GeomCptLabel, position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    key_glyph = la$key_glyph,
    params = c(list(na.rm = na.rm),
               unmapped_params(mapping, list(alpha = alpha, colour = colour)),
               la$params)
  )
}

#' @export
GeomCptLabel <- ggplot2::ggproto("GeomCptLabel", GeomCptRegion,
  default_aes = ggplot2::aes(colour = NA, fill = "grey35", linewidth = 0.5,
                             linetype = 1, alpha = 0.25)
)

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
#' @param angle Text angle in degrees. Defaults to \code{90}, because event labels
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
    cpt_abort("`geom_cpt_event()` needs a mapping with at least `xintercept`.",
              class = "bad_argument")
  }
  has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
  if (is.null(repel)) repel <- has_ggrepel && aes_has(mapping, "y")
  validate_flag(repel, "repel")
  if (isTRUE(repel) && !has_ggrepel) {
    cpt_abort("`repel = TRUE` needs the ggrepel package. ", "Install it with ",
               "install.packages('ggrepel'), or pass ", "`repel = FALSE`.",
              class = "engine_missing")
  }

  # One layer draws both marks: the rule and its label, which is what lets
  # the event have its own legend glyph (a dotted rule with a flag) and
  # take part in the position system. A colour or linetype the caller maps
  # must reach the marks, so the fixed defaults are passed only when not
  # mapped: in ggplot2 a fixed parameter silently beats a mapping.
  styling <- unmapped_params(mapping, list(colour = colour,
                                           linetype = linetype))
  if (!isTRUE(repel)) {
    return(ggplot2::layer(
      data = data, mapping = mapping, stat = "identity",
      geom = GeomCptEvent, position = "identity", show.legend = NA,
      inherit.aes = inherit.aes,
      params = c(list(na.rm = na.rm, angle = angle, size = size,
                      vjust = vjust, hjust = hjust), styling, list(...))))
  }
  # With ggrepel the labels are placed by its own grob, so the rules are
  # this package's layer (without labels) and the text is ggrepel's.
  rule_map <- mapping[setdiff(names(mapping), c("label", "y"))]
  class(rule_map) <- class(mapping)
  rule <- ggplot2::layer(
    data = data, mapping = rule_map, stat = "identity", geom = GeomCptEvent,
    position = "identity", show.legend = NA, inherit.aes = inherit.aes,
    params = c(list(na.rm = na.rm, draw_label = FALSE), styling))
  text_map <- mapping
  if (!aes_has(text_map, "x") && aes_has(text_map, "xintercept")) {
    text_map[["x"]] <- text_map[["xintercept", exact = TRUE]]
  }
  text_map[["xintercept"]] <- NULL
  text_map[["linetype"]] <- NULL
  class(text_map) <- class(mapping)
  text_params <- unmapped_params(text_map, list(colour = colour))
  text_layer <- do.call(ggrepel::geom_text_repel, c(
    list(mapping = text_map, data = data, size = size, angle = angle,
         na.rm = na.rm, inherit.aes = inherit.aes),
    text_params, list(...)))
  list(rule, text_layer)
}

#' @export
GeomCptEvent <- ggplot2::ggproto("GeomCptEvent", ggplot2::Geom,
  required_aes = "xintercept",
  optional_aes = c("label", "y"),
  non_missing_aes = c("colour", "linetype"),
  default_aes = ggplot2::aes(colour = "grey30", linewidth = 0.5,
                             linetype = "dotted", alpha = NA, label = NA,
                             y = -Inf),
  extra_params = c("na.rm", "angle", "size", "vjust", "hjust",
                   "draw_label", "family", "fontface"),
  draw_panel = function(data, panel_params, coord, angle = 90, size = 3,
                        vjust = -0.4, hjust = 0, draw_label = TRUE,
                        family = "", fontface = 1) {
    ranges <- coord$backtransform_range(panel_params)
    rule <- data
    rule$x <- rule$xintercept
    rule$xend <- rule$xintercept
    rule$y <- ranges$y[1]
    rule$yend <- ranges$y[2]
    grobs <- list(ggplot2::GeomSegment$draw_panel(unique0_rows(rule),
                                                  panel_params, coord))
    labelled <- draw_label && "label" %in% names(data) &&
      !all(is.na(data$label))
    if (labelled) {
      txt <- data[!is.na(data$label), , drop = FALSE]
      txt$x <- txt$xintercept
      pts <- coord$transform(txt, panel_params)
      grobs[[2]] <- grid::textGrob(
        as.character(txt$label), x = pts$x, y = pts$y, rot = angle,
        hjust = hjust, vjust = vjust, default.units = "native",
        gp = grid::gpar(col = ggplot2::alpha(txt$colour, txt$alpha),
                        fontsize = size * ggplot2::.pt,
                        fontfamily = family, fontface = fontface))
    }
    grid::gTree(children = do.call(grid::gList, grobs))
  },
  draw_key = draw_key_cpt_event
)

# Internal: one rule per distinct location and style, so an event listed
# twice is not overdrawn.
#' @noRd
unique0_rows <- function(d) {
  keep <- intersect(c("x", "xend", "y", "yend", "colour", "linewidth",
                      "linetype", "alpha", "PANEL", "group"), names(d))
  d[!duplicated(d[, keep, drop = FALSE]), , drop = FALSE]
}

#' Significance regions computed in the layer
#'
#' The region twin of \code{\link{stat_changepoint}()}: runs detection on
#' the layer's own \code{x}/\code{y} and draws what the method returns as
#' an interval, so
#' \code{ggplot(d, aes(t, y)) + geom_line() + stat_cpt_region()} is the
#' whole plot. Narrowest Significance Pursuit (\code{method = "nsp"}, the
#' default) returns regions, each containing a changepoint at a global
#' significance level; a method with location intervals (\code{"smuce"},
#' \code{"strucchange"}, \code{"segmented"}, ...) draws those instead.
#'
#' @inheritParams stat_changepoint
#' @param geom The geom to draw with. Defaults to
#'   \code{\link{geom_cpt_region}()}'s.
#' @param method Detection method: \code{"nsp"} (the default) or any method
#'   whose result carries regions or location intervals (see
#'   \code{cpt_methods()$ci}).
#' @param ... Further arguments: those \code{cpt_detect()} takes (for
#'   example \code{alpha} for NSP's level) go to the detector, the rest to
#'   the geom.
#' @return A ggplot layer.
#' @export
#' @family ggplot2 layers
#' @examplesIf requireNamespace("nsp", quietly = TRUE)
#' library(ggplot2)
#' set.seed(2026)
#' d <- data.frame(t = 1:200, y = c(rnorm(100), rnorm(100, 2)))
#' ggplot(d, aes(t, y)) + stat_cpt_region(seed = 1) + geom_line()
stat_cpt_region <- function(mapping = NULL, data = NULL, geom = GeomCptRegion,
                            position = "identity", ..., method = "nsp",
                            na.rm = FALSE, show.legend = NA) {
  dots <- list(...)
  geom_args <- c("alpha", "fill", "colour", "color", "linewidth",
                 "linetype")
  detect_args <- dots[setdiff(names(dots), geom_args)]
  ggplot2::layer(
    stat = StatCptRegion, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = TRUE,
    params = c(list(method = method, detect_args = detect_args,
                    na.rm = na.rm), dots[intersect(names(dots), geom_args)])
  )
}

#' @export
StatCptRegion <- ggplot2::ggproto("StatCptRegion", ggplot2::Stat,
  required_aes = c("x", "y"),
  dropped_aes = c("x", "y"),
  compute_group = function(data, scales, method = "nsp",
                           detect_args = list()) {
    ord <- order(data$x)
    y <- data$y[ord]
    x <- data$x[ord]
    fit <- do.call(cpt_detect, c(list(y, method = method), detect_args))
    reg <- fit$regions
    if (!is.null(reg) && nrow(reg)) {
      lo <- reg$start
      hi <- reg$end
    } else if (all(c("ci_lower", "ci_upper") %in% names(fit$changepoints))) {
      cp <- fit$changepoints
      cp <- cp[!is.na(cp$ci_lower) & !is.na(cp$ci_upper), , drop = FALSE]
      lo <- cp$ci_lower
      hi <- cp$ci_upper
    } else if (nrow(fit$changepoints) == 0L) {
      return(data.frame())
    } else {
      reg <- builtin_registry()
      with_ci <- reg$method[reg$ci]
      cpt_abort("`method = \"", method, "\"` returns neither regions nor ",
                "location intervals, so `stat_cpt_region()` has nothing to ",
                "draw. Methods that do: ", paste(with_ci, collapse = ", "),
                ". stat_changepoint() draws the changepoints themselves.",
                class = "capability_absent",
                data = list(method = method, supported = with_ci))
    }
    n <- length(x)
    data.frame(xmin = x[pmax(1L, pmin(lo, n))], xmax = x[pmax(1L, pmin(hi, n))])
  }
)
