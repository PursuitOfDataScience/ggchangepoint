# ---------------------------------------------------------------------------
# Accessibility.
#
# A visualization package that encodes meaning in colour alone, and ships
# figures with no alt text, is failing the readers who most need the picture
# explained. Three things here: a colour-vision-deficiency-safe discrete
# palette used wherever a categorical variable is coloured, redundant
# linetype encoding alongside it, and automatic alt text on every autoplot().
# ---------------------------------------------------------------------------

#' Colour-vision-safe scales for changepoint methods
#'
#' The discrete palette used wherever this package colours by method, series
#' or class. It is the Okabe–Ito qualitative palette, which stays
#' distinguishable under deuteranopia, protanopia and tritanopia, extended by
#' recycling with a linetype change so that colour is never the only channel
#' carrying the distinction.
#'
#' @param ... Passed to \code{ggplot2::discrete_scale()}.
#' @param na.value Colour for missing values.
#' @return A ggplot2 scale.
#' @seealso \code{\link{theme_ggcpt}()},
#'   \code{\link{scale_fill_cpt_label}()}.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() + scale_colour_cpt() + theme_ggcpt()
#' @family accessibility scales
scale_colour_cpt <- function(..., na.value = "grey70") {
  ggplot2::discrete_scale("colour", palette = cpt_pal(),
                          na.value = na.value, ...)
}

#' @rdname scale_colour_cpt
#' @export
scale_color_cpt <- scale_colour_cpt

#' @rdname scale_colour_cpt
#' @export
scale_fill_cpt <- function(..., na.value = "grey70") {
  ggplot2::discrete_scale("fill", palette = cpt_pal(), na.value = na.value,
                          ...)
}

#' @rdname scale_colour_cpt
#' @export
scale_linetype_cpt <- function(...) {
  ggplot2::discrete_scale("linetype", palette = cpt_linetype_pal(), ...)
}

# Internal: the Okabe-Ito qualitative palette, black last so the first eight
# levels are the coloured ones. Recycles beyond eight, which is why the
# linetype scale below exists.
#' @noRd
cpt_palette_values <- function() {
  c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442", "#000000")
}

#' @noRd
cpt_pal <- function() {
  function(n) {
    v <- cpt_palette_values()
    if (n > length(v)) rep(v, length.out = n) else v[seq_len(n)]
  }
}

#' @noRd
cpt_linetype_pal <- function() {
  function(n) {
    v <- c("solid", "22", "42", "44", "13", "1343", "73", "2262")
    if (n > length(v)) rep(v, length.out = n) else v[seq_len(n)]
  }
}

# Internal: a one-paragraph description of a ggcpt result, attached to every
# autoplot() as alt text. ggplot2 >= 3.4 carries `labs(alt = )` through to
# the <img alt> attribute in knitr/quarto output, so this reaches a screen
# reader without the author writing anything.
#' @noRd
cpt_alt_text <- function(object, what = "series") {
  n <- nrow(object$data)
  k <- nrow(object$changepoints)
  where <- if (k == 0) {
    "no changepoints"
  } else {
    locs <- if (!is.null(object$index) &&
                "cp_index" %in% names(object$changepoints)) {
      paste(format(object$changepoints$cp_index), collapse = ", ")
    } else {
      paste(object$changepoints$cp, collapse = ", ")
    }
    paste0(k, " changepoint", if (k > 1) "s" else "", " at ", locs)
  }
  rng <- range(object$data$value, na.rm = TRUE)
  paste0(
    "Line chart of a time series of ", n, " observations ranging from ",
    format(rng[1], digits = 3), " to ", format(rng[2], digits = 3),
    ", with ", where, " marked by vertical rules. Detected with the ",
    object$method, " method on a change in ", object$change_in, ".",
    if (!is.null(object$regions) && nrow(object$regions) > 0) {
      paste0(" ", nrow(object$regions),
             " shaded band(s) mark significance regions.")
    } else {
      ""
    }
  )
}

# Internal: add alt text without failing on an older ggplot2 that has no
# `alt` label. `labs()` accepts arbitrary names, so this is safe, but the
# tryCatch keeps a future stricter ggplot2 from breaking every plot.
#' @noRd
with_alt <- function(p, alt) {
  tryCatch(p + ggplot2::labs(alt = alt), error = function(e) p)
}
