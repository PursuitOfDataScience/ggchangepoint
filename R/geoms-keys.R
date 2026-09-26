# ---------------------------------------------------------------------------
# Shared pieces of the ggproto layers: the legend glyphs and the split of a
# constructor's arguments between layer() and the geom. In a file of their
# own because the geoms in geoms-regions.R and geoms.R are built at load
# time and need these defined first (files load in alphabetical order).
# ---------------------------------------------------------------------------

# Internal: the arguments of a layer constructor that belong to layer()
# rather than to the geom's parameters. `inherit.aes` and `key_glyph`
# arrive through `...` in calls written for the stock geoms these layers
# replaced, and passed on as parameters they were ignored with a warning.
#' @noRd
layer_args <- function(dots, inherit_default) {
  inherit <- dots$inherit.aes %||% inherit_default
  key <- dots$key_glyph
  dots$inherit.aes <- NULL
  dots$key_glyph <- NULL
  list(inherit = inherit, key_glyph = key, params = dots)
}

# Internal: key glyphs. Each draws the mark its layer draws, so a legend
# says which layer is which.
#' @noRd
draw_key_cpt_rule <- function(data, params, size) {
  grid::segmentsGrob(0.5, 0, 0.5, 1, gp = grid::gpar(
    col = ggplot2::alpha(data$colour %||% "black", data$alpha %||% NA),
    lwd = (data$linewidth %||% 0.5) * ggplot2::.pt,
    lty = data$linetype %||% 1, lineend = "butt"))
}

#' @noRd
draw_key_cpt_ci <- function(data, params, size) {
  gp <- grid::gpar(
    col = ggplot2::alpha(data$colour %||% "black", data$alpha %||% NA),
    lwd = (data$linewidth %||% 0.5) * ggplot2::.pt,
    lty = data$linetype %||% 1, lineend = "butt")
  grid::gTree(children = grid::gList(
    grid::segmentsGrob(0.15, 0.5, 0.85, 0.5, gp = gp),
    grid::segmentsGrob(c(0.15, 0.85), 0.3, c(0.15, 0.85), 0.7, gp = gp)))
}

#' @noRd
draw_key_cpt_region <- function(data, params, size) {
  grid::rectGrob(width = 0.6, height = 1, gp = grid::gpar(
    fill = ggplot2::alpha(data$fill %||% "steelblue", data$alpha %||% 0.2),
    col = if (is.null(data$colour) || is.na(data$colour)) NA else
      data$colour,
    lwd = (data$linewidth %||% 0.5) * ggplot2::.pt))
}

#' @noRd
draw_key_cpt_event <- function(data, params, size) {
  col <- ggplot2::alpha(data$colour %||% "grey30", data$alpha %||% NA)
  grid::gTree(children = grid::gList(
    grid::segmentsGrob(0.35, 0, 0.35, 1, gp = grid::gpar(
      col = col, lwd = (data$linewidth %||% 0.5) * ggplot2::.pt,
      lty = data$linetype %||% "dotted")),
    grid::polygonGrob(c(0.35, 0.8, 0.35), c(1, 0.85, 0.7),
                      gp = grid::gpar(fill = col, col = NA))))
}
