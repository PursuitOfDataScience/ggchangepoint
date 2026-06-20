#' Autoplot a ggcpt object
#'
#' Renders a changepoint detection result as a ggplot. The raw series is drawn
#' as a line (with optional points), changepoints are shown as vertical lines,
#' and (optionally) fitted segment levels are overlaid.
#'
#' @param object A \code{ggcpt} object.
#' @param show_segments Logical. Whether to draw the fitted segment means.
#'   Defaults to \code{FALSE}.
#' @param show_ci Logical. Whether to draw confidence intervals for changepoint
#'   locations (if available). Defaults to \code{FALSE}.
#' @param cptline_alpha Alpha for changepoint lines. Defaults to \code{1}.
#' @param cptline_color Color for changepoint lines. Defaults to \code{"blue"}.
#' @param cptline_type Linetype for changepoint lines. Defaults to \code{"solid"}.
#' @param cptline_linewidth Linewidth for changepoint lines. Defaults to \code{0.5}.
#' @param show_points Logical. Whether to draw data points. Auto-off above 500 obs.
#' @param show_line Logical. Whether to draw the line. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{ggcptplot_internal}.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.ggcpt <- function(object,
                           show_segments = FALSE,
                           show_ci = FALSE,
                           cptline_alpha = 1,
                           cptline_color = "blue",
                           cptline_type = "solid",
                           cptline_linewidth = 0.5,
                           show_points = NULL,
                           show_line = TRUE,
                           ...) {

  data_vec <- object$data$value
  if (is.null(show_points)) {
    show_points <- length(data_vec) <= 500
  }

  p <- ggcptplot_internal(
    data = data_vec,
    result = object$changepoints,
    cptline_alpha = cptline_alpha,
    cptline_color = cptline_color,
    cptline_type = cptline_type,
    cptline_linewidth = cptline_linewidth,
    index = object$data$index,
    show_points = show_points,
    show_line = show_line,
    show_segments = show_segments
  )

  if (isTRUE(show_segments) && nrow(object$segments) > 0) {
    seg_data <- object$segments
    p <- p + ggplot2::geom_segment(
      data = seg_data,
      ggplot2::aes(x = start, xend = end, y = param_estimate, yend = param_estimate),
      inherit.aes = FALSE,
      color = "darkred", linewidth = 1, na.rm = TRUE
    )
  }

  p + ggplot2::labs(
    title = paste0("Changepoint Detection (", object$method, ")"),
    x = "Index",
    y = "Value"
  )
}

#' ggchangepoint theme
#'
#' A minimal, publication-ready ggplot2 theme for changepoint plots.
#'
#' @param base_size Base font size. Defaults to 11.
#' @param base_family Base font family. Defaults to "".
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_ggcpt()
theme_ggcpt <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Annotate segments with alternating shading
#'
#' Adds alternating shaded rectangles to highlight segments between
#' changepoints.
#'
#' @param cp Changepoint indices (including 0 and n).
#' @param n Length of the series.
#' @param fill Colors for alternating segments. Defaults to c("grey90", "white").
#' @param alpha Alpha for fill. Defaults to 0.5.
#' @param ... Additional arguments passed to \code{annotate}.
#'
#' @return A list of ggplot annotations.
#' @export
annotate_segments <- function(cp, n, fill = c("grey90", "white"),
                               alpha = 0.5, ...) {
  breaks <- sort(unique(c(0, as.integer(cp), n)))
  annotations <- list()
  for (i in seq_len(length(breaks) - 1)) {
    annotations[[i]] <- ggplot2::annotate(
      "rect",
      xmin = breaks[i], xmax = breaks[i + 1],
      ymin = -Inf, ymax = Inf,
      fill = fill[((i - 1) %% length(fill)) + 1],
      alpha = alpha,
      ...
    )
  }
  annotations
}
