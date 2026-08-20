#' Autoplot a ggcpt object
#'
#' Renders a changepoint detection result as a ggplot. The raw series is drawn
#' as a line (with optional points), changepoints are shown as vertical lines,
#' and (optionally) fitted segment levels, the engine's fitted signal, and
#' changepoint-location confidence intervals are overlaid. Multivariate
#' results (from \code{ecp}, \code{inspect}, \code{geomcp}, ...) are drawn as
#' faceted small-multiples with shared changepoint rules.
#'
#' @param object A \code{ggcpt} object.
#' @param show_segments Logical. Whether to draw the fitted segment means.
#'   Defaults to \code{FALSE}.
#' @param show_ci Logical. Whether to draw confidence intervals for
#'   changepoint locations, when the engine provides them (columns
#'   \code{ci_lower}/\code{ci_upper} on the changepoints tibble — SMUCE,
#'   strucchange, segmented). Drawn as horizontal whiskers near the bottom
#'   of the panel. Defaults to \code{FALSE}.
#' @param show_fit Logical. Whether to draw the engine's fitted signal (the
#'   \code{fitted} column of \code{$data}, provided by SMUCE, DeCAFS, cpop,
#'   segmented, bcp, beast). Defaults to \code{FALSE}.
#' @param cptline_alpha Alpha for changepoint lines. Defaults to \code{1}.
#' @param cptline_color Color for changepoint lines. Defaults to \code{"blue"}.
#' @param cptline_type Linetype for changepoint lines. Defaults to \code{"solid"}.
#' @param cptline_linewidth Linewidth for changepoint lines. Defaults to \code{0.5}.
#' @param show_points Logical. Whether to draw data points. Auto-off above 500 obs.
#' @param show_line Logical. Whether to draw the line. Defaults to \code{TRUE}.
#' @param index Optional vector of x-axis values (e.g. dates) of the same
#'   length as the series; defaults to the observation index.
#' @param ... Unknown arguments are ignored with a warning.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.ggcpt <- function(object,
                           show_segments = FALSE,
                           show_ci = FALSE,
                           show_fit = FALSE,
                           cptline_alpha = 1,
                           cptline_color = "blue",
                           cptline_type = "solid",
                           cptline_linewidth = 0.5,
                           show_points = NULL,
                           show_line = TRUE,
                           index = NULL,
                           ...) {

  data_vec <- object$data$value
  if (length(data_vec) == 0) {
    stop("Cannot autoplot an empty ggcpt object (no data).", call. = FALSE)
  }
  validate_flag(show_segments, "show_segments")
  validate_flag(show_ci, "show_ci")
  validate_flag(show_fit, "show_fit")
  validate_flag(show_line, "show_line")
  validate_flag(show_points, "show_points", allow_null = TRUE)
  if (is.null(show_points)) {
    show_points <- length(data_vec) <= 500
  }

  # Multivariate results render as faceted small-multiples.
  if (!is.null(object$data_wide) && ncol(object$data_wide) > 2) {
    unsupported <- c(show_segments = isTRUE(show_segments),
                     show_ci = isTRUE(show_ci),
                     show_fit = isTRUE(show_fit))
    if (any(unsupported)) {
      warning("Ignoring ", paste(names(unsupported)[unsupported],
                                 collapse = ", "),
              " for multivariate results.", call. = FALSE)
    }
    return(autoplot_ggcpt_mv(object,
                             cptline_alpha = cptline_alpha,
                             cptline_color = cptline_color,
                             cptline_type = cptline_type,
                             cptline_linewidth = cptline_linewidth,
                             index = index))
  }

  p <- ggcptplot_internal(
    data = data_vec,
    result = object$changepoints,
    cptline_alpha = cptline_alpha,
    cptline_color = cptline_color,
    cptline_type = cptline_type,
    cptline_linewidth = cptline_linewidth,
    index = index %||% object$data$index,
    show_points = show_points,
    show_line = show_line,
    ...
  )

  # x-axis values for overlays: honour a custom index when supplied
  idx_vals <- index %||% object$data$index

  if (isTRUE(show_segments) && nrow(object$segments) > 0) {
    seg_data <- object$segments
    seg_df <- tibble::tibble(
      x = idx_vals[seg_data$start],
      xend = idx_vals[seg_data$end],
      y = seg_data$param_estimate
    )
    p <- p + ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = x, xend = xend, y = y, yend = y),
      inherit.aes = FALSE,
      color = "darkred", linewidth = 1, na.rm = TRUE
    )
  }

  if (isTRUE(show_fit)) {
    if (!"fitted" %in% names(object$data)) {
      warning("`show_fit = TRUE` but this result carries no fitted signal; ",
              "engines providing one include smuce, decafs, cpop, ",
              "segmented, bcp, and beast.", call. = FALSE)
    } else {
      fit_df <- tibble::tibble(x = idx_vals,
                               y = object$data$fitted)
      p <- p + ggplot2::geom_line(
        data = fit_df, ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE, color = "darkred", linewidth = 0.9
      )
    }
  }

  if (isTRUE(show_ci)) {
    cp_tbl <- object$changepoints
    if (!all(c("ci_lower", "ci_upper") %in% names(cp_tbl))) {
      warning("`show_ci = TRUE` but this result carries no ",
              "ci_lower/ci_upper columns; engines providing them include ",
              "smuce, hsmuce, strucchange, and segmented.", call. = FALSE)
    } else if (nrow(cp_tbl) > 0) {
      rng <- range(data_vec, na.rm = TRUE)
      y_ci <- rng[1] - 0.08 * max(diff(rng), 1)
      ci_df <- tibble::tibble(x = idx_vals[cp_tbl$cp],
                              xmin = idx_vals[cp_tbl$ci_lower],
                              xmax = idx_vals[cp_tbl$ci_upper],
                              y = y_ci)
      p <- p + ggplot2::geom_errorbar(
        data = ci_df,
        ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
        inherit.aes = FALSE, orientation = "y",
        width = 0.03 * max(diff(rng), 1),
        color = cptline_color, linewidth = 0.6
      ) + ggplot2::geom_point(
        data = ci_df, ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE, color = cptline_color, size = 1.2
      )
    }
  }

  p + ggplot2::labs(
    title = paste0("Changepoint Detection (", object$method, ")"),
    x = "Index",
    y = "Value"
  )
}

# Internal: faceted small-multiple rendering for multivariate results.
#' @noRd
autoplot_ggcpt_mv <- function(object, cptline_alpha = 1,
                              cptline_color = "blue",
                              cptline_type = "solid",
                              cptline_linewidth = 0.5,
                              index = NULL) {
  wide <- object$data_wide
  vars <- setdiff(names(wide), "index")
  validate_index(index, nrow(wide))
  # Honour a custom index (e.g. dates) for the x-axis when supplied; default
  # to the observation index otherwise.
  x_vals <- index %||% wide$index
  # The facet column must not be called `variable`: plotly::ggplotly() melts
  # the built plot into a frame that already has a column of that name, so a
  # faceted plot using it fails with "Names must be unique" -- which would
  # make ggcpt_interactive() unusable for every multivariate result.
  long <- do.call(rbind, lapply(vars, function(v) {
    tibble::tibble(index = x_vals, value = as.numeric(wide[[v]]),
                   coordinate = v)
  }))
  long$coordinate <- factor(long$coordinate, levels = vars)

  p <- ggplot2::ggplot(long, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::facet_wrap(~coordinate, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      x = "Index", y = "Value",
      title = paste0("Changepoint Detection (", object$method,
                     ", ", length(vars), " series)")
    )

  if (nrow(object$changepoints) > 0) {
    p <- p + ggplot2::geom_vline(
      xintercept = x_vals[object$changepoints$cp],
      alpha = cptline_alpha, color = cptline_color,
      linetype = cptline_type, linewidth = cptline_linewidth
    )
  }
  p
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
