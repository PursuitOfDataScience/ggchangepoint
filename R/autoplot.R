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
#' @param show_regions Logical. Whether to shade the significance regions an
#'   interval-valued method returns (the \code{regions} slot — currently
#'   \code{\link{nsp_wrapper}()}). Each band is an interval that contains at
#'   least one changepoint at the stated global level; it is not a confidence
#'   interval around a point estimate. Defaults to \code{TRUE} when the
#'   result carries regions, and is ignored otherwise.
#' @param cptline_alpha Alpha for changepoint lines. Defaults to \code{1}.
#' @param cptline_color Color for changepoint lines. Defaults to \code{"blue"}.
#' @param cptline_type Linetype for changepoint lines. Defaults to \code{"solid"}.
#' @param cptline_linewidth Linewidth for changepoint lines. Defaults to \code{0.5}.
#' @param show_points Logical. Whether to draw data points. Auto-off above 500 obs.
#' @param show_line Logical. Whether to draw the line. Defaults to \code{TRUE}.
#' @param index Optional vector of x-axis values (e.g. dates) of the same
#'   length as the series. Defaults to the time index carried by the result
#'   (see the \code{index} argument of \code{\link{cpt_detect}()}), and to
#'   the observation position when there is none.
#' @param labels Optional \code{\link{cpt_labels}()} tibble. When supplied,
#'   the labelled regions are shaded behind the series and coloured by the
#'   outcome \code{\link{cpt_label_error}()} gives them — correct, false
#'   positive, false negative — so scoring a segmentation against expert
#'   labels becomes a picture rather than a table.
#' @param type Which view to draw. \code{"series"} (default) is the series
#'   with its changepoints; \code{"statistic"}, \code{"path"} and
#'   \code{"scale_space"} delegate to \code{\link{ggcpt_statistic}()},
#'   \code{\link{ggcpt_solution_path}()} and
#'   \code{\link{ggcpt_scale_space}()}, which error with the list of
#'   supporting engines when this one does not expose the internals.
#' @param ... Unknown arguments are ignored with a warning, except when
#'   \code{type} is not \code{"series"}, in which case they are passed to
#'   the delegate.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.ggcpt <- function(object,
                           show_segments = FALSE,
                           show_ci = FALSE,
                           show_fit = FALSE,
                           show_regions = NULL,
                           cptline_alpha = 1,
                           cptline_color = "blue",
                           cptline_type = "solid",
                           cptline_linewidth = 0.5,
                           show_points = NULL,
                           show_line = TRUE,
                           index = NULL,
                           labels = NULL,
                           type = c("series", "statistic", "path",
                                    "scale_space"),
                           ...) {

  type <- match.arg(type)
  if (type != "series") {
    return(switch(type,
      statistic = ggcpt_statistic(object, ...),
      path = ggcpt_solution_path(object, ...),
      scale_space = ggcpt_scale_space(object, ...)
    ))
  }

  data_vec <- object$data$value
  if (length(data_vec) == 0) {
    stop("Cannot autoplot an empty ggcpt object (no data).", call. = FALSE)
  }
  validate_flag(show_segments, "show_segments")
  validate_flag(show_ci, "show_ci")
  validate_flag(show_fit, "show_fit")
  validate_flag(show_line, "show_line")
  validate_flag(show_regions, "show_regions", allow_null = TRUE)
  validate_flag(show_points, "show_points", allow_null = TRUE)
  if (is.null(show_points)) {
    show_points <- length(data_vec) <= 500
  }
  # Regions are the inferential object for the methods that produce them, so
  # a result that carries them draws them unless told not to.
  if (is.null(show_regions)) {
    show_regions <- !is.null(object$regions) && nrow(object$regions) > 0
  }

  # Multivariate results render as faceted small-multiples. Count the
  # coordinate columns rather than the frame's width: `data_wide` also
  # carries `index`, and (when the result has a time index) `index_value`.
  if (n_coordinates(object) > 1) {
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

  # x-axis values: an explicit argument, then the index the result carries,
  # then the observation position.
  idx_vals <- plot_index(object, index)
  x_lab <- plot_index_label(object, index)

  p <- ggcptplot_internal(
    data = data_vec,
    result = object$changepoints,
    cptline_alpha = cptline_alpha,
    cptline_color = cptline_color,
    cptline_type = cptline_type,
    cptline_linewidth = cptline_linewidth,
    index = idx_vals,
    show_points = show_points,
    show_line = show_line,
    ...
  )

  if (!is.null(labels)) {
    err <- cpt_label_error(object, labels)
    if (nrow(err) > 0) {
      lab_df <- tibble::tibble(
        xmin = idx_vals[pmax(1L, pmin(err$start, length(idx_vals)))],
        xmax = idx_vals[pmax(1L, pmin(err$end, length(idx_vals)))],
        status = err$status
      )
      # Drawn beneath everything else, like the regions below.
      p$layers <- c(
        list(geom_cpt_label(ggplot2::aes(xmin = xmin, xmax = xmax,
                                         fill = status),
                            data = lab_df)),
        p$layers
      )
      p <- p + scale_fill_cpt_label(name = "Label")
    }
  }

  if (isTRUE(show_regions)) {
    if (is.null(object$regions) || nrow(object$regions) == 0) {
      warning("`show_regions = TRUE` but this result carries no significance ",
              "regions; nsp is the method that produces them.",
              call. = FALSE)
    } else {
      reg_df <- tibble::tibble(
        xmin = idx_vals[object$regions$start],
        xmax = idx_vals[object$regions$end]
      )
      # Drawn first so the series and the rules stay on top of the shading.
      p$layers <- c(
        list(ggplot2::geom_rect(
          data = reg_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE, fill = cptline_color, alpha = 0.18
        )),
        p$layers
      )
    }
  }

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

  with_alt(
    p + ggplot2::labs(
      title = paste0("Changepoint Detection (", object$method, ")"),
      x = x_lab,
      y = "Value"
    ),
    cpt_alt_text(object)
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
  vars <- setdiff(names(wide), c("index", "index_value"))
  validate_index(index, nrow(wide))
  # Honour a custom index (e.g. dates) for the x-axis when supplied; fall back
  # to the index the result carries, then to the observation position.
  x_vals <- plot_index(object, index)
  x_lab <- plot_index_label(object, index)
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
      x = x_lab, y = "Value",
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
  with_alt(p, paste0(cpt_alt_text(object), " Drawn as ", length(vars),
                     " stacked panels, one per coordinate, sharing the ",
                     "changepoint rules."))
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
      strip.text = ggplot2::element_text(face = "bold"),
      # Legends read better beside the panel than under it when the labels
      # are method names, and a slightly larger legend text is the cheapest
      # accessibility win available in a theme.
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.95)),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = "grey30")
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

# Internal: how many coordinates a result has. 1 for a univariate result
# (including a one-column matrix, which `data_wide` still records), more for
# a genuinely multivariate one.
#' @noRd
n_coordinates <- function(object) {
  if (is.null(object$data_wide)) return(1L)
  length(setdiff(names(object$data_wide), c("index", "index_value")))
}
