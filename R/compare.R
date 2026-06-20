#' Compare multiple changepoint detection methods
#'
#' Runs several detectors on the same data and returns a faceted or overlaid
#' ggplot comparison. Respects \code{future::plan()} for parallel execution
#' if the \code{future.apply} package is available.
#'
#' @param x A numeric vector (the data series).
#' @param methods Character vector of method names (passed to \code{cpt_detect}).
#' @param layout Layout type. \code{"facet"} (default) shows one panel per method;
#'   \code{"overlay"} draws all changepoints in one panel, colour-coded.
#' @param change_in What to detect change in. Passed to each detector.
#' @param seed Optional seed for reproducible parallelism. Passed to
#'   \code{future.apply::future_lapply()} as \code{future.seed}.
#' @param ... Additional arguments passed to each detector.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' set.seed(2022)
#' x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
#' ggcpt_compare(x, methods = c("pelt", "binseg"))
ggcpt_compare <- function(x,
                          methods = c("pelt", "binseg", "amoc"),
                          layout = c("facet", "overlay"),
                          change_in = "mean",
                          seed = NULL,
                          ...) {

  layout <- match.arg(layout)
  data_vec <- as.numeric(x)

  # Attempt parallel execution if future is set up
  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  if (has_future) {
    results <- future.apply::future_lapply(methods, function(m) {
      cpt_detect(data_vec, method = m, change_in = change_in, ...)
    }, future.seed = seed)
  } else {
    results <- lapply(methods, function(m) {
      cpt_detect(data_vec, method = m, change_in = change_in, ...)
    })
  }

  names(results) <- methods

  if (layout == "facet") {
    ggcpt_compare_facet(data_vec, results, methods)
  } else {
    ggcpt_compare_overlay(data_vec, results, methods)
  }
}

ggcpt_compare_facet <- function(data_vec, results, methods) {
  plot_data <- tibble::tibble(
    index = seq_along(data_vec),
    value = data_vec
  )

  cp_data <- do.call(rbind, lapply(methods, function(m) {
    cp <- results[[m]]$changepoints
    if (nrow(cp) == 0) return(NULL)
    tibble::tibble(
      index = cp$cp,
      value = data_vec[cp$cp],
      method = m
    )
  }))

  if (is.null(cp_data)) {
    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(index, value)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~method)
    )
  }

  ymin <- min(data_vec) - 0.05 * diff(range(data_vec))
  ymax <- max(data_vec) + 0.05 * diff(range(data_vec))

  cp_data <- dplyr::mutate(cp_data, .ymin = ymin, .ymax = ymax)

  ggplot2::ggplot(plot_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey50") +
    ggplot2::geom_linerange(
      data = cp_data,
      ggplot2::aes(x = index, ymin = .ymin, ymax = .ymax),
      inherit.aes = FALSE, color = "blue", linewidth = 0.5
    ) +
    ggplot2::facet_wrap(~method, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "Changepoint Detection Comparison")
}

ggcpt_compare_overlay <- function(data_vec, results, methods) {
  plot_data <- tibble::tibble(
    index = seq_along(data_vec),
    value = data_vec
  )

  ymin <- min(data_vec) - 0.05 * diff(range(data_vec))
  ymax <- max(data_vec) + 0.05 * diff(range(data_vec))

  cp_data <- do.call(rbind, lapply(methods, function(m) {
    cp <- results[[m]]$changepoints
    if (nrow(cp) == 0) return(NULL)
    tibble::tibble(
      index = cp$cp,
      method = m
    )
  }))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "Changepoint Detection Comparison",
                  color = "Method")

  if (!is.null(cp_data)) {
    cp_data <- dplyr::mutate(cp_data, .ymin = ymin, .ymax = ymax)
    p <- p + ggplot2::geom_linerange(
      data = cp_data,
      ggplot2::aes(x = index, ymin = .ymin, ymax = .ymax, color = method),
      inherit.aes = FALSE, linewidth = 0.5, position = ggplot2::position_dodge(width = 1)
    )
  }

  p
}

#' Comparison table
#'
#' Returns a tidy tibble combining the results of multiple detectors on
#' the same series.
#'
#' @param x A numeric vector (the data series).
#' @param methods Character vector of method names.
#' @param change_in What to detect change in.
#' @param ... Additional arguments passed to each detector.
#'
#' @return A tibble with columns \code{method}, \code{cp}, \code{cp_value}.
#' @export
ggcpt_compare_table <- function(x,
                                methods = c("pelt", "binseg", "amoc"),
                                change_in = "mean",
                                ...) {
  data_vec <- as.numeric(x)

  do.call(rbind, lapply(methods, function(m) {
    res <- cpt_detect(data_vec, method = m, change_in = change_in, ...)
    tbl_cp <- res$changepoints
    if (nrow(tbl_cp) == 0) {
      return(tibble::tibble(method = m, cp = NA_integer_, cp_value = NA_real_))
    }
    tibble::tibble(method = m, cp = tbl_cp$cp, cp_value = tbl_cp$cp_value)
  }))
}
