#' \code{ggchangepoint} package
#'
#' Combines Changepoint Analysis with 'ggplot2'.
#'
#' ggchangepoint tries to offer several changepoint R packages in a tidy
#' format and output the ggplot2 plots so that the tidyverse users can gain some
#' familiarity to work with the changepoint analysis. For the moment, I only
#' include three changepoint packages ('changepoint', 'changepoint.np' and 'ecp'
#' ). More changepoint packages will be included as time progresses.
#'
#' @importFrom generics tidy glance augment
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "cp",
                                                        "cp_value",
                                                        "end",
                                                        "index",
                                                        "param_estimate",
                                                        "raw_value",
                                                        "start",
                                                        "type",
                                                        "value",
                                                        "x",
                                                        ".ymin",
                                                        ".ymax",
                                                        ".ymin_val",
                                                        ".ymax_val"))

# Shared internal helper for rendering changepoint plots
# Used by ggcptplot(), ggecpplot(), and autoplot.ggcpt()
ggcptplot_internal <- function(data, result,
                               cptline_alpha = 1,
                               cptline_color = "blue",
                               cptline_type = "solid",
                               cptline_linewidth = 0.5,
                               index = NULL,
                               show_points = TRUE,
                               show_line = TRUE,
                               show_segments = FALSE) {

  plot_data <- tibble::tibble(raw_value = data)
  if (is.null(index)) {
    plot_data <- dplyr::mutate(plot_data, x = dplyr::row_number())
  } else {
    plot_data <- dplyr::mutate(plot_data, x = index)
  }

  yrange <- diff(range(data, na.rm = TRUE))
  if (yrange == 0) yrange <- 1
  ymin_val <- min(data, na.rm = TRUE) - 0.05 * yrange
  ymax_val <- max(data, na.rm = TRUE) + 0.05 * yrange

  p <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = x, y = raw_value))

  if (isTRUE(show_line)) {
    p <- p + ggplot2::geom_line()
  }
  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point()
  }

  if (nrow(result) > 0) {
    cp_data <- result
    if (!is.null(index)) {
      cp_data <- dplyr::mutate(cp_data, x = index[cp])
    } else {
      cp_data <- dplyr::mutate(cp_data, x = cp)
    }
    cp_data <- dplyr::mutate(cp_data,
      .ymin_val = ymin_val,
      .ymax_val = ymax_val
    )
    p <- p + ggplot2::geom_linerange(
      data = cp_data,
      ggplot2::aes(x = x, ymin = .ymin_val, ymax = .ymax_val),
      inherit.aes = FALSE,
      alpha = cptline_alpha,
      color = cptline_color,
      linetype = cptline_type,
      linewidth = cptline_linewidth
    )
  }

  p
}

.onLoad <- function(libname, pkgname) {
  pkg_ns <- getNamespace(pkgname)
  suppressWarnings({
    registerS3method("tidy", "ggcpt", get("tidy.ggcpt", envir = pkg_ns), envir = asNamespace("generics"))
    registerS3method("glance", "ggcpt", get("glance.ggcpt", envir = pkg_ns), envir = asNamespace("generics"))
    registerS3method("augment", "ggcpt", get("augment.ggcpt", envir = pkg_ns), envir = asNamespace("generics"))
    registerS3method("autoplot", "ggcpt", get("autoplot.ggcpt", envir = pkg_ns), envir = asNamespace("ggplot2"))
    registerS3method("print", "ggcpt", get("print.ggcpt", envir = pkg_ns), envir = asNamespace("base"))
  })
}

# Validate input data
validate_data <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    x_num <- as.matrix(x)
    if (!is.numeric(x_num)) {
      stop("`x` must be numeric.", call. = FALSE)
    }
    if (anyNA(x_num) || any(!is.finite(x_num))) {
      stop("`x` must be finite (no NA/NaN/Inf).", call. = FALSE)
    }
    if (nrow(x_num) < 3) {
      stop("`x` must have at least 3 observations.", call. = FALSE)
    }
  } else if (is.numeric(x)) {
    x <- as.numeric(x)
    if (anyNA(x) || any(!is.finite(x))) {
      stop("`x` must be finite (no NA/NaN/Inf).", call. = FALSE)
    }
    if (length(x) < 3) {
      stop("`x` must have at least 3 observations.", call. = FALSE)
    }
  } else {
    stop("`x` must be a numeric vector, matrix, or data.frame.", call. = FALSE)
  }
  invisible(TRUE)
}
