#' \code{ggchangepoint} package
#'
#' Unified tidy changepoint detection with \code{ggplot2} visualisation.
#'
#' \code{ggchangepoint} provides a consistent S3 result class (\code{ggcpt})
#' for changepoint detection results, \code{broom}-style methods
#' (\code{tidy()}, \code{glance()}, \code{augment()}), \code{ggplot2}
#' integration via \code{autoplot()} and composable geoms
#' (\code{geom_changepoint()}, \code{geom_cpt_segment()},
#' \code{geom_cpt_ci()}, \code{stat_changepoint()}), and a unified dispatcher
#' \code{cpt_detect()} that supports multiple engines.
#'
#' **Detection engines.** The package currently wraps 13 methods behind the
#' unified \code{cpt_detect()} front-end:
#' \itemize{
#'   \item \strong{changepoint:} PELT, BINSEG, SEGNEIGH, AMOC (mean, var, meanvar)
#'   \item \strong{changepoint.np:} NP (non-parametric, distribution-free)
#'   \item \strong{ecp:} E-Divisive, E-Agglo (multivariate, non-parametric)
#'   \item \strong{fpop:} FPOP (functional pruning optimal partitioning)
#'   \item \strong{wbs:} Wild Binary Segmentation
#'   \item \strong{breakfast:} WBS2, TGUH
#'   \item \strong{not:} Narrowest-Over-Threshold (mean, var, slope contrasts)
#'   \item \strong{mosum:} Moving Sum
#'   \item \strong{IDetect:} Isolate-Detect
#' }
#' Additional engines are planned (see \code{cpt_methods()} for the full table).
#'
#' **Key features.** Every detector returns a \code{ggcpt} object with a stable
#' \code{tibble(cp, cp_value)} contract. Visualise any result directly with
#' \code{autoplot()}. Compare methods with \code{ggcpt_compare()} and
#' \code{ggcpt_compare_table()}. Evaluate accuracy with \code{cpt_metrics()}
#' and \code{ggcpt_eval()}. Generate synthetic data with known ground truth
#' via \code{cpt_simulate()} and the built-in canonical test signals.
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
                               ...) {

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
    registerS3method("plot", "ggcpt", get("plot.ggcpt", envir = pkg_ns), envir = asNamespace("base"))
    registerS3method("summary", "ggcpt", get("summary.ggcpt", envir = pkg_ns), envir = asNamespace("base"))
    registerS3method("print", "summary.ggcpt", get("print.summary.ggcpt", envir = pkg_ns), envir = asNamespace("base"))
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
