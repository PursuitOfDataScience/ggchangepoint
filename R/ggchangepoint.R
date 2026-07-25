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
#' \code{cpt_detect()} that supports over thirty methods.
#'
#' **Detection engines.** \code{cpt_detect()} currently dispatches to 31
#' methods across six families (run \code{cpt_methods()} for the live
#' table with installation status):
#' \itemize{
#'   \item \strong{Penalised/optimal:} PELT, BinSeg, SegNeigh, AMOC
#'     (\pkg{changepoint}); FPOP (\pkg{fpop}); the CROPS penalty path
#'     (\code{cpt_crops()}); fastcpd (\pkg{fastcpd}, incl. AR/ARMA/GARCH);
#'     change-in-slope via CPOP (\pkg{cpop}).
#'   \item \strong{Multiscale/search:} WBS (\pkg{wbs}), WBS2 and TGUH
#'     (\pkg{breakfast}), NOT (\pkg{not}), MOSUM incl. multiscale
#'     (\pkg{mosum}), Isolate-Detect (\pkg{IDetect}), SMUCE/HSMUCE with
#'     confidence intervals (\pkg{stepR}).
#'   \item \strong{Nonparametric/kernel:} NP (\pkg{changepoint.np}),
#'     E-Divisive/E-Agglo (\pkg{ecp}), kernel running statistics
#'     (\pkg{kcpRS}), NP-MOJO (\pkg{CptNonPar}), sequential CPM (\pkg{cpm}),
#'     self-normalisation (\pkg{SNSeg}).
#'   \item \strong{Bayesian:} Barry-Hartigan posterior (\pkg{bcp}), online
#'     BOCPD (\pkg{ocp}), BEAST model averaging (\pkg{Rbeast}).
#'   \item \strong{Multivariate/high-dimensional:} sparse projection
#'     (\pkg{InspectChangepoint}), online ocd (\pkg{ocd}), geometric mapping
#'     (\pkg{changepoint.geo}).
#'   \item \strong{Regression breaks and robust detection:} Bai-Perron breaks
#'     with CIs (\pkg{strucchange}), broken-line regression
#'     (\pkg{segmented}), changepoints-vs-autocorrelation model selection
#'     (\pkg{EnvCpt}), drift+AR robust detection (\pkg{DeCAFS}).
#' }
#'
#' **Key features.** Every detector returns a \code{ggcpt} object with a stable
#' \code{tibble(cp, cp_value)} contract (plus engine extras such as
#' \code{ci_lower}/\code{ci_upper} and \code{posterior_prob}). Visualise any
#' result directly with \code{autoplot()} (confidence intervals, fitted
#' signals, multivariate facets), the Bayesian displays
#' (\code{ggcpt_posterior()}, \code{ggcpt_runlength()}), or interactively
#' via \code{ggcpt_interactive()}. Compare methods with
#' \code{ggcpt_compare()}; run panels of series with \code{cpt_batch()};
#' quantify uncertainty with \code{cpt_stability()}; sweep penalties with
#' \code{cpt_crops()}. Evaluate accuracy with \code{cpt_metrics()} and
#' \code{ggcpt_eval()}; simulate ground-truth data with
#' \code{cpt_simulate()} and the canonical test signals; and cite the
#' methodology behind any result with \code{cpt_cite()}.
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
                                                        "xend",
                                                        "y",
                                                        "xmin",
                                                        "xmax",
                                                        "n_cpts",
                                                        "cost",
                                                        "penalty",
                                                        "freq",
                                                        "run_length",
                                                        "time",
                                                        "prob",
                                                        "variable",
                                                        "yint",
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

  extra <- list(...)
  if (length(extra) > 0) {
    warning("Ignoring unknown argument(s): ",
            paste(names(extra), collapse = ", "), call. = FALSE)
  }

  if (length(data) == 0) {
    stop("Cannot plot an empty series.", call. = FALSE)
  }

  plot_data <- tibble::tibble(raw_value = as.numeric(data))
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
