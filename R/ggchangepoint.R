#' \code{ggchangepoint} package
#'
#' Unified tidy changepoint detection with \code{ggplot2} visualisation.
#'
#' \code{ggchangepoint} provides a consistent S3 result class (\code{ggcpt})
#' for changepoint detection results, \code{broom}-style methods
#' (\code{tidy()}, \code{glance()}, \code{augment()}), \code{ggplot2}
#' integration via \code{autoplot()} and composable geoms
#' (\code{geom_changepoint()}, \code{geom_cpt_segment()},
#' \code{geom_cpt_ci()}, \code{geom_cpt_region()}, \code{geom_cpt_label()},
#' \code{geom_cpt_event()}, \code{stat_changepoint()}), and a unified
#' dispatcher \code{cpt_detect()} that reaches fifty methods.
#'
#' **Detection engines.** \code{cpt_detect()} dispatches to the methods in
#' \code{cpt_methods()}, across nine families:
#' \itemize{
#'   \item \strong{Penalised/optimal:} PELT, BinSeg, SegNeigh, AMOC
#'     (\pkg{changepoint}); FPOP (\pkg{fpop}); fast binary segmentation
#'     (\pkg{binsegRcpp}); the CROPS penalty path (\code{cpt_crops()});
#'     fastcpd (\pkg{fastcpd}, incl. AR/ARMA/GARCH); change-in-slope via
#'     CPOP (\pkg{cpop}).
#'   \item \strong{Multiscale/search:} WBS (\pkg{wbs}), WBS2 and TGUH
#'     (\pkg{breakfast}), NOT (\pkg{not}), MOSUM incl. multiscale
#'     (\pkg{mosum}), Isolate-Detect (\pkg{IDetect}), SMUCE/HSMUCE with
#'     confidence intervals (\pkg{stepR}), WBS for nonstationary series
#'     (\pkg{wbsts}).
#'   \item \strong{Inference:} Narrowest Significance Pursuit (\pkg{nsp}),
#'     which returns intervals rather than points.
#'   \item \strong{Nonparametric/kernel:} NP (\pkg{changepoint.np}),
#'     E-Divisive/E-Agglo (\pkg{ecp}), kernel running statistics
#'     (\pkg{kcpRS}), NP-MOJO (\pkg{CptNonPar}), sequential CPM (\pkg{cpm}),
#'     self-normalisation (\pkg{SNSeg}), depth ranks
#'     (\pkg{KWCChangepoint}).
#'   \item \strong{Bayesian:} Barry-Hartigan posterior (\pkg{bcp}), online
#'     BOCPD (\pkg{ocp}), BEAST model averaging (\pkg{Rbeast}),
#'     formula-based regression with changepoints (\pkg{mcp}).
#'   \item \strong{High-dimensional:} sparse projection
#'     (\pkg{InspectChangepoint}), online ocd (\pkg{ocd}), geometric mapping
#'     (\pkg{changepoint.geo}), sparsity-adaptive ESAC and Pilliat
#'     (\pkg{HDCD}), and covariance, network, VAR and
#'     high-dimensional-regression changes (\pkg{changepoints}).
#'   \item \strong{Functional and network:} functional mean and covariance
#'     (\pkg{fChange}), NMF-based network structure (\pkg{fabisearch}).
#'   \item \strong{Regression, trend and season:} Bai-Perron breaks with CIs
#'     (\pkg{strucchange}), broken-line regression (\pkg{segmented}),
#'     changepoints-vs-autocorrelation model selection (\pkg{EnvCpt}),
#'     drift+AR robust detection (\pkg{DeCAFS}), BFAST season-and-trend
#'     breaks (\pkg{bfast}).
#'   \item \strong{Classical single-change tests:} Pettitt, Buishand and
#'     SNHT (\pkg{trend}), Taylor's analyzer (\pkg{ChangePointTaylor}).
#' }
#'
#' **What surrounds the detectors.** Every detector returns a \code{ggcpt}
#' object with a stable \code{tibble(cp, cp_value)} contract, optionally
#' carrying a time index, engine confidence intervals, a fitted signal,
#' significance regions and diagnostics. Around that:
#' \itemize{
#'   \item \strong{Inference:} \code{cpt_confint()} (four provenances, one
#'     contract), \code{cpt_test()}, \code{cpt_regions()}.
#'   \item \strong{Choosing K:} \code{cpt_select()} (BIC, Zhang-Siegmund
#'     mBIC, AIC, CROPS elbow, cross-validation, stability),
#'     \code{cpt_crops()}, \code{cpt_penalty()}.
#'   \item \strong{Diagnostics:} \code{cpt_influence()}, \code{cpt_leverage()},
#'     \code{cpt_sensitivity()}, \code{cpt_stability()},
#'     \code{cpt_statistic()}, \code{cpt_solution_path()},
#'     \code{cpt_scale_space()}.
#'   \item \strong{Supervised detection:} \code{cpt_labels()},
#'     \code{cpt_label_error()}, \code{cpt_label_error_curve()},
#'     \code{cpt_learn_penalty()}.
#'   \item \strong{Choosing a method:} \code{cpt_recommend()},
#'     \code{cpt_consensus()}, \code{ggcpt_compare()}.
#'   \item \strong{Evaluation:} \code{cpt_metrics()},
#'     \code{cpt_metrics_annotated()}, \code{cpt_benchmark()},
#'     \code{cpt_datasets()}, \code{cpt_load_tcpd()}.
#'   \item \strong{Streaming:} \code{cpt_monitor()}, \code{cpt_update()},
#'     \code{alarms()}, \code{cpt_replay()}, \code{cpt_delay()}.
#'   \item \strong{Study design:} \code{cpt_simulate()}, \code{cpt_power()},
#'     \code{cpt_min_detectable()}, \code{cpt_scenarios()}.
#'   \item \strong{Communication:} \code{cpt_annotate_events()},
#'     \code{cpt_report()}, \code{cpt_gt()}, \code{ggcpt_interactive()},
#'     \code{cpt_cite()}.
#'   \item \strong{Extension:} \code{as_ggcpt()} and
#'     \code{cpt_register_method()} bring detectors this package does not and
#'     cannot depend on into the same grammar.
#' }
#'
#' @seealso The entry points, by group:
#'   \itemize{
#'     \item \strong{Detect:} \code{\link{cpt_detect}()},
#'       \code{\link{cpt_methods}()}, \code{\link{cpt_register_method}()}.
#'     \item \strong{Visualise:} \code{\link{autoplot.ggcpt}()},
#'       \code{\link{ggcptplot}()}, \code{\link{ggcpt_compare}()}.
#'     \item \strong{Inference and selection:} \code{\link{cpt_confint}()},
#'       \code{\link{cpt_test}()}, \code{\link{cpt_select}()}.
#'     \item \strong{Choosing and combining methods:}
#'       \code{\link{cpt_consensus}()}, \code{\link{cpt_recommend}()},
#'       \code{\link{cpt_benchmark}()}.
#'     \item \strong{Evaluation:} \code{\link{cpt_metrics}()}.
#'     \item \strong{Streaming:} \code{\link{cpt_monitor}()}.
#'     \item \strong{Study design:} \code{\link{cpt_simulate}()},
#'       \code{\link{cpt_power}()}.
#'     \item \strong{Communication:} \code{\link{cpt_report}()}.
#'   }
#'
#'   Useful links:
#'   \itemize{
#'     \item \url{https://pursuitofdatascience.github.io/ggchangepoint/}
#'     \item Report bugs at
#'       \url{https://github.com/PursuitOfDataScience/ggchangepoint/issues}
#'   }
#'
#' @importFrom generics tidy glance augment
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  # Columns referenced inside aes() by name. R CMD check cannot see through
  # non-standard evaluation, so every such name has to be declared once here
  # or it is reported as an undefined global.
  utils::globalVariables(c(
    # 0.4.0
    ".", "cp", "cp_value", "end", "index", "param_estimate", "raw_value",
    "start", "type", "value", "x", "xend", "y", "xmin", "xmax", "n_cpts",
    "cost", "penalty", "freq", "run_length", "time", "prob", "coordinate",
    "yint", ".ymin", ".ymax", ".ymin_val", ".ymax_val",
    # 0.5.0: diagnostics, selection and influence
    "leverage", "perturbed", "perturbed_x", "cp_x", "param_shift", "shift",
    "position", "k", "chosen", "statistic", "threshold", "step", "contrast",
    "selected", "start_x", "end_x", "index_x", "bandwidth", "panel",
    ".setting", "n_cp",
    # 0.5.0: supervised detection, events and communication
    "change", "status", "label", "count", "kind", "errors",
    # 0.5.0: consensus, benchmarking, monitoring and power
    "method", "series", "dataset", "mean_rank", "within_cd", "delay",
    "detected", "jump", "power", "lower", "upper", "truth", "ci",
    "scale_space", "path", ".at"
  ))
}

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
  validate_flag(show_points, "show_points")
  validate_flag(show_line, "show_line")
  validate_index(index, length(data))

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

# No .onLoad() is needed. Every S3 method this package provides is declared
# in NAMESPACE -- including the ones on base generics, via
# `@exportS3Method base::plot` / `base::summary` -- and R registers those at
# load time on its own. An earlier version also called registerS3method()
# here for print/plot/summary, which wrote into base's methods table for no
# effect and wrapped the lot in suppressWarnings(), so a genuine registration
# failure would have gone unseen. Verified redundant: with the block removed,
# print/auto-print/summary/plot/format/autoplot/tidy/glance/augment/
# as_tibble/as.data.frame all still dispatch, with and without the package
# attached (regression test R41).

# Internal: check a scalar numeric argument against its documented range.
# The engines validate their own arguments (stepR rejects alpha outside
# (0, 1), SNSeg rejects an unlisted confidence), but this package's own
# arguments were taken on trust, and out-of-range values there produce
# answers rather than errors: `margin = -3` scored a perfect segmentation as
# precision 0, `B = 0` gave a stability profile of NaN, `n = -10` a covering
# metric of -1.
#' @noRd
validate_scalar <- function(value, name, min = -Inf, max = Inf,
                            min_open = FALSE, max_open = FALSE) {
  ok <- is.numeric(value) && length(value) == 1L && is.finite(value) &&
    (if (min_open) value > min else value >= min) &&
    (if (max_open) value < max else value <= max)
  if (!ok) {
    lo <- if (is.finite(min)) paste0(if (min_open) "greater than " else "at least ", min)
    hi <- if (is.finite(max)) paste0(if (max_open) "less than " else "at most ", max)
    rng <- paste(Filter(Negate(is.null), list(lo, hi)), collapse = " and ")
    stop("`", name, "` must be a single finite number",
         if (nzchar(rng)) paste0(", ", rng), " (got ",
         paste(format(value), collapse = ", "), ").", call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: check a switch documented as "Logical". `isTRUE()` treats every
# non-TRUE value as FALSE, so `show_segments = 1` or `show_fit = "TRUE"`
# silently drew nothing, and `show_line = 1` silently removed the line the
# user was asking to keep. Refuse instead of quietly doing the opposite.
#' @noRd
validate_flag <- function(value, name, allow_null = FALSE) {
  if (allow_null && is.null(value)) return(invisible(TRUE))
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE",
         if (allow_null) " (or NULL)", " (got ",
         paste(format(value), collapse = ", "), ").", call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: a user-supplied `index` labels the x axis, so it must line up
# with the series one-to-one. Without this check a wrong-length index
# surfaces as an opaque recycling error from dplyr ("`x` must be size n or
# 1") that never mentions the argument at fault.
#' @noRd
validate_index <- function(index, n) {
  if (is.null(index)) return(invisible(TRUE))
  if (length(index) != n) {
    stop("`index` must have one value per observation: the series has ", n,
         " observation(s) but `index` has ", length(index), ".",
         call. = FALSE)
  }
  invisible(TRUE)
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
  } else if (is.numeric(x) || is.logical(x)) {
    # A logical series is a legitimate 0/1 series, and cpt_detect() already
    # coerces one before it gets here; accepting it makes the tools that
    # validate first agree with the tools that coerce first.
    x <- as.numeric(x)
    if (anyNA(x) || any(!is.finite(x))) {
      stop("`x` must be finite (no NA/NaN/Inf).", call. = FALSE)
    }
    if (length(x) < 3) {
      stop("`x` must have at least 3 observations.", call. = FALSE)
    }
  } else {
    # Not a series at all. coerce_series_values() names the specific trap --
    # a factor's level codes, character input -- and rejects anything else
    # with the general message; the stop() below is only a backstop.
    coerce_series_values(x)
    stop("`x` must be a numeric vector, matrix, or data.frame.", call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: user-supplied changepoint LOCATIONS, wherever they arrive --
# `cp`, `pred`, `truth`, `annotations`, `changepoints`, a label's `start`
# and `end`. Every one of these used to be read through a bare
# `as.integer()`, and `as.integer()` on a factor returns LEVEL POSITIONS:
# `cpt_metrics(factor(c("100", "150")), c(100, 150), n = 200)` read the
# predictions as 1 and 2 and reported a recall of 0. That is worse than an
# error, because it is a plausible number. A logical vector is a mask over
# the series rather than a set of positions, and an NA the coercion invented
# (from text that is not a number) is a wrong-type input rather than the
# missing value the drop rules are about.
#' @noRd
as_cp_locations <- function(x, arg = "cp", sort = FALSE) {
  if (is.null(x)) return(integer(0))
  if (is_ggcpt(x)) {
    stop("`", arg, "` takes changepoint indices, not a `ggcpt` object. ",
         "Pass the locations instead, e.g. `fit$changepoints$cp` or ",
         "`tidy(fit)$cp`.", call. = FALSE)
  }
  if (is.data.frame(x)) {
    if ("cp" %in% names(x)) {
      stop("`", arg, "` takes changepoint indices, not a table. Pass the ",
           "column, e.g. `", arg, "$cp`.", call. = FALSE)
    }
    stop("`", arg, "` takes changepoint indices, not a table.", call. = FALSE)
  }
  if (is.factor(x)) {
    stop("`", arg, "` is a factor. Coercing a factor gives its level codes ",
         "(alphabetical positions), not the locations. Convert it first, ",
         "e.g. as.integer(as.character(", arg, ")).", call. = FALSE)
  }
  if (is.logical(x)) {
    stop("`", arg, "` is logical. Changepoint locations are positions, not ",
         "a mask over the series; pass which(", arg, ").", call. = FALSE)
  }
  was_na <- is.na(x)
  out <- suppressWarnings(as.integer(x))
  invented <- is.na(out) & !was_na
  if (any(invented)) {
    bad <- unique(as.character(x)[invented])
    stop("`", arg, "` must be changepoint locations; ", sum(invented),
         " value(s) are not numbers: ",
         paste0("\"", utils::head(bad, 5), "\"", collapse = ", "), ".",
         call. = FALSE)
  }
  if (isTRUE(sort)) sort(unique(out)) else out
}

# Internal: turn a non-matrix series into the numeric vector the engines
# take, refusing the coercions that answer a different question. as.numeric()
# on a factor returns the LEVEL CODES, so a factor series would be detected
# on an alphabetical ordering of its labels with nothing said about it; on
# character it returns NAs with base R's "NAs introduced by coercion", after
# which validate_data() blames non-finite data rather than the text.
#' @noRd
coerce_series_values <- function(x) {
  if (is.factor(x)) {
    stop("`x` is a factor. Detection needs numbers, and coercing a factor ",
         "gives its level codes -- an alphabetical ordering of the labels, ",
         "not the data. Convert it deliberately, e.g. ",
         "as.numeric(as.character(x)).", call. = FALSE)
  }
  if (is.character(x)) {
    stop("`x` is character. `x` must be a numeric vector, matrix, or ",
         "data.frame; convert it first, e.g. as.numeric(x).", call. = FALSE)
  }
  # Everything else keeps whatever as.numeric() already did for it -- a ts,
  # a zoo, a table, a difftime all convert cleanly -- and is refused only
  # when R itself flags the conversion, which is what a list or any other
  # unconvertible type does.
  clean <- TRUE
  num <- tryCatch(
    withCallingHandlers(as.numeric(x), warning = function(w) {
      clean <<- FALSE
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      clean <<- FALSE
      NULL
    }
  )
  if (!clean || is.null(num)) {
    stop("`x` must be a numeric vector, matrix, or data.frame, not ",
         class(x)[1], ".", call. = FALSE)
  }
  num
}
