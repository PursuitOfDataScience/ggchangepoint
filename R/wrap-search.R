#' WBS wrapper — Wild Binary Segmentation
#'
#' Wraps the \code{wbs} package for randomised changepoint detection via
#' Wild Binary Segmentation.
#'
#' @param x A numeric vector.
#' @param n_intervals Number of random intervals. Defaults to \code{5000}.
#' @param threshold Manual threshold for detection. If \code{NULL}, model
#'   selection uses the strengthened Schwarz Information Criterion (sSIC).
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{wbs::wbs()}.
#' @return A \code{ggcpt} object.
#' @export
wbs_wrapper <- function(x, n_intervals = 5000, threshold = NULL, seed = NULL, ...) {

  need_pkg("wbs")
  validate_scalar(n_intervals, "n_intervals", min = 1)
  validate_data(x)
  data_vec <- as_uni_vector(x, "wbs")

  if (!is.null(seed)) set.seed(seed)

  # The engine errors on constant input; a constant series simply has no
  # changepoints, so normalise to the empty-ggcpt contract.
  fit <- tryCatch(
    wbs::wbs(data_vec, M = n_intervals, ...),
    error = function(e) {
      if (grepl("constant", conditionMessage(e), fixed = TRUE)) NULL else stop(e)
    }
  )
  if (is.null(fit)) {
    return(ggcpt_build(data_vec, integer(0), method = "wbs",
                       change_in = "mean",
                       penalty = list(type = "sSIC", value = NA_real_),
                       call = match.call()))
  }
  if (!is.null(threshold)) {
    penalty <- list(type = "threshold", value = as.numeric(threshold))
    # A manual threshold that finds nothing errors ("no change-poinst found,
    # choose larger Kmax"); a series with no detected changepoints is a valid
    # result, so normalise that to the empty-ggcpt contract.
    cp <- tryCatch(
      wbs::changepoints(fit, th = threshold),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("no change-poinst found", msg, fixed = TRUE) ||
            grepl("choose larger Kmax", msg, fixed = TRUE)) {
          NULL
        } else {
          stop(e)
        }
      }
    )
    cp_indices <- if (is.null(cp)) integer(0) else as.integer(cp$cpt.th[[1]])
  } else {
    cp <- wbs::changepoints(fit, penalty = "ssic.penalty")
    # The sSIC model selection lives in cpt.ic; cpt.th holds the
    # (different) default-threshold selection.
    cp_indices <- as.integer(cp$cpt.ic$ssic.penalty)
    penalty <- list(type = "sSIC", value = NA_real_)
  }
  cp_indices <- cp_indices[!is.na(cp_indices)]

  ggcpt_build(
    data_vec, cp_indices,
    method = "wbs",
    change_in = "mean",
    penalty = penalty,
    fit = fit,
    call = match.call()
  )
}

#' WBS2 wrapper — Wild Binary Segmentation 2
#'
#' Wraps the \code{breakfast} package's WBS2 solution path with
#' steepest-drop-to-low-levels (SDLL) model selection.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{breakfast::breakfast()}.
#' @return A \code{ggcpt} object.
#' @export
wbs2_wrapper <- function(x, ...) {

  need_pkg("breakfast")
  validate_data(x)
  data_vec <- as_uni_vector(x, "wbs2")

  fit <- breakfast::breakfast(data_vec, solution.path = "wbs2",
                              model.selection = "sdll", ...)
  cp_indices <- breakfast_cpts(fit)

  ggcpt_build(
    data_vec, cp_indices,
    method = "wbs2",
    change_in = "mean",
    penalty = list(type = "SDLL", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

# Internal: extract changepoints from a breakfast fit, handling the empty
# cptmodel.list of very short inputs and the scalar-0 "no changepoints"
# sentinel some selectors use.
#' @noRd
breakfast_cpts <- function(fit) {
  if (length(fit$cptmodel.list) == 0) return(integer(0))
  cpts <- as.integer(fit$cptmodel.list[[1]]$cpts)
  cpts[!is.na(cpts) & cpts > 0]
}

#' NOT wrapper — Narrowest-Over-Threshold
#'
#' Wraps the \code{not} package for changepoint detection via the
#' Narrowest-Over-Threshold method. The contrast determines what change is
#' detected: piecewise-constant mean (default), mean and variance, or
#' (continuous or discontinuous) piecewise-linear trend.
#'
#' @param x A numeric vector.
#' @param contrast Contrast type. One of \code{"pcwsConstMean"},
#'   \code{"pcwsLinContMean"}, \code{"pcwsLinMean"},
#'   \code{"pcwsConstMeanVar"}. Defaults to \code{"pcwsConstMean"}.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{not::not()}.
#' @return A \code{ggcpt} object whose \code{change_in} reflects the
#'   contrast: \code{"mean"}, \code{"meanvar"}, or \code{"slope"}.
#' @export
not_wrapper <- function(x, contrast = "pcwsConstMean", seed = NULL, ...) {

  need_pkg("not")

  contrast <- match.arg(contrast, c("pcwsConstMean", "pcwsLinContMean",
                                     "pcwsLinMean", "pcwsConstMeanVar"))

  validate_data(x)
  data_vec <- as_uni_vector(x, "not")

  if (!is.null(seed)) set.seed(seed)

  change_in <- switch(contrast,
    pcwsConstMean = "mean",
    pcwsConstMeanVar = "meanvar",
    pcwsLinContMean = "slope",
    pcwsLinMean = "slope"
  )

  # The engine errors on (essentially) constant input; a constant series
  # simply has no changepoints.
  fit <- tryCatch(
    not::not(data_vec, contrast = contrast, ...),
    error = function(e) {
      if (grepl("constant", conditionMessage(e), fixed = TRUE)) NULL else stop(e)
    }
  )
  if (is.null(fit)) {
    return(ggcpt_build(data_vec, integer(0), method = "not",
                       change_in = change_in,
                       penalty = list(type = "sSIC", value = NA_real_),
                       call = match.call()))
  }
  feat <- not::features(fit)
  cp_indices <- as.integer(feat$cpt)
  cp_indices <- cp_indices[!is.na(cp_indices)]

  ggcpt_build(
    data_vec, cp_indices,
    method = "not",
    change_in = change_in,
    penalty = list(type = "sSIC", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' MOSUM wrapper — Moving Sum
#'
#' Wraps the \code{mosum} package for moving-sum-based changepoint
#' detection, either at a single bandwidth or (with
#' \code{multiscale = TRUE}) across a bandwidth grid with localised pruning.
#'
#' @param x A numeric vector.
#' @param G Bandwidth. If \code{NULL}, automatically selected
#'   (\code{min(n/10, 100)}, but never below 2, for the single-bandwidth
#'   procedure; the engine's default bandwidth grid for the multiscale
#'   procedure). A bandwidth of 1 leaves the engine's local variance
#'   estimate undefined, so the automatic choice is floored at 2.
#' @param multiscale Logical. Use the multiscale MOSUM procedure
#'   (\code{mosum::multiscale.localPrune()}) instead of a single bandwidth?
#'   Defaults to \code{FALSE}.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{mosum::mosum()} or
#'   \code{mosum::multiscale.localPrune()}.
#' @return A \code{ggcpt} object.
#' @export
mosum_wrapper <- function(x, G = NULL, multiscale = FALSE, seed = NULL, ...) {

  need_pkg("mosum")
  validate_flag(multiscale, "multiscale")
  validate_data(x)
  data_vec <- as_uni_vector(x, "mosum")

  if (!is.null(seed)) set.seed(seed)

  if (isTRUE(multiscale)) {
    fit <- if (is.null(G)) {
      mosum::multiscale.localPrune(data_vec, ...)
    } else {
      mosum::multiscale.localPrune(data_vec, G = G, ...)
    }
    cp_indices <- as.integer(fit$cpts)
    thresh_val <- NA_real_
  } else {
    if (is.null(G)) {
      # A single-observation window has no within-window variability, so the
      # engine's studentised statistic divides by zero and it returns a
      # "NaNs produced" warning with garbage changepoints rather than
      # failing. n / 10 falls to 1 for every n < 20, so floor the automatic
      # bandwidth at 2.
      G <- max(2L, as.integer(ceiling(min(length(data_vec) / 10, 100))))
      if (length(data_vec) <= 2L * G) {
        stop("`mosum` needs a moving-sum window on each side of a candidate ",
             "changepoint (bandwidth ", G, " requires more than ", 2L * G,
             " observations), but `x` has ", length(data_vec),
             ". Use a longer series, or see cpt_methods() for a method that ",
             "suits short series.", call. = FALSE)
      }
    }
    fit <- mosum::mosum(data_vec, G = G, ...)
    cp_indices <- as.integer(fit$cpts)
    # $threshold holds the threshold *type* string; the numeric value used
    # for detection is $threshold.value.
    thresh_val <- as.numeric(fit$threshold.value %||% NA_real_)
  }

  ggcpt_build(
    data_vec, cp_indices,
    method = "mosum",
    change_in = "mean",
    penalty = list(type = "threshold", value = thresh_val),
    fit = fit,
    call = match.call()
  )
}

#' Isolate-Detect wrapper
#'
#' Wraps the \code{IDetect} package. Requires the \code{IDetect} package.
#'
#' @param x A numeric vector.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{IDetect::ID()}.
#' @return A \code{ggcpt} object. When the engine finds no changepoints
#'   (including when it signals "No change-points found"), an empty result
#'   is returned rather than an error. A constant series likewise returns the
#'   empty result; see the note below.
#'
#' @section Constant input:
#' \code{IDetect::ID()} does not treat a flat series consistently — its
#' statistics become \eqn{0/0}, and what comes back depends on the value and
#' the length. \code{rep(3, 200)} yields \emph{126} changepoints, at
#' 1, 3, 4, 6, 7, ...; \code{rep(0, 100)} raises "No change-points found";
#' \code{rep(-2.5, 60)} returns the sentinel 0. A constant series plainly has
#' no changepoint, and every other search wrapper here reports none, so this
#' one short-circuits to the empty result. Constancy is decided by exact
#' equality, so a series with tiny but genuine variation still reaches the
#' engine.
#' @export
idetect_wrapper <- function(x, seed = NULL, ...) {

  need_pkg("IDetect")
  validate_data(x)
  data_vec <- as_uni_vector(x, "idetect")

  # See the "Constant input" section: on a flat series the engine invents
  # changepoints instead of reporting none.
  if (is_constant(data_vec)) {
    return(ggcpt_build(data_vec, integer(0), method = "idetect",
                       change_in = "mean",
                       penalty = list(type = "threshold", value = NA_real_),
                       call = match.call()))
  }

  if (!is.null(seed)) set.seed(seed)

  # IDetect::ID() errors (rather than returning an empty set) when it finds
  # no changepoints; normalise that to the empty-ggcpt contract every other
  # wrapper follows.
  fit <- tryCatch(
    IDetect::ID(data_vec, ...),
    error = function(e) {
      msg <- conditionMessage(e)
      # No changepoints, or the engine choking on a short valid series, are
      # both "no changepoints" outcomes, not user errors.
      if (grepl("No change-points found", msg, fixed = TRUE) ||
          grepl("wrong sign in 'by' argument", msg, fixed = TRUE) ||
          grepl("Sample size is too small", msg, fixed = TRUE)) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  cp_indices <- if (is.null(fit)) integer(0) else as.integer(fit$cpt)
  cp_indices <- cp_indices[!is.na(cp_indices)]

  ggcpt_build(
    data_vec, cp_indices,
    method = "idetect",
    change_in = "mean",
    penalty = list(type = "threshold", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' TGUH wrapper
#'
#' Wraps the \code{breakfast} package for Tail-Greedy Unbalanced-Haar
#' detection, with information-criterion model selection.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{breakfast::breakfast()}.
#' @return A \code{ggcpt} object.
#' @export
tguh_wrapper <- function(x, ...) {

  need_pkg("breakfast")
  validate_data(x)
  data_vec <- as_uni_vector(x, "tguh")

  # Pin the model selector: breakfast's default choice ("lp") reports
  # spurious changepoints on constant data; "ic" (strengthened SIC) is the
  # selector the TGUH paper pairs with the solution path.
  fit <- suppressWarnings(
    breakfast::breakfast(data_vec, solution.path = "tguh",
                         model.selection = "ic", ...)
  )
  cp_indices <- breakfast_cpts(fit)

  ggcpt_build(
    data_vec, cp_indices,
    method = "tguh",
    change_in = "mean",
    penalty = list(type = "sSIC", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}
