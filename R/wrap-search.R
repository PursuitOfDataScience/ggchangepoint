#' WBS wrapper — Wild Binary Segmentation
#'
#' Wraps the \code{wbs} package for randomised changepoint detection via
#' Wild Binary Segmentation.
#'
#' @param x A numeric vector.
#' @param n_intervals Number of random intervals. Defaults to \code{5000}.
#' @param threshold Manual threshold for detection. If \code{NULL}, uses
#'   the strengthened Schwarz Information Criterion (sSIC).
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{wbs::wbs()}.
#' @return A \code{ggcpt} object.
#' @export
wbs_wrapper <- function(x, n_intervals = 5000, threshold = NULL, seed = NULL, ...) {

  if (!requireNamespace("wbs", quietly = TRUE)) {
    stop("Package 'wbs' is required. Install it with install.packages('wbs').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  if (!is.null(seed)) set.seed(seed)

  fit <- wbs::wbs(data_vec, M = n_intervals, ...)
  if (!is.null(threshold)) {
    cp <- wbs::changepoints(fit, th = threshold)
  } else {
    cp <- wbs::changepoints(fit, penalty = "ssic.penalty")
  }

  cp_indices <- sort(as.integer(cp$cpt.th[[1]]))

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "wbs"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "wbs",
    change_in = "mean",
    penalty = list(type = "sSIC", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' WBS2 wrapper — Wild Binary Segmentation 2
#'
#' Wraps the \code{breakfast} package. Requires the \code{breakfast} package.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{breakfast::breakfast()}.
#' @return A \code{ggcpt} object.
#' @export
wbs2_wrapper <- function(x, ...) {

  if (!requireNamespace("breakfast", quietly = TRUE)) {
    stop("Package 'breakfast' is required. Install it with install.packages('breakfast').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  fit <- breakfast::breakfast(data_vec, solution.path = "wbs2", model.selection = "sdll", ...)
  cp_indices <- sort(as.integer(fit$cptmodel.list[[1]]$cpts))

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "wbs2"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "wbs2",
    change_in = "mean",
    penalty = list(type = "SDLL", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' NOT wrapper — Narrowest-Over-Threshold
#'
#' Wraps the \code{not} package for changepoint detection via the
#' Narrowest-Over-Threshold method.
#'
#' @param x A numeric vector.
#' @param contrast Contrast type. One of \code{"pcwsConstMean"},
#'   \code{"pcwsLinContMean"}, \code{"pcwsLinMean"},
#'   \code{"pcwsConstMeanVar"}. Defaults to \code{"pcwsConstMean"}.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{not::not()}.
#' @return A \code{ggcpt} object.
#' @export
not_wrapper <- function(x, contrast = "pcwsConstMean", seed = NULL, ...) {

  if (!requireNamespace("not", quietly = TRUE)) {
    stop("Package 'not' is required. Install it with install.packages('not').",
         call. = FALSE)
  }

  contrast <- match.arg(contrast, c("pcwsConstMean", "pcwsLinContMean",
                                     "pcwsLinMean", "pcwsConstMeanVar"))

  validate_data(x)
  data_vec <- as.numeric(x)

  if (!is.null(seed)) set.seed(seed)

  fit <- not::not(data_vec, contrast = contrast, ...)
  feat <- not::features(fit)
  cp_indices <- sort(feat$cpt)

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "not"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "not",
    change_in = "mean",
    penalty = list(type = "sSIC", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' MOSUM wrapper — Moving Sum
#'
#' Wraps the \code{mosum} package for moving-sum-based changepoint detection.
#'
#' @param x A numeric vector.
#' @param G Bandwidth. If \code{NULL}, automatically selected.
#' @param multiscale Logical. Use multiscale MOSUM? Defaults to \code{FALSE}.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to \code{mosum::mosum()}.
#' @return A \code{ggcpt} object.
#' @export
mosum_wrapper <- function(x, G = NULL, multiscale = FALSE, seed = NULL, ...) {

  if (!requireNamespace("mosum", quietly = TRUE)) {
    stop("Package 'mosum' is required. Install it with install.packages('mosum').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  if (!is.null(seed)) set.seed(seed)

  if (is.null(G)) {
    G <- ceiling(min(length(data_vec) / 10, 100))
  }

  fit <- mosum::mosum(data_vec, G = G, ...)
  cp_indices <- sort(fit$cpts)

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "mosum"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "mosum",
    change_in = "mean",
    penalty = list(type = "threshold", value = fit$threshold),
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
#' @return A \code{ggcpt} object.
#' @export
idetect_wrapper <- function(x, seed = NULL, ...) {

  if (!requireNamespace("IDetect", quietly = TRUE)) {
    stop("Package 'IDetect' is required. Install it with install.packages('IDetect').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  if (!is.null(seed)) set.seed(seed)

  fit <- IDetect::ID(data_vec, ...)
  # IDetect::ID() reports detected changepoints in `$cpt` (0 or numeric(0)
  # when none are found), not `$change_points`.
  cp_indices <- sort(as.integer(fit$cpt))
  cp_indices <- cp_indices[cp_indices > 0 & cp_indices < length(data_vec)]

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "IDetect"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "IDetect",
    change_in = "mean",
    penalty = list(type = "threshold", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' TGUH wrapper
#'
#' Wraps the \code{breakfast} package for Tail-Greedy Unbalanced-Haar detection.
#'
#' @param x A numeric vector.
#' @param ... Additional arguments passed to \code{breakfast::breakfast()}.
#' @return A \code{ggcpt} object.
#' @export
tguh_wrapper <- function(x, ...) {

  if (!requireNamespace("breakfast", quietly = TRUE)) {
    stop("Package 'breakfast' is required. Install it with install.packages('breakfast').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  fit <- breakfast::breakfast(data_vec, solution.path = "tguh", ...)
  cp_indices <- sort(as.integer(fit$cptmodel.list[[1]]$cpts))

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "tguh"))
  }

  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[cp_indices]
  )

  segments <- build_segments(data_vec, cp_indices)
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  new_ggcpt(
    changepoints = changepoints,
    segments = segments,
    data = data_tbl,
    method = "tguh",
    change_in = "mean",
    penalty = list(type = "threshold", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}
