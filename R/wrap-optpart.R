#' FPOP wrapper — Functional Pruning Optimal Partitioning
#'
#' Wraps the \code{fpop} package for optimal changepoint detection via
#' functional pruning.
#'
#' @param x A numeric vector.
#' @param penalty Penalty value. Defaults to \code{2 * log(length(x))} (BIC).
#'   This is an \emph{absolute} penalty on the residual sum of squares, so
#'   it is only calibrated for noise of standard deviation 1: on wider data
#'   the default under-penalises badly and the segmentation shatters.
#'   Standardise the series, or scale the penalty by the noise variance
#'   (for example \code{2 * log(length(x)) * stats::var(diff(x)) / 2}).
#'   See the scale-sensitivity section of \code{\link{cpt_detect}}. This
#'   default differs from the one \code{\link{cpt_detect}} applies, which
#'   resolves its \code{"MBIC"} default to a stronger numeric value, so the
#'   two entry points need not agree unless \code{penalty} is given.
#' @param ... Additional arguments passed to \code{fpop::Fpop()}.
#' @return A \code{ggcpt} object.
#' @export
fpop_wrapper <- function(x, penalty = NULL, ...) {

  need_pkg("fpop")
  validate_data(x)
  data_vec <- as_uni_vector(x, "fpop")

  if (is.null(penalty)) {
    penalty <- 2 * log(length(data_vec))
  }

  fit <- fpop::Fpop(data_vec, lambda = penalty, ...)
  cp_indices <- as.integer(fit$t.est)

  ggcpt_build(
    data_vec, cp_indices,
    method = "fpop",
    change_in = "mean",
    penalty = list(type = "Manual", value = penalty),
    fit = fit,
    call = match.call()
  )
}

# Internal: build segments tibble from changepoint indices
#' @noRd
build_segments <- function(data_vec, cp_indices) {
  n <- length(data_vec)
  starts <- c(1L, cp_indices + 1L)
  ends <- c(cp_indices, n)
  n_seg <- length(starts)

  tibble::tibble(
    seg_id = seq_len(n_seg),
    start = starts,
    end = ends,
    n = ends - starts + 1L,
    param_estimate = vapply(seq_len(n_seg), function(i) {
      mean(data_vec[starts[i]:ends[i]])
    }, numeric(1))
  )
}

# Internal: create an empty ggcpt for no-change results
#' @noRd
ggcpt_empty <- function(data_vec, method = "unknown") {
  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = as.numeric(data_vec))
  segments <- tibble::tibble(
    seg_id = 1L,
    start = 1L,
    end = length(data_vec),
    n = length(data_vec),
    param_estimate = mean(data_vec, na.rm = TRUE)
  )
  new_ggcpt(
    changepoints = tibble::tibble(cp = integer(), cp_value = numeric()),
    segments = segments,
    data = data_tbl,
    method = method,
    change_in = "mean",
    penalty = list(type = NA_character_, value = NA_real_),
    fit = NULL,
    call = NULL
  )
}
