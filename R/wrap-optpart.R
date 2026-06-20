#' FPOP wrapper — Functional Pruning Optimal Partitioning
#'
#' Wraps the \code{fpop} package for optimal changepoint detection via
#' functional pruning.
#'
#' @param x A numeric vector.
#' @param penalty Penalty value. Defaults to \code{2 * log(length(x))} (BIC).
#' @param ... Additional arguments passed to \code{fpop::Fpop()}.
#' @return A \code{ggcpt} object.
#' @export
fpop_wrapper <- function(x, penalty = NULL, ...) {

  if (!requireNamespace("fpop", quietly = TRUE)) {
    stop("Package 'fpop' is required. ",
         "Install it with install.packages('fpop').",
         call. = FALSE)
  }

  validate_data(x)
  data_vec <- as.numeric(x)

  if (is.null(penalty)) {
    penalty <- 2 * log(length(data_vec))
  }

  fit <- fpop::Fpop(data_vec, lambda = penalty, ...)
  cp_indices <- sort(fit$t.est)
  cp_indices <- cp_indices[cp_indices > 0 & cp_indices < length(data_vec)]

  if (length(cp_indices) == 0) {
    return(ggcpt_empty(data_vec, "fpop"))
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
  starts <- c(1, cp_indices + 1)
  ends <- c(cp_indices, n)
  n_seg <- length(starts)

  tibble::tibble(
    seg_id = seq_len(n_seg),
    start = starts,
    end = ends,
    n = ends - starts + 1,
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
    call = match.call()
  )
}
