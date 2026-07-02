#' fastcpd wrapper — fast changepoint detection via sequential gradient descent
#'
#' Wraps the \pkg{fastcpd} package (Li and Zhang, 2024), a modern PELT-family
#' engine that pairs pruning with sequential gradient descent so that exact or
#' near-exact segmentations of many model families run in near-linear time.
#' This wrapper exposes the time-series families most useful alongside the
#' other engines: mean, variance, mean-and-variance, and AR/ARMA/GARCH model
#' changepoints.
#'
#' @param x A numeric vector, or (for \code{family} \code{"mean"},
#'   \code{"variance"}, \code{"meanvariance"}) a matrix with one row per time
#'   point for multivariate detection.
#' @param family Model family: \code{"mean"}, \code{"variance"},
#'   \code{"meanvariance"}, \code{"ar"}, \code{"arma"}, or \code{"garch"}.
#'   Defaults to \code{"mean"}.
#' @param order Model order for \code{"ar"} (a single integer),
#'   \code{"arma"} (length-2), or \code{"garch"} (length-2). Defaults to
#'   \code{1} for AR, \code{c(1, 1)} otherwise.
#' @param ... Additional arguments passed to the corresponding
#'   \code{fastcpd::fastcpd.*()} function (e.g. \code{beta}, \code{trim}).
#' @return A \code{ggcpt} object.
#' @references
#' \insertRef{li2024fastcpd}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("fastcpd", quietly = TRUE)
#' set.seed(2026)
#' res <- fastcpd_wrapper(c(rnorm(100), rnorm(100, 4)))
#' res$changepoints
fastcpd_wrapper <- function(x, family = c("mean", "variance", "meanvariance",
                                          "ar", "arma", "garch"),
                            order = NULL, ...) {
  need_pkg("fastcpd")
  family <- match.arg(family)

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  if (is_mv && !family %in% c("mean", "variance", "meanvariance")) {
    stop("Multivariate input is only supported for `family` \"mean\", ",
         "\"variance\", or \"meanvariance\".", call. = FALSE)
  }
  X <- if (is_mv) as_mv_matrix(x) else as.numeric(x)
  data_vec <- if (is_mv) as.numeric(X[, 1]) else X

  fit <- switch(family,
    mean = fastcpd::fastcpd.mean(X, r.progress = FALSE, ...),
    variance = fastcpd::fastcpd.variance(X, r.progress = FALSE, ...),
    meanvariance = fastcpd::fastcpd.meanvariance(X, r.progress = FALSE, ...),
    ar = fastcpd::fastcpd.ar(X, order = order %||% 1, r.progress = FALSE, ...),
    arma = fastcpd::fastcpd.arma(X, order = order %||% c(1, 1),
                                 r.progress = FALSE, ...),
    garch = fastcpd::fastcpd.garch(X, order = order %||% c(1, 1),
                                   r.progress = FALSE, ...)
  )

  change_lab <- switch(family,
    mean = "mean", variance = "var", meanvariance = "meanvar",
    ar = "model (AR)", arma = "model (ARMA)", garch = "model (GARCH)"
  )

  ggcpt_build(
    data_vec, as.integer(fit@cp_set),
    method = "fastcpd",
    change_in = change_lab,
    penalty = list(type = "MBIC", value = NA_real_),
    fit = fit,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(X)
  )
}
