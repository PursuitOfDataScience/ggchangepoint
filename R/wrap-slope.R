#' CPOP wrapper — optimal change-in-slope detection
#'
#' Wraps \code{cpop::cpop()} (Fearnhead, Maidstone and Letchford, 2019;
#' Fearnhead and Grose, 2024): exact penalised estimation of a
#' \emph{continuous} piecewise-linear mean via dynamic programming with
#' functional pruning. This is the engine behind
#' \code{cpt_detect(change_in = "slope")}. The fitted broken line is stored
#' in the \code{fitted} column and renders via
#' \code{autoplot(show_fit = TRUE)}.
#'
#' @param x A numeric vector.
#' @param penalty Penalty for adding a changepoint. Defaults to
#'   \code{2 * log(length(x))}. \code{\link{cpt_detect}} resolves its own
#'   \code{"MBIC"} default to a stronger numeric value, so the two entry
#'   points need not agree unless \code{penalty} is given.
#' @param sd Noise standard deviation; when \code{NULL} it is estimated from
#'   the data by the engine's default difference-based estimator.
#' @param ... Additional arguments passed to \code{cpop::cpop()}.
#' @return A \code{ggcpt} object with \code{change_in = "slope"}. For a
#'   continuous fit the reported location is the kink point itself.
#' @references
#' \insertRef{fearnhead2019cpop}{ggchangepoint}
#'
#' \insertRef{fearnhead2024cpop}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("cpop", quietly = TRUE)
#' set.seed(2026)
#' y <- cumsum(c(rep(0.3, 100), rep(-0.4, 100))) + rnorm(200)
#' res <- cpop_wrapper(y)
#' res$changepoints
#' ggplot2::autoplot(res, show_fit = TRUE)
cpop_wrapper <- function(x, penalty = NULL, sd = NULL, ...) {
  need_pkg("cpop")

  if (!is.null(sd)) validate_scalar(sd, "sd", min = 0, min_open = TRUE)

  validate_data(x)
  data_vec <- as_uni_vector(x, "cpop")
  n <- length(data_vec)

  if (is.null(penalty)) {
    penalty <- 2 * log(n)
  }

  # Pass an explicit 1-based x grid so reported locations are indices.
  args <- list(y = data_vec, x = seq_len(n), beta = penalty, ...)
  if (!is.null(sd)) args$sd <- sd
  fit <- do.call(cpop::cpop, args)

  cps <- cpop::changepoints(fit)
  cp_indices <- as.integer(round(cps$location))

  est <- tryCatch(cpop::estimate(fit, x = seq_len(n)), error = function(e) NULL)
  fitted <- if (!is.null(est)) as.numeric(est$y_hat)

  ggcpt_build(
    data_vec, cp_indices,
    method = "cpop",
    change_in = "slope",
    penalty = list(type = "Manual", value = penalty),
    fit = fit,
    call = match.call(),
    fitted = fitted
  )
}
