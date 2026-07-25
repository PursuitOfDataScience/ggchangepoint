#' SMUCE / HSMUCE wrapper — multiscale changepoint inference
#'
#' Wraps \code{stepR::stepFit()} for the Simultaneous MUltiscale Changepoint
#' Estimator (SMUCE) of Frick, Munk and Sieling (2014) and its heterogeneous
#' extension HSMUCE (Pein, Sieling and Munk, 2017). SMUCE estimates a step
#' function subject to a simultaneous multiscale test at level \code{alpha};
#' the level bounds the probability of over-estimating the number of
#' changepoints, and the fit delivers \emph{confidence intervals for every
#' changepoint location}, which populate the \code{ci_lower}/\code{ci_upper}
#' columns of the result and render via \code{autoplot(show_ci = TRUE)} or
#' \code{\link{geom_cpt_ci}()}.
#'
#' @param x A numeric vector.
#' @param alpha Significance level of the multiscale test in \eqn{(0, 1)};
#'   smaller values yield more conservative (fewer-changepoint) fits.
#'   Defaults to \code{0.5}, the upstream recommendation for estimation.
#' @param family Noise model: \code{"gauss"} (SMUCE, homogeneous Gaussian
#'   noise) or \code{"hsmuce"} (HSMUCE, segment-wise variance). Defaults to
#'   \code{"gauss"}. The remaining \code{stepR} families (\code{"jsmurf"},
#'   \code{"mDependentPS"}, ...) all require a filter or covariance
#'   specification; call \code{stepR::stepFit()} directly for those.
#' @param ... Additional arguments passed to \code{stepR::stepFit()}.
#' @return A \code{ggcpt} object. The \code{changepoints} tibble carries
#'   \code{ci_lower}/\code{ci_upper} (confidence interval for each
#'   changepoint location) and the \code{data} tibble carries the SMUCE step
#'   fit in its \code{fitted} column.
#' @references
#' \insertRef{frick2014smuce}{ggchangepoint}
#'
#' \insertRef{pein2017hsmuce}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("stepR", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' x <- c(rnorm(100), rnorm(100, 3))
#' res <- smuce_wrapper(x)
#' res$changepoints
#' ggplot2::autoplot(res, show_ci = TRUE)
#' }
smuce_wrapper <- function(x, alpha = 0.5,
                          family = c("gauss", "hsmuce"), ...) {
  need_pkg("stepR")
  family <- match.arg(family)

  validate_data(x)
  data_vec <- as_uni_vector(x, if (family == "hsmuce") "hsmuce" else "smuce")

  fit <- stepR::stepFit(data_vec, alpha = alpha, family = family,
                        jumpint = TRUE, ...)

  # stepFit returns one row per segment; the changepoint before segment i
  # (i > 1) is at rightIndex[i - 1] (last index of the left segment), with a
  # confidence interval given by the bounds on segment i's left index.
  n_seg <- length(fit$rightIndex)
  method_name <- if (family == "hsmuce") "hsmuce" else "smuce"

  cp_indices <- integer(0)
  ci_lower <- integer(0)
  ci_upper <- integer(0)
  if (n_seg > 1) {
    cp_indices <- as.integer(fit$rightIndex[-n_seg])
    ci_lower <- pmax(1L, as.integer(fit$leftIndexLeftBound[-1]) - 1L)
    ci_upper <- pmin(length(data_vec) - 1L,
                     as.integer(fit$leftIndexRightBound[-1]) - 1L)
  }

  fitted <- rep(fit$value, times = fit$rightIndex - fit$leftIndex + 1)

  ggcpt_build(
    data_vec, cp_indices,
    method = method_name,
    change_in = "mean",
    penalty = list(type = "alpha", value = alpha),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) {
      list(ci_lower = ci_lower, ci_upper = ci_upper)
    },
    fitted = fitted
  )
}
