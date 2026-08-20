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
#'   \code{"hsmuce"} additionally refuses a series whose point-to-point
#'   variation lies more than about seven orders of magnitude below its own
#'   scale — a globally flat series, or a step whose segments are numerically
#'   constant, as \code{cpt_simulate(sd = 0)} produces once any rounding is
#'   added. \pkg{stepR}'s heterogeneous variance estimator aborts the \R
#'   session on such input rather than raising an error, so it cannot be
#'   caught. \code{"gauss"} handles the whole range.
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

  # HSMUCE estimates a variance per *segment*, and when the data carry
  # essentially no noise at that scale stepR's compiled code does not raise
  # an R error -- it aborts the session, so nothing can catch it and the user
  # loses their work. Measured: it dies whenever the local variation sits
  # more than about seven orders of magnitude below the data's magnitude,
  # both for a globally flat series (rep(4, 300) + rnorm(300, 0, 2e-7)) and,
  # more dangerously, for an ordinary-looking step whose segments are
  # numerically constant (c(rep(0, 150), rep(5, 150)) + rnorm(300, 0, 1e-9)) --
  # which is exactly what cpt_simulate(sd = 0) produces once any rounding is
  # added. An *exactly* noiseless series is safe, because the engine
  # short-circuits, so only the numerically-degenerate band is refused. The
  # threshold sits a little inside the crash zone: HSMUCE's variance estimate
  # is meaningless there anyway (it returned five to thirteen "changepoints"
  # on such input), and refusing beats terminating the session.
  if (family == "hsmuce") {
    local_scale <- stats::mad(diff(data_vec))
    magnitude <- max(abs(data_vec))
    if (local_scale > 0 && magnitude > 0 && local_scale < 1e-7 * magnitude) {
      stop("`hsmuce` cannot be used on this series: its point-to-point ",
           "variation (", format(local_scale, digits = 3), ") is more than ",
           "seven orders of magnitude below the data's own scale (",
           format(magnitude, digits = 3), "), and stepR's heterogeneous ",
           "variance estimator aborts the R session on such input rather ",
           "than returning. Use `family = \"gauss\"` (SMUCE), which handles ",
           "it.", call. = FALSE)
    }
  }

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
