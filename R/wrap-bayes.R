#' Bayesian changepoint wrapper (Barry-Hartigan product partition model)
#'
#' Wraps \code{bcp::bcp()}, the MCMC implementation (Erdman and Emerson, 2007)
#' of the Barry and Hartigan (1993) product partition model. The engine
#' returns a \emph{posterior probability of a changepoint at every location};
#' locations whose posterior probability reaches \code{prob_threshold} are
#' reported as changepoints, and the full probability profile is kept so that
#' \code{\link{ggcpt_posterior}()} can draw the classic two-panel posterior
#' plot.
#'
#' @param x A numeric vector.
#' @param prob_threshold Posterior probability cutoff in \eqn{(0, 1)} above
#'   which a location is reported as a changepoint. Defaults to \code{0.5}.
#' @param burnin Number of burn-in MCMC iterations. Defaults to \code{50}.
#' @param mcmc Number of post-burn-in MCMC iterations. Defaults to \code{500}.
#' @param seed Optional seed for reproducibility of the MCMC run. The seed
#'   is scoped to this call: \code{.Random.seed} is saved and restored, so a
#'   seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Additional arguments passed to \code{bcp::bcp()}.
#' @return A \code{ggcpt} object. The \code{changepoints} tibble carries a
#'   \code{posterior_prob} column, and the \code{data} tibble carries the
#'   posterior mean in its \code{fitted} column.
#' @references
#' \insertRef{barry1993bayesian}{ggchangepoint}
#'
#' \insertRef{erdman2007bcp}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("bcp", quietly = TRUE)
#' res <- bcp_wrapper(c(rnorm(60), rnorm(60, 4)), seed = 2026)
#' res$changepoints
#' ggcpt_posterior(res)
#' @family changepoint engines
bcp_wrapper <- function(x, prob_threshold = 0.5, burnin = 50, mcmc = 500,
                        seed = NULL, ...) {
  need_pkg("bcp")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(burnin, "burnin", min = 1)

  validate_data(x)
  validate_scalar(prob_threshold, "prob_threshold", min = 0, max = 1,
                  min_open = TRUE)
  data_vec <- as_uni_vector(x, "bcp")
  # bcp::bcp() crashes the R session (segfault in its C++ code) on n = 3.
  if (length(data_vec) < 4) {
    stop("`x` must have at least 4 observations for the bcp engine.",
         call. = FALSE)
  }
  local_seed(seed)

  # Loading bcp attaches `package:bcp` and `package:grid`; a detection call
  # should not change where the caller's names resolve from.
  fit <- with_search_path_restored(
    bcp::bcp(data_vec, burnin = burnin, mcmc = mcmc, ...))

  # bcp's posterior.prob[i] is the posterior probability of a changepoint
  # between i and i + 1, i.e. location i in the "left" convention. The last
  # element is NA by construction.
  prob <- as.numeric(fit$posterior.prob)
  cp_indices <- which(!is.na(prob) & prob >= prob_threshold)

  ggcpt_build(
    data_vec, cp_indices,
    method = "bcp",
    change_in = "mean",
    penalty = list(type = "prob_threshold", value = prob_threshold),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) {
      list(posterior_prob = prob[cp_indices])
    },
    fitted = as.numeric(fit$posterior.mean[, 1])
  )
}

#' Bayesian online changepoint detection wrapper (BOCPD)
#'
#' Wraps \code{ocp::onlineCPD()}, an implementation of Bayesian Online
#' Changepoint Detection (Adams and MacKay, 2007). BOCPD recursively updates
#' a posterior over the current \emph{run length} (time since the last
#' change); the maximum a posteriori set of changepoints is reported, and the
#' full run-length posterior is kept so that \code{\link{ggcpt_runlength}()}
#' can draw the signature run-length heatmap.
#'
#' @param x A numeric vector.
#' @param hazard Constant hazard rate \eqn{1/\lambda} of the change process;
#'   larger \code{hazard} values mean changes are expected less often.
#'   Defaults to \code{100} (the upstream default).
#' @param ... Additional arguments passed to \code{ocp::onlineCPD()}.
#' @return A \code{ggcpt} object with the MAP changepoint set. The full
#'   \code{ocp} fit (including the run-length posterior) is kept in
#'   \code{$fit}.
#' @references
#' \insertRef{adams2007bocpd}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("ocp", quietly = TRUE)
#' res <- bocpd_wrapper(c(rnorm(60), rnorm(60, 4)))
#' res$changepoints
#' ggcpt_runlength(res)
#' @family changepoint engines
bocpd_wrapper <- function(x, hazard = 100, ...) {
  need_pkg("ocp")
  reject_renamed_args(list(...), "bocpd")
  reject_managed_args(list(...), "bocpd", c(
    getR = paste("the wrapper needs the run-length posterior to report",
                 "changepoint probabilities and to draw",
                 "`ggcpt_runlength()`")))
  validate_scalar(hazard, "hazard", min = 0, min_open = TRUE)

  validate_data(x)
  data_vec <- as_uni_vector(x, "bocpd")

  fit <- ocp::onlineCPD(data_vec, getR = TRUE,
                        hazard_func = function(x, lambda) {
    ocp::const_hazard(x, lambda = hazard)
  }, ...)

  # The MAP changepoint list opens with 1 (the start of the series) and uses
  # the "first index of the right segment" convention; normalise to "left".
  cp_raw <- as.integer(unlist(fit$changepoint_lists$maxCPs))
  cp_indices <- cp_raw[cp_raw > 1] - 1L

  ggcpt_build(
    data_vec, cp_indices,
    method = "bocpd",
    change_in = "mean",
    penalty = list(type = "hazard", value = hazard),
    fit = fit,
    call = match.call()
  )
}

#' BEAST wrapper — Bayesian estimation of abrupt change, seasonality, and trend
#'
#' Wraps \code{Rbeast::beast()} (Zhao et al., 2019), a Bayesian
#' model-averaging ensemble that estimates the number and location of trend
#' changepoints together with their posterior occurrence probabilities.
#' Locations whose posterior probability reaches \code{prob_threshold} are
#' reported; the probability profile renders via
#' \code{\link{ggcpt_posterior}()}.
#'
#' @param x A numeric vector (treated as a non-seasonal series).
#' @param prob_threshold Posterior probability cutoff in \eqn{(0, 1)} above
#'   which a candidate trend changepoint is reported. Defaults to \code{0.5}.
#' @param seed Optional seed for the engine's MCMC sampler (passed to
#'   \code{Rbeast::beast()} as \code{mcmc.seed}). The seed is scoped to this
#'   call: \code{.Random.seed} is saved and restored, so a seeded call
#'   inside a simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments passed to \code{Rbeast::beast()}.
#' @return A \code{ggcpt} object. The \code{changepoints} tibble carries
#'   \code{posterior_prob}, and the \code{data} tibble carries the posterior
#'   mean trend in its \code{fitted} column.
#' @references
#' \insertRef{zhao2019beast}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("Rbeast", quietly = TRUE) && .Platform$OS.type != "windows"
#' # try(): Rbeast intermittently returns an all-NaN fit and the condition
#' # can persist for a session, so a check must not fail on it -- the
#' # wrapper reports it by name when it happens.
#' res <- try(beast_wrapper(c(rnorm(60), rnorm(60, 4)), seed = 2026),
#'            silent = TRUE)
#' if (!inherits(res, "try-error")) res$changepoints
#' @family changepoint engines
beast_wrapper <- function(x, prob_threshold = 0.5, seed = NULL, ...) {
  need_pkg("Rbeast")
  reject_managed_args(list(...), "beast", c(
    season = paste("the wrapper runs Rbeast in trend-only mode; a seasonal",
                   "decomposition changes what the reported breakpoints",
                   "mean"),
    quiet = "the wrapper keeps the engine's console narration out of the result",
    print.progress = "the wrapper keeps the engine's progress bar out of the result"))
  validate_data(x)
  validate_scalar(prob_threshold, "prob_threshold", min = 0, max = 1,
                  min_open = TRUE)
  data_vec <- as_uni_vector(x, "beast")

  args <- list(y = data_vec, season = "none", quiet = TRUE,
               print.progress = FALSE, ...)
  # Rbeast::beast() has no `seed` argument; its sampler is seeded via
  # mcmc.seed (0 means "random").
  if (!is.null(seed)) args$mcmc.seed <- seed
  fit <- do.call(Rbeast::beast, args)
  # Rbeast (<= 1.0.2) intermittently returns an all-NaN fit. The rate is not
  # quotable: re-measured across fresh processes on one identical series it
  # was 0 of 8, 1 of 8, 1 of 10 and 30 of 30, so a session either mostly
  # works or mostly does not, and an earlier note here claiming "roughly
  # 0.7% of calls" was reporting one session as if it were a rate. A retry
  # recovers it often enough to be worth trying -- and sometimes not at
  # all: five consecutive attempts failed in one session. So retry a few
  # times (each is cheap) and then fail loudly, rather than reporting "no
  # changepoints" from a broken fit. The perturbed call in between is there
  # because identical retries can stay stuck.
  #
  # Three explanations measured and refuted, so they are not re-chased.
  # (1) Other compiled engines loaded in the same session: an earlier note
  # blamed these, but a session with 66 namespaces loaded ran 5 of 5 seeds
  # clean. (2) `do.call()` inlining the whole series into the call object,
  # which is a real hazard this package avoids elsewhere: inline and
  # quoted-symbol forms were both 10 of 10 clean while a plain direct call
  # produced the all-NaN in the same process. (3) `mcmc.seed`, and the
  # chain configuration generally: `mcmc.chains`, `mcmc.samples` and
  # `mcmc.burnin` all gave a finite fit on every seed that had just failed
  # through the wrapper.
  # With `mcmc.seed` set from `seed`, every retry runs the same chain, so a
  # SEEDED call that hits the bug has less variation to recover from than an
  # unseeded one -- the perturbed call in between is doing all the work.
  # Vary the perturbation across attempts so the seeded case is not just the
  # same call six times.
  attempt <- 1
  while (!is.finite(fit$trend$ncp) && attempt < 6) {
    perturbed <- args
    perturbed$dump.ci <- TRUE
    perturbed$mcmc.seed <- attempt
    try(do.call(Rbeast::beast, perturbed), silent = TRUE)
    fit <- do.call(Rbeast::beast, args)
    attempt <- attempt + 1
  }
  if (!is.finite(fit$trend$ncp)) {
    stop("Rbeast::beast() returned an invalid (all-NaN) fit on ", attempt,
         " attempts. This is an intermittent upstream issue, and it is not ",
         "always transient: it can persist for the rest of an R session, ",
         "so a fresh session is a better bet than another call. Measured ",
         "rates across sessions on one identical series ranged from 0 to ",
         "100 percent.", call. = FALSE)
  }

  # beast reports candidate changepoints (most probable first) as the first
  # index of the new regime; keep those clearing the probability cutoff and
  # normalise to the "left" convention.
  cp_raw <- as.numeric(fit$trend$cp)
  cp_prob <- as.numeric(fit$trend$cpPr)
  keep <- !is.na(cp_raw) & !is.na(cp_prob) & cp_prob >= prob_threshold
  cp_indices <- as.integer(round(cp_raw[keep])) - 1L
  cp_prob <- cp_prob[keep]
  ord <- order(cp_indices)
  cp_indices <- cp_indices[ord]
  cp_prob <- cp_prob[ord]
  in_range <- cp_indices >= 1 & cp_indices < length(data_vec)
  cp_indices <- cp_indices[in_range]
  cp_prob <- cp_prob[in_range]

  ggcpt_build(
    data_vec, cp_indices,
    method = "beast",
    change_in = "mean",
    penalty = list(type = "prob_threshold", value = prob_threshold),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) {
      list(posterior_prob = cp_prob)
    },
    fitted = as.numeric(fit$trend$Y)
  )
}
