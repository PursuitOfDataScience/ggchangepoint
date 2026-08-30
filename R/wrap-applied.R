# ---------------------------------------------------------------------------
# Engine wave #2, part 3: the applied vocabularies and the fast path.
#
#   pettitt / buishand / snht  (trend)             hydrology and climatology
#   taylor                     (ChangePointTaylor) quality control, Six Sigma
#   bfast                      (bfast)             remote sensing
#   wbsts                      (wbsts)             nonstationary time series
#   binsegrcpp                 (binsegRcpp)        a fast binary-segmentation
#                                                  path across many losses
#
# These are small wrappers with disproportionate reach: they let whole
# applied communities into a tidy, ggplot2-native interface using the method
# names they already use.
# ---------------------------------------------------------------------------

#' Classical single-changepoint tests (Pettitt, Buishand, SNHT)
#'
#' Wraps the three single-change tests that hydrology and climatology use as
#' their standard vocabulary, from the \pkg{trend} package. All three test
#' \eqn{H_0}: no change against a single change in the mean, differing in how
#' they measure it:
#' \describe{
#'   \item{\code{"pettitt"}}{a rank-based (Mann–Whitney) statistic —
#'     distribution-free and robust to outliers.}
#'   \item{\code{"buishand"}}{the Buishand range test, based on rescaled
#'     adjusted partial sums; assumes normality.}
#'   \item{\code{"snht"}}{the standard normal homogeneity test of Alexandersson,
#'     the reference method for detecting inhomogeneities in climate records.}
#' }
#' Each reports a location \emph{and} a p-value, and — unlike most engines
#' here — that p-value is valid, because the location was not chosen from a
#' larger model search.
#'
#' @param x A numeric vector.
#' @param test Which test to run. Defaults to \code{"pettitt"}.
#' @param alpha Significance level below which the changepoint is reported.
#'   Defaults to \code{0.05}. A test that does not reject returns an empty
#'   result rather than an unsupported location.
#' @param ... Additional arguments passed to the \pkg{trend} function.
#' @return A \code{ggcpt} object with the test's \code{p_value} and
#'   \code{statistic} on the changepoints tibble. The per-position test
#'   statistic is available through \code{\link{cpt_statistic}()}.
#' @references
#' \insertRef{pettitt1979test}{ggchangepoint}
#'
#' \insertRef{buishand1982tests}{ggchangepoint}
#'
#' \insertRef{alexandersson1986snht}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("trend", quietly = TRUE)
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 2))
#' trend_wrapper(x, test = "pettitt")
#' trend_wrapper(x, test = "snht")
#' @family changepoint engines
trend_wrapper <- function(x, test = c("pettitt", "buishand", "snht"),
                          alpha = 0.05, ...) {
  need_pkg("trend")
  test <- match.arg(test)
  validate_scalar(alpha, "alpha", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_data(x)
  data_vec <- as_uni_vector(x, test)

  fit <- switch(test,
    pettitt = trend::pettitt.test(data_vec, ...),
    buishand = trend::br.test(data_vec, ...),
    snht = trend::snh.test(data_vec, ...)
  )
  p_value <- as.numeric(fit$p.value)
  loc <- as.integer(fit$estimate)[1]
  cp <- if (is.finite(p_value) && p_value <= alpha && !is.na(loc)) {
    loc
  } else {
    integer(0)
  }

  ggcpt_build(
    data_vec, cp,
    method = test,
    change_in = "mean",
    penalty = list(type = "alpha", value = alpha),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp) > 0) {
      list(p_value = p_value, statistic = as.numeric(fit$statistic)[1])
    }
  )
}

#' Taylor's change point analyzer
#'
#' Wraps \code{ChangePointTaylor::change_point_analyzer()}: the
#' bootstrap-and-recursion procedure of Wayne Taylor that the quality-control
#' and Six Sigma community uses as its default. Each candidate is scored by
#' the bootstrap probability that a change occurred there, which gives a
#' confidence level per changepoint and a confidence interval for its
#' location — both carried onto the result.
#'
#' @param x A numeric vector.
#' @param n_bootstraps Bootstrap samples per candidate. Defaults to
#'   \code{1000}; the engine accepts 100 to 1,000,000.
#' @param min_candidate_conf Minimum confidence for a candidate to be
#'   considered. Defaults to \code{0.5}.
#' @param min_conf Minimum confidence for a changepoint to be reported.
#'   Defaults to \code{0.9}.
#' @param conf_level Confidence level of the reported location intervals.
#'   Defaults to \code{0.95}.
#' @param seed Optional seed (the procedure is bootstrap-based).
#' @return A \code{ggcpt} object with \code{ci_lower}/\code{ci_upper} (so
#'   \code{autoplot(show_ci = TRUE)} works) and a \code{confidence} column.
#' @references
#' \insertRef{taylor2000changepoint}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("ChangePointTaylor", quietly = TRUE)
#' set.seed(2026)
#' taylor_wrapper(c(rnorm(60), rnorm(60, 3)), n_bootstraps = 200, seed = 1)
#' @family changepoint engines
taylor_wrapper <- function(x, n_bootstraps = 1000, min_candidate_conf = 0.5,
                           min_conf = 0.9, conf_level = 0.95, seed = NULL) {
  need_pkg("ChangePointTaylor")
  # The engine's own limits, enforced here so the message names this
  # function's argument rather than the misspelled `n_bootraps` it would
  # otherwise complain about from several frames down.
  validate_scalar(n_bootstraps, "n_bootstraps", min = 100, max = 1e6)
  validate_scalar(min_candidate_conf, "min_candidate_conf", min = 0, max = 1)
  validate_scalar(min_conf, "min_conf", min = 0, max = 1)
  validate_scalar(conf_level, "conf_level", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_data(x)
  data_vec <- as_uni_vector(x, "taylor")
  n <- length(data_vec)
  if (!is.null(seed)) set.seed(seed)

  # The engine narrates ("3 Change(s) Identified", "NA supplied to 'label'")
  # through both stdout and the message stream; keep the console clean.
  suppressMessages(utils::capture.output(
    fit <- ChangePointTaylor::change_point_analyzer(
      data_vec, n_bootstraps = n_bootstraps,
      min_candidate_conf = min_candidate_conf,
      min_tbl_conf = min_conf, CI = conf_level
    )
  ))
  if (is.null(fit) || nrow(fit) == 0) {
    return(ggcpt_build(data_vec, integer(0), method = "taylor",
                       change_in = "mean",
                       penalty = list(type = "confidence", value = min_conf),
                       fit = fit, call = match.call()))
  }

  # change_ix is the first index of the NEW segment (the "right"
  # convention); this package reports the last index of the left one.
  cp <- as.integer(fit$change_ix) - 1L
  ci <- parse_taylor_ci(fit[[grep("^CI", names(fit))[1]]])
  ord <- order(cp)

  ggcpt_build(
    data_vec, cp[ord],
    method = "taylor",
    change_in = "mean",
    penalty = list(type = "confidence", value = min_conf),
    fit = fit,
    call = match.call(),
    extra_cp_cols = list(
      ci_lower = pmax(1L, pmin(ci$lower[ord] - 1L, n - 1L)),
      ci_upper = pmax(1L, pmin(ci$upper[ord] - 1L, n - 1L)),
      confidence = as.numeric(fit$change_conf)[ord]
    )
  )
}

# Internal: ChangePointTaylor reports its interval as the string
# "(100 - 101)", so it has to be parsed rather than read.
#' @noRd
parse_taylor_ci <- function(v) {
  v <- as.character(v)
  nums <- regmatches(v, gregexpr("-?[0-9]+", v))
  lower <- vapply(nums, function(p) {
    if (length(p) >= 1) as.integer(p[1]) else NA_integer_
  }, integer(1))
  upper <- vapply(nums, function(p) {
    if (length(p) >= 2) as.integer(p[2]) else NA_integer_
  }, integer(1))
  list(lower = lower, upper = upper)
}

#' BFAST wrapper — breaks for additive season and trend
#'
#' Wraps the \pkg{bfast} family (Verbesselt et al.), the standard tool in
#' remote sensing and land-cover monitoring. BFAST decomposes a seasonal
#' series into trend and seasonal components and detects breaks in each
#' separately, which is the right question for satellite time series where a
#' shift in phenology and a shift in level mean different things.
#'
#' @param x A numeric vector, or a \code{ts} — a \code{ts} is strongly
#'   preferred, because BFAST needs the seasonal frequency and cannot guess
#'   it. A bare vector is turned into a \code{ts} with \code{frequency}.
#' @param frequency Observations per season, used when \code{x} carries none.
#'   Defaults to \code{12}.
#' @param change_in Which component's breaks to report: \code{"mean"} or
#'   \code{"slope"} give the trend breaks (the usual choice),
#'   \code{"seasonality"} gives the seasonal ones.
#' @param h Minimal segment size as a fraction of the series. Defaults to
#'   \code{0.15}.
#' @param season Seasonal model: \code{"harmonic"} (default), \code{"dummy"}
#'   or \code{"none"}.
#' @param max_iter Maximum iterations of the trend/season loop. Defaults to
#'   \code{5}.
#' @param ... Additional arguments passed to \code{bfast::bfast()}.
#' @return A \code{ggcpt} object whose \code{fitted} column holds the
#'   estimated trend component (so \code{autoplot(show_fit = TRUE)} draws it)
#'   and whose changepoints carry \code{ci_lower}/\code{ci_upper} from
#'   \pkg{strucchange}'s break-date intervals when available.
#' @references
#' \insertRef{verbesselt2010bfast}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("bfast", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' season <- rep(sin(seq(0, 2 * pi, length.out = 12)), 10)
#' y <- stats::ts(c(rnorm(60, 1), rnorm(60, 5)) + season,
#'                frequency = 12, start = c(2000, 1))
#' bfast_wrapper(y)
#' }
#' @family changepoint engines
bfast_wrapper <- function(x, frequency = 12,
                          change_in = c("mean", "slope", "seasonality"),
                          h = 0.15, season = c("harmonic", "dummy", "none"),
                          max_iter = 5, ...) {
  need_pkg("bfast")
  change_in <- match.arg(change_in)
  season <- match.arg(season)
  if (change_in == "seasonality" && season == "none") {
    stop("`change_in = \"seasonality\"` needs a seasonal component to break: ",
         "use season = \"harmonic\" or season = \"dummy\".", call. = FALSE)
  }
  validate_scalar(h, "h", min = 0, max = 0.5, min_open = TRUE)
  validate_scalar(max_iter, "max_iter", min = 1)

  yt <- if (stats::is.ts(x)) {
    x
  } else {
    validate_data(x)
    stats::ts(as_uni_vector(x, "bfast"), frequency = frequency)
  }
  data_vec <- as.numeric(yt)
  n <- length(data_vec)
  if (stats::frequency(yt) < 2 && season != "none") {
    stop("`bfast` needs a seasonal frequency of at least 2 to fit a seasonal ",
         "component; pass a `ts` with the right frequency, set `frequency`, ",
         "or use `season = \"none\"`.", call. = FALSE)
  }

  fit <- bfast::bfast(yt, h = h, season = season, max.iter = max_iter, ...)
  last <- fit$output[[length(fit$output)]]
  bp <- if (change_in == "seasonality") {
    last[["bp.Wt", exact = TRUE]]
  } else {
    last[["bp.Vt", exact = TRUE]]
  }

  cp <- integer(0)
  ci_lower <- NULL
  ci_upper <- NULL
  # bfast reports "no breakpoints in this component" as a bare logical NA,
  # not as a breakpoints object with an empty slot, so `bp$breakpoints` is a
  # `$`-on-an-atomic-vector error rather than a missing value. That is the
  # ordinary outcome for `change_in = "seasonality"` on a series whose
  # seasonal amplitude is stable, which is most of them.
  if (inherits(bp, "breakpoints") && !is.null(bp$breakpoints) &&
      length(bp$breakpoints) > 0 && !all(is.na(bp$breakpoints))) {
    cp <- as.integer(bp$breakpoints)
    ci <- tryCatch(stats::confint(bp)$confint, error = function(e) NULL)
    if (!is.null(ci) && nrow(ci) == length(cp)) {
      ci_lower <- pmax(1L, as.integer(ci[, 1]))
      ci_upper <- pmin(n - 1L, as.integer(ci[, 3]))
    }
  }

  trend <- as.numeric(last$Tt)
  if (length(trend) != n) trend <- NULL

  ggcpt_build(
    data_vec, cp,
    method = "bfast",
    change_in = change_in,
    penalty = list(type = "BIC", value = NA_real_),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (!is.null(ci_lower)) {
      list(ci_lower = ci_lower, ci_upper = ci_upper)
    },
    fitted = trend
  )
}

#' WBS for nonstationary time series
#'
#' Wraps \code{wbsts::wbs.lsw()} (Korkas and Fryzlewicz): wild binary
#' segmentation applied to the locally stationary wavelet spectrum, so it
#' detects changes in the \emph{second-order} structure — variance and
#' autocovariance — of a nonstationary series. Where \code{wbs} looks for
#' jumps in the level, this looks for jumps in how the series behaves.
#'
#' @param x A numeric vector.
#' @param n_intervals Number of random intervals (\code{M}). Defaults to
#'   \code{0}, which is the engine's "all dyadic intervals" setting.
#' @param cstar,lambda Post-processing constants; the engine's defaults are
#'   \code{0.75} for both.
#' @param scales Wavelet scales to use. \code{NULL} lets the engine choose.
#' @param seed Optional seed.
#' @param ... Additional arguments passed to \code{wbsts::wbs.lsw()}.
#' @return A \code{ggcpt} object with \code{change_in = "var"}.
#' @references
#' \insertRef{korkas2017wbsts}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("wbsts", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' y <- c(as.numeric(stats::arima.sim(list(ar = 0.1), 250)),
#'        as.numeric(stats::arima.sim(list(ar = 0.9), 250)))
#' wbsts_wrapper(y)
#' }
#' @family changepoint engines
wbsts_wrapper <- function(x, n_intervals = 0, cstar = 0.75, lambda = 0.75,
                          scales = NULL, seed = NULL, ...) {
  need_pkg("wbsts")
  validate_data(x)
  data_vec <- as_uni_vector(x, "wbsts")
  validate_scalar(n_intervals, "n_intervals", min = 0)
  if (!is.null(seed)) set.seed(seed)

  fit <- suppressWarnings(
    wbsts::wbs.lsw(data_vec, M = n_intervals, cstar = cstar,
                   lambda = lambda, scales = scales, ...)
  )
  # $cp.aft holds the post-processed final estimates; $cp.bef the raw ones.
  cp <- as.integer(fit$cp.aft %||% integer(0))
  cp <- sort(cp[!is.na(cp)])

  ggcpt_build(
    data_vec, cp,
    method = "wbsts",
    change_in = "var",
    penalty = list(type = "threshold", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}

#' Fast binary segmentation across loss functions
#'
#' Wraps \code{binsegRcpp::binseg()} (Hocking): a C++ binary segmentation
#' that runs in \eqn{O(n \log n)} for the best case and supports several
#' loss functions, including ones no other engine here offers (Poisson,
#' \eqn{\ell_1}). It is the package's \emph{performance} path for binary
#' segmentation on long series, and it returns the whole nested family of
#' segmentations, so it also feeds \code{\link{cpt_solution_path}()}.
#'
#' @param x A numeric vector.
#' @param change_in \code{"mean"} (Gaussian, the default) or
#'   \code{"meanvar"}, mapped to the engine's \code{mean_norm} and
#'   \code{meanvar_norm} distributions. \pkg{binsegRcpp} has no
#'   variance-only cost, so \code{"var"} is not offered here — use
#'   \code{"meanvar"}, or \code{\link{cpt_detect}(method = "pelt",
#'   change_in = "var")} for a variance-only change.
#' @param distribution Loss function, overriding the mapping from
#'   \code{change_in}. Run \code{binsegRcpp::get_distribution_info()} for the
#'   list (\code{"mean_norm"}, \code{"meanvar_norm"}, \code{"poisson"},
#'   \code{"l1"}, ...).
#' @param max_segments Largest number of segments searched. Defaults to
#'   \code{min(20, floor(n / 5))}.
#' @param n_segments Number of segments to report. When \code{NULL} (the
#'   default) it is chosen by BIC over the nested family the engine returns.
#' @param min_segment_length Minimum segment length. Passed through when
#'   supplied.
#' @return A \code{ggcpt} object.
#' @references
#' \insertRef{hocking2024binsegrcpp}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("binsegRcpp", quietly = TRUE)
#' set.seed(2026)
#' binsegrcpp_wrapper(c(rnorm(100), rnorm(100, 3)))
#' @family changepoint engines
binsegrcpp_wrapper <- function(x, change_in = c("mean", "meanvar"),
                               distribution = NULL, max_segments = NULL,
                               n_segments = NULL,
                               min_segment_length = NULL) {
  need_pkg("binsegRcpp")
  change_in <- match.arg(change_in)
  validate_data(x)
  data_vec <- as_uni_vector(x, "binsegrcpp")
  n <- length(data_vec)

  if (is.null(distribution)) {
    # binsegRcpp 2025.5.13 offers mean_norm, meanvar_norm, poisson, laplace
    # and l1 -- there is no variance-only cost, so `change_in = "var"` is not
    # offered rather than mapped to a name the engine would reject several
    # frames down.
    distribution <- switch(change_in, mean = "mean_norm",
                           meanvar = "meanvar_norm")
  }
  if (is.null(max_segments)) {
    max_segments <- max(2L, min(20L, floor(n / 5)))
  }
  validate_scalar(max_segments, "max_segments", min = 2)
  max_segments <- min(as.integer(max_segments), n)

  args <- list(distribution.str = distribution, data.vec = data_vec,
               max.segments = max_segments)
  if (!is.null(min_segment_length)) {
    args$min.segment.length <- as.integer(min_segment_length)
  }
  fit <- do.call(binsegRcpp::binseg, args)
  splits <- as.data.frame(fit$splits)

  if (is.null(n_segments)) {
    # The engine returns the whole nested family; pick the BIC-optimal rung
    # rather than the largest, which would always be max_segments.
    loss <- as.numeric(splits$loss)
    k <- as.integer(splits$segments)
    bic <- n * log(pmax(loss, .Machine$double.eps) / n) + (2 * (k - 1) + 1) *
      log(n)
    n_segments <- k[which.min(bic)]
  }
  validate_scalar(n_segments, "n_segments", min = 1)
  n_segments <- min(as.integer(n_segments), max(splits$segments))

  # `end` is the last index of each segment; the first `n_segments` rows of
  # the (nested) family give that segmentation, and the final n is not a
  # changepoint.
  ends <- as.integer(splits$end[seq_len(n_segments)])
  cp <- sort(unique(ends[ends < n]))

  ggcpt_build(
    data_vec, cp,
    method = "binsegrcpp",
    change_in = change_in,
    penalty = list(type = "BIC over nested family",
                   value = as.numeric(n_segments)),
    fit = fit,
    call = match.call()
  )
}
