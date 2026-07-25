#' Sequential change point model wrapper (CPM)
#'
#' Wraps \code{cpm::processStream()} (Ross, 2015) for distribution-free
#' sequential changepoint detection via repeated two-sample tests
#' (Mann-Whitney for location, Mood for scale, Lepage, Kolmogorov-Smirnov and
#' Cramer-von-Mises for general changes, and parametric Student/Bartlett/GLR
#' variants). Although the engine is designed for streams, it is run here
#' over the full series in one pass, mimicking online monitoring with average
#' run length \code{arl0}.
#'
#' @param x A numeric vector.
#' @param cpm_type Test statistic, passed to \code{cpm::processStream()} as
#'   \code{cpmType}. Distribution-free: \code{"Mann-Whitney"} (location, the
#'   default), \code{"Mood"} (scale), \code{"Lepage"},
#'   \code{"Kolmogorov-Smirnov"}, \code{"Cramer-von-Mises"}. Parametric:
#'   \code{"Student"}, \code{"Bartlett"}, \code{"GLR"} (Gaussian),
#'   \code{"Exponential"} (positive data), \code{"FET"} (Fisher's exact test,
#'   for 0/1 Bernoulli data — this one also needs a \code{lambda} value passed
#'   through \code{...}, e.g. \code{lambda = 0.3}).
#' @param arl0 Target in-control average run length (how many observations,
#'   on average, before a false alarm). Defaults to \code{500}.
#' @param startup Number of observations after each restart before monitoring
#'   begins. Defaults to \code{20}.
#' @param ... Additional arguments passed to \code{cpm::processStream()}.
#' @return A \code{ggcpt} object. The \code{changepoints} tibble carries a
#'   \code{detection_time} column: the index at which the sequential test
#'   flagged each change (always later than the estimated location).
#' @references
#' \insertRef{ross2015cpm}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("cpm", quietly = TRUE)
#' res <- cpm_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
cpm_wrapper <- function(x, cpm_type = "Mann-Whitney", arl0 = 500,
                        startup = 20, ...) {
  need_pkg("cpm")

  # "GLRAdjusted"/"ExponentialAdjusted" are documented by cpm but rejected by
  # its own processStream() dispatch (it prints "not a valid
  # ChangePointModel type" and returns no changepoints instead of erroring),
  # so they are not offered here: a silent empty result is worse than a
  # refusal.
  cpm_type <- match.arg(cpm_type, c(
    "Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov",
    "Cramer-von-Mises", "Student", "Bartlett", "GLR", "Exponential", "FET"
  ))

  validate_data(x)
  data_vec <- as_uni_vector(x, "cpm")

  fit <- cpm::processStream(data_vec, cpmType = cpm_type, ARL0 = arl0,
                            startup = startup, ...)

  cp_indices <- as.integer(fit$changePoints)

  ggcpt_build(
    data_vec, cp_indices,
    method = "cpm",
    change_in = "distribution",
    penalty = list(type = "ARL0", value = arl0),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) {
      list(detection_time = as.integer(fit$detectionTimes))
    }
  )
}

#' Kernel changepoint wrapper (KCP on running statistics)
#'
#' Wraps \code{kcpRS::kcpRS()} (Cabrieto et al., 2018; the KCP framework of
#' Arlot, Celisse and Harchaoui, 2019). The data are mapped to a running
#' statistic (mean, variance, autocorrelation, or correlation) computed on a
#' sliding window, and a Gaussian-kernel change point analysis with a
#' permutation significance test is run on the statistic. Detecting changes
#' in running correlations or variances captures higher-order changes that
#' mean-based methods miss. Multivariate input (matrix or data frame) is
#' supported.
#'
#' @param x A numeric vector, matrix, or data frame (columns are variables).
#' @param running_stat Which running statistic to monitor: \code{"mean"},
#'   \code{"var"}, \code{"autocorr"}, or \code{"corr"} (correlation requires
#'   at least two columns). Defaults to \code{"mean"}.
#' @param wsize Sliding window size for the running statistic. Defaults to
#'   \code{25}.
#' @param nperm Number of permutations for the significance test. Defaults to
#'   \code{1000}.
#' @param kmax Maximum number of changepoints considered. Defaults to
#'   \code{10}.
#' @param alpha Significance level of the permutation test. Defaults to
#'   \code{0.05}.
#' @param seed Optional seed for reproducibility of the permutation test.
#' @param ... Additional arguments passed to \code{kcpRS::kcpRS()}.
#' @return A \code{ggcpt} object. Reported locations refer to the centre of
#'   the sliding window in which the change occurs. The series must be at
#'   least \code{wsize} long to form one window. Constant coordinates make
#'   every running statistic \code{NA}, so they are dropped (with a warning)
#'   before detection and an all-constant input returns an empty result.
#' @references
#' \insertRef{arlot2019kernel}{ggchangepoint}
#'
#' \insertRef{cabrieto2018kcprs}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("kcpRS", quietly = TRUE)
#' res <- kcp_wrapper(c(rnorm(100), rnorm(100, 3)), nperm = 200, seed = 2026)
#' res$changepoints
kcp_wrapper <- function(x, running_stat = c("mean", "var", "autocorr", "corr"),
                        wsize = 25, nperm = 1000, kmax = 10, alpha = 0.05,
                        seed = NULL, ...) {
  need_pkg("kcpRS")
  running_stat <- match.arg(running_stat)

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  X <- if (is_mv) as_mv_matrix(x) else matrix(as.numeric(x), ncol = 1)
  if (running_stat == "corr" && ncol(X) < 2) {
    stop("`running_stat = \"corr\"` requires at least two columns.",
         call. = FALSE)
  }
  data_vec <- as.numeric(X[, 1])

  # The running statistic needs at least one full window; without one the
  # engine fails with "wrong sign in 'by' argument".
  if (nrow(X) < wsize) {
    stop("`kcp` needs at least `wsize` observations to form a running ",
         "window, but `x` has ", nrow(X), " and `wsize` is ", wsize,
         ". Lower `wsize` or use a longer series.", call. = FALSE)
  }

  # A flat coordinate makes the running statistic NA on every window, which
  # the engine rejects outright ("NA values are found in the running
  # statistics") even when the other coordinates carry a real change.
  X_fit <- drop_constant_cols(X, "kcp")
  if (is.null(X_fit)) {
    return(ggcpt_build(data_vec, integer(0), method = "kcp",
                       change_in = paste0("running ", running_stat),
                       penalty = list(type = "permutation", value = alpha),
                       call = match.call(),
                       data_wide = if (is_mv) mv_data_wide(X)))
  }
  if (running_stat == "corr" && ncol(X_fit) < 2) {
    stop("`running_stat = \"corr\"` needs at least two varying columns.",
         call. = FALSE)
  }

  rs_fun <- switch(running_stat,
    mean     = kcpRS::runMean,
    var      = kcpRS::runVar,
    autocorr = kcpRS::runAR,
    corr     = kcpRS::runCorr
  )

  if (!is.null(seed)) set.seed(seed)

  fit <- kcpRS::kcpRS(data = as.data.frame(X_fit), RS_fun = rs_fun,
                      RS_name = running_stat, wsize = wsize, nperm = nperm,
                      Kmax = kmax, alpha = alpha, ...)

  # kcpRS reports the first index of the new phase (right convention);
  # normalise to the package's left convention.
  cp_indices <- as.integer(fit$changePoints) - 1L

  ggcpt_build(
    data_vec, cp_indices,
    method = "kcp",
    change_in = paste0("running ", running_stat),
    penalty = list(type = "permutation", value = alpha),
    fit = fit,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(X)
  )
}

#' Nonparametric MOSUM wrapper (NP-MOJO)
#'
#' Wraps \code{CptNonPar::np.mojo()} (McGonigle and Cho, 2025): nonparametric
#' moving-sum detection of changes in the marginal or joint distribution of a
#' (possibly multivariate) time series, robust to serial dependence.
#'
#' @param x A numeric vector or matrix (rows are time points).
#' @param G Moving-window bandwidth. Defaults to \code{max(20, 0.1 * n)}
#'   observations.
#' @param lag Time lag at which changes in the joint distribution are
#'   examined; \code{0} targets the marginal distribution. Defaults to
#'   \code{0}.
#' @param ... Additional arguments passed to \code{CptNonPar::np.mojo()}.
#' @return A \code{ggcpt} object. Constant coordinates leave the kernel
#'   statistics undefined, so they are dropped (with a warning) before
#'   detection and an all-constant input returns an empty result.
#' @references
#' \insertRef{mcgonigle2023npmojo}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("CptNonPar", quietly = TRUE)
#' res <- npmojo_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
npmojo_wrapper <- function(x, G = NULL, lag = 0, ...) {
  need_pkg("CptNonPar")

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  X <- if (is_mv) as_mv_matrix(x) else as.numeric(x)
  n <- if (is_mv) nrow(X) else length(X)
  data_vec <- if (is_mv) as.numeric(X[, 1]) else X

  if (is.null(G)) {
    G <- max(20L, floor(0.1 * n))
  }

  # A flat coordinate leaves the kernel statistics undefined and the engine
  # fails with "missing value where TRUE/FALSE needed"; a wholly flat series
  # simply has no changepoint.
  X_fit <- if (is_mv) drop_constant_cols(X, "npmojo") else {
    if (is_constant(X)) NULL else X
  }
  if (is.null(X_fit)) {
    return(ggcpt_build(data_vec, integer(0), method = "npmojo",
                       change_in = "distribution",
                       penalty = list(type = "threshold", value = NA_real_),
                       call = match.call(),
                       data_wide = if (is_mv) mv_data_wide(X)))
  }

  fit <- CptNonPar::np.mojo(X_fit, G = G, lag = lag, ...)

  cp_indices <- as.integer(fit$cpts)

  ggcpt_build(
    data_vec, cp_indices,
    method = "npmojo",
    change_in = "distribution",
    penalty = list(type = "threshold", value = fit$threshold.val %||% NA_real_),
    fit = fit,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(X)
  )
}
