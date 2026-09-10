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
#'   on average, before a false alarm). Defaults to \code{500}. \pkg{cpm}
#'   ships thresholds only for a fixed grid -- 100, 200, 300, 370, 400, 500,
#'   600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,
#'   9000, 10000, 20000, 30000, 40000 and 50000 -- and any other value is
#'   refused, because the engine answers it by printing an error and
#'   reporting no changepoints. The grid is the same for every
#'   \code{cpm_type}, and 50000 is the ceiling: a long series cannot be
#'   given an \code{arl0} proportional to its length indefinitely.
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
#' @family changepoint engines
cpm_wrapper <- function(x, cpm_type = "Mann-Whitney", arl0 = 500,
                        startup = 20, ...) {
  need_pkg("cpm")
  reject_renamed_args(list(...), "cpm")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  # No `min` here on purpose: the printed-error guard further down names
  # the average run lengths cpm actually ships thresholds for, which is
  # far more useful than a range complaint. This catches only what that
  # guard cannot see -- NA, a string, a length-2 vector.
  validate_scalar(arl0, "arl0")
  validate_scalar(startup, "startup", min = 1)

  cpm_type <- cpm_check_type(cpm_type, list(...))

  validate_data(x)
  data_vec <- as_uni_vector(x, "cpm")

  # cpm ships thresholds for a fixed set of average run lengths. For any
  # other value processStream() *prints* "Error: No thresholds available for
  # selected ARL0" and returns an empty result rather than raising a
  # condition, so tryCatch() never sees it and the wrapper would report "no
  # changepoints" as though the analysis had run. This is the same trap the
  # 0.4.0 audit found for cpm_type = "GLRAdjusted", on a different argument.
  # Matching the printed text keeps this working if the supported set
  # changes upstream.
  cpm_out <- utils::capture.output(
    fit <- cpm::processStream(data_vec, cpmType = cpm_type, ARL0 = arl0,
                              startup = startup, ...)
  )
  cpm_check_printed_error(cpm_out, arl0, list(...))
  # anything else the engine printed is still the user's to see
  if (length(cpm_out)) cat(cpm_out, sep = "\n")

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

# Internal: the cpm argument guards, in one place because there are two
# doors into the engine. cpm reports a bad `cpmType`, a missing FET `lambda`
# or an unsupported `ARL0` by *printing* an error and handing back something
# unusable rather than raising a condition, so each caller has to look for
# it -- and `cpt_monitor(method = "cpm")` built its model straight from
# cpm::makeChangePointModel() and so inherited none of these. Measured
# against cpm 2.3: a withheld type surfaced there as `no applicable method
# for '@' applied to an object of class "NULL"`, a missing lambda as base R's
# `only 0's may be mixed with negative subscripts`, and an off-grid arl0 as
# the same `@`-on-NULL message. None of the three named the argument.
#
# "GLRAdjusted" is documented by cpm but rejected by its own dispatch (it
# prints "not a valid ChangePointModel type" and returns no changepoints
# instead of erroring), so it is not offered here: a silent empty result is
# worse than a refusal. Note that "ExponentialAdjusted" -- named alongside it
# in an earlier version of this comment -- is NOT rejected: it runs and
# returns changepoints, so the pair is not interchangeable and only the one
# type is withheld.
#' @noRd
cpm_types <- function() {
  c("Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov",
    "Cramer-von-Mises", "Student", "Bartlett", "GLR", "Exponential", "FET")
}

# Internal: validate `cpm_type` and the `lambda` that one of its values
# requires. FET is the one type that needs `lambda`, and without it cpm dies
# with base R's "only 0's may be mixed with negative subscripts" -- a message
# about neither the argument nor the method. cpm ships FET thresholds for
# lambda = 0.1 and 0.3 only (measured across 0.01-1.0 against cpm 2.3); every
# other value takes the printed-error path in cpm_check_printed_error().
#' @noRd
cpm_check_type <- function(cpm_type, dots) {
  cpm_type <- match.arg(cpm_type, cpm_types())
  if (identical(cpm_type, "FET") && !"lambda" %in% names(dots)) {
    stop("`cpm_type = \"FET\"` needs a `lambda` value passed through `...`; ",
         "cpm has no default for it and fails with an unrelated subscript ",
         "error when it is missing. Supported values are `lambda = 0.1` and ",
         "`lambda = 0.3`.", call. = FALSE)
  }
  cpm_type
}

# Internal: turn cpm's *printed* threshold complaint into a real error. The
# same printed line covers two different arguments, and it names which:
# "selected ARL0" or "selected lambda". Blaming arl0 for a lambda cpm has no
# thresholds for sent the reader after an argument that was already correct,
# so the branch follows the printed text.
#' @noRd
cpm_check_printed_error <- function(cpm_out, arl0, dots) {
  if (!any(grepl("No thresholds available", cpm_out, fixed = TRUE))) {
    return(invisible(NULL))
  }
  if (any(grepl("selected lambda", cpm_out, fixed = TRUE))) {
    lam <- dots[["lambda"]]
    stop("`lambda = ", if (is.null(lam)) "<unset>" else lam, "` is not a ",
         "value cpm ships FET thresholds for; it returns no changepoints ",
         "rather than failing, which is indistinguishable from a genuine ",
         "\"no changes\" result. Supported values are 0.1 and 0.3.",
         call. = FALSE)
  }
  stop("`arl0 = ", arl0, "` is not an average run length that cpm ships ",
       "thresholds for; it returns no changepoints rather than failing, ",
       "which is indistinguishable from a genuine \"no changes\" result. ",
       "Supported values are 100, 200, 300, 370, 400, 500, 600, 700, ",
       "800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, ",
       "10000, 20000, 30000, 40000 and 50000.", call. = FALSE)
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
#' @param nperm Number of permutations for the significance test, at least 2.
#'   Defaults to \code{1000}. Fewer than two leaves the engine with no
#'   permutation distribution: it reports no changepoints at all for 0, and
#'   fails inside its own code for 1.
#' @param kmax Maximum number of changepoints considered. Defaults to
#'   \code{10}.
#' @param alpha Significance level of the permutation test. Defaults to
#'   \code{0.05}.
#' @param seed Optional seed for reproducibility of the permutation test.
#'   The seed is scoped to this call: \code{.Random.seed} is saved and
#'   restored, so a seeded call inside a simulation loop does not pin the
#'   loop's own stream.
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
#' res <- kcp_wrapper(c(rnorm(60), rnorm(60, 3)), nperm = 100, seed = 2026)
#' res$changepoints
#' @family changepoint engines
kcp_wrapper <- function(x, running_stat = c("mean", "var", "autocorr", "corr"),
                        wsize = 25, nperm = 1000, kmax = 10, alpha = 0.05,
                        seed = NULL, ...) {
  need_pkg("kcpRS")
  reject_renamed_args(list(...), "kcp")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(wsize, "wsize", min = 2)
  validate_scalar(kmax, "kmax", min = 1)
  running_stat <- match.arg(running_stat)
  validate_scalar(alpha, "alpha", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  # A permutation test needs a permutation distribution. With nperm = 0 or a
  # negative value kcpRS quietly reports no changepoints -- the same
  # looks-legitimate failure as a test that never ran -- and nperm = 1 dies
  # inside the engine with "'row.names' is not a character or integer vector
  # of length 11". Two is the smallest value that actually works.
  validate_scalar(nperm, "nperm", min = 2)

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

  local_seed(seed)

  # kcpRS::kcpRS() opens a PSOCK cluster unconditionally --
  # `kcpRS.default()` runs `makeCluster(ncpu)` whenever
  # `ncpu <= detectCores()`, so no value of `ncpu`, not even 1, avoids it --
  # and `base::serverSocket()` fails outright when the port it picked is
  # taken. Two `R CMD build` runs on the same machine collided on port
  # 11246 and one of them died mid-vignette with "creation of server socket
  # failed", which is the shape of failure CRAN's parallel package checks
  # produce. The port is chosen inside makeCluster(), so a retry gets a
  # different one; three attempts, and only for that error, so a genuine
  # engine failure is still raised on the first try.
  attempt <- 1L
  repeat {
    fit <- tryCatch(
      kcpRS::kcpRS(data = as.data.frame(X_fit), RS_fun = rs_fun,
                   RS_name = running_stat, wsize = wsize, nperm = nperm,
                   Kmax = kmax, alpha = alpha, ...),
      error = function(e) {
        if (attempt < 3L &&
            grepl("server socket|cannot be opened|port",
                  conditionMessage(e))) {
          return(NULL)
        }
        stop(e)
      })
    if (!is.null(fit)) break
    attempt <- attempt + 1L
  }

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
#'   observations, capped at \code{n / 2} — the largest bandwidth the engine
#'   accepts — so the default also works on series shorter than 40.
#' @param lag Time lag at which changes in the joint distribution are
#'   examined; \code{0} targets the marginal distribution. Defaults to
#'   \code{0}.
#' @param ... Additional arguments passed to \code{CptNonPar::np.mojo()}.
#' @return A \code{ggcpt} object. Constant coordinates leave the kernel
#'   statistics undefined, so they are dropped (with a warning) before
#'   detection and an all-constant input returns an empty result. The
#'   engine calibrates its detection threshold by bootstrap, so the value
#'   recorded in the penalty descriptor varies between runs; call
#'   \code{set.seed()} beforehand, or pass \code{threshold = "manual"} and
#'   \code{threshold.val} through \code{...}, for a reproducible one.
#' @references
#' \insertRef{mcgonigle2023npmojo}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("CptNonPar", quietly = TRUE)
#' res <- npmojo_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
#' @family changepoint engines
npmojo_wrapper <- function(x, G = NULL, lag = 0, ...) {
  need_pkg("CptNonPar")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(lag, "lag", min = 0)

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  X <- if (is_mv) as_mv_matrix(x) else as.numeric(x)
  n <- if (is_mv) nrow(X) else length(X)
  data_vec <- if (is_mv) as.numeric(X[, 1]) else X

  if (is.null(G)) {
    # The engine rejects any bandwidth above n / 2 ("Bandwidth is too large
    # for the length of time series"), so an uncapped default of 20 makes
    # every series shorter than 40 fail outright. The cap only ever binds
    # below n = 40; longer series keep max(20, 0.1 * n) unchanged.
    G <- max(2L, min(as.integer(max(20L, floor(0.1 * n))),
                     as.integer(floor(n / 2))))
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

  cp_indices <- as.integer(fit[["cpts", exact = TRUE]])

  # Exact [[ ]], not `$`: np.mojo has both `threshold` (the RULE, e.g.
  # "bootstrap") and `threshold.val` (the number), and `$threshold.val`
  # partial-matches nothing today only because the longer name exists --
  # rename or drop it upstream and `$` would silently resolve to the
  # character rule and report a cutoff of NA. cpt_scale_space() already
  # reads it exactly for this reason; this door did not.
  ggcpt_build(
    data_vec, cp_indices,
    method = "npmojo",
    change_in = "distribution",
    penalty = list(type = "threshold",
                   value = fit[["threshold.val", exact = TRUE]] %||%
                     NA_real_),
    fit = fit,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(X)
  )
}
