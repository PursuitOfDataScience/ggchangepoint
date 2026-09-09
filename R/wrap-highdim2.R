# ---------------------------------------------------------------------------
# Engine wave #2, part 1: high-dimensional methods.
#
#   esac, pilliat  (HDCD)         sparsity-adaptive mean changes
#   hdcov          (changepoints) covariance changes
#   network        (changepoints) dynamic-network changes
#   var            (changepoints) VAR(1) transition-matrix changes
#
# All four take a matrix with rows = time and columns = coordinates, which is
# this package's convention; the engines themselves transpose internally
# (they work with p x n matrices), and that transposition happens here rather
# than in the user's head.
# ---------------------------------------------------------------------------

# Internal: changepoints::thresholdBS() with its single-level-tree failure
# routed around.
#
# The upstream pruning loop is
#   for (i in 2:level_length) { ... 1:table(BS_object$Level)[i] ... }
# so a tree with one level makes that `2:1`, the body runs with i = 2,
# `table(...)[2]` is NA, and `1:NA` stops with base R's "NA/NaN argument" --
# naming neither the shape of the input nor the cause. Binary segmentation
# stops at one level whenever the series is short relative to the dimension,
# and because the threshold comes from a permutation draw, whether a given
# call lands there is random. Measured over repeated seeds: hdcov failed on
# 23 of 25 runs at n = 120, p = 8 and 24 of 25 at n = 400, p = 20; network
# on 10 of 12 at n = 20 with 4-node graphs.
#
# Refusing would be wrong -- the answer is not in doubt. One level means one
# candidate split with no ancestors to prune against, so it is a changepoint
# exactly when its own statistic clears the threshold. On a multi-level tree
# that same rule reproduces thresholdBS()'s own output (checked: locations
# 156 and 218 either way), which is what makes it safe to apply here.
#
# Shared by hdcov_wrapper() and network_wrapper(), the two wrappers that
# call thresholdBS().
#' @noRd
threshold_bs <- function(bs, threshold) {
  if (length(unique(bs$Level)) < 2L) {
    keep <- which(as.numeric(bs$Dval) > threshold)
    return(list(cpt_hat = if (length(keep) > 0) {
      cbind(as.integer(bs$S)[keep], as.numeric(bs$Dval)[keep])
    } else {
      NULL
    }))
  }
  changepoints::thresholdBS(bs, threshold)
}

#' ESAC wrapper — sparsity-adaptive high-dimensional detection
#'
#' Wraps \code{HDCD::ESAC()} (Moen, Glad and Tveten, 2023): Efficient
#' Sparsity Adaptive Changepoint estimation for a change in the mean vector
#' of a high-dimensional series. Where \code{inspect} projects onto a single
#' estimated sparse direction, ESAC adapts across the whole sparsity range at
#' once, which is a different regime rather than a refinement of the same one
#' — it is competitive both when a handful of coordinates change and when all
#' of them do.
#'
#' @param x A numeric matrix or data frame with rows as time points and
#'   columns as coordinates.
#' @param threshold_d,threshold_s Leading constants of the dense and sparse
#'   thresholds. Defaults follow the engine (\code{1.5} and \code{1}).
#' @param empirical Calibrate the thresholds by Monte Carlo rather than using
#'   the theoretical values? Slower but sharper; defaults to \code{FALSE}.
#' @param N Monte Carlo samples when \code{empirical = TRUE}.
#' @param seed Optional seed (used by the empirical calibration). The seed
#'   is scoped to this call: \code{.Random.seed} is saved and restored, so a
#'   seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Additional arguments passed to \code{HDCD::ESAC()}.
#' @return A \code{ggcpt} object. The changepoints tibble carries
#'   \code{cusum} (the ESAC statistic at each detected location) and
#'   \code{depth} (its level in the recursion).
#' @references
#' \insertRef{moen2023esac}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("HDCD", quietly = TRUE)
#' set.seed(2026)
#' X <- matrix(rnorm(100 * 20), nrow = 100)
#' X[51:100, 1:5] <- X[51:100, 1:5] + 3
#' esac_wrapper(X)
#' @family changepoint engines
esac_wrapper <- function(x, threshold_d = 1.5, threshold_s = 1,
                         empirical = FALSE, N = 1000, seed = NULL, ...) {
  need_pkg("HDCD")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(threshold_d, "threshold_d", min = 0, min_open = TRUE)
  validate_scalar(threshold_s, "threshold_s", min = 0, min_open = TRUE)
  validate_scalar(N, "N", min = 1)
  validate_flag(empirical, "empirical")
  validate_data(x)
  X <- as_mv_matrix(x)
  X <- drop_constant_cols(X, "esac")
  if (is.null(X)) {
    # `data_wide` too: esac is `univariate = FALSE` in the registry, so its
    # result is always meant to be multivariate. Omitting it made
    # n_coordinates() report 1, which cascaded -- autoplot() took the
    # UNIVARIATE branch, augment() took `use_wide = FALSE`, and
    # ggcpt_compare(X, methods = c("inspect", "esac")) returned two
    # structurally different objects for one input.
    XX <- as_mv_matrix(x)
    return(ggcpt_build(as.numeric(XX[, 1]), integer(0),
                       method = "esac", change_in = "mean",
                       penalty = list(type = "threshold", value = NA_real_),
                       call = match.call(),
                       data_wide = mv_data_wide(XX)))
  }
  data_vec <- as.numeric(X[, 1])
  local_seed(seed)

  # HDCD works with p x n (coordinates down, time across).
  fit <- HDCD::ESAC(t(X), threshold_d = threshold_d,
                    threshold_s = threshold_s, empirical = empirical,
                    N = N, ...)
  # Filter and order in lockstep across all three engine vectors. `ord` used
  # to be computed from the NA-filtered `cp` and then applied to the
  # UNFILTERED CUSUMval and depth, so one NA changepoint drew those two
  # columns from the wrong rows -- silently, because the lengths still
  # agreed and ggcpt_build()'s assignment succeeded. This is the rule
  # ggcpt_build() states for itself: build the row set before filtering, so
  # the optional extra columns stay row-aligned.
  cp_raw <- as.integer(fit$changepoints)
  cusum_raw <- as.numeric(fit$CUSUMval)
  depth_raw <- as.integer(fit$depth)
  keep <- !is.na(cp_raw)
  ord <- which(keep)[order(cp_raw[keep])]
  cp <- cp_raw[ord]

  ggcpt_build(
    data_vec, cp,
    method = "esac",
    change_in = "mean",
    penalty = list(type = "threshold", value = threshold_d),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp) > 0) {
      list(cusum = if (length(cusum_raw) == length(cp_raw)) cusum_raw[ord]
                   else rep(NA_real_, length(cp)),
           depth = if (length(depth_raw) == length(cp_raw)) depth_raw[ord]
                   else rep(NA_integer_, length(cp)))
    },
    data_wide = mv_data_wide(X)
  )
}

# Internal: is `p` an exact power of two?
#' @noRd
is_power_of_two <- function(p) {
  p <- as.integer(p)
  !is.na(p) && p >= 2L && bitwAnd(p, p - 1L) == 0L
}

# Internal: HDCD 1.1's Pilliat() builds its partial-sum threshold vector with
#     t <- 1; repeat { push(t * log(2 * e * p / t) + log(2 * n^2))
#                      t <- 2 * t; if (t >= p) break }
# which produces floor(log2(p - 1)) + 1 entries -- one short of the
# floor(log2(p)) + 1 sparsity scales the C routine then indexes. When `p` is
# an exact power of two the C code reads past the end of that vector, every
# candidate clears the garbage value, and the engine returns a changepoint at
# *every* observation. Measured at p = 2, 4, 8, 16, 32, 64 and 128 with HDCD
# 1.1, on both signal and pure-noise series; every other p is fine, and
# HDCD::ESAC() is unaffected at all of them.
#
# A corrected vector cannot be injected: the analytic branch overwrites
# whatever is passed in, and the empirical branch leaves the Berk-Jones scale
# count at zero unless it runs its own calibration, which silently disables
# one of the three tests. So the wrapper refuses instead, and says what to do.
#' @noRd
pilliat_dimension_guard <- function(p, p_supplied) {
  if (!is_power_of_two(p)) return(invisible(TRUE))
  if (!isTRUE(utils::packageVersion("HDCD") <= "1.1")) {
    return(invisible(TRUE))
  }
  stop("`pilliat` cannot be trusted on exactly ", p, " coordinates",
       if (!identical(p, p_supplied)) {
         paste0(" (", p_supplied, " supplied, ", p_supplied - p,
                " constant and dropped)")
       } else "",
       ": HDCD ", as.character(utils::packageVersion("HDCD")),
       "'s Pilliat() computes one fewer partial-sum threshold than it uses ",
       "whenever the number of coordinates is a power of two, and reports a ",
       "changepoint at every observation as a result. That is an engine bug, ",
       "not a property of the data. Use method = \"esac\", which is ",
       "unaffected at every dimension, or change the number of coordinates ",
       "so it is not a power of two.", call. = FALSE)
}

#' Pilliat wrapper — high-dimensional detection by three complementary tests
#'
#' Wraps \code{HDCD::Pilliat()} (Pilliat, Carpentier and Verzelen, 2023): a
#' high-dimensional mean-change procedure combining a dense test, a
#' Berk–Jones test and a partial-sum test, so it is powerful across sparsity
#' regimes without estimating the sparsity level. A useful cross-check on
#' \code{\link{esac_wrapper}()} — the two adapt differently and disagreeing
#' answers are informative.
#'
#' @inheritParams esac_wrapper
#' @param threshold_d_const,threshold_bj_const,threshold_partial_const
#'   Leading constants of the dense, Berk–Jones and partial-sum thresholds.
#' @param ... Additional arguments passed to \code{HDCD::Pilliat()}.
#' @return A \code{ggcpt} object.
#' @section Dimension precondition:
#' \code{HDCD} 1.1's \code{Pilliat()} builds one fewer partial-sum threshold
#' than it uses whenever the number of coordinates is an exact power of two,
#' so the C routine reads past the end of that vector and the engine reports
#' a changepoint at \emph{every} observation — on pure noise as readily as on
#' a real change. This wrapper refuses those dimensions rather than returning
#' the result, because it is wrong in a way that looks like a finding.
#' \code{\link{esac_wrapper}()} is unaffected at every dimension. Note that
#' constant coordinates are dropped before the count, so 9 coordinates one of
#' which is constant is 8 for this purpose.
#'
#' The same reasoning makes this the one engine that \strong{errors} on a
#' degenerate segmentation (a changepoint at more than 90\% of
#' observations) where every other engine in the package warns and reports
#' what it found. That has a consequence for comparisons:
#' \code{\link{cpt_batch}()} and \code{\link{cpt_benchmark}()} record a
#' failure row for \code{pilliat} where the same output from \code{pelt}
#' gives a result plus a warning, so a benchmark table is not scoring the
#' two on equal terms in that case --- read the \code{error} column
#' alongside the metrics.
#' @references
#' \insertRef{pilliat2023optimal}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("HDCD", quietly = TRUE)
#' set.seed(2026)
#' X <- matrix(rnorm(100 * 20), nrow = 100)
#' X[51:100, 1:5] <- X[51:100, 1:5] + 3
#' pilliat_wrapper(X)
#' @family changepoint engines
pilliat_wrapper <- function(x, threshold_d_const = 4,
                            threshold_bj_const = 6,
                            threshold_partial_const = 4,
                            empirical = FALSE, N = 100, seed = NULL, ...) {
  need_pkg("HDCD")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(threshold_d_const, "threshold_d_const", min = 0, min_open = TRUE)
  # `threshold_bj_const` is structurally identical to the other two and was
  # the one omitted, while the comment above claims the set is complete.
  validate_scalar(threshold_bj_const, "threshold_bj_const", min = 0, min_open = TRUE)
  validate_scalar(threshold_partial_const, "threshold_partial_const", min = 0, min_open = TRUE)
  validate_scalar(N, "N", min = 1)
  validate_flag(empirical, "empirical")
  validate_data(x)
  X <- as_mv_matrix(x)
  X <- drop_constant_cols(X, "pilliat")
  if (is.null(X)) {
    # See the note in esac_wrapper(): the shape has to survive an all-flat
    # input, because this method is high-dimensional only.
    XX <- as_mv_matrix(x)
    return(ggcpt_build(as.numeric(XX[, 1]), integer(0),
                       method = "pilliat", change_in = "mean",
                       penalty = list(type = "threshold", value = NA_real_),
                       call = match.call(),
                       data_wide = mv_data_wide(XX)))
  }
  data_vec <- as.numeric(X[, 1])
  pilliat_dimension_guard(ncol(X), ncol(as_mv_matrix(x)))
  local_seed(seed)

  fit <- HDCD::Pilliat(t(X), threshold_d_const = threshold_d_const,
                       threshold_bj_const = threshold_bj_const,
                       threshold_partial_const = threshold_partial_const,
                       empirical = empirical, N = N, ...)
  cp <- sort(as.integer(fit$changepoints))
  cp <- cp[!is.na(cp)]
  # Backstop, in case a later HDCD moves the fault rather than fixing it: a
  # changepoint at nearly every observation is not a segmentation.
  #
  # This is the one engine that STOPS on a degenerate segmentation where
  # warn_if_degenerate() gives every other one a warning plus a diagnosis.
  # That is deliberate -- the cited upstream fault produces this output
  # rather than a plausible one -- but it has a consequence worth naming:
  # cpt_batch() and cpt_benchmark() record a failure row here where the
  # same output from `pelt` yields a result plus a warning, so the two are
  # not directly comparable in a benchmark table. @section notes it.
  if (length(cp) > 0.9 * nrow(X)) {
    stop("`pilliat` returned ", length(cp), " changepoints on ", nrow(X),
         " observations, which is a degenerate threshold rather than a ",
         "segmentation. Cross-check with method = \"esac\" and report this ",
         "to HDCD if the dimension is not a power of two.", call. = FALSE)
  }

  ggcpt_build(
    data_vec, cp,
    method = "pilliat",
    change_in = "mean",
    penalty = list(type = "threshold", value = threshold_d_const),
    fit = fit,
    call = match.call(),
    data_wide = mv_data_wide(X)
  )
}

#' High-dimensional covariance changepoints
#'
#' Wraps \code{changepoints::BS.cov()} (Wang, Yu and Rinaldo): binary
#' segmentation on the sample covariance operator, which detects a change in
#' the \emph{dependence structure} of a multivariate series even when every
#' marginal mean and variance is unchanged. No other engine in the package
#' can see that.
#'
#' @param x A numeric matrix or data frame, rows as time points.
#' @param threshold Detection threshold on the CUSUM statistic. When
#'   \code{NULL} (the default) it is calibrated by permutation: the time
#'   order is shuffled \code{n_perm} times, which destroys any changepoint
#'   while preserving the marginal distributions, and the threshold is the
#'   \code{1 - alpha} quantile of the largest statistic seen.
#' @param alpha Family-wise level for the permutation threshold. Defaults to
#'   \code{0.05}.
#' @param n_perm Permutations used to calibrate the threshold. Defaults to
#'   \code{20}; raise it for a sharper threshold at proportional cost.
#' @param delta Minimum spacing between changepoints. Defaults to
#'   \code{max(10, floor(n / 20))}.
#' @param seed Optional seed (the permutation calibration is random). The
#'   seed is scoped to this call: \code{.Random.seed} is saved and restored,
#'   so a seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @return A \code{ggcpt} object with \code{change_in = "covariance"}; the
#'   changepoints tibble carries the CUSUM statistic in \code{cusum}.
#' @references
#' \insertRef{wang2021covariance}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("changepoints", quietly = TRUE)
#' set.seed(2026)
#' p <- 5
#' A <- matrix(rnorm(100 * p), ncol = p)
#' B <- matrix(rnorm(100 * p), ncol = p)
#' B[, 2] <- B[, 1] + 0.2 * B[, 2]        # correlation appears
#' hdcov_wrapper(rbind(A, B), n_perm = 20, alpha = 0.05, seed = 1)
#' @family changepoint engines
hdcov_wrapper <- function(x, threshold = NULL, alpha = 0.05, n_perm = 20,
                          delta = NULL, seed = NULL) {
  need_pkg("changepoints")
  validate_data(x)
  validate_scalar(alpha, "alpha", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_scalar(n_perm, "n_perm", min = 1)
  X <- as_mv_matrix(x)
  # The engine fails on a single column with a message that names nothing
  # ("non-conformable arrays"), while ocd, geomcp, fmean, fcov and
  # fabisearch all name the requirement. Match them.
  if (ncol(X) < 2) {
    stop("Method `hdcov` is high-dimensional and needs at least two ",
         "coordinates, but `x` has ", ncol(X),
         ". See cpt_methods() for univariate methods.", call. = FALSE)
  }
  n <- nrow(X)
  data_vec <- as.numeric(X[, 1])
  if (is.null(delta)) delta <- max(10L, floor(n / 20))
  validate_scalar(delta, "delta", min = 1)
  local_seed(seed)
  if (is.null(threshold) && n_perm < 1 / alpha) {
    warning("The permutation threshold is the ", format(1 - alpha),
            " quantile of ", n_perm, " values, which extrapolates beyond ",
            "the permutation sample. Use at least ", ceiling(1 / alpha),
            " permutations for `alpha = ", format(alpha),
            "`, or pass `threshold` directly.", call. = FALSE)
  }

  bs <- changepoints::BS.cov(t(X), 1, n)
  if (is.null(threshold)) {
    maxima <- vapply(seq_len(as.integer(n_perm)), function(b) {
      perm <- X[sample.int(n), , drop = FALSE]
      pb <- changepoints::BS.cov(t(perm), 1, n)
      d <- as.numeric(pb$Dval)
      if (length(d) == 0) return(0)
      max(d, na.rm = TRUE)
    }, numeric(1))
    threshold <- as.numeric(stats::quantile(maxima, 1 - alpha, names = FALSE))
  }
  validate_scalar(threshold, "threshold", min = 0)

  # thresholdBS() returns cpt_hat = NULL (not a zero-row matrix) when
  # nothing clears the threshold, so it has to be tested before subsetting;
  # `NULL[, 1]` is an error, not an empty vector. threshold_bs() below also
  # routes around an upstream failure on single-level trees.
  th <- threshold_bs(bs, threshold)
  hat <- th$cpt_hat
  cp <- integer(0)
  cusum <- numeric(0)
  if (!is.null(hat) && length(hat) > 0) {
    hat <- as.matrix(hat)
    cp <- as.integer(hat[, 1])
    cusum <- if (ncol(hat) >= 2) as.numeric(hat[, 2]) else {
      rep(NA_real_, length(cp))
    }
  }
  keep <- !is.na(cp)
  cp <- cp[keep]; cusum <- cusum[keep]
  ord <- order(cp)

  # Enforce the minimum spacing the engine does not.
  cp_s <- cp[ord]; cu_s <- cusum[ord]
  if (length(cp_s) > 1) {
    keep2 <- c(TRUE, diff(cp_s) >= delta)
    cp_s <- cp_s[keep2]; cu_s <- cu_s[keep2]
  }

  ggcpt_build(
    data_vec, cp_s,
    method = "hdcov",
    change_in = "covariance",
    penalty = list(type = "threshold", value = threshold),
    fit = bs,
    call = match.call(),
    extra_cp_cols = if (length(cp_s) > 0) list(cusum = cu_s),
    data_wide = mv_data_wide(X)
  )
}

#' Dynamic-network changepoints
#'
#' Wraps \code{changepoints::WBS.network()} (Yu, Padilla, Wang and Rinaldo):
#' wild binary segmentation on a sequence of networks, detecting the times at
#' which the edge-probability structure changes. The input is one row per
#' time point holding the \emph{vectorised adjacency matrix}, so a series of
#' \eqn{p \times p} networks over \eqn{n} times is an \eqn{n \times p^2}
#' matrix.
#'
#' @param x The network sequence: an \eqn{n \times p^2} matrix of vectorised
#'   adjacency matrices, or an \eqn{n \times p \times p} array.
#' @param copy2 An independent second observation of the same network
#'   sequence, in the same shape. The method's guarantees rest on sample
#'   splitting; see the section below for what happens when there is only
#'   one copy.
#' @param n_intervals Number of random intervals. Defaults to \code{100}.
#' @param threshold Detection threshold. When \code{NULL}, calibrated by
#'   permutation exactly as in \code{\link{hdcov_wrapper}()}.
#' @param alpha,n_perm Level and number of permutations for that
#'   calibration.
#' @param delta Minimum spacing. Defaults to \code{max(5, floor(n / 20))}.
#' @param seed Optional seed. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
#' @section When you have only one copy of the network:
#' \code{WBS.network()} takes two independent observations of the sequence,
#' which is how the theory controls the bias of the squared-Frobenius
#' statistic. Given a single sequence of \emph{binary} networks this wrapper
#' constructs the second copy by splitting each edge indicator at random
#' (each present edge is assigned to one copy with probability one half),
#' which is the usual independent-thinning device and is reported in a
#' message. For weighted networks it splits the weight instead, which is
#' exact for Poisson weights and approximate otherwise. If you have a genuine
#' replicate, pass it as \code{copy2} and none of this applies.
#'
#' @return A \code{ggcpt} object with \code{change_in = "network"}. The
#'   series it carries -- and so the one \code{autoplot()} draws -- is the
#'   \strong{mean edge weight} at each time point, \code{rowMeans()} of the
#'   vectorised adjacency matrices. This is the one multivariate method with
#'   no \code{data_wide} slot: a \eqn{p \times p} network has \eqn{p^2}
#'   entries per time point, so a facet per coordinate would be unreadable.
#'   The changepoints are estimated from the networks themselves, not from
#'   the summary.
#' @references
#' \insertRef{yu2021network}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("changepoints", quietly = TRUE)
#' set.seed(2026)
#' p <- 5
#' mk <- function(n, prob) {
#'   t(replicate(n, as.numeric(matrix(stats::rbinom(p * p, 1, prob), p))))
#' }
#' X <- rbind(mk(40, 0.2), mk(40, 0.6))
#' network_wrapper(X, n_intervals = 20, n_perm = 20, seed = 1)
#' @family changepoint engines
network_wrapper <- function(x, copy2 = NULL, n_intervals = 100,
                            threshold = NULL, alpha = 0.05, n_perm = 20,
                            delta = NULL, seed = NULL) {
  need_pkg("changepoints")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(alpha, "alpha", min = 0, max = 1, min_open = TRUE, max_open = TRUE)
  validate_scalar(n_perm, "n_perm", min = 1)
  # network_matrix() hands a vector to the engine, which stops with base R's
  # "'x' must be an array of at least two dimensions" -- naming neither the
  # method nor the shape. ocd, geomcp, fmean, fcov and fabisearch all name
  # the requirement; match them.
  if (is.null(dim(x)) && !is.list(x)) {
    stop("Method `network` needs a sequence of networks: an n x p^2 matrix ",
         "of vectorised adjacency matrices, or an n x p x p array. `x` is a ",
         "plain vector. See cpt_methods() for univariate methods.",
         call. = FALSE)
  }
  X <- network_matrix(x)
  n <- nrow(X)
  if (n < 6) {
    stop("Network changepoint detection needs at least 6 time points; got ",
         n, ".", call. = FALSE)
  }
  # `network` reaches the engine through network_matrix() rather than
  # validate_data(), so it was the one high-dimensional route with no
  # finiteness check: a single NA surfaced as base R's "replacement has
  # length zero" from inside the random edge-splitting, which names neither
  # the argument nor the problem. Every sibling wrapper here refuses with
  # this message; match them.
  if (anyNA(X) || any(!is.finite(X))) {
    stop_nonfinite(X)
  }
  data_vec <- as.numeric(rowMeans(X))
  if (is.null(delta)) delta <- max(5L, floor(n / 20))
  validate_scalar(delta, "delta", min = 1)
  validate_scalar(n_intervals, "n_intervals", min = 1)
  local_seed(seed)
  if (is.null(threshold) && n_perm < 1 / alpha) {
    warning("The permutation threshold is the ", format(1 - alpha),
            " quantile of ", n_perm, " values, which extrapolates beyond ",
            "the permutation sample. Use at least ", ceiling(1 / alpha),
            " permutations for `alpha = ", format(alpha),
            "`, or pass `threshold` directly.", call. = FALSE)
  }

  if (is.null(copy2)) {
    message("No independent second observation supplied: splitting each ",
            "edge at random to build one. See the \"When you have only one ",
            "copy\" section of ?network_wrapper.")
    # Binomial thinning is exact for binary and count networks: each edge is
    # assigned to one copy with probability one half, and the two copies are
    # independent given the total. For continuous weights there is no exact
    # thinning, so fall back to halving with symmetric noise, which at least
    # preserves the mean.
    is_count <- all(abs(X - round(X)) < 1e-8) && all(X >= 0)
    if (is_count) {
      A <- matrix(stats::rbinom(length(X), size = as.integer(round(X)),
                                prob = 0.5), nrow = n)
      B <- X - A
    } else {
      half <- stats::sd(as.numeric(X)) / 10
      A <- X / 2 + stats::rnorm(length(X), 0, if (is.finite(half)) half else 0)
      B <- X - A
    }
  } else {
    A <- X
    B <- network_matrix(copy2)
    if (!identical(dim(A), dim(B))) {
      stop("`copy2` must have the same shape as `x` (", nrow(A), " x ",
           ncol(A), "); got ", nrow(B), " x ", ncol(B), ".", call. = FALSE)
    }
  }

  iv <- changepoints::WBS.intervals(M = as.integer(n_intervals),
                                    lower = 1, upper = n)
  run <- function(m1, m2) {
    changepoints::WBS.network(t(m1), t(m2), 1, n,
                              iv$Alpha, iv$Beta, delta)
  }
  bs <- run(A, B)

  if (is.null(threshold)) {
    maxima <- vapply(seq_len(as.integer(n_perm)), function(b) {
      idx <- sample.int(n)
      pb <- run(A[idx, , drop = FALSE], B[idx, , drop = FALSE])
      d <- as.numeric(pb$Dval)
      if (length(d) == 0) return(0)
      max(d, na.rm = TRUE)
    }, numeric(1))
    threshold <- as.numeric(stats::quantile(maxima, 1 - alpha, names = FALSE))
  }
  validate_scalar(threshold, "threshold", min = 0)

  # Same upstream single-level failure as in hdcov_wrapper(): measured at
  # 10 of 12 runs for a 20-point sequence of 4-node networks.
  th <- threshold_bs(bs, threshold)
  hat <- th$cpt_hat
  cp <- if (is.null(hat) || length(hat) == 0) {
    integer(0)
  } else {
    as.integer(as.matrix(hat)[, 1])
  }
  cp <- sort(cp[!is.na(cp)])

  ggcpt_build(
    data_vec, cp,
    method = "network",
    change_in = "network",
    penalty = list(type = "threshold", value = threshold),
    fit = bs,
    call = match.call()
  )
}

# Internal: accept either an n x p^2 matrix of vectorised adjacency matrices
# or an n x p x p array, and return the matrix form.
#' @noRd
network_matrix <- function(x) {
  if (is.array(x) && length(dim(x)) == 3L) {
    d <- dim(x)
    return(matrix(aperm(x, c(1, 2, 3)), nrow = d[1], ncol = d[2] * d[3]))
  }
  X <- as.matrix(x)
  if (!is.numeric(X)) stop("`x` must be numeric.", call. = FALSE)
  X
}

#' VAR(1) changepoints
#'
#' Wraps \code{changepoints::CV.search.DP.VAR1()} (Wang, Yu, Rinaldo and
#' Willett): dynamic programming with an \eqn{\ell_0} penalty for changes in
#' the transition matrix of a vector autoregression, with the two tuning
#' parameters chosen by cross-validation. The change here is in the
#' \emph{dynamics} — how the series predicts itself — not in the level, so it
#' is invisible to every mean-change engine in the package.
#'
#' @param x A numeric matrix or data frame, rows as time points.
#' @param gamma_set Candidate values of the \eqn{\ell_0} tuning parameter.
#'   Defaults to a small grid scaled by the series length.
#' @param lambda_set Candidate lasso penalties. Defaults to
#'   \code{c(0.01, 0.1, 1)}.
#' @param delta Minimum spacing. Defaults to \code{max(5, floor(n / 20))}.
#' @param ... Additional arguments passed to the engine.
#' @return A \code{ggcpt} object with \code{change_in = "regression"}.
#' @references
#' \insertRef{wang2019var}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("changepoints", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' p <- 3
#' step <- function(n, a) {
#'   Y <- matrix(0, n, p)
#'   for (i in 2:n) Y[i, ] <- a * Y[i - 1, ] + stats::rnorm(p)
#'   Y
#' }
#' var_wrapper(rbind(step(50, 0.1), step(50, 0.8)),
#'             gamma_set = c(1, 10), lambda_set = c(0.1, 1))
#' }
#' @family changepoint engines
var_wrapper <- function(x, gamma_set = NULL, lambda_set = NULL,
                        delta = NULL, ...) {
  need_pkg("changepoints")
  validate_data(x)
  X <- as_mv_matrix(x)
  # The engine fails on a single column with a message that names nothing
  # ("incorrect number of dimensions"), while ocd, geomcp, fmean, fcov and
  # fabisearch all name the requirement. Match them.
  if (ncol(X) < 2) {
    stop("Method `var` is high-dimensional and needs at least two ",
         "coordinates, but `x` has ", ncol(X),
         ". See cpt_methods() for univariate methods.", call. = FALSE)
  }
  n <- nrow(X)
  data_vec <- as.numeric(X[, 1])
  if (is.null(delta)) delta <- max(5L, floor(n / 20))
  if (is.null(gamma_set)) gamma_set <- c(0.1, 1, 10) * log(n)
  if (is.null(lambda_set)) lambda_set <- c(0.01, 0.1, 1)
  validate_scalar(delta, "delta", min = 1)

  # The engine wants a p x n matrix and forms the lag pair internally from
  # DATA[, -1] and DATA[, -n].
  fit <- changepoints::CV.search.DP.VAR1(t(X), gamma_set, lambda_set,
                                         as.integer(delta), ...)
  # cpt_hat is a list over the (gamma, lambda) grid; take the setting with
  # the smallest test error, which is what the CV search is for.
  errs <- suppressWarnings(as.numeric(unlist(fit$test_error)))
  best <- which.min(errs)
  if (length(best) == 0 || !is.finite(errs[best])) best <- 1L
  cp <- as.integer(unlist(fit$cpt_hat[[best]]))
  cp <- sort(cp[!is.na(cp)])

  ggcpt_build(
    data_vec, cp,
    method = "var",
    change_in = "regression",
    penalty = list(type = "l0 (CV)", value = NA_real_),
    fit = fit,
    call = match.call(),
    data_wide = mv_data_wide(X)
  )
}

#' High-dimensional regression changepoints
#'
#' Wraps \code{changepoints::CV.search.DP.regression()} (Rinaldo, Wang, Wen,
#' Willett and Yu): dynamic programming with an \eqn{\ell_0} penalty for
#' changes in the coefficient vector of a high-dimensional sparse regression,
#' with the two tuning parameters chosen by cross-validation. Where
#' \code{\link{strucchange_wrapper}()} dates breaks in a low-dimensional
#' regression by dynamic programming on the residual sum of squares, this
#' handles the case where there are more covariates than the segments have
#' observations to fit them with.
#'
#' @param x A numeric matrix or data frame of covariates, rows as time
#'   points.
#' @param response A numeric vector of responses, one per row of \code{x}.
#'   Required. Reachable through \code{\link{cpt_detect}()} as
#'   \code{cpt_detect(X, method = "hdreg", response = y)}.
#' @param gamma_set Candidate values of the \eqn{\ell_0} tuning parameter.
#'   Defaults to a small grid scaled by the series length.
#' @param lambda_set Candidate lasso penalties. Defaults to
#'   \code{c(0.01, 0.1, 1)}.
#' @param delta Minimum spacing. Defaults to \code{max(5, floor(n / 20))}.
#' @param ... Additional arguments passed to the engine.
#' @return A \code{ggcpt} object with \code{change_in = "regression"}. The
#'   plotted series is the \emph{response}, which is what a reader of a
#'   regression-break plot expects to see.
#' @references
#' \insertRef{rinaldo2021hdreg}{ggchangepoint}
#' @seealso \code{\link{var_wrapper}()}, \code{\link{strucchange_wrapper}()}.
#' @export
#' @examplesIf requireNamespace("changepoints", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' p <- 10
#' n <- 80
#' X <- matrix(stats::rnorm(n * p), n, p)
#' beta1 <- c(rep(2, 3), rep(0, p - 3))
#' beta2 <- c(rep(0, p - 3), rep(2, 3))
#' y <- c(X[1:40, ] %*% beta1, X[41:n, ] %*% beta2) + stats::rnorm(n)
#' hdreg_wrapper(X, response = y, gamma_set = c(1, 10),
#'               lambda_set = c(0.1, 1))
#' }
#' @family changepoint engines
hdreg_wrapper <- function(x, response = NULL, gamma_set = NULL,
                          lambda_set = NULL, delta = NULL, ...) {
  need_pkg("changepoints")
  if (is.null(response)) {
    stop("`hdreg` regresses a response on the covariates, so `response` is ",
         "required: cpt_detect(X, method = \"hdreg\", response = y).",
         call. = FALSE)
  }
  validate_data(x)
  X <- as_mv_matrix(x)
  n <- nrow(X)
  y <- as.numeric(response)
  if (length(y) != n) {
    stop("`response` must have one value per row of `x`: `x` has ", n,
         " row(s) but `response` has ", length(y), ".", call. = FALSE)
  }
  if (anyNA(y) || any(!is.finite(y))) {
    stop_nonfinite(y, "response")
  }
  if (is.null(delta)) delta <- max(5L, floor(n / 20))
  if (is.null(gamma_set)) gamma_set <- c(0.1, 1, 10) * log(n)
  if (is.null(lambda_set)) lambda_set <- c(0.01, 0.1, 1)
  validate_scalar(delta, "delta", min = 1)

  fit <- changepoints::CV.search.DP.regression(y, X, gamma_set, lambda_set,
                                               as.integer(delta), ...)
  errs <- suppressWarnings(as.numeric(unlist(fit$test_error)))
  best <- which.min(errs)
  if (length(best) == 0 || !is.finite(errs[best])) best <- 1L
  cp <- as.integer(unlist(fit$cpt_hat[[best]]))
  cp <- sort(cp[!is.na(cp)])

  ggcpt_build(
    y, cp,
    method = "hdreg",
    change_in = "regression",
    penalty = list(type = "l0 (CV)", value = NA_real_),
    fit = fit,
    call = match.call()
  )
}
