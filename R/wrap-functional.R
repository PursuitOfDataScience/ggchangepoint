# ---------------------------------------------------------------------------
# Engine wave #2, part 2: functional data and network structure.
#
#   fmean, fcov  (fChange)        functional mean / covariance changes
#   kwc          (KWCChangepoint) robust depth-based changes
#   fabisearch   (fabisearch)     changes in network structure via NMF
#
# A functional series is one curve per time point, which in matrix form is
# one ROW per time point and one column per grid location -- the same
# orientation as every other multivariate method here, so nothing new is
# asked of the user.
# ---------------------------------------------------------------------------

#' Functional mean changepoints
#'
#' Wraps \code{fChange::fchange()} for changes in the \emph{mean function} of
#' a functional time series: each observation is a curve, and the question is
#' when the average curve shape changes. The binary-segmentation
#' (\code{"segmentation"}) mode finds multiple changes; the \code{"single"}
#' mode runs the one-change test and reports its p-value.
#'
#' @param x A numeric matrix or data frame with one row per time point and
#'   one column per grid location (the curve's resolution).
#' @param statistic Test statistic: \code{"Tn"} (integrated, the default) or
#'   \code{"Mn"} (maximum).
#' @param critical How critical values are obtained: \code{"simulation"}
#'   (default), \code{"resample"} or \code{"welch"}.
#' @param type \code{"segmentation"} (default, multiple changes) or
#'   \code{"single"} (one change).
#' @param alpha Significance level. Defaults to \code{0.05}.
#' @param robust Use the robust (\code{"robustmean"}) statistic instead of
#'   the classical mean one? Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \code{fChange::fchange()}.
#' @return A \code{ggcpt} object; the changepoints tibble carries the
#'   engine's \code{p_value} for each location.
#' @references
#' \insertRef{aue2018fchange}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("fChange", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' X <- matrix(rnorm(60 * 20), nrow = 60)
#' X[31:60, ] <- X[31:60, ] + 2
#' fmean_wrapper(X, M = 200)
#' }
#' @family changepoint engines
fmean_wrapper <- function(x, statistic = c("Tn", "Mn"),
                          critical = c("simulation", "resample", "welch"),
                          type = c("segmentation", "single"), alpha = 0.05,
                          robust = FALSE, ...) {
  need_pkg("fChange")
  statistic <- match.arg(statistic)
  critical <- match.arg(critical)
  type <- match.arg(type)
  validate_flag(robust, "robust")
  fchange_run(x, method = if (robust) "robustmean" else "mean",
              statistic = statistic, critical = critical, type = type,
              alpha = alpha, change_in = "mean",
              method_name = "fmean", call = match.call(), ...)
}

#' Functional covariance changepoints
#'
#' Wraps \code{fChange::fchange()} for changes in the covariance operator,
#' eigenstructure or trace of a functional time series — the changes that
#' leave the mean curve untouched.
#'
#' @inheritParams fmean_wrapper
#' @param target What to test: \code{"covariance"} (default), \code{"trace"},
#'   \code{"eigenjoint"} or \code{"eigensingle"}.
#' @return A \code{ggcpt} object with \code{change_in = "covariance"}.
#' @references
#' \insertRef{aue2018fchange}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("fChange", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' X <- matrix(rnorm(60 * 20), nrow = 60)
#' X[31:60, ] <- X[31:60, ] * 3
#' fcov_wrapper(X, target = "trace", M = 200)
#' }
#' @family changepoint engines
fcov_wrapper <- function(x,
                         target = c("covariance", "trace", "eigenjoint",
                                    "eigensingle"),
                         statistic = c("Tn", "Mn"),
                         critical = c("simulation", "resample", "welch"),
                         type = c("segmentation", "single"), alpha = 0.05,
                         ...) {
  need_pkg("fChange")
  target <- match.arg(target)
  statistic <- match.arg(statistic)
  critical <- match.arg(critical)
  type <- match.arg(type)
  fchange_run(x, method = target, statistic = statistic, critical = critical,
              type = type, alpha = alpha, change_in = "covariance",
              method_name = "fcov", call = match.call(), ...)
}

# Internal: shared fChange plumbing. The engine narrates its search to the
# console and returns either a data frame of (location, pvalue) or NULL when
# nothing is found, so both are normalised here.
#' @noRd
fchange_run <- function(x, method, statistic, critical, type, alpha,
                        change_in, method_name, call, ...) {
  validate_data(x)
  X <- as_mv_matrix(x)
  if (ncol(X) < 2) {
    stop("`", method_name, "` needs functional observations: one row per ",
         "time point and one column per grid location. `x` has a single ",
         "column, which is a scalar series -- see cpt_methods() for the ",
         "univariate engines.", call. = FALSE)
  }
  n <- nrow(X)
  data_vec <- as.numeric(rowMeans(X))

  # fChange takes curves down the columns (grid x time).
  utils::capture.output(
    fit <- fChange::fchange(t(X), method = method, statistic = statistic,
                            critical = critical, type = type,
                            alpha = alpha, ...)
  )

  loc <- integer(0)
  pval <- numeric(0)
  if (is.data.frame(fit) && nrow(fit) > 0) {
    loc <- as.integer(fit$location)
    pval <- as.numeric(fit$pvalue %||% rep(NA_real_, length(loc)))
  } else if (is.list(fit) && !is.null(fit$location)) {
    loc <- as.integer(fit$location)
    pval <- as.numeric(fit$pvalue %||% rep(NA_real_, length(loc)))
  }
  keep <- !is.na(loc)
  loc <- loc[keep]; pval <- pval[keep]
  ord <- order(loc)

  ggcpt_build(
    data_vec, loc[ord],
    method = method_name,
    change_in = change_in,
    penalty = list(type = "alpha", value = alpha),
    fit = fit,
    call = call,
    extra_cp_cols = if (length(loc) > 0) list(p_value = pval[ord]),
    data_wide = mv_data_wide(X)
  )
}

#' Robust depth-based changepoints for functional and multivariate data
#'
#' Wraps \pkg{KWCChangepoint} (Ramsay and Chenouri): the functional
#' Kruskal–Wallis covariance test, which ranks observations by statistical
#' \emph{depth} and segments on the ranks. Because it never touches the
#' values themselves it is insensitive to heavy tails and outlying curves,
#' which is exactly where the moment-based functional tests degrade.
#'
#' @param x A numeric matrix or data frame with one row per observation
#'   (time point) and one column per grid location or coordinate.
#' @param algorithm \code{"fkwc"} (default; pruned exact linear time over the
#'   depth ranks) or \code{"dwbs"} (depth-based wild binary segmentation).
#' @param depth Depth function. For \code{"fkwc"} one of \code{"RPD"}
#'   (default), \code{"FM"}, \code{"LTR"}, \code{"FMd"}, \code{"RPDd"}; for
#'   \code{"dwbs"} one of \code{"spat"}, \code{"hs"}, \code{"mahal"},
#'   \code{"mahal75"}.
#' @param change_in Reported change type: \code{"covariance"} (default) or
#'   \code{"distribution"}. The test is sensitive to both; this only labels
#'   the result.
#' @param seed Optional seed — the random-projection depths and the wild
#'   binary segmentation both randomise.
#' @param ... Additional arguments passed to the engine.
#' @return A \code{ggcpt} object.
#' @references
#' \insertRef{ramsay2024kwc}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("KWCChangepoint", quietly = TRUE)
#' set.seed(2026)
#' X <- matrix(rnorm(100 * 20), nrow = 100)
#' X[51:100, ] <- X[51:100, ] * 3
#' kwc_wrapper(X, seed = 1)
#' @family changepoint engines
kwc_wrapper <- function(x, algorithm = c("fkwc", "dwbs"), depth = NULL,
                        change_in = c("covariance", "distribution"),
                        seed = NULL, ...) {
  need_pkg("KWCChangepoint")
  algorithm <- match.arg(algorithm)
  change_in <- match.arg(change_in)
  validate_data(x)
  X <- as_mv_matrix(x)
  data_vec <- as.numeric(rowMeans(X))
  if (!is.null(seed)) set.seed(seed)

  if (is.null(depth)) {
    depth <- if (algorithm == "fkwc") "RPD" else "spat"
  }
  fit <- if (algorithm == "fkwc") {
    KWCChangepoint::fkwc(X, depth = depth, ...)
  } else {
    suppressWarnings(KWCChangepoint::dwbs(X, depth = depth, ...))
  }
  cp <- sort(as.integer(fit$changepoints))
  cp <- cp[!is.na(cp)]

  ggcpt_build(
    data_vec, cp,
    method = "kwc",
    change_in = change_in,
    penalty = list(type = algorithm, value = NA_real_),
    fit = fit,
    call = match.call(),
    data_wide = mv_data_wide(X)
  )
}

#' Network-structure changepoints via non-negative matrix factorisation
#'
#' Wraps \code{fabisearch::detect.cps()} (Ondrus, Olds and Cribben, 2024):
#' factorised binary search for changes in the \emph{network structure} of a
#' high-dimensional series. Each candidate split is scored by how much better
#' a rank-\eqn{r} non-negative matrix factorisation fits the two halves
#' separately than together, and significance is assessed by permutation.
#'
#' @param x A non-negative numeric matrix or data frame with rows as time
#'   points and columns as nodes. Non-negativity is a hard requirement of
#'   NMF, not a preference; see the section below.
#' @param min_dist Minimum distance between changepoints. Defaults to
#'   \code{35} (the engine's default), lowered automatically when the series
#'   is too short for it.
#' @param n_runs NMF runs per candidate split. Defaults to \code{50}.
#' @param n_reps Permutation replicates for the significance test. Defaults
#'   to \code{100}.
#' @param alpha Significance level applied to the permutation p-value each
#'   candidate split receives. Defaults to \code{0.05}. Note that a
#'   permutation p-value cannot fall below \code{1 / n_reps}, so
#'   \code{n_reps} must be at least \code{1 / alpha} for any split to be
#'   significant; the wrapper warns when it is not.
#' @param rank NMF rank. \code{NULL} estimates it with
#'   \code{fabisearch::opt.rank()}, which is expensive; supplying a rank is
#'   much faster.
#' @param n_core Cores for the permutation stage. Defaults to \code{1}.
#' @param seed Optional seed.
#' @param ... Additional arguments passed to \code{fabisearch::detect.cps()}.
#'
#' @section Non-negativity, cost, and the attached namespace:
#' Three practical notes. (1) NMF is undefined for negative entries, so this
#' wrapper refuses them rather than letting the engine fail deep inside a
#' factorisation; shift or rescale the series first if it has negatives.
#' (2) It is by far the most expensive engine here — \code{n_runs} times
#' \code{n_reps} factorisations — so the defaults are lowered in the examples
#' and a progress note is printed. (3) \pkg{fabisearch} calls \pkg{NMF}'s
#' multi-run machinery, which resolves helpers through the search path and
#' fails with "none of the packages are loaded" when \pkg{NMF} is merely
#' loaded; this wrapper therefore attaches \pkg{NMF} for the duration of the
#' call and detaches it again afterwards.
#'
#' @return A \code{ggcpt} object with \code{change_in = "network"}.
#' @references
#' \insertRef{ondrus2024fabisearch}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("fabisearch", quietly = TRUE)
#' \donttest{
#' # A change in *structure*, not in scale: two latent factors drive
#' # different halves of the node set before and after the change.
#' # Deliberately tiny -- this is by far the most expensive engine in the
#' # package (n_runs x n_reps factorisations per candidate split), and the
#' # settings below are chosen to keep the example inside a check budget,
#' # not to detect anything. Use the defaults on real data.
#' set.seed(2026)
#' block <- function(n, cols) {
#'   f <- abs(stats::rnorm(n)) + 0.5
#'   Y <- matrix(abs(stats::rnorm(n * 5)) * 0.2 + 0.1, n, 5)
#'   Y[, cols] <- Y[, cols] + f
#'   Y
#' }
#' Y <- rbind(block(25, 1:2), block(25, 3:5))
#' fabisearch_wrapper(Y, min_dist = 10, n_runs = 1, n_reps = 4,
#'                    alpha = 0.25, rank = 2)
#' }
#' @family changepoint engines
fabisearch_wrapper <- function(x, min_dist = 35, n_runs = 50, n_reps = 100,
                               alpha = NULL, rank = NULL, n_core = 1,
                               seed = NULL, ...) {
  need_pkg("fabisearch")
  validate_data(x)
  X <- as_mv_matrix(x)
  n <- nrow(X)
  if (any(X < 0)) {
    stop("`fabisearch` factorises the series with non-negative matrix ",
         "factorisation, which is undefined for negative values (", sum(X < 0),
         " of ", length(X), " entries are negative). Shift or rescale the ",
         "series first.", call. = FALSE)
  }
  # NMF also rejects an all-zero row, and the engine's own message names
  # neither the rows nor the reason. Catching it here keeps the wrapper's
  # preconditions in one place.
  zero_rows <- which(rowSums(X) == 0)
  if (length(zero_rows) > 0) {
    stop("`fabisearch` cannot factorise a series with an all-zero time ",
         "point: row", if (length(zero_rows) > 1) "s " else " ",
         paste(utils::head(zero_rows, 5), collapse = ", "),
         if (length(zero_rows) > 5) ", ..." else "",
         " (", length(zero_rows), " of ", nrow(X), ") ",
         if (length(zero_rows) > 1) "sum" else "sums",
         " to zero. Drop those rows or add a small positive offset.",
         call. = FALSE)
  }
  data_vec <- as.numeric(rowMeans(X))
  validate_scalar(min_dist, "min_dist", min = 2)
  min_dist <- min(as.integer(min_dist), max(2L, floor(n / 3)))
  if (!is.null(seed)) set.seed(seed)

  # See the "attached namespace" note in the docs.
  if (!"package:NMF" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("NMF"))
    on.exit(try(detach("package:NMF", unload = FALSE), silent = TRUE),
            add = TRUE)
  }

  # The engine prints its search progress and, with ncore = 1, foreach
  # warns once per call that no parallel backend is registered. Neither is
  # information the caller asked for; a genuine warning still gets through.
  utils::capture.output(
    withCallingHandlers(
      fit <- fabisearch::detect.cps(X, mindist = min_dist, nruns = n_runs,
                                    nreps = n_reps, alpha = alpha,
                                    rank = rank, ncore = n_core, ...),
      warning = function(w) {
        if (grepl("no parallel backend registered", conditionMessage(w),
                  fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )

  # `change_points$stat_test` has TWO shapes, and confusing them is a silent
  # wrong answer either way:
  #   alpha = NULL  -> the permutation p-value of each candidate split, so
  #                    the wrapper thresholds it;
  #   alpha = <num> -> the engine has already thresholded, and the column is
  #                    a logical verdict.
  # Reading the logical form as a number turns FALSE into 0, which clears
  # any p-value threshold -- so every candidate the search ever proposed
  # would come back as a changepoint. Branch on the type.
  level <- alpha %||% 0.05
  cps <- fit$change_points
  # A permutation p-value cannot fall below 1 / n_reps, so a small n_reps
  # with a conventional alpha makes significance unreachable and the engine
  # returns "no changepoints" for reasons that have nothing to do with the
  # data. Say so rather than letting it look like a finding.
  if (n_reps < 1 / level) {
    warning("With `n_reps = ", n_reps, "` the smallest attainable ",
            "permutation p-value is ", format(1 / n_reps),
            ", which is above `alpha = ", format(level),
            "`, so no split can be significant whatever the data. Raise ",
            "`n_reps` to at least ", ceiling(1 / level),
            ", or raise `alpha`.", call. = FALSE)
  }
  cp <- if (is.data.frame(cps) && nrow(cps) > 0) {
    st <- cps$stat_test
    keep <- if (is.logical(st)) {
      !is.na(st) & st
    } else {
      pv <- suppressWarnings(as.numeric(st))
      if (all(is.na(pv))) rep(TRUE, nrow(cps)) else !is.na(pv) & pv <= level
    }
    sort(as.integer(cps$T[keep]))
  } else {
    integer(0)
  }
  cp <- cp[!is.na(cp)]

  ggcpt_build(
    data_vec, cp,
    method = "fabisearch",
    change_in = "network",
    penalty = list(
      type = if (is.null(alpha)) "permutation p <=" else "engine alpha",
      value = level
    ),
    fit = fit,
    call = match.call(),
    data_wide = mv_data_wide(X)
  )
}
