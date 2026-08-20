#' inspect wrapper — high-dimensional changepoints via sparse projection
#'
#' Wraps \code{InspectChangepoint::inspect()} (Wang and Samworth, 2018). For
#' a \eqn{p}-variate series whose mean changes in an unknown sparse subset of
#' coordinates, the algorithm computes the CUSUM transformation, finds the
#' optimal sparse projection direction via a convex relaxation, and locates
#' changepoints on the projected univariate series, recursing via wild binary
#' segmentation.
#'
#' @param x A numeric matrix or data frame with one row per time point and
#'   one column per coordinate.
#' @param lambda Regularisation parameter of the sparse projection; when
#'   \code{NULL} the engine default \eqn{\sqrt{\log(p \log n)/2}} is used.
#' @param threshold Detection threshold; when \code{NULL} it is computed by
#'   Monte Carlo (via the engine).
#' @param ... Additional arguments passed to
#'   \code{InspectChangepoint::inspect()}.
#' @return A \code{ggcpt} object. The changepoints tibble carries a
#'   \code{strength} column (the maximum projected CUSUM statistic). The
#'   first coordinate is used for \code{cp_value} and the univariate plot
#'   line; the full matrix is kept for the faceted multivariate
#'   \code{autoplot()}. Coordinates that are constant carry no changepoint
#'   information and would make the engine's variance rescaling undefined, so
#'   they are dropped (with a warning) before detection and an all-constant
#'   matrix returns an empty result; the dropped coordinates are still kept
#'   for plotting, and reported locations always refer to the original rows.
#' @references
#' \insertRef{wang2018inspect}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("InspectChangepoint", quietly = TRUE)
#' set.seed(2026)
#' X <- cbind(c(rnorm(80), rnorm(80, 3)), c(rnorm(80), rnorm(80, -2)),
#'            rnorm(160))
#' res <- inspect_wrapper(X)
#' res$changepoints
inspect_wrapper <- function(x, lambda = NULL, threshold = NULL, ...) {
  need_pkg("InspectChangepoint")

  validate_data(x)
  X <- as_mv_matrix(x)
  data_vec <- as.numeric(X[, 1])

  # A flat coordinate makes the engine's variance rescaling divide by zero,
  # which fails with "missing value where TRUE/FALSE needed" even when the
  # other coordinates carry a real change.
  X_fit <- drop_constant_cols(X, "inspect")
  if (is.null(X_fit)) {
    return(ggcpt_build(data_vec, integer(0), method = "inspect",
                       change_in = "mean",
                       penalty = list(type = "threshold", value = NA_real_),
                       call = match.call(), data_wide = mv_data_wide(X)))
  }

  args <- list(x = t(X_fit), ...)
  if (!is.null(lambda)) args$lambda <- lambda
  if (!is.null(threshold)) args$threshold <- threshold

  # The engine prints Monte Carlo progress; keep the console clean.
  utils::capture.output(fit <- do.call(InspectChangepoint::inspect, args))

  cp_mat <- fit$changepoints
  cp_indices <- if (is.null(cp_mat)) integer(0) else as.integer(cp_mat[, "location"])

  ggcpt_build(
    data_vec, cp_indices,
    method = "inspect",
    change_in = "mean",
    penalty = list(type = "threshold",
                   value = if (!is.null(threshold)) threshold else NA_real_),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) {
      list(strength = as.numeric(cp_mat[, "max.proj.cusum"]))
    },
    data_wide = mv_data_wide(X)
  )
}

#' ocd wrapper — online high-dimensional changepoint detection
#'
#' Wraps the \code{ocd} package (Chen, Wang and Samworth, 2022): online
#' multiscale detection of a mean change in a high-dimensional stream, with
#' worst-case detection-delay guarantees and per-observation cost independent
#' of history. The detector assumes standardised data with known pre-change
#' mean; this wrapper estimates the baseline mean and standard deviation
#' from an initial training window, then monitors the remainder of the
#' series, resetting after each declaration so multiple changes can be
#' found.
#'
#' @param x A numeric matrix or data frame with one row per time point and at
#'   least two columns. The \code{ocd} detector is inherently
#'   high-dimensional and cannot be constructed for a single coordinate, so
#'   univariate input is rejected; use a univariate engine
#'   (see \code{\link{cpt_methods}()}) for one series.
#' @param train Number of initial observations used to estimate the baseline
#'   mean/sd (not monitored). Defaults to
#'   \code{max(20, floor(0.2 * n))}, capped at \code{n/2}.
#' @param thresh Threshold specification passed to
#'   \code{ocd::ChangepointDetector()}; \code{"MC"} (default) calibrates by
#'   Monte Carlo, which is what makes this the slowest wrapper — see the
#'   timing note below. Supplying the three thresholds directly, as a named
#'   numeric vector \code{c(diag =, off_d =, off_s =)}, skips calibration
#'   altogether.
#' @param patience Target average run length to false alarm. Defaults to
#'   \code{5000}.
#' @param beta Assumed lower bound on the squared Euclidean norm of the mean
#'   change. Defaults to \code{1}.
#' @param mc_reps Monte Carlo repetitions for threshold calibration.
#'   Defaults to \code{100}. The cost is linear in this and grows with the
#'   number of coordinates; see the timing note below.
#' @param ... Additional arguments passed to
#'   \code{ocd::ChangepointDetector()}.
#' @return A \code{ggcpt} object. Because the detector is online, reported
#'   locations are \emph{declaration times} (the changepoint plus the
#'   detection delay), stored together with a \code{declared_at} column.
#' @section How long this takes:
#' Nearly all of the run time is \code{ocd}'s Monte Carlo threshold
#' calibration, which happens before a single observation is read. It is
#' linear in \code{mc_reps} and grows with the number of coordinates:
#' measured at \code{mc_reps = 5}, construction takes about 3 s at
#' \eqn{p = 3}, 9 s at \eqn{p = 10} and 55 s at \eqn{p = 50}, and four
#' times as long at \code{mc_reps = 20}. At the default \code{mc_reps = 100}
#' that extrapolates to roughly a minute at \eqn{p = 3} and a quarter of an
#' hour at \eqn{p = 50}. Monitoring the observations afterwards is cheap by
#' comparison — well under a second for a thousand of them. Lower
#' \code{mc_reps} while exploring, or pass \code{thresh} directly to skip
#' calibration entirely.
#'
#' @references
#' \insertRef{chen2022ocd}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("ocd", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' X <- rbind(matrix(rnorm(60 * 3), 60), matrix(rnorm(40 * 3, 3), 40))
#' res <- ocd_wrapper(X, mc_reps = 5)
#' res$changepoints
#' }
ocd_wrapper <- function(x, train = NULL, thresh = "MC", patience = 5000,
                        beta = 1, mc_reps = 100, ...) {
  need_pkg("ocd")

  validate_data(x)
  X <- if (is.matrix(x) || is.data.frame(x)) as_mv_matrix(x)
       else matrix(as.numeric(x), ncol = 1)
  n <- nrow(X)
  p <- ncol(X)
  # ocd::ChangepointDetector() fails to construct at dim = 1 ("subscript out
  # of bounds"): the method projects across coordinates, so it needs at
  # least two. Say so rather than surfacing the engine's internal error.
  if (p < 2) {
    stop("Method `ocd` is high-dimensional and needs at least two ",
         "coordinates, but `x` has ", p,
         ". See cpt_methods() for univariate methods.", call. = FALSE)
  }
  data_vec <- as.numeric(X[, 1])

  if (is.null(train)) {
    train <- max(20L, floor(0.2 * n))
  }
  train <- min(as.integer(train), floor(n / 2))
  if (train < 2) {
    stop("`train` must be at least 2 observations.", call. = FALSE)
  }

  estimate_baseline <- function(rows) {
    m <- colMeans(X[rows, , drop = FALSE])
    s <- apply(X[rows, , drop = FALSE], 2, stats::sd)
    s[s == 0 | is.na(s)] <- 1
    list(mean = m, sd = s)
  }
  base <- estimate_baseline(seq_len(train))

  detector <- ocd::ChangepointDetector(dim = p, method = "ocd",
                                       thresh = thresh, patience = patience,
                                       beta = beta, MC_reps = mc_reps, ...)

  # The engine prints "Changepoint declared at time = ..." on detection.
  declared <- integer(0)
  i <- train + 1
  while (i <= n) {
    z <- (X[i, ] - base$mean) / base$sd
    utils::capture.output(detector <- ocd::getData(detector, z))
    if (!identical(ocd::status(detector), "monitoring")) {
      declared <- c(declared, i)
      if (i + 2 > n) break  # no room to re-train after the declaration
      detector <- ocd::reset(detector)
      # Re-estimate the baseline from a window after the declaration —
      # keeping the pre-change baseline would re-declare immediately on the
      # shifted regime.
      new_train <- seq(i + 1, min(i + train, n))
      base <- estimate_baseline(new_train)
      i <- new_train[length(new_train)] + 1
    } else {
      i <- i + 1
    }
  }

  ggcpt_build(
    data_vec, declared,
    method = "ocd",
    change_in = "mean",
    penalty = list(type = "patience", value = patience),
    fit = detector,
    call = match.call(),
    extra_cp_cols = if (length(declared) > 0) {
      list(declared_at = as.integer(declared))
    },
    data_wide = mv_data_wide(X)
  )
}

#' Geometrically-inspired multivariate changepoint wrapper (geomcp)
#'
#' Wraps \code{changepoint.geo::geomcp()} (Grundy, Killick and Mihaylov,
#' 2020): each multivariate observation is mapped to its distance from, and
#' angle to, a reference point, and univariate PELT is run on the two mapped
#' series. Distance changes capture shifts in magnitude, angle changes
#' capture shifts in orientation/correlation structure.
#'
#' @param x A numeric matrix or data frame with one row per time point.
#' @param penalty Penalty for the univariate PELT runs (a
#'   \pkg{changepoint}-style character penalty). Defaults to \code{"MBIC"}.
#' @param mapping Which mapped series' changepoints to report:
#'   \code{"both"} (union, default), \code{"distance"}, or \code{"angle"}.
#' @param ... Additional arguments passed to
#'   \code{changepoint.geo::geomcp()}.
#' @return A \code{ggcpt} object whose changepoints tibble carries a
#'   \code{mapping} column (\code{"distance"} or \code{"angle"}; a location
#'   found in both is labelled \code{"both"}).
#' @references
#' \insertRef{grundy2020geomcp}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("changepoint.geo", quietly = TRUE)
#' set.seed(2026)
#' X <- rbind(matrix(rnorm(100 * 4), 100), matrix(rnorm(100 * 4, 2), 100))
#' res <- geomcp_wrapper(X)
#' res$changepoints
geomcp_wrapper <- function(x, penalty = "MBIC",
                           mapping = c("both", "distance", "angle"), ...) {
  need_pkg("changepoint.geo")
  mapping <- match.arg(mapping)

  validate_data(x)
  X <- as_mv_matrix(x)
  data_vec <- as.numeric(X[, 1])

  fit <- changepoint.geo::geomcp(X, penalty = penalty, ...)

  dist_cp <- as.integer(fit@dist.cpts)
  ang_cp <- as.integer(fit@ang.cpts)

  cp_indices <- switch(mapping,
    both = sort(unique(c(dist_cp, ang_cp))),
    distance = sort(unique(dist_cp)),
    angle = sort(unique(ang_cp))
  )

  map_lab <- vapply(cp_indices, function(cp) {
    in_d <- cp %in% dist_cp
    in_a <- cp %in% ang_cp
    if (in_d && in_a) "both" else if (in_d) "distance" else "angle"
  }, character(1))

  ggcpt_build(
    data_vec, cp_indices,
    method = "geomcp",
    change_in = "distribution",
    penalty = list(type = penalty, value = NA_real_),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(cp_indices) > 0) list(mapping = map_lab),
    data_wide = mv_data_wide(X)
  )
}
