# ---------------------------------------------------------------------------
# Theme P (0.7.0): distribution-free confidence sets for a changepoint's
# location. Of the routes cpt_confint() had, only the engines' own intervals
# came with a guarantee, and those exist for seven methods; the other
# forty-odd got the bootstrap, which re-runs the detector on resampled
# segments and has no finite-sample coverage statement at all.
#
# CONCH (Hore and Ramdas 2026) needs nothing but exchangeability within
# each segment. For every candidate split t it asks whether permuting the
# observations within each side of t leaves the data looking as changed as
# they do: under the hypothesis that t is the true changepoint those
# permutations do not change the distribution, so the rank of the observed
# score among the permuted ones is a valid p-value, and the candidates
# whose p-value exceeds 1 - level form a set that contains the true
# changepoint with probability at least `level`.
#
# The score used here is the likelihood-ratio form the paper recommends:
# S_t(x) = Q_t(x) - max_s Q_s(x), with Q_s a two-sample statistic at split
# s. Q_t is unchanged by a permutation within the sides of t, so the
# comparison reduces to max_s Q_s(permuted) >= max_s Q_s(observed), which is
# cheap to evaluate with cumulative sums.
# ---------------------------------------------------------------------------

# Internal: which score serves this result, or NULL with the reason. A
# result records what it detected, and kcp labels its answer by the running
# statistic ("running mean"), which is the same exchangeable two-segment
# problem as "mean".
#' @noRd
conformal_score_for <- function(object) {
  ch <- scalar_chr(object$change_in)
  label <- sub("^running ", "", ch %||% "")
  score <- switch(label,
    mean = "mean",
    var = "var",
    variance = "var",
    meanvar = "meanvar",
    distribution = "rank",
    NULL
  )
  if (is.null(score)) {
    return(list(score = NULL, why = paste0(
      "a change in ", if (is.na(ch)) "an unrecorded quantity" else
        paste0("`", ch, "`"), " is not a change between two exchangeable ",
      "segments (a trend, a model or a dependence structure changes within ",
      "each one)")))
  }
  list(score = score, why = NULL)
}

# Internal: can the conformal route serve this result?
#' @noRd
conformal_possible <- function(object) {
  !is.null(conformal_score_for(object)$score)
}

# Internal: the series as a matrix, one column per coordinate.
#' @noRd
conformal_matrix <- function(object) {
  if (n_coordinates(object) > 1L) {
    wide <- object$data_wide
    coords <- setdiff(names(wide), c("index", "index_value"))
    X <- as.matrix(as.data.frame(wide)[, coords, drop = FALSE])
  } else {
    X <- matrix(object$data$value, ncol = 1L)
  }
  storage.mode(X) <- "double"
  X
}

# Internal: cpt_confint(method = "conformal").
#' @noRd
confint_conformal <- function(object, level, B = 200, seed = NULL) {
  sc <- conformal_score_for(object)
  if (is.null(sc$score)) {
    cpt_abort("`method = \"conformal\"` needs each segment to be ",
              "exchangeable, and here ", sc$why, ". Use `method = ",
              "\"bootstrap\"`, or the engine's own interval with `method = ",
              "\"native\"` where it supplies one.",
              class = "capability_absent")
  }
  local_seed(seed)
  B <- as.integer(B)
  alpha <- 1 - level
  X <- conformal_matrix(object)
  n <- nrow(X)
  cp <- sort(unique(as.integer(object$changepoints$cp)))
  bounds <- c(0L, cp, n)
  lower <- upper <- size <- rep(NA_integer_, length(cp))
  profile <- vector("list", length(cp))
  empty <- character(0)
  for (k in seq_along(cp)) {
    rows <- seq.int(bounds[k] + 1L, bounds[k + 2L])
    keep <- rows[stats::complete.cases(X[rows, , drop = FALSE])]
    p <- conch_window(X[keep, , drop = FALSE], B = B, score = sc$score)
    if (is.null(p)) {
      empty <- c(empty, as.character(cp[k]))
      next
    }
    # Candidate t keeps the first t complete rows of the window on the
    # left, so its location is the last of them, in the original positions.
    pos <- keep[p$t]
    profile[[k]] <- tibble::tibble(cp = cp[k], position = pos,
                                   p_value = p$p)
    inside <- pos[!is.na(p$p) & p$p > alpha]
    if (!length(inside)) {
      empty <- c(empty, as.character(cp[k]))
      next
    }
    lower[k] <- min(inside)
    upper[k] <- max(inside)
    size[k] <- length(inside)
  }
  if (length(empty)) {
    cpt_warn("No location in the window around changepoint(s) ",
             paste(empty, collapse = ", "), " is consistent with a single ",
             "change between two exchangeable segments at level ",
             format(level), ", so the conformal set is empty and the bounds ",
             "are NA. That is evidence against the model the interval ",
             "assumes there (another change in the window, a trend, or ",
             "dependent noise), not a precise location.",
             class = "assumption")
  }
  out <- tibble::tibble(cp = cp, ci_lower = lower, ci_upper = upper,
                        level = level, source = "conformal",
                        set_size = size)
  attr(out, "profile") <- do.call(rbind, Filter(Negate(is.null), profile))
  out
}

# Internal: CONCH-MC on one window (rows are time, columns coordinates).
# Returns the candidate splits `t` (the number of rows on the left) and
# their p-values, or NULL when the window is too short.
#
# Every candidate's p-value costs B evaluations of the score over the whole
# window, so an exhaustive pass is quadratic in the window's length (about
# nine seconds at 500 observations). Beyond `exhaustive_max` the candidates
# are evaluated in decreasing order of their own two-sample statistic, the
# order in which their p-values fall, and the pass stops once ten in a row
# have no permuted score reaching the observed one, the smallest p-value
# there is. The rest, whose statistics are smaller still, are left out of
# the set and carry an NA p-value in the profile.
#' @noRd
conch_window <- function(W, B, score, exhaustive_max = 300L) {
  m <- nrow(W)
  edge <- if (score %in% c("var", "meanvar")) 2L else 1L
  if (m < 2L * edge + 2L) return(NULL)
  grid <- seq.int(edge, m - edge)
  prep <- conch_prepare(W, score)
  q_obs <- conch_q(prep, matrix(seq_len(m), m, 1L), grid, score,
                   reduce = FALSE)[, 1L]
  t_obs <- max(q_obs)
  tol <- 1e-10 * max(1, abs(t_obs))
  ord <- conch_orders(m, B)
  p_of <- function(t) {
    idx <- rbind(matrix(ord[ord <= t], t, B), matrix(ord[ord > t], m - t, B))
    q <- conch_q(prep, idx, grid, score)
    (1 + sum(q >= t_obs - tol)) / (B + 1)
  }
  p <- rep(NA_real_, length(grid))
  if (m <= exhaustive_max) {
    p <- vapply(grid, p_of, numeric(1))
  } else {
    floor_p <- 1 / (B + 1)
    run <- 0L
    for (i in order(q_obs, decreasing = TRUE)) {
      p[i] <- p_of(grid[i])
      run <- if (p[i] <= floor_p) run + 1L else 0L
      if (run >= 10L && sum(!is.na(p)) >= 20L) break
    }
  }
  list(t = grid, p = p)
}

# Internal: B independent uniform orderings of 1..m, as the columns of an
# m x B matrix, from one sort. For any split t, the entries of a column
# that are <= t appear in a uniformly random order, and so do those > t,
# independently: taking them in place is a draw from the permutations that
# shuffle each side of t within itself, which is the group CONCH needs.
# The same draws serve every candidate split, which is allowed (each
# candidate's p-value is valid on its own) and saves a sort per candidate.
#' @noRd
conch_orders <- function(m, B) {
  keys <- matrix(stats::runif(m * B), m, B)
  o <- order(col(keys), keys)
  matrix(o - rep((seq_len(B) - 1L) * m, each = m), m, B)
}

# Internal: per-coordinate quantities the score needs that a permutation
# cannot change (the window's own centre and scale, or its ranks).
#' @noRd
conch_prepare <- function(W, score) {
  m <- nrow(W)
  if (score == "rank") {
    R <- apply(W, 2L, rank, ties.method = "average")
    if (!is.matrix(R)) R <- matrix(R, ncol = ncol(W))
    C <- sweep(R, 2L, colMeans(R))
    S <- sweep(C^2, 2L, colMeans(C^2))
    return(list(m = m, C = C, S = S, ss_c = colSums(C^2),
                ss_s = colSums(S^2)))
  }
  centre <- colMeans(W)
  Z <- sweep(W, 2L, centre)
  v <- colSums(Z^2) / m
  list(m = m, Z = Z, v = v)
}

# Internal: column-wise cumulative sums of a matrix in one pass.
#' @noRd
col_cumsum <- function(M) {
  cs <- cumsum(as.numeric(M))
  r <- nrow(M)
  if (ncol(M) > 1L) {
    cs <- cs - rep(c(0, cs[r * seq_len(ncol(M) - 1L)]), each = r)
  }
  matrix(cs, r, ncol(M))
}

# Internal: max over splits of the score's two-sample statistic, for each
# column of the index matrix `idx` (rows of the window in permuted order),
# summed over coordinates.
#' @noRd
conch_q <- function(prep, idx, grid, score, reduce = TRUE) {
  ncoord <- if (score == "rank") ncol(prep$C) else ncol(prep$Z)
  total <- matrix(0, length(grid), ncol(idx))
  for (j in seq_len(ncoord)) {
    q <- conch_q_coord(prep, idx, grid, score, j)
    if (!is.null(q)) total <- total + q
  }
  if (!reduce) return(total)
  total[cbind(max.col(t(total), ties.method = "first"), seq_len(ncol(total)))]
}

# Internal: one coordinate's statistic at every split in `grid` (rows) for
# every permutation (columns); NULL for a coordinate with no variation.
#' @noRd
conch_q_coord <- function(prep, idx, grid, score, j) {
  m <- prep$m
  B <- ncol(idx)
  s <- grid
  if (score == "rank") {
    f <- s * (m - s) / (m * (m - 1))
    if (prep$ss_c[j] <= 0 && prep$ss_s[j] <= 0) return(NULL)
    cl <- col_cumsum(matrix(prep$C[idx, j], m, B))[s, , drop = FALSE]
    cs2 <- col_cumsum(matrix(prep$S[idx, j], m, B))[s, , drop = FALSE]
    zl <- if (prep$ss_c[j] > 0) cl^2 / (f * prep$ss_c[j]) else 0 * cl
    zs <- if (prep$ss_s[j] > 0) cs2^2 / (f * prep$ss_s[j]) else 0 * cs2
    return(pmax(zl, zs))
  }
  v <- prep$v[j]
  if (!is.finite(v) || v <= 0) return(NULL)
  floor_v <- 1e-12 * v
  z <- matrix(prep$Z[idx, j], m, B)
  switch(score,
    mean = {
      cs <- col_cumsum(z)[s, , drop = FALSE]
      cs^2 * (m / (s * (m - s))) / v
    },
    var = {
      cs <- col_cumsum(z^2)
      left <- cs[s, , drop = FALSE] / s
      right <- (rep(cs[m, ], each = length(s)) - cs[s, , drop = FALSE]) /
        (m - s)
      m * log(v) - s * log(pmax(left, floor_v)) -
        (m - s) * log(pmax(right, floor_v))
    },
    meanvar = {
      c1 <- col_cumsum(z)
      c2 <- col_cumsum(z^2)
      l1 <- c1[s, , drop = FALSE]
      l2 <- c2[s, , drop = FALSE]
      r1 <- rep(c1[m, ], each = length(s)) - l1
      r2 <- rep(c2[m, ], each = length(s)) - l2
      vl <- l2 / s - (l1 / s)^2
      vr <- r2 / (m - s) - (r1 / (m - s))^2
      m * log(v) - s * log(pmax(vl, floor_v)) -
        (m - s) * log(pmax(vr, floor_v))
    }
  )
}

#' How often do the confidence intervals cover? A Monte Carlo check
#'
#' Simulates series with one changepoint of known location, detects with
#' \code{method}, computes \code{\link{cpt_confint}()} by each route in
#' \code{routes}, and reports how often the interval contains the true
#' location and how wide it was. This is the measurement behind the
#' coverage statements in \code{?cpt_confint}, and the way to check them
#' for your own series length, jump size and noise before relying on an
#' interval.
#'
#' @param method Detection method passed to \code{\link{cpt_detect}()}.
#'   Defaults to \code{"pelt"}.
#' @param routes Which \code{cpt_confint()} routes to measure: any of
#'   \code{"conformal"}, \code{"bootstrap"}, \code{"native"},
#'   \code{"posterior"} and \code{"nsp"}. Defaults to \code{c("conformal",
#'   "bootstrap")}.
#' @param n Series length. Defaults to \code{200}.
#' @param jump Size of the mean shift, in noise standard deviations.
#'   Defaults to \code{2}.
#' @param noise Noise model, as in \code{\link{cpt_simulate}()}:
#'   \code{"gauss"} (default), \code{"t"} or \code{"ar1"}.
#' @param n_sim Number of simulated series. Defaults to \code{100}.
#' @param level Nominal level. Defaults to \code{0.95}.
#' @param B Replicates (bootstrap) or permutations (conformal) per
#'   interval. Defaults to \code{100}.
#' @param seed Optional seed, scoped to this call.
#' @param ... Passed to \code{\link{cpt_simulate}()} (\code{rho}, \code{df},
#'   ...).
#'
#' @details The changepoint sits at \code{floor(n / 2)}. Coverage is
#'   computed over the replicates in which the detector reported at least
#'   one changepoint, taking the one nearest the truth, so it is coverage
#'   \emph{given a detection}; \code{n_detected} says how many replicates
#'   that is. A route that fails on a replicate (an engine that supplies no
#'   native interval, say) is counted in \code{n_failed} rather than as a
#'   miss.
#' @return A \code{ggcpt_coverage} tibble with one row per route:
#'   \code{method}, \code{route}, \code{n_sim}, \code{n_detected},
#'   \code{n_failed}, \code{coverage}, \code{coverage_se} (its Monte Carlo
#'   standard error), \code{mean_width}, \code{level}. \code{print()},
#'   \code{autoplot()} (coverage with a two-standard-error bar against the
#'   nominal level) and \code{tidy()} work on it.
#' @seealso \code{\link{cpt_confint}()}, \code{\link{cpt_calibration}}.
#' @family inference
#' @export
#' @examples
#' cov <- cpt_confint_coverage(n_sim = 6, n = 80, B = 20, seed = 1)
#' cov
#' ggplot2::autoplot(cov)
cpt_confint_coverage <- function(method = "pelt",
                                 routes = c("conformal", "bootstrap"),
                                 n = 200, jump = 2,
                                 noise = c("gauss", "t", "ar1"),
                                 n_sim = 100, level = 0.95, B = 100,
                                 seed = NULL, ...) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    cpt_abort("`method` must be one method name.", class = "bad_argument")
  }
  routes <- cpt_match_arg(routes, c("conformal", "bootstrap", "native",
                                    "posterior", "nsp"), several.ok = TRUE)
  routes <- unique(routes)
  noise <- cpt_match_arg(noise)
  validate_scalar(n, "n", min = 20)
  validate_scalar(jump, "jump")
  validate_scalar(n_sim, "n_sim", min = 1)
  validate_scalar(level, "level", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  validate_scalar(B, "B", min = 1)
  local_seed(seed)
  n <- as.integer(n)
  truth <- n %/% 2L
  hits <- matrix(NA, n_sim, length(routes), dimnames = list(NULL, routes))
  width <- hits
  failed <- stats::setNames(integer(length(routes)), routes)
  for (r in seq_len(n_sim)) {
    sim <- cpt_simulate(n, changepoints = truth, params = c(0, jump),
                        noise = noise, ...)
    fit <- tryCatch(suppressWarnings(cpt_detect(sim$value, method = method)),
                    error = function(e) e)
    if (inherits(fit, "error")) {
      cpt_abort("`method = \"", method, "\"` failed on a simulated series: ",
                conditionMessage(fit), class = "engine_error", parent = fit)
    }
    if (!nrow(fit$changepoints)) next
    for (rt in routes) {
      ci <- tryCatch(suppressWarnings(cpt_confint(fit, method = rt,
                                                  level = level, B = B)),
                     error = function(e) NULL)
      if (is.null(ci) || !nrow(ci)) {
        failed[[rt]] <- failed[[rt]] + 1L
        next
      }
      i <- which.min(abs(ci$cp - truth))
      lo <- ci$ci_lower[i]
      hi <- ci$ci_upper[i]
      hits[r, rt] <- !is.na(lo) && !is.na(hi) && lo <= truth && truth <= hi
      width[r, rt] <- if (is.na(lo) || is.na(hi)) NA_real_ else hi - lo
    }
  }
  out <- do.call(rbind, lapply(routes, function(rt) {
    h <- hits[, rt]
    h <- h[!is.na(h)]
    cov <- if (length(h)) mean(h) else NA_real_
    tibble::tibble(
      method = method, route = rt, n_sim = as.integer(n_sim),
      n_detected = length(h), n_failed = failed[[rt]], coverage = cov,
      coverage_se = if (length(h)) sqrt(cov * (1 - cov) / length(h)) else
        NA_real_,
      mean_width = if (length(h)) mean(width[, rt], na.rm = TRUE) else
        NA_real_,
      level = level)
  }))
  structure(out, class = c("ggcpt_coverage", class(tibble::tibble())),
            n = n, jump = jump, noise = noise)
}

#' @rdname cpt_confint_coverage
#' @param x A \code{ggcpt_coverage} object.
#' @export
print.ggcpt_coverage <- function(x, ...) {
  cat("ggcpt_coverage (realised coverage of cpt_confint())\n")
  cat_field("Design", paste0("n = ", attr(x, "n"), ", one change of ",
                             format(attr(x, "jump")), " sd at n/2, ",
                             attr(x, "noise"), " noise"))
  cat("\n")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @rdname cpt_confint_coverage
#' @export
tidy.ggcpt_coverage <- function(x, ...) {
  tibble::as_tibble(x)
}

#' @rdname cpt_confint_coverage
#' @param object A \code{ggcpt_coverage} object.
#' @export
autoplot.ggcpt_coverage <- function(object, ...) {
  d <- tibble::as_tibble(object)
  d$lower <- pmax(0, d$coverage - 2 * d$coverage_se)
  d$upper <- pmin(1, d$coverage + 2 * d$coverage_se)
  ggplot2::ggplot(d, ggplot2::aes(x = coverage, y = route)) +
    ggplot2::geom_vline(xintercept = unique(d$level), linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper),
                           orientation = "y", width = 0.15, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
    ggplot2::labs(x = "Realised coverage", y = NULL,
                  title = paste0("Coverage of cpt_confint() for `",
                                 unique(d$method)[1], "`"),
                  subtitle = "Dashed: nominal level; bars: two standard errors") +
    theme_ggcpt()
}
