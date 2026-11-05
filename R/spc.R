# ---------------------------------------------------------------------------
# Theme AS (0.7.0): statistical process control. Sequential changepoint
# detection IS process control: a CUSUM is a sequential likelihood-ratio
# test for a change in mean, an EWMA its smoothed relative, and a control
# chart's in-control average run length is the false-alarm budget the
# monitoring literature calls ARL0. These two monitors are textbook and
# short, so cpt_monitor() implements them rather than wrapping anything;
# what has to be right is the calibration, and that is computed here.
#
# Both limits are calibrated to a target ARL0 by the Markov-chain
# approximation (Brook and Evans 1972 for the CUSUM; Lucas and Saccucci
# 1990 for the EWMA), solved for the limit with uniroot(). The test suite
# measures the in-control run length these limits give by simulation, and
# checks the chain against the published tables.
# ---------------------------------------------------------------------------

# Internal: one-sided CUSUM average run length, S_t = max(0, S_{t-1} + z_t -
# k) with alarm at S_t >= h, for standard normal z shifted by `delta`.
# States: 0 (the reflecting barrier) and N - 1 cells of [0, h).
#' @noRd
cusum_arl_one <- function(h, k, delta = 0, N = 200L) {
  w <- 2 * h / (2 * N - 1)
  mid <- c(0, w * seq_len(N - 1L))
  # From state i the next value is mid[i] + z - k; it lands in state 0 when
  # it is at most w/2, in state j when it is within w/2 of mid[j].
  P <- matrix(0, N, N)
  for (i in seq_len(N)) {
    base <- mid[i] - k + delta
    P[i, 1] <- stats::pnorm(w / 2 - base)
    if (N > 1L) {
      up <- mid[-1] + w / 2 - base
      lo <- mid[-1] - w / 2 - base
      P[i, -1] <- stats::pnorm(up) - stats::pnorm(lo)
    }
  }
  arl <- solve(diag(N) - P, rep(1, N))
  arl[1]
}

# Internal: two-sided CUSUM ARL from the two one-sided ones (the standard
# combination 1 / ARL = 1 / ARL+ + 1 / ARL-).
#' @noRd
cusum_arl <- function(h, k, delta = 0) {
  up <- cusum_arl_one(h, k, delta)
  down <- cusum_arl_one(h, k, -delta)
  1 / (1 / up + 1 / down)
}

# Internal: the decision interval h whose two-sided in-control ARL is arl0.
#' @noRd
cusum_h_for <- function(arl0, k) {
  f <- function(h) log(cusum_arl(h, k)) - log(arl0)
  hi <- 1
  while (f(hi) < 0 && hi < 200) hi <- hi * 2
  stats::uniroot(f, c(1e-3, hi), tol = 1e-6)$root
}

# Internal: two-sided EWMA ARL with asymptotic limits +/- L * sigma_z, for
# z_t = lambda x_t + (1 - lambda) z_{t-1}, z_0 = 0, x standard normal
# shifted by `delta`. 2N + 1 cells across the in-control band.
#' @noRd
ewma_arl <- function(L, lambda, delta = 0, N = 100L) {
  ucl <- L * sqrt(lambda / (2 - lambda))
  M <- 2L * N + 1L
  w <- 2 * ucl / M
  mid <- -ucl + w * (seq_len(M) - 0.5)
  lo <- mid - w / 2
  hi <- mid + w / 2
  P <- matrix(0, M, M)
  for (i in seq_len(M)) {
    centre <- (1 - lambda) * mid[i] + lambda * delta
    P[i, ] <- stats::pnorm((hi - centre) / lambda) -
      stats::pnorm((lo - centre) / lambda)
  }
  arl <- solve(diag(M) - P, rep(1, M))
  arl[N + 1L]
}

# Internal: the limit multiplier L whose in-control ARL is arl0.
#' @noRd
ewma_L_for <- function(arl0, lambda) {
  f <- function(L) log(ewma_arl(L, lambda)) - log(arl0)
  stats::uniroot(f, c(0.5, 6), tol = 1e-6)$root
}

# Internal: validate a control-chart target ARL. Unlike cpm's, which picks
# from a grid the engine ships, any value of at least 2 can be met.
#' @noRd
check_chart_arl0 <- function(arl0) {
  validate_scalar(arl0, "arl0", min = 2)
  invisible(TRUE)
}
