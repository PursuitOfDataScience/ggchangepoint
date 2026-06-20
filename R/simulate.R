#' Generate simulated changepoint data
#'
#' Creates a synthetic time series with known changepoints for testing and
#' benchmarking.
#'
#' @param n Length of the series.
#' @param changepoints Integer vector of changepoint locations (last index of
#'   each segment before the change).
#' @param change_in What changes: \code{"mean"}, \code{"var"}, \code{"meanvar"},
#'   or \code{"slope"}.
#' @param params A list of parameters per segment. For \code{mean} changes, a
#'   vector of segment means. For \code{var} changes, a vector of segment sds.
#'   For \code{meanvar}, a list of lists with \code{mean} and \code{sd} per
#'   segment. For \code{slope}, a list with \code{intercept} and \code{slope}
#'   per segment.
#' @param noise Noise type: \code{"gauss"} (Gaussian), \code{"t"} (Student-t),
#'   \code{"ar1"} (AR(1)), or \code{"rw"} (random walk).
#' @param sd Noise standard deviation (for Gaussian and t). Defaults to 1.
#' @param df Degrees of freedom for t-noise. Defaults to 3.
#' @param rho AR(1) autocorrelation parameter. Defaults to 0.
#' @param seed Optional seed for reproducibility.
#'
#' @return A tibble with columns \code{index} and \code{value}. The true
#'   changepoints are stored in the \code{true_changepoints} attribute.
#' @export
#'
#' @examples
#' dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
#'                     params = c(0, 10), seed = 2022)
#' attr(dat, "true_changepoints")
cpt_simulate <- function(n,
                         changepoints = integer(),
                         change_in = c("mean", "var", "meanvar", "slope"),
                         params = NULL,
                         noise = c("gauss", "t", "ar1", "rw"),
                         sd = 1,
                         df = 3,
                         rho = 0,
                         seed = NULL) {

  change_in <- match.arg(change_in)
  noise <- match.arg(noise)

  if (!is.null(seed)) set.seed(seed)

  changepoints <- sort(unique(as.integer(changepoints)))
  changepoints <- changepoints[changepoints > 0 & changepoints < n]

  # Build segment boundaries
  seg_ends <- unique(c(changepoints, n))
  seg_starts <- c(1, seg_ends[-length(seg_ends)] + 1)

  n_seg <- length(seg_ends)

  # Build the per-observation signal (mean) and noise scale (sd). For "var"
  # and "meanvar" the per-segment standard deviation is applied to the noise,
  # so a change in variance is genuinely simulated.
  signal <- numeric(n)
  sd_vec <- rep(sd, n)

  for (i in seq_len(n_seg)) {
    idx <- seg_starts[i]:seg_ends[i]

    if (change_in == "mean") {
      if (is.null(params)) params <- rep(0, n_seg)
      signal[idx] <- params[min(i, length(params))]
    } else if (change_in == "var") {
      if (is.null(params)) params <- rep(1, n_seg)
      signal[idx] <- 0
      sd_vec[idx] <- params[min(i, length(params))]
    } else if (change_in == "meanvar") {
      if (is.list(params)) {
        p <- params[[min(i, length(params))]]
        signal[idx] <- p$mean
        if (!is.null(p$sd)) sd_vec[idx] <- p$sd
      } else {
        signal[idx] <- params[min(i, length(params))]
      }
    } else if (change_in == "slope") {
      if (is.null(params)) {
        params <- rep(list(list(intercept = 0, slope = 0)), n_seg)
      }
      p <- params[[min(i, length(params))]]
      t_vals <- seq_along(idx)
      signal[idx] <- p$intercept + p$slope * t_vals
    }
  }

  # Generate noise, honouring the per-observation scale sd_vec
  if (noise == "gauss") {
    errors <- stats::rnorm(n, mean = 0, sd = sd_vec)
  } else if (noise == "t") {
    errors <- stats::rt(n, df = df) * sd_vec
  } else if (noise == "ar1") {
    errors <- numeric(n)
    errors[1] <- stats::rnorm(1, mean = 0, sd = sd_vec[1])
    for (i in 2:n) {
      errors[i] <- rho * errors[i - 1] +
        stats::rnorm(1, mean = 0, sd = sd_vec[i] * sqrt(1 - rho^2))
    }
  } else if (noise == "rw") {
    errors <- cumsum(stats::rnorm(n, mean = 0, sd = sd_vec))
  }

  series <- signal + errors

  # Build segments
  n_segs <- length(seg_starts)
  seg_tbl <- tibble::tibble(
    seg_id = seq_len(n_segs),
    start = seg_starts,
    end = seg_ends,
    param_estimate = vapply(seq_len(n_segs), function(i) {
      mean(series[seg_starts[i]:seg_ends[i]])
    }, numeric(1))
  )

  res <- tibble::tibble(
    index = seq_len(n),
    value = series,
    seg_id = rep(seq_len(n_segs), seg_ends - seg_starts + 1)
  )

  attr(res, "true_changepoints") <- changepoints
  attr(res, "true_segments") <- seg_tbl
  res
}

#' @rdname cpt_simulate
#' @param ... Passed to \code{\link{cpt_simulate}}.
#' @export
rcpt <- function(...) cpt_simulate(...)

# ---- Canonical test signals ----

#' Blocks test signal
#'
#' The classic Donoho-Johnstone blocks test signal with known changepoints.
#'
#' @param n Length of the signal. Defaults to 2048.
#' @param seed Optional seed.
#' @return A tibble with columns \code{index} and \code{value}. The \code{true_changepoints}
#'   attribute contains the known changepoint locations.
#' @export
#' @references Donoho, D. L. and Johnstone, I. M. (1994). Ideal spatial adaptation
#'   by wavelet shrinkage. \emph{Biometrika}, 81(3), 425-455.
signal_blocks <- function(n = 2048, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  t <- seq(0, 1, length.out = n)
  # Standard blocks changepoints (scaled to [0,1])
  cp_scaled <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.44, 0.65, 0.76, 0.78, 0.81)
  heights <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)

  cp_idx <- as.integer(round(cp_scaled * n))
  cp_idx <- cp_idx[cp_idx > 0 & cp_idx < n]

  signal <- rep(0, n)
  for (i in rev(seq_along(cp_idx))) {
    signal[t >= cp_scaled[i]] <- heights[i]
  }

  # Add noise
  signal <- signal + stats::rnorm(n, 0, 1)

  res <- tibble::tibble(index = seq_len(n), value = signal)
  attr(res, "true_changepoints") <- cp_idx
  res
}

#' FMS (Four-Metric-Segments) test signal
#'
#' A piecewise-constant test signal from the WBS/NOT literature.
#'
#' @param n Length of the signal. Defaults to 2000.
#' @param seed Optional seed.
#' @return A tibble with columns \code{index} and \code{value}.
#' @export
signal_fms <- function(n = 2000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  seg_means <- c(0, 1, 0, -1, 0, 0.5, -0.5, 0)
  seg_lens <- round(n * c(0.2, 0.1, 0.15, 0.1, 0.1, 0.15, 0.1, 0.1))

  # Adjust last segment to match n
  seg_lens[length(seg_lens)] <- n - sum(seg_lens[-length(seg_lens)])

  signal <- rep(seg_means, times = seg_lens)
  cp_idx <- cumsum(seg_lens)[-length(seg_lens)]

  signal <- signal + stats::rnorm(n, 0, 0.5)

  res <- tibble::tibble(index = seq_len(n), value = signal)
  attr(res, "true_changepoints") <- cp_idx
  res
}

#' Mix test signal
#'
#' A piecewise-constant/linear signal from the literature.
#'
#' @param n Length of the signal. Defaults to 2000.
#' @param seed Optional seed.
#' @return A tibble with columns \code{index} and \code{value}.
#' @export
signal_mix <- function(n = 2000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  seg_lens <- round(n * c(0.15, 0.2, 0.1, 0.2, 0.15, 0.2))
  seg_lens[length(seg_lens)] <- n - sum(seg_lens[-length(seg_lens)])

  signal <- c(
    rep(0, seg_lens[1]),
    seq(0, 2, length.out = seg_lens[2]),
    rep(2, seg_lens[3]),
    seq(2, -1, length.out = seg_lens[4]),
    rep(-1, seg_lens[5]),
    seq(-1, 1, length.out = seg_lens[6])
  )

  cp_idx <- cumsum(seg_lens)[-length(seg_lens)]
  signal <- signal + stats::rnorm(n, 0, 0.5)

  res <- tibble::tibble(index = seq_len(n), value = signal)
  attr(res, "true_changepoints") <- cp_idx
  res
}

#' Teeth test signal
#'
#' A piecewise-constant signal with regularly spaced changepoints.
#'
#' @param n Length of the signal. Defaults to 2000.
#' @param seed Optional seed.
#' @return A tibble with columns \code{index} and \code{value}.
#' @export
signal_teeth <- function(n = 2000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  teeth_width <- 100
  n_teeth <- floor(n / teeth_width)

  vals <- rep(c(0, 3), length.out = n_teeth)
  signal <- rep(vals, each = teeth_width)
  signal <- c(signal, rep(0, n - length(signal)))

  cp_idx <- seq(teeth_width, by = teeth_width, length.out = n_teeth - 1)
  signal <- signal + stats::rnorm(n, 0, 0.5)

  res <- tibble::tibble(index = seq_len(n), value = signal)
  attr(res, "true_changepoints") <- cp_idx
  res
}

#' Stairs test signal
#'
#' A monotonically stepping signal (staircase).
#'
#' @param n Length of the signal. Defaults to 2000.
#' @param seed Optional seed.
#' @return A tibble with columns \code{index} and \code{value}.
#' @export
signal_stairs <- function(n = 2000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_steps <- 10
  step_size <- n %/% n_steps
  heights <- seq(0, by = 2, length.out = n_steps)

  signal <- rep(heights, each = step_size)
  signal <- c(signal, rep(heights[length(heights)], n - length(signal)))

  cp_idx <- seq(step_size, by = step_size, length.out = n_steps - 1)
  signal <- signal + stats::rnorm(n, 0, 0.5)

  res <- tibble::tibble(index = seq_len(n), value = signal)
  attr(res, "true_changepoints") <- cp_idx
  res
}
