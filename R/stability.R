#' Changepoint stability diagnostics via bootstrap
#'
#' Most engines report a point set of changepoints with no measure of how
#' fragile it is. \code{cpt_stability()} fits the detector once, then
#' resamples residuals \emph{within} the fitted segments (so the estimated
#' regime structure is preserved), re-runs the detector on each replicate,
#' and reports how often each location is re-detected. The resulting
#' detection-frequency profile is a cheap, model-agnostic confidence signal
#' available for every wrapped engine, including the many that ship no
#' confidence intervals.
#'
#' @param x For \code{cpt_stability()}, a numeric vector; for the
#'   \code{print()} method, a \code{ggcpt_stability} object.
#' @param method Detection method, passed to \code{\link{cpt_detect}()}.
#' @param B Number of bootstrap replicates. Defaults to \code{100}.
#' @param margin Tolerance (in indices) when counting a replicate detection
#'   as a re-detection of a location. Defaults to \code{5}.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to every \code{cpt_detect()} call.
#' @return A \code{ggcpt_stability} object: a list with \code{frequency}
#'   (a tibble of \code{index} and \code{freq}, the proportion of replicates
#'   detecting a changepoint within \code{margin} of that index),
#'   \code{original} (the point-estimate \code{ggcpt}), and \code{B}.
#'   Methods: \code{print()} and \code{autoplot()} (frequency profile with
#'   the original detections marked).
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 4))
#' st <- cpt_stability(x, method = "pelt", B = 20)
#' st
#' ggplot2::autoplot(st)
cpt_stability <- function(x, method = "pelt", B = 100, margin = 5,
                          seed = NULL, ...) {
  validate_data(x)
  validate_scalar(B, "B", min = 1)
  validate_scalar(margin, "margin", min = 0)
  data_vec <- as_uni_vector(x, method)
  n <- length(data_vec)

  if (!is.null(seed)) set.seed(seed)

  original <- cpt_detect(data_vec, method = method, ...)

  seg <- original$segments
  fitted_step <- rep(seg$param_estimate, times = seg$n)
  resid <- data_vec - fitted_step
  seg_id <- rep(seq_len(nrow(seg)), times = seg$n)

  hits <- numeric(n)
  for (b in seq_len(B)) {
    resampled <- resid
    for (s in seq_len(nrow(seg))) {
      idx <- which(seg_id == s)
      resampled[idx] <- sample(resid[idx], length(idx), replace = TRUE)
    }
    rep_series <- fitted_step + resampled
    rep_cp <- tryCatch(
      cpt_detect(rep_series, method = method, ...)$changepoints$cp,
      error = function(e) integer(0)
    )
    # Each replicate contributes at most 1 to any index: mark the covered
    # window first, then add the mask. Incrementing once per changepoint
    # instead counts a replicate twice wherever two detections' windows
    # overlap, which the old `pmin(hits / B, 1)` then hid by clipping --
    # reporting 1.00 ("re-detected every time") for indices that only half
    # the replicates actually covered.
    covered <- logical(n)
    for (cp in rep_cp) {
      lo <- max(1, cp - margin)
      hi <- min(n, cp + margin)
      covered[lo:hi] <- TRUE
    }
    hits <- hits + covered
  }

  structure(
    list(
      # hits is now bounded by B by construction, so freq is a genuine
      # proportion in [0, 1] and needs no clipping.
      frequency = tibble::tibble(index = seq_len(n),
                                 freq = hits / B),
      original = original,
      B = B,
      margin = margin,
      method = method
    ),
    class = "ggcpt_stability"
  )
}

#' @rdname cpt_stability
#' @param object A \code{ggcpt_stability} object (for \code{autoplot()}).
#' @export
print.ggcpt_stability <- function(x, ...) {
  cat("ggcpt_stability (", x$B, " bootstrap replicates, method: ",
      x$method, ")\n", sep = "")
  cp <- x$original$changepoints$cp
  if (length(cp) > 0) {
    freq_at <- x$frequency$freq[cp]
    cat("\nOriginal changepoints and their re-detection frequency:\n")
    print(tibble::tibble(cp = cp, stability = freq_at))
  } else {
    cat("\nNo changepoints detected in the original fit.\n")
  }
  invisible(x)
}

#' @rdname cpt_stability
#' @export
autoplot.ggcpt_stability <- function(object, ...) {
  freq <- object$frequency
  cp <- object$original$changepoints$cp

  p <- ggplot2::ggplot(freq, ggplot2::aes(index, freq)) +
    ggplot2::geom_area(fill = "steelblue", alpha = 0.4) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Index", y = "Detection frequency",
                  title = paste0("Changepoint stability (", object$B,
                                 " bootstrap replicates)"))

  if (length(cp) > 0) {
    p <- p + ggplot2::geom_vline(xintercept = cp, color = "blue",
                                 linetype = "dashed", linewidth = 0.4)
  }
  p
}
