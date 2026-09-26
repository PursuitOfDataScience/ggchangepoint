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
#' @param x A numeric vector, or a \code{ggcpt} fit (its series, method,
#'   change type and penalty are used, and it is the original the
#'   replicates are compared with); for the \code{print()} method, a
#'   \code{ggcpt_stability} object.
#' @param method Detection method, passed to \code{\link{cpt_detect}()}.
#'   Ignored when \code{x} is a fit.
#' @param B Number of bootstrap replicates. Defaults to \code{100}.
#' @param margin Tolerance (in indices) when counting a replicate detection
#'   as a re-detection of a location. Defaults to \code{5}.
#' @param bootstrap \code{"iid"} (the default) resamples residuals one at a
#'   time within each segment; \code{"block"} resamples contiguous blocks
#'   of them, which keeps the residuals' short-range dependence, so an
#'   artifact of autocorrelated noise can move between replicates instead
#'   of being rebuilt in the same place every time.
#' @param block_length Block length for \code{bootstrap = "block"}. Defaults
#'   to \eqn{n^{1/3}} lengthened by the residuals' lag-1 autocorrelation
#'   \eqn{r} by the factor \eqn{(1 + |r|)/(1 - |r|)}, and capped at a
#'   quarter of the series.
#' @param reversal Also run the detector once on the reversed series, and
#'   flag each changepoint that the reversed series does not find within
#'   \code{margin}? Defaults to \code{TRUE}. An offline detector looks at the
#'   whole series, so a changepoint that does not survive reversal is a
#'   borderline one: measured, it is the same instability the bootstrap
#'   finds, for one extra fit. Sequential (online) detectors genuinely
#'   depend on direction and get \code{NA}.
#' @param seed Optional seed for reproducibility. The seed is scoped to this
#'   call: \code{.Random.seed} is saved and restored, so a seeded call
#'   inside a simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments passed to every \code{cpt_detect()} call.
#' @return A \code{ggcpt_stability} object: a list with \code{frequency}
#'   (a tibble of \code{index} and \code{freq}, the proportion of replicates
#'   detecting a changepoint within \code{margin} of that index),
#'   \code{original} (the point-estimate \code{ggcpt}), \code{B},
#'   \code{n_failed} (replicates on which the detector failed, which are
#'   left out of the proportion rather than counted as "found nothing"),
#'   \code{bootstrap}, \code{block_length} and \code{reversal} (one row per
#'   original changepoint: \code{cp} and \code{survives}).
#'   Methods: \code{print()} and \code{autoplot()} (frequency profile with
#'   the original detections marked).
#'
#' @section What a high score means:
#' \emph{Reproducible under resampling}, which is not the same as
#' \emph{real}, and under dependent noise the two diverge: measured under
#' AR(1) noise, a spurious changepoint reached a stability of 1.00 and the
#' score separated real from spurious changepoints in only 4 of 10
#' replicates. The artifact is a feature of the realised noise path, and an
#' i.i.d. resample of the residuals rebuilds it every time.
#' \code{bootstrap = "block"} lets it vary; \code{\link{cpt_robustness}()},
#' which changes the noise model rather than the sample, is the check that
#' touches the cause; and \code{\link{cpt_assumptions}()} tests the
#' residuals directly.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 4))
#' st <- cpt_stability(x, method = "pelt", B = 20)
#' st
#' ggplot2::autoplot(st)
cpt_stability <- function(x, method = "pelt", B = 100, margin = 5,
                          seed = NULL, bootstrap = c("iid", "block"),
                          block_length = NULL, reversal = TRUE, ...) {
  validate_scalar(B, "B", min = 1)
  validate_scalar(margin, "margin", min = 0)
  bootstrap <- cpt_match_arg(bootstrap)
  validate_flag(reversal, "reversal")
  if (!is.null(block_length)) {
    validate_scalar(block_length, "block_length", min = 1)
  }
  dots <- list(...)
  if (is_ggcpt(x)) {
    # The fit already made: its series, method, change type and penalty,
    # which is what every other post-detection tool takes. This function
    # used to refuse a fit with "`x` must be a numeric vector, matrix, or
    # data.frame", naming neither what arrived nor the fix.
    if (!bootstrap_possible(x) || !rerun_matches_result(x)) {
      cpt_abort("`cpt_stability()` re-runs the detector, and this result ",
                "cannot be re-run from the object (",
                if (!bootstrap_possible(x)) "its method is not one cpt_detect() knows"
                else "it was fitted from a formula", ").",
                class = "capability_absent")
    }
    refuse_resampling_family(x, "cpt_stability()")
    method <- scalar_chr(x$method)
    dots <- rerun_dots(x, dots, "cpt_stability()")
    if (is.null(dots$family) && !is.null(x$family)) dots$family <- x$family
    original <- x
    data_vec <- x$data$value
  } else {
    validate_data(x)
    data_vec <- as_uni_vector(x, method)
    original <- NULL
  }
  n <- length(data_vec)

  local_seed(seed)

  detect <- function(v) do.call(cpt_detect, c(list(v, method = method), dots))
  original <- original %||% detect(data_vec)

  seg <- original$segments
  fitted_step <- rep(seg$param_estimate, times = seg$n)
  resid <- data_vec - fitted_step
  seg_id <- rep(seq_len(nrow(seg)), times = seg$n)
  if (bootstrap == "block" && is.null(block_length)) {
    r1 <- tryCatch(stats::acf(resid, lag.max = 1, plot = FALSE)$acf[2],
                   error = function(e) 0)
    if (!is.finite(r1)) r1 <- 0
    r1 <- min(abs(r1), 0.95)
    block_length <- ceiling(n^(1 / 3) * (1 + r1) / (1 - r1))
  }
  if (bootstrap == "block") {
    block_length <- as.integer(max(1, min(block_length, floor(n / 4))))
  }

  hits <- numeric(n)
  n_failed <- 0L
  first_error <- NULL
  for (b in seq_len(B)) {
    resampled <- resid
    for (s in seq_len(nrow(seg))) {
      idx <- which(seg_id == s)
      # `sample.int()` on the index, not `sample()` on the values: R's
      # classic pitfall is that `sample(x, n)` means `sample.int(x, n)` when
      # `x` is a single number >= 1, so a one-observation segment resamples
      # `1:round(resid)` instead of the residual itself. It is currently
      # safe only by accident -- a length-1 segment's residual against its
      # own mean is exactly 0, and `0 >= 1` is FALSE -- which couples this
      # bootstrap to `param_estimate` staying the exact segment mean.
      resampled[idx] <- if (bootstrap == "block" && length(idx) > block_length) {
        resid[idx][block_indices(length(idx), block_length)]
      } else {
        resid[idx][sample.int(length(idx), length(idx), replace = TRUE)]
      }
    }
    rep_series <- fitted_step + resampled
    # A replicate the detector FAILED on is not one on which it found
    # nothing: counting it as integer(0) diluted every frequency by the
    # failure rate and said nothing about it.
    rep_cp <- tryCatch(detect(rep_series)$changepoints$cp,
                       error = function(e) {
                         n_failed <<- n_failed + 1L
                         if (is.null(first_error)) {
                           first_error <<- conditionMessage(e)
                         }
                         NULL
                       })
    if (is.null(rep_cp)) next
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
  n_ok <- B - n_failed
  if (n_ok == 0L) {
    cpt_abort("`", method, "` failed on all ", B, " bootstrap replicates, so ",
              "there is no stability to report. The first error was: ",
              first_error, class = "engine_error",
              data = list(method = method))
  }
  if (n_failed > 0L) {
    cpt_warn("`", method, "` failed on ", n_failed, " of ", B, " bootstrap ",
             "replicates; the frequencies are over the ", n_ok, " that ",
             "ran. The first error was: ", first_error,
             class = "replicates_failed",
             data = list(n_failed = n_failed, B = B))
  }

  rev_tab <- NULL
  cps <- original$changepoints$cp
  if (reversal && length(cps)) {
    reg <- builtin_registry()
    sequential <- isTRUE(reg$online[match(method, reg$method)])
    survives <- if (sequential) {
      rep(NA, length(cps))
    } else {
      rev_cp <- tryCatch(detect(rev(data_vec))$changepoints$cp,
                         error = function(e) NULL)
      if (is.null(rev_cp)) rep(NA, length(cps)) else {
        mapped <- n - rev_cp
        vapply(cps, function(cp) any(abs(mapped - cp) <= margin), logical(1))
      }
    }
    rev_tab <- tibble::tibble(cp = cps, survives = survives)
  }

  structure(
    list(
      # hits is bounded by the number of replicates that ran, so freq is a
      # genuine proportion in [0, 1] and needs no clipping.
      frequency = tibble::tibble(index = seq_len(n),
                                 freq = hits / n_ok),
      original = original,
      B = B,
      n_failed = n_failed,
      margin = margin,
      method = method,
      bootstrap = bootstrap,
      block_length = if (bootstrap == "block") block_length,
      reversal = rev_tab
    ),
    class = "ggcpt_stability"
  )
}

# Internal: a moving-block bootstrap of 1..m with blocks of length l.
#' @noRd
block_indices <- function(m, l) {
  starts <- sample.int(m - l + 1L, ceiling(m / l), replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + l - 1L)))
  idx[seq_len(m)]
}

# Internal: the residual bootstraps add resampled residuals to the fitted
# levels, which a count, binary or waiting-time series cannot absorb:
# counts stop being counts, and the family's own refusal follows on every
# replicate.
#' @noRd
refuse_resampling_family <- function(object, what) {
  fam <- object$family
  if (!is.null(fam) && fam %in% c("poisson", "binomial", "exponential",
                                  "gamma")) {
    cpt_abort("`", what, "` perturbs the series with resampled residuals, ",
              "and a ", fam, " series cannot absorb them (the replicates ",
              "would no longer be ", switch(fam, poisson = "counts",
                                           binomial = "0/1 outcomes",
                                           "positive"), "). For a ", fam,
              " fit use cpt_confint(method = \"native\") where the engine ",
              "gives one, or cpt_effect(method = \"split\").",
              class = "capability_absent",
              data = list(family = fam))
  }
  invisible(TRUE)
}

#' @rdname cpt_stability
#' @param object A \code{ggcpt_stability} object (for \code{autoplot()}).
#' @export
print.ggcpt_stability <- function(x, ...) {
  cat("ggcpt_stability (", x$B, " ", x$bootstrap %||% "iid",
      if (!is.null(x$block_length)) paste0(" (block length ",
                                           x$block_length, ")"),
      " bootstrap replicates, method: ", x$method, ")\n", sep = "")
  if (isTRUE(x$n_failed > 0)) {
    cat("  ", x$n_failed, " replicate(s) failed and are left out.\n",
        sep = "")
  }
  cp <- x$original$changepoints$cp
  if (length(cp) > 0) {
    freq_at <- x$frequency$freq[cp]
    cat("\nOriginal changepoints and their re-detection frequency:\n")
    tab <- tibble::tibble(cp = cp, stability = freq_at)
    if (!is.null(x$reversal)) tab$survives_reversal <- x$reversal$survives
    print(tab)
    cat("\nA high score means reproducible under resampling, not real; ",
        "see ?cpt_stability.\n", sep = "")
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

  # This is the one plot in the package drawn against series position that
  # did not honour a time index, so a dated series came back in positions
  # here while autoplot(fit), ggcpt_statistic(), ggcpt_scale_space(),
  # ggcpt_solution_path() and the influence and events plots all showed
  # dates. plot_index() returns positions when there is no index, so an
  # unindexed result is unchanged.
  idx_vals <- plot_index(object$original)
  d <- tibble::tibble(x = idx_vals[freq$index], freq = freq$freq)

  p <- ggplot2::ggplot(d, ggplot2::aes(x, freq)) +
    ggplot2::geom_area(fill = "steelblue", alpha = 0.4) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = plot_index_label(object$original),
                  y = "Detection frequency",
                  title = paste0("Changepoint stability (", object$B,
                                 " bootstrap replicates)"))

  if (length(cp) > 0) {
    p <- p + ggplot2::geom_vline(xintercept = idx_vals[cp], color = "blue",
                                 linetype = "dashed", linewidth = 0.4)
  }
  p
}
