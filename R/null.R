# ---------------------------------------------------------------------------
# "No changepoints detected" is not an answer.
#
# An empty result means one of two things that call for opposite actions:
# the series is stable, or the analysis had no power to see a change of the
# size that matters. The package knew enough to tell them apart
# (cpt_min_detectable() answers the second question) and said neither. It
# also offered no test of the question an empty answer raises, "is there a
# change anywhere at all?", which is better posed than any test of a
# located change because nothing was selected.
# ---------------------------------------------------------------------------

#' Test for a change anywhere in a series
#'
#' A test of the global null hypothesis "the series has no changepoint",
#' the question an empty detection raises. Nothing is located first, so
#' nothing is selected, and the p-value means what it says.
#'
#' @param x A numeric series, or a \code{ggcpt} fit (its series is used).
#' @param method Which test:
#'   \describe{
#'     \item{\code{"cusum"}}{(the default) the CUSUM test for a change in
#'       mean: the largest standardised cumulative-sum deviation, referred to
#'       the Kolmogorov distribution of the supremum of a Brownian bridge.
#'       The noise scale is estimated from first differences, which a change
#'       barely affects.}
#'     \item{\code{"pettitt"}}{Pettitt's rank test, distribution-free, for a
#'       shift in location (approximate p-value).}
#'     \item{\code{"supF"}}{Andrews' sup-F structural change test for a
#'       change in mean, with 15\% trimming, through \pkg{strucchange}.}
#'   }
#' @param index Optional time index for a bare series.
#' @return A tibble with one row: \code{method}, \code{statistic},
#'   \code{p_value}, \code{location} (where the statistic peaks: the most
#'   likely changepoint, reported for orientation only), \code{n} and
#'   \code{selection_adjusted} (\code{TRUE}).
#' @section Assumptions:
#' All three assume independent observations. Under positive
#' autocorrelation they reject too often, which is the same failure the
#' detectors share; see \code{\link{cpt_assumptions}()}.
#' @seealso \code{\link{cpt_null_power}()} for what an empty answer could
#'   have detected, \code{\link{cpt_test_at}()} for a pre-specified date.
#' @export
#' @family inference
#' @examplesIf requireNamespace("strucchange", quietly = TRUE)
#' set.seed(1)
#' cpt_test_null(rnorm(200))
#' shifted <- c(rnorm(100), rnorm(100, 0.6))
#' cpt_test_null(shifted, method = "pettitt")
#' cpt_test_null(shifted, method = "supF")
cpt_test_null <- function(x, method = c("cusum", "pettitt", "supF"),
                          index = NULL) {
  method <- cpt_match_arg(method)
  if (is_ggcpt(x)) {
    if (n_coordinates(x) > 1L) {
      cpt_abort("`cpt_test_null()` tests one series; this fit has ",
                n_coordinates(x), " coordinates.",
                class = "wrong_dimension")
    }
    v <- x$data$value
    idx <- x$index
  } else {
    s <- as_cpt_series(x, index = index)
    if (is.matrix(s$values) || is.data.frame(s$values)) {
      cpt_abort("`cpt_test_null()` tests one series; `x` has several ",
                "columns.", class = "wrong_dimension")
    }
    v <- as.numeric(s$values)
    idx <- s$index
  }
  validate_data(v)
  n <- length(v)
  res <- switch(method,
    cusum = cusum_test(v),
    pettitt = pettitt_test(v),
    supF = supf_test(v)
  )
  out <- tibble::tibble(method = res$method, statistic = res$statistic,
                        p_value = res$p_value, location = res$location,
                        n = n, selection_adjusted = TRUE)
  if (!is.null(idx) && is.finite(res$location)) {
    out$location_index <- idx[res$location]
  }
  out
}

#' @noRd
cusum_test <- function(v) {
  n <- length(v)
  sigma <- noise_sd(v)
  s <- cumsum(v - mean(v))[-n]
  stat_path <- abs(s) / (sigma * sqrt(n))
  k <- which.max(stat_path)
  stat <- if (is.finite(sigma) && sigma > 0) stat_path[k] else Inf
  list(method = "CUSUM (Kolmogorov limit)", statistic = stat,
       p_value = kolmogorov_tail(stat), location = k)
}

# Internal: P(sup |Brownian bridge| > x), the Kolmogorov distribution's
# upper tail.
#' @noRd
kolmogorov_tail <- function(x) {
  if (!is.finite(x)) return(0)
  if (x <= 0) return(1)
  j <- seq_len(100)
  p <- 2 * sum((-1)^(j - 1) * exp(-2 * j^2 * x^2))
  min(1, max(0, p))
}

#' @noRd
pettitt_test <- function(v) {
  n <- length(v)
  r <- rank(v)
  u <- 2 * cumsum(r) - seq_len(n) * (n + 1)
  u <- u[-n]
  k <- which.max(abs(u))
  stat <- abs(u[k])
  p <- min(1, 2 * exp(-6 * stat^2 / (n^3 + n^2)))
  list(method = "Pettitt rank test", statistic = stat, p_value = p,
       location = k)
}

#' @noRd
supf_test <- function(v) {
  need_pkg("strucchange")
  d <- data.frame(.y = v)
  fs <- strucchange::Fstats(.y ~ 1, data = d, from = 0.15)
  tst <- strucchange::sctest(fs, type = "supF")
  loc <- fs$breakpoint
  list(method = "sup-F (Andrews, 15% trimming)",
       statistic = unname(tst$statistic), p_value = unname(tst$p.value),
       location = if (length(loc) == 1L && is.finite(loc)) as.integer(loc)
       else NA_integer_)
}

#' What an empty answer could have detected
#'
#' When a detection comes back empty, the useful question is whether a
#' change of the size you care about would have been found. This runs
#' \code{\link{cpt_min_detectable}()} at the fit's own length, method and
#' penalty, with the noise level estimated from the series, and reports the
#' smallest shift detected with the requested power: changes smaller than
#' that could well be there and missed.
#'
#' @param fit A \code{ggcpt} object (usually one with no changepoints; it
#'   works for any).
#' @param power Target power. Defaults to \code{0.8}.
#' @param n_sim Simulations per power evaluation. Defaults to \code{50}.
#' @param seed Optional seed, scoped to this call.
#' @param ... Further arguments for \code{\link{cpt_min_detectable}()}.
#' @return A \code{ggcpt_null_power} list with \code{n}, \code{sigma} (the
#'   noise level used), \code{jump} (the smallest detectable shift, in the
#'   data's units), \code{jump_sd} (the same in noise standard deviations),
#'   \code{power}, \code{method} and \code{constant} (\code{TRUE} when the
#'   series has no variation, in which case nothing is simulated). With a
#'   \code{print()} method.
#' @export
#' @family inference
#' @examples
#' \donttest{
#' set.seed(1)
#' fit <- cpt_detect(rnorm(100), method = "pelt")
#' cpt_null_power(fit, n_sim = 20, seed = 1)
#' }
cpt_null_power <- function(fit, power = 0.8, n_sim = 50, seed = NULL, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  v <- fit$data$value
  n <- length(v)
  method <- scalar_chr(fit$method)
  out <- list(n = n, sigma = NA_real_, jump = NA_real_, jump_sd = NA_real_,
              power = power, method = method, constant = is_constant(v))
  if (out$constant) {
    return(structure(out, class = "ggcpt_null_power"))
  }
  if (!bootstrap_possible(fit)) {
    cpt_abort("`cpt_null_power()` simulates the detector, and `", method,
              "` is not a method cpt_detect() knows.",
              class = "capability_absent")
  }
  sigma <- noise_sd(v)
  dots <- list(...)
  if (is.null(dots$penalty)) dots$penalty <- rerun_penalty(fit)
  ci <- rerun_change_in(fit) %||% "mean"
  if (!ci %in% c("mean", "var", "meanvar", "slope")) ci <- "mean"
  md <- do.call(cpt_min_detectable,
                c(list(n = n, sigma = sigma, method = method, power = power,
                       n_sim = n_sim, change_in = ci, seed = seed), dots))
  # cpt_min_detectable() works in units of `sigma`.
  out$sigma <- sigma
  out$jump_sd <- md$jump
  out$jump <- md$jump * sigma
  out$note <- md$note
  structure(out, class = "ggcpt_null_power")
}

#' @rdname cpt_null_power
#' @param x A \code{ggcpt_null_power} object.
#' @export
print.ggcpt_null_power <- function(x, ...) {
  cat("ggcpt_null_power (method: ", x$method, ", n = ", x$n, ")\n", sep = "")
  if (isTRUE(x$constant)) {
    cat("The series is constant: there is no variation, so there is nothing",
        "to detect and\nno power to compute.\n")
    return(invisible(x))
  }
  if (is.na(x$jump)) {
    cat("No shift in the range searched reached power ", x$power, ": ",
        x$note %||% "", "\n", sep = "")
    return(invisible(x))
  }
  cat("At this length and noise level (sd ", format(signif(x$sigma, 3)),
      "), a single shift of about ", format(signif(x$jump, 3)), " (",
      format(signif(x$jump_sd, 3)), " sd) is\ndetected with power ",
      x$power, ". A smaller change could be present and missed.\n", sep = "")
  invisible(x)
}
