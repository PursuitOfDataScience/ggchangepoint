# ---------------------------------------------------------------------------
# The assumption report.
#
# Twelve passes of measurement found no broken estimator in the engines;
# every failure was a violated assumption the package did not mention. Under
# AR(1) noise sixteen of forty-two engines report more than five spurious
# changepoints, `pelt` on data multiplied by ten reports 139, and a
# sequential test run offline reports n / arl0 by construction. The check
# that the independence assumption fails has 100% power at n = 300; every
# automatic correction measured costs about one true changepoint for every
# one or two false ones it removes. So the package reports, names the
# alternatives with what they cost, and stops: which side of that trade a
# user wants is not something it can decide for them.
# ---------------------------------------------------------------------------

# Internal: residual dependence of a segmentation. Ljung-Box on the
# within-segment residuals (lag 10, or n / 5 for a short series) and their
# lag-1 autocorrelation. Measured (§201): the check rejects in 48 of 48
# replicate-engine combinations under AR(1), rho = 0.7, even for an engine
# that fitted fourteen changepoints into the dependence.
#' @noRd
residual_dependence <- function(value, cps, fitted = NULL, lag = NULL) {
  n <- length(value)
  if (n < 12L) return(NULL)
  if (is.null(fitted) || length(fitted) != n) {
    fitted <- rep(NA_real_, n)
    bounds <- c(0L, sort(cps), n)
    for (s in seq_len(length(bounds) - 1L)) {
      rows <- (bounds[s] + 1L):bounds[s + 1L]
      fitted[rows] <- mean(value[rows], na.rm = TRUE)
    }
  }
  r <- value - fitted
  r <- r[is.finite(r)]
  if (length(r) < 12L || stats::sd(r) == 0) return(NULL)
  lag <- lag %||% max(1L, min(10L, floor(length(r) / 5)))
  lb <- tryCatch(stats::Box.test(r, lag = lag, type = "Ljung-Box"),
                 error = function(e) NULL)
  if (is.null(lb)) return(NULL)
  acf1 <- tryCatch(stats::acf(r, lag.max = 1, plot = FALSE)$acf[2],
                   error = function(e) NA_real_)
  list(p_value = unname(lb$p.value), statistic = unname(lb$statistic),
       lag = lag, acf1 = acf1)
}

# Internal: the argument of each engine that selects its noise or error
# model (§189), read from the wrappers' formals (a test keeps it in step).
# The fix for a violated assumption is often one of these, on the engine
# already chosen.
#' @noRd
noise_model_args <- function() {
  c(pelt = "change_in", binseg = "change_in", segneigh = "change_in",
    amoc = "change_in", smuce = "family", hsmuce = "family",
    nsp = "variant", fastcpd = "family", fmean = "robust",
    envcpt = "models", cpm = "cpm_type", decafs = "model_param",
    kcp = "running_stat")
}

# Internal: the argument that governs each engine's false-alarm rate
# (§210.2), and whether its default should grow with the series (a rate per
# observation does; a global level does not).
#' @noRd
rate_args <- function() {
  tibble::tribble(
    ~method,    ~rate_arg,        ~scales_with_n,
    "cpm",      "arl0",           TRUE,
    "nsp",      "alpha",          FALSE,
    "smuce",    "alpha",          FALSE,
    "hsmuce",   "alpha",          FALSE,
    "ocd",      "patience",       TRUE,
    "pettitt",  "alpha",          FALSE,
    "buishand", "alpha",          FALSE,
    "snht",     "alpha",          FALSE,
    "kcp",      "alpha",          FALSE,
    "hdcov",    "alpha",          FALSE,
    "network",  "alpha",          FALSE,
    "fmean",    "alpha",          FALSE,
    "fcov",     "alpha",          FALSE,
    "bcp",      "prob_threshold", FALSE,
    "beast",    "prob_threshold", FALSE,
    "bocpd",    "hazard",         FALSE
  )
}

# Internal: how many changepoints an engine can return, where that is fixed
# by construction (§184.3): the single-change designs, and the two whose
# count is an argument with a default of one.
#' @noRd
max_cp_table <- function() {
  c(amoc = 1L, pettitt = 1L, buishand = 1L, snht = 1L, segmented = 1L,
    mcp = 1L)
}

# Internal: the workhorses (§186.2): mature, fast, widely cited, correct on
# the common case. A judgement, recorded as one, which the recommender uses
# to break ties instead of sorting by name.
#' @noRd
general_purpose_methods <- function() {
  c("pelt", "binseg", "fpop", "wbs", "wbs2", "not", "mosum")
}

# Internal: a measured table shipped with the package, or NULL before it
# exists.
#' @noRd
measured_data <- function(name) {
  tryCatch(getExportedValue("ggchangepoint", name), error = function(e) NULL)
}

# Internal: is this engine's answer independent of the data's units, as
# measured by the invariance probe? NA when unmeasured.
#' @noRd
scale_invariant <- function(method) {
  inv <- measured_data("cpt_invariances")
  if (is.null(inv) || !method %in% inv$method) return(NA)
  isTRUE(inv$scale_invariant[match(method, inv$method)])
}

# Internal: the one warning §179.3 asked for. A scale-sensitive engine
# (measured) given a series whose noise is far from unit scale answers a
# different question than it appears to: `pelt` returns 139 changepoints on
# data multiplied by ten, and none on data divided by ten.
#' @noRd
warn_scale_sensitive <- function(x, method, change_in) {
  if (!identical(change_in, "mean")) return(invisible(FALSE))
  inv <- scale_invariant(method)
  if (!identical(inv, FALSE)) return(invisible(FALSE))
  v <- if (is.matrix(x) || is.data.frame(x)) as.numeric(as.matrix(x)[, 1]) else
    as.numeric(x)
  s <- noise_sd(v)
  if (!is.finite(s) || s == 0 || (s > 0.5 && s < 2)) return(invisible(FALSE))
  cpt_warn("`", method, "` is scale-sensitive for a change in mean (its cost ",
           "assumes unit noise), and this series' noise standard deviation ",
           "is about ", format(signif(s, 2)), ". On data ten times larger ",
           "it over-segments badly; on data ten times smaller it finds ",
           "nothing. Standardise the series first (e.g. `x / ",
           format(signif(s, 2)), "`), or use `change_in = \"meanvar\"`, which ",
           "estimates the noise per segment.", class = "scale_sensitive",
           data = list(method = method, noise_sd = s))
  invisible(TRUE)
}

# Internal: the dependence-aware alternatives, with what they cost, from the
# noise benchmark when it has been measured and from §213's measurement
# otherwise.
#' @noRd
dependence_alternatives <- function(method) {
  bench <- measured_data("cpt_noise_benchmark")
  if (!is.null(bench)) {
    ar <- bench[bench$regime == "ar1", , drop = FALSE]
    ar <- ar[is.finite(ar$fp) & is.finite(ar$hits), , drop = FALSE]
    best <- ar[ar$fp <= 1, , drop = FALSE]
    best <- best[order(-best$hits, best$fp), , drop = FALSE]
    best <- utils::head(best, 3)
    if (nrow(best)) {
      return(paste0(best$call, " (", format(round(best$fp, 2)),
                    " spurious, ", format(round(best$hits, 2)),
                    " of 2 real changes found)", collapse = "; "))
    }
  }
  paste0("decafs and envcpt model autocorrelated noise (measured at ",
         "n = 500, AR(1) rho = 0.7: no spurious changepoints on pure noise, ",
         "at the cost of finding 0.67 and 1.08 of 2 real changes where pelt ",
         "finds 1.75)")
}

#' Check the assumptions behind a segmentation
#'
#' A short report on whether the result's assumptions look violated for
#' this series, what that does to the answer, and what would model it. It
#' reports and does not act: every automatic correction measured trades
#' true changepoints for false ones, and which side of that trade matters is
#' the analyst's decision.
#'
#' @param fit A \code{ggcpt} object.
#' @param lag Lag for the Ljung-Box test. Defaults to \code{10}, or
#'   \code{n / 5} for a short series.
#' @return A \code{ggcpt_assumptions} tibble with one row per check:
#'   \code{component}, \code{value}, \code{flag} (\code{TRUE} when the check
#'   raises a concern), \code{detail} and \code{advice}. The components:
#'   \describe{
#'     \item{\code{residual_dependence}}{Ljung-Box p-value on the
#'       within-segment residuals (and their lag-1 autocorrelation in
#'       \code{detail}). Flagged below 0.05. Autocorrelated noise is the
#'       commonest reason for spurious changepoints.}
#'     \item{\code{scale_sensitivity}}{the noise standard deviation, flagged
#'       when the engine's answer depends on the data's units (measured) and
#'       the noise is far from unit scale.}
#'     \item{\code{expected_false_positives}}{how many changepoints the
#'       engine is expected to report on this much pure noise: \code{n /
#'       arl0} for \code{cpm}, and the measured null-size table otherwise.
#'       Flagged above one.}
#'     \item{\code{count_plausibility}}{reported changepoints per hundred
#'       observations, flagged above one: more than \code{n / 100} is more
#'       often misconfiguration than a finding.}
#'     \item{\code{data_type}}{what the series looks like (continuous,
#'       counts, binary, proportions), flagged when a Gaussian cost is fitted
#'       to non-Gaussian data.}
#'   }
#' @seealso \code{\link{cpt_report}()}, which prints this;
#'   \code{\link{cpt_robustness}()} for the answer under other noise models.
#' @export
#' @family inference
#' @examples
#' set.seed(1)
#' ar <- as.numeric(arima.sim(list(ar = 0.7), 300))
#' cpt_assumptions(cpt_detect(ar, method = "pelt"))
cpt_assumptions <- function(fit, lag = NULL) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  if (!is.null(lag)) validate_scalar(lag, "lag", min = 1)
  v <- fit$data$value
  n <- length(v)
  k <- nrow(fit$changepoints)
  method <- scalar_chr(fit$method)
  rows <- list()
  add <- function(component, value, flag, detail, advice = NULL) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      component = component, value = as.numeric(value),
      flag = as.logical(flag), detail = detail,
      advice = advice %||% NA_character_)
  }

  dep <- residual_dependence(v, fit$changepoints$cp,
                             fitted = fit$data[["fitted"]], lag = lag)
  if (is.null(dep)) {
    add("residual_dependence", NA_real_, NA,
        "too few residuals to test")
  } else {
    flagged <- dep$p_value < 0.05
    add("residual_dependence", dep$p_value, flagged,
        paste0("Ljung-Box at lag ", dep$lag, "; lag-1 autocorrelation ",
               format(round(dep$acf1, 2))),
        if (flagged) paste0("The residuals are autocorrelated, the ",
                            "commonest cause of spurious changepoints. ",
                            dependence_alternatives(method)))
  }

  s <- noise_sd(v)
  inv <- scale_invariant(method)
  sens <- identical(inv, FALSE) && identical(scalar_chr(fit$change_in),
                                               "mean")
  far <- is.finite(s) && s > 0 && (s <= 0.5 || s >= 2)
  add("scale_sensitivity", s, sens && far,
      paste0("noise sd ", format(signif(s, 3)), "; ",
             if (is.na(inv)) "scale behaviour of this engine not measured"
             else if (sens) "this engine's cost assumes unit noise"
             else "this engine's answer does not depend on the units"),
      if (sens && far) "Standardise the series, or use change_in = \"meanvar\".")

  efp <- expected_false_positives(fit)
  add("expected_false_positives", efp$value, isTRUE(efp$value > 1),
      efp$detail,
      if (isTRUE(efp$value > 1)) efp$advice)

  rate <- k / max(1, n / 100)
  add("count_plausibility", rate, rate > 1 && k >= 3,
      paste0(k, " changepoints in ", n, " observations (",
             format(round(100 * k / n, 2)), " per hundred)"),
      if (rate > 1 && k >= 3) {
        paste0("More than one changepoint per hundred observations is ",
               "more often a penalty on the wrong scale or dependent noise ",
               "than a finding; check the two rows above.")
      })

  type <- detect_data_type(v)
  fam <- fit$family %||% "gaussian"
  gauss_misfit <- type %in% c("binary", "counts", "proportion") &&
    fam == "gaussian" && !method %in% distribution_free_methods()
  add("data_type", NA_real_, gauss_misfit,
      paste0("looks ", type, "; family ", fam),
      if (gauss_misfit) {
        paste0("Refit with family = \"",
               if (type == "counts") "poisson" else "binomial",
               "\" (see cpt_families()) or a distribution-free method.")
      })

  out <- do.call(rbind, rows)
  structure(out, class = c("ggcpt_assumptions", class(tibble::tibble())),
            method = method)
}

# Internal: the expected number of false positives for this fit's engine at
# this length, and where the number comes from.
#' @noRd
expected_false_positives <- function(fit) {
  n <- nrow(fit$data)
  method <- scalar_chr(fit$method)
  efp <- fit$diagnostics$expected_false_positives
  if (is.numeric(efp) && length(efp) == 1L) {
    return(list(value = efp,
                detail = paste0("n / arl0 = ", format(signif(efp, 3)),
                                " for a sequential test run over the series"),
                advice = "Raise `arl0` (cpm's default now scales with n)."))
  }
  if (identical(method, "nsp")) {
    a <- fit$penalty$value
    if (is.numeric(a) && is.finite(a)) {
      return(list(value = a, detail = paste0(
        "at most one false region with probability ", format(a),
        " (NSP's global level)"), advice = NA_character_))
    }
  }
  tab <- measured_data("cpt_null_sizes")
  if (!is.null(tab) && method %in% tab$method) {
    t <- tab[tab$method == method, , drop = FALSE]
    j <- which.min(abs(log(t$n) - log(n)))
    return(list(value = t$mean_fp[j],
                detail = paste0("measured on pure noise at n = ",
                                format(t$n[j], big.mark = ","), " (",
                                t$reps[j], " replicates)"),
                advice = paste0("This engine reports changepoints on pure ",
                                "noise at this scale; see ",
                                "cpt_null_sizes.")))
  }
  list(value = NA_real_, detail = "not measured for this engine",
       advice = NA_character_)
}

#' @rdname cpt_assumptions
#' @param x A \code{ggcpt_assumptions} object.
#' @param ... Ignored.
#' @export
print.ggcpt_assumptions <- function(x, ...) {
  cat("ggcpt_assumptions (method: ", attr(x, "method") %||% "?", ")\n",
      sep = "")
  for (i in seq_len(nrow(x))) {
    mark <- if (isTRUE(x$flag[i])) "!" else if (is.na(x$flag[i])) "?" else
      " "
    cat(" ", mark, " ", formatC(x$component[i], width = -25), " ",
        x$detail[i], "\n", sep = "")
    if (isTRUE(x$flag[i]) && !is.na(x$advice[i])) {
      cat(paste0("      ", strwrap(x$advice[i], width = 70), collapse = "\n"),
          "\n", sep = "")
    }
  }
  if (!any(x$flag %in% TRUE)) {
    cat("\nNo assumption check raised a concern.\n")
  }
  invisible(x)
}

# Internal: the registry columns that come from measurement rather than
# construction (§214.2), as columns for cpt_methods(). Each is NA for a
# method the measurement did not cover (a registered one, or an engine that
# could not run where the tables were built).
#   scale_invariant  same answer at x, 10x and 0.1x (§179)
#   sequential       the answer depends on the direction of time: the
#                    reversal test fails, as it must for an online
#                    detector (§184.2)
#   max_cp           changepoints the method can return, when fixed by
#                    construction (§184.3), NA when unbounded
#   tier             "general" for the workhorses, "specialised" otherwise
#                    (§186.2)
#   noise_model_arg  the argument that selects the noise model (§189)
#   rate_arg         the argument that sets the false-alarm rate (§210.2)
#   cost             "fast", "moderate" or "slow" at n = 10,000 (§208)
#   max_n            the longest series the runtime table saw it finish
#                    within its time cap
#' @noRd
measured_registry_columns <- function(methods) {
  inv <- measured_data("cpt_invariances")
  rt <- measured_data("cpt_runtimes")
  ra <- rate_args()
  pick <- function(tab, col, default = NA) {
    if (is.null(tab)) return(rep(default, length(methods)))
    v <- tab[[col]][match(methods, tab$method)]
    v
  }
  cost <- rep(NA_character_, length(methods))
  max_n <- rep(NA_real_, length(methods))
  if (!is.null(rt)) {
    for (i in seq_along(methods)) {
      r <- rt[rt$method == methods[i], , drop = FALSE]
      if (!nrow(r)) next
      ok <- r[r$status == "ok" & is.finite(r$seconds), , drop = FALSE]
      # 0: it did not finish the shortest length within the time cap. A
      # method that only errored was not measured, which is NA.
      max_n[i] <- if (nrow(ok)) max(ok$n) else
        if (any(r$status == "timeout")) 0 else NA_real_
      if (!nrow(ok) && !any(r$status == "timeout")) next
      s10k <- ok$seconds[ok$n == 1e4]
      cost[i] <- if (!length(s10k)) "slow" else if (s10k < 1) "fast" else
        if (s10k < 10) "moderate" else "slow"
    }
  }
  mc <- max_cp_table()
  tibble::tibble(
    scale_invariant = as.logical(pick(inv, "scale_invariant")),
    sequential = as.logical(pick(inv, "sequential")),
    max_cp = unname(ifelse(methods %in% names(mc), mc[methods], NA_integer_)),
    tier = ifelse(methods %in% general_purpose_methods(), "general",
                  "specialised"),
    noise_model_arg = unname(noise_model_args()[methods]),
    rate_arg = ra$rate_arg[match(methods, ra$method)],
    cost = cost,
    max_n = max_n
  )
}
