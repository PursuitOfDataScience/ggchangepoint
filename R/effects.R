# ---------------------------------------------------------------------------
# How big was the change, did anything change at a date you already had in
# mind, and was the detected change the event you think it was.
#
# Detection answers where; the report needs how much, and the most common
# applied question starts from a date rather than from a detector. Testing a
# pre-specified date is also the statistically easy case: no selection to
# adjust for, so the p-value means what it says, which is the one thing the
# package's post-detection tests cannot promise.
# ---------------------------------------------------------------------------

#' Effect size at each changepoint
#'
#' The size of every change a fit reports, in the data's own units and
#' standardised: the level before and after, their difference with an
#' interval, the difference in noise standard deviations, the percentage
#' change, and for count and waiting-time data the rate or hazard ratio.
#'
#' @param fit A \code{ggcpt} object.
#' @param level Confidence level for the intervals. Defaults to \code{0.95}.
#' @param method \code{"naive"} (the default) measures each effect on the
#'   data that located the change. \code{"split"} locates the changepoints
#'   on the odd-numbered observations and measures their effects on the
#'   even-numbered ones, which removes the winner's curse at the price of
#'   locating on half the data (see below).
#' @param seed Optional seed for \code{method = "split"}, scoped to this call.
#' @param ... Further arguments for the re-run \code{method = "split"} makes
#'   (overriding what is recovered from the fit).
#' @return A \code{ggcpt_effect} tibble with one row per changepoint (per
#'   changepoint and coordinate for a multivariate fit, per changepoint and
#'   term for a regression fit): \code{cp} (and \code{cp_index}),
#'   \code{before}, \code{after}, \code{n_before}, \code{n_after},
#'   \code{delta}, \code{delta_lower}, \code{delta_upper} (a Welch
#'   interval), \code{delta_std} (the change in noise standard deviations,
#'   the unit \code{cpt_detect(min_effect = )} filters on),
#'   \code{pct_change}, \code{sd_before}, \code{sd_after},
#'   \code{sd_ratio}, the family-specific ratio where it applies
#'   (\code{rate_ratio} for Poisson counts, \code{odds_ratio} for binary
#'   data, \code{hazard_ratio} for exponential waiting times, each with an
#'   interval), \code{selection_adjusted} and \code{method}. With
#'   \code{print()} and \code{autoplot()}.
#'
#' @section The winner's curse:
#' A change measured at a location the same data chose is biased upward:
#' the detector put the boundary where the two sides differ most, so the
#' difference it reports is the largest the noise allowed. The bias is
#' worst for the marginal detections, which are exactly the ones whose size
#' matters. \code{method = "naive"} rows carry \code{selection_adjusted =
#' FALSE} for that reason. \code{method = "split"} is honest about the size
#' (the measuring half never influenced the locations) and less precise
#' about the location (the locating half has half the data), and it assumes
#' the noise is independent from one observation to the next, since the
#' two halves are interleaved.
#' @seealso \code{\link{cpt_test_at}()} for a change at a date fixed in
#'   advance, \code{\link{cpt_detect}()}'s \code{min_effect}.
#' @export
#' @family inference
#' @examples
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(100, 10), rnorm(100, 12)), method = "pelt")
#' cpt_effect(fit)
#'
#' counts <- c(rpois(100, 4), rpois(100, 8))
#' cpt_effect(cpt_detect(counts, method = "pelt", family = "poisson"))
cpt_effect <- function(fit, level = 0.95, method = c("naive", "split"),
                       seed = NULL, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  validate_scalar(level, "level", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  method <- cpt_match_arg(method)
  family <- fit$family %||% "gaussian"
  if (identical(scalar_chr(fit$change_in), "regression") &&
      !is.null(fit$coefficients)) {
    if (method == "split") {
      cpt_abort("`method = \"split\"` re-runs the detector, and a ",
                "regression fit's covariates are not stored in a form it can ",
                "re-run. Use the naive estimates, or refit on half the rows ",
                "yourself.", class = "capability_absent")
    }
    out <- coefficient_effects(fit, level)
    return(new_effect(out, fit, "naive"))
  }
  if (method == "split") {
    return(split_effect(fit, level, seed, ...))
  }
  out <- series_effects(fit, fit$changepoints$cp, level, family)
  out$selection_adjusted <- rep(FALSE, nrow(out))
  out$method <- rep("naive (measured where the data located the change)",
                    nrow(out))
  new_effect(out, fit, "naive")
}

# Internal: effects on every coordinate of a (possibly multivariate) fit at
# the locations `cps`, measured on `values` (the fit's own series unless
# given).
#' @noRd
series_effects <- function(fit, cps, level, family, values = NULL,
                           keep = NULL) {
  if (is.null(values)) {
    values <- if (n_coordinates(fit) > 1L) {
      w <- fit$data_wide
      as.data.frame(w[, setdiff(names(w), c("index", "index_value")),
                      drop = FALSE])
    } else {
      data.frame(value = fit$data$value)
    }
  }
  rows <- lapply(names(values), function(col) {
    tb <- effect_table(values[[col]], cps, level, family, keep = keep)
    if (ncol(values) > 1L) tb <- tibble::add_column(tb, coordinate = col,
                                                     .after = "cp")
    tb
  })
  out <- do.call(rbind, rows)
  if (!is.null(fit$index) && nrow(out)) {
    out <- tibble::add_column(out, cp_index = fit$index[out$cp],
                              .after = "cp")
  }
  out
}

# Internal: the effect of each changepoint in `cps` on one series. `keep`
# optionally masks which observations are used to measure (the even half,
# for the split estimator); the segment bounds are always the positions in
# the full series.
#' @noRd
effect_table <- function(v, cps, level, family = "gaussian", keep = NULL) {
  n <- length(v)
  cps <- sort(cps)
  empty <- tibble::tibble(cp = integer(), before = numeric(),
                          after = numeric(), n_before = integer(),
                          n_after = integer(), delta = numeric(),
                          delta_lower = numeric(), delta_upper = numeric(),
                          delta_std = numeric(), pct_change = numeric(),
                          sd_before = numeric(), sd_after = numeric(),
                          sd_ratio = numeric())
  if (!length(cps)) return(empty)
  sigma <- noise_sd(if (is.null(keep)) v else v[keep])
  bounds <- c(0L, cps, n)
  z <- stats::qnorm(1 - (1 - level) / 2)
  rows <- lapply(seq_along(cps), function(k) {
    ia <- (bounds[k] + 1L):bounds[k + 1L]
    ib <- (bounds[k + 1L] + 1L):bounds[k + 2L]
    if (!is.null(keep)) {
      ia <- ia[keep[ia]]
      ib <- ib[keep[ib]]
    }
    a <- v[ia]
    b <- v[ib]
    a <- a[!is.na(a)]
    b <- b[!is.na(b)]
    ma <- if (length(a)) mean(a) else NA_real_
    mb <- if (length(b)) mean(b) else NA_real_
    sa <- if (length(a) > 1L) stats::sd(a) else NA_real_
    sb <- if (length(b) > 1L) stats::sd(b) else NA_real_
    delta <- mb - ma
    se <- sqrt(sa^2 / length(a) + sb^2 / length(b))
    dfw <- if (is.finite(se) && se > 0) {
      se^4 / ((sa^2 / length(a))^2 / (length(a) - 1) +
                (sb^2 / length(b))^2 / (length(b) - 1))
    } else {
      NA_real_
    }
    crit <- if (is.finite(dfw)) stats::qt(1 - (1 - level) / 2, dfw) else z
    row <- tibble::tibble(
      cp = cps[k], before = ma, after = mb, n_before = length(a),
      n_after = length(b), delta = delta,
      delta_lower = delta - crit * se, delta_upper = delta + crit * se,
      delta_std = if (is.finite(sigma) && sigma > 0) delta / sigma else
        NA_real_,
      pct_change = if (is.finite(ma) && ma != 0) 100 * delta / abs(ma) else
        NA_real_,
      sd_before = sa, sd_after = sb,
      sd_ratio = if (is.finite(sa) && sa > 0) sb / sa else NA_real_)
    fam_ratio <- family_ratio(a, b, family, z)
    if (!is.null(fam_ratio)) row <- cbind(row, fam_ratio)
    tibble::as_tibble(row)
  })
  do.call(rbind, rows)
}

# Internal: the ratio a family is naturally reported in, with a Wald
# interval on the log scale.
#' @noRd
family_ratio <- function(a, b, family, z) {
  if (identical(family, "poisson")) {
    sa <- sum(a)
    sb <- sum(b)
    rr <- (sb / length(b)) / (sa / length(a))
    se <- sqrt(1 / sb + 1 / sa)
    return(tibble::tibble(rate_ratio = rr,
                          rate_ratio_lower = rr * exp(-z * se),
                          rate_ratio_upper = rr * exp(z * se)))
  }
  if (identical(family, "exponential")) {
    # hazard = 1 / mean, so the ratio after:before is mean_before / mean_after
    hr <- mean(a) / mean(b)
    se <- sqrt(1 / length(a) + 1 / length(b))
    return(tibble::tibble(hazard_ratio = hr,
                          hazard_ratio_lower = hr * exp(-z * se),
                          hazard_ratio_upper = hr * exp(z * se)))
  }
  if (identical(family, "binomial")) {
    # Haldane's half added to every cell, so a segment of all zeros or all
    # ones still has a finite ratio.
    xa <- sum(a) + 0.5
    xb <- sum(b) + 0.5
    fa <- length(a) - sum(a) + 0.5
    fb <- length(b) - sum(b) + 0.5
    or <- (xb / fb) / (xa / fa)
    se <- sqrt(1 / xa + 1 / fa + 1 / xb + 1 / fb)
    return(tibble::tibble(odds_ratio = or,
                          odds_ratio_lower = or * exp(-z * se),
                          odds_ratio_upper = or * exp(z * se)))
  }
  NULL
}

# Internal: changes in each regression coefficient across each break.
#' @noRd
coefficient_effects <- function(fit, level) {
  co <- fit$coefficients
  cps <- sort(fit$changepoints$cp)
  if (!length(cps) || is.null(co) || !nrow(co)) {
    return(tibble::tibble(cp = integer(), term = character(),
                          before = numeric(), after = numeric(),
                          delta = numeric(), delta_lower = numeric(),
                          delta_upper = numeric()))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  rows <- lapply(seq_along(cps), function(k) {
    a <- co[co$segment == k, , drop = FALSE]
    b <- co[co$segment == k + 1L, , drop = FALSE]
    terms <- intersect(a$term, b$term)
    ea <- a$estimate[match(terms, a$term)]
    eb <- b$estimate[match(terms, b$term)]
    se <- sqrt(a$std_error[match(terms, a$term)]^2 +
                 b$std_error[match(terms, b$term)]^2)
    tibble::tibble(cp = cps[k], term = terms, before = ea, after = eb,
                   delta = eb - ea, delta_lower = eb - ea - z * se,
                   delta_upper = eb - ea + z * se)
  })
  out <- do.call(rbind, rows)
  if (!is.null(fit$index)) {
    out <- tibble::add_column(out, cp_index = fit$index[out$cp],
                              .after = "cp")
  }
  out$selection_adjusted <- FALSE
  out$method <- "naive (coefficient change across the break)"
  out
}

# Internal: locate on the odd observations, measure on the even ones.
#' @noRd
split_effect <- function(fit, level, seed, ...) {
  mname <- scalar_chr(fit$method)
  if (!bootstrap_possible(fit)) {
    cpt_abort("`method = \"split\"` re-runs the detector on half the data, ",
              "and `", mname, "` is not a method cpt_detect() knows (a ",
              "result built with as_ggcpt() records someone else's ",
              "changepoints).", class = "capability_absent")
  }
  dots <- rerun_dots(fit, list(...), "cpt_effect(method = \"split\")")
  if (is.null(dots$family) && !is.null(fit$family)) dots$family <- fit$family
  v <- fit$data$value
  n <- length(v)
  odd <- seq(1L, n, by = 2L)
  if (length(odd) < 6L) {
    cpt_abort("`method = \"split\"` locates changes on the odd-numbered ",
              "observations; ", n, " observations leave too few.",
              class = "short_series", data = list(n = n))
  }
  local_seed(seed)
  refit <- do.call(cpt_detect, c(list(v[odd], method = mname), dots))
  # A changepoint after odd observation k (position 2k - 1) falls between
  # full-series positions 2k - 1 and 2k + 1: the even observation 2k is the
  # ambiguous one, and it belongs to neither side's measurement.
  cps <- 2L * refit$changepoints$cp - 1L
  cps <- cps[cps >= 1L & cps < n]
  keep <- rep(FALSE, n)
  keep[seq(2L, n, by = 2L)] <- TRUE
  keep[cps + 1L] <- FALSE
  out <- series_effects(fit, cps, level, fit$family %||% "gaussian",
                        keep = keep)
  out$selection_adjusted <- rep(TRUE, nrow(out))
  out$method <- rep(paste("split (located on odd observations, measured on",
                          "even ones)"), nrow(out))
  new_effect(out, fit, "split")
}

#' @noRd
new_effect <- function(out, fit, how) {
  structure(out, class = c("ggcpt_effect", class(tibble::tibble())),
            method = scalar_chr(fit$method), how = how,
            family = fit$family %||% "gaussian")
}

#' @rdname cpt_effect
#' @param x A \code{ggcpt_effect} object.
#' @export
print.ggcpt_effect <- function(x, ...) {
  cat("ggcpt_effect (method: ", attr(x, "method") %||% "?", ", ",
      attr(x, "how") %||% "naive", ")\n", sep = "")
  if (identical(attr(x, "how"), "naive") && nrow(x)) {
    cat("Measured where the same data located each change, so the sizes are",
        "biased upward\n(selection_adjusted = FALSE).",
        if (!"term" %in% names(x)) {
          "method = \"split\" measures on held-out observations."
        }, "\n\n")
  }
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @rdname cpt_effect
#' @param object A \code{ggcpt_effect} object.
#' @export
autoplot.ggcpt_effect <- function(object, ...) {
  d <- tibble::as_tibble(object)
  if (!nrow(d)) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No changepoints, so no effects"))
  }
  if (!"delta" %in% names(d)) {
    cpt_abort("This effect table has no `delta` column to draw.",
              class = "bad_argument")
  }
  lab <- if ("term" %in% names(d)) paste0(d$cp, ": ", d$term) else
    if ("coordinate" %in% names(d)) paste0(d$cp, ": ", d$coordinate) else
      as.character(d$cp)
  d$.label <- factor(lab, levels = unique(lab))
  ggplot2::ggplot(d, ggplot2::aes(x = delta, y = .label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_segment(ggplot2::aes(x = delta_lower, xend = delta_upper,
                                       yend = .label)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "Change (after minus before)", y = "Changepoint",
                  title = paste0("Effect sizes (", attr(object, "how"), ")"),
                  subtitle = if (identical(attr(object, "how"), "naive")) {
                    "Measured where the data located each change: biased upward"
                  })
}

#' Test for a change at a date fixed in advance
#'
#' The question behind most applied changepoint work: the policy took
#' effect on 1 March, the plant was retooled in week 14, the drug was
#' approved in Q2. \emph{Did anything change then?} Because the date was
#' chosen before looking at the data, this is an ordinary two-sample
#' comparison at a fixed split and needs no adjustment for selection: the
#' p-value means what it says.
#'
#' @param x A numeric series (a vector, \code{ts}, \code{zoo} and so on), a
#'   \code{ggcpt} fit (its series and index are used), or a formula with
#'   \code{data} for a break in a regression (a Chow test).
#' @param when When the change took effect: the \strong{first observation
#'   of the new regime}, as a position or a value of the series' index (a
#'   date; for a numeric index such as a \code{ts}'s years, a number
#'   outside \code{1..n} is read as an index value). A detected changepoint
#'   \code{cp} (the last observation of the old regime) corresponds to
#'   \code{when = cp + 1}.
#' @param window Allow the change anywhere within \code{window}
#'   observations either side of \code{when}. The statistic is then the
#'   largest over the window and its p-value comes from a permutation
#'   distribution of that maximum, which pays for the search. Defaults to
#'   \code{0}: the date is exact.
#' @param change_in What to test: \code{"mean"} (the default), \code{"var"},
#'   \code{"meanvar"} or \code{"distribution"}.
#' @param family For \code{change_in = "mean"}: \code{"gaussian"} (a Welch
#'   t-test, the default), \code{"poisson"} (an exact test of two rates),
#'   \code{"binomial"} (Fisher's exact test of two proportions),
#'   \code{"exponential"} (an exact F test of two rates) or \code{"l1"} (a
#'   Wilcoxon rank-sum test, robust to outliers).
#' @param span Optional number of observations each side of the split to
#'   compare. Defaults to all of them; a span keeps a change elsewhere in a
#'   long series out of the comparison.
#' @param index Optional time index when \code{x} is a bare series.
#' @param data A data frame, for formula input.
#' @param level Confidence level for the interval on the change.
#' @param B Permutations for \code{window > 0}. Defaults to \code{999}.
#' @param seed Optional seed for the permutations, scoped to this call.
#' @return A \code{ggcpt_test_at} tibble with one row: \code{when},
#'   \code{cp} (the last observation before it, in the package's
#'   convention) and \code{cp_index}, \code{window}, \code{estimate} (the
#'   change: a difference in means, a ratio of rates or variances),
#'   \code{conf_low}, \code{conf_high}, \code{statistic}, \code{p_value},
#'   \code{n_before}, \code{n_after}, \code{method} and
#'   \code{selection_adjusted} (\code{TRUE}: the location was not chosen
#'   from the data, and a window's search is paid for by the permutation).
#' @seealso \code{\link{cpt_attribute_event}()} for the mirror question
#'   (was a detected change the event you know about?),
#'   \code{\link{cpt_effect}()}.
#' @export
#' @family inference
#' @examples
#' set.seed(1)
#' dates <- as.Date("2026-01-01") + 0:119
#' x <- c(rnorm(60), rnorm(60, 0.8))
#' cpt_test_at(x, when = as.Date("2026-03-02"), index = dates)
#' # the policy took effect sometime in that fortnight
#' cpt_test_at(x, when = 61, window = 7, seed = 1)
cpt_test_at <- function(x, when, window = 0,
                        change_in = c("mean", "var", "meanvar",
                                      "distribution"),
                        family = c("gaussian", "poisson", "binomial",
                                   "exponential", "l1"),
                        span = NULL, index = NULL, data = NULL,
                        level = 0.95, B = 999, seed = NULL) {
  change_in <- cpt_match_arg(change_in)
  family <- cpt_match_arg(family)
  validate_scalar(window, "window", min = 0)
  validate_scalar(level, "level", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  validate_scalar(B, "B", min = 19)
  if (!is.null(span)) validate_scalar(span, "span", min = 2)
  if (missing(when) || length(when) != 1L) {
    cpt_abort("`when` must be one location: a position, or a value of the ",
              "series' index.", class = "bad_argument")
  }
  if (inherits(x, "formula")) {
    return(test_at_formula(x, data, when, window, span, level, B, seed))
  }
  if (is_ggcpt(x)) {
    idx <- x$index
    v <- x$data$value
  } else {
    series <- as_cpt_series(x, index = index)
    if (is.matrix(series$values) || is.data.frame(series$values)) {
      cpt_abort("`cpt_test_at()` tests one series; `x` has several columns.",
                class = "wrong_dimension")
    }
    v <- as.numeric(series$values)
    idx <- series$index
  }
  validate_data(v)
  n <- length(v)
  pos <- effective_position(when, idx, n)
  if (family != "gaussian" && change_in != "mean") {
    cpt_abort("`family = \"", family, "\"` tests a change in its rate or ",
              "location, which is `change_in = \"mean\"`.",
              class = "unsupported")
  }
  if (family %in% c("poisson", "binomial", "exponential")) {
    check_family_data(v, family, "cpt_test_at")
  }
  test_one <- function(p, vv) two_sample_test(vv, p, change_in, family, span,
                                              level)
  window <- as.integer(window)
  if (window == 0L) {
    res <- test_one(pos, v)
    res$method <- paste(res$method, "at a pre-specified location")
  } else {
    cand <- seq(max(2L, pos - window), min(n, pos + window))
    stat_of <- function(vv) {
      s <- vapply(cand, function(p) abs(test_one(p, vv)$statistic_z),
                  numeric(1))
      s[!is.finite(s)] <- 0
      s
    }
    obs <- stat_of(v)
    best <- cand[which.max(obs)]
    local_seed(seed)
    null_max <- vapply(seq_len(as.integer(B)), function(b) {
      max(stat_of(v[sample.int(n)]))
    }, numeric(1))
    res <- test_one(best, v)
    res$p_value <- (1 + sum(null_max >= max(obs))) / (1 + as.integer(B))
    res$method <- paste0(res$method, ", largest over a window of +/-",
                         window, " (permutation, B = ", B, ")")
    pos <- best
  }
  out <- tibble::tibble(
    when = when, cp = as.integer(pos - 1L),
    window = window, estimate = res$estimate, conf_low = res$conf_low,
    conf_high = res$conf_high, statistic = res$statistic,
    p_value = res$p_value, n_before = res$n_before, n_after = res$n_after,
    method = res$method, selection_adjusted = TRUE)
  if (!is.null(idx)) {
    out <- tibble::add_column(out, cp_index = idx[out$cp], .after = "cp")
  }
  structure(out, class = c("ggcpt_test_at", class(tibble::tibble())),
            change_in = change_in, family = family)
}

# Internal: `when` (first observation of the new regime) as a position.
#' @noRd
effective_position <- function(when, idx, n) {
  p <- locate_on_series(when, idx, n, "when", side = "after")
  if (is.na(p)) {
    cpt_abort("`when` is after the last observation",
              if (!is.null(idx)) paste0(" (", format_index_range(idx), ")"),
              ".", class = "bad_argument")
  }
  p <- as.integer(round(p))
  if (p < 2L || p > n) {
    cpt_abort("`when` must leave at least one observation before it: a ",
              "position in 2..", n, " (got ", p, ").",
              class = "bad_argument")
  }
  p
}

# Internal: the two-sample test at split position `p` (the first
# observation of the new regime). `statistic_z` is the statistic on a
# common, sign-free scale for the window's maximum.
#' @noRd
two_sample_test <- function(v, p, change_in, family, span, level) {
  n <- length(v)
  lo <- if (is.null(span)) 1L else max(1L, p - as.integer(span))
  hi <- if (is.null(span)) n else min(n, p - 1L + as.integer(span))
  a <- v[lo:(p - 1L)]
  b <- v[p:hi]
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  na <- length(a)
  nb <- length(b)
  out <- list(n_before = na, n_after = nb)
  fail <- function(m) c(out, list(estimate = NA_real_, conf_low = NA_real_,
                                  conf_high = NA_real_, statistic = NA_real_,
                                  statistic_z = NA_real_, p_value = NA_real_,
                                  method = m))
  if (change_in == "mean" && family == "gaussian") {
    if (na < 2L || nb < 2L) return(fail("Welch two-sample t"))
    tt <- tryCatch(stats::t.test(b, a, conf.level = level),
                   error = function(e) NULL)
    if (is.null(tt)) return(fail("Welch two-sample t"))
    return(c(out, list(estimate = mean(b) - mean(a),
                       conf_low = tt$conf.int[1], conf_high = tt$conf.int[2],
                       statistic = unname(tt$statistic),
                       statistic_z = unname(tt$statistic),
                       p_value = tt$p.value, method = "Welch two-sample t")))
  }
  if (change_in == "mean" && family == "poisson") {
    pt <- stats::poisson.test(c(sum(b), sum(a)), c(nb, na),
                              conf.level = level)
    rr <- unname(pt$estimate)
    return(c(out, list(estimate = rr, conf_low = pt$conf.int[1],
                       conf_high = pt$conf.int[2], statistic = sum(b),
                       statistic_z = abs(log(max(rr, 1e-12))) /
                         sqrt(1 / max(sum(a), 0.5) + 1 / max(sum(b), 0.5)),
                       p_value = pt$p.value,
                       method = "Exact test of two Poisson rates (rate ratio)")))
  }
  if (change_in == "mean" && family == "binomial") {
    tab <- matrix(c(sum(b), nb - sum(b), sum(a), na - sum(a)), nrow = 2)
    ft <- stats::fisher.test(tab, conf.level = level)
    or <- unname(ft$estimate)
    return(c(out, list(estimate = or, conf_low = ft$conf.int[1],
                       conf_high = ft$conf.int[2], statistic = or,
                       statistic_z = stats::qnorm(1 - ft$p.value / 2),
                       p_value = ft$p.value,
                       method = "Fisher's exact test (odds ratio)")))
  }
  if (change_in == "mean" && family == "exponential") {
    # 2 * rate * sum ~ chi-squared(2n) per side, so the ratio of means is F
    ratio <- mean(a) / mean(b)
    f <- ratio
    pval <- 2 * min(stats::pf(f, 2 * na, 2 * nb),
                    stats::pf(f, 2 * na, 2 * nb, lower.tail = FALSE))
    alpha <- 1 - level
    ci <- c(ratio / stats::qf(1 - alpha / 2, 2 * na, 2 * nb),
            ratio / stats::qf(alpha / 2, 2 * na, 2 * nb))
    return(c(out, list(estimate = ratio, conf_low = ci[1], conf_high = ci[2],
                       statistic = f,
                       statistic_z = stats::qnorm(1 - min(1, pval) / 2),
                       p_value = min(1, pval),
                       method = "Exact F test of two exponential rates (hazard ratio)")))
  }
  if (change_in == "mean" && family == "l1") {
    wt <- stats::wilcox.test(b, a, conf.int = TRUE, conf.level = level,
                             exact = FALSE)
    return(c(out, list(estimate = unname(wt$estimate),
                       conf_low = wt$conf.int[1], conf_high = wt$conf.int[2],
                       statistic = unname(wt$statistic),
                       statistic_z = stats::qnorm(1 - wt$p.value / 2),
                       p_value = wt$p.value,
                       method = "Wilcoxon rank-sum (location shift)")))
  }
  if (change_in == "var") {
    if (na < 2L || nb < 2L) return(fail("F test of two variances"))
    vt <- stats::var.test(b, a, conf.level = level)
    return(c(out, list(estimate = unname(vt$estimate),
                       conf_low = vt$conf.int[1], conf_high = vt$conf.int[2],
                       statistic = unname(vt$statistic),
                       statistic_z = stats::qnorm(1 - vt$p.value / 2),
                       p_value = vt$p.value,
                       method = "F test of two variances (variance ratio)")))
  }
  if (change_in == "meanvar") {
    if (na < 2L || nb < 2L) {
      return(fail("Likelihood ratio, normal mean and variance"))
    }
    all <- c(a, b)
    nt <- length(all)
    s0 <- mean((all - mean(all))^2)
    s1 <- mean((a - mean(a))^2)
    s2 <- mean((b - mean(b))^2)
    lr <- nt * log(s0) - na * log(s1) - nb * log(s2)
    pval <- stats::pchisq(lr, df = 2, lower.tail = FALSE)
    return(c(out, list(estimate = mean(b) - mean(a), conf_low = NA_real_,
                       conf_high = NA_real_, statistic = lr,
                       statistic_z = stats::qnorm(1 - pval / 2),
                       p_value = pval,
                       method = "Likelihood ratio, normal mean and variance (chi-squared, 2 df)")))
  }
  ks <- suppressWarnings(stats::ks.test(b, a))
  c(out, list(estimate = unname(ks$statistic), conf_low = NA_real_,
              conf_high = NA_real_, statistic = unname(ks$statistic),
              statistic_z = stats::qnorm(1 - ks$p.value / 2),
              p_value = ks$p.value,
              method = "Two-sample Kolmogorov-Smirnov (distribution)"))
}

# Internal: a Chow test of a regression at a pre-specified break.
#' @noRd
test_at_formula <- function(formula, data, when, window, span, level, B,
                            seed) {
  spec <- formula_series(formula, data, NULL, parent.frame())
  y <- spec$y
  X <- spec$X
  ok <- !is.na(y) & stats::complete.cases(X)
  if (!all(ok)) {
    cpt_abort("The regression has missing values; drop the incomplete rows ",
              "first.", class = "non_finite")
  }
  n <- length(y)
  idx <- NULL
  pos <- effective_position(when, idx, n)
  chow <- function(p, yy) {
    rss <- function(r) {
      f <- stats::lm.fit(X[r, , drop = FALSE], yy[r])
      sum(f$residuals^2)
    }
    k <- ncol(X)
    left <- seq_len(p - 1L)
    right <- p:n
    if (length(left) <= k || length(right) <= k) return(NA_real_)
    r0 <- rss(seq_len(n))
    r1 <- rss(left) + rss(right)
    ((r0 - r1) / k) / (r1 / (n - 2 * k))
  }
  k <- ncol(X)
  window <- as.integer(window)
  if (window == 0L) {
    f <- chow(pos, y)
    pval <- stats::pf(f, k, n - 2 * k, lower.tail = FALSE)
    method <- "Chow F at a pre-specified break"
  } else {
    cand <- seq(max(k + 2L, pos - window), min(n - k, pos + window))
    obs <- vapply(cand, chow, numeric(1), yy = y)
    obs[!is.finite(obs)] <- 0
    pos <- cand[which.max(obs)]
    f <- max(obs)
    local_seed(seed)
    null_max <- vapply(seq_len(as.integer(B)), function(b) {
      yy <- y[sample.int(n)]
      s <- vapply(cand, chow, numeric(1), yy = yy)
      max(s[is.finite(s)], 0)
    }, numeric(1))
    pval <- (1 + sum(null_max >= f)) / (1 + as.integer(B))
    method <- paste0("Chow F, largest over a window of +/-", window,
                     " (permutation, B = ", B, ")")
  }
  structure(tibble::tibble(
    when = when, cp = as.integer(pos - 1L), window = window,
    estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
    statistic = f, p_value = pval, n_before = pos - 1L,
    n_after = n - pos + 1L, method = method, selection_adjusted = TRUE),
    class = c("ggcpt_test_at", class(tibble::tibble())),
    change_in = "regression", family = "gaussian")
}

#' @rdname cpt_test_at
#' @param ... Ignored.
#' @export
print.ggcpt_test_at <- function(x, ...) {
  cat("ggcpt_test_at (change in ", attr(x, "change_in") %||% "?",
      "; the location was fixed in advance, so no selection adjustment ",
      "is needed)\n\n", sep = "")
  print(tibble::as_tibble(x))
  invisible(x)
}

#' Was a detected change the event you have in mind?
#'
#' The mirror of \code{\link{cpt_test_at}()}. A detector found a change on
#' 14 March and something is known to have happened on 1 March: is that the
#' same event? The detection's confidence interval answers it: an event
#' inside the interval is consistent with being the change, one outside is
#' not.
#'
#' @param fit A \code{ggcpt} object.
#' @param event One or more event locations: positions, values of the fit's
#'   index, or a \code{\link{cpt_annotate_events}()} result.
#' @param level Confidence level of the interval. Defaults to \code{0.95}.
#' @param ... Further arguments for \code{\link{cpt_confint}()} (for
#'   example \code{B} or \code{seed} for a bootstrap interval).
#' @return A tibble with one row per event: \code{event},
#'   \code{event_position}, the nearest changepoint \code{cp} (and
#'   \code{cp_index}), its interval \code{ci_lower}/\code{ci_upper},
#'   \code{distance} (positions from the changepoint to the event),
#'   \code{inside} and a plain-language \code{verdict}.
#' @export
#' @family inference
#' @examplesIf requireNamespace("strucchange", quietly = TRUE)
#' set.seed(1)
#' dates <- as.Date("2026-01-01") + 0:119
#' fit <- cpt_detect(c(rnorm(60), rnorm(60, 3)), method = "strucchange",
#'                   index = dates)
#' cpt_attribute_event(fit, as.Date(c("2026-02-28", "2026-04-10")))
cpt_attribute_event <- function(fit, event, level = 0.95, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  labels <- NULL
  if (inherits(event, "ggcpt_events")) {
    ev <- dplyr::bind_rows(event$matched, event$undetected)
    labels <- ev$event
    event_pos <- ev$event_position
    event <- ev$event_value %||% event_pos
  } else {
    event_pos <- locate_on_series(event, fit$index, nrow(fit$data), "event",
                                  side = "before")
  }
  cps <- fit$changepoints$cp
  if (!length(cps)) {
    cpt_abort("The fit has no changepoints, so there is nothing to attribute ",
              "an event to. cpt_test_at() tests a change at the event's date ",
              "directly.", class = "capability_absent")
  }
  # The level is passed only when asked for: an engine's own interval
  # records no level, and cpt_confint() rightly warns when one it cannot
  # honour is requested explicitly.
  ci <- if (missing(level)) cpt_confint(fit, ...) else
    cpt_confint(fit, level = level, ...)
  rows <- lapply(seq_along(event_pos), function(i) {
    p <- event_pos[i]
    j <- if (is.na(p)) NA_integer_ else which.min(abs(cps - p))
    lo <- if (is.na(j)) NA_real_ else ci$ci_lower[j]
    hi <- if (is.na(j)) NA_real_ else ci$ci_upper[j]
    inside <- !is.na(lo) && !is.na(hi) && p >= lo && p <= hi
    verdict <- if (is.na(p)) {
      "the event is outside the series"
    } else if (is.na(lo)) {
      "no interval available for the nearest changepoint"
    } else if (inside) {
      "consistent: the event lies inside the changepoint's interval"
    } else {
      "not supported: the event lies outside the changepoint's interval"
    }
    tibble::tibble(event = if (!is.null(labels)) labels[i] else
      as.character(event[i]), event_position = p,
      cp = if (is.na(j)) NA_integer_ else cps[j],
      ci_lower = lo, ci_upper = hi,
      distance = if (is.na(j)) NA_real_ else p - cps[j],
      inside = inside, verdict = verdict)
  })
  out <- do.call(rbind, rows)
  if (!is.null(fit$index)) {
    out <- tibble::add_column(out, cp_index = fit$index[out$cp],
                              .after = "cp")
  }
  attr(out, "level") <- level
  out
}
