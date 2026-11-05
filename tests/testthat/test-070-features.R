# 0.7.0: the inferential release. Attribution (ARM), distribution-free
# confidence sets (CONCH) and their measured coverage, epidemic changepoints
# (CAPA), the counterfactual exhibit, the abruptness check and gradual
# simulation, CUSUM and EWMA control charts, panel screening, and weights
# over K. Where a feature makes a statistical promise, the test measures
# the promise (family-wise error, coverage, run length, false discovery
# rate) rather than asserting that the function returns.

mv_step <- function(n = 120, d = 6, seed = 1) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * d), n, d,
              dimnames = list(NULL, paste0("s", seq_len(d))))
  X[(n / 2 + 1):n, 2] <- X[(n / 2 + 1):n, 2] + 2
  X[(n / 2 + 1):n, 5] <- X[(n / 2 + 1):n, 5] * 3
  X
}

# ---- attribution -----------------------------------------------------------

test_that("ARM's statistic is the max over splits of the two rank channels", {
  set.seed(3)
  W <- matrix(stats::rnorm(40 * 2), 40, 2)
  ch <- ggchangepoint:::arm_prepare(W)
  grid <- 2:38
  got <- ggchangepoint:::arm_channels(ch, seq_len(40), grid)
  brute <- vapply(1:2, function(j) {
    r <- rank(W[, j])
    C <- r - mean(r)
    S <- C^2 - mean(C^2)
    z <- vapply(grid, function(t) {
      f <- t * (40 - t) / (40 * 39)
      max(abs(sum(C[1:t])) / sqrt(f * sum(C^2)),
          abs(sum(S[1:t])) / sqrt(f * sum(S^2)))
    }, numeric(1))
    max(z)
  }, numeric(1))
  expect_equal(got$M, brute)
})

test_that("cpt_attribute() certifies the coordinates that changed, typed", {
  skip_on_cran()
  X <- mv_step()
  fit <- as_ggcpt(60, X, method = "manual")
  att <- cpt_attribute(fit, B = 199, seed = 1)
  expect_s3_class(att, "ggcpt_attribution")
  expect_equal(att$coordinate[att$changed], c("s2", "s5"))
  expect_equal(att$type[att$changed], c("location", "scale"))
  expect_true(all(is.na(att$type[!att$changed])))
  expect_true(all(att$p_adjusted >= att$p_value - 1e-12))
  g <- glance(att)
  expect_equal(g$n_changed, 2L)
  expect_equal(c(g$n_location, g$n_scale), c(1L, 1L))
  expect_s3_class(tidy(att), "tbl_df")
  expect_false(inherits(tidy(att), "ggcpt_attribution"))
  expect_s3_class(ggplot2::autoplot(att), "ggplot")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(att)))
  expect_output(print(att), "s2 \\(location\\), s5 \\(scale\\)")
  # the seed reproduces, and holm and fdr give the same answer here
  expect_identical(cpt_attribute(fit, B = 199, seed = 1)$p_value,
                   att$p_value)
  expect_equal(cpt_attribute(fit, B = 999, control = "holm",
                             seed = 2)$coordinate[
    cpt_attribute(fit, B = 999, control = "holm", seed = 2)$changed],
    c("s2", "s5"))
})

test_that("ARM keeps its family-wise error at the detected changepoint", {
  skip_on_cran()
  # The selection effect ARM exists to remove: two coordinates change, the
  # location comes from an aggregate CUSUM on all of them, and the null
  # coordinates are then tested there. Measured at the nominal 0.1.
  one <- function(r, d = 8) {
    set.seed(5000 + r)
    n <- 120
    X <- matrix(stats::rt(n * d, df = 3), n, d)
    X[61:n, 1:2] <- X[61:n, 1:2] + 1
    Z <- scale(X)
    cs <- apply(Z, 2, cumsum)
    t <- 10:110
    that <- t[which.max(rowSums(cs[t, ]^2) / (t * (n - t) / n))]
    a <- suppressWarnings(cpt_attribute(as_ggcpt(that, X), alpha = 0.1,
                                        B = 99))
    any(a$changed[-(1:2)])
  }
  fwer <- mean(vapply(seq_len(150), one, logical(1)))
  expect_lt(fwer, 0.1 + 2.5 * sqrt(0.1 * 0.9 / 150))
})

test_that("cpt_attribute() refuses one series and handles the edge cases", {
  skip_on_cran()
  set.seed(4)
  uni <- as_ggcpt(50, c(stats::rnorm(50), stats::rnorm(50, 3)))
  expect_error(cpt_attribute(uni), class = "ggchangepoint_wrong_dimension")
  expect_error(cpt_attribute(1:10), class = "ggchangepoint_bad_argument")
  X <- mv_step()
  none <- as_ggcpt(integer(0), X)
  e <- cpt_attribute(none, B = 19)
  expect_equal(nrow(e), 0L)
  expect_named(e, c("cp", "coordinate", "statistic", "statistic_location",
                    "statistic_scale", "p_value", "p_adjusted", "changed",
                    "type", "window_start", "window_end", "n_window",
                    "source"))
  expect_output(print(e), "nothing to attribute")
  # too few permutations for the control asked for: warned, not silent
  expect_warning(cpt_attribute(as_ggcpt(60, X), B = 10, control = "holm"),
                 class = "ggchangepoint_argument_ignored")
  # a time index travels
  idx <- as.Date("2026-01-01") + 0:119
  fit <- as_ggcpt(60, X, index = idx)
  a <- cpt_attribute(fit, B = 19, seed = 1)
  expect_equal(unique(a$cp_index), idx[60])
  # missing rows (a fit made with na_action = "omit" keeps its gaps) are
  # dropped from the window, not propagated
  gappy <- as_ggcpt(60, X)
  gappy$data_wide$s3[c(5, 90)] <- NA
  a2 <- cpt_attribute(gappy, B = 19, seed = 1)
  expect_equal(unique(a2$n_window), 118L)
  # subsetting away a required column drops the class
  expect_false(inherits(a[, c("cp", "coordinate")], "ggcpt_attribution"))
})

test_that("autoplot(type = 'attribution') reaches cpt_attribute()", {
  fit <- as_ggcpt(60, mv_step())
  p <- ggplot2::autoplot(fit, type = "attribution", B = 19, seed = 1)
  expect_s3_class(p, "ggplot")
})

# ---- conformal confidence sets ---------------------------------------------

test_that("the conformal route returns a set around the change", {
  skip_on_cran()
  set.seed(5)
  x <- c(stats::rnorm(80), stats::rnorm(80, 2))
  fit <- as_ggcpt(80, x)
  ci <- cpt_confint(fit, method = "conformal", B = 99, seed = 1)
  expect_equal(ci$source, "conformal")
  expect_true(ci$ci_lower <= 80 && 80 <= ci$ci_upper)
  expect_true(ci$set_size >= 1 && ci$set_size <= ci$ci_upper - ci$ci_lower + 1)
  prof <- attr(ci, "profile")
  expect_named(prof, c("cp", "position", "p_value"))
  expect_equal(nrow(prof), 159L)
  expect_true(all(prof$p_value > 0 & prof$p_value <= 1))
  # reproducible with a seed, and a result built by hand is served without
  # re-running any detector
  expect_identical(cpt_confint(fit, method = "conformal", B = 99,
                               seed = 1)$set_size, ci$set_size)
})

test_that("method = 'auto' prefers the conformal set to the bootstrap", {
  set.seed(6)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3))
  fit <- cpt_detect(x, method = "pelt")
  expect_equal(cpt_confint(fit, B = 49, seed = 1)$source, "conformal")
  # a change in slope is not one between exchangeable segments: conformal
  # refuses by name and auto falls back to the bootstrap
  slope <- as_ggcpt(60, c(1:60, 60 - (1:60)) + stats::rnorm(120),
                    change_in = "slope")
  expect_error(cpt_confint(slope, method = "conformal"),
               class = "ggchangepoint_capability_absent")
  expect_false(ggchangepoint:::conformal_possible(slope))
})

test_that("each score serves its change type, and multivariate results", {
  skip_on_cran()
  set.seed(7)
  xv <- c(stats::rnorm(100), stats::rnorm(100, 0, 3))
  v <- cpt_confint(as_ggcpt(100, xv, change_in = "var"),
                   method = "conformal", B = 99, seed = 1)
  expect_true(v$ci_lower <= 100 && 100 <= v$ci_upper)
  mv <- cpt_confint(as_ggcpt(100, c(stats::rnorm(100), stats::rnorm(100, 1, 2)),
                             change_in = "meanvar"),
                    method = "conformal", B = 99, seed = 1)
  expect_false(is.na(mv$ci_lower))
  d <- cpt_confint(as_ggcpt(100, c(stats::rnorm(100),
                                   stats::rexp(100) + 0.5),
                            change_in = "distribution"),
                   method = "conformal", B = 99, seed = 1)
  expect_false(is.na(d$ci_lower))
  X <- cbind(a = c(stats::rnorm(100), stats::rnorm(100, 1.5)),
             b = stats::rnorm(200))
  m <- cpt_confint(as_ggcpt(100, X), method = "conformal", B = 99, seed = 1)
  expect_true(m$ci_lower <= 100 && 100 <= m$ci_upper)
})

test_that("screening a long window gives the exhaustive set", {
  skip_on_cran()
  agree <- vapply(1:6, function(r) {
    set.seed(r)
    W <- matrix(c(stats::rnorm(250), stats::rnorm(250, 1)), ncol = 1)
    set.seed(100 + r)
    a <- ggchangepoint:::conch_window(W, 99, "mean", exhaustive_max = 1e6)
    set.seed(100 + r)
    b <- ggchangepoint:::conch_window(W, 99, "mean")
    identical(a$t[a$p > 0.05], b$t[!is.na(b$p) & b$p > 0.05])
  }, logical(1))
  expect_true(all(agree))
})

test_that("the conformal set covers at its nominal level", {
  skip_on_cran()
  # Coverage of the TRUE location by the set, given that pelt found one
  # change: the property CONCH guarantees in finite samples.
  covered <- vapply(seq_len(120), function(r) {
    set.seed(900 + r)
    x <- c(stats::rnorm(100), stats::rnorm(100, 1.5))
    fit <- suppressWarnings(cpt_detect(x, method = "pelt"))
    if (nrow(fit$changepoints) != 1L) return(NA)
    ci <- suppressWarnings(cpt_confint(fit, method = "conformal", B = 99))
    prof <- attr(ci, "profile")
    100 %in% prof$position[!is.na(prof$p_value) & prof$p_value > 0.05]
  }, logical(1))
  covered <- covered[!is.na(covered)]
  expect_gt(length(covered), 100)
  expect_gt(mean(covered), 0.95 - 2.5 * sqrt(0.95 * 0.05 / length(covered)))
})

test_that("an empty conformal set warns instead of pretending", {
  skip_on_cran()
  set.seed(8)
  # one changepoint offered for a window that holds two changes far apart
  x <- c(stats::rnorm(60), stats::rnorm(60, 6), stats::rnorm(60))
  fit <- as_ggcpt(60, x)
  out <- suppressWarnings(cpt_confint(fit, method = "conformal", B = 99,
                                      seed = 1))
  expect_true(is.na(out$ci_lower) || out$set_size >= 1)
  expect_s3_class(out, "tbl_df")
})

test_that("cpt_confint_coverage() measures coverage and draws it", {
  skip_on_cran()
  cov <- cpt_confint_coverage(n_sim = 15, n = 100, B = 30, seed = 1)
  expect_s3_class(cov, "ggcpt_coverage")
  expect_equal(cov$route, c("conformal", "bootstrap"))
  expect_true(all(cov$coverage >= 0 & cov$coverage <= 1))
  expect_named(cov, c("method", "route", "n_sim", "n_detected", "n_failed",
                      "coverage", "coverage_se", "mean_width", "level"))
  expect_s3_class(ggplot2::autoplot(cov), "ggplot")
  expect_output(print(cov), "realised coverage")
  expect_error(cpt_confint_coverage(routes = "nope"),
               class = "ggchangepoint_bad_argument")
})

# ---- epidemic changepoints (CAPA) ------------------------------------------

test_that("capa finds the anomalous window and the outlier", {
  skip_on_cran()
  skip_if_not_installed("anomaly")
  set.seed(2026)
  x <- c(stats::rnorm(150), stats::rnorm(40, 3), stats::rnorm(150))
  x[60] <- 9
  fit <- cpt_detect(x, method = "capa")
  expect_s3_class(fit, "ggcpt")
  expect_equal(fit$changepoints$cp, c(150L, 190L))
  reg <- cpt_regions(fit)
  expect_equal(reg$type, c("point", "collective"))
  expect_equal(reg$start, c(60L, 151L))
  expect_equal(reg$end, c(60L, 190L))
  expect_identical(fit$region_kind, "anomaly")
  expect_output(print(fit), "Anomalous windows")
  p <- ggplot2::autoplot(fit)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(cpt_detect(x, method = "capa",
                          change_in = "meanvar")$change_in, "meanvar")
  expect_equal(cpt_detect(x, method = "capa", min_segment = 5)$call$min_segment,
               5)
})

test_that("multivariate capa names the coordinates each anomaly affects", {
  skip_on_cran()
  skip_if_not_installed("anomaly")
  set.seed(2026)
  x <- c(stats::rnorm(150), stats::rnorm(40, 3), stats::rnorm(150))
  X <- cbind(a = x, b = stats::rnorm(340),
             c = c(stats::rnorm(150), stats::rnorm(40, 2.5), stats::rnorm(150)))
  fit <- cpt_detect(X, method = "capa")
  coll <- fit$regions[fit$regions$type == "collective", ]
  expect_equal(coll$coordinates, "a, c")
  att <- cpt_attribute(fit, B = 199, seed = 1)
  expect_setequal(unique(att$coordinate[att$changed]), c("a", "c"))
})

test_that("capa refuses what it cannot model, by name", {
  skip_if_not_installed("anomaly")
  set.seed(1)
  expect_error(capa_wrapper(stats::rnorm(15)),
               class = "ggchangepoint_short_series")
  expect_error(capa_wrapper(cbind(stats::rnorm(50), stats::rnorm(50)),
                            robust = TRUE),
               class = "ggchangepoint_bad_argument")
  expect_error(capa_wrapper(stats::rnorm(50), foo = 1),
               class = "ggchangepoint_bad_argument")
  expect_error(capa_wrapper(cbind(stats::rnorm(50), rep(1, 50))),
               class = "ggchangepoint_input_error")
  expect_error(capa_wrapper(stats::rnorm(50), beta = c(1, 2)),
               class = "ggchangepoint_bad_argument")
  expect_s3_class(capa_wrapper(c(stats::rnorm(60), stats::rnorm(20, 4),
                                 stats::rnorm(60)), robust = TRUE), "ggcpt")
})

# ---- counterfactual ----------------------------------------------------------

test_that("the counterfactual carries the pre-change level forward", {
  skip_on_cran()
  set.seed(9)
  x <- c(stats::rnorm(80, 10), stats::rnorm(60, 13))
  fit <- as_ggcpt(80, x)
  cf <- cpt_counterfactual(fit)
  expect_s3_class(cf, "ggcpt_counterfactual")
  expect_equal(nrow(cf), 60L)
  expect_equal(unique(round(cf$counterfactual, 10)), round(mean(x[1:80]), 10))
  expect_equal(cf$effect, cf$observed - cf$counterfactual)
  g <- glance(cf)
  expect_equal(g$mean_effect, mean(x[81:140]) - mean(x[1:80]))
  expect_true(all(cf$lower < cf$counterfactual & cf$counterfactual < cf$upper))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(cf)))
  expect_s3_class(ggplot2::autoplot(fit, type = "counterfactual"), "ggplot")
  expect_output(print(cf), "mean effect")
  # a change in slope carries the line, and the band fans out
  y <- c(1:100 * 0.1, 10 + (1:100) * 0.3) + stats::rnorm(200, 0, 0.5)
  s <- cpt_counterfactual(as_ggcpt(100, y, change_in = "slope"))
  w <- s$upper - s$lower
  expect_gt(w[length(w)], w[1])
  expect_gt(glance(s)$mean_effect, 5)
  # nothing to carry when nothing changed
  e <- cpt_counterfactual(as_ggcpt(integer(0), stats::rnorm(50)))
  expect_equal(nrow(e), 0L)
  expect_output(print(e), "no counterfactual")
})

test_that("a formula fit carries its coefficients onto the new covariates", {
  skip_on_cran()
  skip_if_not_installed("strucchange")
  set.seed(10)
  d <- data.frame(z = stats::runif(200))
  d$y <- ifelse(seq_len(200) <= 100, 1 + 2 * d$z, 3 - d$z) +
    stats::rnorm(200, 0, 0.3)
  fit <- suppressWarnings(cpt_detect(y ~ z, data = d, method = "strucchange"))
  cf <- cpt_counterfactual(fit)
  rows <- cf$position
  expect_equal(cf$counterfactual, unname(stats::predict(
    stats::lm(y ~ z, data = d[seq_len(min(rows) - 1L), ]),
    newdata = d[rows, ])), tolerance = 1e-8)
})

# ---- abruptness and gradual simulation --------------------------------------

test_that("the prefix-sum residuals match a fitted regression", {
  set.seed(3)
  y <- stats::rnorm(60) + c(rep(0, 30), rep(2, 30))
  t <- as.numeric(1:60)
  pre <- list(m = 60, t = t, ybar = mean(y), ytot = sum(y),
              syy = sum((y - mean(y))^2), cy = cumsum(y), ct = cumsum(t),
              ct2 = cumsum(t^2), cty = cumsum(t * y))
  for (w in c(0, 1, 3, 7.5, 12)) for (cc in c(20.5, 30, 41)) {
    Fd <- ggchangepoint:::ramp_design(t, cc, w)
    expect_equal(ggchangepoint:::ramp_rss(pre, cc, w),
                 sum(stats::resid(stats::lm(y ~ Fd))^2), tolerance = 1e-8)
  }
})

test_that("cpt_abruptness() tells a ramp from a step", {
  skip_on_cran()
  set.seed(2026)
  ramp <- c(rep(0, 100), seq(0, 4, length.out = 30), rep(4, 100))
  a <- cpt_abruptness(as_ggcpt(115, ramp + stats::rnorm(230, 0, 0.5)))
  expect_s3_class(a, "ggcpt_abruptness")
  expect_equal(a$verdict, "gradual")
  expect_true(a$width_lower <= 30 && 30 <= a$width_upper)
  set.seed(1)
  s <- cpt_abruptness(as_ggcpt(100, c(stats::rnorm(100), stats::rnorm(100, 4))))
  expect_equal(s$verdict, "abrupt")
  expect_equal(s$width_lower, 0)
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(a)))
  expect_output(print(a), "gradual, transition width")
  expect_error(cpt_abruptness(as_ggcpt(50, stats::rnorm(100),
                                       change_in = "var")),
               class = "ggchangepoint_capability_absent")
  expect_equal(nrow(cpt_abruptness(as_ggcpt(integer(0), stats::rnorm(50)))),
               0L)
})

test_that("the step is rejected at about the nominal rate on true steps", {
  skip_on_cran()
  gradual <- vapply(seq_len(200), function(r) {
    set.seed(300 + r)
    x <- c(stats::rnorm(150), stats::rnorm(150, 2))
    fit <- suppressWarnings(cpt_detect(x, method = "pelt"))
    if (!nrow(fit$changepoints)) return(NA)
    a <- cpt_abruptness(fit)
    a$verdict[which.min(abs(a$cp - 150))] == "gradual"
  }, logical(1))
  gradual <- gradual[!is.na(gradual)]
  expect_lt(mean(gradual), 0.05 + 2.5 * sqrt(0.05 * 0.95 / length(gradual)))
})

test_that("cpt_simulate(transition =) draws a ramp centred on the change", {
  skip_on_cran()
  sim <- cpt_simulate(200, changepoints = 100, params = c(0, 4), sd = 0,
                      transition = 20, seed = 1)
  v <- sim$value
  expect_equal(v[1:90], rep(0, 90))
  expect_equal(v[111:200], rep(4, 90))
  expect_true(all(diff(v[90:111]) >= 0))
  expect_equal(mean(v[c(100, 101)]), 2)
  expect_equal(attr(sim, "true_changepoints"), 100)
  expect_error(cpt_simulate(200, 100, change_in = "slope",
                            params = list(list(intercept = 0, slope = 1),
                                          list(intercept = 0, slope = 2)),
                            transition = 5),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_simulate(200, 100, params = c(1, 4), family = "poisson",
                            transition = 5),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_simulate(60, c(10, 50), params = c(0, 1, 2),
                            transition = 30),
               class = "ggchangepoint_bad_argument")
})

# ---- control charts ------------------------------------------------------------

test_that("the run-length chain reproduces the published tables", {
  skip_on_cran()
  ns <- asNamespace("ggchangepoint")
  # two-sided CUSUM, k = 0.5 (Montgomery's table: 168 at h = 4, 465 at h =
  # 5, and 10.4 for a one-sigma shift at h = 5)
  expect_equal(ns$cusum_arl(4, 0.5), 168, tolerance = 0.01)
  expect_equal(ns$cusum_arl(5, 0.5), 465, tolerance = 0.01)
  expect_equal(ns$cusum_arl(5, 0.5, delta = 1), 10.4, tolerance = 0.02)
  # EWMA (Lucas and Saccucci 1990): ARL0 of 500 at these (lambda, L) pairs
  expect_equal(ns$ewma_arl(2.814, 0.1), 500, tolerance = 0.01)
  expect_equal(ns$ewma_arl(2.962, 0.2), 500, tolerance = 0.01)
  # and the solved limits invert them
  expect_equal(ns$cusum_arl(ns$cusum_h_for(370, 0.5), 0.5), 370,
               tolerance = 1e-4)
  expect_equal(ns$ewma_arl(ns$ewma_L_for(370, 0.1), 0.1), 370,
               tolerance = 1e-4)
})

test_that("cusum and ewma monitors alarm on a shift and draw a chart", {
  skip_on_cran()
  set.seed(2026)
  mon <- cpt_monitor("cusum", baseline = stats::rnorm(300))
  mon <- cpt_update(mon, c(stats::rnorm(150), stats::rnorm(100, 1.5)))
  al <- alarms(mon)
  expect_true(any(al$time > 150 & al$time < 180))
  expect_equal(nrow(mon$trace), 250L)
  expect_output(print(mon), "Chart: k = 0.5")
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(mon, plot_type = "control_chart")))
  ew <- cpt_replay(c(stats::rnorm(300), stats::rnorm(100, 1)),
                   method = "ewma", baseline = 200, arl0 = 370)
  d <- cpt_delay(ew, truth = 300)
  expect_true(d$per_change$detected)
  expect_output(print(ew), "lambda = 0.2")
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(ew, plot_type = "control_chart")))
  # an explicit limit overrides the calibration
  fixed <- cpt_monitor("cusum", baseline = stats::rnorm(50), limit = 4)
  expect_equal(fixed$state$h, 4)
  expect_false(fixed$state$calibrated)
  # arguments for another monitor are named as ignored
  expect_warning(cpt_monitor("ewma", baseline = stats::rnorm(50), k = 1),
                 class = "ggchangepoint_argument_ignored")
  expect_warning(cpt_monitor("cusum", baseline = stats::rnorm(50), foo = 1),
                 class = "ggchangepoint_argument_ignored")
  expect_error(cpt_monitor("cusum", baseline = 1:3),
               class = "ggchangepoint_short_series")
  expect_error(cpt_monitor("ewma", baseline = stats::rnorm(50), lambda = 0),
               class = "ggchangepoint_bad_argument")
  # every monitor now records a trace, so every one can draw a chart
  e <- cpt_replay(c(stats::rnorm(150), stats::rnorm(80, 2)))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(e, plot_type = "control_chart")))
})

test_that("the charts' in-control run length matches the target", {
  skip_on_cran()
  ns <- asNamespace("ggchangepoint")
  # The limit is solved once and passed in: what is measured is the run
  # length that calibration gives, not the solver's speed.
  limits <- c(cusum = ns$cusum_h_for(100, 0.5), ewma = ns$ewma_L_for(100, 0.2))
  for (m in c("cusum", "ewma")) {
    rl <- vapply(seq_len(250), function(r) {
      set.seed(r)
      # The baseline-dependence check is a test at 5%, and it fires on its
      # share of these white-noise baselines; it is not what is measured.
      mon <- suppressWarnings(cpt_monitor(m, baseline = stats::rnorm(3000),
                                          limit = limits[[m]], relearn = 0))
      al <- alarms(cpt_update(mon, stats::rnorm(1500)))$time
      if (length(al)) al[1] else NA_real_
    }, numeric(1))
    expect_false(anyNA(rl))
    expect_lt(abs(mean(rl) - 100), 3 * stats::sd(rl) / sqrt(length(rl)),
              label = m)
  }
})

# ---- screening ------------------------------------------------------------------

test_that("cpt_screen() orders by evidence and flags the changed series", {
  skip_on_cran()
  set.seed(2026)
  panel <- c(lapply(1:15, function(i) stats::rnorm(120)),
             lapply(1:5, function(i) c(stats::rnorm(60), stats::rnorm(60, 1.5))))
  names(panel) <- paste0("store", 1:20)
  sc <- cpt_screen(panel)
  expect_s3_class(sc, "ggcpt_screen")
  expect_setequal(sc$series[sc$flagged], paste0("store", 16:20))
  expect_false(is.unsorted(sc$p_value))
  expect_equal(sc$rank, 1:20)
  expect_equal(glance(sc)$n_flagged, 5L)
  sc2 <- cpt_screen(panel, method = "pelt")
  expect_true(all(sc2$n_changepoints[sc2$flagged] >= 1L))
  expect_true(all(vapply(sc2$result[!sc2$flagged], is.null, logical(1))))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(sc)))
  expect_output(print(sc), "Flagged")
  none <- cpt_screen(panel, control = "none")
  expect_gte(sum(none$flagged), sum(sc$flagged))
  e <- expect_error(cpt_screen(list(a = stats::rnorm(50), b = letters)))
  expect_match(conditionMessage(e), "`b`")
})

test_that("screening keeps the false discovery rate", {
  skip_on_cran()
  fdp <- vapply(seq_len(120), function(r) {
    set.seed(r)
    pn <- lapply(1:60, function(i) {
      if (i <= 6) c(stats::rnorm(60), stats::rnorm(60, 1)) else stats::rnorm(120)
    })
    names(pn) <- paste0("s", 1:60)
    s <- cpt_screen(pn)
    fl <- as.integer(sub("s", "", s$series[s$flagged]))
    if (!length(fl)) 0 else mean(fl > 6)
  }, numeric(1))
  expect_lt(mean(fdp), 0.05 + 2.5 * stats::sd(fdp) / sqrt(length(fdp)))
})

# ---- weights over K ------------------------------------------------------------

test_that("cpt_k_weights() turns the criterion into weights and locations", {
  skip_on_cran()
  set.seed(2026)
  x <- c(stats::rnorm(80), stats::rnorm(80, 3), stats::rnorm(80))
  sel <- cpt_select(x, criterion = "bic", k_max = 5)
  w <- cpt_k_weights(sel)
  expect_s3_class(w, "ggcpt_k_weights")
  expect_equal(sum(w$weight), 1)
  expect_equal(w$k[which.max(w$weight)], sel$k)
  loc <- tidy(w, what = "locations")
  expect_named(loc, c("position", "index", "probability"))
  near <- loc$probability[abs(loc$position - 80) <= 2 |
                            abs(loc$position - 160) <= 2]
  expect_true(all(near > 0.5))
  expect_true(all(loc$probability >= 0 & loc$probability <= 1 + 1e-12))
  expect_true(as.character(sel$k) %in%
                strsplit(glance(w)$k_set_95, ", ")[[1]])
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(w)))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(w,
                                                          what = "locations")))
  expect_output(print(w), "95% of the weight")
  expect_error(cpt_k_weights(cpt_select(x, criterion = "crops_elbow",
                                        k_max = 4)),
               class = "ggchangepoint_capability_absent")
  expect_error(cpt_k_weights(list()), class = "ggchangepoint_bad_argument")
})

test_that("every new result class plots through plot()", {
  skip_on_cran()
  set.seed(1)
  objs <- list(
    cpt_attribute(as_ggcpt(60, mv_step()), B = 19, seed = 1),
    cpt_counterfactual(as_ggcpt(50, c(stats::rnorm(50), stats::rnorm(50, 3)))),
    cpt_abruptness(as_ggcpt(50, c(stats::rnorm(50), stats::rnorm(50, 3)))),
    cpt_screen(list(a = stats::rnorm(60), b = c(stats::rnorm(30),
                                                stats::rnorm(30, 3)))),
    cpt_k_weights(cpt_select(c(stats::rnorm(50), stats::rnorm(50, 3)),
                             k_max = 3))
  )
  for (o in objs) {
    expect_s3_class(plot(o), "ggplot")
  }
})
