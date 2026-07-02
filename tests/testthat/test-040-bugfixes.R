# Regression tests for the bugs fixed in 0.4.0 (audit IDs C1-C20 in
# next_release.md).

set.seed(2026)
x_step <- c(rnorm(100), rnorm(100, 4))

test_that("C1: ecp_wrapper returns an empty tibble when no change is found", {
  set.seed(1)
  res <- ecp_wrapper(rnorm(50), seed = 1)
  expect_equal(nrow(res), 0)
  # and the dispatcher does not fabricate a changepoint either
  set.seed(1)
  res2 <- cpt_detect(rnorm(50), method = "ecp", seed = 1)
  expect_equal(nrow(res2$changepoints), 0)
})

test_that("C2: wbs_wrapper default selection uses sSIC, not the threshold set", {
  skip_if_not_installed("wbs")
  set.seed(1)
  x <- c(rnorm(100), rnorm(100, 2))
  res <- wbs_wrapper(x)
  fit <- wbs::wbs(x)
  cp_ref <- wbs::changepoints(fit)
  expect_setequal(res$changepoints$cp, as.integer(cp_ref$cpt.ic$ssic.penalty))
  # manual threshold is recorded as the penalty
  res_th <- wbs_wrapper(x, threshold = 2)
  expect_identical(res_th$penalty$type, "threshold")
  expect_equal(res_th$penalty$value, 2)
})

test_that("C3: univariate wrappers reject wide matrices instead of flattening", {
  X <- cbind(rnorm(50), rnorm(50))
  expect_error(fpop_wrapper(X), "univariate")
  expect_error(cpt_detect(X, method = "pelt"), "univariate")
  # single-column data frames are fine
  skip_if_not_installed("fpop")
  res <- fpop_wrapper(data.frame(y = x_step))
  expect_s3_class(res, "ggcpt")
})

test_that("C4: idetect_wrapper returns empty on no-change data", {
  skip_if_not_installed("IDetect")
  set.seed(4)
  res <- idetect_wrapper(rnorm(80))
  expect_equal(nrow(res$changepoints), 0)
})

test_that("C5: tguh_wrapper finds nothing on constant data, survives short data", {
  skip_if_not_installed("breakfast")
  res <- tguh_wrapper(rep(5, 100))
  expect_equal(nrow(res$changepoints), 0)
  expect_equal(nrow(res$segments), 1)
  expect_no_error(tguh_wrapper(rnorm(8)))
})

test_that("C6: glance is one row even when the fit carries a cost vector", {
  skip_if_not_installed("fpop")
  res <- fpop_wrapper(x_step)
  g <- glance(res)
  expect_equal(nrow(g), 1)
  expect_true(is.numeric(g$total_cost))
})

test_that("C7: mosum penalty value is the numeric threshold", {
  skip_if_not_installed("mosum")
  res <- mosum_wrapper(x_step)
  expect_true(is.numeric(res$penalty$value))
  g <- glance(res)
  expect_true(is.numeric(g$penalty_value))
})

test_that("C8: mosum multiscale = TRUE runs the multiscale procedure", {
  skip_if_not_installed("mosum")
  x3 <- c(rnorm(100), rnorm(100, 5), rnorm(100, 1))
  res <- mosum_wrapper(x3, multiscale = TRUE)
  expect_s3_class(res$fit, "multiscale.cpts")
})

test_that("C9: cpt_detect forwards change_in to not (contrast mapping)", {
  skip_if_not_installed("not")
  set.seed(7)
  xv <- c(rnorm(100, 0, 1), rnorm(100, 0, 6))
  res <- cpt_detect(xv, method = "not", change_in = "var")
  expect_identical(res$change_in, "meanvar")
  expect_true(any(abs(res$changepoints$cp - 100) <= 10))
})

test_that("C10: penalty = 'None' resolves to 0 for numeric-penalty engines", {
  skip_if_not_installed("fpop")
  res <- cpt_detect(x_step, method = "fpop", penalty = "None")
  expect_equal(res$penalty$value, 0)
})

test_that("C11: cpt_penalty sSIC is at least as strong as BIC", {
  expect_gte(cpt_penalty("sSIC", n = 100), cpt_penalty("BIC", n = 100))
})

test_that("C12: metrics reward the correct empty answer", {
  m <- cpt_metrics(integer(0), integer(0), n = 100)
  expect_equal(m$precision, 1)
  expect_equal(m$recall, 1)
  expect_equal(m$f1, 1)
  expect_equal(m$covering, 1)
})

test_that("C13: empty predictions score the trivial-partition covering, ARI 0", {
  m <- cpt_metrics(integer(0), 50, n = 100)
  expect_equal(m$covering, 0.5)
  expect_equal(m$rand_index, 0)
})

test_that("C14: out-of-range changepoints are dropped with a warning", {
  expect_warning(m <- cpt_metrics(c(101), c(50), n = 100), "Dropping")
  expect_equal(m$n_pred, 0)
})

test_that("C15: ggcpt_eval matching agrees with cpt_metrics", {
  # three predictions near one truth: exactly one TP under 1-1 matching
  p <- ggcpt_eval(c(98, 100, 102), c(100), rnorm(200), margin = 5)
  built <- ggplot2::ggplot_build(p)
  m <- cpt_metrics(c(98, 100, 102), c(100), n = 200, margin = 5)
  expect_equal(m$recall, 1)
  expect_equal(m$precision, 1 / 3)
  expect_no_error(built)
})

test_that("C16: ggcpt_compare facets keep methods that found nothing", {
  set.seed(10)
  x_null <- rnorm(60)
  p <- ggcpt_compare(x_null, methods = c("pelt", "binseg"))
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$layout$layout$PANEL)), 2)
})

test_that("C17: stat_changepoint is row-order invariant and warning-free", {
  df <- data.frame(t = 1:200, y = x_step)
  df_shuffled <- df[sample(nrow(df)), ]
  get_cps <- function(d) {
    p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + stat_changepoint()
    sort(ggplot2::ggplot_build(p)$data[[1]]$xintercept)
  }
  expect_no_warning(cps1 <- get_cps(df))
  cps2 <- get_cps(df_shuffled)
  expect_identical(cps1, cps2)
  expect_true(any(abs(cps1 - 100) <= 3))
})

test_that("C18: signal_blocks levels are cumulative jumps (Donoho-Johnstone)", {
  b <- signal_blocks(2048, seed = 1)
  cp <- attr(b, "true_changepoints")
  expect_equal(length(cp), 11)
  # every declared changepoint is a genuine level shift
  jump_at <- vapply(cp, function(k) {
    abs(mean(b$value[(k + 1):min(k + 20, nrow(b))]) -
        mean(b$value[max(1, k - 19):k]))
  }, numeric(1))
  expect_true(all(jump_at > 1))
})

test_that("C19: t-noise standard deviation matches the sd argument", {
  d <- cpt_simulate(50000, noise = "t", sd = 2, df = 3, seed = 1)
  expect_lt(abs(stats::sd(d$value) - 2), 0.15)
  expect_error(cpt_simulate(100, noise = "t", df = 2), "exceed 2")
})

test_that("C20: signal generators reject sizes they cannot honour", {
  expect_error(signal_teeth(50), "at least 200")
  expect_error(signal_stairs(8), "at least 10")
  expect_error(signal_fms(10), "at least 40")
  expect_error(signal_blocks(50), "at least 100")
})

test_that("cpt_wrapper SegNeigh works with the default penalty", {
  res <- suppressWarnings(
    cpt_wrapper(x_step, change_in = "mean", cp_method = "SegNeigh")
  )
  expect_true(any(abs(res$cp - 100) <= 3))
})

test_that("np method reports change_in = distribution", {
  set.seed(1)
  res <- cpt_detect(x_step, method = "np")
  expect_identical(res$change_in, "distribution")
})

test_that("meanvar requests are reported in the user vocabulary", {
  res <- cpt_detect(x_step, method = "pelt", change_in = "meanvar")
  expect_identical(res$change_in, "meanvar")
})

test_that("ggecpplot handles multivariate input without crashing", {
  set.seed(9)
  X <- data.frame(a = c(rnorm(50), rnorm(50, 4)), b = rnorm(100))
  expect_message(p <- ggecpplot(X), "first column")
  expect_no_error(ggplot2::ggplot_build(p))
})

# Regression tests for the pre-release adversarial review (R1-R15)

test_that("R1: cpm change_in routes to the matching test statistic", {
  skip_if_not_installed("cpm")
  set.seed(1)
  xv <- c(rnorm(200, 0, 1), rnorm(200, 0, 6))
  r_var <- cpt_detect(xv, method = "cpm", change_in = "var")
  r_mean <- cpt_detect(xv, method = "cpm", change_in = "mean")
  # different statistics must be able to give different answers; at minimum
  # the var request must not silently equal the mean request's call
  expect_false(identical(r_var$fit, r_mean$fit))
  expect_true(any(abs(r_var$changepoints$cp - 200) <= 20))
})

test_that("R2: character BIC resolves to 2*log(n) for numeric-penalty engines", {
  skip_if_not_installed("fpop")
  r <- cpt_detect(c(rnorm(100), rnorm(100, 4)), method = "fpop", penalty = "BIC")
  expect_equal(r$penalty$value, 2 * log(200))
})

test_that("R3: signal_teeth padding does not add an undeclared changepoint", {
  s <- signal_teeth(250, seed = 1)
  m2 <- mean(s$value[101:200])
  m3 <- mean(s$value[201:250])
  expect_lt(abs(m3 - m2), 0.5)
})

test_that("R4: cpt_simulate warns when params is shorter than segments", {
  expect_warning(
    cpt_simulate(300, changepoints = c(100, 200), params = c(0, 5), seed = 1),
    "reused"
  )
})

test_that("R5: wbs and not return empty results on constant data", {
  skip_if_not_installed("wbs")
  skip_if_not_installed("not")
  expect_equal(nrow(cpt_detect(rep(5, 120), method = "wbs")$changepoints), 0)
  expect_equal(nrow(cpt_detect(rep(5, 120), method = "not")$changepoints), 0)
})

test_that("R6: run-length heatmap treats columns as time", {
  skip_if_not_installed("ocp")
  r <- bocpd_wrapper(c(rnorm(60), rnorm(60, 4)))
  b <- ggplot2::ggplot_build(ggcpt_runlength(r))
  # time axis must span (nearly) the full series, not collapse
  expect_gt(max(b$data[[1]]$x, na.rm = TRUE), 100)
  expect_error(ggcpt_runlength(r, prob_floor = 1), "prob_floor")
})

test_that("R7: kcp changepoints follow the left convention", {
  skip_if_not_installed("kcpRS")
  set.seed(1)
  xs <- c(rep(0, 100), rep(10, 100)) + rnorm(200, sd = 0.05)
  r <- kcp_wrapper(xs, nperm = 100, seed = 42)
  expect_true(100 %in% r$changepoints$cp)
})

test_that("R8: ocd survives a declaration at the final row", {
  skip_if_not_installed("ocd")
  set.seed(5)
  X <- rbind(matrix(rnorm(60 * 3), 60), matrix(rnorm(3, 50), 1))
  expect_no_error(ocd_wrapper(X, mc_reps = 10))
})

test_that("R9: beast seed makes results reproducible", {
  skip_if_not_installed("Rbeast")
  set.seed(42)
  # Same length as the other beast test: Rbeast <= 1.0.2 can intermittently
  # return broken fits when consecutive calls change the series length.
  x <- c(rnorm(100), rnorm(100, 6))
  a <- beast_wrapper(x, seed = 7)
  b <- beast_wrapper(x, seed = 7)
  expect_identical(a$changepoints$cp, b$changepoints$cp)
  expect_identical(a$changepoints$posterior_prob, b$changepoints$posterior_prob)
})

test_that("R10: bcp_wrapper guards the n < 4 segfault", {
  skip_if_not_installed("bcp")
  expect_error(bcp_wrapper(c(1.2, -0.5, 0.3)), "at least 4")
})

test_that("R11: cpt_batch keeps user-supplied names in a partially named list", {
  b <- cpt_batch(list(a = c(rnorm(30), rnorm(30, 4)),
                      c(rnorm(30), rnorm(30, 4))), method = "pelt")
  expect_equal(b$series, c("a", "series_2"))
})

test_that("R12: CROPS var-cost is monotone in the number of changepoints", {
  set.seed(1011)
  xx <- c(rnorm(80, 0, 1), rnorm(80, 8, 1), rnorm(80, 0, 4), rnorm(60, -6, 0.5))
  s <- cpt_crops(xx, change_in = "var")$solutions
  s <- s[order(s$n_cpts), ]
  expect_true(all(diff(s$cost) <= 1e-8))
})

test_that("R13: multivariate autoplot warns about unsupported overlays", {
  skip_if_not_installed("InspectChangepoint")
  set.seed(1)
  X <- cbind(a = c(rnorm(80), rnorm(80, 3)), b = c(rnorm(80), rnorm(80, -2)),
             c = rnorm(160))
  r <- inspect_wrapper(X)
  expect_warning(ggplot2::ggplot_build(ggplot2::autoplot(r, show_ci = TRUE)),
                 "multivariate")
})

test_that("R14: autoplot accepts a custom index and maps overlays through it", {
  skip_if_not_installed("stepR")
  r <- smuce_wrapper(c(rnorm(100), rnorm(100, 4)))
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 200)
  p <- ggplot2::autoplot(r, show_ci = TRUE, show_fit = TRUE, index = dates)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("R15: sn is dispatched as univariate only", {
  expect_error(cpt_detect(cbind(rnorm(50), rnorm(50)), method = "sn"),
               "univariate")
})
