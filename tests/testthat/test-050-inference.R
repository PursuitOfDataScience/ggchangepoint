# 0.5.0: inference. Regions, the unified confidence-interval contract, and
# the honesty of cpt_test().

set.seed(2026)
x_step <- c(rnorm(100), rnorm(100, 4))

test_that("nsp returns regions and flags its cp column as a midpoint", {
  skip_if_not_installed("nsp")
  fit <- nsp_wrapper(x_step, M = 100, seed = 1)
  expect_s3_class(fit, "ggcpt")
  expect_equal(fit$method, "nsp")
  reg <- cpt_regions(fit)
  expect_true(all(c("start", "end", "length") %in% names(reg)))
  expect_true(all(reg$end >= reg$start))
  expect_gte(nrow(reg), 1)
  # The interval must actually bracket the true change.
  expect_true(any(reg$start <= 100 & reg$end >= 100))
  expect_true(isTRUE(fit$cp_from_regions))
  expect_equal(fit$region_level, 0.1)
  expect_true("cp_source" %in% names(fit$changepoints))
  expect_true(all(fit$changepoints$cp_source == "region_midpoint"))
  expect_output(print(fit), "midpoint of each region")

  p <- ggplot2::autoplot(fit)
  expect_no_error(ggplot2::ggplot_build(p))
  # Regions are shaded by default and drawn beneath the series.
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
})

test_that("cpt_regions is empty and typed for a result with no regions", {
  fit <- cpt_detect(x_step, method = "pelt")
  reg <- cpt_regions(fit)
  expect_equal(nrow(reg), 0)
  expect_true(all(c("start", "end", "length") %in% names(reg)))
  expect_warning(ggplot2::autoplot(fit, show_regions = TRUE),
                 "carries no significance regions")
  expect_error(cpt_regions(1:10), "must be a ggcpt object")
})

test_that("nsp validates its own arguments", {
  skip_if_not_installed("nsp")
  expect_error(nsp_wrapper(x_step, alpha = 0), "greater than 0")
  expect_error(nsp_wrapper(x_step, alpha = 1), "less than 1")
  expect_error(nsp_wrapper(x_step, variant = "tvreg", M = 20),
               "needs `covariates`")
  expect_error(nsp_wrapper(x_step, variant = "tvreg", M = 20,
                           covariates = matrix(1, nrow = 5)),
               "one row per observation")
})

test_that("cpt_confint reports one contract and four provenances", {
  fit <- cpt_detect(x_step, method = "pelt")
  ci <- cpt_confint(fit, method = "bootstrap", B = 15, seed = 1)
  expect_true(all(c("cp", "ci_lower", "ci_upper", "level", "source") %in%
                    names(ci)))
  expect_equal(unique(ci$source), "bootstrap")
  expect_true(all(ci$ci_lower <= ci$cp & ci$cp <= ci$ci_upper))
  expect_true(all(ci$ci_lower >= 1 & ci$ci_upper < nrow(fit$data)))

  # "auto" picks native when the engine supplies one.
  skip_if_not_installed("stepR")
  sm <- smuce_wrapper(x_step)
  auto <- cpt_confint(sm)
  expect_equal(unique(auto$source), "native")
  expect_equal(auto$ci_lower, as.integer(sm$changepoints$ci_lower))
})

test_that("cpt_confint refuses what it cannot do, and handles empties", {
  fit <- cpt_detect(x_step, method = "pelt")
  expect_error(cpt_confint(fit, method = "native"),
               "carries no engine confidence intervals")
  expect_error(cpt_confint(fit, method = "posterior"),
               "no posterior changepoint-probability profile")
  flat <- cpt_detect(rnorm(50), method = "pelt")
  if (nrow(flat$changepoints) == 0) {
    out <- cpt_confint(flat)
    expect_equal(nrow(out), 0)
    expect_true(all(c("cp", "ci_lower", "ci_upper", "level", "source") %in%
                      names(out)))
  }
  expect_error(cpt_confint(fit, level = 1.5), "less than 1")
})

test_that("a posterior credible interval brackets its changepoint", {
  skip_if_not_installed("bcp")
  fit <- bcp_wrapper(x_step, seed = 1)
  skip_if(nrow(fit$changepoints) == 0)
  ci <- cpt_confint(fit, method = "posterior", level = 0.9)
  expect_equal(unique(ci$source), "posterior")
  expect_true(all(ci$ci_lower <= ci$cp & ci$cp <= ci$ci_upper))
})

test_that("a date index is carried onto the interval table", {
  dates <- as.Date("2020-01-01") + seq_along(x_step) - 1
  fit <- cpt_detect(x_step, method = "pelt", index = dates)
  ci <- cpt_confint(fit, method = "bootstrap", B = 10, seed = 1)
  expect_s3_class(ci$cp_index, "Date")
  expect_s3_class(ci$ci_lower_index, "Date")
})

test_that("cpt_test marks unadjusted p-values as unadjusted", {
  fit <- cpt_detect(x_step, method = "pelt")
  expect_warning(res <- cpt_test(fit), "anti-conservative")
  expect_true(all(c("cp", "estimate", "statistic", "p_value", "method",
                    "selection_adjusted") %in% names(res)))
  expect_false(any(res$selection_adjusted))
  expect_equal(nrow(res), nrow(fit$changepoints))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))
  # The estimate is the level difference across the changepoint.
  expect_gt(res$estimate[1], 2)

  expect_warning(seg <- cpt_test(fit, type = "segment"),
                 "`selection_adjusted` is FALSE")
  expect_true("seg_id" %in% names(seg))
  expect_equal(nrow(seg), nrow(fit$segments) - 1)

  expect_warning(adj <- cpt_test(fit, correction = "holm"),
                 "`selection_adjusted` is FALSE")
  expect_true("p_adjusted" %in% names(adj))
  expect_true(all(adj$p_adjusted >= adj$p_value - 1e-12))
})

test_that("cpt_test uses a native test when the engine has one", {
  skip_if_not_installed("strucchange")
  fit <- strucchange_wrapper(x_step)
  skip_if(nrow(fit$changepoints) == 0)
  res <- cpt_test(fit)
  expect_true(all(res$selection_adjusted))
  expect_match(res$method[1], "Chow")
})

test_that("cpt_test on an empty result returns a typed zero-row tibble", {
  empty <- as_ggcpt(integer(0), x_step)
  res <- cpt_test(empty)
  expect_equal(nrow(res), 0)
  expect_true(all(c("cp", "estimate", "p_value", "selection_adjusted") %in%
                    names(res)))
})

test_that("the region geom builds and defaults to the panel height", {
  d <- data.frame(t = seq_along(x_step), y = x_step)
  regions <- data.frame(xmin = 90, xmax = 110)
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax), data = regions) +
    ggplot2::geom_line()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("cpt_confint reads NSP's own interval instead of bootstrapping", {
  skip_if_not_installed("nsp")
  set.seed(51)
  x <- c(stats::rnorm(100), stats::rnorm(100, 5))
  fit <- cpt_detect(x, method = "nsp")
  skip_if(nrow(fit$changepoints) == 0)

  ci <- cpt_confint(fit)
  expect_equal(ci$source, rep("nsp_region", nrow(ci)))
  # the interval is NSP's region, not a resample of it
  expect_equal(ci$ci_lower, as.integer(fit$changepoints$region_start))
  expect_equal(ci$ci_upper, as.integer(fit$changepoints$region_end))
  # and it is reported at NSP's own global level
  expect_equal(ci$level, rep(1 - fit$region_level, nrow(ci)))
  expect_true(all(ci$ci_lower <= ci$cp & ci$cp <= ci$ci_upper))

  # engines that fill ci_lower/ci_upper keep the "native" label
  skip_if_not_installed("stepR")
  sm <- cpt_confint(cpt_detect(x, method = "smuce"))
  expect_equal(sm$source, rep("native", nrow(sm)))

  # a result with neither still falls back, and says so
  pelt <- cpt_detect(x, method = "pelt")
  expect_null(ggchangepoint:::native_bounds(pelt))
  bs <- cpt_confint(pelt, method = "bootstrap", B = 20, seed = 1)
  expect_equal(bs$source, rep("bootstrap", nrow(bs)))
})
