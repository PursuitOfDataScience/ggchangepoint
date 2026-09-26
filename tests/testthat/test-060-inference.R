# 0.6.0: how big, at a date fixed in advance, anywhere at all, and whether
# the assumptions hold (§111, §119, §121.3, §152, §214).

test_that("cpt_effect() sizes each change in the data's units and in sd", {
  skip_on_cran()
  set.seed(61)
  fit <- cpt_detect(c(stats::rnorm(100, 10), stats::rnorm(100, 13)),
                    method = "pelt")
  ef <- cpt_effect(fit)
  expect_s3_class(ef, "ggcpt_effect")
  expect_true(all(c("cp", "before", "after", "n_before", "n_after", "delta",
                    "delta_lower", "delta_upper", "delta_std", "pct_change",
                    "sd_before", "sd_after", "sd_ratio",
                    "selection_adjusted", "method") %in% names(ef)))
  expect_equal(ef$delta, 3, tolerance = 0.15)
  expect_true(ef$delta_lower < 3 && 3 < ef$delta_upper)
  expect_equal(ef$delta_std, 3, tolerance = 0.25)
  expect_equal(ef$pct_change, 30, tolerance = 0.15)
  expect_false(ef$selection_adjusted)
  expect_output(print(ef), "selection")
  expect_s3_class(ggplot2::autoplot(ef), "ggplot")
  expect_s3_class(plot(ef), "ggplot")
  expect_error(cpt_effect(1:10), class = "ggchangepoint_bad_argument")
})

test_that("a family's effect is its rate or odds ratio", {
  skip_on_cran()
  skip_if_not_installed("changepoint")
  set.seed(62)
  counts <- c(stats::rpois(150, 4), stats::rpois(150, 8))
  ef <- cpt_effect(cpt_detect(counts, method = "pelt", family = "poisson"))
  expect_true("rate_ratio" %in% names(ef))
  expect_equal(ef$rate_ratio, 2, tolerance = 0.2)
  expect_true(ef$rate_ratio_lower < 2 && 2 < ef$rate_ratio_upper)
})

test_that("the split estimate is marked as adjusted for selection", {
  skip_on_cran()
  set.seed(63)
  x <- c(stats::rnorm(150), stats::rnorm(150, 2))
  fit <- cpt_detect(x, method = "pelt")
  ef <- cpt_effect(fit, method = "split", seed = 1)
  expect_true(all(ef$selection_adjusted))
  expect_equal(ef$delta, 2, tolerance = 0.25)
  # Reproducible under its seed.
  expect_equal(cpt_effect(fit, method = "split", seed = 1)$delta, ef$delta)
})

test_that("a regression fit's effects are per term", {
  skip_on_cran()
  skip_if_not_installed("strucchange")
  set.seed(64)
  d <- data.frame(x = stats::rnorm(160))
  d$y <- c(1 + 2 * d$x[1:80], 1 - 1 * d$x[81:160]) + stats::rnorm(160, 0, 0.3)
  ef <- cpt_effect(cpt_detect(y ~ x, data = d, method = "strucchange"))
  expect_true("term" %in% names(ef))
  expect_equal(ef$delta[ef$term == "x"], -3, tolerance = 0.2)
  expect_error(cpt_effect(cpt_detect(y ~ x, data = d, method = "strucchange"),
                          method = "split"),
               class = "ggchangepoint_capability_absent")
})

test_that("cpt_test_at() tests a date fixed in advance", {
  skip_on_cran()
  set.seed(65)
  dates <- as.Date("2026-01-01") + 0:119
  x <- c(stats::rnorm(60), stats::rnorm(60, 1))
  r <- cpt_test_at(x, when = as.Date("2026-03-02"), index = dates)
  expect_s3_class(r, "ggcpt_test_at")
  expect_equal(r$cp, 60L)
  expect_equal(r$n_before, 60L)
  expect_lt(r$p_value, 0.001)
  expect_true(r$selection_adjusted)
  expect_true(r$conf_low < 1 && 1 < r$conf_high)
  expect_output(print(r), "p")
  # A position means the first observation of the new regime.
  expect_equal(cpt_test_at(x, when = 61)$cp, 60L)
  # A stable stretch: no change there.
  expect_gt(cpt_test_at(x[1:60], when = 31)$p_value, 0.01)
  # A fit carries its own index.
  fit <- cpt_detect(x, method = "pelt", index = dates)
  expect_equal(cpt_test_at(fit, when = as.Date("2026-03-02"))$cp, 60L)
  expect_error(cpt_test_at(x), class = "ggchangepoint_bad_argument")
  expect_error(cpt_test_at(x, when = 500), class = "ggchangepoint_bad_argument")
})

test_that("a window pays for its search with a permutation p-value", {
  skip_on_cran()
  set.seed(66)
  x <- c(stats::rnorm(60), stats::rnorm(60, 1.5))
  r <- cpt_test_at(x, when = 58, window = 5, B = 199, seed = 1)
  expect_equal(r$window, 5)
  expect_lt(r$p_value, 0.05)
  expect_equal(cpt_test_at(x, when = 58, window = 5, B = 199, seed = 1)$p_value,
               r$p_value)
  noise <- stats::rnorm(120)
  expect_gt(cpt_test_at(noise, when = 60, window = 10, B = 199,
                        seed = 2)$p_value, 0.01)
})

test_that("each family has its own exact test", {
  skip_on_cran()
  set.seed(67)
  counts <- c(stats::rpois(80, 3), stats::rpois(80, 7))
  r <- cpt_test_at(counts, when = 81, family = "poisson")
  expect_lt(r$p_value, 0.001)
  expect_equal(r$estimate, 7 / 3, tolerance = 0.3)
  b <- c(stats::rbinom(100, 1, 0.2), stats::rbinom(100, 1, 0.6))
  expect_lt(cpt_test_at(b, when = 101, family = "binomial")$p_value, 0.001)
  w <- c(stats::rexp(80, 1), stats::rexp(80, 4))
  expect_lt(cpt_test_at(w, when = 81, family = "exponential")$p_value, 0.001)
  heavy <- c(stats::rt(80, 2), stats::rt(80, 2) + 2)
  expect_lt(cpt_test_at(heavy, when = 81, family = "l1")$p_value, 0.01)
  v <- c(stats::rnorm(80), stats::rnorm(80, 0, 3))
  expect_lt(cpt_test_at(v, when = 81, change_in = "var")$p_value, 0.001)
  expect_error(cpt_test_at(c(1.5, counts), when = 81, family = "poisson"),
               class = "ggchangepoint_bad_type")
})

test_that("a formula tests a break in a regression at the date (Chow)", {
  skip_on_cran()
  set.seed(68)
  d <- data.frame(x = stats::rnorm(120))
  d$y <- c(1 + d$x[1:60], 1 + 3 * d$x[61:120]) + stats::rnorm(120, 0, 0.5)
  r <- cpt_test_at(y ~ x, data = d, when = 61)
  expect_lt(r$p_value, 0.001)
  expect_match(r$method, "Chow")
})

test_that("cpt_attribute_event() says whether an event explains a change", {
  skip_on_cran()
  skip_if_not(engine_usable("stepR"))
  set.seed(69)
  dates <- as.Date("2026-01-01") + 0:119
  fit <- cpt_detect(c(stats::rnorm(60), stats::rnorm(60, 3)),
                    method = "smuce", index = dates)
  at <- cpt_attribute_event(fit, as.Date(c("2026-03-01", "2026-04-20")))
  expect_equal(nrow(at), 2L)
  expect_true(all(c("event", "event_position", "cp", "ci_lower", "ci_upper",
                    "distance", "inside", "verdict") %in% names(at)))
  expect_true(at$inside[1])
  expect_false(at$inside[2])
  flat <- cpt_detect(stats::rnorm(80), method = "smuce")
  if (!nrow(flat$changepoints)) {
    expect_error(cpt_attribute_event(flat, 40),
                 class = "ggchangepoint_capability_absent")
  }
})

test_that("cpt_test_null() tests for a change anywhere", {
  skip_on_cran()
  set.seed(70)
  noise <- stats::rnorm(200)
  shifted <- c(stats::rnorm(100), stats::rnorm(100, 1))
  for (m in c("cusum", "pettitt")) {
    expect_gt(cpt_test_null(noise, method = m)$p_value, 0.01)
    r <- cpt_test_null(shifted, method = m)
    expect_lt(r$p_value, 0.001)
    expect_true(abs(r$location - 100) <= 10)
    expect_true(r$selection_adjusted)
  }
  fit <- cpt_detect(shifted, method = "pelt")
  expect_equal(cpt_test_null(fit)$p_value, cpt_test_null(shifted)$p_value)
  skip_if_not_installed("strucchange")
  expect_lt(cpt_test_null(shifted, method = "supF")$p_value, 0.001)
  expect_error(cpt_test_null(cbind(noise, noise)),
               class = "ggchangepoint_wrong_dimension")
})

test_that("the size of the null distribution is right for the CUSUM test", {
  skip_on_cran()
  set.seed(71)
  p <- vapply(1:200, function(i) cpt_test_null(stats::rnorm(150))$p_value,
              numeric(1))
  # Asymptotic, and conservative at this length; never anti-conservative.
  expect_lt(mean(p < 0.05), 0.09)
})

test_that("an empty answer says which kind of empty it is", {
  skip_on_cran()
  flat <- cpt_detect(rep(3, 60), method = "pelt") |> suppressWarnings()
  expect_output(print(flat), "constant")
  np <- cpt_null_power(flat)
  expect_true(np$constant)
  expect_output(print(np), "constant")
  set.seed(72)
  quiet <- cpt_detect(stats::rnorm(100), method = "pelt")
  expect_output(print(quiet), "cpt_null_power")
  np <- cpt_null_power(quiet, n_sim = 20, seed = 1)
  expect_s3_class(np, "ggcpt_null_power")
  expect_true(is.na(np$jump) || np$jump > 0)
  expect_output(print(np), "power")
})

test_that("cpt_test(relevance =) tests for a change worth acting on", {
  skip_on_cran()
  set.seed(73)
  x <- c(stats::rnorm(200), stats::rnorm(200, 0.4))
  fit <- cpt_detect(x, method = "pelt", penalty = "BIC")
  skip_if(nrow(fit$changepoints) == 0L)
  # Both warn that the locations were chosen from these data; that is
  # cpt_test()'s standing caveat, not what this test is about.
  plain <- suppressWarnings(cpt_test(fit))
  rel <- suppressWarnings(cpt_test(fit, relevance = 1))
  expect_true("relevance" %in% names(rel))
  expect_true(all(rel$p_value >= plain$p_value, na.rm = TRUE))
  expect_match(rel$method[1], "relevance")
})

test_that("cpt_assumptions() flags dependent noise and names the fix", {
  skip_on_cran()
  set.seed(74)
  ar <- as.numeric(stats::arima.sim(list(ar = 0.8), 400))
  fit <- cpt_detect(ar, method = "pelt") |> suppressWarnings()
  a <- cpt_assumptions(fit)
  expect_s3_class(a, "ggcpt_assumptions")
  expect_equal(a$component, c("residual_dependence", "scale_sensitivity",
                              "expected_false_positives",
                              "count_plausibility", "data_type"))
  expect_true(a$flag[a$component == "residual_dependence"])
  expect_match(a$advice[a$component == "residual_dependence"], "cpt_detect")
  expect_output(print(a), "!")
  # A clean fit raises nothing.
  clean <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 4)),
                      method = "pelt")
  expect_false(any(cpt_assumptions(clean)$flag, na.rm = TRUE))
  # The fit records the dependence check itself.
  expect_true(is.list(fit$diagnostics$residual_dependence))
})

test_that("cpt_gof() and the diagnostics plot check each segment", {
  skip_on_cran()
  set.seed(75)
  fit <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 3, 2)),
                    method = "pelt")
  g <- cpt_gof(fit)
  expect_s3_class(g, "ggcpt_gof")
  expect_equal(nrow(g), nrow(fit$segments))
  expect_true(all(c("seg_id", "start", "end", "n", "mean", "sd", "acf1",
                    "ljung_box_p", "shapiro_p", "too_short") %in% names(g)))
  expect_false(is.null(attr(g, "overall")))
  expect_output(print(g), "seg_id")
  p <- ggplot2::autoplot(fit, type = "diagnostics")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("cpt_robustness() sweeps the noise model and marks the powerless", {
  skip_on_cran()
  set.seed(76)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3)) +
    as.numeric(stats::arima.sim(list(ar = 0.6), 200))
  rb <- cpt_robustness(x, method = "pelt")
  expect_s3_class(rb, "ggcpt_robustness")
  expect_equal(rb$settings$setting,
               c("change_in = \"mean\"", "change_in = \"meanvar\""))
  expect_true(all(c("cp", "found_by", "of", "survives") %in%
                    names(rb$changepoints)))
  expect_output(print(rb), "Reference changepoints")
  expect_s3_class(ggplot2::autoplot(rb), "ggplot")
  expect_s3_class(plot(rb), "ggplot")
  # From a fit.
  fit <- cpt_detect(x, method = "pelt")
  expect_equal(cpt_robustness(fit)$changepoints$cp, rb$changepoints$cp)
  e <- expect_error(cpt_robustness(x, method = "wbs"),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "decafs")
  custom <- cpt_robustness(x, method = "pelt", settings = list(
    bic = list(penalty = "BIC"), mbic = list(penalty = "MBIC")))
  expect_equal(custom$settings$setting, c("bic", "mbic"))
  expect_error(cpt_robustness(x, settings = list(a = list())),
               class = "ggchangepoint_bad_argument")
})

test_that("cpt_recommend() answers with calls and measured numbers", {
  skip_on_cran()
  rec <- cpt_recommend()
  expect_s3_class(rec, "ggcpt_recommendation")
  expect_true(all(c("method", "engine", "installed", "score", "tie", "call",
                    "hits", "false_positives", "why", "caveat") %in%
                    names(rec)))
  expect_false(is.unsorted(rev(rec$score)))
  expect_match(rec$call[1], "^cpt_detect\\(")
  # Counts are recommended with the family that models them.
  counts <- cpt_recommend(data_type = "counts")
  expect_match(counts$call[1], "family = \"poisson\"")
  # A single-change design is ranked down unless one change is expected.
  one <- cpt_recommend(n_expected = 1)
  two <- cpt_recommend(n_expected = 3)
  rank <- function(r, m) match(m, r$method)
  expect_lt(rank(one, "amoc"), rank(two, "amoc"))
  # Autocorrelated noise favours the engines measured to cope with it.
  ar <- cpt_recommend(noise = "autocorrelated")
  expect_false(ar$method[1] %in% c("wbs2", "pelt"))
  expect_output(print(rec), "cpt_detect")
  expect_s3_class(ggplot2::autoplot(rec), "ggplot")
  expect_s3_class(plot(rec), "ggplot")
  # A fit fills in what it knows.
  set.seed(77)
  fit <- cpt_detect(as.numeric(stats::arima.sim(list(ar = 0.8), 300)),
                    method = "pelt") |> suppressWarnings()
  from_fit <- cpt_recommend(fit = fit)
  expect_match(paste(attr(from_fit, "notes"), collapse = " "),
               "autocorrelated")
})

test_that("cpt_stability() offers a block bootstrap and a reversal check", {
  skip_on_cran()
  set.seed(78)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  st <- cpt_stability(x, method = "pelt", B = 20, bootstrap = "block",
                      seed = 1)
  expect_equal(st$bootstrap, "block")
  expect_true(st$block_length >= 1)
  expect_true(all(st$reversal$survives))
  expect_output(print(st), "block")
  iid <- cpt_stability(x, method = "pelt", B = 20, reversal = FALSE,
                       seed = 1)
  expect_null(iid$reversal)
  expect_null(iid$block_length)
  fit <- cpt_detect(x, method = "pelt")
  expect_equal(cpt_stability(fit, B = 10, seed = 1)$original$changepoints$cp,
               fit$changepoints$cp)
})

test_that("cpt_consensus() takes a majority by default and reports disagreement", {
  skip_on_cran()
  set.seed(79)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4), stats::rnorm(80, 4.5))
  cons <- cpt_consensus(x, methods = c("pelt", "binseg", "amoc"))
  info <- attr(cons, "consensus")
  expect_equal(info$threshold, 2)
  expect_true(info$disagreement >= 0 && info$disagreement <= 1)
  expect_equal(names(info$counts), c("pelt", "binseg", "amoc"))
  expect_output(print(cons), "Disagreement")
  all3 <- cpt_consensus(x, methods = c("pelt", "binseg", "amoc"),
                        min_votes = "all")
  expect_equal(attr(all3, "consensus")$threshold, 3)
  expect_lte(nrow(all3$changepoints), nrow(cons$changepoints))
})

test_that("cpt_metrics() reports the covering a trivial answer gets", {
  skip_on_cran()
  set.seed(80)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  fit <- cpt_detect(x, method = "pelt")
  m <- cpt_metrics(fit, truth = 100)
  expect_true(all(c("covering_floor", "covering_scaled") %in% names(m)))
  expect_equal(m$covering_floor,
               ggchangepoint:::calc_covering(integer(0), 100, 200))
  expect_true(m$covering_scaled <= 1)
  expect_equal(cpt_metrics(fit$changepoints$cp, truth = 100, n = 200)$f1,
               m$f1)
})
