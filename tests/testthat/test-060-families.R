# 0.6.0: families, the modelling-choice vocabulary and the formula front
# door (§111, §124, §126, §152, §158, §161).

test_that("cpt_families() lists every legal combination and its engine call", {
  tab <- cpt_families()
  expect_named(tab, c("method", "change_in", "family", "engine_call", "data"))
  expect_true(all(tab$family %in% c(NA, "gaussian", "poisson", "binomial",
                                    "exponential", "gamma", "laplace", "l1")))
  pois <- unique(tab$method[tab$family %in% "poisson"])
  expect_true(all(c("pelt", "binseg", "amoc", "binsegrcpp", "fastcpd",
                    "smuce", "bocpd", "segmented") %in% pois))
  # A distribution-free method takes no family: its rows say what runs.
  expect_true(all(is.na(tab$family[tab$method == "ecp"])))
  expect_equal(unique(tab$data[tab$family %in% "poisson"]), "counts")
  expect_equal(unique(tab$data[tab$family %in% "binomial"]), "binary")
  # One row per combination: the table is a function, not a list of notes.
  key <- paste(tab$method, tab$change_in, tab$family)
  expect_false(anyDuplicated(key) > 0)
  expect_equal(unique(cpt_families("pelt")$method), "pelt")
  expect_error(cpt_families("peltt"), class = "ggchangepoint_unknown_method")
})

test_that("cpt_methods() carries the vocabulary columns", {
  tab <- cpt_methods()
  expect_true(all(c("families", "choices", "formula", "min_segment",
                    "na_handling", "cp_convention_upstream") %in% names(tab)))
  expect_match(tab$families[tab$method == "pelt"], "poisson")
  expect_true(is.na(tab$families[tab$method == "ecp"]))
  expect_true(tab$formula[tab$method == "strucchange"])
  expect_false(tab$formula[tab$method == "wbs"])
  expect_equal(tab$min_segment[tab$method == "pelt"], "minseglen")
  expect_match(tab$choices[tab$method == "pelt"], "test.stat")
  expect_match(tab$choices[tab$method == "pelt"], "Poisson")
})

test_that("a Poisson request runs a Poisson cost and is recorded", {
  skip_on_cran()
  skip_if_not_installed("changepoint")
  set.seed(30)
  counts <- c(stats::rpois(100, 3), stats::rpois(100, 9))
  fit <- cpt_detect(counts, method = "pelt", family = "poisson")
  expect_equal(fit$family, "poisson")
  expect_equal(fit$change_in, "mean")
  expect_true(any(abs(fit$changepoints$cp - 100) <= 3))
  expect_equal(glance(fit)$family, "poisson")
  expect_output(print(fit), "Family: +poisson")
  # The Gaussian cost on the same counts is not the same request, and the
  # package says so: counts whose variance tracks the mean.
  expect_warning(gauss <- cpt_detect(counts, method = "pelt"),
                 class = "ggchangepoint_data_type") |>
    suppressWarnings()
  expect_null(gauss$family)
})

test_that("each family reaches its engine", {
  skip_on_cran()
  set.seed(31)
  counts <- c(stats::rpois(120, 2), stats::rpois(120, 8))
  binary <- c(stats::rbinom(150, 1, 0.1), stats::rbinom(150, 1, 0.8))
  waits <- c(stats::rexp(120, 1), stats::rexp(120, 6))
  near <- function(fit, at) any(abs(fit$changepoints$cp - at) <= 8)
  if (engine_usable("binsegRcpp")) {
    expect_true(near(cpt_detect(counts, "binsegrcpp", family = "poisson"),
                     120))
  }
  if (engine_usable("fastcpd")) {
    expect_true(near(suppressWarnings(
      cpt_detect(counts, "fastcpd", family = "poisson")), 120))
    expect_true(near(suppressWarnings(
      cpt_detect(binary, "fastcpd", family = "binomial")), 150))
  }
  if (engine_usable("stepR")) {
    f <- cpt_detect(counts, "smuce", family = "poisson")
    expect_true(near(f, 120))
    expect_true(all(c("ci_lower", "ci_upper") %in% names(f$changepoints)))
    expect_true(near(cpt_detect(binary, "smuce", family = "binomial"), 150))
    # stepR's own vocabulary still reaches it unchanged.
    expect_equal(suppressWarnings(
      cpt_detect(counts, "smuce", family = "hsmuce"))$method, "hsmuce")
  }
  if (engine_usable("changepoint")) {
    expect_true(near(cpt_detect(waits, "pelt", family = "exponential"), 120))
  }
  if (engine_usable("cpm")) {
    f <- cpt_detect(waits, "cpm", family = "exponential")
    expect_equal(f$family, "exponential")
  }
})

test_that("a family a method cannot fit is refused with the ones that can", {
  skip_on_cran()
  x <- c(stats::rpois(50, 2), stats::rpois(50, 6))
  e <- expect_error(cpt_detect(x, "wbs", family = "poisson"),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "pelt")
  expect_identical(e$requested, "poisson")
  # A distribution-free method has no family to set.
  expect_error(cpt_detect(x, "ecp", family = "gaussian"),
               class = "ggchangepoint_unsupported")
  # A one-parameter family changes in its mean.
  expect_error(cpt_detect(x, "pelt", change_in = "var", family = "poisson"),
               class = "ggchangepoint_unsupported")
  expect_error(cpt_detect(x, "pelt", family = "poison"),
               class = "ggchangepoint_bad_argument")
})

test_that("the data are checked against the family before the engine", {
  skip_on_cran()
  e <- expect_error(cpt_detect(c(-1, 2.5, 3, rep(4, 30)), "pelt",
                               family = "poisson"),
                    class = "ggchangepoint_bad_type")
  expect_match(conditionMessage(e), "whole numbers")
  expect_error(cpt_detect(c(0, 1, 2, rep(1, 30)), "pelt",
                          family = "binomial"),
               class = "ggchangepoint_unsupported")
  if (engine_usable("stepR")) {
    expect_error(smuce_wrapper(c(1.5, rep(2, 40)), family = "poisson"),
                 class = "ggchangepoint_bad_type")
  }
})

test_that("a modelling choice outside the vocabulary is refused by name", {
  skip_on_cran()
  x <- c(stats::rnorm(50), stats::rnorm(50, 3))
  e <- expect_error(cpt_detect(x, "pelt", test.stat = "Poison"),
                    class = "ggchangepoint_bad_argument")
  expect_match(conditionMessage(e), "Poisson")
  e <- expect_error(cpt_detect(x, "pelt", minseglenn = 5),
                    class = "ggchangepoint_bad_argument")
  expect_match(conditionMessage(e), "minseglen")
})

test_that("min_segment is translated to each engine's own argument", {
  skip_on_cran()
  set.seed(32)
  x <- c(stats::rnorm(40), stats::rnorm(6, 5), stats::rnorm(40))
  fit <- cpt_detect(x, "pelt", min_segment = 10)
  expect_true(all(diff(c(0, fit$changepoints$cp, length(x))) >= 10))
  short <- cpt_detect(x, "pelt", min_segment = 2)
  expect_true(any(diff(c(0, short$changepoints$cp, length(x))) < 10))
  # A method with no such argument refuses rather than ignoring it.
  e <- expect_error(cpt_detect(x, "wbs", min_segment = 5),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "min_segment")
  expect_error(cpt_detect(x, "pelt", min_segment = 0),
               class = "ggchangepoint_bad_argument")
})

test_that("a formula fits a regression with a break and reports coefficients", {
  skip_on_cran()
  skip_if_not_installed("strucchange")
  set.seed(33)
  n <- 160
  d <- data.frame(t = seq_len(n), x = stats::rnorm(n))
  d$y <- ifelse(d$t <= 80, 1 + 2 * d$x, 4 - 1 * d$x) + stats::rnorm(n, 0, 0.4)
  fit <- cpt_detect(y ~ x, data = d, method = "strucchange")
  expect_equal(fit$change_in, "regression")
  expect_true(any(abs(fit$changepoints$cp - 80) <= 3))
  co <- tidy(fit, what = "coefficients")
  expect_true(all(c("segment", "start", "end", "term", "estimate",
                    "std_error", "conf_low", "conf_high") %in% names(co)))
  slope <- co$estimate[co$term == "x"]
  expect_equal(slope, c(2, -1), tolerance = 0.2)
  expect_true("fitted" %in% names(fit$data))
  # predict() carries the last segment's model forward.
  new <- data.frame(x = c(0, 1))
  p <- predict(fit, newdata = new)
  expect_equal(p$.pred, c(4, 3), tolerance = 0.3)
  expect_true(all(p$.lower < p$.pred & p$.pred < p$.upper))
  models <- cpt_segment_models(fit)
  expect_s3_class(models, "ggcpt_segment_models")
  expect_equal(nrow(glance(models)), nrow(fit$segments))
  expect_output(print(models), "segment")
})

test_that("the formula front door checks what it is given", {
  skip_on_cran()
  d <- data.frame(y = c(stats::rnorm(40), stats::rnorm(40, 3)),
                  x = stats::rnorm(80))
  # y ~ 1 is the response alone, which any univariate method takes.
  fit <- cpt_detect(y ~ 1, data = d, method = "pelt")
  expect_equal(nrow(fit$data), 80)
  expect_true(any(abs(fit$changepoints$cp - 40) <= 2))
  e <- expect_error(cpt_detect(y ~ x, data = d, method = "wbs"),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "strucchange")
  expect_error(cpt_detect(d, data = d), class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(~ x, data = d, method = "strucchange"),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(y ~ x, data = list(1), method = "strucchange"),
               class = "ggchangepoint_bad_argument")
})

test_that("segmented and fastcpd take the formula too", {
  skip_on_cran()
  set.seed(34)
  n <- 150
  d <- data.frame(t = seq_len(n))
  d$y <- ifelse(d$t <= 75, 0.05 * d$t, 3.75 + 0.3 * (d$t - 75)) +
    stats::rnorm(n, 0, 0.5)
  if (engine_usable("segmented")) {
    fit <- cpt_detect(y ~ t, data = d, method = "segmented", seg_z = ~t,
                      seed = 1)
    expect_true(any(abs(fit$changepoints$cp - 75) <= 5))
  }
  if (engine_usable("fastcpd")) {
    d$x <- stats::rnorm(n)
    d$z <- ifelse(d$t <= 75, 1 + 2 * d$x, 1 - 2 * d$x) + stats::rnorm(n, 0, 0.3)
    fit <- suppressWarnings(cpt_detect(z ~ x, data = d, method = "fastcpd"))
    expect_true(any(abs(fit$changepoints$cp - 75) <= 5))
  }
})

test_that("cpt_simulate() and cpt_power() speak the families", {
  skip_on_cran()
  sim <- cpt_simulate(200, changepoints = 100, params = c(2, 8),
                      family = "poisson", seed = 1)
  v <- sim$value
  expect_true(all(v >= 0 & v == round(v)))
  expect_gt(mean(v[101:200]), mean(v[1:100]))
  expect_equal(attr(sim, "true_changepoints"), 100)
  b <- cpt_simulate(200, changepoints = 100, params = c(0.2, 0.8),
                    family = "binomial", seed = 1)
  expect_true(all(b$value %in% c(0, 1)))
  expect_error(cpt_simulate(100, changepoints = 50, params = c(-1, 2),
                            family = "poisson", seed = 1),
               class = "ggchangepoint_bad_argument")
  skip_if_not_installed("changepoint")
  pw <- cpt_power(n = 120, jump = 4, method = "pelt", n_sim = 5,
                  family = "poisson", baseline = 2, seed = 1,
                  parallel = FALSE)
  expect_equal(attr(pw, "family"), "poisson")
  expect_output(print(pw), "poisson")
})

test_that("SMUCE's count families fail by class if stepR drops smuceR()", {
  skip_if_not(engine_usable("stepR"))
  # stepR documents smuceR() as deprecated but working; the day it goes,
  # the refusal names the methods that fit the same family.
  local_mocked_bindings(get0 = function(x, envir, ...) NULL, .package = "base")
  e <- expect_error(smuce_wrapper(stats::rpois(60, 3), family = "poisson"),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "pelt")
})
