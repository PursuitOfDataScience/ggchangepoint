# 0.6.0 polish pass: the defects found auditing the 0.6.0 surface before
# release. Most sat where two new features meet: missing values or
# constraints on a fit that another tool then re-runs, reads back, or
# exports.

gappy_step <- function(seed = 1) {
  set.seed(seed)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  x[c(20, 21, 150)] <- NA
  x
}

test_that("constraints on a fit with gaps are reported in original positions", {
  g <- gappy_step()
  fit <- cpt_detect(g, method = "pelt", na_action = "omit", fixed = 100,
                    within = c(90, 160))
  expect_equal(fit$changepoints$cp, 100L)
  expect_equal(fit$constraints$fixed, 100L)
  expect_equal(unname(fit$constraints$within[1, ]), c(90L, 160L))
  fe <- cpt_detect(g, method = "pelt", na_action = "omit", min_effect = 50)
  expect_equal(fe$constraints$dropped_below_min_effect, 100L)
})

test_that("a `within` window may reach either end of the series", {
  set.seed(2)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  fit <- cpt_detect(x, method = "pelt", within = c(80, 200))
  expect_equal(unname(fit$constraints$within[1, ]), c(80L, 199L))
  expect_equal(fit$changepoints$cp, 100L)
  fit0 <- cpt_detect(x, method = "pelt", within = c(0, 120))
  expect_equal(unname(fit0$constraints$within[1, ]), c(1L, 120L))
  dates <- as.Date("2020-01-01") + 0:199
  fd <- cpt_detect(x, method = "pelt", index = dates,
                   within = as.Date(c("2020-03-01", "2021-12-31")))
  expect_equal(unname(fd$constraints$within[1, "hi"]), 199L)
  # A window with nothing of the series in it is still refused.
  expect_error(cpt_detect(x, method = "pelt", index = dates,
                          within = as.Date(c("2019-01-01", "2019-06-01"))),
               "before the start", class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(x, method = "pelt", index = dates,
                          within = as.Date(c("2021-01-01", "2021-06-01"))),
               "after the last", class = "ggchangepoint_bad_argument")
})

test_that("a location that cannot be placed says why", {
  set.seed(3)
  x <- c(stats::rnorm(50), stats::rnorm(50, 4))
  expect_error(cpt_detect(x, method = "pelt", within = c(NA, 60)),
               "missing values", class = "ggchangepoint_bad_argument")
  lab <- paste0("w", 1:100)
  expect_error(cpt_detect(x, method = "pelt", index = lab, fixed = "nope"),
               "not in the series' index", class = "ggchangepoint_bad_argument")
  expect_equal(cpt_detect(x, method = "pelt", index = lab,
                          fixed = "w50")$constraints$fixed, 50L)
})

test_that("cpt_penalty() checks a Manual value", {
  expect_equal(cpt_penalty("Manual", value = 3L), 3)
  expect_error(cpt_penalty("Manual", value = -1),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_penalty("Manual", value = "a"),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_penalty("Manual", value = c(1, 2)),
               class = "ggchangepoint_bad_argument")
})

test_that("a missing or empty choice is refused by name", {
  x <- c(stats::rnorm(30), stats::rnorm(30, 3))
  expect_error(cpt_detect(x, method = NA_character_), "must not be NA",
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(x, na_action = NA_character_), "must not be NA",
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(x, method = ""),
               class = "ggchangepoint_unknown_method")
})

test_that("segment models work for transformed and factor terms", {
  skip_if_not(engine_usable("strucchange"))
  set.seed(4)
  n <- 200
  d <- data.frame(t = 1:n, z = stats::runif(n, 1, 5),
                  f = factor(sample(c("a", "b", "c"), n, TRUE)))
  d$y <- ifelse(d$t <= 120, 1 + 2 * log(d$z), 3 - log(d$z)) +
    stats::rnorm(n, 0, 0.3)
  fit <- cpt_detect(y ~ log(z), data = d, method = "strucchange")
  sm <- cpt_segment_models(fit)
  expect_true(all(!vapply(sm$model, is.null, logical(1))))
  p <- predict(fit, newdata = data.frame(z = c(2, 3)))
  expect_equal(nrow(p), 2L)
  expect_true(all(is.finite(p$.pred)))
  # A forecast by horizon needs the covariates' future values, and says so.
  expect_error(predict(fit), "`z`", class = "ggchangepoint_bad_argument")
  f2 <- cpt_detect(y ~ z + f, data = d, method = "strucchange")
  expect_true(all(!vapply(cpt_segment_models(f2)$model, is.null, logical(1))))
})

test_that("a regression fit with gaps reports coefficients in original rows", {
  skip_if_not(engine_usable("strucchange"))
  set.seed(5)
  n <- 200
  d <- data.frame(z = stats::runif(n))
  d$y <- ifelse(seq_len(n) <= 120, 1 + 2 * d$z, 3 - d$z) +
    stats::rnorm(n, 0, 0.3)
  d$y[c(5, 6, 130)] <- NA
  fit <- cpt_detect(y ~ z, data = d, method = "strucchange",
                    na_action = "omit")
  co <- fit$coefficients
  expect_equal(unique(co$start), fit$segments$start)
  expect_equal(unique(co$end), fit$segments$end)
  tc <- tidy(fit, "coefficients")
  expect_equal(unique(tc$start), fit$segments$start)
  expect_equal(unique(tc$end), fit$segments$end)
})

test_that("re-running a fit replays its gaps and constraints", {
  skip_on_cran()
  g <- gappy_step(6)
  fo <- cpt_detect(g, method = "pelt", na_action = "omit")
  expect_equal(ggchangepoint:::rerun_constraints(fo)$na_action, "omit")
  ci <- cpt_confint(fo, method = "bootstrap", B = 20, seed = 1)
  expect_equal(ci$n_replicates, 20L)
  expect_no_warning(cpt_stability(fo, B = 5, seed = 1),
                    class = "ggchangepoint_replicates_failed")
  expect_equal(cpt_select(fo)$fit$changepoints$cp, fo$changepoints$cp)
  expect_equal(nrow(cpt_select(fo)$fit$data), 200L)
  inf <- cpt_influence(fo, subset = c(20, 50, 100))
  # The missing observation has nothing to perturb.
  expect_equal(inf$influence$index, c(50L, 100L))
  expect_true(is.finite(cpt_test_at(fo, when = 101)$p_value))
  expect_true(is.finite(cpt_test_null(fo)$p_value))

  set.seed(7)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  ff <- cpt_detect(x, method = "pelt", fixed = 50)
  cf <- cpt_confint(ff, method = "bootstrap", B = 20, seed = 1)
  # A fixed changepoint is known, not estimated.
  expect_equal(unname(unlist(cf[cf$cp == 50, c("ci_lower", "ci_upper")])),
               c(50L, 50L))
  sp <- cpt_effect(ff, method = "split", seed = 1)
  expect_true(50L %in% sp$cp)
  st <- cpt_stability(ff, B = 5, seed = 1)
  expect_true(isTRUE(st$reversal$survives[st$reversal$cp == 50]))
  del <- cpt_influence(ff, engine = "recompute", subset = c(30, 50, 51))
  expect_true(all(del$influence$n_cp == 2L))
  expect_warning(cpt_select(ff), class = "ggchangepoint_constraint")
  fs <- cpt_detect(x, method = "pelt", min_segment = 30)
  expect_equal(fs$constraints$min_segment, 30)
  sens <- cpt_sensitivity(fs, over = list(min_segment = c(5, 40)))
  expect_true(all(is.na(sens$grid$error)))
})

test_that("cpt_verify() replays constraints given as index values", {
  set.seed(8)
  x <- stats::ts(c(stats::rnorm(100), stats::rnorm(100, 3)), start = 1801)
  fit <- cpt_detect(x, method = "pelt", fixed = 1850)
  expect_equal(fit$constraints$fixed, 50L)
  expect_true(cpt_verify(fit)$verified)
})

test_that("results with gaps, zones and coordinates survive export", {
  skip_if_not_installed("jsonlite")
  g <- gappy_step(9)
  path <- withr::local_tempfile(fileext = ".json")
  fo <- cpt_detect(g, method = "pelt", na_action = "omit", fixed = 60)
  cpt_export(fo, path)
  back <- cpt_import(path)
  expect_equal(back$changepoints$cp, fo$changepoints$cp)
  expect_equal(back$diagnostics$na_omitted$positions, c(20L, 21L, 150L))
  expect_equal(back$constraints$fixed, 60L)
  csv <- withr::local_tempfile(fileext = ".csv")
  cpt_export(fo, csv)
  expect_equal(cpt_import(csv)$changepoints$cp, fo$changepoints$cp)

  set.seed(10)
  x <- c(stats::rnorm(50), stats::rnorm(50, 3))
  tt <- as.POSIXct("2020-01-01", tz = "America/Chicago") + 3600 * (0:99)
  ft <- cpt_detect(x, method = "pelt", index = tt)
  cpt_export(ft, path)
  bt <- cpt_import(path)
  expect_equal(attr(bt$index, "tzone"), "America/Chicago")
  expect_equal(as.numeric(bt$index), as.numeric(tt))

  fm <- as_ggcpt(50, cbind(a = x, b = rev(x)))
  cpt_export(fm, path)
  bm <- cpt_import(path)
  expect_equal(ggchangepoint:::n_coordinates(bm), 2L)

  # Every field of `data` is present, as null where it does not apply.
  j <- jsonlite::fromJSON(as_json(cpt_detect(x, method = "pelt")),
                          simplifyVector = FALSE)
  expect_true(all(c("value", "index", "fitted", "coordinates") %in%
                    names(j$data)))
})

test_that("the global tests say nothing changes in a constant series", {
  for (m in c("cusum", "pettitt", "supF")) {
    if (m == "supF") skip_if_not(engine_usable("strucchange"))
    r <- cpt_test_null(rep(2, 50), method = m)
    expect_equal(nrow(r), 1L)
    expect_equal(r$p_value, 1)
    expect_equal(r$statistic, 0)
  }
  skip_if_not(engine_usable("strucchange"))
  expect_error(cpt_test_null(stats::rnorm(8), method = "supF"),
               class = "ggchangepoint_short_series")
})

test_that("a count fit's null power is simulated as counts", {
  skip_on_cran()
  set.seed(11)
  fit <- cpt_detect(c(stats::rpois(100, 3), stats::rpois(100, 3)),
                    method = "pelt", family = "poisson")
  np <- cpt_null_power(fit, n_sim = 5, seed = 1, max_iter = 2)
  expect_equal(np$family, "poisson")
  expect_equal(np$baseline, mean(fit$data$value))
  expect_output(print(np), "rate")
  md <- cpt_min_detectable(n = 100, family = "poisson", n_sim = 5,
                           max_iter = 2, seed = 1)
  expect_equal(md$family, "poisson")
  expect_output(print(md), "in the rate")
})

test_that("a window test peaks at the split, however strong the change", {
  # The statistic was qnorm(1 - p / 2): infinite for every p below 1e-16, so
  # all strong candidates tied and the window's first one won.
  set.seed(12)
  zeros <- c(rep(0, 60), stats::rpois(60, 30))
  expect_equal(cpt_test_at(zeros, when = 58, window = 4, family = "poisson",
                           B = 19, seed = 1)$cp, 60L)
  wide <- c(stats::rnorm(60), stats::rnorm(60, 0, 20))
  expect_equal(cpt_test_at(wide, when = 58, window = 4, change_in = "var",
                           B = 19, seed = 1)$cp, 60L)
})

test_that("tools that forward `...` accept gaps they route to cpt_detect()", {
  g <- gappy_step(13)
  cons <- cpt_consensus(g, na_action = "omit")
  expect_equal(cons$changepoints$cp, 100L)
  tab <- ggcpt_compare_table(g, na_action = "omit")
  expect_true(all(tab$cp == 100L))
  expect_error(cpt_consensus(g), class = "ggchangepoint_non_finite")
})

test_that("stat_cpt_region() hands the detector its own arguments", {
  skip_if_not(engine_usable("nsp"))
  set.seed(14)
  d <- data.frame(t = 1:200, y = c(stats::rnorm(100), stats::rnorm(100, 1.2)))
  layer_of <- function(...) {
    ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(t, y)) +
                            stat_cpt_region(seed = 1, ...))$data[[1]]
  }
  loose <- layer_of(alpha = 0.5)
  strict <- layer_of(alpha = 0.01)
  # NSP's level, not the band's transparency.
  expect_equal(loose$alpha, 0.2)
  expect_gte(strict$xmax - strict$xmin, loose$xmax - loose$xmin)
})

test_that("simulated counts carry the attributes Gaussian series do", {
  sim <- cpt_simulate(100, changepoints = 50, params = c(2, 6),
                      family = "poisson", seed = 1)
  expect_equal(unique(attr(sim, "signal")), c(2, 6))
  expect_s3_class(attr(sim, "true_segments"), "tbl_df")
  expect_warning(cpt_simulate(100, changepoints = 50, params = c(2, 6, 9),
                              family = "poisson", seed = 1),
                 class = "ggchangepoint_argument_ignored")
})

test_that("a JSON report checks its path", {
  skip_if_not_installed("jsonlite")
  fit <- cpt_detect(c(stats::rnorm(30), stats::rnorm(30, 3)), method = "pelt")
  expect_error(cpt_report(fit, format = "json", file = ""),
               class = "ggchangepoint_bad_argument")
})

test_that("a multivariate result survives a CSV round trip", {
  set.seed(15)
  X <- cbind(a = c(stats::rnorm(50), stats::rnorm(50, 3)), b = stats::rnorm(100))
  fit <- as_ggcpt(50, X, index = as.Date("2020-01-01") + 0:99)
  path <- withr::local_tempfile(fileext = ".csv")
  cpt_export(fit, path)
  expect_true(all(c("value_a", "value_b") %in% names(utils::read.csv(path))))
  back <- cpt_import(path)
  expect_equal(ggchangepoint:::n_coordinates(back), 2L)
  expect_equal(back$changepoints$cp, 50L)
  expect_s3_class(back$index, "Date")
})

test_that("a result that cannot be re-run is refused before any re-run", {
  fit <- as_ggcpt(50, c(stats::rnorm(50), stats::rnorm(50, 3)))
  expect_error(cpt_influence(fit), class = "ggchangepoint_capability_absent")
  expect_error(cpt_sensitivity(fit, over = list(penalty = c(1, 2))),
               class = "ggchangepoint_capability_absent")
})

test_that("the recommender answers a request for a regression break", {
  rec <- cpt_recommend(change_in = "regression")
  expect_setequal(rec$method, c("strucchange", "segmented", "fastcpd"))
  expect_true(all(startsWith(rec$call, "cpt_detect(y ~ x, data = d")))
  # The benchmark measured mean shifts, so it does not score these.
  expect_true(all(is.na(rec$hits)))
})

test_that("the irregular-index warning gives advice that works where it fires", {
  set.seed(16)
  x <- c(stats::rnorm(30), stats::rnorm(30, 3))
  d <- as.Date("2020-01-01") + c(0:29, seq(40, 127, by = 3))
  expect_warning(cpt_detect(x, method = "pelt", index = d), "classes",
                 class = "ggchangepoint_irregular_index")
  expect_no_warning(suppressWarnings(
    cpt_detect(x, method = "pelt", index = d),
    classes = "ggchangepoint_irregular_index"))
})

test_that("rows with a missing group key are left out, not made a series", {
  set.seed(17)
  long <- data.frame(id = rep(c("a", "b"), each = 60), t = rep(1:60, 2),
                     v = stats::rnorm(120))
  long$id[1:3] <- NA
  expect_warning(b <- cpt_detect(long, y = v, index = t, group = id,
                                 method = "pelt"),
                 class = "ggchangepoint_dropped_input")
  expect_equal(b$series, c("a", "b"))
})

test_that("binsegrcpp takes a minimum segment its default search cannot fit", {
  skip_if_not(engine_usable("binsegRcpp"))
  set.seed(18)
  x <- c(stats::rnorm(60), stats::rnorm(8, 6), stats::rnorm(60),
         stats::rnorm(60, 5))
  fit <- cpt_detect(x, method = "binsegrcpp", min_segment = 15)
  expect_gte(min(fit$segments$n), 15L)
  expect_error(binsegrcpp_wrapper(x, max_segments = 20,
                                  min_segment_length = 15),
               class = "ggchangepoint_bad_argument")
})

test_that("the functional engines see the curves at unit spread", {
  skip_if_not_installed("fChange")
  # fChange's covariance test is not numerically scale-free: measured on
  # pure noise, `fcov` found 109 changepoints in 120 curves at a thousandth
  # of the units and none at unit scale. The engine itself is mocked, so
  # this checks the rescaling rather than paying for fChange's simulation.
  seen <- NULL
  local_mocked_bindings(
    fchange = function(X, ...) {
      seen <<- stats::sd(as.numeric(X))
      NULL
    },
    .package = "fChange")
  set.seed(19)
  X <- matrix(stats::rnorm(180), 60) * 1e-3
  fit <- fcov_wrapper(X)
  expect_equal(seen, 1)
  # ... and the result keeps the data as given.
  expect_equal(fit$data_wide[[2]], X[, 1])
  fmean_wrapper(X * 1e6)
  expect_equal(seen, 1)
})

test_that("the scale warning offers only remedies the method has", {
  set.seed(20)
  x <- c(stats::rnorm(100), stats::rnorm(100, 5)) * 30
  shattered <- function(m) {
    suppressWarnings(cpt_detect(x, method = m),
                     classes = "ggchangepoint_implausible_count")
  }
  w <- expect_warning(shattered("pelt"),
                      class = "ggchangepoint_scale_sensitive")
  expect_match(conditionMessage(w), "meanvar")
  # Every scale-sensitive engine without a "meanvar" route is a Suggests.
  skip_if_not(engine_usable("fpop"))
  w <- expect_warning(shattered("fpop"),
                      class = "ggchangepoint_scale_sensitive")
  expect_false(grepl("meanvar", conditionMessage(w)))
})

test_that("a family request with the default change type reaches the method", {
  skip_if_not(engine_usable("segmented"))
  set.seed(21)
  mu <- exp(0.5 + c(seq(0, 1.5, length.out = 80), seq(1.5, 0, length.out = 80)))
  fit <- cpt_detect(stats::rpois(160, mu), method = "segmented",
                    family = "poisson")
  expect_equal(fit$change_in, "slope")
  expect_equal(fit$family, "poisson")
  e <- expect_error(cpt_detect(stats::rpois(160, mu), method = "segmented",
                               family = "poisson", change_in = "var"),
                    class = "ggchangepoint_unsupported")
  # The advice to use change_in = "mean" appears only where it is legal.
  expect_false(grepl("change_in = \"mean\"`)", conditionMessage(e),
                     fixed = TRUE))
})

test_that("cpm's FET statistic runs at cpm's documented default lambda", {
  skip_if_not(engine_usable("cpm"))
  set.seed(22)
  b <- c(stats::rbinom(100, 1, 0.2), stats::rbinom(100, 1, 0.8))
  fit <- cpt_detect(b, method = "cpm", family = "binomial")
  expect_equal(fit$family, "binomial")
  expect_true(any(abs(fit$changepoints$cp - 100) <= 10))
})

test_that("every 0.6.0 result class has a tidy() method", {
  skip_on_cran()
  set.seed(23)
  x <- c(stats::rnorm(80), stats::rnorm(80, 3))
  fit <- cpt_detect(x, method = "pelt")
  results <- list(cpt_effect(fit), cpt_test_at(x, when = 81),
                  cpt_assumptions(fit), cpt_gof(fit), cpt_robustness(fit),
                  cpt_verify(fit),
                  cpt_null_power(fit, n_sim = 5, max_iter = 2, seed = 1))
  for (r in results) {
    td <- tidy(r)
    expect_s3_class(td, "tbl_df")
    expect_identical(class(td), class(tibble::tibble()))
  }
  expect_equal(tidy(cpt_verify(fit))$status, "reproduced")
})

test_that("a minimum segment that leaves no room for a change is refused", {
  set.seed(24)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3))
  expect_error(cpt_detect(x[1:30], method = "pelt", min_segment = 20),
               "no room", class = "ggchangepoint_short_series")
  # A fixed stretch shorter than two minimum segments is left unsearched.
  fit <- cpt_detect(x, method = "pelt", fixed = 5, min_segment = 20)
  expect_equal(fit$changepoints$cp, c(5L, 60L))
})

test_that("a monitor's missing value gets advice a monitor can use", {
  m <- cpt_monitor("edetector", baseline = stats::rnorm(50))
  e <- expect_error(cpt_update(m, c(1, NA)), class = "ggchangepoint_non_finite")
  expect_match(conditionMessage(e), "leave a missing observation out")
  expect_false(grepl("cpt_detect", conditionMessage(e)))
})

test_that("the penalty path and the scale space take a fit with gaps", {
  skip_on_cran()
  g <- gappy_step(25)
  fit <- cpt_detect(g, method = "pelt", na_action = "omit")
  cr <- cpt_crops(fit)
  expect_equal(nrow(cr$data), 200L)
  expect_true(list(100L) %in% cr$solutions$cpts)
  skip_if_not(engine_usable("mosum"))
  ss <- cpt_scale_space(fit)
  # mosum never sees the missing values, and the gaps come back as empty
  # cells of a complete grid.
  expect_true(all(is.na(ss$statistic[ss$index %in% c(20, 21, 150)])))
  expect_equal(nrow(ss), 200L * length(unique(ss$bandwidth)))
})

test_that("print() names every constraint that changed what is reported", {
  set.seed(26)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3), stats::rnorm(60, 3.3))
  loose <- function(...) {
    suppressWarnings(cpt_detect(x, method = "pelt", penalty = 2, ...),
                     classes = "ggchangepoint_implausible_count")
  }
  fw <- loose(within = c(40, 80), min_segment = 10)
  expect_output(print(fw), "Within: +40-80 \\([0-9]+ found outside, dropped\\)")
  expect_output(print(fw), "Minimum segment: +10")
  fe <- loose(min_effect = 1)
  expect_output(print(fe), "Minimum effect: +1 noise sd")
})

test_that("displays read from the engine are put back in original positions", {
  skip_on_cran()
  g <- gappy_step(27)
  gaps <- c(20, 21, 150)
  if (engine_usable("bcp")) {
    fb <- cpt_detect(g, method = "bcp", na_action = "omit", seed = 1)
    pp <- ggchangepoint:::posterior_prob_profile(fb)
    expect_length(pp, 200L)
    expect_equal(pp[gaps], c(0, 0, 0))
    if (nrow(fb$changepoints)) {
      expect_equal(which.max(pp), fb$changepoints$cp[which.max(
        fb$changepoints$posterior_prob)])
    }
  }
  if (engine_usable("mosum")) {
    fm <- cpt_detect(g, method = "mosum", na_action = "omit")
    st <- ggchangepoint:::extract_statistic(fm)
    expect_equal(nrow(st), 200L)
    expect_true(all(is.na(st$statistic[gaps])))
    expect_lte(abs(which.max(st$statistic) - 100L), 2L)
  }
  if (engine_usable("wbs")) {
    fw <- cpt_detect(g, method = "wbs", na_action = "omit", seed = 1)
    sp <- ggchangepoint:::extract_solution_path(fw)
    expect_true(all(sp$cp[sp$selected] %in% fw$changepoints$cp))
    expect_lte(max(sp$end, na.rm = TRUE), 200L)
  }
  if (engine_usable("ocp")) {
    fo <- cpt_detect(g, method = "bocpd", na_action = "omit")
    d <- ggplot2::ggplot_build(ggcpt_runlength(fo))$data[[1]]
    expect_equal(range(d$x), c(1, 200))
  }
})
