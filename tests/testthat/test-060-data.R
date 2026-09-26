# 0.6.0: the data cpt_detect() is handed (grouped and long frames, missing
# values, values that are not a series) and the constraints it can be given
# (§111, §147.3, §171, §172, §214, §222).

test_that("a long data frame with `group =` is one series per group", {
  skip_on_cran()
  set.seed(41)
  d <- data.frame(
    site = rep(c("b", "a"), each = 120),
    t = rep(1:120, 2),
    y = c(stats::rnorm(60), stats::rnorm(60, 4), stats::rnorm(120))
  )
  # Rows out of order within a group are put back in index order.
  d <- d[c(seq(2, 240, 2), seq(1, 239, 2)), ]
  res <- cpt_detect(d, y = y, index = t, group = site)
  expect_s3_class(res, "ggcpt_batch")
  expect_equal(res$site, c("a", "b"))
  expect_equal(attr(res, "group_vars"), "site")
  b <- res$result[[which(res$site == "b")]]
  expect_true(any(abs(b$changepoints$cp - 60) <= 2))
  expect_identical(res$n_changepoints[res$site == "a"], 0L)
  # A string names the column just as well.
  expect_equal(cpt_detect(d, y = "y", index = "t", group = "site")$site,
               c("a", "b"))
  e <- expect_error(cpt_detect(d, y = y, group = region),
                    class = "ggchangepoint_bad_argument")
  expect_match(conditionMessage(e), "region")
})

test_that("a dplyr-grouped frame is detected group by group", {
  skip_on_cran()
  set.seed(42)
  d <- data.frame(g = rep(c("x", "y"), each = 80),
                  v = c(stats::rnorm(40), stats::rnorm(40, 5),
                        stats::rnorm(80)))
  gd <- dplyr::group_by(d, g)
  res <- cpt_detect(gd, method = "pelt")
  expect_s3_class(res, "ggcpt_batch")
  expect_equal(res$g, c("x", "y"))
  expect_true(any(abs(res$result[[1]]$changepoints$cp - 40) <= 2))
  # With two numeric columns the one to detect on must be named.
  gd2 <- dplyr::group_by(dplyr::mutate(d, w = v * 2), g)
  expect_error(cpt_detect(gd2), class = "ggchangepoint_bad_argument")
})

test_that("missing values are refused by default, with the way out named", {
  x <- c(stats::rnorm(50), NA, stats::rnorm(50, 3))
  e <- expect_error(cpt_detect(x, method = "pelt"),
                    class = "ggchangepoint_input_error")
  expect_match(conditionMessage(e), "na_action")
})

test_that("na_action = \"omit\" reports locations in the original positions", {
  skip_on_cran()
  set.seed(43)
  x <- c(stats::rnorm(60), stats::rnorm(60, 5))
  x[c(10, 11, 59, 90)] <- NA
  fit <- cpt_detect(x, method = "pelt", na_action = "omit")
  expect_equal(nrow(fit$data), 120)
  expect_true(all(is.na(fit$data$value[c(10, 11, 59, 90)])))
  # The change is after observation 60, which is observed.
  expect_true(any(abs(fit$changepoints$cp - 60) <= 1))
  expect_equal(fit$diagnostics$na_omitted$positions, c(10L, 11L, 59L, 90L))
  expect_equal(fit$diagnostics$na_omitted$n_observed, 116L)
  expect_equal(sum(fit$segments$end - fit$segments$start + 1), 120)
  expect_output(print(fit), "Missing values")
  # A changepoint never lands on a missing observation.
  expect_false(any(is.na(x[fit$changepoints$cp])))
  # An index travels with the gaps.
  dates <- as.Date("2026-01-01") + 0:119
  fit_d <- cpt_detect(x, method = "pelt", index = dates, na_action = "omit")
  expect_s3_class(fit_d$changepoints$cp_index, "Date")
  # Fewer than three complete observations is not a series.
  expect_error(cpt_detect(c(NA, NA, 1, 2, NA), method = "pelt",
                          na_action = "omit"),
               class = "ggchangepoint_input_error")
})

test_that("na_action = \"engine\" passes gaps only to engines that model them", {
  skip_on_cran()
  set.seed(44)
  x <- c(stats::rnorm(60), stats::rnorm(60, 5))
  x[c(20, 80)] <- NA
  e <- expect_error(cpt_detect(x, method = "pelt", na_action = "engine"),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "omit")
  tab <- cpt_methods()
  expect_true(all(tab$na_handling %in% c(NA, "native", "compacts",
                                         "silent_loss", "reject")))
  skip_if_not(engine_usable("Rbeast"))
  fit <- cpt_detect(x, method = "beast", na_action = "engine", seed = 1)
  expect_equal(nrow(fit$data), 120)
})

test_that("values that are not a series are refused rather than coerced", {
  when <- as.POSIXct("2026-01-01", tz = "UTC") + 3600 * (0:49)
  e <- expect_error(cpt_detect(when), class = "ggchangepoint_bad_type")
  expect_match(conditionMessage(e), "index")
  expect_error(cpt_detect(as.Date("2026-01-01") + 0:49),
               class = "ggchangepoint_bad_type")
  # A survival object is a two-column matrix of times and censoring flags
  # with class "Surv"; built by hand so the test needs no package.
  s <- structure(cbind(time = stats::rexp(40),
                       status = stats::rbinom(40, 1, 0.7)),
                 class = "Surv", type = "right")
  expect_error(cpt_detect(s), class = "ggchangepoint_bad_type")
})

test_that("fixed changepoints are kept and the rest estimated around them", {
  skip_on_cran()
  set.seed(45)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4), stats::rnorm(80, 8))
  fit <- cpt_detect(x, method = "pelt", fixed = 80)
  expect_true(80L %in% fit$changepoints$cp)
  expect_true(fit$changepoints$fixed[fit$changepoints$cp == 80])
  expect_true(any(abs(fit$changepoints$cp[!fit$changepoints$fixed] - 160) <=
                    2))
  expect_equal(fit$constraints$fixed, 80L)
  expect_output(print(fit), "Fixed")
  # A fixed changepoint where nothing changes is still reported: it was
  # asked for.
  fit2 <- cpt_detect(x, method = "pelt", fixed = 40)
  expect_true(40L %in% fit2$changepoints$cp)
  # By date.
  dates <- as.Date("2026-01-01") + 0:239
  fit3 <- cpt_detect(x, method = "pelt", index = dates,
                     fixed = as.Date("2026-03-21"))
  expect_true(80L %in% fit3$changepoints$cp)
  expect_error(cpt_detect(x, method = "pelt", fixed = 240),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(x, method = "pelt", fixed = "a"),
               class = "ggchangepoint_bad_argument")
})

test_that("within keeps only changepoints inside the windows", {
  skip_on_cran()
  set.seed(46)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4), stats::rnorm(80, 8))
  fit <- cpt_detect(x, method = "pelt", within = c(150, 170))
  expect_true(all(fit$changepoints$cp >= 150 & fit$changepoints$cp <= 170))
  expect_true(any(abs(fit$constraints$dropped_outside_within - 80) <= 2))
  both <- cpt_detect(x, method = "pelt",
                     within = list(c(70, 90), c(150, 170)))
  expect_equal(nrow(both$changepoints), 2L)
  tab <- data.frame(start = c(70, 150), end = c(90, 170))
  expect_equal(cpt_detect(x, method = "pelt", within = tab)$changepoints$cp,
               both$changepoints$cp)
  expect_error(cpt_detect(x, method = "pelt", within = c(1, 2, 3)),
               class = "ggchangepoint_bad_argument")
})

test_that("min_effect drops the changes too small to matter", {
  skip_on_cran()
  set.seed(47)
  x <- c(stats::rnorm(100), stats::rnorm(100, 0.6), stats::rnorm(100, 5))
  all_cp <- cpt_detect(x, method = "pelt", penalty = "BIC")
  big <- cpt_detect(x, method = "pelt", penalty = "BIC", min_effect = 2)
  expect_true(any(abs(big$changepoints$cp - 200) <= 2))
  expect_lte(nrow(big$changepoints), nrow(all_cp$changepoints))
  expect_true(all(abs(cpt_effect(big)$delta_std) >= 2))
  expect_equal(big$constraints$min_effect, 2)
  e <- expect_error(cpt_detect(x, method = "pelt", change_in = "var",
                               min_effect = 1),
                    class = "ggchangepoint_unsupported")
  expect_match(conditionMessage(e), "mean")
})

test_that("keep_fit = FALSE drops the engine object and nothing else", {
  skip_on_cran()
  set.seed(48)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3))
  kept <- cpt_detect(x, method = "pelt")
  lean <- cpt_detect(x, method = "pelt", keep_fit = FALSE)
  expect_false(is.null(kept$fit))
  expect_null(lean$fit)
  expect_equal(lean$changepoints, kept$changepoints)
  expect_error(cpt_detect(x, keep_fit = NA),
               class = "ggchangepoint_bad_argument")
})

test_that("a large engine object warns once per method per session", {
  st <- ggchangepoint:::.cpt_state
  old <- st$large_fit_warned
  on.exit(st$large_fit_warned <- old, add = TRUE)
  st$large_fit_warned <- NULL
  res <- list(method = "demo", fit = numeric(2e6))
  expect_warning(ggchangepoint:::warn_large_fit(res),
                 class = "ggchangepoint_large_fit")
  expect_no_warning(ggchangepoint:::warn_large_fit(res))
  small <- list(method = "other", fit = numeric(10))
  expect_no_warning(ggchangepoint:::warn_large_fit(small))
})

test_that("the checks on the data and on the answer are classed warnings", {
  skip_on_cran()
  # Too short to interpret, whatever the engine returns.
  expect_warning(cpt_detect(c(1, 5, 9, 2, 6), method = "pelt"),
                 class = "ggchangepoint_short_series_warning") |>
    suppressWarnings()
  # Binary data under a Gaussian cost.
  set.seed(49)
  b <- c(stats::rbinom(100, 1, 0.2), stats::rbinom(100, 1, 0.7))
  w <- tryCatch(cpt_detect(b, method = "pelt"),
                ggchangepoint_data_type = function(w) w)
  expect_s3_class(w, "ggchangepoint_data_type")
  expect_equal(w$data_type, "binary")
  # Not when the family was chosen, or the method is distribution-free.
  expect_no_warning(ggchangepoint:::warn_data_type(b, "binomial", "fastcpd"))
  expect_no_warning(ggchangepoint:::warn_data_type(b, NULL, "ecp"))
  # Wide noise under a unit-noise cost.
  x <- 25 * c(stats::rnorm(100), stats::rnorm(100, 3))
  w <- tryCatch(cpt_detect(x, method = "pelt"),
                ggchangepoint_scale_sensitive = function(w) w)
  expect_s3_class(w, "ggchangepoint_scale_sensitive")
})

test_that("the data-type sniffer tells binary, counts and measurements apart", {
  set.seed(50)
  sniff <- ggchangepoint:::detect_data_type
  expect_equal(sniff(stats::rbinom(200, 1, 0.4)), "binary")
  expect_equal(sniff(stats::rpois(200, 4)), "counts")
  expect_equal(sniff(stats::rnorm(200)), "continuous")
  # Integers that are not counts: a level far above its spread.
  expect_equal(sniff(round(stats::rnorm(200, 1000, 30))), "continuous")
  # A clean 0/1 step is a step, not Bernoulli noise.
  expect_equal(sniff(rep(0:1, each = 50)), "continuous")
  expect_equal(sniff(rep(c(0, 0.25, 0.5), 30)), "proportion")
})

test_that("a location is read on a numeric index the way events always were", {
  skip_on_cran()
  # A `ts` of years: 1899 cannot be a position in a 100-point series, so it
  # is the year, for every argument that takes a location.
  r <- cpt_test_at(Nile, when = 1899)
  expect_equal(r$cp, 28L)
  expect_equal(r$cp_index, 1898)
  fit <- suppressWarnings(cpt_detect(Nile, method = "pelt", fixed = 1898,
                                     change_in = "meanvar"))
  expect_true(28L %in% fit$changepoints$cp)
  expect_error(cpt_detect(Nile, method = "pelt", fixed = 1860),
               class = "ggchangepoint_bad_argument")
  # Inside 1..n the number is a position, and the guess is announced.
  expect_warning(r2 <- cpt_test_at(Nile, when = 29), "read as positions")
  expect_equal(r2$cp, 28L)
  expect_error(cpt_test_at(Nile, when = 1980),
               class = "ggchangepoint_bad_argument")
  skip_if_not(engine_usable("not"))
  win <- cpt_detect(Nile, method = "not", within = c(1890, 1910))
  expect_true(all(win$changepoints$cp_index >= 1890 &
                    win$changepoints$cp_index <= 1910))
})

test_that("`seed` is honoured by every method, and NULL means unset", {
  skip_on_cran()
  set.seed(51)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3))
  # pelt's wrapper takes no seed: it is scoped around the fit rather than
  # reaching changepoint as an unknown argument.
  expect_equal(cpt_detect(x, method = "pelt", seed = NULL)$changepoints,
               cpt_detect(x, method = "pelt")$changepoints)
  before <- .Random.seed
  expect_equal(cpt_detect(x, method = "pelt", seed = 1)$changepoints$cp,
               cpt_detect(x, method = "pelt")$changepoints$cp)
  expect_identical(.Random.seed, before)
  skip_if_not_installed("strucchange")
  d <- data.frame(y = x, t = seq_along(x))
  expect_s3_class(cpt_detect(y ~ t, data = d, method = "strucchange",
                             seed = 1), "ggcpt")
})
