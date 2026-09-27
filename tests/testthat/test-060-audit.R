# 0.6.0 second pre-release audit: every analysis tool run against fits of
# every shape (no changepoints, gaps, dates, a slope, a formula, fixed
# locations, small units). The defects sat where a tool assumed a change in
# the mean of a unit-scale series.

slope_series <- function(seed = 42) {
  set.seed(seed)
  c(seq(0, 10, length.out = 100), seq(10, 0, length.out = 100)) +
    stats::rnorm(200, 0, 0.5)
}

count_warnings <- function(expr, class = "warning") {
  n <- 0L
  withCallingHandlers(expr, warning = function(w) {
    if (inherits(w, class)) n <<- n + 1L
    invokeRestart("muffleWarning")
  })
  n
}

test_that("a slope fit is resampled around its line, not its segment means", {
  skip_on_cran()
  skip_if_not(engine_usable("cpop"))
  fit <- cpt_detect(slope_series(), method = "cpop", change_in = "slope")
  expect_equal(fit$changepoints$cp, 100L)
  expect_equal(ggchangepoint:::resampling_signal(fit), fit$data$fitted)
  # Around the segment means every replicate came back flat: 0 of 30
  # replicates used, and a stability of 0 for an unmistakable change.
  ci <- cpt_confint(fit, method = "bootstrap", B = 30, seed = 1)
  expect_equal(ci$n_replicates, 30L)
  expect_lte(ci$ci_upper - ci$ci_lower, 6L)
  st <- cpt_stability(fit, B = 10, seed = 1)
  expect_gte(tidy(st)$stability, 0.8)
  sel <- suppressWarnings(cpt_select(fit, criterion = "stability", B = 10,
                                     seed = 1))
  expect_equal(sel$k, 1L)
})

test_that("a slope engine with no fitted signal gets a line per segment", {
  skip_on_cran()
  skip_if_not(engine_usable("not"))
  fit <- suppressWarnings(cpt_detect(slope_series(), method = "not",
                                     change_in = "slope"))
  sig <- ggchangepoint:::resampling_signal(fit)
  # Against the segment means the residuals keep the trend (sd near 3).
  expect_lt(stats::sd(fit$data$value - sig), 0.7)
  expect_equal(ggchangepoint:::residual_signal(fit), sig)
  a <- cpt_assumptions(fit)
  expect_false(a$flag[a$component == "residual_dependence"])
  g <- cpt_gof(fit)
  expect_true(all(abs(g$acf1) < 0.3))
})

test_that("tools that re-run a fit refuse a formula fit they cannot re-run", {
  skip_on_cran()
  skip_if_not(engine_usable("strucchange"))
  set.seed(5)
  d <- data.frame(x1 = stats::rnorm(120))
  d$y <- ifelse(seq_len(120) <= 60, 1 + 2 * d$x1, 4 - d$x1) +
    stats::rnorm(120, 0, 0.5)
  fit <- cpt_detect(y ~ x1, data = d, method = "strucchange")
  expect_error(cpt_influence(fit), "fitted from a formula",
               class = "ggchangepoint_capability_absent")
  expect_error(cpt_leverage(fit), class = "ggchangepoint_capability_absent")
  expect_error(cpt_sensitivity(fit, over = list(h = c(0.1, 0.2))),
               class = "ggchangepoint_capability_absent")
  expect_error(cpt_select(fit), class = "ggchangepoint_capability_absent")
  expect_error(cpt_null_power(fit), "covariates",
               class = "ggchangepoint_capability_absent")
})

test_that("cpt_select() says when it scores a ladder as the wrong change", {
  skip_on_cran()
  set.seed(7)
  fit <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 0, 3)),
                    method = "pelt", change_in = "var")
  expect_warning(cpt_select(fit), "change in the mean",
                 class = "ggchangepoint_assumption")
  # "stability" re-detects with the ladder's own change type.
  expect_equal(count_warnings(cpt_select(fit, criterion = "stability",
                                         B = 5, seed = 1),
                              "ggchangepoint_assumption"), 0L)
})

test_that("an over-segmenting rung of the ladder does not warn each time", {
  skip_on_cran()
  skip_if_not(engine_usable("cpop"))
  fit <- cpt_detect(slope_series(), method = "cpop", change_in = "slope")
  expect_equal(count_warnings(cpt_select(fit),
                              "ggchangepoint_implausible_count"), 0L)
})

test_that("cpt_test() estimates a fit with gaps and names a mismatched test", {
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  x[c(5, 50:55, 150)] <- NA
  fit <- cpt_detect(x, method = "pelt", na_action = "omit")
  tt <- suppressWarnings(cpt_test(fit))
  expect_equal(tt$estimate, mean(x[101:200], na.rm = TRUE) -
                 mean(x[1:100], na.rm = TRUE))
  ts <- suppressWarnings(cpt_test(fit, type = "segment"))
  expect_true(is.finite(ts$estimate))
  fv <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 0, 3)),
                   method = "pelt", change_in = "var")
  suppressWarnings(
    expect_warning(cpt_test(fv), "shift in the mean only",
                   class = "ggchangepoint_assumption"),
    classes = "ggchangepoint_selection_unadjusted")
})

test_that("a changepoint fixed in advance is not flagged as selected", {
  skip_on_cran()
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  fit <- cpt_detect(x, method = "pelt", fixed = 50)
  expect_warning(tt <- cpt_test(fit), "FALSE for 1 of 2",
                 class = "ggchangepoint_selection_unadjusted")
  expect_equal(tt$selection_adjusted, c(TRUE, FALSE))
  ef <- cpt_effect(fit)
  expect_equal(ef$selection_adjusted, c(TRUE, FALSE))
  expect_match(ef$method[1], "fixed in advance")
  seg <- suppressWarnings(cpt_test(fit, type = "segment"))
  expect_equal(seg$selection_adjusted, c(TRUE, FALSE))
})

test_that("cpt_null_power() describes the change its fit is about", {
  skip_on_cran()
  set.seed(7)
  fit <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 0, 3)),
                    method = "pelt", change_in = "var")
  np <- cpt_null_power(fit, n_sim = 10, seed = 1)
  expect_equal(np$change_in, "var")
  expect_equal(tidy(np)$change_in, "var")
  out <- paste(capture.output(print(np)), collapse = " ")
  expect_match(out, "standard deviation")
  expect_no_match(out, "single shift")
})

test_that("cpt_effect() warns that levels do not measure a slope change", {
  skip_on_cran()
  skip_if_not(engine_usable("cpop"))
  fit <- cpt_detect(slope_series(), method = "cpop", change_in = "slope")
  expect_warning(cpt_effect(fit), "cpt_segment_models",
                 class = "ggchangepoint_assumption")
})

test_that("selecting columns of a 0.6.0 result drops its class, not print()", {
  skip_on_cran()
  set.seed(11)
  fit <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 3)),
                    method = "pelt")
  objs <- list(cpt_assumptions(fit), cpt_gof(fit), cpt_effect(fit),
               cpt_segment_models(fit),
               cpt_power(100, jump = 1, n_sim = 5, seed = 1))
  for (o in objs) {
    sub <- o[, 1]
    expect_false(inherits(sub, class(o)[1]))
    expect_s3_class(sub, "tbl_df")
    expect_no_warning(capture.output(print(sub)))
    # Rows alone keep every column, and the class.
    expect_s3_class(o[1, ], class(o)[1])
  }
})

test_that("resampling tools raise each advisory once, not once per replicate", {
  skip_on_cran()
  set.seed(3)
  x <- c(stats::rnorm(150, 0, 0.3), stats::rnorm(150, 1, 0.3))
  fit <- suppressWarnings(cpt_detect(x, method = "pelt"))
  scale <- "ggchangepoint_scale_sensitive"
  # 100, 50, 180 and 20 of them before.
  expect_equal(count_warnings(cpt_confint(fit, B = 100, seed = 1), scale), 1L)
  expect_equal(count_warnings(cpt_stability(fit, B = 50, seed = 1), scale),
               1L)
  expect_equal(count_warnings(cpt_null_power(fit, n_sim = 20, seed = 1),
                              scale), 1L)
  expect_equal(count_warnings(cpt_power(300, jump = 1, sigma = 0.3,
                                        n_sim = 20, seed = 1), scale), 1L)
})

test_that("tidy() works on the result classes that had no method", {
  skip_on_cran()
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  fit <- cpt_detect(x, method = "pelt", index = as.Date("2020-01-01") + 0:199)
  st <- tidy(cpt_stability(fit, B = 5, seed = 1))
  expect_named(st, c("cp", "cp_index", "stability", "survives_reversal"))
  expect_equal(st$cp, 100L)
  md <- tidy(cpt_min_detectable(200, n_sim = 5, max_iter = 2, seed = 1))
  expect_equal(nrow(md), 1L)
  expect_true(all(c("jump", "achieved_power", "target") %in% names(md)))
  none <- tidy(cpt_stability(stats::rnorm(80), B = 3, seed = 1))
  expect_equal(nrow(none), 0L)
})

test_that("a result's call records no inlined data and no function source", {
  set.seed(1)
  x <- stats::rnorm(1000)
  fit <- do.call(cpt_detect, list(x, method = "pelt", penalty = 30))
  expect_identical(as.character(fit$call[[1]]), "cpt_detect")
  expect_identical(fit$call$penalty, 30)
  expect_lt(as.numeric(utils::object.size(fit$call)), 2000)
  skip_if_not_installed("jsonlite")
  # data = FALSE used to write every value anyway, inside `call`.
  j <- jsonlite::fromJSON(as_json(fit, data = FALSE))
  expect_lt(nchar(j$call), 200)
  expect_true(cpt_verify(fit)$verified)
})

test_that("an imported result keeps the call it was written with", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3), stats::rnorm(100, 1))
  fit <- suppressWarnings(cpt_detect(x, method = "binseg", Q = 1))
  path <- tempfile(fileext = ".json")
  cpt_export(fit, path)
  back <- cpt_import(path)
  expect_identical(back$call$Q, 1)
  # Re-run without `Q = 1`, it read back as "CHANGED".
  expect_true(suppressWarnings(cpt_verify(back))$verified)
  csv <- tempfile(fileext = ".csv")
  cpt_export(fit, csv)
  from_csv <- cpt_import(csv)
  expect_null(from_csv$call)
  rep <- cpt_report(from_csv, format = "md", confint = FALSE,
                    session = FALSE)
  expect_true(any(grepl("(not recorded)", rep, fixed = TRUE)))
  unlink(c(path, csv))
})

test_that("cpt_verify() refuses a result no cpt_detect() call made", {
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3), stats::rnorm(100, 1))
  sel <- cpt_select(x, criterion = "bic")
  expect_identical(as.character(sel$fit$call[[1]]), "cpt_select")
  expect_error(cpt_verify(sel$fit), "made by `cpt_select()`", fixed = TRUE,
               class = "ggchangepoint_capability_absent")
  built <- as_ggcpt(c(100, 200), x, method = "pelt")
  expect_error(cpt_verify(built), class = "ggchangepoint_capability_absent")
})

test_that("a warning from one series of a panel names the series", {
  set.seed(4)
  d <- data.frame(g = c(rep("a", 120), rep("tiny", 3)),
                  value = c(stats::rnorm(60), stats::rnorm(60, 3), 1, 2, 3))
  expect_warning(cpt_detect(d, y = value, group = g),
                 "^Series `tiny` \\(2 of 2\\)",
                 class = "ggchangepoint_short_series_warning")
})

test_that("a formula fit refuses grouped data rather than stacking the groups", {
  set.seed(8)
  d <- data.frame(x1 = stats::rnorm(60))
  d$y <- 1 + 2 * d$x1 + stats::rnorm(60, 0, 0.5)
  dg <- rbind(transform(d, g = "a"), transform(d, g = "b"))
  expect_error(cpt_detect(y ~ x1, data = dg, method = "strucchange",
                          group = g),
               "grouped by `g`", class = "ggchangepoint_unsupported")
  expect_error(cpt_detect(y ~ 1, data = dg, method = "pelt", group = g),
               class = "ggchangepoint_unsupported")
  expect_error(cpt_detect(y ~ x1, data = dplyr::group_by(dg, g),
                          method = "strucchange"),
               class = "ggchangepoint_unsupported")
  expect_error(cpt_test_at(y ~ x1, data = dplyr::group_by(dg, g), when = 31),
               class = "ggchangepoint_unsupported")
})

test_that("print() formats both ends of a timestamp index alike", {
  idx <- as.POSIXct("2020-03-07", tz = "UTC") + 3600 * 0:199
  # Formatted apart, the midnight end lost its time of day; which format
  # the pair gets together is R's choice, so compare with that.
  ends <- format(idx[c(1L, 200L)])
  expect_identical(ggchangepoint:::format_index_range(idx),
                   paste(ends[1], "to", ends[2]))
  expect_identical(nchar(ends[1]), nchar(ends[2]))
  expect_identical(ggchangepoint:::format_index_range(c(2000, 2016.58333)),
                   "2000 to 2016.58")
})

test_that("a fit passed where a series belongs says how to pass the series", {
  set.seed(1)
  fit <- cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 3)), method = "pelt")
  expect_error(cpt_consensus(fit), "x$data$value", fixed = TRUE,
               class = "ggchangepoint_bad_type")
})

test_that("bad arguments are refused by name, not by base R", {
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  fit <- cpt_detect(x, method = "pelt")
  bad <- "ggchangepoint_bad_argument"
  # "supplied seed is not a valid integer", unclassed, from every seeded tool
  expect_error(cpt_detect(x, seed = "a"), "`seed`", class = bad)
  expect_error(cpt_simulate(100, changepoints = 50, seed = NA), class = bad)
  # "missing value where TRUE/FALSE needed"
  expect_error(cpt_detect(x, fixed = Inf), "finite", class = bad)
  expect_error(cpt_test_at(x, when = Inf), "finite", class = bad)
  expect_error(cpt_labels(start = NA_real_, end = 110), class = bad)
  expect_equal(cpt_assumptions(fit, lag = 1e6)$flag[1], NA)
  # "non-numeric argument to binary operator", and NA or Inf simulated
  expect_error(cpt_simulate(100, changepoints = 50, params = "a"), class = bad)
  expect_error(cpt_simulate(100, changepoints = 50, params = c(0, NA)),
               class = bad)
  expect_error(cpt_simulate(100, changepoints = 50, change_in = "var",
                            params = c(1, -1)), class = bad)
  # a missing changepoint was dropped without a word
  expect_error(cpt_simulate(100, changepoints = NA), class = bad)
  # "attempt to set an attribute on NULL"
  expect_error(cpt_power(numeric(0), jump = 1), class = bad)
  expect_error(cpt_power(100, jump = "a", n_sim = 2), class = bad)
  expect_error(cpt_attribute_event(fit, event = numeric(0)), class = bad)
  # subscript errors and "the condition has length > 1"
  ev <- data.frame(when = 101, what = "ev")
  expect_error(cpt_annotate_events(fit, ev, location = c("when", "what")),
               class = bad)
  expect_error(cpt_annotate_events(fit, ev, label = 2), class = bad)
  expect_error(cpt_annotate_events(fit, ev, label = "nope"), class = bad)
  expect_error(cpt_influence(fit, subset = Inf), class = bad)
  expect_error(predict(fit, h = 2, level = 2), class = bad)
})

test_that("an event outside the series is reported as outside it", {
  set.seed(11)
  fit <- cpt_detect(c(stats::rnorm(100), stats::rnorm(100, 3)),
                    method = "pelt")
  skip_on_cran()
  out <- cpt_attribute_event(fit, event = c(-1, 1e6), B = 10, seed = 1)
  expect_equal(out$verdict, rep("the event is outside the series", 2))
})

test_that("an accessor on a fit without its engine object names the cause", {
  set.seed(3)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  fit <- cpt_detect(x, method = "binseg", keep_fit = FALSE)
  # It said binseg "does not expose a solution path", then listed binseg.
  expect_error(cpt_solution_path(fit), "keep_fit = TRUE", fixed = TRUE,
               class = "ggchangepoint_capability_absent")
  expect_error(cpt_statistic(cpt_detect(x, method = "pelt")),
               "does not expose", class = "ggchangepoint_capability_absent")
})

test_that("an export needs a file, and bandwidths and locations are checked", {
  set.seed(11)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3))
  fit <- cpt_detect(x, method = "pelt")
  expect_error(cpt_export(fit, file = NULL), class = "ggchangepoint_bad_argument")
  skip_if_not_installed("mosum")
  expect_error(cpt_scale_space(x, bandwidths = "a"),
               class = "ggchangepoint_bad_argument")
  # A missing true location was scored as no truth at all, silently.
  expect_warning(m <- cpt_metrics(fit, truth = c(100, NA)), "missing",
                 class = "ggchangepoint_dropped_input")
  expect_equal(m$n_truth, 1L)
})
