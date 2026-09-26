# 0.6.0: results leave R and come back (§121.3, §223), reports say how to
# keep them, and the sequential tools state their false-alarm budget
# (§240, §214).

test_that("as_json() writes the documented schema", {
  skip_if_not_installed("jsonlite")
  set.seed(101)
  dates <- as.Date("2026-01-01") + 0:99
  fit <- cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 3)), method = "pelt",
                    index = dates)
  j <- as_json(fit)
  expect_s3_class(j, "json")
  p <- jsonlite::fromJSON(j)
  expect_equal(p$schema, "ggchangepoint.ggcpt")
  expect_equal(p$schema_version, "1.0.0")
  fields <- c("schema", "schema_version", "method", "engine",
              "engine_version", "ggchangepoint_version", "r_version",
              "created", "change_in", "family", "penalty", "cp_convention",
              "n", "index", "changepoints", "segments", "regions",
              "coefficients", "constraints", "diagnostics", "assumptions",
              "call", "data")
  expect_true(all(fields %in% names(jsonlite::fromJSON(j,
                                                       simplifyVector = FALSE))))
  expect_equal(p$changepoints$cp, fit$changepoints$cp)
  expect_equal(p$index$class, "Date")
  expect_equal(p$changepoints$cp_index, format(fit$changepoints$cp_index))
  lean <- jsonlite::fromJSON(as_json(fit, data = FALSE, assumptions = FALSE),
                             simplifyVector = FALSE)
  expect_null(lean$data)
  expect_null(lean$assumptions)
  expect_error(as_json(1:3), class = "ggchangepoint_bad_argument")
  expect_equal(unclass(cpt_report(fit, format = "json")), unclass(j),
               ignore_attr = TRUE)
})

test_that("a JSON export rebuilds a working result", {
  skip_if_not_installed("jsonlite")
  set.seed(102)
  dates <- as.Date("2026-01-01") + 0:119
  fit <- cpt_detect(c(stats::rnorm(60), stats::rnorm(60, 4)), method = "pelt",
                    index = dates)
  path <- withr::local_tempfile(fileext = ".json")
  expect_identical(cpt_export(fit, path), path)
  back <- cpt_import(path)
  expect_s3_class(back, "ggcpt")
  expect_equal(back$changepoints$cp, fit$changepoints$cp)
  expect_equal(back$data$value, fit$data$value)
  expect_s3_class(back$index, "Date")
  expect_equal(back$index, fit$index)
  expect_equal(back$method, "pelt")
  expect_null(back$fit)
  expect_equal(back$versions$engine, fit$versions$engine)
  # The rebuilt result works with the rest of the package.
  expect_s3_class(ggplot2::autoplot(back), "ggplot")
  expect_equal(tidy(back)$cp, tidy(fit)$cp)
  # Written without the data, there is nothing to rebuild on.
  lean <- withr::local_tempfile(fileext = ".json")
  cpt_export(fit, lean, data = FALSE)
  expect_error(cpt_import(lean), class = "ggchangepoint_capability_absent")
})

test_that("a CSV export carries the segmentation and reads back", {
  set.seed(103)
  fit <- cpt_detect(c(stats::rnorm(60), stats::rnorm(60, 4)), method = "pelt")
  path <- withr::local_tempfile(fileext = ".csv")
  cpt_export(fit, path)
  d <- utils::read.csv(path)
  expect_true(all(c("index", "value", "seg_id", "is_changepoint") %in%
                    names(d)))
  expect_equal(which(d$is_changepoint), fit$changepoints$cp)
  back <- cpt_import(path, method = "pelt")
  expect_equal(back$changepoints$cp, fit$changepoints$cp)
  expect_equal(back$method, "pelt")
})

test_that("export and import refuse what they cannot handle", {
  set.seed(104)
  fit <- cpt_detect(c(stats::rnorm(40), stats::rnorm(40, 3)), method = "pelt")
  expect_error(cpt_export(fit, withr::local_tempfile(fileext = ".xlsx")),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_export(1:3, withr::local_tempfile(fileext = ".csv")),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_import("no/such/file.json"),
               class = "ggchangepoint_bad_argument")
  bad <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3), bad, row.names = FALSE)
  expect_error(cpt_import(bad), class = "ggchangepoint_bad_argument")
  skip_if_not_installed("jsonlite")
  other <- withr::local_tempfile(fileext = ".json")
  writeLines('{"hello": 1}', other)
  expect_error(cpt_import(other), class = "ggchangepoint_bad_argument")
  newer <- withr::local_tempfile(fileext = ".json")
  writeLines('{"schema": "ggchangepoint.ggcpt", "schema_version": "2.0.0"}',
             newer)
  expect_error(cpt_import(newer), class = "ggchangepoint_unsupported")
})

test_that("a report says how to keep the result", {
  set.seed(105)
  fit <- cpt_detect(c(stats::rnorm(40), stats::rnorm(40, 3)), method = "pelt")
  rep_kept <- cpt_report(fit, session = FALSE)
  expect_true(any(grepl("keep_fit = FALSE", rep_kept, fixed = TRUE)))
  lean <- cpt_detect(c(stats::rnorm(40), stats::rnorm(40, 3)), method = "pelt",
                     keep_fit = FALSE)
  rep_lean <- cpt_report(lean, session = FALSE)
  expect_true(any(grepl("no packages installed", rep_lean, fixed = TRUE)))
  expect_false(any(grepl("drop it before saving", rep_lean, fixed = TRUE)))
  expect_true(any(grepl("drop it before saving", rep_kept, fixed = TRUE)))
  expect_true(any(grepl("Assumptions", rep_kept, fixed = TRUE)))
})

test_that("cpm's default false-alarm budget scales with the series", {
  expect_equal(ggchangepoint:::cpm_default_arl0(100), 500)
  expect_equal(ggchangepoint:::cpm_default_arl0(1000), 5000)
  expect_equal(ggchangepoint:::cpm_default_arl0(1e6), 50000)
  skip_on_cran()
  skip_if_not(engine_usable("cpm"))
  set.seed(106)
  fit <- cpt_detect(c(stats::rnorm(300), stats::rnorm(300, 2)), method = "cpm")
  expect_equal(fit$penalty$value, 3000)
  expect_equal(fit$diagnostics$expected_false_positives, 600 / 3000)
})

test_that("a monitor's baseline is checked before it is trusted", {
  skip_on_cran()
  skip_if_not(engine_usable("cpm"))
  set.seed(107)
  shifted <- c(stats::rnorm(100), stats::rnorm(100, 3))
  caught <- list()
  mon <- withCallingHandlers(
    cpt_monitor("cpm", baseline = shifted),
    warning = function(cnd) {
      caught[[length(caught) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    })
  tripped <- Filter(function(w) !is.null(w$n_changes), caught)
  expect_length(tripped, 1L)
  expect_s3_class(tripped[[1]], "ggchangepoint_assumption")
  expect_gte(tripped[[1]]$n_changes, 1L)
  # The inherited alarm is gone: data like the stretch after the baseline's
  # last change raise nothing at once.
  mon <- cpt_update(mon, stats::rnorm(5, 3))
  expect_equal(nrow(mon$alarms), 0L)
  ar <- as.numeric(stats::arima.sim(list(ar = 0.8), 200))
  expect_warning(cpt_monitor("edetector", baseline = ar),
                 class = "ggchangepoint_dependence")
  expect_no_warning(cpt_monitor("edetector", baseline = stats::rnorm(200)))
})
