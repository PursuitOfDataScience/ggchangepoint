test_that("cpt_wrapper returns correct output", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- cpt_wrapper(x, change_in = "mean", cp_method = "PELT")
  expect_s3_class(res, "tbl_df")
  expect_true("cp" %in% names(res))
  expect_true("cp_value" %in% names(res))
  expect_gt(nrow(res), 0)
  expect_true(is.numeric(res$cp))
  expect_type(res$cp_value, "double")
})

test_that("cpt_wrapper validates input", {
  expect_error(cpt_wrapper("a"), "must be numeric")
  expect_error(cpt_wrapper(c(1, NA, 3)), "be finite")
  expect_error(cpt_wrapper(c(1, 2)), "at least 3 observations")
  expect_error(cpt_wrapper(1:10, change_in = "MEAN"), "should be one of")
  expect_error(cpt_wrapper(1:10, cp_method = "NOPE"), "should be one of")
})

test_that("cpt_wrapper accepts np alias", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res1 <- cpt_wrapper(x, change_in = "np", cp_method = "PELT")
  res2 <- cpt_wrapper(x, change_in = "cpt_np", cp_method = "PELT")
  expect_equal(nrow(res1), nrow(res2))
})

test_that("cpt_wrapper handles no change (no changepoints)", {
  set.seed(2022)
  res <- cpt_wrapper(rnorm(100), change_in = "mean")
  expect_equal(nrow(res), 0)
})

test_that("ecp_wrapper no-change bug is fixed", {
  # ecp::e.divisive always returns estimates including boundaries.
  # The wrapper should strip them unconditionally.
  set.seed(2022)
  x <- rnorm(200)
  res <- ecp_wrapper(x, algorithm = "divisive", min_size = 30)
  expect_s3_class(res, "tbl_df")
  # Should be empty or at most a small number due to random test
  expect_true(nrow(res) == 0 || all(res$cp > 1 & res$cp < length(x)))
  # No NA should appear
  expect_false(anyNA(res$cp_value))
})

test_that("ecp_wrapper validates input", {
  expect_error(ecp_wrapper(1:3, algorithm = "nope"), "should be one of")
})

test_that("ggcptplot and ggecpplot return ggplot", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  p <- ggcptplot(x, change_in = "mean")
  expect_s3_class(p, "ggplot")
  p2 <- ggecpplot(x)
  expect_s3_class(p2, "ggplot")
})

test_that("size -> linewidth deprecation works", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  expect_warning(
    ggcptplot(x, change_in = "mean", cptline_size = 2),
    "deprecated"
  )
})

test_that("need_pkg muffles a load-time warning but still reports failure", {
  # Loading an engine can warn about the machine rather than the data:
  # mosum reaches tcltk through plot3D and misc3d, so on any headless box
  # the first cpt_scale_space() call used to warn "no DISPLAY variable so
  # Tk is not available". Measuring that directly is impossible after the
  # first load in a session, so this reproduces the mechanism -- a package
  # that warns while its namespace loads -- on whichever suggested package
  # has not been loaded yet.
  # Light candidates first, then anything in Suggests that is installed and
  # not yet loaded -- this test runs late in the suite, so a short fixed
  # list is all loaded by the time it gets here and the test would skip.
  desc <- read.dcf(system.file("DESCRIPTION", package = "ggchangepoint"))
  suggests <- if ("Suggests" %in% colnames(desc)) {
    trimws(gsub("\\s*\\(.*\\)", "", strsplit(desc[1, "Suggests"], ",")[[1]]))
  } else {
    character(0)
  }
  cand <- unique(c("ChangePointTaylor", "trend", "crossvalidationCP",
                   "binsegRcpp", "gt", "ggiraph", "plotly", suggests))
  cand <- setdiff(cand, loadedNamespaces())
  cand <- cand[vapply(cand, function(p) {
    length(find.package(p, quiet = TRUE)) > 0
  }, logical(1))]
  skip_if(length(cand) == 0, "no installed-but-unloaded package to load")
  pkg <- cand[[1]]

  setHook(packageEvent(pkg, "onLoad"), function(...) warning("load noise"))
  withr::defer(setHook(packageEvent(pkg, "onLoad"), NULL,
                       action = "replace"))

  expect_no_warning(ggchangepoint:::need_pkg(pkg))
  expect_true(pkg %in% loadedNamespaces())

  # Muffling the warning must not muffle the answer: a package that is not
  # there is still an error naming it and how to install it.
  expect_error(ggchangepoint:::need_pkg("ggchangepoint.no.such.engine"),
               "ggchangepoint.no.such.engine")
})

test_that("a factor or character series is refused, not silently recoded", {
  # as.numeric() on a factor returns the LEVEL CODES, so this used to run to
  # completion and report changepoints in an alphabetical ordering of the
  # labels -- a wrong answer with nothing said about it.
  f <- factor(c(rep("low", 60), rep("high", 60)))
  expect_error(cpt_detect(f, method = "pelt"), "level codes")
  expect_error(as_cpt_series(f), "level codes")
  expect_error(cpt_select(f, method = "pelt"), "level codes")

  # Character input warned "NAs introduced by coercion" from base R and was
  # then reported as non-finite data, which blames the wrong thing.
  ch <- as.character(c(rnorm(60), rnorm(60, 4)))
  expect_error(cpt_detect(ch, method = "pelt"), "character")
  expect_no_warning(try(cpt_detect(ch, method = "pelt"), silent = TRUE))

  # A logical series is a legitimate 0/1 series and must still work.
  set.seed(1)
  lg <- c(stats::runif(60) < 0.2, stats::runif(60) < 0.8)
  expect_s3_class(cpt_detect(lg, method = "pelt"), "ggcpt")
})

test_that("empty input is reported by this package, not by base R", {
  # as.matrix(NULL) stops with "'data' must be of a vector type, was 'NULL'"
  # and a zero-column frame reaches X[, 1] and stops with "subscript out of
  # bounds"; neither names the argument or the package.
  expect_error(cpt_batch(NULL), "`x` is empty")
  expect_error(cpt_batch(list()), "at least one series")
  expect_error(cpt_consensus(data.frame(), methods = c("pelt", "amoc")),
               "`x` is empty")
  expect_error(cpt_detect(list(a = NULL, b = NULL), method = "pelt"),
               "must be a numeric vector")
})

test_that("the tools that validate first agree with the ones that coerce", {
  # cpt_detect() coerces the series before validating it and cpt_select()
  # validates before coercing, so the two used to disagree about what a
  # series is: a logical vector worked in one and was refused by the other.
  set.seed(2)
  lg <- c(stats::runif(60) < 0.2, stats::runif(60) < 0.8)
  expect_s3_class(cpt_select(lg, method = "pelt", criterion = "bic",
                             k_max = 3), "ggcpt_selection")
  expect_true(ggchangepoint:::validate_data(lg))
})

test_that("the evaluation functions say so when handed a fit", {
  # cpt_metrics(), cpt_metrics_annotated() and ggcpt_eval() are the only
  # tools in the package that take bare changepoint indices rather than the
  # `ggcpt`, so passing the fit is the obvious mistake. as.integer() used to
  # answer it with "'list' object cannot be coerced to type 'integer'", and
  # a `ggcpt` IS a list, so cpt_metrics_annotated() read one as a set of
  # annotators and scored its own fields.
  set.seed(7)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  fit <- cpt_detect(x, method = "pelt")
  cp <- fit$changepoints$cp

  expect_error(cpt_metrics(fit, 100, n = 200), "not a `ggcpt` object")
  expect_error(cpt_metrics(cp, fit, n = 200), "not a `ggcpt` object")
  expect_error(ggcpt_eval(fit, 100, x), "not a `ggcpt` object")
  expect_error(cpt_metrics_annotated(cp, fit, 200), "not a `ggcpt` object")
  expect_error(cpt_metrics(generics::tidy(fit), 100, n = 200),
               "not a table")

  # and the documented calls still work
  expect_s3_class(cpt_metrics(cp, 100, n = 200), "tbl_df")
  expect_s3_class(cpt_metrics_annotated(cp, list(100, 105), 200), "tbl_df")
  expect_s3_class(cpt_metrics_annotated(cp, 100, 200), "tbl_df")
  expect_s3_class(ggcpt_eval(cp, 100, x), "ggplot", exact = FALSE)
})

test_that("cpt_monitor says when a tuning knob the method ignores is set", {
  # The three detectors are calibrated in different currencies and each
  # ignores the others': `arl0` on an e-detector and `alpha` on cpm change
  # nothing. ?cpt_monitor and the monitoring vignette both say so, which
  # does not reach the user who set one and is reading an unchanged answer.
  set.seed(1)
  b <- stats::rnorm(100)

  expect_warning(cpt_monitor("edetector", baseline = b, arl0 = 5000),
                 "does not affect `method = \"edetector\"`")
  expect_warning(cpt_monitor("edetector", baseline = b, arl0 = 5000,
                             mc_reps = 3),
                 "`arl0`, `mc_reps` do not affect")
  # it survives cpt_replay()'s `...`
  expect_warning(cpt_replay(stats::rnorm(200), method = "edetector",
                            arl0 = 5000),
                 "does not affect")

  # the knobs that DO apply, and the defaults, stay silent
  expect_no_warning(cpt_monitor("edetector", baseline = b))
  expect_no_warning(cpt_monitor("edetector", baseline = b, alpha = 0.005))
  expect_no_warning(cpt_monitor("edetector", baseline = b,
                                deltas = c(1, 2)))
  skip_if_not_installed("cpm")
  expect_no_warning(cpt_monitor("cpm", arl0 = 500))
  expect_warning(cpt_monitor("cpm", alpha = 0.005),
                 "does not affect `method = \"cpm\"`")
})

test_that("a clamped scenario location is reported, not silent", {
  # `location` is a fraction, the realised changepoint is clamped to
  # 2..(n - 2), and the scenario table keeps the requested fraction -- so a
  # clamp leaves the row and its data disagreeing about where the change is.
  expect_no_warning(cpt_scenarios(n = 100, jump = 2, location = 0.5,
                                  seed = 1))
  expect_warning(cpt_scenarios(n = 100, jump = 2, location = 9, seed = 1),
                 "asked for a position outside")
  expect_warning(cpt_scenarios(n = 100, jump = 2, location = 0.001,
                               seed = 1),
                 "asked for a position outside")
  # the table-only form generates nothing, so there is nothing to disagree
  expect_no_warning(cpt_scenarios(n = 100, jump = 2, location = 9, seed = 1,
                                  as_datasets = FALSE))

  # and the clamp still lands inside the series
  ds <- suppressWarnings(cpt_scenarios(n = 100, jump = 2, location = 9,
                                       seed = 1))
  expect_equal(unlist(ds[[1]]$annotations), 98L)
})

test_that("ggcpt_eval validates margin the way cpt_metrics does", {
  # A negative margin drew its tolerance rectangles inside out
  # (xmin > xmax) while cpt_metrics() refused the same value.
  expect_error(ggcpt_eval(100, 100, stats::rnorm(200), margin = -1),
               "at least 0")
  expect_error(cpt_metrics(100, 100, n = 200, margin = -1), "at least 0")
  expect_s3_class(ggcpt_eval(100, 100, stats::rnorm(200), margin = 0),
                  "ggplot", exact = FALSE)
})

test_that("a factor is refused wherever changepoint locations are read", {
  # as.integer() on a factor returns LEVEL POSITIONS, so every entry point
  # that read locations through a bare as.integer() answered with the codes:
  # cpt_metrics(factor(c("100","150")), c(100,150), n = 200) reported a
  # recall of 0 for predictions that were exactly right. A wrong number that
  # looks like a measurement is worse than an error, so all of these refuse.
  set.seed(41)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  n <- length(x)
  f <- factor(c("100", "150"))       # codes are 1 and 2
  f1 <- factor("100")
  mon <- cpt_replay(x, method = "edetector")

  expect_error(cpt_metrics(f, c(100, 150), n = n), "level codes")
  expect_error(cpt_metrics(c(100, 150), f, n = n), "level codes")
  expect_error(cpt_metrics_annotated(c(100, 150), f, n = n), "level codes")
  expect_error(ggcpt_eval(c(100, 150), f, x), "level codes")
  expect_error(cpt_delay(mon, truth = f1), "level codes")
  expect_error(as_cpt_labels(f, n = n), "level codes")
  expect_error(cpt_labels(f, factor(c("110", "160"))), "level codes")
  expect_error(cpt_simulate(200, changepoints = f1), "level codes")
  expect_error(as_ggcpt(f, x), "level codes")
  expect_error(
    cpt_benchmark(list(a = list(series = x, annotations = list(f1))),
                  methods = "pelt", progress = FALSE),
    "level codes")

  # and each names the argument the caller passed
  expect_error(cpt_metrics(f, c(100, 150), n = n), "`pred`")
  expect_error(cpt_metrics(c(100, 150), f, n = n), "`truth`")
  expect_error(cpt_metrics_annotated(c(100, 150), f, n = n), "`annotations`")
  expect_error(cpt_labels(f, factor("160")), "`start`")
  expect_error(cpt_simulate(200, changepoints = f1), "`changepoints`")
})

test_that("the honest ways of passing locations all still work", {
  set.seed(41)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  n <- length(x)
  mon <- cpt_replay(x, method = "edetector")

  expect_s3_class(cpt_metrics(c(100, 150), c(100, 150), n = n), "tbl_df")
  # a character vector that converts cleanly is still accepted
  expect_s3_class(cpt_metrics(c("100", "150"), c(100, 150), n = n), "tbl_df")
  expect_s3_class(cpt_metrics_annotated(100, list(100, 102), n = n), "tbl_df")
  expect_s3_class(cpt_metrics_annotated(100, 100, n = n), "tbl_df")
  expect_s3_class(ggcpt_eval(100, 100, x), "ggplot", exact = FALSE)
  expect_s3_class(cpt_delay(mon, truth = 100), "ggcpt_delay")
  expect_s3_class(as_cpt_labels(100, n = n), "cpt_labels")
  expect_s3_class(cpt_labels(c(90, 140), c(110, 160)), "cpt_labels")
  expect_s3_class(cpt_simulate(200, changepoints = 100, seed = 1), "tbl_df")
  expect_s3_class(as_ggcpt(100, x), "ggcpt")
  expect_s3_class(
    cpt_benchmark(list(a = list(series = x, annotations = list(100L))),
                  methods = "pelt", progress = FALSE),
    "ggcpt_benchmark")
  expect_s3_class(
    cpt_benchmark(list(a = list(series = x, changepoints = 100L)),
                  methods = "pelt", progress = FALSE),
    "ggcpt_benchmark")
})

test_that("cpt_report validates `file` before writing anything", {
  set.seed(51)
  fit <- cpt_detect(c(stats::rnorm(80), stats::rnorm(80, 4)), method = "pelt")

  # writeLines() answers these with "cannot open the connection", "'con' is
  # not a connection" and "invalid 'description' argument"; `file = ""`
  # answers with the report on the console and no file at all, so the
  # caller has a report they believe they saved.
  expect_error(cpt_report(fit, file = "", session = FALSE),
               "single non-empty file path")
  expect_error(cpt_report(fit, file = NA, session = FALSE),
               "single non-empty file path")
  expect_error(cpt_report(fit, file = c(tempfile(), tempfile()),
                          session = FALSE),
               "single non-empty file path")
  expect_error(cpt_report(fit, file = tempdir(), session = FALSE),
               "is a directory")
  expect_error(cpt_report(fit, file = file.path(tempdir(), "no", "such",
                                                "r.md"), session = FALSE),
               "does not exist")

  # a good path writes the report and returns it invisibly
  tf <- withr::local_tempfile(fileext = ".md")
  res <- withVisible(cpt_report(fit, file = tf, session = FALSE))
  expect_false(res$visible)
  expect_true(file.exists(tf))
  expect_identical(readLines(tf), res$value)

  # `file` is documented as ignored for format = "gt", so a bad path there
  # must not become an error
  expect_no_error(cpt_report(fit, format = "gt", file = "", session = FALSE))
})

test_that("a changepoint after every observation is reported, not returned quietly", {
  # validate_data() accepts three observations, and at that length several
  # engines return a changepoint after every one -- k = n - 1, every segment
  # one point long, which is a failure to segment rather than a
  # segmentation. Measured at n = 3: pelt, fpop, wbs2, tguh, smuce, decafs
  # and nsp all do it; at n = 5, wbs2, decafs and nsp still do. The
  # threshold is engine-specific, so the check is on the result.
  expect_warning(cpt_detect(c(1, 5, 9), method = "pelt"),
                 "changepoint after every observation")
  # `fpop` shows the same thing, but it is a Suggests engine -- the
  # guarded test below covers it, so this block stays runnable on an
  # Imports-only installation.
  # it reaches user-supplied segmentations too, since they share the builder
  expect_warning(as_ggcpt(1:2, c(1, 5, 9)),
                 "changepoint after every observation")

  # and it stays quiet for every non-degenerate result, including a
  # legitimate single changepoint on the same three points
  expect_no_warning(cpt_detect(c(1, 5, 9), method = "amoc"))
  expect_no_warning(cpt_detect(c(1, 2, 10, 11, 12), method = "pelt"))
  set.seed(101)
  expect_no_warning(cpt_detect(c(stats::rnorm(60), stats::rnorm(60, 4)),
                               method = "pelt"))
  expect_no_warning(as_ggcpt(60, c(stats::rnorm(60), stats::rnorm(60, 4))))
  # a result with no changepoints at all is not degenerate
  expect_no_warning(cpt_detect(stats::rnorm(200), method = "pelt"))
})

test_that("the degenerate warning names the penalty, not the series length", {
  # Two situations give one segment per observation, and blaming the wrong
  # one is worse than silence: with `penalty = 0` it is the correct
  # unpenalised optimum at any series length, while a positive penalty
  # reaching the same place means the series is too short for the engine.
  skip_if_not_installed("fpop")
  set.seed(111)
  long <- c(stats::rnorm(100), stats::rnorm(100, 4))

  expect_warning(cpt_detect(long, method = "fpop", penalty = 0),
                 "penalty of 0 that is the unpenalised optimum")
  expect_warning(cpt_detect(long, method = "fpop", penalty = "None"),
                 "penalty of 0 that is the unpenalised optimum")
  expect_warning(cpt_detect(c(1, 5, 9), method = "fpop"),
                 "too short for this engine")

  # a positive penalty on a long series is not degenerate at all
  expect_no_warning(cpt_detect(long, method = "fpop", penalty = 5))
})
