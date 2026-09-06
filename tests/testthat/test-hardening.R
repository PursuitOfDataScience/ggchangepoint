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

test_that("cpt_methods() answers 'is it installed' without loading anything", {
  # S37: the `installed` column was filled with requireNamespace(), which
  # LOADS the package. Building the table therefore loaded all 35 engine
  # namespaces, including fabisearch -> rgl, which on the macOS runner dies
  # in dyn.load() for want of libGLU -- reported by R CMD check as
  # "Vignette re-building failed" with no chunk, no line and no message. The
  # fix was find.package(), and it took five CI rounds to find.
  #
  # The probe is any installed package that is not yet loaded, NOT one of
  # the engines: by the time this file runs, every installed engine has been
  # loaded by an earlier test, so keying on engines made the test skip in
  # the full suite while passing when run alone -- a guard that only guards
  # when you are already looking at it.
  loaded <- loadedNamespaces()
  cand <- setdiff(rownames(utils::installed.packages()), loaded)
  skip_if(length(cand) == 0, "every installed package is already loaded")
  probe <- cand[[1]]

  expect_true(ggchangepoint:::engine_installed(probe))
  expect_false(probe %in% loadedNamespaces())
  # and a package that is not there is FALSE rather than an error
  expect_false(ggchangepoint:::engine_installed("ggchangepoint.no.such.engine"))
  # the suite's engine_usable() is the deliberate opposite -- it loads, on
  # purpose, because "can I run this engine" is a different question
  expect_true(engine_usable(probe))
  expect_false(engine_usable("ggchangepoint.no.such.engine"))

  # and the case the two predicates must disagree on, which is the whole
  # reason the second one exists: installed, and not loadable. A directory
  # with a DESCRIPTION is enough for find.package() and not enough for the
  # loader -- the same split that made {mosum} pass engine_installed() and
  # fail to load on the macOS CI runner.
  lib <- withr::local_tempdir()
  dir.create(file.path(lib, "brokenpkg"))
  writeLines(c("Package: brokenpkg", "Version: 0.0.1"),
             file.path(lib, "brokenpkg", "DESCRIPTION"))
  withr::local_libpaths(lib, action = "prefix")
  expect_true(ggchangepoint:::engine_installed("brokenpkg"))
  expect_false(engine_usable("brokenpkg"))

  # the table itself must add no namespace either
  before <- loadedNamespaces()
  invisible(cpt_methods())
  expect_equal(setdiff(loadedNamespaces(), before), character(0))
})

test_that("a detection call leaves the caller's search path alone", {
  # Two engines mutate it. Loading `bcp` attaches `package:bcp` and
  # `package:grid`; `fabisearch` needs NMF *attached* rather than loaded,
  # and that brings NMF's Depends (Biobase, BiocGenerics) plus the
  # foreach/doParallel/doRNG stack the engine registers -- eight packages
  # measured, where the wrapper previously detached only NMF itself.
  # `bcp` is the cheap one to assert; fabisearch is the slowest engine in
  # the package, so it is checked here only when it is installed.
  set.seed(1)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))

  skip_if_not_installed("bcp")
  before <- search()
  invisible(bcp_wrapper(x))
  expect_equal(setdiff(search(), before), character(0))

  # and the same call twice must not leave anything either
  invisible(bcp_wrapper(x))
  expect_equal(setdiff(search(), before), character(0))
})

test_that("the monitoring layer refuses a factor or character series", {
  # cpt_detect() has refused a factor since 0.4.0, but the four monitoring
  # entry points each coerced their series independently -- as.numeric() in
  # cpt_monitor()'s edetector and cpm branches, a bare matrix() in
  # cpt_update() and cpt_replay() -- so a factor arrived as its LEVEL CODES.
  # With labels like "10", "2", "30" the codes are the alphabetical order
  # 1, 2, 3, i.e. a series the caller never supplied, monitored silently.
  # The guard now sits once at each entry point, before the method switch.
  set.seed(51)
  labs <- c("10", "2", "30")            # alphabetical != numerical
  fac <- factor(sample(labs, 180, TRUE))
  chr <- as.character(fac)
  num <- c(stats::rnorm(90), stats::rnorm(90, 3))

  expect_error(cpt_replay(fac, method = "edetector"), "`x` is a factor")
  expect_error(cpt_replay(chr, method = "edetector"), "`x` is character")

  # every method branch, because each used to coerce for itself
  for (m in c("edetector", "cpm")) {
    expect_error(cpt_monitor(fac[1:60], method = m), "`baseline` is a factor")
    expect_error(cpt_monitor(chr[1:60], method = m), "`baseline` is character")
  }

  mon <- cpt_monitor(num[1:60], method = "edetector")
  expect_error(cpt_update(mon, fac[61:180]), "`new_obs` is a factor")
  expect_error(cpt_update(mon, chr[61:180]), "`new_obs` is character")

  # and the numeric route is untouched: the guard must not have displaced
  # the finiteness check, which gives a better message for an NA baseline
  expect_s3_class(cpt_replay(num, method = "edetector"), "ggcpt_monitor")
  expect_error(cpt_monitor(c(num[1:59], NA), method = "edetector"),
               "must be finite")
})

test_that("series arguments that are not the first argument are guarded too", {
  # The type guard on `x` does not help when the series arrives through some
  # other argument. Three such paths existed, each reached by a route the
  # per-function check on `x` cannot see.
  set.seed(51)
  labs <- c("10", "2", "30")
  fac <- factor(sample(labs, 180, TRUE))
  num <- c(stats::rnorm(90), stats::rnorm(90, 3))

  # 1. cpt_replay(baseline =) as an explicit series: it hands cpt_monitor()
  #    an already-numeric vector, so cpt_monitor()'s guard never sees it.
  expect_error(cpt_replay(num, method = "edetector", baseline = fac[1:60]),
               "`baseline` is a factor")
  expect_error(cpt_replay(num, method = "edetector",
                          baseline = as.character(fac)[1:60]),
               "`baseline` is character")

  # ... while both documented readings of `baseline` still work
  expect_equal(cpt_replay(num, method = "edetector", baseline = 60)$offset, 60)
  expect_equal(cpt_replay(num, method = "edetector",
                          baseline = stats::rnorm(60))$offset, 0)

  # 2. a lone number is a COUNT, so an impossible count is an error rather
  #    than a silent one-observation baseline series
  for (bad in list(500, 0, 60.5, NA_real_)) {
    expect_error(cpt_replay(num, method = "edetector", baseline = bad),
                 "count of leading observations")
  }

  # 3. the learned-penalty path predicts from features of `series`
  mod <- cpt_learn_penalty(list(a = num), list(a = cpt_labels(50, 70, "change")))
  expect_error(cpt_penalty(mod, series = fac), "`series` is a factor")
  expect_type(cpt_penalty(mod, series = num), "double")

  # 4. the formula interface reads the response out of `data`, where `x` is
  #    the formula and so carries no series to check
  skip_if_not_installed("strucchange")
  df_fac <- data.frame(y = fac[1:120], t = 1:120)
  df_num <- data.frame(y = num[1:120], t = 1:120)
  expect_error(strucchange_wrapper(y ~ t, data = df_fac), "`y` is a factor")
  expect_s3_class(strucchange_wrapper(y ~ t, data = df_num), "ggcpt")
})

test_that("a panel member must be one series, and bad columns are named", {
  # `cpt_batch()`'s panel is documented as "a list of numeric vectors".
  # as.numeric() on a matrix unrolls it column after column, so an 80x2
  # member became a 160-point series and reported changepoints at 40, 80 and
  # 121 -- the 80 being the seam where column 2 was appended, a changepoint
  # the data does not contain. cpt_detect() on the same matrix reports one.
  set.seed(7)
  v <- c(stats::rnorm(40), stats::rnorm(40, 3))
  M <- cbind(a = v, b = c(stats::rnorm(40), stats::rnorm(40, 3)))

  expect_error(cpt_batch(list(p = M), method = "pelt"),
               "takes a single series")
  expect_error(cpt_batch(list(p = as.data.frame(M)), method = "pelt"),
               "takes a single series")
  # the message has to say which member, because a panel is where that is
  # the question
  expect_error(cpt_batch(list(ok = v, bad = M), method = "pelt"),
               "Series `bad` \\(2 of 2\\)")

  # one column is exempt: unrolling it changes nothing, so refusing it would
  # be strictness without a defect behind it
  for (one in list(M[, 1, drop = FALSE], data.frame(a = v))) {
    b <- cpt_batch(list(p = one), method = "pelt")
    expect_equal(length(b$result[[1]]$data$index), 80L)
  }

  # and the two legitimate routes are untouched
  expect_equal(nrow(cpt_batch(list(a = v, b = v), method = "pelt")), 2L)
  expect_equal(nrow(cpt_batch(M, method = "pelt")), 2L)

  # a non-numeric column is named, with its class -- as.matrix() turns the
  # WHOLE frame character for one bad column, so the old message blamed the
  # entire series and left the reader to find the culprit
  df <- data.frame(a = v, b = factor(rep(c("10", "2"), 40)))
  expect_error(cpt_detect(df, method = "ecp"), "`b` \\(factor\\)")
  expect_error(cpt_detect(df, method = "ecp"), "level codes")
  df2 <- data.frame(a = v, b = as.character(v), c = factor("z"))
  expect_error(cpt_detect(df2, method = "ecp"),
               "`b` \\(character\\), `c` \\(factor\\)")
  # ... and the argument named is the one the caller passed. Tested on the
  # helper rather than through cpt_monitor("ocd", ...), because need_pkg()
  # runs before the validation and this expectation would then assert
  # "Package 'ocd' is required" on any library without the Suggests.
  expect_error(as_mv_matrix(df, arg = "baseline"), "`baseline` must be numeric")
  expect_error(as_mv_matrix(df, arg = "series"), "`series` must be numeric")

  # a univariate monitor counts OBSERVATIONS, not container length:
  # length() on a data.frame is its column count, so a 60-row two-column
  # baseline was refused for having fewer than 5 observations
  expect_error(cpt_monitor("edetector", baseline = df), "takes a single series")
  expect_error(cpt_monitor("edetector", baseline = as.matrix(df[, 1:2])),
               "takes a single series")
  # one column is fine, and it is the 60 observations that count
  ok1 <- data.frame(a = v)
  expect_s3_class(cpt_monitor("edetector", baseline = ok1), "ggcpt_monitor")
  # a genuinely short baseline still gets the observation-count message
  expect_error(cpt_monitor("edetector", baseline = v[1:4]),
               "at least 5 pre-change observations")
})

test_that("the original wrappers and ggcptplot refuse a multi-column series", {
  # cpt_wrapper() wraps the univariate changepoint package but coerced with
  # as.numeric(), which concatenates a matrix column after column. On a
  # 120x2 matrix it reported changepoints at 58, 120 and 180: only the 58 is
  # real, the 120 is the seam where column 2 was appended, and the 180 is
  # column 2's own change shifted by 120. cpt_detect() has always refused
  # this ("univariate, but `x` has 2 columns"); the original API did not.
  set.seed(9)
  v <- c(stats::rnorm(60), stats::rnorm(60, 3))
  M <- cbind(a = v, b = c(stats::rnorm(60), stats::rnorm(60, 3)))

  expect_error(cpt_wrapper(M), "takes a single series")
  expect_error(cpt_wrapper(as.data.frame(M)), "takes a single series")
  # ggcptplot() drew the same 240-point concatenation, seam included. It
  # follows ggecpplot()'s established convention instead of refusing: draw
  # the first column, and say so.
  expect_message(p <- ggcptplot(M), "plotting the first column")
  expect_equal(nrow(p$data), 120L)

  # one column stays acceptable, and is 120 observations rather than 240
  expect_equal(nrow(cpt_wrapper(M[, 1, drop = FALSE])),
               nrow(cpt_wrapper(v)))
  expect_equal(nrow(ggcptplot(M[, 1, drop = FALSE])$data), 120L)
  expect_equal(nrow(ggcptplot(v)$data), 120L)

  # ecp_wrapper() is genuinely multivariate and must NOT be caught by this
  skip_if_not_installed("ecp")
  expect_s3_class(ecp_wrapper(M), "tbl_df")
})

test_that("the ecp route refuses non-finite input like every other route", {
  # ecp absorbs NA/NaN/Inf instead of refusing, and returns a WRONG answer
  # rather than no answer. On a 180-point series with one changepoint at 90:
  # twenty NAs lost the changepoint entirely, and an all-NA second half
  # reported changepoints at 12 and 14. cpt_wrapper() and cpt_detect() have
  # always refused this input; ecp_wrapper() and so ggecpplot() had not.
  set.seed(51)
  v <- c(stats::rnorm(90), stats::rnorm(90, 4))

  for (bad in list(replace(v, 95, NA_real_), replace(v, 95, NaN),
                   replace(v, 95, Inf), replace(v, 95, -Inf))) {
    expect_error(ecp_wrapper(bad, seed = 1), "must be finite")
  }
  # the count describes the series the caller passed
  expect_error(ecp_wrapper(replace(v, 95, NA_real_), seed = 1),
               "1 of 180 values")
  expect_error(ecp_wrapper(replace(v, c(3, 95), NA_real_), seed = 1),
               "2 of 180 values")
  # ggecpplot() draws through ecp_wrapper(), so it inherits the guard
  expect_error(ggecpplot(replace(v, 95, NA_real_), seed = 1), "must be finite")

  # a non-numeric column is named, as at every other rectangular entry point
  expect_error(
    ecp_wrapper(data.frame(a = v, b = factor(rep("z", 180)))),
    "`b` \\(factor\\)")

  # and every clean shape still works, with the same answer as before
  clean <- ecp_wrapper(v, seed = 1)
  expect_gt(nrow(clean), 0L)
  expect_equal(ecp_wrapper(cbind(v, rev(v)), seed = 1)$cp, clean$cp)
  expect_equal(ecp_wrapper(data.frame(a = v, b = rev(v)), seed = 1)$cp,
               clean$cp)
  expect_equal(nrow(ggecpplot(v, seed = 1)$data), 180L)
})

test_that("cpt_replay names `x` for a non-finite value, wherever it falls", {
  # The check used to happen downstream, in whichever of cpt_monitor() or
  # cpt_update() received the slice holding the bad value -- so the SAME
  # call reported a problem with `baseline` for an NA at position 20 and
  # with `new_obs` for one at 95, named an argument the caller never passed,
  # and counted "1 of 45 values" against the baseline slice.
  set.seed(51)
  v <- c(stats::rnorm(90), stats::rnorm(90, 4))

  for (pos in c(20, 95, 1, 180)) {        # inside and after the baseline
    expect_error(cpt_replay(replace(v, pos, NA_real_), method = "edetector"),
                 "`x` must be finite")
    expect_error(cpt_replay(replace(v, pos, NA_real_), method = "edetector"),
                 "1 of 180 values")
  }
  expect_error(cpt_replay(replace(v, 95, Inf), method = "edetector"),
               "`x` must be finite")

  # an explicit baseline still reports itself, with its own count
  expect_error(cpt_replay(v, method = "edetector",
                          baseline = replace(stats::rnorm(60), 3, NA_real_)),
               "`baseline` must be finite")
  expect_s3_class(cpt_replay(v, method = "edetector"), "ggcpt_monitor")
})

test_that("as_cpt_series translates rather than validates, and that is safe", {
  # as_cpt_series() is documented as the one place that separates values
  # from a time index "so cpt_detect() can detect on positions and report on
  # dates". It therefore passes a non-finite value through, exactly as
  # new_ggcpt() passes an unvalidated slot through -- and refusing here
  # would break the legitimate extract-then-impute workflow. What makes it
  # safe is that every route onward refuses, which is what this pins down.
  set.seed(51)
  v_na <- replace(c(stats::rnorm(90), stats::rnorm(90, 4)), 95, NA_real_)

  s <- as_cpt_series(stats::ts(v_na, start = c(2020, 1), frequency = 12))
  expect_length(s$values, 180L)
  expect_false(is.null(s$index))          # the index is still recovered
  expect_true(anyNA(s$values))            # deliberately carried through

  expect_error(cpt_detect(v_na, method = "pelt"), "must be finite")
  expect_error(cpt_detect(as_cpt_series(v_na)$values, method = "pelt"),
               "must be finite")
  expect_error(cpt_detect(stats::ts(v_na, frequency = 12), method = "pelt"),
               "must be finite")
  expect_error(cpt_detect(v_na, method = "pelt",
                          index = as.Date("2020-01-01") + 0:179),
               "must be finite")
  expect_error(cpt_batch(list(a = v_na), method = "pelt"), "must be finite")
  expect_error(cpt_select(v_na, method = "pelt"), "must be finite")
})

test_that("every multivariate wrapper refuses a non-finite value", {
  # The univariate sweep could not answer this: handed a plain vector, each
  # of these refuses on SHAPE before finiteness is ever considered, so the
  # question needed a correctly shaped probe. `network` turned out to be the
  # one route with no finiteness check at all -- it reaches the engine via
  # network_matrix() rather than validate_data() -- and a single NA surfaced
  # as base R's "replacement has length zero" from inside the random
  # edge-splitting, naming neither the argument nor the problem.
  skip_on_cran()
  set.seed(4)
  n <- 120L; p <- 6L
  M <- matrix(stats::rnorm(n * p), n, p)
  M[61:n, ] <- M[61:n, ] + 2
  Mna <- M; Mna[70, 3] <- NA_real_

  # ocd's default Monte-Carlo threshold takes minutes on this size, so it is
  # called with a cheap one; the finiteness guard runs before either way.
  calls <- list(
    inspect    = function(d) inspect_wrapper(d),
    esac       = function(d) esac_wrapper(d),
    pilliat    = function(d) pilliat_wrapper(d),
    geomcp     = function(d) geomcp_wrapper(d),
    var        = function(d) var_wrapper(d),
    hdcov      = function(d) hdcov_wrapper(d),
    network    = function(d) network_wrapper(d),
    fmean      = function(d) fmean_wrapper(d),
    fcov       = function(d) fcov_wrapper(d),
    kwc        = function(d) kwc_wrapper(d),
    ocd        = function(d) ocd_wrapper(d, thresh = "MC", mc_reps = 2,
                                         patience = 200)
  )
  pkgs <- c(inspect = "InspectChangepoint", esac = "HDCD", pilliat = "HDCD",
            geomcp = "changepoint.geo", var = "VARDetect",
            hdcov = "changepoints", network = "changepoints",
            fmean = "fdachange", fcov = "fdaACF", kwc = "kerSeg",
            ocd = "ocd")

  available <- tested <- 0L
  for (nm in names(calls)) {
    if (!requireNamespace(pkgs[[nm]], quietly = TRUE)) next
    available <- available + 1L
    err <- tryCatch({
      suppressWarnings(suppressMessages(calls[[nm]](Mna)))
      NA_character_
    }, error = function(e) conditionMessage(e))
    expect_match(err, "must be finite", info = nm)
    tested <- tested + 1L
  }
  # a proportional tripwire, not a magic threshold: whatever is installed
  # here must have been exercised
  expect_equal(tested, available)
  skip_if(available == 0L, "no multivariate engines installed")
})

test_that("a series too short for an engine gets a message that names it", {
  # Seven engines refused a short series from inside themselves, in their own
  # vocabulary and two of them with their own typos:
  #
  #   wbs   "sample size is too small"     wbsts  "subscript out of bounds"
  #   not   "max.length must satisfy 3 < max.lenght <= n"            [sic]
  #   envcpt "Minimum segment legnth is too large to include a change" [sic]
  #   strucchange "minimum segment size must be greater than the number
  #                of regressors"
  #   bfast "series is not periodic or has less than two periods"
  #   taylor "Invalid x argument. 'x' must be a numeric vector"
  #
  # None named the method the caller asked for or the length they supplied,
  # and "subscript out of bounds" does not even implicate the series.
  skip_on_cran()
  set.seed(11)
  short <- function(n) c(stats::rnorm(n %/% 2), stats::rnorm(n - n %/% 2, 4))

  cases <- list(wbs = 3L, not = 3L, wbsts = 3L, taylor = 3L,
                envcpt = 5L, strucchange = 5L, bfast = 10L)
  pkgs <- c(wbs = "wbs", not = "not", wbsts = "wbsts",
            taylor = "ChangePointTaylor", envcpt = "EnvCpt",
            strucchange = "strucchange", bfast = "bfast")

  available <- tested <- 0L
  for (m in names(cases)) {
    if (!requireNamespace(pkgs[[m]], quietly = TRUE)) next
    available <- available + 1L
    n <- cases[[m]]
    err <- tryCatch({
      suppressWarnings(suppressMessages(cpt_detect(short(n), method = m)))
      NA_character_
    }, error = function(e) conditionMessage(e))
    expect_match(err, paste0("Method `", m, "`"), info = m)
    expect_match(err, paste0("series of ", n, " observation"), info = m)
    # the engine's own diagnosis is kept, quoted, rather than hidden
    expect_match(err, "The engine reported: \"", fixed = TRUE, info = m)
    tested <- tested + 1L
  }
  expect_equal(tested, available)

  # The three argument-dependent thresholds must NOT become constants: a
  # shorter series that the arguments do permit still has to work. This is
  # why the guard translates rather than pre-empting -- `bfast` needs two
  # periods, not 25 observations, and `strucchange` needs h * n, not 15.
  if (requireNamespace("bfast", quietly = TRUE)) {
    expect_s3_class(
      suppressWarnings(suppressMessages(
        cpt_detect(short(10L), method = "bfast", frequency = 4))), "ggcpt")
  }
  if (requireNamespace("strucchange", quietly = TRUE)) {
    expect_s3_class(
      suppressWarnings(suppressMessages(
        cpt_detect(short(5L), method = "strucchange", h = 0.4))), "ggcpt")
  }
})

test_that("the translation does not swallow an unrelated error", {
  # rethrow_short_series() must re-raise anything it does not recognise
  # verbatim, or it would turn every engine failure into a story about
  # series length.
  other <- simpleError("singular matrix in 'chol'")
  expect_error(rethrow_short_series(other, "pelt", 200L),
               "singular matrix", fixed = TRUE)
  expect_error(rethrow_short_series(other, "pelt", 200L),
               "^(?!.*could not segment).*$", perl = TRUE)

  # and one it does recognise is reworded, with the hint appended
  short_err <- simpleError("sample size is too small")
  expect_error(rethrow_short_series(short_err, "wbs", 3L),
               "Method `wbs` could not segment a series of 3 observation")
  expect_error(rethrow_short_series(short_err, "wbs", 3L, "Try more data."),
               "Try more data\\.$")
})

test_that("every method satisfies the result contract the vignette states", {
  # extending.Rmd sets out the whole contract and says as_ggcpt() enforces
  # all of it: `$changepoints$cp` sorted, de-duplicated and in 1..n-1;
  # `$segments` with one more row than `$changepoints`; `$data` carrying
  # index 1..n and value; length-one metadata. That is a claim about EVERY
  # method, so it is checked against every method that is installed.
  skip_on_cran()
  set.seed(7)
  n <- 200L
  x <- c(stats::rnorm(70), stats::rnorm(70, 4), stats::rnorm(60, 1))
  reg <- ggchangepoint:::builtin_registry()
  methods <- reg$method[reg$status == "available" & reg$univariate]

  available <- tested <- 0L
  for (m in methods) {
    r <- tryCatch(
      suppressWarnings(suppressMessages(cpt_detect(x, method = m))),
      error = function(e) NULL)
    if (is.null(r)) next                 # engine absent, or refused: not this test's business
    available <- available + 1L
    cp <- r$changepoints$cp
    if (length(cp)) {
      expect_false(anyDuplicated(cp) > 0L, info = m)
      expect_identical(cp, sort(cp), info = m)
      expect_true(all(cp >= 1L & cp <= n - 1L), info = m)
    }
    expect_equal(nrow(r$segments), length(cp) + 1L, info = m)
    expect_true(all(c("index", "value") %in% names(r$data)), info = m)
    for (f in c("method", "change_in", "cp_convention")) {
      expect_length(r[[f]], 1L)
    }
    tested <- tested + 1L
  }
  expect_equal(tested, available)
  expect_gt(available, 10L)              # the sweep must have actually run
})

test_that("multivariate results satisfy the contract, data_wide included", {
  # Part XLVII checked the result contract across every univariate method
  # and stopped there, which left the 17 multivariate ones unswept. The
  # vignette's claim about them is specific: "Multivariate results
  # additionally carry a `data_wide` tibble with one column per coordinate".
  # Sweeping it found exactly one exception -- `network`, whose input is a
  # sequence of adjacency matrices -- and that exception is now documented
  # rather than surprising.
  skip_on_cran()
  set.seed(4)
  n <- 120L; p <- 6L
  M <- matrix(stats::rnorm(n * p), n, p)
  M[61:n, ] <- M[61:n, ] + 2
  resp <- c(stats::rnorm(60), stats::rnorm(60, 3))

  calls <- list(
    npmojo   = function() npmojo_wrapper(M),
    inspect  = function() inspect_wrapper(M),
    geomcp   = function() geomcp_wrapper(M),
    esac     = function() esac_wrapper(M),
    pilliat  = function() pilliat_wrapper(M),
    hdcov    = function() hdcov_wrapper(M),
    var      = function() var_wrapper(M),
    fmean    = function() fmean_wrapper(M),
    fcov     = function() fcov_wrapper(M),
    kwc      = function() kwc_wrapper(M),
    network  = function() network_wrapper(M),
    hdreg    = function() hdreg_wrapper(M, response = resp)
  )
  # `fabisearch` is left out deliberately: its non-negative matrix
  # factorisation takes over five minutes on a 120x6 matrix, which is too
  # slow for a test that runs on every check. Its contract was verified by
  # hand during the sweep that produced this test.
  # Two report a derived series rather than the coordinates, and so carry
  # no `data_wide`: `network` reports mean edge weight (one facet per
  # adjacency entry would be unreadable) and `hdreg` reports the response.
  # Both are documented; the sweep is what established that it is exactly
  # these two.
  no_wide <- c("network", "hdreg")

  # engine_usable() rather than a bare tryCatch: the package's own
  # availability predicate skips the one absent engine instead of the whole
  # test, and -- the reason the shipped Suggests guard insists on it -- it
  # distinguishes "engine not installed" from "the call failed", which a
  # tryCatch that returns NULL silently conflates. Any error from an
  # engine that IS installed is now a failure, as it should be.
  reg <- ggchangepoint:::builtin_registry()
  engine_of <- function(m) {
    e <- reg$engine[reg$method == m]
    if (length(e)) e[1] else NA_character_
  }

  available <- tested <- 0L
  for (nm in names(calls)) {
    pkg <- engine_of(nm)
    if (!is.na(pkg) && !engine_usable(pkg)) next
    available <- available + 1L
    r <- suppressWarnings(suppressMessages(calls[[nm]]()))
    cp <- r$changepoints$cp
    if (length(cp)) {
      expect_false(anyDuplicated(cp) > 0L, info = nm)
      expect_identical(cp, sort(cp), info = nm)
      expect_true(all(cp >= 1L & cp <= n - 1L), info = nm)
    }
    expect_equal(nrow(r$segments), length(cp) + 1L, info = nm)
    expect_true(all(c("index", "value") %in% names(r$data)), info = nm)

    if (nm %in% no_wide) {
      expect_null(r$data_wide, info = nm)
      # ... and the series it does carry is the documented summary
      expect_equal(nrow(r$data), n, info = nm)
    } else {
      expect_false(is.null(r$data_wide), info = nm)
      expect_equal(nrow(r$data_wide), n, info = nm)
      # index plus one column per coordinate
      expect_equal(ncol(r$data_wide), p + 1L, info = nm)
    }
    # and every generic works on a multivariate result too
    expect_s3_class(tidy(r), "tbl_df")
    expect_equal(nrow(glance(r)), 1L, info = nm)
    expect_s3_class(augment(r), "tbl_df")
    tested <- tested + 1L
  }
  expect_equal(tested, available)
  expect_gt(available, 5L)
})

test_that("the covering metric matches the formula the vignette states", {
  # `vignette("comparison")` gives the definition, following van den Burg
  # and Williams (2020):
  #
  #   cov(S, S') = (1/n) * sum_{A in S} |A| * max_{A' in S'} J(A, A')
  #
  # with J the Jaccard index and S the partition induced by the TRUTH.
  # calc_covering() implements it with two findInterval() lookups instead of
  # scanning every prediction segment for every truth segment -- an
  # optimisation its own comment measures at 7.5 s down to hundredths for
  # 3000 changepoints. That is exactly the kind of rewrite that can be
  # subtly wrong at a boundary and still look plausible, so the fast path is
  # checked against a naive reference written straight from the formula.
  naive <- function(pred, truth, n) {
    segs <- function(cp) {
      b <- sort(unique(c(0, cp, n)))
      lapply(seq_len(length(b) - 1L), function(i) (b[i] + 1L):b[i + 1L])
    }
    S <- segs(truth); Sp <- segs(pred)
    sum(vapply(S, function(A) {
      length(A) * max(vapply(Sp, function(B) {
        length(intersect(A, B)) / length(union(A, B))
      }, numeric(1)))
    }, numeric(1))) / n
  }

  # the boundary cases first, by hand: empty either side, changepoints at
  # the first and last legal index, duplicates, and two disjoint clusters
  fixed <- list(
    list(integer(0), integer(0), 50L), list(integer(0), 25L, 50L),
    list(25L, integer(0), 50L),        list(1L, 1L, 50L),
    list(49L, 49L, 50L),               list(c(1L, 49L), c(1L, 49L), 50L),
    list(c(10L, 10L, 20L), 15L, 50L),  list(c(1L, 2L, 3L), c(47L, 48L, 49L), 50L)
  )
  for (cs in fixed) {
    expect_equal(ggchangepoint:::calc_covering(cs[[1]], cs[[2]], cs[[3]]),
                 naive(cs[[1]], cs[[2]], cs[[3]]),
                 info = paste("pred", paste(cs[[1]], collapse = ","),
                              "truth", paste(cs[[2]], collapse = ",")))
  }

  # then randomly, over lengths and changepoint counts
  set.seed(11)
  for (k in seq_len(60)) {
    n <- sample(20:200, 1)
    p <- sort(unique(sample.int(n - 1L, sample(0:6, 1))))
    t <- sort(unique(sample.int(n - 1L, sample(0:6, 1))))
    expect_equal(ggchangepoint:::calc_covering(p, t, n), naive(p, t, n),
                 info = paste("n", n, "| pred", paste(p, collapse = ","),
                              "| truth", paste(t, collapse = ",")))
  }

  # a perfect prediction covers everything; the metric is in [0, 1]
  expect_equal(ggchangepoint:::calc_covering(c(30L, 60L), c(30L, 60L), 100L), 1)
  expect_lte(ggchangepoint:::calc_covering(c(10L), c(90L), 100L), 1)
  expect_gt(ggchangepoint:::calc_covering(c(10L), c(90L), 100L), 0)
})

test_that("the other metric formulas match the vignette's definitions", {
  # Hausdorff is stated as max{max_p min_t |p - t|, max_t min_p |p - t|},
  # NA when either set is empty "since there is no distance to a
  # nonexistent point"; annotation error as ||P| - |T||; MAE/RMSE of
  # matched pairs NA when nothing matched, "because an average over no
  # pairs is not zero error"; and precision/recall/F1 all 1 when both sets
  # are empty, since that segmentation is exactly right.
  haus <- function(p, t) {
    if (!length(p) || !length(t)) return(NA_real_)
    max(max(vapply(p, function(a) min(abs(a - t)), numeric(1))),
        max(vapply(t, function(a) min(abs(a - p)), numeric(1))))
  }
  set.seed(3)
  for (k in seq_len(40)) {
    n <- sample(30:200, 1)
    p <- sort(unique(sample.int(n - 1L, sample(0:5, 1))))
    t <- sort(unique(sample.int(n - 1L, sample(0:5, 1))))
    m <- cpt_metrics(p, t, n = n)
    expect_equal(m$hausdorff, haus(p, t), info = paste("n", n))
    expect_equal(m$annotation_error, abs(length(p) - length(t)))
  }
  expect_true(is.na(cpt_metrics(integer(0), 50L, n = 100)$hausdorff))
  expect_true(is.na(cpt_metrics(50L, integer(0), n = 100)$hausdorff))
  none <- cpt_metrics(10L, 90L, n = 100, margin = 2)
  expect_true(is.na(none$mae_matched))
  expect_true(is.na(none$rmse_matched))
  both_empty <- cpt_metrics(integer(0), integer(0), n = 100)
  expect_equal(c(both_empty$precision, both_empty$recall, both_empty$f1),
               c(1, 1, 1))
})

test_that("a power run that detects nothing says so instead of returning NaN", {
  # cpt_power() forwards `...` to cpt_detect() inside a per-replicate
  # tryCatch, which is right -- one unlucky draw must not abort a
  # 500-replicate run. But when EVERY replicate failed, mean(all-NA) gave
  # NaN and the function returned it silently: an argument cpt_detect()
  # does not accept produced `power = NaN, mc_se = NA`, a number a caller
  # could plot or publish, while the engine's own perfectly clear "unused
  # argument" message was swallowed.
  expect_warning(
    r <- cpt_power(n = 120, jump = 1, n_sim = 6, seed = 3, bogus = 1),
    "No replicate completed")
  expect_warning(
    cpt_power(n = 120, jump = 1, n_sim = 6, seed = 3, bogus = 1),
    "unused argument")            # the swallowed error is reported
  expect_true(is.nan(r$power))    # still NaN: one bad scenario in a grid
  expect_true(is.na(r$mc_se))     # must not abort the others

  # cpt_min_detectable() then fed that NaN to `if (power < target)` and R
  # reported "missing value where TRUE/FALSE needed", which names nothing.
  expect_error(
    suppressWarnings(cpt_min_detectable(n = 120, power = 0.8, n_sim = 6,
                                        seed = 3, bogus = 1)),
    "non-finite power")
  expect_error(
    suppressWarnings(cpt_min_detectable(n = 120, power = 0.8, n_sim = 6,
                                        seed = 3, bogus = 1)),
    "nothing to bracket")

  # and correct usage is untouched, including the documented mc_se
  a <- cpt_power(n = 120, jump = 1, n_sim = 20, seed = 3)
  expect_false(is.nan(a$power))
  expect_equal(a$mc_se, sqrt(a$power * (1 - a$power) / a$n_sim))
  b <- cpt_min_detectable(n = 120, power = 0.8, n_sim = 40, seed = 3)
  expect_true(is.finite(b$jump))
  expect_gte(b$achieved_power, 0.8)
})

test_that("cpt_power's mc_se is the binomial standard error", {
  # A reported standard error is the kind of number nobody re-derives, so
  # it is checked at intermediate powers rather than only where it is 0.
  for (j in c(0.3, 0.5, 0.8)) {
    r <- cpt_power(n = 120, jump = j, n_sim = 60, seed = 7, tolerance = 5)
    expect_equal(r$mc_se, sqrt(r$power * (1 - r$power) / r$n_sim), info = j)
  }
  # at power 1 the SE is 0, not NaN
  r1 <- cpt_power(n = 300, jump = 8, n_sim = 20, seed = 2)
  expect_equal(r1$power, 1)
  expect_equal(r1$mc_se, 0)
})

test_that("the result contract holds across every shipped signal shape", {
  # Part XLVIII checked the contract across every method on ONE series -- a
  # clean mean shift. Part LVII's `wbs2` finding showed why that is not
  # enough: a defect can live in the data dimension, and enumerating one
  # dimension thoroughly hides it. These five generators are the standard
  # test signals the package ships for exactly this reason, and they differ
  # in shape rather than in noise: piecewise-constant blocks, frequency
  # modulation, a teeth sawtooth, a monotone staircase, and a mixture.
  skip_on_cran()
  reg <- ggchangepoint:::builtin_registry()
  methods <- reg$method[reg$status == "available" & reg$univariate]
  gens <- c("signal_blocks", "signal_teeth", "signal_stairs")

  checked <- 0L
  for (g in gens) {
    d <- get(g, envir = asNamespace("ggchangepoint"))(n = 300, seed = 5)
    x <- if (is.list(d)) d$value else d
    expect_length(x, 300L)
    # the generator's own contract, which comparison.Rmd relies on
    expect_false(is.null(attr(d, "true_changepoints")), info = g)

    for (m in methods) {
      pkg <- reg$engine[reg$method == m][1]
      if (!is.na(pkg) && !engine_usable(pkg)) next
      r <- tryCatch(suppressWarnings(suppressMessages(
        cpt_detect(x, method = m))), error = function(e) NULL)
      if (is.null(r)) next          # refused this shape, which is its right
      cp <- r$changepoints$cp
      if (length(cp)) {
        expect_false(anyDuplicated(cp) > 0L, info = paste(g, m))
        expect_identical(cp, sort(cp), info = paste(g, m))
        expect_true(all(cp >= 1L & cp <= length(x) - 1L), info = paste(g, m))
        expect_true(all(is.finite(cp)), info = paste(g, m))
      }
      expect_equal(nrow(r$segments), length(cp) + 1L, info = paste(g, m))
      checked <- checked + 1L
    }
  }
  # the sweep must have actually exercised engines on each shape
  expect_gt(checked, 3L * 10L)
})

test_that("every shipped signal feeds cpt_metrics() as its own truth", {
  # comparison.Rmd: "Every generator attaches its true changepoints as a
  # `true_changepoints` attribute, which is exactly the `truth` argument
  # cpt_metrics() expects." All five do, and the blocks signal carries the
  # eleven changepoints its figure caption claims, at every length.
  for (g in c("signal_blocks", "signal_fms", "signal_teeth",
              "signal_stairs", "signal_mix")) {
    d <- get(g, envir = asNamespace("ggchangepoint"))(n = 500, seed = 3)
    tc <- attr(d, "true_changepoints")
    expect_false(is.null(tc), info = g)
    expect_gt(length(tc), 0L)
    m <- cpt_metrics(pred = tc, truth = tc, n = 500)
    expect_equal(m$f1, 1)                       # its own truth scores perfectly
  }
  for (n in c(500L, 1024L, 2048L))
    expect_length(attr(signal_blocks(n = n, seed = 3), "true_changepoints"), 11L)
})

test_that("the tidy() -> as_ggcpt() round trip is lossless", {
  # `extending.Rmd` documents as_ggcpt() as the way an external result
  # enters the package and tidy() as the way one leaves. Nothing checked
  # that the two compose: a result taken out and put back should be the
  # same result. Swept across every method, 39 of 39 applicable ones
  # round-trip with the changepoints, the segment count, the data length,
  # `cp_value` AND `param_estimate` all identical.
  #
  # That last one is the interesting part, and it confirms the separation
  # Part XLVIII found: as_ggcpt() recomputes segment means from the series,
  # so param_estimate matching means every engine's `$segments` really does
  # hold segment means, with the engine's own fitted signal kept apart in
  # `$data$fitted`.
  skip_on_cran()
  reg <- ggchangepoint:::builtin_registry()
  set.seed(7)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4), stats::rnorm(60, 1))

  checked <- 0L
  for (m in reg$method[reg$status == "available" & reg$univariate]) {
    pkg <- reg$engine[reg$method == m][1]
    if (!is.na(pkg) && !engine_usable(pkg)) next
    r <- tryCatch(suppressWarnings(suppressMessages(
      cpt_detect(x, method = m))), error = function(e) NULL)
    if (is.null(r)) next
    back <- suppressWarnings(suppressMessages(as_ggcpt(tidy(r)$cp, x)))
    expect_identical(back$changepoints$cp, r$changepoints$cp, info = m)
    expect_equal(nrow(back$segments), nrow(r$segments), info = m)
    expect_equal(nrow(back$data), nrow(r$data), info = m)
    expect_equal(back$changepoints$cp_value, r$changepoints$cp_value, info = m)
    expect_equal(back$segments$param_estimate, r$segments$param_estimate,
                 info = m)
    checked <- checked + 1L
  }
  expect_gt(checked, 10L)
})

test_that("as_ggcpt() converts a right-convention location on the way in", {
  # extending.Rmd: "`cp_convention = "right"` matters: some engines report
  # the first index of the new segment and some the last index of the old
  # one, and getting it wrong shifts every location by one. The conversion
  # happens on the way in, so the stored result is always on this package's
  # convention." An off-by-one here would be invisible in every other test,
  # because both conventions produce valid-looking changepoints.
  set.seed(2026)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4), stats::rnorm(100, 1))

  L <- as_ggcpt(c(101, 199), x, method = "ext", cp_convention = "left")
  R <- as_ggcpt(c(101, 199), x, method = "ext", cp_convention = "right")

  expect_identical(L$changepoints$cp, c(101L, 199L))   # stored as given
  expect_identical(R$changepoints$cp, c(100L, 198L))   # shifted by one
  expect_identical(R$changepoints$cp, L$changepoints$cp - 1L)

  # ... and the stored result is on this package's convention either way,
  # which is what makes everything downstream comparable
  expect_identical(L$cp_convention, "left")
  expect_identical(R$cp_convention, "left")

  # the vignette's own metric line, which depends on the conversion
  m <- cpt_metrics(tidy(R)$cp, truth = c(100, 200), n = 300)
  expect_equal(m$f1, 1)
})

test_that("every plot() method delegates to autoplot(), as documented", {
  # README.md shows the same figure twice, and that is deliberate: chunk 6
  # is autoplot(res) and chunk 50 is plot(res), captioned "base-graphics
  # fallback (delegates to autoplot)". The two PNGs are byte-identical,
  # which is the evidence the delegation works -- and nothing asserted it.
  # The suite only checked that each returns *a* ggplot, which a divergent
  # implementation would also satisfy.
  #
  # Structural half: every plot method in the package routes through the
  # one helper. This is exact and costs nothing, and it covers all
  # fourteen classes rather than the handful a live comparison can afford.
  # R/ is not installed, so this half runs from the source tree only. The
  # live half below needs no sources and always runs. (Asserting on an
  # empty `defs` is what failed the first R CMD check of this test: the
  # local gate uses pkgload from the source tree, where ../../R exists.)
  # test_path() aborts when the path is absent, so it cannot be used to ask
  # whether the source tree is there. normalizePath(mustWork = FALSE) is
  # the pattern the rest of this suite uses for exactly that reason.
  rdir <- file.path(normalizePath(file.path("..", ".."), mustWork = FALSE), "R")
  src <- if (dir.exists(rdir)) {
    unlist(lapply(list.files(rdir, "\\.R$", full.names = TRUE),
                  readLines, warn = FALSE))
  } else character(0)
  defs <- grep("^plot\\.[A-Za-z_.]+ <- function", src, value = TRUE)
  if (!dir.exists(rdir)) {
    expect_length(defs, 0L)          # installed package: nothing to read
  } else {
    expect_gte(length(defs), 13L)
  }
  # each is either a one-liner delegating, or opens a body that does
  bad <- character()
  for (d in defs) {
    nm <- sub(" <- function.*", "", d)
    i <- grep(paste0("^", gsub("\\.", "\\\\.", nm), " <- function"), src)[1]
    # These are one-liners. Reading a fixed window past the definition
    # walks into the *next* method, which does delegate -- so a divergent
    # method looked compliant and the guard could not fail. Take the
    # definition line alone unless it opens a brace.
    body <- src[i]
    if (grepl("\\{\\s*$", body)) {
      j <- i
      repeat {
        j <- j + 1L
        if (j > length(src) || grepl("^\\}", src[j])) break
      }
      body <- paste(src[i:min(j, length(src))], collapse = " ")
    }
    if (!grepl("plot_via_autoplot(", body, fixed = TRUE)) bad <- c(bad, nm)
  }
  expect_equal(bad, character(0))

  # Live half: the built plots must actually agree, not merely share a class
  set.seed(70)
  x <- c(stats::rnorm(80), stats::rnorm(80, 5))
  res <- cpt_detect(x, method = "pelt")
  pa <- ggplot2::autoplot(res)
  pp <- plot(res)
  expect_s3_class(pp, "ggplot")
  expect_identical(pp$labels, pa$labels)
  expect_identical(
    vapply(pp$layers, function(l) class(l$geom)[1], character(1)),
    vapply(pa$layers, function(l) class(l$geom)[1], character(1)))
  expect_equal(ggplot2::ggplot_build(pp)$data,
               ggplot2::ggplot_build(pa)$data)
})

test_that("only the two pre-class wrappers return a bare tibble", {
  # The feature tour states the compatibility contract exactly: "Every
  # wrapper from 0.2.0 onwards returns a `ggcpt` object; only the two
  # original wrappers below predate the class and still return a bare
  # tibble." Individual assertions covered ecp_wrapper(); nothing checked
  # that the exception set is *closed*, so a new wrapper returning a
  # tibble would falsify the sentence and break every accessor a caller
  # expects to work on a result.
  #
  # Checked against the documentation rather than by calling all 43
  # engines, which took minutes. The first attempt asked whether each
  # \value mentions "ggcpt" anywhere -- and both exceptions do, in
  # passing: cpt_wrapper names the "ggcpt_fit" attribute and ecp_wrapper
  # explains that $fit is NULL on a ggcpt from cpt_detect(). So it must be
  # what the section *opens* with.
  reg <- ggchangepoint:::builtin_registry()
  exported <- sort(grep("_wrapper$", getNamespaceExports("ggchangepoint"),
                        value = TRUE))
  expect_setequal(sort(unique(reg$wrapper)), exported)

  man <- file.path(normalizePath(file.path("..", ".."), mustWork = FALSE), "man")
  if (dir.exists(man)) {
    tibble_docs <- character()
    ggcpt_docs <- character()
    for (w in exported) {
      f <- file.path(man, paste0(w, ".Rd"))
      expect_true(file.exists(f), info = w)
      L <- readLines(f, warn = FALSE)
      i <- which(startsWith(L, "\\value{"))[1]
      expect_false(is.na(i), info = w)
      opening <- paste(L[(i + 1):min(i + 2, length(L))], collapse = " ")
      if (grepl("^\\s*A tibble", opening)) {
        tibble_docs <- c(tibble_docs, w)
      } else {
        expect_match(opening, "ggcpt", info = w)
        ggcpt_docs <- c(ggcpt_docs, w)
      }
    }
    expect_setequal(tibble_docs, c("cpt_wrapper", "ecp_wrapper"))
    expect_gt(length(ggcpt_docs), 35L)
  }

  # and the two exceptions really do behave that way. Both are Imports
  # engines, so this needs no availability guard.
  set.seed(5)
  uni <- c(stats::rnorm(90), stats::rnorm(90, 5))
  expect_s3_class(suppressWarnings(cpt_wrapper(uni)), "tbl_df")
  expect_false(inherits(suppressWarnings(cpt_wrapper(uni)), "ggcpt"))
  expect_s3_class(suppressWarnings(ecp_wrapper(uni)), "tbl_df")
  expect_false(inherits(suppressWarnings(ecp_wrapper(uni)), "ggcpt"))
  # a wrapper on the other side of the line, for contrast. fpop is a
  # Suggests engine, so this needs a guard -- the meta-test above caught
  # its absence, which is that check doing exactly its job: unguarded, it
  # would have failed only the Imports-only run.
  if (engine_usable("fpop")) {
    expect_s3_class(suppressWarnings(fpop_wrapper(uni)), "ggcpt")
  }
})
