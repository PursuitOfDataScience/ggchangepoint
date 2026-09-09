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

# ---------------------------------------------------------------------------
# The `...` collision sweep, extended past the fifty wrappers.
#
# reject_managed_args()/reject_renamed_args() were built for the engine
# wrappers, and the four entry points that are not wrappers were not swept:
# cpt_crops(), cpt_wrapper(), ggcptplot() and cpt_monitor() (which
# cpt_replay() forwards to). Each pins or renames an engine argument and
# each answered R's raw "formal argument \"x\" matched by multiple actual
# arguments", naming neither the function nor what to use instead.
# ---------------------------------------------------------------------------

test_that("the non-wrapper entry points name a colliding `...` argument", {
  set.seed(11)
  uni <- c(stats::rnorm(80), stats::rnorm(80, 4))

  # A collision must never surface as R's own argument-matching error.
  no_raw <- function(expr) {
    msg <- tryCatch({ force(expr); NA_character_ },
                    error = function(e) conditionMessage(e))
    expect_false(is.na(msg))
    expect_false(grepl("matched by multiple actual arguments", msg))
    msg
  }

  # cpt_crops() pins the two arguments that make the call CROPS and the
  # interval it sweeps.
  expect_match(no_raw(cpt_crops(uni, penalty = 10)),
               "`crops` sets `penalty` itself", fixed = TRUE)
  expect_match(no_raw(cpt_crops(uni, method = "BinSeg")),
               "`crops` sets `method` itself", fixed = TRUE)
  expect_match(no_raw(cpt_crops(uni, pen.value = c(1, 2))),
               "use `pen_min` and `pen_max`", fixed = TRUE)

  # cpt_wrapper() and ggcptplot() rename changepoint's `method` to
  # `cp_method`, and each must name itself rather than the other.
  expect_match(no_raw(cpt_wrapper(uni, method = "BinSeg")),
               "`cpt_wrapper` renames its engine's `method`", fixed = TRUE)
  expect_match(no_raw(cpt_wrapper(uni, method = "BinSeg")),
               "Use `cp_method` instead", fixed = TRUE)
  expect_match(no_raw(ggcptplot(uni, method = "BinSeg")),
               "`ggcptplot` renames its engine's `method`", fixed = TRUE)

  # ...and the redirection actually works.
  expect_s3_class(suppressWarnings(cpt_wrapper(uni, cp_method = "BinSeg")),
                  "tbl_df")
})

test_that("a monitor names a colliding engine argument, for both engines", {
  set.seed(12)
  uni <- c(stats::rnorm(80), stats::rnorm(80, 4))
  msg_of <- function(expr) {
    tryCatch({ force(expr); NA_character_ },
             error = function(e) conditionMessage(e))
  }

  if (engine_usable("cpm")) {
    m <- msg_of(cpt_monitor("cpm", baseline = uni[1:60], ARL0 = 500))
    expect_match(m, "Use `arl0` instead", fixed = TRUE)
    m <- msg_of(cpt_monitor("cpm", baseline = uni[1:60], cpmType = "Mood"))
    expect_match(m, "Use `cpm_type` instead", fixed = TRUE)
    # cpt_replay() forwards `...` to cpt_monitor(), so one guard covers it.
    m <- msg_of(cpt_replay(uni, method = "cpm", cpmType = "Mood"))
    expect_match(m, "Use `cpm_type` instead", fixed = TRUE)
  }

  if (engine_usable("ocd")) {
    # An explicit `thresh` keeps this off ocd's Monte Carlo calibration,
    # which costs minutes.
    mv <- cbind(stats::rnorm(90), stats::rnorm(90), stats::rnorm(90))
    m <- msg_of(cpt_monitor("ocd", baseline = mv[1:60, ],
                            thresh = c(10, 10, 10), MC_reps = 5))
    expect_match(m, "Use `mc_reps` instead", fixed = TRUE)
    m <- msg_of(cpt_monitor("ocd", baseline = mv[1:60, ],
                            thresh = c(10, 10, 10), dim = 3))
    expect_match(m, "is not yours to set", fixed = TRUE)
    m <- msg_of(cpt_monitor("ocd", baseline = mv[1:60, ],
                            thresh = c(10, 10, 10), beta = 2))
    expect_match(m, "`ocd` sets `beta` itself", fixed = TRUE)
  }
})

# ---------------------------------------------------------------------------
# A detection call is not allowed to narrate, and print() headers are not
# allowed to be ragged. Both were true of `bcp` and of four print methods.
# ---------------------------------------------------------------------------

test_that("no engine announces someone else's package loads", {
  skip_if_not(engine_usable("bcp"))
  set.seed(13)
  uni <- c(stats::rnorm(40), stats::rnorm(40, 4))
  # bcp::bcp() calls require(bcp) in its own body, which attaches bcp and
  # grid AND announces both through packageStartupMessage().
  out <- utils::capture.output(
    inner <- utils::capture.output(res <- bcp_wrapper(uni, seed = 2026),
                                   type = "message"))
  expect_s3_class(res, "ggcpt")
  expect_length(out, 0L)
  expect_length(inner, 0L)
  # and the search path is given back
  expect_false(any(c("package:bcp", "package:grid") %in% search()))
})

test_that("print() headers align their values in one column", {
  set.seed(14)
  uni <- c(stats::rnorm(80), stats::rnorm(80, 4))
  res <- cpt_detect(uni, method = "pelt")

  # A header line is "  Label: value". Every one in a block must put its
  # value in the same column, and none may end in whitespace: cat()'s
  # default separator lands between the value and the newline.
  #
  # Only the header is checked, not the whole output: the tibble printed
  # below it pads its own columns, so `<lgl> ` legitimately ends in a
  # space and is tibble's business rather than this package's.
  check_header <- function(txt, label) {
    blank <- which(!nzchar(txt))
    head_txt <- if (length(blank)) txt[seq_len(blank[1] - 1L)] else txt
    expect_false(any(grepl("[ \t]+$", head_txt)),
                 info = paste(label, "trailing"))
    fields <- grep("^  [A-Z][^:]*: ", head_txt, value = TRUE)
    expect_gt(length(fields), 1L)
    starts <- vapply(fields, function(l) {
      m <- regexpr("^  [^:]*:[ ]*", l)
      attr(m, "match.length")
    }, integer(1))
    expect_length(unique(starts), 1L)
  }

  check_header(utils::capture.output(print(res)), "print.ggcpt")
  check_header(utils::capture.output(print(summary(res))), "summary.ggcpt")
  # the two views of one object use the same column
  a <- utils::capture.output(print(res))
  b <- utils::capture.output(print(summary(res)))
  col_of <- function(txt) {
    l <- grep("^  Method: ", txt, value = TRUE)[1]
    attr(regexpr("^  [^:]*:[ ]*", l), "match.length")
  }
  expect_equal(col_of(a), col_of(b))

  if (engine_usable("cpm")) {
    mon <- cpt_update(cpt_monitor("edetector", baseline = stats::rnorm(100)),
                      c(stats::rnorm(50), stats::rnorm(50, 3)))
    check_header(utils::capture.output(print(cpt_delay(mon, truth = 100))),
                 "print.ggcpt_delay")
  }
  check_header(utils::capture.output(print(cpt_crops(uni))),
               "print.ggcpt_path")
})

# ---------------------------------------------------------------------------
# `scale_space` is not the same kind of capability column as `statistic` and
# `path`, and three help pages had said it was.
#
# `statistic` and `path` gate their accessors: nothing stored, nothing
# drawn. A scale space is stored by no engine at all -- cpt_scale_space()
# computes one by sweeping a multiscale detector over the series -- so it
# works on any result, and the column marks the two engines the sweep can
# be run *with* rather than the results it accepts.
# ---------------------------------------------------------------------------

test_that("scale_space works on a result whose method does not claim it", {
  skip_if_not(engine_usable("mosum"))
  set.seed(15)
  x <- c(stats::rnorm(150), stats::rnorm(150, 3))
  res <- cpt_detect(x, method = "pelt")

  caps <- cpt_methods()
  expect_false(isTRUE(caps$scale_space[caps$method == "pelt"]))
  # ...and yet:
  ss <- cpt_scale_space(res, bandwidths = c(20, 40))
  expect_s3_class(ss, "tbl_df")
  expect_gt(nrow(ss), 0L)
  expect_true(all(c("index", "bandwidth", "statistic", "threshold",
                    "significant", "detected") %in% names(ss)))
  expect_setequal(unique(ss$bandwidth), c(20, 40))
  expect_s3_class(ggplot2::autoplot(res, type = "scale_space"), "ggplot")

  # Nothing writes a scale space onto a result, which is why the above holds.
  expect_null(res$diagnostics$scale_space)

  # The column's real meaning: the domain of cpt_scale_space(method = ).
  expect_setequal(caps$method[which(caps$scale_space)], c("mosum", "npmojo"))

  # The two columns that DO gate their accessors still gate them, and name
  # the engines that work.
  expect_error(cpt_statistic(res), "does not expose a per-location statistic")
  expect_error(cpt_solution_path(res), "does not expose a solution path")
})

# ---------------------------------------------------------------------------
# `cpt_metrics()` returns twelve numbers and the help page used to list them
# as twelve names. Three are ratios with a zero denominator when one side is
# empty, and the row does not resolve them all the same way -- so the
# conventions are now documented, and pinned here.
#
# The numbers themselves were verified against brute-force reference
# implementations (covering by explicit set intersection over every segment
# pair, ARI from a contingency table, Hausdorff by double minimax) on nine
# hand-built cases and twenty random 400-point ones; they agreed exactly.
# What that sweep could not check is the *documented* meaning, which is what
# this test is for.
# ---------------------------------------------------------------------------

test_that("cpt_metrics() resolves the degenerate cases as documented", {
  # Both empty: the segmentation is exactly right.
  m <- cpt_metrics(integer(0), integer(0), n = 100)
  expect_equal(m$precision, 1); expect_equal(m$recall, 1); expect_equal(m$f1, 1)
  expect_equal(m$covering, 1); expect_equal(m$rand_index, 1)
  expect_equal(m$annotation_error, 0)
  # ...but the distances have no pair to measure.
  expect_true(is.na(m$hausdorff))
  expect_true(is.na(m$mae_matched)); expect_true(is.na(m$rmse_matched))

  # One side empty: 0, not NA, on both ratios.
  m <- cpt_metrics(integer(0), c(30, 70), n = 100)
  expect_equal(m$precision, 0); expect_equal(m$recall, 0); expect_equal(m$f1, 0)
  expect_equal(m$rand_index, 0)
  expect_equal(m$annotation_error, 2)
  expect_true(is.na(m$hausdorff))

  m <- cpt_metrics(c(50), integer(0), n = 100)
  expect_equal(m$precision, 0); expect_equal(m$recall, 0); expect_equal(m$f1, 0)
  expect_equal(m$annotation_error, 1)

  # The mixed-convention row the docs now warn about: nothing matched, so
  # f1 is 0 while mae_matched is NA -- and annotation_error is a perfect 0
  # for a segmentation that got the location off by 85 observations.
  m <- cpt_metrics(5, 90, n = 100, margin = 5)
  expect_equal(m$f1, 0)
  expect_true(is.na(m$mae_matched))
  expect_equal(m$annotation_error, 0)
  expect_equal(m$hausdorff, 85)
  expect_lt(m$rand_index, 0)          # worse than chance

  # An out-of-range index is dropped with a warning, not silently kept: it
  # would corrupt the partition metrics rather than merely miss.
  expect_warning(mm <- cpt_metrics(c(50, 100), c(50), n = 100),
                 "outside 1..(n-1)", fixed = TRUE)
  expect_equal(mm$n_pred, 1L)
})

test_that("the covering metric matches an explicit set computation", {
  # calc_covering() reaches its overlapping prediction segments with two
  # findInterval() lookups rather than scanning them all. That is a
  # performance change to a formula, so the formula is checked against the
  # definition: sum over true segments of |A| * max Jaccard, over n.
  ref <- function(pred, truth, n) {
    seg <- function(cp) {
      b <- sort(unique(c(0, cp, n)))
      lapply(seq_len(length(b) - 1), function(i) (b[i] + 1):b[i + 1])
    }
    A <- seg(truth); B <- seg(pred)
    sum(vapply(A, function(a) {
      length(a) * max(vapply(B, function(b) {
        length(intersect(a, b)) / length(union(a, b))
      }, numeric(1)))
    }, numeric(1))) / n
  }
  expect_equal(cpt_metrics(12, 10, n = 20)$covering, ref(12, 10, 20))
  expect_equal(cpt_metrics(integer(0), c(30, 70), n = 100)$covering,
               ref(integer(0), c(30, 70), 100))
  set.seed(16)
  for (i in 1:5) {
    n <- 300
    truth <- sort(sample(2:(n - 2), 8)); pred <- sort(sample(2:(n - 2), 11))
    expect_equal(cpt_metrics(pred, truth, n = n)$covering,
                 ref(pred, truth, n), tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# `cpt_confint(method = "auto")` could quietly answer at a different level
# than the one asked for.
#
# An `nsp` result carries regions, so native_bounds() finds them and "auto"
# resolves to "native" -- which reports the level the engine already used
# (the `nsp` wrapper's own `alpha = 0.1`, i.e. 0.9). `?cpt_confint` said
# "native" ignores `level`, and that is true; the caller who is surprised
# asked for "auto" and was never told where it landed. The `level` column
# was honest throughout, but only if you inspected it.
# ---------------------------------------------------------------------------

test_that("cpt_confint() warns when the level it reports is not the one asked", {
  skip_if_not(engine_usable("nsp"))
  set.seed(17)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  r <- suppressWarnings(cpt_detect(x, method = "nsp"))
  skip_if(nrow(r$changepoints) == 0)

  # explicit level, route cannot honour it -> warn, and say which route
  expect_warning(ci <- cpt_confint(r, method = "auto", level = 0.95),
                 "`level = 0.95` was not applied", fixed = TRUE)
  expect_warning(cpt_confint(r, method = "auto", level = 0.95),
                 "method = \"native\"", fixed = TRUE)
  # the column was always honest; that is not what changed
  expect_equal(unique(ci$level), 0.9)

  # the default level never warns, whatever the route reports
  expect_silent(suppressMessages(cpt_confint(r, method = "auto")))

  # a route that computes the interval honours the request, silently
  expect_silent(
    ci2 <- suppressMessages(cpt_confint(r, method = "nsp", level = 0.95,
                                        M = 100, seed = 1)))
  expect_equal(unique(ci2$level), 0.95)
})

test_that("cpt_confint() honours `level` on every computing route", {
  set.seed(18)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  r <- cpt_detect(x, method = "pelt")
  for (lv in c(0.80, 0.95, 0.99)) {
    ci <- cpt_confint(r, method = "bootstrap", level = lv, B = 30, seed = 1)
    expect_equal(unique(ci$level), lv)
  }
  # ...and the invariants hold whatever the level: the interval brackets the
  # changepoint and stays inside the series.
  n <- nrow(r$data)
  ci <- cpt_confint(r, method = "bootstrap", level = 0.95, B = 30, seed = 1)
  expect_true(all(ci$ci_lower <= ci$cp)); expect_true(all(ci$cp <= ci$ci_upper))
  expect_true(all(ci$ci_lower >= 1L)); expect_true(all(ci$ci_upper <= n - 1L))
  expect_true(all(nzchar(as.character(ci$source))))
  expect_equal(nrow(ci), nrow(r$changepoints))
})

# ---------------------------------------------------------------------------
# `seed` is scoped to the call, so it cannot pin the caller's stream.
#
# A bare `set.seed(seed)` in a function's own frame does not merely consume
# the caller's random stream, it RESETS it. Measured before the fix, with
# the data generated outside the call so generation cannot be mistaken for
# the effect: a six-iteration generate-then-detect loop analysed
# **2 distinct datasets of 6** when a seed was passed, and 6 of 6 when it
# was not. Five entry points behaved that way -- `cpt_detect()` with a
# stochastic engine, `cpt_stability()`, `cpt_select(criterion = "cv")`,
# `cpt_simulate()` and `cpt_power()` -- which is to say exactly the
# functions a user calls inside a simulation loop. Nothing warned and no
# test failed; the study silently had a sample size of one.
#
# These are metamorphic assertions: they compare calls to each other rather
# than to an expected value, which is the only kind that could have caught
# this.
# ---------------------------------------------------------------------------

test_that("a seeded call does not pin the caller's random stream", {
  distinct_datasets <- function(run) {
    set.seed(2026)
    firsts <- numeric(6)
    for (i in 1:6) {
      d <- c(stats::rnorm(60), stats::rnorm(60, 3))
      firsts[i] <- d[1]
      invisible(run(d))
    }
    length(unique(round(firsts, 9)))
  }

  # The reference: no seed at all must give six different datasets. If this
  # ever fails the test below proves nothing.
  expect_equal(distinct_datasets(function(d) cpt_detect(d, method = "pelt")), 6L)

  # ...and neither does passing one.
  expect_equal(
    distinct_datasets(function(d) cpt_stability(d, method = "pelt", B = 3,
                                                seed = 7)), 6L)
  expect_equal(
    distinct_datasets(function(d) cpt_simulate(40, changepoints = 20,
                                               seed = 3)), 6L)
  expect_equal(
    distinct_datasets(function(d) {
      suppressWarnings(cpt_select(d, method = "pelt", criterion = "cv",
                                  seed = 5))
    }), 6L)
  if (engine_usable("wbs")) {
    expect_equal(
      distinct_datasets(function(d) {
        suppressWarnings(cpt_detect(d, method = "wbs", seed = 1))
      }), 6L)
  }
})

test_that("a seeded call leaves .Random.seed exactly as it found it", {
  set.seed(11)
  x <- c(stats::rnorm(120), stats::rnorm(120, 3))
  preserved <- function(run) {
    set.seed(99); before <- get(".Random.seed", envir = globalenv())
    invisible(run())
    identical(get(".Random.seed", envir = globalenv()), before)
  }
  expect_true(preserved(function() cpt_simulate(40, changepoints = 20, seed = 3)))
  expect_true(preserved(function() cpt_stability(x, method = "pelt", B = 3, seed = 7)))
  expect_true(preserved(function() {
    cpt_power(n = 50, jump = 2, n_sim = 2, parallel = FALSE, seed = 4)
  }))
  if (engine_usable("wbs")) {
    expect_true(preserved(function() {
      suppressWarnings(cpt_detect(x, method = "wbs", seed = 42))
    }))
  }
  if (engine_usable("nsp")) {
    expect_true(preserved(function() {
      suppressWarnings(cpt_detect(x, method = "nsp", seed = 42, M = 50))
    }))
  }

  # A fresh session has no `.Random.seed` until the first draw, and leaving
  # one behind would itself be a change to the caller's state.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(cpt_simulate(40, changepoints = 20, seed = 3))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("scoping the seed did not cost reproducibility", {
  set.seed(12)
  x <- c(stats::rnorm(150), stats::rnorm(150, 3))

  # Same seed twice, with a thousand draws in between: identical answers.
  same <- function(run) {
    a <- run()
    invisible(stats::rnorm(1000))
    b <- run()
    identical(a, b)
  }
  expect_true(same(function() cpt_simulate(60, changepoints = 30, seed = 3)$value))
  expect_true(same(function() {
    # `$frequency` is the re-detection table -- the part that is random.
    cpt_stability(x, method = "pelt", B = 5, seed = 7)$frequency
  }))
  if (engine_usable("wbs")) {
    expect_true(same(function() {
      suppressWarnings(cpt_detect(x, method = "wbs", seed = 42))$changepoints$cp
    }))
  }

  # And a seeded call still reproduces after an unseeded one has moved the
  # stream on, which is what "reproducible" has to mean.
  if (engine_usable("wbs")) {
    a <- suppressWarnings(cpt_detect(x, method = "wbs", seed = 42))$changepoints$cp
    invisible(suppressWarnings(cpt_detect(x, method = "wbs")))
    b <- suppressWarnings(cpt_detect(x, method = "wbs", seed = 42))$changepoints$cp
    expect_identical(a, b)
  }
})

# ---------------------------------------------------------------------------
# Reproducibility under a `future::plan()`.
#
# Seven exported functions dispatch on the plan. §180.2 of the development
# ledger guessed that `cpt_power()` and `cpt_stability()` might disagree
# between the sequential and parallel paths even with a seed fixed, and
# asked for exactly this test. Measuring corrects the guess twice:
# `cpt_stability()` has no parallel path at all, and of the seven that do,
# only `cpt_power()` diverges -- because it is the only one whose farmed-out
# tasks consume random numbers. The other six farm out work that is
# deterministic given its input, so the worker stream never enters the
# answer.
#
# The guarantee worth pinning is therefore "same seed and same plan, same
# answer", for both plans. Cross-plan equality is deliberately NOT asserted:
# under a parallel plan the numbers come from future.apply's L'Ecuyer
# streams and sequentially from the calling stream, and both being
# deterministic is the property that matters.
# ---------------------------------------------------------------------------

test_that("a seeded call reproduces under whichever future plan is set", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  set.seed(19)
  x <- c(stats::rnorm(120), stats::rnorm(120, 3))

  runs <- list(
    cpt_power = function() {
      cpt_power(n = 100, jump = c(0.5, 1.5), n_sim = 6, seed = 11)$power
    },
    cpt_consensus = function() {
      cpt_consensus(x, methods = c("pelt", "binseg", "amoc"),
                    seed = 3)$consensus
    },
    cpt_batch = function() {
      cpt_batch(list(a = x, b = rev(x)), method = "pelt", seed = 3)$changepoints
    },
    cpt_sensitivity = function() {
      cpt_sensitivity(x, method = "pelt", over = list(penalty = c(4, 10)),
                      seed = 3)$grid
    }
  )

  twice <- function(run) {
    a <- run()
    b <- run()
    identical(a, b)
  }

  # (1) the default plan, which is sequential
  expect_true(inherits(future::plan(), "sequential"))
  for (nm in names(runs)) expect_true(twice(runs[[nm]]), info = nm)

  # (2) and a real two-worker plan. Restored on exit however this exits, so
  # a failure here cannot leave a parallel plan set for the rest of the
  # suite.
  old <- future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(old))
  expect_false(inherits(future::plan(), "sequential"))
  for (nm in names(runs)) expect_true(twice(runs[[nm]]), info = nm)
})

test_that("only cpt_power()'s answer depends on the plan", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  set.seed(20)
  x <- c(stats::rnorm(120), stats::rnorm(120, 3))

  seq_of <- function(run) {
    stopifnot(inherits(future::plan(), "sequential"))
    run()
  }
  par_of <- function(run) {
    old <- future::plan(future::multisession, workers = 2)
    on.exit(future::plan(old), add = TRUE)
    run()
  }

  # Deterministic tasks: the plan must not enter the answer, and this is
  # the claim `?cpt_power`'s section makes about the other six.
  det <- list(
    cpt_consensus = function() {
      cpt_consensus(x, methods = c("pelt", "binseg", "amoc"), seed = 3)$consensus
    },
    cpt_batch = function() {
      cpt_batch(list(a = x, b = rev(x)), method = "pelt", seed = 3)$changepoints
    },
    cpt_sensitivity = function() {
      cpt_sensitivity(x, method = "pelt", over = list(penalty = c(4, 10)),
                      seed = 3)$grid
    }
  )
  for (nm in names(det)) {
    expect_equal(seq_of(det[[nm]]), par_of(det[[nm]]), info = nm)
  }

  # cpt_power() is the exception, and its `mc_se` column is what says the
  # two are estimates of the same quantity rather than a disagreement.
  pw <- function() cpt_power(n = 100, jump = 1, n_sim = 6, seed = 11)
  a <- seq_of(pw)
  b <- par_of(pw)
  expect_equal(nrow(a), nrow(b))
  expect_equal(a$n, b$n)
  expect_equal(a$jump, b$jump)
  expect_true(all(is.finite(a$power)) && all(is.finite(b$power)))
  expect_true(all(a$power >= 0 & a$power <= 1))
  expect_true(all(b$power >= 0 & b$power <= 1))
})

# ---------------------------------------------------------------------------
# `as_ggcpt()` drops a `cp` it cannot use -- documented -- but it used to do
# it in silence.
#
# `ggcpt_build()` discards an index that is NA or outside 1..(n-1), and for a
# wrapper that is correct: the indices come from an engine, some of which
# legitimately emit a boundary value, and normalising a machine's output is
# the wrapper's job. `as_ggcpt()` is given the caller's own values -- a
# published paper's breaks, an analyst's annotations -- and there one
# mistyped index vanishing without a word leaves a result that looks
# complete and is short a changepoint. Measured on a 200-point series
# before the fix: `as_ggcpt(c(50, 500), x)` returned one changepoint at 50.
#
# The dropping is kept, because it is documented and refusing would break
# working code. Only the silence is gone.
# ---------------------------------------------------------------------------

test_that("as_ggcpt() says what it dropped from `cp`", {
  set.seed(21)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  n <- length(x)

  # Each of the five, with the value named in the message.
  expect_warning(a <- as_ggcpt(c(50, 500), x), "outside 1..199", fixed = TRUE)
  expect_warning(as_ggcpt(c(50, 500), x), "(500)", fixed = TRUE)
  expect_equal(a$changepoints$cp, 50L)          # behaviour unchanged

  expect_warning(as_ggcpt(c(0, 50), x), "outside 1..199", fixed = TRUE)
  expect_warning(as_ggcpt(c(50, n), x), "(200)", fixed = TRUE)
  expect_warning(b <- as_ggcpt(50.5, x), "truncated to whole numbers",
                 fixed = TRUE)
  expect_equal(b$changepoints$cp, 50L)
  expect_warning(as_ggcpt(c(50, 50), x), "duplicated (50)", fixed = TRUE)
  expect_warning(as_ggcpt(c(50, NA), x), "1 missing", fixed = TRUE)

  # The message explains the convention, because that is what makes `n`
  # out of range and 1 valid under "left".
  expect_warning(as_ggcpt(c(50, n), x),
                 "last index of the segment before it", fixed = TRUE)
  expect_warning(as_ggcpt(1, x, cp_convention = "right"),
                 "first index of the segment after it", fixed = TRUE)
  expect_warning(as_ggcpt(1, x, cp_convention = "right"), "outside 2..200",
                 fixed = TRUE)
})

test_that("as_ggcpt() stays silent when nothing is lost", {
  set.seed(22)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  n <- length(x)

  # Valid input, both convention boundaries, and reordering -- which loses
  # nothing and so must not warn.
  expect_silent(as_ggcpt(c(50, 150), x))
  expect_silent(as_ggcpt(1, x))
  expect_silent(as_ggcpt(n - 1L, x))
  expect_silent(as_ggcpt(2, x, cp_convention = "right"))
  expect_silent(as_ggcpt(n, x, cp_convention = "right"))
  expect_silent(as_ggcpt(integer(0), x))
  expect_silent(as_ggcpt(NULL, x))
  # an integer-valued double is not a truncation
  expect_silent(as_ggcpt(c(50, 150.0), x))
  # ...and sorting happens, silently
  expect_silent(s <- as_ggcpt(c(150, 50), x))
  expect_equal(s$changepoints$cp, c(50L, 150L))

  # The optional slots still work alongside the guard.
  expect_silent(as_ggcpt(c(50, 150), x, ci = cbind(c(45, 145), c(55, 155))))
  expect_silent(as_ggcpt(50, x, fitted = rep(0, n)))
  expect_silent(as_ggcpt(50, cbind(x, rev(x))))
})

test_that("a registered method's own indices are normalised without a report", {
  # `cpt_detect()` routes a registered method's bare-vector return through
  # as_ggcpt(), so the new drop report reached a caller it was not written
  # for: the indices came from the detector, not from a person transcribing
  # published breaks, and "check the values against the series" is advice
  # about code the user did not write. The report is a classed condition
  # (`ggchangepoint_cp_dropped`) and only that one caller muffles it.
  set.seed(23)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4))
  withr::defer(for (nm in c("dropprobe")) {
    try(cpt_unregister_method(nm), silent = TRUE)
  })

  for (fn in list(function(x, ...) c(0L, 80L, 500L),   # both boundaries
                  function(x, ...) 80.4,               # fractional
                  function(x, ...) c(80L, 80L))) {     # duplicated
    cpt_register_method("dropprobe", fn = fn, overwrite = TRUE)
    expect_silent(fit <- cpt_detect(x, method = "dropprobe"))
    expect_equal(fit$changepoints$cp, 80L)
    expect_true(isTRUE(fit$registered))
  }

  # The same values through the user-facing door DO report, which is the
  # asymmetry the whole thing rests on.
  expect_warning(as_ggcpt(c(0L, 80L, 500L), x), "outside 1..159", fixed = TRUE)

  # And the condition is classed, so a caller can muffle exactly this and
  # nothing else.
  cnd <- tryCatch(as_ggcpt(c(80L, 500L), x),
                  warning = function(w) w)
  expect_s3_class(cnd, "ggchangepoint_cp_dropped")
  expect_s3_class(cnd, "warning")
})

# ---------------------------------------------------------------------------
# `cpt_simulate()` is the package's own ground truth -- `cpt_power()`,
# `cpt_benchmark()` and `cpt_datasets()` are all scored against what it says
# it generated -- so a `params` that does not describe the series it built
# corrupts every number downstream of it.
#
# Supplying too FEW entries already warned: the last one is recycled, so the
# trailing changepoints would be recorded as ground truth with no change
# behind them. Too MANY was silent. `k` changepoints make `k + 1` segments,
# which is the arithmetic easiest to get wrong, and a caller who wrote three
# segment means against one changepoint got an ordinary two-segment series
# with the third value dropped and nothing said.
# ---------------------------------------------------------------------------

test_that("cpt_simulate() reports a params/segment mismatch either way", {
  # too few: recycled, and it says so (unchanged)
  expect_warning(cpt_simulate(200, changepoints = 100, params = c(0), seed = 1),
                 "1 value(s) for 2 segments", fixed = TRUE)
  # too many: the surplus is dropped, and now it says so
  expect_warning(cpt_simulate(200, changepoints = 100, params = c(0, 3, 9),
                              seed = 1),
                 "3 value(s) but 1 changepoint(s) make only 2 segment(s)",
                 fixed = TRUE)
  expect_warning(cpt_simulate(200, changepoints = 100, params = c(0, 3, 9),
                              seed = 1),
                 "`changepoints` sets the number of segments", fixed = TRUE)
  # ...for every change type, not just "mean"
  expect_warning(cpt_simulate(200, changepoints = 100, change_in = "var",
                              params = c(1, 4, 9), seed = 1),
                 "the last 1 are unused", fixed = TRUE)
  expect_warning(
    cpt_simulate(200, changepoints = 100, change_in = "meanvar",
                 params = list(list(mean = 0, sd = 1), list(mean = 3, sd = 1),
                               list(mean = 9, sd = 1)), seed = 1),
    "only 2 segment(s)", fixed = TRUE)

  # and silent when the counts agree, or when params is left to the default
  expect_silent(cpt_simulate(200, changepoints = 100, params = c(0, 3), seed = 1))
  expect_silent(cpt_simulate(300, changepoints = c(100, 200),
                             params = c(0, 3, 9), seed = 1))
  expect_silent(cpt_simulate(200, changepoints = 100, seed = 1))
  expect_silent(cpt_simulate(200, changepoints = integer(0), seed = 1))
})

test_that("cpt_simulate() builds the series it says it built", {
  # The quantities, not just the shape: everything downstream is scored
  # against these.
  jump_err <- function(jump) {
    e <- vapply(1:30, function(s) {
      d <- cpt_simulate(400, changepoints = 200, change_in = "mean",
                        params = c(0, jump), sd = 1, seed = s)
      (mean(d$value[201:400]) - mean(d$value[1:200])) - jump
    }, numeric(1))
    abs(mean(e)) / (stats::sd(e) / sqrt(length(e)))       # |bias| in SEs
  }
  for (j in c(0.5, 2, 5)) expect_lt(jump_err(j), 4)

  sd_ratio <- function(r) {
    v <- vapply(1:30, function(s) {
      d <- cpt_simulate(400, changepoints = 200, change_in = "var",
                        params = c(1, r), seed = s)
      stats::sd(d$value[201:400]) / stats::sd(d$value[1:200])
    }, numeric(1))
    mean(v)
  }
  expect_equal(sd_ratio(2), 2, tolerance = 0.1)
  expect_equal(sd_ratio(4), 4, tolerance = 0.1)

  # seg_id must increment exactly at the requested changepoints, because
  # that is the ground-truth column the metrics are matched against.
  for (cps in list(200L, c(100L, 300L), c(50L, 150L, 250L, 350L))) {
    d <- cpt_simulate(400, changepoints = cps,
                      params = seq_len(length(cps) + 1L), seed = 1)
    expect_identical(as.integer(which(diff(d$seg_id) != 0)), as.integer(cps))
  }

  # the noise models are the ones they claim to be
  rho_hat <- function(rho) mean(vapply(1:20, function(s) {
    d <- cpt_simulate(600, changepoints = integer(0), noise = "ar1",
                      rho = rho, seed = s)
    stats::acf(d$value, lag.max = 1, plot = FALSE)$acf[2]
  }, numeric(1)))
  expect_equal(rho_hat(0.3), 0.3, tolerance = 0.08)
  expect_equal(rho_hat(0.7), 0.7, tolerance = 0.08)

  kur <- function(v) mean((v - mean(v))^4) / stats::var(v)^2
  kg <- mean(vapply(1:20, function(s) kur(cpt_simulate(
    500, changepoints = integer(0), noise = "gauss", seed = s)$value), numeric(1)))
  kt <- mean(vapply(1:20, function(s) kur(cpt_simulate(
    500, changepoints = integer(0), noise = "t", df = 3, seed = s)$value), numeric(1)))
  expect_gt(kt, kg * 2)
})

# ---------------------------------------------------------------------------
# `cpt_select()`'s closed-form criteria, against their definitions.
#
# The `value` column is what a user compares rungs by, so it is worth
# pinning to the formula rather than to a recorded number: a test that
# records 152.7985 passes after a formula change that silently alters what
# the criterion means. The reference implementations below are written from
# the definitions and share no code with the package.
#
# Measured: all three agree exactly at every rung, and the documented
# behaviour of "aic" -- that a penalty which does not grow with n
# over-selects -- shows up as it taking the whole ladder while "bic" and
# "mbic" find the truth.
# ---------------------------------------------------------------------------

test_that("bic, aic and mbic are the formulas the help page states", {
  set.seed(24)
  x <- c(stats::rnorm(100), stats::rnorm(100, 3), stats::rnorm(100, 1))

  rss_cost <- function(y, cp) {
    b <- c(0L, sort(unique(cp[cp >= 1 & cp < length(y)])), length(y))
    r <- sum(vapply(seq_len(length(b) - 1), function(i) {
      s <- y[(b[i] + 1):b[i + 1]]
      sum((s - mean(s))^2)
    }, numeric(1)))
    length(y) * log(max(r, .Machine$double.eps) / length(y))
  }
  ref <- list(
    bic = function(y, cp) rss_cost(y, cp) + (2 * length(cp) + 1) * log(length(y)),
    aic = function(y, cp) rss_cost(y, cp) + 2 * (2 * length(cp) + 1),
    mbic = function(y, cp) {
      k <- length(cp)
      pen <- if (k == 0) 0 else {
        l <- diff(c(0L, sort(cp), length(y)))
        3 * k * log(length(y)) + sum(log(l / length(y)))
      }
      rss_cost(y, cp) + pen
    }
  )

  chosen_k <- integer(0)
  for (crit in names(ref)) {
    s <- cpt_select(x, method = "pelt", criterion = crit, k_max = 6)
    tb <- s$criterion_table
    expect_true(all(c("k", "value", "cost", "chosen", "cpts") %in% names(tb)))
    # exactly one row chosen, and it is the argmin of `value`
    expect_equal(sum(tb$chosen), 1L)
    expect_equal(tb$k[tb$chosen], tb$k[which.min(tb$value)])
    expect_equal(s$k, tb$k[tb$chosen])
    # every rung's value and cost equal the definition at that rung's own
    # locations
    for (i in seq_len(nrow(tb))) {
      loc <- as.integer(unlist(tb$cpts[[i]]))
      if (length(loc) != tb$k[i]) next
      expect_equal(tb$value[i], ref[[crit]](x, loc), tolerance = 1e-8,
                   info = paste(crit, "K =", tb$k[i]))
      expect_equal(tb$cost[i], rss_cost(x, loc), tolerance = 1e-8,
                   info = paste(crit, "cost at K =", tb$k[i]))
    }
    chosen_k <- c(chosen_k, tb$k[tb$chosen])
  }
  names(chosen_k) <- names(ref)

  # The help page's claim about aic, as behaviour rather than prose: bic and
  # mbic recover the two real changes, aic takes the whole ladder.
  expect_equal(chosen_k[["bic"]], 2L)
  expect_equal(chosen_k[["mbic"]], 2L)
  expect_gt(chosen_k[["aic"]], chosen_k[["bic"]])
})

# ---------------------------------------------------------------------------
# `cpt_consensus(min_votes =)`: an unreachable threshold, and a boundary that
# falls where a reader would write "unanimous".
#
# The rule is that a value strictly between 0 and 1 is a proportion and
# anything else is a count. With three methods that puts `min_votes = 0.99`
# at all three and `min_votes = 1` at *one* -- the least strict setting
# there is -- so the two neighbouring values mean opposite things. And a
# count above the number of methods that ran cannot be reached, so the
# consensus is empty by construction, which is indistinguishable from the
# methods having agreed on nothing.
#
# The clustering itself was verified separately against hand-computed cases
# and is unchanged.
# ---------------------------------------------------------------------------

test_that("cpt_consensus() warns when min_votes cannot be reached", {
  set.seed(25)
  x <- c(stats::rnorm(120), stats::rnorm(120, 4))
  three <- c("pelt", "binseg", "amoc")

  # reachable: silent, whatever the value, including exact unanimity
  for (mv in c(0.5, 0.99, 1, 2, 3)) {
    expect_silent(r <- cpt_consensus(x, methods = three, min_votes = mv))
    expect_lte(attr(r, "consensus")$threshold, 3L)
  }

  # unreachable: warns, and names both numbers
  expect_warning(cpt_consensus(x, methods = three, min_votes = 4),
                 "threshold of 4 but only 3 method(s) ran", fixed = TRUE)
  expect_warning(cpt_consensus(x, methods = three, min_votes = 4),
                 "empty by construction", fixed = TRUE)
  # ...and the message explains the count/proportion boundary, which is the
  # likeliest reason someone got here
  expect_warning(cpt_consensus(x, methods = three, min_votes = 10),
                 "a count of methods, not a proportion", fixed = TRUE)
  # the count that matters is the methods that RAN, not those requested
  expect_warning(cpt_consensus(x, methods = c("pelt", "binseg"),
                               min_votes = 3),
                 "only 2 method(s) ran", fixed = TRUE)

  # the boundary itself, as behaviour: 0.99 is stricter than 1
  a <- cpt_consensus(x, methods = three, min_votes = 0.99)
  b <- cpt_consensus(x, methods = three, min_votes = 1)
  expect_equal(attr(a, "consensus")$threshold, 3L)
  expect_equal(attr(b, "consensus")$threshold, 1L)
})

test_that("cpt_consensus() clusters by leader, not transitively", {
  cl <- ggchangepoint:::cluster_changepoints
  # A cluster spans at most `tolerance` from its first member, so a chain of
  # near-neighbours does not merge into one wide cluster.
  o <- cl(list(a = 100L, b = 105L, c = 110L), 5)
  expect_equal(as.integer(o$cp), c(102L, 110L))
  expect_equal(as.integer(o$votes), c(2L, 1L))
  expect_equal(as.integer(o$spread), c(5L, 0L))

  # the tolerance boundary is inclusive
  expect_equal(nrow(cl(list(a = 100L, b = 105L), 5)), 1L)
  expect_equal(nrow(cl(list(a = 100L, b = 106L), 5)), 2L)

  # one method contributing two nearby locations is still one vote
  o <- cl(list(a = c(99L, 101L), b = 100L), 5)
  expect_equal(as.integer(o$votes), 2L)
  expect_equal(as.integer(o$cp), 100L)

  # `cp` is the median of the cluster's members, `spread` its range
  o <- cl(list(a = 98L, b = 100L, c = 102L), 5)
  expect_equal(as.integer(o$cp), 100L)
  expect_equal(as.integer(o$spread), 4L)

  # nothing detected is an empty table, not an error
  o <- cl(list(a = integer(0), b = integer(0)), 5)
  expect_equal(nrow(o), 0L)
  expect_true(all(c("cp", "votes", "methods", "spread") %in% names(o)))
})

# ---------------------------------------------------------------------------
# The benchmark's ranking arithmetic and its Nemenyi critical distance.
#
# The critical distance is the number a reader takes "method A beats method
# B" from, so it is checked against Demsar (2006) Table 5 rather than
# against a recorded value: q_alpha = qtukey(1 - alpha, k, Inf) / sqrt(2),
# and CD = q_alpha * sqrt(k(k + 1) / (6N)).
# ---------------------------------------------------------------------------

test_that("the Nemenyi critical distance matches Demsar's published table", {
  cd <- ggchangepoint:::nemenyi_cd
  # Demsar (2006), Table 5: q_0.05 for k = 2..10, to three decimals.
  published <- c(`2` = 1.960, `3` = 2.343, `4` = 2.569, `5` = 2.728,
                 `6` = 2.850, `7` = 2.949, `8` = 3.031, `9` = 3.102,
                 `10` = 3.164)
  for (k in as.integer(names(published))) {
    q <- stats::qtukey(0.95, nmeans = k, df = Inf) / sqrt(2)
    expect_equal(q, published[[as.character(k)]], tolerance = 1e-3,
                 info = paste("k =", k))
  }
  # and CD assembles those constants the documented way
  for (spec in list(c(3, 10), c(5, 20), c(8, 6))) {
    k <- spec[1]; n_d <- spec[2]
    want <- (stats::qtukey(0.95, k, Inf) / sqrt(2)) * sqrt(k * (k + 1) / (6 * n_d))
    expect_equal(cd(k, n_d), want)
  }
  # CD narrows as the number of datasets grows -- the reason the help page
  # says to read a small-N diagram descriptively
  expect_gt(cd(5, 5), cd(5, 50))
})

test_that("benchmark ranks respect the metric's direction and penalise NA", {
  br <- ggchangepoint:::benchmark_ranks
  mk <- function(df, metric) {
    structure(tibble::as_tibble(df), metrics = metric,
              class = c("ggcpt_benchmark", class(tibble::as_tibble(df))))
  }

  # higher-is-better: rank 1 to the largest
  r <- br(mk(data.frame(dataset = "A", method = c("m1", "m2", "m3"),
                        covering = c(0.9, 0.5, 0.7)), "covering"), "covering")
  expect_equal(r$method, c("m1", "m3", "m2"))
  expect_equal(r$mean_rank, c(1, 2, 3))

  # lower-is-better: rank 1 to the smallest
  r <- br(mk(data.frame(dataset = "A", method = c("m1", "m2", "m3"),
                        hausdorff = c(9, 1, 5)), "hausdorff"), "hausdorff")
  expect_equal(r$method, c("m2", "m3", "m1"))

  # NA takes the worst rank, in both directions, rather than being dropped
  r <- br(mk(data.frame(dataset = "A", method = c("m1", "m2", "m3"),
                        covering = c(0.9, NA, 0.7)), "covering"), "covering")
  expect_equal(r$method[3], "m2")
  expect_equal(r$mean_rank[3], 3)
  r <- br(mk(data.frame(dataset = "A", method = c("m1", "m2", "m3"),
                        hausdorff = c(9, NA, 5)), "hausdorff"), "hausdorff")
  expect_equal(r$method[3], "m2")

  # ties share the average rank, and mean_rank averages over datasets:
  # dataset A is a tie (1.5 each), B splits 1 and 2 -> 1.25 and 1.75
  r <- br(mk(data.frame(dataset = c("A", "A", "B", "B"),
                        method = c("m1", "m2", "m1", "m2"),
                        covering = c(0.5, 0.5, 0.9, 0.1)), "covering"),
          "covering")
  got <- stats::setNames(r$mean_rank, r$method)
  expect_equal(got[["m1"]], 1.25)
  expect_equal(got[["m2"]], 1.75)
  expect_equal(unique(r$n_datasets), 2L)

  # an all-NA metric has no ranking to report, and says so with NULL rather
  # than a table of ties
  expect_null(br(mk(data.frame(dataset = "A", method = "m1",
                               covering = NA_real_), "covering"), "covering"))
})

test_that("cpt_stability() frequencies are replicate proportions", {
  set.seed(26)
  x <- c(stats::rnorm(120), stats::rnorm(120, 4))
  B <- 40
  st <- cpt_stability(x, method = "pelt", B = B, margin = 5, seed = 1)
  f <- st$frequency

  expect_equal(nrow(f), length(x))
  expect_identical(f$index, seq_along(x))
  expect_true(all(f$freq >= 0 & f$freq <= 1))
  # Each replicate contributes at most 1 to any index, so freq is a genuine
  # multiple of 1/B. Incrementing once per changepoint instead double-counts
  # a replicate wherever two detections' windows overlap -- the defect the
  # old `pmin(hits / B, 1)` clipped out of sight.
  expect_true(all(abs(f$freq * B - round(f$freq * B)) < 1e-9))
  # and it locates the change rather than the quiet stretches
  expect_gt(max(f$freq[abs(f$index - 120) <= 5]),
            max(f$freq[f$index < 40 | f$index > 200]))
})

# ---------------------------------------------------------------------------
# Three contracts that were correct and untested. Round 4's lesson was that
# checked surfaces stay right and unchecked ones decay, so the tests are the
# deliverable here rather than a fix.
# ---------------------------------------------------------------------------

test_that("cpt_penalty() computes the formulas its help page gives", {
  ref <- list(
    BIC = function(n, k, a) k * log(n),
    SIC = function(n, k, a) k * log(n),
    MBIC = function(n, k, a) 0.5 * (k + 1) * log(n) + lchoose(n, k),
    AIC = function(n, k, a) 2 * k,
    `Hannan-Quinn` = function(n, k, a) 2 * k * log(log(n)),
    sSIC = function(n, k, a) k * log(n)^a
  )
  for (ty in names(ref)) {
    for (n in c(50, 200, 1000, 5000)) {
      for (k in c(0, 1, 3, 10)) {
        expect_equal(cpt_penalty(ty, n = n, k = k, alpha = 1.01),
                     ref[[ty]](n, k, 1.01),
                     info = sprintf("%s n=%d k=%d", ty, n, k))
      }
    }
  }
  expect_equal(as.numeric(cpt_penalty("None", n = 200, k = 3)), 0)

  # the orderings the page asserts: sSIC and MBIC are *strengthened* forms
  # of BIC, and BIC outgrows AIC once n is any size
  for (n in c(50, 200, 1000)) {
    for (k in c(1, 3, 10)) {
      expect_gt(cpt_penalty("sSIC", n = n, k = k, alpha = 1.01),
                cpt_penalty("BIC", n = n, k = k))
      expect_gt(cpt_penalty("MBIC", n = n, k = k),
                cpt_penalty("BIC", n = n, k = k))
      expect_gt(cpt_penalty("BIC", n = n, k = k),
                cpt_penalty("AIC", n = n, k = k))
    }
  }

  # the guards, each refusing by name
  expect_error(cpt_penalty("sSIC", n = 200, k = 3, alpha = 1),
               "greater than 1")
  expect_error(cpt_penalty("sSIC", n = 200, k = 3, alpha = 0.5),
               "greater than 1")
  expect_error(cpt_penalty("MBIC", n = 20, k = 50), "between 0 and `n`")
  expect_error(cpt_penalty("MBIC", n = 200, k = -1), "between 0 and `n`")
  expect_error(cpt_penalty("BIC", n = 2, k = 1), "at least 3")
  # ...and AIC is exempt from the n >= 3 rule, because 2k does not involve n
  expect_equal(cpt_penalty("AIC", n = 2, k = 1), 2)
})

test_that("cpt_min_detectable() brackets the answer it reports", {
  skip_on_cran()
  set.seed(27)
  rng <- c(0.1, 5)
  o <- cpt_min_detectable(n = 150, power = 0.8, n_sim = 12, range = rng,
                          max_iter = 5, seed = 1)
  tr <- o$trace
  expect_true(all(c("jump", "power", "mc_se") %in% names(tr)))
  # the search opens on both endpoints, then adds at most max_iter midpoints
  expect_equal(tr$jump[1], rng[1])
  expect_equal(tr$jump[2], rng[2])
  expect_lte(nrow(tr), 2 + 5)
  expect_true(all(tr$power >= 0 & tr$power <= 1))
  expect_gte(o$jump, rng[1])
  expect_lte(o$jump, rng[2])
  # every midpoint lies strictly inside the interval the earlier
  # evaluations left, which is what makes it a bisection
  lo <- tr$jump[1]; hi <- tr$jump[2]
  for (i in seq_len(nrow(tr))[-(1:2)]) {
    expect_gt(tr$jump[i], lo)
    expect_lt(tr$jump[i], hi)
    if (tr$power[i] >= 0.8) hi <- tr$jump[i] else lo <- tr$jump[i]
  }

  # the two degenerate brackets are reported, not silently returned as a
  # number the search never found
  o2 <- cpt_min_detectable(n = 40, power = 0.999, n_sim = 8,
                           range = c(0.01, 0.05), max_iter = 3, seed = 2)
  expect_true(is.na(o2$jump))
  expect_match(o2$note, "reached only")
  o3 <- cpt_min_detectable(n = 400, power = 0.2, n_sim = 8, range = c(3, 6),
                           max_iter = 3, seed = 3)
  expect_equal(o3$jump, 3)
  expect_match(o3$note, "already reaches the target")
  for (o_i in list(o, o2, o3)) expect_output(print(o_i))
})

test_that("tidy() on an events result is one table with three statuses", {
  set.seed(28)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  res <- cpt_detect(x, method = "pelt")
  cp <- res$changepoints$cp
  skip_if(length(cp) != 1)

  # a pair, and an event nothing explains
  t1 <- tidy(cpt_annotate_events(res,
    data.frame(index = c(cp, cp + 50), label = c("hit", "miss")),
    tolerance = 5))
  expect_setequal(t1$status, c("matched", "undetected_event"))
  expect_equal(t1$cp[t1$status == "matched"], cp)
  expect_true(is.na(t1$cp[t1$status == "undetected_event"]))

  # a changepoint nothing explains keeps a non-missing `cp`, which is why
  # the help page says to filter on `status` rather than on is.na(cp)
  t2 <- tidy(cpt_annotate_events(res,
    data.frame(index = cp + 50, label = "far"), tolerance = 5))
  expect_setequal(t2$status, c("unexplained_changepoint", "undetected_event"))
  expect_false(is.na(t2$cp[t2$status == "unexplained_changepoint"]))
  expect_equal(sum(!is.na(t2$cp)), 1L)          # and it is NOT a match
  expect_equal(sum(t2$status == "matched"), 0L)

  # the tolerance boundary is inclusive, on the position scale
  matched_at <- function(off, tol) {
    tt <- tidy(cpt_annotate_events(res,
      data.frame(index = cp + off, label = "e"), tolerance = tol))
    any(tt$status == "matched")
  }
  expect_true(matched_at(0, 0))
  expect_false(matched_at(1, 0))
  expect_true(matched_at(5, 5))
  expect_false(matched_at(6, 5))

  # a Date column is read on the index scale, never as a position: an event
  # on the first date must land at position 1
  dates <- as.Date("2020-01-01") + seq_along(x) - 1
  rd <- cpt_detect(x, method = "pelt", index = dates)
  td <- tidy(cpt_annotate_events(rd,
    data.frame(when = dates[1], what = "start"), tolerance = 3))
  expect_equal(td$position[td$status == "undetected_event"], 1L)
  # ...and an event on the changepoint's own date matches it
  td2 <- tidy(cpt_annotate_events(rd,
    data.frame(when = dates[rd$changepoints$cp[1]], what = "shift"),
    tolerance = 3))
  expect_true(any(td2$status == "matched"))

  expect_error(cpt_annotate_events(res, data.frame(index = integer(0),
                                                   label = character(0))),
               "no rows")
  expect_error(cpt_annotate_events(res, data.frame(a = 100, b = "x"),
                                   location = "nope"),
               "is not a column of `events`")
})

# ---------------------------------------------------------------------------
# The supervised penalty learner: Hocking's target intervals, and the
# squared-hinge interval regression fitted against them.
#
# `cpt_label_error()` -- the objective -- was verified against its
# definition earlier in this cycle. These are the two pieces built on top of
# it, and neither was tested: the target-interval construction, and the fit.
# ---------------------------------------------------------------------------

test_that("target_interval() takes the longest run of minimum label error", {
  ti <- ggchangepoint:::target_interval
  mk <- function(pen, err) list(penalty = pen, errors = err)

  # a run in the middle closes on both sides, at the run's own penalties
  expect_equal(ti(mk(c(1, 2, 4, 8, 16), c(2, 0, 0, 1, 3))),
               c(log(2), log(4)))
  # a run touching an end of the grid is open on that side, because the
  # grid says nothing about what lies beyond it
  expect_equal(ti(mk(c(1, 2, 4, 8), c(0, 0, 1, 2))), c(-Inf, log(2)))
  expect_equal(ti(mk(c(1, 2, 4, 8), c(2, 1, 0, 0))), c(log(4), Inf))
  expect_equal(ti(mk(c(1, 2, 4), c(0, 0, 0))), c(-Inf, Inf))
  # a single optimum is a degenerate interval, not an error
  expect_equal(ti(mk(c(1, 2, 4, 8), c(2, 0, 1, 3))), c(log(2), log(2)))
  # two runs at the same error: the LONGER one wins
  expect_equal(ti(mk(c(1, 2, 4, 8, 16, 32), c(0, 1, 0, 0, 0, 1))),
               c(log(4), log(16)))
  # nothing scored means nothing is ruled out
  expect_equal(ti(mk(c(1, 2, 4), c(NA, NA, NA))), c(-Inf, Inf))
})

test_that("interval_regression() minimises the squared-hinge objective", {
  ir <- ggchangepoint:::interval_regression
  feats <- cbind(f1 = c(0, 1, 2, 3))
  targ <- cbind(c(1, 2, 3, 4), c(2, 3, 4, 5))
  margin <- 1; lambda <- 1e-3

  # the objective, written from the definition
  obj <- function(w) {
    X <- cbind(1, feats); pred <- as.numeric(X %*% w)
    l <- pmax(0, targ[, 1] + margin - pred)
    r <- pmax(0, pred - targ[, 2] + margin)
    sum(l^2 + r^2) / nrow(X) + lambda * sum(w[-1]^2)
  }
  fit <- ir(feats, targ, lambda = lambda, margin = margin)
  # the reported loss is the objective evaluated at the weights returned
  expect_equal(as.numeric(fit$loss), obj(fit$coefficients), tolerance = 1e-8)
  # and the search improved on the constant it started from
  w0 <- c(mean(rowMeans(targ)), rep(0, ncol(feats)))
  expect_lte(fit$loss, obj(w0) + 1e-8)
  expect_named(fit$coefficients, c("intercept", "f1"))

  # a prediction comfortably inside every interval costs nothing
  wide <- ir(cbind(f1 = c(0, 1)), cbind(c(0, 0), c(10, 10)),
             lambda = 0, margin = 1)
  expect_lt(wide$loss, 1e-8)
  # an infinite bound contributes nothing on its side, rather than Inf
  one_sided <- ir(cbind(f1 = c(0, 1)), cbind(c(5, 5), c(Inf, Inf)),
                  lambda = 0, margin = 1)
  expect_true(is.finite(one_sided$loss))
})

test_that("the learned penalty lands inside each series' target interval", {
  skip_on_cran()
  set.seed(29)
  ti <- ggchangepoint:::target_interval
  series <- list(); labels <- list()
  for (i in 1:5) {
    series[[paste0("s", i)]] <- c(stats::rnorm(100),
                                  stats::rnorm(100, 3 + i * 0.5))
    labels[[paste0("s", i)]] <- cpt_labels(start = 90, end = 110,
                                           change = "one_change")
  }
  m <- cpt_learn_penalty(series, labels, method = "pelt")

  for (nm in names(series)) {
    p <- stats::predict(m, series[[nm]])
    expect_true(is.finite(p) && p > 0, info = nm)
    cv <- cpt_label_error_curve(series[[nm]], labels[[nm]],
                                penalties = 2^(0:6))
    tt <- ti(list(penalty = cv$penalty, errors = cv$errors))
    # predict() is on the natural scale, the target on the log scale
    expect_gte(log(p), tt[1] - 1e-9)
    expect_lte(log(p), tt[2] + 1e-9)
  }
})

test_that("the penalty model is a function of the series, not a constant", {
  skip_on_cran()
  set.seed(30)
  # Series of very different length and noise, so a model that ignored its
  # features would give the same answer four times.
  series <- list(
    short_loud  = c(stats::rnorm(40), stats::rnorm(40, 8)),
    short_quiet = c(stats::rnorm(40, 0, 3), stats::rnorm(40, 1, 3)),
    long_loud   = c(stats::rnorm(400), stats::rnorm(400, 8)),
    long_quiet  = c(stats::rnorm(400, 0, 3), stats::rnorm(400, 1, 3))
  )
  labels <- list(
    short_loud  = cpt_labels(start = 35, end = 45, change = "one_change"),
    short_quiet = cpt_labels(start = 35, end = 45, change = "one_change"),
    long_loud   = cpt_labels(start = 390, end = 410, change = "one_change"),
    long_quiet  = cpt_labels(start = 390, end = 410, change = "one_change")
  )
  # Two of these four have an unbounded target interval -- their labels are
  # satisfied at every penalty in the grid -- and are now dropped from the
  # fit with a warning naming them, rather than contributing a
  # (-Inf, Inf) row that the penaltyLearning path rejects.
  expect_warning(m <- cpt_learn_penalty(series, labels, method = "pelt"),
                 "unbounded target interval")
  cf <- stats::coef(m)
  expect_true("intercept" %in% names(cf))
  expect_gt(length(cf), 1L)

  preds <- vapply(series, function(s) stats::predict(m, s), numeric(1))
  expect_true(all(is.finite(preds) & preds > 0))

  # Whether the predictions DIFFER is a property of the labels, not of the
  # contract: when every target interval is open above -- the common case,
  # since a large penalty keeps the one changepoint the labels ask for --
  # any large prediction is optimal, the slopes are unidentified, and the
  # L2 term takes them to zero, so the model is a constant. An earlier
  # version of this test asserted the spread directly and failed on a
  # different seed for exactly that reason. Assert the implication.
  if (any(abs(cf[-1]) > 1e-10)) {
    expect_gt(length(unique(round(preds, 6))), 1L)
  } else {
    expect_equal(length(unique(round(preds, 6))), 1L)
  }

  # `coef()` is on the log-penalty scale and `predict()` on the natural
  # one -- the distinction the help page now makes.
  x <- series$short_loud
  feats <- ggchangepoint:::cpt_features(x)
  manual <- exp(sum(c(1, feats[m$features]) * cf))
  expect_equal(unname(preds[["short_loud"]]), manual, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# `cpt_leverage()` ranked the most influential observation last.
#
# `max_shift` is how far each original changepoint had to move to find a
# match, and `param_shift` the largest change in a segment parameter. Both
# are undefined for a perturbation that left the engine with NO changepoints
# -- nothing to match against, no parameters to compare -- so
# `NA + z + z` is NA and `order(-leverage)` sent that row to the bottom of a
# table whose entire purpose is "which observations matter most". An
# observation whose deletion destroys the whole segmentation is the most
# influential one there is; measured, it ranked 3 of 3.
#
# The NA is kept, because the two components genuinely are undefined and a
# fabricated number would be worse. Only the ordering changed.
# ---------------------------------------------------------------------------

test_that("cpt_leverage() puts a collapsed fit first, not last", {
  si <- ggchangepoint:::summarise_influence
  mk <- function(cpts, pm, orig_cp, orig_param) {
    s <- si(cpts, pm, orig_cp, orig_param, index = seq_along(cpts))
    structure(list(influence = s$influence, param = pm, original = orig_cp,
                   type = "delete", engine = "recompute", method = "pelt"),
              class = "ggcpt_influence")
  }

  # perturbation 3 destroys a two-changepoint segmentation
  lv <- cpt_leverage(mk(list(c(50L, 100L), c(52L, 100L), integer(0)),
                        rbind(c(0, 3, 1), c(0.1, 3, 1), c(NA, NA, NA)),
                        c(50L, 100L), c(0, 3, 1)))
  expect_equal(lv$index[1], 3L)
  expect_true(is.na(lv$leverage[1]))
  # and the row still says what happened, which is why the NA can stay
  expect_equal(lv$delta_n_cp[1], -2L)
  # the finite rows keep their own descending order below it
  fin <- lv$leverage[!is.na(lv$leverage)]
  expect_true(all(diff(fin) <= 1e-12))

  # with no collapse, nothing changes: no NA, strictly descending
  lv2 <- cpt_leverage(mk(list(c(50L, 100L), c(52L, 100L), c(60L, 100L)),
                         rbind(c(0, 3, 1), c(0.1, 3, 1), c(0.4, 3, 1)),
                         c(50L, 100L), c(0, 3, 1)))
  expect_false(any(is.na(lv2$leverage)))
  expect_true(all(diff(lv2$leverage) <= 1e-12))
  expect_equal(lv2$index[1], 3L)

  # an NA leverage always means THIS perturbation collapsed the fit: when
  # the ORIGINAL found no changepoints, max_shift is missing for every row,
  # the zero-variance guard returns zeros, and no leverage is NA
  lv3 <- cpt_leverage(mk(list(integer(0), c(5L), integer(0)),
                         rbind(c(0), c(0.2), c(0)), integer(0), c(0)))
  expect_true(all(is.na(lv3$max_shift)))
  expect_false(any(is.na(lv3$leverage)))
})

test_that("summarise_influence() computes the shifts it documents", {
  si <- ggchangepoint:::summarise_influence
  s <- si(list(c(50L, 100L), c(52L, 100L), integer(0)),
          rbind(c(0, 3, 1), c(0.1, 3, 1), c(NA, NA, NA)),
          c(50L, 100L), c(0, 3, 1), index = 1:3)
  inf <- s$influence
  expect_equal(inf$n_cp, c(2L, 2L, 0L))
  expect_equal(inf$delta_n_cp, c(0L, 0L, -2L))
  # max_shift is the WORST distance an original changepoint had to move
  expect_equal(inf$max_shift, c(0, 2, NA))
  # param_shift is the largest absolute change in a segment parameter
  expect_equal(inf$param_shift, c(0, 0.1, NA))

  # a perturbation that ADDS a changepoint does not move the originals, so
  # max_shift stays 0 while delta_n_cp rises -- the two components are
  # measuring different things
  s2 <- si(list(c(50L, 75L, 100L)), rbind(c(0, 3, 1)), c(50L, 100L),
           c(0, 3, 1), index = 1L)
  expect_equal(s2$influence$delta_n_cp, 1L)
  expect_equal(s2$influence$max_shift, 0)
})

test_that("cpt_leverage() on a real fit is ordered and complete", {
  set.seed(31)
  x <- c(stats::rnorm(60), stats::rnorm(60, 5))
  r <- cpt_leverage(cpt_detect(x, method = "pelt"),
                    subset = seq(5, 115, by = 10))
  expect_true(all(c("index", "delta_n_cp", "max_shift", "param_shift",
                    "leverage") %in% names(r)))
  expect_equal(nrow(r), 12L)
  fin <- r$leverage[!is.na(r$leverage)]
  expect_true(all(diff(fin) <= 1e-12))
  # every NA, if any, precedes every finite value
  if (any(is.na(r$leverage))) {
    expect_true(all(which(is.na(r$leverage)) < min(which(!is.na(r$leverage)))))
  }
})

# ---------------------------------------------------------------------------
# `cpt_metrics_annotated()` returns a NARROWER table than `cpt_metrics()`,
# which a call moved from one to the other loses columns to. The four it
# does average are plain unweighted means; the three distance metrics are
# omitted because they are NA whenever an annotator shares no matched pair
# with the prediction, so averaging them would divide by fewer annotators
# than `n_annotators` reports.
# ---------------------------------------------------------------------------

test_that("cpt_metrics_annotated() averages exactly four metrics", {
  full <- names(cpt_metrics(c(100, 200), c(100, 200), n = 300))
  ann <- names(cpt_metrics_annotated(c(100, 200), list(c(98, 200), c(100, 203)),
                                     n = 300))
  expect_setequal(ann, c("n", "n_annotators", "n_pred",
                         "precision", "recall", "f1", "covering"))
  # the columns a caller loses, named so a future widening is deliberate
  expect_setequal(setdiff(full, ann),
                  c("n_truth", "hausdorff", "rand_index", "annotation_error",
                    "mae_matched", "rmse_matched"))

  # each averaged value is the plain unweighted mean of the per-annotator one
  anns <- list(c(98, 200), c(100, 203), c(150))
  per <- do.call(rbind, lapply(anns, function(t) {
    cpt_metrics(c(100, 200), t, n = 300, margin = 5)
  }))
  got <- cpt_metrics_annotated(c(100, 200), anns, n = 300, margin = 5)
  for (m in c("precision", "recall", "f1", "covering")) {
    expect_equal(got[[m]], mean(per[[m]]), info = m)
  }
  expect_equal(got$n_annotators, 3L)
  expect_equal(got$n_pred, 2L)

  # f1 and covering are defined for every annotator, which is what makes
  # averaging them safe
  expect_false(any(is.na(per$f1)))
  expect_false(any(is.na(per$covering)))
})

test_that("the distance metrics could not have been averaged safely", {
  # The reason `cpt_metrics_annotated()` omits hausdorff/mae/rmse, on the
  # input that shows it: an annotator with no changepoints shares no
  # matched pair with the prediction, so those columns are NA for it and an
  # average would divide by fewer annotators than `n_annotators` reports.
  #
  # This needs its own annotator set. An earlier version asserted these
  # counts inside the averaging test above, whose annotators all overlap
  # the prediction -- so the numbers were measured on one input and
  # asserted about another, which is the mistake this suite exists to
  # catch.
  anns <- list(c(100, 200), integer(0), 150)
  per <- do.call(rbind, lapply(anns, function(t) {
    cpt_metrics(c(100, 200), t, n = 300, margin = 5)
  }))
  expect_equal(sum(!is.na(per$mae_matched)), 1L)
  expect_equal(sum(!is.na(per$rmse_matched)), 1L)
  expect_equal(sum(!is.na(per$hausdorff)), 2L)
  # while the two that ARE averaged are defined throughout
  expect_false(any(is.na(per$f1)))
  expect_false(any(is.na(per$covering)))
  # ...and the reported table averages over all three regardless
  got <- cpt_metrics_annotated(c(100, 200), anns, n = 300, margin = 5)
  expect_equal(got$n_annotators, 3L)
  expect_equal(got$f1, mean(per$f1))
  expect_equal(got$covering, mean(per$covering))
})

test_that("cpt_metrics_annotated() reads a bare vector as one annotator", {
  a <- cpt_metrics_annotated(c(100, 200), c(100, 200), n = 300)
  expect_equal(a$n_annotators, 1L)
  expect_equal(a$f1, 1)
  expect_equal(a$covering, 1)

  # degenerate annotator sets stay finite rather than returning NaN
  for (anns in list(list(c(100, 200), integer(0)),
                    list(integer(0), integer(0)),
                    list(c(100, 200)))) {
    r <- cpt_metrics_annotated(c(100, 200), anns, n = 300)
    expect_true(all(is.finite(c(r$precision, r$recall, r$f1, r$covering))))
    expect_equal(r$n_annotators, length(anns))
  }

  # and the two shapes that would be read as annotator sets by accident are
  # refused by name
  fit <- cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 4)), method = "pelt")
  expect_error(cpt_metrics_annotated(100, fit, n = 100),
               "not a `ggcpt` object", fixed = TRUE)
  expect_error(cpt_metrics_annotated(100, list(data.frame(cp = 100)), n = 300),
               "not a table", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Findings from the external pre-CRAN audit. Each block names the finding it
# pins, and each fix was verified by measurement before being written.
# ---------------------------------------------------------------------------

test_that("B14: an object-consuming path inherits the fit's change type", {
  # Every raw-data entry point forwarded `change_in` to cpt_detect(); every
  # object-consuming one read `object$method` and left `change_in` at its own
  # default of "mean". So a var or meanvar fit was silently re-detected as a
  # change in the MEAN -- and on a pure variance change the mean detector
  # finds nothing, so every bootstrap replicate was discarded and the caller
  # got a ZERO-WIDTH interval plus a warning blaming the detector.
  set.seed(32)
  x <- c(stats::rnorm(150, 0, 1), stats::rnorm(150, 0, 4))   # variance only
  f <- cpt_detect(x, method = "pelt", change_in = "var")
  skip_if(nrow(f$changepoints) == 0)
  expect_equal(f$change_in, "var")

  ci <- cpt_confint(f, method = "bootstrap", B = 25, seed = 1)
  # the interval must have width, i.e. replicates actually found the change
  expect_true(all(ci$ci_upper - ci$ci_lower > 0))
  # and it must not warn that the detector found nothing
  expect_silent(cpt_confint(f, method = "bootstrap", B = 25, seed = 1))

  # `...` still wins over the object, which is the precedence cpt_detect()
  # gives dots over derived args
  expect_silent(cpt_confint(f, method = "bootstrap", B = 15, seed = 1,
                            change_in = "var"))

  # cpt_sensitivity() re-detects with the fit's change type, so the penalty
  # actually moves the answer
  sn <- cpt_sensitivity(f, over = list(penalty = c(5, 20)))
  expect_gt(length(unique(sn$grid$n_cp)), 1L)

  # cpt_select() inherits it too, and an explicit value still overrides
  s <- cpt_select(f, criterion = "cv", k_max = 3, seed = 1)
  expect_s3_class(s, "ggcpt_selection")
})

test_that("B9: cpt_simulate() reports a changepoint it had to drop", {
  # The ground truth recorded on the result is the FILTERED set, so a
  # discarded location becomes a scoring error nothing can see --
  # cpt_datasets() and cpt_benchmark() read that attribute directly.
  expect_warning(r <- cpt_simulate(200, changepoints = c(100, 500), seed = 1),
                 "1 of 2 outside 1..199", fixed = TRUE)
  expect_warning(cpt_simulate(200, changepoints = c(100, 500), seed = 1),
                 "(500)", fixed = TRUE)
  expect_equal(as.integer(attr(r, "true_changepoints")), 100L)
  # in range, and the boundary cases, stay silent
  expect_silent(cpt_simulate(200, changepoints = c(50, 150), seed = 1))
  expect_silent(cpt_simulate(200, changepoints = 199, seed = 1))
  expect_silent(cpt_simulate(200, changepoints = integer(0), seed = 1))
})

test_that("B4: a JSON null becomes NA rather than vanishing", {
  # cpt_load_tcpd() substituted NA *after* unlist(), by which point there
  # were no NULLs left to find -- so a null did not become NA, it
  # disappeared, shifting every later observation down one and invalidating
  # the annotations cpt_benchmark() scores against. The order is what
  # matters, so the order is what this pins.
  v <- list(1, 2, NULL, 4)
  # the old order silently shortens
  expect_length(unlist(v), 3L)
  # the new order preserves the slot
  v2 <- v
  v2[vapply(v2, is.null, logical(1))] <- NA
  got <- as.numeric(unlist(v2))
  expect_length(got, 4L)
  expect_true(is.na(got[3]))
  expect_equal(got[c(1, 2, 4)], c(1, 2, 4))
  # and the source really does substitute before unlisting
  src <- readLines(test_path("..", "..", "R", "benchmark.R"), warn = FALSE)
  i <- grep("v\\[vapply\\(v, is.null, logical\\(1\\)\\)\\] <- NA", src)
  j <- grep("as.numeric\\(unlist\\(v\\)\\)", src)
  expect_true(length(i) > 0 && length(j) > 0)
  expect_lt(min(i), min(j))
})

test_that("C17: no help page promises a warning cpt_metrics() cannot give", {
  # `?cpt_delay` said cpt_metrics() "warns if you point it at" an online
  # detector. It does not, and it cannot: cpt_metrics() takes bare integer
  # vectors and never learns which detector produced them.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  metrics <- readLines(file.path(root, "R", "metrics.R"), warn = FALSE)
  expect_false(any(grepl("online", metrics, ignore.case = TRUE)))
  monitor <- readLines(file.path(root, "R", "monitor.R"), warn = FALSE)
  expect_false(any(grepl("and warns if\\s*$|warns if you point it at",
                         monitor)))
  # and the corrected sentence says why it cannot
  expect_true(any(grepl("takes bare integer vectors", monitor, fixed = TRUE)))
})

test_that("A1/A2/A3: the declared interface matches what the code needs", {
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  desc <- read.dcf(file.path(root, "DESCRIPTION"))

  # A1: only `changepoint` is full-imported, and it has to be -- glance()'s
  # cost column calls bare logLik(), whose method for class `cpt` is an S4
  # method owned by changepoint. stats::logLik has no S3 method for it.
  imports <- grep("^import\\(", ns, value = TRUE)
  expect_equal(imports, "import(changepoint)")
  expect_true(isGeneric("logLik", where = asNamespace("changepoint")))
  expect_length(methods::findMethods("logLik", classes = "cpt"), 1L)
  expect_null(utils::getS3method("logLik", "cpt", optional = TRUE))

  # A2: three exported scales call discrete_scale() without `scale_name`,
  # which only became optional in ggplot2 3.5.0
  sn <- formals(ggplot2::discrete_scale)$scale_name
  expect_true(grepl("deprecated", paste(deparse(sn), collapse = "")))
  expect_match(desc[1, "Imports"], "ggplot2 \\(>= 3\\.5")

  # A3: fourteen S3method(base::plot, ...) entries need R >= 4.0.0, where
  # `plot` moved from graphics to base
  expect_gt(length(grep("^S3method\\(base::plot", ns)), 10L)
  expect_true("Depends" %in% colnames(desc))
  expect_match(desc[1, "Depends"], "R \\(>= 4")
})

test_that("B27: .resid is a residual for both data_vec conventions", {
  # Two conventions exist for a multivariate result's univariate series:
  # twelve wrappers store X[, 1], while fmean/fcov/kwc/fabisearch store
  # rowMeans(X). build_segments() derives `param_estimate` from whichever
  # the wrapper passed, and augment() was reading the VALUE out of
  # data_wide's first coordinate -- so for those four it subtracted two
  # different quantities and `.resid` was not a residual. `?augment.ggcpt`
  # asserted the coordinate-one convention universally.
  set.seed(33)
  X <- matrix(stats::rnorm(120 * 6), nrow = 120)

  resid_is_residual <- function(f) {
    ag <- augment(f)
    isTRUE(all.equal(ag$.resid, f$data$value - ag$.fitted))
  }

  # univariate, and the coordinate-one convention
  expect_true(resid_is_residual(cpt_detect(X[, 1], method = "pelt")))
  if (engine_usable("InspectChangepoint")) {
    fi <- suppressWarnings(cpt_detect(X, method = "inspect"))
    expect_true(resid_is_residual(fi))
    # inspect keeps coordinate one, so the two agree there
    expect_equal(fi$data$value, X[, 1])
  }
  # the rowMeans convention -- the four this fixes
  if (engine_usable("fChange")) {
    fm <- suppressWarnings(cpt_detect(X, method = "fmean"))
    expect_true(resid_is_residual(fm))
    expect_equal(fm$data$value, as.numeric(rowMeans(X)))
    # and augment() still returns every coordinate plus one row per obs
    ag <- augment(fm)
    expect_equal(nrow(ag), nrow(X))
  }
})

test_that("B18: every event is matched or undetected, never neither", {
  # match_changepoints() matched per element of `truth`, but the event row
  # was recovered by value lookup with [1], so two events at one position
  # both reported the FIRST label -- and `undetected` excluded by position,
  # so `%in%` removed both. The second event was neither matched nor
  # undetected: it vanished from a function whose purpose is to report both.
  set.seed(34)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  f <- cpt_detect(x, method = "pelt")
  cp <- f$changepoints$cp
  skip_if(length(cp) != 1)

  accounted <- function(ev, tol) {
    td <- tidy(cpt_annotate_events(f, ev, tolerance = tol))
    setequal(as.character(ev$label), as.character(stats::na.omit(td$event)))
  }
  # two events on the changepoint: one can be claimed, the other must show
  # up as undetected -- not disappear
  ev <- data.frame(index = c(cp, cp), label = c("first", "second"))
  expect_true(accounted(ev, 2))
  td <- tidy(cpt_annotate_events(f, ev, tolerance = 2))
  expect_equal(nrow(td), 2L)
  expect_setequal(td$status, c("matched", "undetected_event"))
  expect_equal(td$event[td$status == "matched"], "first")
  expect_equal(td$event[td$status == "undetected_event"], "second")

  # three at one position, and two at a position no changepoint reaches
  expect_true(accounted(data.frame(index = rep(cp, 3),
                                   label = c("a", "b", "c")), 2))
  expect_true(accounted(data.frame(index = rep(cp + 50, 2),
                                   label = c("a", "b")), 2))
  # ordinary distinct events are unchanged
  expect_true(accounted(data.frame(index = c(cp, cp + 50),
                                   label = c("hit", "miss")), 5))
})

test_that("B25: an nsp result's changepoints and regions agree in count", {
  skip_if_not(engine_usable("nsp"))
  # `mids` went through ggcpt_build(), which dedups `cp` and range-filters;
  # `regions` went through normalise_regions(), which does neither. Nested
  # intervals are the normal output of the narrowest-significance
  # construction and two can round to one midpoint, so a changepoint row
  # was dropped -- taking its region bounds with it -- while $regions kept
  # both. print() then reported one fewer changepoint than the regions
  # table below it, and cpt_confint() one fewer interval than there were
  # regions, under-reporting the object the method exists to produce.
  set.seed(35)
  for (s in 1:3) {
    for (a in c(0.05, 0.2)) {
      x <- c(stats::rnorm(90), stats::rnorm(90, 3), stats::rnorm(90, 1))
      f <- tryCatch(suppressWarnings(
        cpt_detect(x, method = "nsp", alpha = a, M = 120, seed = s)),
        error = function(e) NULL)
      if (is.null(f)) next
      info <- paste("seed", s, "alpha", a)
      expect_equal(nrow(f$changepoints), nrow(f$regions), info = info)
      if (nrow(f$changepoints) > 0) {
        # each changepoint sits inside the region it was keyed to
        expect_true(all(f$changepoints$cp >= f$changepoints$region_start),
                    info = info)
        expect_true(all(f$changepoints$cp <= f$changepoints$region_end),
                    info = info)
        # and cpt_confint() reports one interval per region
        ci <- suppressWarnings(cpt_confint(f))
        expect_equal(nrow(ci), nrow(f$regions), info = info)
      }
    }
  }
})

test_that("B1: a ts's seasonal frequency reaches the engine that needs it", {
  # `as_cpt_series()` reduces every accepted input to a bare numeric vector,
  # which threw away the one piece of information bfast cannot guess and
  # bfast_wrapper()'s own documentation tells the user to supply by passing a
  # `ts`. cpt_detect(quarterly_ts, method = "bfast") therefore refitted at
  # the wrapper's default frequency of 12 -- monthly seasonality on
  # quarterly data -- with nothing said.
  y <- stats::ts(1:24, frequency = 4, start = c(2000, 1))
  expect_equal(as_cpt_series(y)$frequency, 4)
  # A frequency of 1 is "no seasonality", not a frequency to forward, and a
  # plain vector has none at all.
  expect_null(as_cpt_series(stats::ts(1:24))$frequency)
  expect_null(as_cpt_series(1:24)$frequency)
  expect_null(as_cpt_series(as.numeric(y), index = seq_along(y))$frequency)

  skip_if_not(engine_usable("bfast"))
  set.seed(1)
  season <- rep(c(0, 3, 0, -3), length.out = 120)
  x <- stats::ts(c(stats::rnorm(60, 1), stats::rnorm(60, 6)) + season,
                 frequency = 4, start = c(2000, 1))
  freq_fitted <- function(r) stats::frequency(r$fit$Yt)
  # The measured quantity is the frequency bfast actually fitted with, not
  # whether the changepoints happen to agree: a series can give the same
  # answer at 4 and at 12.
  expect_equal(freq_fitted(cpt_detect(x, method = "bfast")), 4)
  expect_equal(freq_fitted(bfast_wrapper(x)), 4)
  # An explicit `frequency` still wins over the one read off the series.
  expect_equal(freq_fitted(cpt_detect(x, method = "bfast", frequency = 6)), 6)
  # And an engine with no `frequency` argument must not be handed one.
  expect_s3_class(cpt_detect(x, method = "pelt"), "ggcpt")
})

test_that("B2: cpt_monitor(method = 'cpm') applies the wrapper's guards", {
  skip_if_not(engine_usable("cpm"))
  # cpm reports a withheld `cpmType`, a missing FET `lambda` and an off-grid
  # `ARL0` by *printing* an error and handing back something unusable, so
  # each door into the engine has to look for it. This one called
  # cpm::makeChangePointModel() directly and inherited none of the three:
  # measured, the user saw `no applicable method for '@' applied to an
  # object of class "NULL"` for the type and for the arl0, and base R's
  # `only 0's may be mixed with negative subscripts` for the lambda.
  b <- stats::rnorm(60)
  expect_error(cpt_monitor("cpm", cpm_type = "GLRAdjusted", baseline = b),
               "should be one of")
  expect_error(cpt_monitor("cpm", cpm_type = "FET", baseline = b),
               "needs a `lambda` value")
  expect_error(cpt_monitor("cpm", arl0 = 123, baseline = b),
               "not an average run length")
  expect_error(cpt_monitor("cpm", arl0 = NA, baseline = b),
               "single finite number")
  # The same names cpm_wrapper() accepts still work, partial matching and all.
  expect_equal(cpt_monitor("cpm", cpm_type = "Mann",
                           baseline = b)$state$cpm_type, "Mann-Whitney")
  expect_s3_class(cpt_monitor("cpm", cpm_type = "Mood", baseline = b),
                  "ggcpt_monitor")
  expect_s3_class(cpt_monitor("cpm", cpm_type = "FET", lambda = 0.3,
                              baseline = stats::rbinom(60, 1, 0.2)),
                  "ggcpt_monitor")
  # and the batch wrapper's own messages are unchanged.
  expect_error(cpm_wrapper(stats::rnorm(50), cpm_type = "GLRAdjusted"),
               "should be one of")
  expect_error(cpm_wrapper(stats::rnorm(50), arl0 = 123),
               "not an average run length")
})

test_that("B3: an accumulating e-detector does not go silent on overflow", {
  # `R <- (1 + R) * inc` grows multiplicatively, so with reset = FALSE and
  # relearn = 0 -- the configuration the arguments explicitly offer -- it
  # passes .Machine$double.xmax a few hundred observations after a real
  # change and the next product is Inf. The alarm rule reads a non-finite
  # statistic as "no alarm": measured 82 alarms and then 1418 observations
  # of total silence on a stream that had shifted by five baseline SDs.
  set.seed(7)
  mo <- cpt_monitor("edetector", baseline = stats::rnorm(100),
                    reset = FALSE, relearn = 0)
  mo <- cpt_update(mo, stats::rnorm(1500, 5))
  expect_true(all(is.finite(mo$state$R)))
  expect_true(all(is.finite(mo$alarms$statistic)))
  # The detector is still alarming at the end of the stream, not just at the
  # start of it.
  expect_gt(sum(mo$alarms$time > mo$t - 100), 0)
  expect_equal(max(mo$alarms$time), mo$t)
  # Saturation is not absorbing the way Inf is: in-control data brings the
  # statistic back down.
  back <- cpt_update(mo, stats::rnorm(2000))
  expect_lt(max(back$state$R), max(mo$state$R))
  # The default (reset = TRUE) path is untouched -- it never reaches the cap.
  set.seed(7)
  mor <- cpt_update(cpt_monitor("edetector", baseline = stats::rnorm(100)),
                    stats::rnorm(300, 5))
  expect_true(all(mor$state$R < .Machine$double.xmax / 2))
  expect_gt(nrow(mor$alarms), 0)
})

test_that("B5: the seed stays scoped under a parallel plan too", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  # Five functions run detection through future.apply. Three registered the
  # restore handler above the branch; cpt_batch() and ggcpt_compare() did it
  # only inside the SEQUENTIAL one -- and future.apply documents that for
  # every `future.seed` value except FALSE/NULL the caller's RNG state is
  # forwarded one step. Measured directly: future_lapply(future.seed = 7)
  # and future_lapply(future.seed = TRUE) both leave a different
  # `.Random.seed` behind, so under plan(multisession) these two broke the
  # promise `@param seed` makes verbatim ("scoped to this call ... a seeded
  # call inside a simulation loop does not pin the loop's own stream").
  set.seed(13)
  x <- c(stats::rnorm(90), stats::rnorm(90, 3))
  d <- list(a = x, b = rev(x))
  preserved <- function(run) {
    set.seed(99); before <- get(".Random.seed", envir = globalenv())
    invisible(run())
    identical(get(".Random.seed", envir = globalenv()), before)
  }
  runs <- list(
    cpt_batch = function() cpt_batch(d, method = "pelt", seed = 7),
    ggcpt_compare = function() {
      ggcpt_compare(x, methods = c("pelt", "binseg"), seed = 7)
    }
  )
  for (nm in names(runs)) expect_true(preserved(runs[[nm]]), info = nm)

  old <- future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(old))
  expect_false(inherits(future::plan(), "sequential"))
  for (nm in names(runs)) expect_true(preserved(runs[[nm]]), info = nm)
  # and the seed still governs the answer on the parallel path
  expect_equal(cpt_batch(d, method = "pelt", seed = 7)$changepoints,
               cpt_batch(d, method = "pelt", seed = 7)$changepoints)
})

test_that("B6: cpt_metrics_annotated() refuses an empty annotation list", {
  # `is.list(list())` is TRUE, so the empty list was not wrapped as a single
  # annotator; do.call(rbind, list()) is NULL, NULL$n_pred[1] is NULL, and
  # tibble() drops a NULL argument -- so the caller got a one-row tibble
  # with no `n_pred` column at all, four NA metrics, and four base-R
  # warnings about a non-numeric argument to mean().
  expect_error(cpt_metrics_annotated(c(100, 200), annotations = list(),
                                     n = 300),
               "no ground truth")
  # The shape of a real call is unchanged.
  ok <- cpt_metrics_annotated(c(100, 200),
                              annotations = list(c(98, 200), c(100, 203)),
                              n = 300, margin = 5)
  expect_true("n_pred" %in% names(ok))
  expect_equal(nrow(ok), 1L)
})

test_that("B7: a criterion that cannot score says so", {
  # `stability` is NA at every rung with no changepoints, so on a series
  # where the method returns one empty segmentation at every penalty the
  # curve is all-NA, which.max() gives integer(0), and `if (integer(0) %in%
  # ks)` failed with base R's "argument is of length zero" -- a message
  # naming neither the criterion nor the series.
  skip_if_not(engine_usable("wbs"))
  set.seed(3)
  flat <- stats::rnorm(120, 0, 0.01)
  err <- tryCatch(suppressWarnings(
    cpt_select(flat, method = "wbs", criterion = "stability", B = 5)),
    error = function(e) conditionMessage(e))
  # Either it scored the ladder or it refused it by name; what it must not
  # do is fail with base R's length-zero message.
  if (is.character(err)) {
    expect_match(err, "could not score")
    expect_match(err, "stability")
    expect_false(grepl("argument is of length zero", err, fixed = TRUE))
  } else {
    expect_s3_class(err, "ggcpt_selection")
  }
  # A criterion that can score is unaffected.
  s <- suppressWarnings(cpt_select(c(stats::rnorm(80), stats::rnorm(80, 5)),
                                   method = "pelt", criterion = "stability",
                                   B = 5))
  expect_equal(length(s$k), 1L)
  expect_false(is.na(s$k))
})

test_that("B8: every bundled package is one this package can use", {
  # `tsbox` (in "time") and `patchwork` (in "reporting") appeared nowhere
  # else in the package -- not in Suggests, not in R/, not in the tests or
  # vignettes -- so the installer offered to fetch two dependencies nothing
  # could then use, and "reporting" installed seven packages where
  # `@param bundle` documented six.
  bundles <- cpt_install_engines(bundle = c("time", "reporting"),
                                 dry_run = TRUE)
  expect_false(any(bundles$package %in% c("tsbox", "patchwork")))
  # Every non-engine extra must be declared in Suggests, which is the only
  # thing that makes it reachable from this package.
  suggests <- strsplit(
    gsub("\\s+|\\([^)]*\\)", "",
         paste(read.dcf(system.file("DESCRIPTION", package = "ggchangepoint"),
                        fields = "Suggests"), collapse = "")),
    ",")[[1]]
  expect_true(all(bundles$package %in% suggests),
              info = paste(setdiff(bundles$package, suggests),
                           collapse = ", "))
})

test_that("B10: cpt_simulate(change_in = 'slope') names the shape it wants", {
  # `mean` and `var` take an atomic vector, so `params = c(0, 1)` is the
  # natural mistake -- and it reached `p$intercept` on an atomic value,
  # which is base R's "$ operator is invalid for atomic vectors": a message
  # naming neither the argument nor the shape. The "meanvar" branch guards.
  expect_error(cpt_simulate(200, 100, change_in = "slope", params = c(0, 1)),
               "list\\(intercept")
  expect_error(suppressWarnings(
    cpt_simulate(200, 100, change_in = "slope",
                 params = list(list(a = 1, b = 2)))),
    "list\\(intercept")
  expect_equal(nrow(cpt_simulate(200, 100, change_in = "slope",
                                 params = list(
                                   list(intercept = 0, slope = 0.1),
                                   list(intercept = 5, slope = -0.2)))),
               200L)
  expect_equal(nrow(cpt_simulate(200, 100, change_in = "slope")), 200L)
})

test_that("B13: the segneigh solution path keeps every candidate", {
  # Row i of cpts.full holds the segmentation with i changepoints. Binary
  # segmentation's rows are nested; Segment Neighbourhood's are NOT -- it
  # re-solves the dynamic program at each K -- so consecutive rows can
  # differ by more than one changepoint, and keeping only `new[1]` dropped
  # the rest and mis-numbered `step`.
  set.seed(4)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4), stats::rnorm(60, 1),
         stats::rnorm(60, 5))
  f <- suppressWarnings(cpt_detect(x, method = "segneigh", penalty = "SIC",
                                   Q = 6))
  full <- changepoint::cpts.full(f$fit)
  cands <- sort(unique(as.integer(full[!is.na(full)])))
  cands <- cands[cands >= 1 & cands < nrow(f$data)]
  sp <- cpt_solution_path(f)
  expect_setequal(sp$cp, cands)
  expect_identical(sp$step, seq_len(nrow(sp)))
  # binseg is nested, so its path is one candidate per row either way.
  fb <- suppressWarnings(cpt_detect(x, method = "binseg", Q = 6))
  spb <- cpt_solution_path(fb)
  expect_identical(spb$step, seq_len(nrow(spb)))
})

test_that("B11: a registered solution path goes through the same contract", {
  with_diag <- function(nm, dp) function(x, ...) {
    r <- as_ggcpt(c(50, 100), x, method = nm)
    r$diagnostics <- list(solution_path = dp)
    r
  }
  cpt_register_method("bp_nocp", with_diag("bp_nocp",
                                           data.frame(loc = c(50, 100))),
                      engine = "test")
  cpt_register_method("bp_range", with_diag("bp_range",
                                            data.frame(cp = c(50, 100, 9999))),
                      engine = "test")
  withr::defer({
    cpt_unregister_method("bp_nocp"); cpt_unregister_method("bp_range")
  })
  z <- stats::rnorm(150)
  # Without `cp` this used to die inside the plot at `idx_vals[path$cp]`.
  expect_error(cpt_solution_path(cpt_detect(z, method = "bp_nocp")),
               "needs a `cp` column")
  p <- cpt_solution_path(cpt_detect(z, method = "bp_range"))
  expect_equal(p$cp, c(50L, 100L))                  # 9999 range-filtered
  expect_true(all(c("step", "cp", "contrast", "start", "end",
                    "selected") %in% names(p)))
  expect_true(all(p$selected))                      # both are in the fit
  expect_s3_class(ggplot2::ggplot_build(
    ggcpt_solution_path(cpt_detect(z, method = "bp_range"))), "ggplot_built")
})

test_that("B15: geom_cpt_event() lets the caller map colour and linetype", {
  # Five aesthetics are pulled out of the caller's mapping for the rule
  # layer, and two of them were then also set as fixed parameters from the
  # function's own formals. A fixed parameter beats a mapping in ggplot2,
  # silently, so the extraction was dead for exactly the two aesthetics a
  # caller is most likely to map -- while `alpha` and `linewidth`, not
  # shadowed, worked.
  ev <- data.frame(x = c(60, 120), kind = c("a", "b"))
  d <- data.frame(index = 1:180,
                  value = c(stats::rnorm(60), stats::rnorm(60, 3),
                            stats::rnorm(60)))
  p <- ggplot2::ggplot(d, ggplot2::aes(index, value)) + ggplot2::geom_line() +
    geom_cpt_event(ggplot2::aes(xintercept = x, colour = kind, label = kind),
                   data = ev)
  b <- ggplot2::ggplot_build(p)
  expect_gt(length(unique(b$data[[2]]$colour)), 1L)
  # and a fixed colour still wins when nothing is mapped
  p2 <- ggplot2::ggplot(d, ggplot2::aes(index, value)) + ggplot2::geom_line() +
    geom_cpt_event(ggplot2::aes(xintercept = x, label = kind), data = ev,
                   colour = "red")
  b2 <- ggplot2::ggplot_build(p2)
  expect_identical(unique(b2$data[[2]]$colour), "red")
})

test_that("B17: every change_in a wrapper can label is one the registry lists", {
  # not_wrapper() maps `contrast = "pcwsConstMeanVar"` to change_in =
  # "meanvar", but "meanvar" was not in `not`'s registry row -- so a
  # `cpt_detect(x, method = "not", change_in = "var")` result carried a
  # change_in that validate_method_change_in() would refuse and the
  # dispatcher could never be asked for.
  levels_for <- function(m) {
    strsplit(subset(cpt_methods(), method == m)$change_in, ",\\s*")[[1]]
  }
  expect_true("meanvar" %in% levels_for("not"))
  skip_if_not(engine_usable("not"))
  set.seed(5)
  x <- c(stats::rnorm(80), stats::rnorm(80, 0, 4))
  for (ci in c("var", "meanvar")) {
    f <- suppressWarnings(cpt_detect(x, method = "not", change_in = ci))
    expect_true(f$change_in %in% levels_for("not"), info = ci)
  }
})

test_that("B19: a character index does not empty the events table", {
  # A character index is explicitly supported, and a character events column
  # matched it on class -- but the lookup was arithmetic, `as.numeric()` on
  # a label is NA, and the is.na() filter then emptied the table. The
  # function reported zero matched events, zero undetected events, and every
  # changepoint unexplained -- plus one "NAs introduced by coercion" warning
  # per event.
  set.seed(6)
  lab <- sprintf("w%03d", 1:120)
  f <- cpt_detect(c(stats::rnorm(60), stats::rnorm(60, 4)), method = "pelt",
                  index = lab)
  a <- expect_silent(cpt_annotate_events(
    f, data.frame(when = lab[c(60, 100)], what = c("hit", "miss")),
    tolerance = 3))
  td <- tidy(a)
  expect_equal(nrow(td), 2L)
  expect_setequal(td$status, c("matched", "undetected_event"))
  expect_equal(sort(td$position), c(60L, 100L))
  # A label that is not in the index has no position, and is dropped -- but
  # it must not take the others with it.
  b <- suppressWarnings(tidy(cpt_annotate_events(
    f, data.frame(when = c(lab[60], "nosuchweek"), what = c("hit", "?")),
    tolerance = 3)))
  expect_true(any(b$status == "matched"))
})

test_that("B20: check_labels() validates the change vocabulary", {
  # cpt_labels() checks it; this door checked only that the columns exist,
  # and `@param labels` deliberately widens the contract to "anything with
  # start/end/change columns". So documented input reached a switch() with
  # no default and failed with "replacement has length zero".
  set.seed(7)
  fit <- cpt_detect(c(stats::rnorm(80), stats::rnorm(80, 4)), method = "pelt")
  expect_error(cpt_label_error(fit, data.frame(start = 1, end = 20,
                                               change = "maybe")),
               "Unknown `change` label")
  expect_s3_class(cpt_label_error(fit, data.frame(start = 1, end = 20,
                                                  change = "no_change")),
                  "cpt_label_error")
})

test_that("B21/B22: cpt_recommend()'s two lists reach the methods they name", {
  # "changepoints" is an ENGINE, not a method, so it matched no registry row
  # and the four high-dimensional dynamic-programming methods never got the
  # "slow at n" caveat.
  rec <- cpt_recommend(n = 20000, dimension = "multivariate")
  hd <- subset(rec, method %in% c("hdcov", "network", "var", "hdreg"))
  if (nrow(hd) > 0) expect_true(all(grepl("slow at n", hd$caveat)))
  # And `change_in = "mean"` is accepted by every method (the dispatcher's
  # own rule), so reading `supports` strictly here dropped every
  # distribution-only method -- including the two this function's own
  # heavy-tail preference list names first.
  heavy <- cpt_recommend(dimension = "univariate", change_in = "mean",
                         noise = "heavy")
  expect_true("np" %in% heavy$method)
  expect_true("ecp" %in% heavy$method)
})

test_that("B23/B24: the benchmark plots refuse what they cannot draw", {
  skip_on_cran()
  ds <- cpt_datasets()[1:2]
  bm1 <- suppressWarnings(cpt_benchmark(ds, methods = "pelt",
                                        progress = FALSE))
  # qtukey() is undefined below nmeans = 2 and answers with NaN, which drew
  # a diagram with an NaN rectangle and an all-NA `within_cd`.
  expect_error(ggplot2::autoplot(bm1, plot_type = "critical_difference"),
               "at least two")
  bm2 <- suppressWarnings(cpt_benchmark(ds, methods = c("pelt", "binseg"),
                                        progress = FALSE))
  expect_error(ggplot2::autoplot(bm2, plot_type = "critical_difference",
                                 alpha = 1.5),
               "alpha")
  expect_s3_class(ggplot2::ggplot_build(
    ggplot2::autoplot(bm2, plot_type = "critical_difference")),
    "ggplot_built")
})

test_that("B29/B30: esac and pilliat keep their shape on an all-flat input", {
  skip_if_not(engine_usable("HDCD"))
  # Both are `univariate = FALSE`, so their results are always meant to be
  # multivariate; omitting `data_wide` on the all-flat early return made
  # n_coordinates() report 1, which sent autoplot() down the univariate
  # branch and augment() to `use_wide = FALSE`.
  flat <- matrix(rep(1, 400), ncol = 4)
  shapes <- vapply(c("esac", "pilliat", "inspect"), function(m) {
    r <- suppressWarnings(cpt_detect(flat, method = m))
    ncol(r$data_wide %||% matrix(1))
  }, numeric(1))
  expect_equal(length(unique(shapes)), 1L, info = paste(shapes, collapse = ","))
})

test_that("B31: fastcpd's penalty reaches the engine and is reported", {
  skip_if_not(engine_usable("fastcpd"))
  # `derived_args_for("fastcpd", ...)` returned only the family, so
  # cpt_detect(x, method = "fastcpd", penalty = 5) resolved the 5 and threw
  # it away; and `penalty = list(type = "MBIC")` was hard-coded, so
  # fastcpd_wrapper(x, beta = 40) ran at 40 while print() said MBIC.
  set.seed(9)
  y <- c(stats::rnorm(60), stats::rnorm(60, 0.8), stats::rnorm(60),
         stats::rnorm(60, 1.2))
  loose <- cpt_detect(y, method = "fastcpd", penalty = 1)
  tight <- cpt_detect(y, method = "fastcpd", penalty = 50)
  # The measured quantity is that the penalty changes the answer, not just
  # that the call ran.
  expect_gt(nrow(loose$changepoints), nrow(tight$changepoints))
  expect_equal(loose$penalty$type, "Manual")
  expect_equal(loose$penalty$value, 1)
  expect_equal(fastcpd_wrapper(y, beta = 40)$penalty$value, 40)
  # The default is fastcpd's own MBIC, and is still reported as such.
  expect_equal(cpt_detect(y, method = "fastcpd")$penalty$type, "MBIC")
  expect_equal(fastcpd_wrapper(y)$penalty$type, "MBIC")
  # A name the two packages share is translated; one they do not is left to
  # the engine rather than silently approximated.
  expect_equal(cpt_detect(y, method = "fastcpd", penalty = "BIC")$penalty$type,
               "BIC")
  expect_equal(cpt_detect(y, method = "fastcpd", penalty = "AIC")$penalty$type,
               "MBIC")
})

test_that("B33: the label-error target band survives both edge cases", {
  # `all(logical(0))` is TRUE, so a MISSING target attribute passed the
  # guard and annotate() got zero-length xmin/xmax. And an infinite endpoint
  # is returned deliberately for "the minimum extends past the grid" --
  # which is the diagnosis the reader needs, and the finiteness test hid the
  # band in exactly that case.
  mk <- function(target) {
    structure(tibble::tibble(penalty = c(1, 10, 100),
                             false_positive = c(2, 0, 0),
                             false_negative = c(0, 0, 2),
                             errors = c(2, 0, 2)),
              class = c("ggcpt_label_curve", "tbl_df", "tbl", "data.frame"),
              method = "pelt", target = target)
  }
  half <- ggplot2::ggplot_build(ggplot2::autoplot(mk(c(log(2), Inf))))
  closed <- ggplot2::ggplot_build(ggplot2::autoplot(mk(c(log(2), log(20)))))
  # the band is a layer in both cases
  expect_equal(length(half$data), length(closed$data))
  expect_s3_class(ggplot2::ggplot_build(ggplot2::autoplot(mk(NULL))),
                  "ggplot_built")
})

test_that("B34: a wrong-length fitted signal is reported, not just dropped", {
  # as_ggcpt() was changed to error on this; ggcpt_build() -- the door all
  # nine `fitted = TRUE` engines come through -- still dropped it silently,
  # after which autoplot(show_fit = TRUE) told the user the result "carries
  # no fitted signal" about a signal the engine had computed.
  expect_warning(
    r <- ggcpt_build(stats::rnorm(50), 25L, method = "toy",
                     change_in = "mean",
                     penalty = list(type = "None", value = NA_real_),
                     fitted = stats::rnorm(40)),
    "length 40 for a series of length 50")
  expect_false("fitted" %in% names(r$data))
  expect_silent(
    ok <- ggcpt_build(stats::rnorm(50), 25L, method = "toy",
                      change_in = "mean",
                      penalty = list(type = "None", value = NA_real_),
                      fitted = stats::rnorm(50)))
  expect_true("fitted" %in% names(ok$data))
})

test_that("B36: a multi-series label set is not silently scored as one", {
  # The `series` column is carried into the output and described in @return,
  # and nothing filtered on it -- so the multi-series set cpt_learn_penalty()
  # takes had every series' labels scored against this one fit.
  set.seed(8)
  fit <- cpt_detect(c(stats::rnorm(80), stats::rnorm(80, 4)), method = "pelt")
  ml <- data.frame(series = c("a", "a", "b"), start = c(1, 40, 1),
                   end = c(20, 60, 20),
                   change = c("no_change", "change", "change"))
  expect_warning(cpt_label_error(fit, ml), "names 2 series")
  expect_silent(cpt_label_error(fit, ml[ml$series == "a", ]))
})

test_that("C1/C2: selection_adjusted says what each test can support", {
  # The Chow F's reference distribution assumes the break date was fixed in
  # advance, so quoting it AT an estimated break is exactly the circularity
  # the column exists to flag -- it was set TRUE. And Davies' test is one
  # global test, so its rows repeat one p-value; the method string now says
  # so rather than reading as one test per changepoint.
  skip_if_not(engine_usable("strucchange"))
  set.seed(21)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4))
  f <- suppressWarnings(cpt_detect(x, method = "strucchange"))
  tt <- suppressWarnings(cpt_test(f))
  expect_true(all(grepl("Chow", tt$method)))
  expect_false(any(tt$selection_adjusted))
  expect_true(all(grepl("unadjusted", tt$method)))
})

test_that("C6: the residual bootstrap cannot fall into sample()'s trap", {
  # `sample(x, n)` means `sample.int(x, n)` when x is a single number >= 1,
  # so a one-observation segment would resample `1:round(resid)`. It was
  # safe only because a length-1 segment's residual against its own mean is
  # exactly 0 -- a coupling to param_estimate that no longer has to hold.
  srcs <- vapply(c("confint_bootstrap", "stability_curve"), function(f) {
    paste(deparse(get(f, envir = asNamespace("ggchangepoint"))),
          collapse = " ")
  }, character(1))
  for (nm in names(srcs)) {
    expect_false(grepl("sample(resid[idx]", srcs[[nm]], fixed = TRUE),
                 info = nm)
    expect_true(grepl("sample.int(length(idx)", srcs[[nm]], fixed = TRUE),
                info = nm)
  }
})

test_that("C9/C10: the power and benchmark tables name what they hold", {
  skip_on_cran()
  set.seed(31)
  pw <- cpt_power(n = 200, jump = 3, n_sim = 6, seed = 1, parallel = FALSE)
  # `extra` is a COUNT of detections outside the tolerance window, not a
  # rate, and the old name invited reading 137 as a percentage.
  expect_true("false_positives" %in% names(pw))
  expect_false("false_positive_rate" %in% names(pw))
  expect_true(all(pw$false_positives >= 0))

  # A dataset every method failed on ties them at the same worst rank -- no
  # information -- but it still incremented N, and CD shrinks with N. So a
  # failed dataset made the critical distance NARROWER than the informative
  # ones support: anti-conservative.
  ds <- cpt_datasets()[1:2]
  bm <- suppressWarnings(cpt_benchmark(ds, methods = c("pelt", "binseg"),
                                       progress = FALSE))
  metric <- attr(bm, "metrics")[1]
  rk_ok <- ggchangepoint:::benchmark_ranks(bm, metric)
  bm_bad <- bm
  bm_bad[[metric]][bm_bad$dataset == unique(bm$dataset)[1]] <- NA_real_
  rk_bad <- ggchangepoint:::benchmark_ranks(bm_bad, metric)
  expect_lt(rk_bad$n_datasets[1], rk_ok$n_datasets[1])
  expect_equal(rk_bad$n_datasets_total[1], rk_ok$n_datasets_total[1])
  # ...so the critical distance gets WIDER, not narrower, when a dataset
  # stops carrying information.
  expect_gt(ggchangepoint:::nemenyi_cd(2, rk_bad$n_datasets[1]),
            ggchangepoint:::nemenyi_cd(2, rk_ok$n_datasets[1]))
})

test_that("D1: cpt_monitor() validates the arguments each method uses", {
  b <- stats::rnorm(60)
  # `deltas = 0` makes every likelihood ratio exactly 1, so R grows linearly
  # whatever the data does and the monitor alarms at t = 1/alpha on noise.
  expect_error(cpt_monitor("edetector", baseline = b, deltas = 0),
               "non-zero")
  expect_error(cpt_monitor("edetector", baseline = b, deltas = c(NA, 1)),
               "finite")
  expect_warning(cpt_monitor("edetector", baseline = b, deltas = c(0, 1)),
                 "contains 0")
  # `...` reaches the engine in the other two branches and has nowhere to go
  # in this one, so it used to vanish silently.
  expect_warning(cpt_monitor("edetector", baseline = b, nosucharg = 3),
                 "takes no engine arguments")
  skip_if_not(engine_usable("ocd"))
  B <- matrix(stats::rnorm(120), ncol = 3)
  expect_error(cpt_monitor("ocd", baseline = B, patience = 0), "patience")
  expect_error(cpt_monitor("ocd", baseline = B, mc_reps = 0), "mc_reps")
})

test_that("D6/D11/D34: three silent drops now say something", {
  # attach_index() dropped a wrong-length index without a word, leaving a
  # result that plots in positions and a caller with no way to know. Every
  # public door validates the length first -- cpt_detect() refuses it in
  # validate_index() -- so this is the internal backstop, reached only if a
  # future caller forgets, and it must not go back to being silent.
  fit50 <- cpt_detect(stats::rnorm(50), method = "pelt")
  expect_warning(
    dropped <- ggchangepoint:::attach_index(fit50,
                                            as.Date("2020-01-01") + 0:9),
    "value\\(s\\) for a series of length 50")
  expect_null(dropped$index)
  expect_error(cpt_detect(stats::rnorm(50), method = "pelt",
                          index = as.Date("2020-01-01") + 0:9),
               "one value per observation")
  # cpt_simulate() accepted an n every consumer in the package then refuses.
  expect_error(cpt_simulate(2), "n")
  expect_s3_class(cpt_simulate(3), "tbl_df")
  # And the influence loop's index shift is type-stable when a perturbed
  # fit finds nothing: `ifelse(logical(0), ...)` returned logical(0), so the
  # list of segmentations mixed integer and logical vectors.
  set.seed(41)
  x <- c(stats::rnorm(40), stats::rnorm(40, 5))
  fit <- cpt_detect(x, method = "pelt")
  inf <- suppressWarnings(cpt_influence(fit, type = "delete"))
  cpts <- attr(inf, "cpts") %||% inf$cpts
  if (!is.null(cpts)) {
    expect_true(all(vapply(cpts, function(v) {
      length(v) == 0L || is.numeric(v)
    }, logical(1))))
  } else {
    expect_s3_class(inf, "ggcpt_influence")
  }
})

test_that("D10: a data frame is not a list of annotators", {
  # A data frame IS a list, so tidy(fit) was read column-by-column and
  # `cp_value` -- raw data values -- was scored as changepoint locations.
  expect_error(cpt_metrics_annotated(c(100, 200),
                                     annotations = data.frame(cp = c(100, 200)),
                                     n = 300),
               "data frame is a list")
  expect_s3_class(cpt_metrics_annotated(c(100, 200),
                                        annotations = list(c(100, 200)),
                                        n = 300), "tbl_df")
})

test_that("D15/D16/D17/D18: the registration doors validate what they store", {
  # ifelse() evaluates both arms, so find.package() ran for every planned
  # and registered row and the answers were then overwritten with NA.
  cpt_register_method("d15_probe", function(x, ...) as_ggcpt(50, x),
                      engine = NULL)
  withr::defer(try(cpt_unregister_method("d15_probe"), silent = TRUE))
  tab <- cpt_methods()
  expect_true(is.na(tab$installed[tab$method == "d15_probe"]))
  expect_true(all(is.na(tab$installed[tab$status != "available"])))

  # Flags are read through isTRUE(), so a non-logical registered as FALSE
  # and the capability silently disappeared.
  expect_error(cpt_register_method("d17_probe", function(x, ...) 50,
                                   capabilities = list(ci = 1)),
               "must be TRUE or FALSE")
  # A citation is cat()ed verbatim.
  expect_error(cpt_register_method("d18_probe", function(x, ...) 50,
                                   citation = list("a", "b")),
               "single string")
  # And the unregister door checked nothing.
  expect_error(cpt_unregister_method(42), "single non-empty string")
  expect_error(cpt_unregister_method(character(0)), "single non-empty string")
})

test_that("D19: an empty cpt_regions() has the columns the docs promise", {
  # rbind(cpt_regions(a), cpt_regions(b)) failed whenever one was empty and
  # the other indexed, because the empty return was always three columns.
  set.seed(42)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  d <- as.Date("2020-01-01") + seq_along(x) - 1
  plain <- cpt_detect(x, method = "pelt", index = d)
  expect_equal(nrow(cpt_regions(plain)), 0L)
  expect_true(all(c("start_index", "end_index") %in%
                    names(cpt_regions(plain))))
  withreg <- as_ggcpt(60, x, index = d,
                      regions = data.frame(start = 55, end = 66))
  expect_s3_class(rbind(cpt_regions(plain), cpt_regions(withreg)), "tbl_df")
})

test_that("D20: a location is validated against the shortest n", {
  # nmax meant `n = c(50, 500)` with `location = 400` passed against 500 and
  # was then silently clamped to 48 in the n = 50 scenario.
  expect_error(cpt_power(n = c(50, 500), jump = 2, location = 400, n_sim = 2),
               "shortest")
  expect_s3_class(cpt_power(n = c(50, 500), jump = 2, location = 0.5,
                            n_sim = 2, parallel = FALSE, seed = 1),
                  "ggcpt_power")
})

test_that("D25: a row subset of a subclassed tibble still prints", {
  # reclass_subset() keeps the subclass when the required COLUMNS survive,
  # and the tests asserted exactly that and never that the result prints --
  # which reads an attribute. Measured: tibble's `[` does carry the
  # attributes through a row subset here, so the guards are belt-and-braces
  # and this is the test that would notice if that changed.
  skip_on_cran()
  ds <- cpt_datasets()[1:2]
  bm <- suppressWarnings(cpt_benchmark(ds, methods = c("pelt", "binseg"),
                                       progress = FALSE))
  expect_s3_class(bm[1, ], "ggcpt_benchmark")
  expect_output(print(bm[1, ]))
  expect_output(print(bm[, names(bm)]))
  fit <- cpt_detect(c(stats::rnorm(80), stats::rnorm(80, 4)), method = "pelt")
  err <- cpt_label_error(fit, data.frame(start = c(1, 40), end = c(20, 100),
                                         change = c("no_change", "change")))
  expect_s3_class(err[1, ], "cpt_label_error")
  expect_output(print(err[1, ]))
})

test_that("D21: the CROPS penalty column is the interval's lower end", {
  # pen.value.full() returns the penalty-axis BREAKPOINTS, so K
  # segmentations come with K + 1 of them -- measured 23 against 22 rows on
  # a 240-point sweep. The old code reconciled the lengths blind; the
  # pairing it happened to keep is the correct one, and now says so.
  set.seed(2)
  x <- c(stats::rnorm(80), stats::rnorm(80, 3), stats::rnorm(80))
  p <- cpt_crops(x, pen_min = 2, pen_max = 200)
  sol <- p$solutions
  expect_true(nrow(sol) > 1)
  expect_false(anyNA(sol$penalty))
  # The invariant, whatever order the table is returned in: a larger
  # penalty buys fewer changepoints. Measured on this series -- 22
  # segmentations against 23 penalty breakpoints -- so an off-by-one in the
  # pairing would break the monotonicity somewhere.
  by_pen <- sol[order(sol$penalty), , drop = FALSE]
  expect_true(all(diff(by_pen$n_cpts) <= 0))
  expect_equal(min(sol$penalty), 2)
  expect_lt(max(sol$penalty), 200)
})

test_that("D30: a wide multivariate plot warns before it draws", {
  skip_if_not(engine_usable("InspectChangepoint"))
  # Measured: no wrapper actually produces p^2 coordinates (network_wrapper()
  # deliberately carries no data_wide, and hdcov takes an n x p matrix), and
  # 30 panels build in half a second -- so the cost is readability, not
  # time. Say so rather than drawing 30 unreadable slivers silently.
  set.seed(4)
  X <- matrix(stats::rnorm(80 * 30), nrow = 80)
  f <- suppressWarnings(cpt_detect(X, method = "inspect"))
  # A message, not a warning: "this will be hard to read" is advice about a
  # correct plot, and a 30-coordinate functional result is an ordinary
  # thing for fmean/fcov to produce.
  expect_message(ggplot2::ggplot_build(ggplot2::autoplot(f)), "coordinates")
  small <- matrix(stats::rnorm(80 * 4), nrow = 80)
  fs <- suppressWarnings(cpt_detect(small, method = "inspect"))
  expect_s3_class(ggplot2::ggplot_build(ggplot2::autoplot(fs)),
                  "ggplot_built")
})

test_that("E8: a registered method with an uppercase name can be cited", {
  # The registry is an environment, so its lookup is case-sensitive, and
  # cpt_cite() lowercased the name before looking there -- so cpt_cite() on
  # a "MyDetector" result told the user to supply a citation with
  # cpt_register_method(), which is exactly what they had done.
  cpt_register_method("MyDetector", function(x, ...) as_ggcpt(50, x),
                      citation = "Someone (2026). A detector.")
  withr::defer(cpt_unregister_method("MyDetector"))
  cited <- cpt_cite("MyDetector")
  expect_equal(nrow(cited), 1L)
  expect_match(cited$reference, "Someone")
  expect_equal(cited$method, "MyDetector")
  fit <- cpt_detect(stats::rnorm(100), method = "MyDetector")
  expect_match(cpt_cite(fit)$reference, "Someone")
})

test_that("E9: a short engine statistic is padded at both ends", {
  # A moving window trims BOTH ends, so left-aligning the pad shifted every
  # value left by the bandwidth -- the mis-alignment the branch exists to
  # prevent.
  n <- 101
  short <- c(rep(0, 20), 5, rep(0, 20))          # peak at position 21 of 41
  cpt_register_method("e9_probe", function(x, ...) {
    r <- as_ggcpt(50, x, method = "e9_probe")
    r$diagnostics <- list(statistic = list(statistic = short,
                                           label = "probe"))
    r
  }, capabilities = list(statistic = TRUE))
  withr::defer(cpt_unregister_method("e9_probe"))
  st <- cpt_statistic(cpt_detect(stats::rnorm(n), method = "e9_probe"))
  expect_equal(nrow(st), n)
  peak <- which.max(replace(st$statistic, is.na(st$statistic), -Inf))
  # Centred: 60 NAs to distribute, 30 before, so the peak lands at 30 + 21.
  expect_equal(peak, 30L + 21L)
  expect_equal(sum(is.na(st$statistic[seq_len(30)])), 30L)
})

test_that("B16: the bootstrap refuses a result it cannot reproduce", {
  # A `strucchange` formula fit reports change_in = "regression" and keeps
  # neither the formula nor `data` -- `$data$value` is the response alone.
  # So the bootstrap resampled the response and re-ran an INTERCEPT-ONLY
  # breakpoint search, then reported the spread of that search as this
  # result's interval. bootstrap_possible() said TRUE because the method is
  # in the registry.
  skip_if_not(engine_usable("strucchange"))
  set.seed(51)
  n <- 160
  x1 <- stats::rnorm(n)
  y <- c(stats::rnorm(80), stats::rnorm(80, 4)) + 2 * x1
  d <- data.frame(y = y, x1 = x1)
  f <- tryCatch(suppressWarnings(strucchange_wrapper(y ~ x1, data = d)),
                error = function(e) NULL)
  skip_if(is.null(f))
  skip_if(!identical(f$change_in, "regression"))
  expect_error(suppressWarnings(cpt_confint(f, method = "bootstrap", B = 5)),
               "re-run a different model")
  # The engine's own intervals are still available, and that is what `auto`
  # picks for this engine.
  expect_s3_class(cpt_confint(f, method = "native"), "tbl_df")
  # A plain change-in-mean strucchange fit is unaffected.
  g <- suppressWarnings(cpt_detect(c(stats::rnorm(80), stats::rnorm(80, 4)),
                                   method = "strucchange"))
  expect_s3_class(suppressWarnings(cpt_confint(g, method = "bootstrap",
                                               B = 5)), "tbl_df")
})

test_that("B12: a recomputed solution path says that it is one", {
  skip_if_not(engine_usable("breakfast"))
  # breakfast's fit keeps no candidate list, so this is the only branch that
  # recomputes -- and wbs2 is randomised, so the path is a second search of
  # the same series and need not contain the fit's own changepoints.
  set.seed(52)
  x <- c(stats::rnorm(120), stats::rnorm(120, 3))
  f <- suppressWarnings(cpt_detect(x, method = "wbs2"))
  expect_warning(sp <- cpt_solution_path(f), "recomputed")
  expect_true(all(c("step", "cp", "contrast", "selected") %in% names(sp)))
  # Every other engine's path is read off the fit, and stays silent.
  fb <- suppressWarnings(cpt_detect(x, method = "binseg", Q = 5))
  expect_silent(cpt_solution_path(fb))
})
