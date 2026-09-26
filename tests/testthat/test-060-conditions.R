# 0.6.0: typed conditions, version stamping and cpt_verify().

test_that("every condition kind resolves to a documented class chain", {
  errs <- ggchangepoint:::cpt_error_classes()
  for (k in names(errs)) {
    cls <- ggchangepoint:::cpt_condition_class(k, "error")
    expect_identical(utils::tail(cls, 3),
                     c("ggchangepoint_error", "error", "condition"),
                     info = k)
    expect_true(all(grepl("^ggchangepoint_", utils::head(cls, -2))),
                info = k)
  }
  warns <- ggchangepoint:::cpt_warning_classes()
  for (k in names(warns)) {
    cls <- ggchangepoint:::cpt_condition_class(k, "warning")
    expect_identical(utils::tail(cls, 3),
                     c("ggchangepoint_warning", "warning", "condition"),
                     info = k)
  }
  # The input-error family shares a parent a caller can catch once.
  for (k in c("short_series", "wrong_dimension", "non_finite", "bad_type",
              "bad_argument")) {
    expect_true("ggchangepoint_input_error" %in%
                  ggchangepoint:::cpt_condition_class(k, "error"), info = k)
  }

  # And every class a caller could meet is named on the help page, because
  # an undocumented class is as undiscoverable as an undocumented argument.
  skip_if_no_sources()
  rd <- paste(pkg_source_lines("man", "ggchangepoint-conditions.Rd"),
              collapse = " ")
  classes <- unique(c(unlist(errs), unlist(warns)))
  missing <- classes[!vapply(classes, grepl, logical(1), x = rd,
                             fixed = TRUE)]
  expect_identical(missing, character(0))
})

test_that("real refusals carry their class, their data and no call", {
  catch <- function(expr) tryCatch(expr, error = function(e) e)

  e <- catch(cpt_detect(c(1, 2), method = "pelt"))
  expect_s3_class(e, "ggchangepoint_short_series")
  expect_s3_class(e, "ggchangepoint_input_error")
  expect_s3_class(e, "ggchangepoint_error")
  expect_null(conditionCall(e))

  e <- catch(cpt_detect(factor(rep(c("a", "b"), each = 20))))
  expect_s3_class(e, "ggchangepoint_bad_type")

  e <- catch(cpt_detect(c(stats::rnorm(20), NA, stats::rnorm(20))))
  expect_s3_class(e, "ggchangepoint_non_finite")

  e <- catch(cpt_detect(stats::rnorm(50), method = "no_such_method"))
  expect_s3_class(e, "ggchangepoint_error")
  expect_null(conditionCall(e))

  e <- catch(cpt_detect(stats::rnorm(50), method = "pelt",
                        change_in = "network"))
  expect_s3_class(e, "ggchangepoint_unsupported")
  expect_identical(e$method, "pelt")
  expect_identical(e$requested, "network")
  expect_true("mean" %in% e$supported)

  e <- catch(cpt_detect(matrix(stats::rnorm(100), ncol = 2),
                        method = "pelt"))
  expect_s3_class(e, "ggchangepoint_wrong_dimension")

  # The message is unchanged by the class: the text tests still hold.
  expect_error(cpt_detect(c(1, 2), method = "pelt"), "at least 3")
})

test_that("a missing engine is classed and names what to install", {
  local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base"
  )
  e <- tryCatch(ggchangepoint:::need_pkg("wbs"), error = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_missing")
  expect_identical(e$package, "wbs")
  expect_match(e$install, "wbs")
})

test_that("a missing engine's error gains the installed alternatives", {
  # The same error whether the engine was never installed or CRAN archived
  # it: the analysis should not stop at "install this".
  e0 <- tryCatch(ggchangepoint:::cpt_abort("Package 'x' is required.",
                                           class = "engine_missing",
                                           data = list(package = "x")),
                 error = function(e) e)
  e <- tryCatch(ggchangepoint:::abort_with_alternatives(e0, "fpop", "mean"),
                error = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_missing")
  expect_identical(e$package, "x")
  expect_true("pelt" %in% e$alternatives)
  expect_false("fpop" %in% e$alternatives)
  expect_match(conditionMessage(e), "Installed methods that detect a change")
})

test_that("cpt_detect() adds the alternatives to a missing engine's error", {
  # Dispatches to the wbs wrapper, whose engine the mock then "loses".
  skip_if_not(engine_usable("wbs"))
  local_mocked_bindings(
    need_pkg = function(pkg) {
      ggchangepoint:::cpt_abort("Package '", pkg, "' is required.",
                                class = "engine_missing",
                                data = list(package = pkg, install = "x"))
    })
  e <- tryCatch(cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 3)),
                           method = "wbs"),
                ggchangepoint_engine_missing = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_missing")
  expect_identical(e$package, "wbs")
  expect_true("pelt" %in% e$alternatives)
  expect_false("wbs" %in% e$alternatives)
  expect_match(conditionMessage(e), "Installed methods that detect a change")
  expect_null(conditionCall(e))
})

test_that("an engine failure is classed and keeps the engine's call", {
  skip_if_not(engine_usable("wbs"))
  local_mocked_bindings(
    wbs_wrapper = function(x, ...) log(list())
  )
  set.seed(1)
  e <- tryCatch(cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 3)),
                           method = "wbs"),
                error = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_error")
  expect_identical(e$method, "wbs")
  expect_identical(e$engine, "wbs")
  # The call stays, so the provenance sweep can still tell a leak from a
  # deliberate refusal, and the original condition rides along.
  expect_false(is.null(conditionCall(e)))
  expect_s3_class(e$parent, "error")
  expect_false(inherits(e$parent, "ggchangepoint_error"))
})

test_that("a registered detector's failure is an engine error either way", {
  on.exit(try(cpt_unregister_method("cond_boom"), silent = TRUE),
          add = TRUE)
  x <- c(stats::rnorm(40), stats::rnorm(40, 3))
  cpt_register_method("cond_boom", function(x, ...) stop("deliberate",
                                                         call. = FALSE))
  e <- tryCatch(cpt_detect(x, method = "cond_boom"), error = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_error")
  expect_null(conditionCall(e))
  expect_match(conditionMessage(e), "deliberate")

  cpt_register_method("cond_boom", function(x, ...) log(-"a"),
                      overwrite = TRUE)
  e <- tryCatch(cpt_detect(x, method = "cond_boom"), error = function(e) e)
  expect_s3_class(e, "ggchangepoint_engine_error")
  expect_match(conditionMessage(e), "registered for `cond_boom` failed")
})

test_that("fan-outs record method failures and raise everything else", {
  on.exit(try(cpt_unregister_method("cond_fails"), silent = TRUE),
          add = TRUE)
  cpt_register_method("cond_fails", function(x, ...) stop("engine down"))
  set.seed(3)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  ds <- list(a = list(series = x, truth = 60))

  # a method failure is a result...
  bm <- cpt_benchmark(ds, methods = c("pelt", "cond_fails"),
                      progress = FALSE)
  expect_match(bm$error[bm$method == "cond_fails"], "engine down")
  expect_true(is.na(bm$error[bm$method == "pelt"]))
  expect_warning(
    cons <- cpt_consensus(x, methods = c("pelt", "binseg", "cond_fails")),
    class = "ggchangepoint_engine_failed")
  expect_s3_class(cons, "ggcpt")

  # ...a bad argument every method would reject is not, and neither is an
  # unclassed error, which can only be a bug here.
  expect_error(cpt_benchmark(ds, methods = c("pelt", "binseg"),
                             progress = FALSE, penalty = -1),
               class = "ggchangepoint_bad_argument")
  expect_error(cpt_benchmark(ds, methods = c("pelt", "binseg"),
                             progress = FALSE, penalty = -1),
               "Dataset `a`, method `pelt`")
  expect_error(cpt_consensus(x, methods = c("pelt", "binseg"),
                             penalty = -1),
               class = "ggchangepoint_bad_argument")
  unclassed <- simpleError("a bug")
  expect_error(ggchangepoint:::fanout_failure(unclassed, "ctx: "),
               "^a bug$")
  expect_false(inherits(
    tryCatch(ggchangepoint:::fanout_failure(unclassed), error = identity),
    "ggchangepoint_error"))
})

test_that("classed warnings can be caught and muffled by class", {
  set.seed(4)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  d <- as.Date("2020-01-01") + sort(sample(0:400, 120))
  expect_warning(cpt_detect(x, index = d),
                 class = "ggchangepoint_irregular_index")
  # suppressing one class leaves the rest alone
  fit <- withCallingHandlers(
    cpt_detect(x, index = d),
    ggchangepoint_irregular_index = function(w) invokeRestart("muffleWarning"))
  expect_s3_class(fit, "ggcpt")
  expect_warning(as_ggcpt(c(0, 30, 500), x),
                 class = "ggchangepoint_cp_dropped")
})

test_that("a result records the versions that produced it", {
  set.seed(5)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  fit <- cpt_detect(x, method = "pelt")
  v <- fit$versions
  expect_identical(v$engine, "changepoint")
  expect_identical(v$engine_version,
                   as.character(utils::packageVersion("changepoint")))
  expect_identical(v$ggchangepoint,
                   as.character(utils::packageVersion("ggchangepoint")))
  expect_match(v$r, "^[0-9]+\\.[0-9]+")
  expect_identical(glance(fit)$engine_version, v$engine_version)

  # an empty result is stamped too
  # (Noise this far below unit scale draws the scale warning, rightly.)
  flat <- suppressWarnings(cpt_detect(rep(c(0, 0.001), 60), method = "pelt"))
  expect_false(is.null(flat$versions$engine_version))

  # A version that no longer matches is noted in print(), and only then.
  expect_false(any(grepl("Engine version", utils::capture.output(print(fit)))))
  old <- fit
  old$versions$engine_version <- "0.0.1"
  out <- utils::capture.output(print(old))
  expect_true(any(grepl("changepoint 0.0.1 made this result", out,
                        fixed = TRUE)))
})

test_that("a registered method is stamped with its registration's engine", {
  on.exit(try(cpt_unregister_method("cond_ver"), silent = TRUE), add = TRUE)
  cpt_register_method("cond_ver", function(x, ...) 40L, engine = "stats")
  fit <- cpt_detect(c(stats::rnorm(40), stats::rnorm(40, 3)),
                    method = "cond_ver")
  expect_identical(fit$versions$engine, "stats")
  expect_identical(fit$versions$engine_version,
                   as.character(utils::packageVersion("stats")))
})

test_that("cpt_verify() re-runs a result and reports what moved", {
  set.seed(6)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4), stats::rnorm(80, -2))
  fit <- cpt_detect(x, method = "binseg", penalty = "BIC", Q = 4)
  ver <- cpt_verify(fit)
  expect_s3_class(ver, "ggcpt_verification")
  expect_true(ver$verified)
  expect_identical(ver$then, ver$now)
  expect_false(any(ver$versions$changed))
  expect_output(print(ver), "reproduces")
  # the literal engine argument was replayed, not dropped
  expect_identical(ver$refit$call$Q, 4)

  # a result whose recorded locations differ is reported as changed
  moved <- fit
  moved$changepoints$cp <- moved$changepoints$cp + 7L
  ver2 <- cpt_verify(moved)
  expect_false(ver2$verified)
  expect_length(ver2$added, length(fit$changepoints$cp))
  expect_output(print(ver2), "CHANGED")
  expect_true(cpt_verify(moved, tolerance = 7)$verified)

  # an argument written as a variable is listed, not silently lost
  q <- 4
  fit3 <- cpt_detect(x, method = "binseg", Q = q)
  expect_identical(cpt_verify(fit3)$not_recovered, "Q")
  expect_output(print(cpt_verify(fit3)), "Not replayed")

  # someone else's changepoints cannot be re-run
  foreign <- as_ggcpt(c(80, 160), x)
  expect_error(cpt_verify(foreign), class = "ggchangepoint_capability_absent")
  expect_error(cpt_verify(1:3), class = "ggchangepoint_bad_argument")
})

test_that("a mistyped choice names the argument and suggests the fix", {
  set.seed(7)
  x <- c(stats::rnorm(50), stats::rnorm(50, 3))
  e <- tryCatch(cpt_detect(x, method = "PELT"), error = identity)
  expect_s3_class(e, "ggchangepoint_unknown_method")
  msg <- conditionMessage(e)
  expect_match(msg, "`method = \"PELT\"`", fixed = TRUE)
  expect_match(msg, "Did you mean \"pelt\"?", fixed = TRUE)
  expect_match(msg, "cpt_methods()", fixed = TRUE)
  expect_identical(e$suggestion, "pelt")
  # a handful of names, not all fifty
  expect_lt(lengths(regmatches(msg, gregexpr("\"[a-z0-9_.]+\"", msg))), 8)
  # the typos measured in the roadmap each land on the intended method
  typos <- c(pelts = "pelt", binsegs = "binseg", smucee = "smuce")
  for (t in names(typos)) {
    e <- tryCatch(cpt_detect(x, method = t), error = identity)
    expect_match(conditionMessage(e), paste0("\"", typos[[t]], "\""),
                 fixed = TRUE, info = t)
  }
  # a registered method is suggested too
  on.exit(try(cpt_unregister_method("my_detector"), silent = TRUE),
          add = TRUE)
  cpt_register_method("my_detector", function(x, ...) 50L)
  e <- tryCatch(cpt_detect(x, method = "my_detectr"), error = identity)
  expect_match(conditionMessage(e), "Did you mean \"my_detector\"?",
               fixed = TRUE)

  # A short choice list is shown whole, with the suggestion.
  e <- tryCatch(cpt_detect(x, change_in = "maen"), error = identity)
  expect_s3_class(e, "ggchangepoint_bad_argument")
  expect_match(conditionMessage(e), "`change_in = \"maen\"`", fixed = TRUE)
  expect_match(conditionMessage(e), "Did you mean \"mean\"?", fixed = TRUE)
  # match.arg()'s semantics are kept: partial matching, the default vector
  # meaning its first element, and an ambiguous prefix refused as such.
  expect_s3_class(cpt_detect(x, change_in = "meanv"), "ggcpt")
  set.seed(1)
  a <- cpt_simulate(50, changepoints = 25)
  set.seed(1)
  b <- cpt_simulate(50, changepoints = 25, change_in = "mean")
  expect_identical(a$value, b$value)
  e <- tryCatch(cpt_detect(x, change_in = "me"), error = identity)
  expect_match(conditionMessage(e), "matches more than one value")
  expect_error(cpt_detect(x, change_in = 1), class = "ggchangepoint_bad_argument")
  expect_error(cpt_detect(x, change_in = c("mean", "var")),
               "must be one value")
})
