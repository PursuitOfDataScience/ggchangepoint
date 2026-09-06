# 0.5.0: the engine registry (one declarative source of truth) and the
# extension mechanism built on the same code path.

set.seed(2026)
x_step <- c(rnorm(80), rnorm(80, 4))

# NA-safe TRUE test: the capability flags are NA for planned methods.
is_true_flag <- function(v) !is.na(v) & v

test_that("the registry is the single source of the method tables", {
  reg <- ggchangepoint:::builtin_registry()
  expect_true(all(c("method", "change_in", "engine", "supports", "wrapper",
                    "multivariate", "online", "ci", "fitted", "posterior",
                    "statistic", "path", "scale_space") %in% names(reg)))
  expect_false(anyDuplicated(reg$method) > 0)
  # `supports` must be a list of character vectors, not deparsed strings --
  # the tribble list-column trap that silently broke every multi-capability
  # method's change_in validation.
  expect_type(reg$supports, "list")
  expect_true(all(vapply(reg$supports, is.character, logical(1))))
  expect_true(all(vapply(reg$supports, function(s) all(nzchar(s)),
                         logical(1))))
  expect_setequal(ggchangepoint:::method_change_in_support()[["pelt"]],
                  c("mean", "var", "meanvar"))
  # Every declared support level must be one the dispatcher accepts.
  expect_true(all(unlist(reg$supports) %in%
                    ggchangepoint:::cpt_change_in_levels()))
  # Every wired method must name a wrapper that exists.
  for (w in unique(reg$wrapper)) {
    expect_true(exists(w, envir = asNamespace("ggchangepoint")),
                info = w)
  }
})

test_that("cpt_methods reports capabilities and installation status", {
  tab <- cpt_methods()
  expect_s3_class(tab, "tbl_df")
  expect_true(all(c("method", "change_in", "engine", "status", "installed",
                    "target_release", "ci", "fitted", "posterior",
                    "statistic", "path", "scale_space") %in% names(tab)))
  expect_true(all(tab$status %in% c("available", "planned", "registered")))
  expect_true(all(is.na(tab$installed[tab$status != "available"])))
  # The compact form drops the flags but keeps the 0.4.0 columns.
  compact <- cpt_methods(capabilities = FALSE)
  expect_false("ci" %in% names(compact))
  expect_true(all(c("method", "change_in", "engine", "status", "installed",
                    "target_release") %in% names(compact)))
  # Capability claims must be self-consistent with the wrappers.
  expect_true(all(c("smuce", "hsmuce", "strucchange", "segmented") %in%
                    tab$method[is_true_flag(tab$ci)]))
})

test_that("planned methods are named, not denied", {
  pl <- ggchangepoint:::planned_methods()
  expect_true(all(pl$target_release == "when on CRAN"))
  expect_error(cpt_detect(x_step, method = "gfpop"), "planned but not wired")
  expect_error(cpt_detect(x_step, method = "sbs"), "hdbinseg")
  # hdbinseg went back into the CRAN archive after the 0.5.0 roadmap was
  # written, so `sbs` waits on CRAN like the rest rather than on a wrapper.
  expect_error(cpt_detect(x_step, method = "sbs"),
               "being available from CRAN")
})

test_that("as_ggcpt enforces the same contract as a wrapper", {
  fit <- as_ggcpt(c(40, 40, 200, NA, 20), x_step, method = "custom")
  expect_s3_class(fit, "ggcpt")
  # sorted, de-duplicated, in range, NA dropped
  expect_equal(fit$changepoints$cp, c(20L, 40L))
  expect_equal(nrow(fit$segments), 3)
  expect_equal(fit$cp_convention, "left")

  right <- as_ggcpt(41, x_step, cp_convention = "right")
  expect_equal(right$changepoints$cp, 40L)

  with_ci <- as_ggcpt(40, x_step, ci = cbind(35, 45))
  expect_equal(with_ci$changepoints$ci_lower, 35L)
  expect_error(as_ggcpt(40, x_step, ci = cbind(1, 2, 3)), "two columns")
  expect_error(as_ggcpt(c(40, 50), x_step, ci = cbind(35, 45)),
               "one row per changepoint")
  expect_error(as_ggcpt(40, x_step, extra = list(a = c(1, 2))),
               "one value per changepoint")
  expect_error(as_ggcpt(40, x_step, method = ""), "non-empty string")
})

test_that("as_ggcpt carries regions, a fitted signal and an index", {
  fit <- as_ggcpt(50, x_step, fitted = rep(0, length(x_step)),
                  regions = data.frame(start = 45, end = 60),
                  index = as.Date("2020-01-01") + seq_along(x_step) - 1)
  expect_true("fitted" %in% names(fit$data))
  expect_equal(nrow(cpt_regions(fit)), 1)
  expect_equal(cpt_regions(fit)$length, 16L)
  expect_s3_class(fit$changepoints$cp_index, "Date")
  expect_true(all(c("start_index", "end_index") %in% names(fit$regions)))
  # A reversed interval is a slip, not a different meaning.
  flipped <- as_ggcpt(50, x_step, regions = data.frame(start = 60, end = 45))
  expect_equal(flipped$regions$start, 45L)
  expect_equal(flipped$regions$end, 60L)
})

test_that("registration round-trips and is visibly user-supplied", {
  on.exit(try(cpt_unregister_method("toy"), silent = TRUE), add = TRUE)
  cpt_register_method("toy", fn = function(x, ...) which.max(abs(diff(x))),
                      engine = "example", citation = "Nobody (2026).")
  expect_true("toy" %in% cpt_registered_methods()$method)
  tab <- cpt_methods()
  expect_equal(tab$status[tab$method == "toy"], "registered")
  expect_true(is.na(tab$installed[tab$method == "toy"]))

  fit <- cpt_detect(x_step, method = "toy")
  expect_s3_class(fit, "ggcpt")
  expect_true(isTRUE(fit$registered))
  expect_equal(fit$method, "toy")
  expect_output(print(fit), "user-registered")
  expect_output(cpt_cite("toy"), "Nobody")

  cpt_unregister_method("toy")
  expect_false("toy" %in% cpt_registered_methods()$method)
  expect_error(cpt_unregister_method("toy"), "not a registered method")
})

test_that("registration refuses to shadow or mislabel", {
  expect_error(cpt_register_method("pelt", function(x, ...) 1),
               "built-in method")
  expect_error(cpt_register_method("gfpop", function(x, ...) 1),
               "planned built-in")
  expect_error(cpt_register_method("bad", "not a function"),
               "must be a function")
  expect_error(cpt_register_method("bad", function(x, ...) 1,
                                   change_in = "colour"),
               "Unknown `change_in`")
  expect_error(cpt_register_method("bad", function(x, ...) 1,
                                   capabilities = list(wings = TRUE)),
               "Unknown capability")

  on.exit(try(cpt_unregister_method("dup"), silent = TRUE), add = TRUE)
  cpt_register_method("dup", function(x, ...) 10)
  expect_error(cpt_register_method("dup", function(x, ...) 11),
               "already registered")
  expect_silent(cpt_register_method("dup", function(x, ...) 11,
                                    overwrite = TRUE))
})

test_that("a registered method's contract violations are caught", {
  on.exit(try(cpt_unregister_method("bad"), silent = TRUE), add = TRUE)
  cpt_register_method("bad", function(x, ...) "not changepoints")
  expect_error(cpt_detect(x_step, method = "bad"),
               "must return a ggcpt object or a numeric vector")
  cpt_unregister_method("bad")

  cpt_register_method("bad", function(x, ...) 40, change_in = "mean")
  # The capability check runs before dispatch, so a registered method gets
  # the same message a built-in one would.
  expect_error(cpt_detect(x_step, method = "bad", change_in = "var"),
               "is not supported for method `bad`")
  entry <- ggchangepoint:::registry_get("bad")
  expect_error(
    ggchangepoint:::run_registered_method(entry, x_step, change_in = "var",
                                          penalty = "MBIC"),
    "not supported by the registered method"
  )
})

test_that("a registration may return a finished ggcpt", {
  on.exit(try(cpt_unregister_method("full"), silent = TRUE), add = TRUE)
  cpt_register_method(
    "full",
    fn = function(x, ...) as_ggcpt(40, x, method = "whatever-it-calls-itself")
  )
  fit <- cpt_detect(x_step, method = "full")
  # The registration's name wins, so cpt_methods() and the result agree.
  expect_equal(fit$method, "full")
  expect_true(isTRUE(fit$registered))
})

test_that("engine bundles resolve to real packages", {
  bundles <- ggchangepoint:::engine_bundles()
  expect_true(all(lengths(bundles) > 0))
  reg <- ggchangepoint:::builtin_registry()
  engine_pkgs <- unique(reg$engine)
  in_bundles <- unique(unlist(bundles))
  # Every Suggests engine must be reachable through some bundle, or
  # cpt_install_engines() cannot install it.
  missing_from_bundles <- setdiff(
    setdiff(engine_pkgs, c("changepoint", "changepoint.np", "ecp")),
    in_bundles
  )
  expect_equal(missing_from_bundles, character(0))
  dry <- cpt_install_engines("core", dry_run = TRUE)
  expect_true(all(c("package", "bundle", "installed_before",
                    "installed_after") %in% names(dry)))
})

test_that("cpt_methods is silent even when an engine talks on load", {
  # Asking whether an engine is installed loads its namespace, and a
  # namespace may talk: `fabisearch` pulls in `rgl`, which warns
  # "unable to open X11 display" on every headless machine. Reporting an
  # installation status must not make the console noisy.
  expect_silent(cpt_methods())
  expect_silent(cpt_methods(capabilities = FALSE))
  expect_silent(cpt_recommend())
  # cpt_install_engines() reports what it would do -- that message is the
  # point of a dry run -- but it must not also leak an engine's load noise.
  expect_warning(invisible(cpt_install_engines("core", dry_run = TRUE)), NA)
})

test_that("as_ggcpt() drops what it documents and refuses what it cannot read", {
  set.seed(3)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  n <- length(x)

  # The documented contract: out-of-range, duplicated and missing values are
  # dropped and the result is sorted. All of this must keep working.
  expect_equal(as_ggcpt(c(90, 30), x)$changepoints$cp, c(30L, 90L))
  expect_equal(as_ggcpt(c(60, 60), x)$changepoints$cp, 60L)
  expect_equal(as_ggcpt(c(60, NA), x)$changepoints$cp, 60L)
  expect_equal(as_ggcpt(c(0, -5, 60, n, 500), x)$changepoints$cp, 60L)
  expect_equal(as_ggcpt(60.5, x)$changepoints$cp, 60L)
  expect_equal(nrow(as_ggcpt(integer(0), x)$changepoints), 0L)
  expect_equal(nrow(as_ggcpt(NULL, x)$changepoints), 0L)
  expect_equal(as_ggcpt("60", x)$changepoints$cp, 60L)

  # What it must NOT do is swallow the coercion warning and report a clean
  # "no changepoints" for input it could not read.
  expect_error(as_ggcpt(c("a", "b"), x), "are not numbers")
  expect_error(as_ggcpt(c("60", "x"), x), "are not numbers")
  # as.integer() on a factor returns level codes: factor(c("60", "90"))
  # used to become changepoints at 1 and 2.
  expect_error(as_ggcpt(factor(c("60", "90")), x), "level codes")
  # and a logical vector is a mask, not a set of positions
  expect_error(as_ggcpt(c(TRUE, FALSE), x), "which\\(cp\\)")
})

test_that("as_ggcpt() reports a wrong-length fitted signal", {
  set.seed(3)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  # ggcpt_build() drops a wrong-length fitted signal silently, after which
  # autoplot(show_fit = TRUE) says the result "carries no fitted signal" --
  # about a signal the caller supplied. Every sibling slot (index, ci,
  # regions, extra) reports its length mismatch, so this one does too.
  expect_error(as_ggcpt(60, x, fitted = stats::rnorm(10)),
               "one value per observation")
  ok <- as_ggcpt(60, x, fitted = stats::rnorm(length(x)))
  expect_true("fitted" %in% names(ok$data))
  expect_length(ok$data$fitted, length(x))
})

test_that("ci and extra columns follow their changepoint through the drop", {
  set.seed(3)
  x <- c(stats::rnorm(60), stats::rnorm(60, 4))
  # ci/extra are validated against the SUPPLIED cp vector and the dropping
  # happens afterwards, so the columns have to be filtered and reordered in
  # step with cp or they end up describing a different changepoint.
  front <- as_ggcpt(c(0, 60), x, ci = cbind(c(-5, 55), c(5, 65)),
                    extra = list(score = c(111, 222)))
  expect_equal(front$changepoints$cp, 60L)
  expect_equal(front$changepoints$score, 222)
  expect_equal(front$changepoints$ci_lower, 55L)

  back <- as_ggcpt(c(60, 500), x, extra = list(score = c(111, 222)))
  expect_equal(back$changepoints$score, 111)

  sorted <- as_ggcpt(c(90, 30), x, extra = list(score = c(999, 111)))
  expect_equal(sorted$changepoints$cp, c(30L, 90L))
  expect_equal(sorted$changepoints$score, c(111, 999))
})

test_that("every engine that declares a fitted signal delivers a full one", {
  # ggcpt_build() keeps `fitted` only when its length matches the series, so
  # a wrapper that returned a short signal would advertise the capability in
  # cpt_methods() and quietly not have it.
  set.seed(5)
  x <- c(stats::rnorm(150), stats::rnorm(150, 3))
  reg <- ggchangepoint:::builtin_registry()
  # Same reason as R23 in test-040-bugfixes.R: every engine that declares a
  # fitted signal is a Suggests, so without them this ran no expectations
  # at all and still passed.
  tested <- 0L
  for (m in reg$method[reg$fitted]) {
    # `mcp` is installed on the Windows runner while JAGS is not, so it
    # warns "Returning an `mcpfit` without samples" on the way to the error
    # the wrapper raises. Muffled by text so the suite report stays about
    # this package; the error itself still lands in the tryCatch below.
    fit <- withCallingHandlers(
      tryCatch(cpt_detect(x, method = m), error = function(e) NULL),
      warning = function(w) {
        if (grepl("without samples|JAGS failed", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      })
    if (is.null(fit)) next          # engine not installed or not runnable
    tested <- tested + 1L
    expect_true("fitted" %in% names(fit$data), info = m)
    expect_length(fit$data$fitted, length(x))
  }
  skip_if(tested == 0L, "no engine declaring a fitted signal is installed")
})

test_that("a registered method must detect on the series it is given", {
  set.seed(9)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4))
  withr::defer(try(cpt_unregister_method("shapeprobe"), silent = TRUE))

  # A returned ggcpt used to be taken entirely on trust, so a function that
  # built its result from some other series handed back a result whose
  # $data, row count and n were about that series, not about `x`.
  cpt_register_method(
    "shapeprobe",
    fn = function(x, ...) as_ggcpt(20, stats::rnorm(40), method = "inner")
  )
  expect_error(cpt_detect(x, method = "shapeprobe"),
               "result for a different series")

  # the honest version is accepted, and the registration's name wins
  cpt_register_method(
    "shapeprobe",
    fn = function(x, ...) as_ggcpt(80, x, method = "inner"),
    overwrite = TRUE
  )
  fit <- cpt_detect(x, method = "shapeprobe")
  expect_equal(nrow(fit$data), length(x))
  expect_equal(fit$method, "shapeprobe")
  expect_true(isTRUE(fit$registered))

  # and a multivariate result is measured in observations, not coordinates
  X <- cbind(a = x, b = x)
  cpt_register_method(
    "shapeprobe",
    fn = function(x, ...) as_ggcpt(80, x, method = "inner"),
    capabilities = list(multivariate = TRUE), overwrite = TRUE
  )
  mv <- cpt_detect(X, method = "shapeprobe")
  expect_equal(nrow(mv$data), nrow(X))
})

test_that("dispatch refuses every return shape that is not changepoints", {
  set.seed(9)
  x <- c(stats::rnorm(80), stats::rnorm(80, 4))
  withr::defer(try(cpt_unregister_method("retprobe"), silent = TRUE))

  bad <- list(
    NULL_ = function(x, ...) NULL,
    character = function(x, ...) "80",
    factor = function(x, ...) factor(c("40", "80")),
    logical = function(x, ...) seq_along(x) == 80,
    list = function(x, ...) list(cp = 80),
    frame = function(x, ...) data.frame(cp = 80)
  )
  for (nm in names(bad)) {
    cpt_register_method("retprobe", fn = bad[[nm]], overwrite = TRUE)
    expect_error(cpt_detect(x, method = "retprobe"),
                 "must return a ggcpt object", info = nm)
  }

  # and the shapes that ARE changepoints go through as_ggcpt()'s contract
  for (fn in list(function(x, ...) 80L, function(x, ...) 80.4,
                  function(x, ...) c(0L, 80L, 500L))) {
    cpt_register_method("retprobe", fn = fn, overwrite = TRUE)
    fit <- cpt_detect(x, method = "retprobe")
    expect_equal(fit$changepoints$cp, 80L)
  }
})

test_that("an engine argument the wrapper manages is refused by name", {
  # Every wrapper forwards `...` to its engine, and eight of them also pin
  # one of that engine's own arguments. Passing one of those through `...`
  # reached R's argument matcher and stopped with "formal argument
  # \"verbose\" matched by multiple actual arguments" -- a message that
  # names neither the wrapper, nor the engine, nor what to do instead. A
  # sweep of the registry found twelve such pairs. The pins themselves are
  # deliberate (they are what make `tguh` tguh, what makes SMUCE's
  # intervals extractable at all, and what stops SNSeg drawing to the
  # device), so the fix is to refuse clearly rather than to honour them.
  managed <- list(
    list(m = "wbs2",   eng = "breakfast",    a = "solution.path"),
    list(m = "wbs2",   eng = "breakfast",    a = "model.selection"),
    list(m = "tguh",   eng = "breakfast",    a = "solution.path"),
    list(m = "tguh",   eng = "breakfast",    a = "model.selection"),
    list(m = "smuce",  eng = "stepR",        a = "jumpint"),
    list(m = "bocpd",  eng = "ocp",          a = "getR"),
    list(m = "beast",  eng = "Rbeast",       a = "season"),
    list(m = "beast",  eng = "Rbeast",       a = "quiet"),
    list(m = "beast",  eng = "Rbeast",       a = "print.progress"),
    list(m = "decafs", eng = "DeCAFS",       a = "warningMessage"),
    list(m = "sn",     eng = "SNSeg",        a = "plot_SN"),
    list(m = "envcpt", eng = "EnvCpt",       a = "verbose")
  )
  # Every engine involved here is a Suggests one, so against an
  # Imports-only library there is nothing to sweep and "tested nothing" is
  # a true observation rather than a regression. Skip instead of asserting
  # -- the R-devel run learned this the hard way.
  if (!any(vapply(unique(vapply(managed, function(z) z$eng, character(1))),
                  engine_installed, logical(1)))) {
    skip("none of the engines with a managed argument is installed")
  }
  set.seed(7)
  y <- c(rnorm(50), rnorm(50, 5))
  tested <- 0L
  for (z in managed) {
    if (!engine_installed(z$eng)) next
    tested <- tested + 1L
    args <- list(y, method = z$m); args[[z$a]] <- TRUE
    err <- tryCatch({ do.call(cpt_detect, args); NULL },
                    error = function(e) conditionMessage(e))
    expect_true(!is.null(err), info = paste(z$m, z$a, "did not error at all"))
    # the raw R message must not be what the user sees
    expect_false(grepl("matched by multiple actual arguments", err),
                 info = paste(z$m, z$a))
    # and ours must name both the method and the argument
    expect_match(err, paste0("`", z$m, "` sets `",
                             gsub("\\.", "\\\\.", z$a), "` itself"),
                 info = paste(z$m, z$a))
  }
  expect_gt(tested, 0L)

  # An argument the wrapper does NOT manage still reaches the engine.
  skip_if_not_installed("EnvCpt")
  expect_s3_class(
    suppressWarnings(cpt_detect(y, method = "envcpt", minseglen = 10)),
    "ggcpt")
})

test_that("hdcov survives the single-level BS tree that killed thresholdBS", {
  skip_if_not_installed("changepoints")
  # changepoints::thresholdBS.BS() prunes with
  #   for (i in 2:level_length) ... 1:table(BS_object$Level)[i]
  # so a one-level tree makes that `2:1`, the body runs with i = 2,
  # table(...)[2] is NA, and the call dies with base R's "NA/NaN argument".
  # BS.cov() returns one level whenever the series is short relative to the
  # number of coordinates, which is not a corner case: over 25 seeds per
  # cell, hdcov_wrapper() failed on 11/25 runs at n = 200, p = 10, 23/25 at
  # n = 120, p = 8 and 24/25 at n = 400, p = 20 -- an intermittent opaque
  # error on exactly the shapes a high-dimensional covariance method is for.
  mk <- function(n, p, sd0) {
    set.seed(sd0)
    h <- floor(n / 2)
    X <- matrix(stats::rnorm(n * p), n, p)
    X[(h + 1):n, ] <- X[(h + 1):n, ] * 3
    colnames(X) <- paste0("v", seq_len(p))
    X
  }

  # a shape that reliably produces the single-level tree
  X <- mk(120, 8, 1)
  bs <- changepoints::BS.cov(t(X), 1, nrow(X))
  expect_length(unique(bs$Level), 1L)
  # upstream still cannot be called on it -- this is the bug being routed
  # around, so if it is ever fixed this expectation is the signal
  expect_error(changepoints::thresholdBS(bs, 50), "NA/NaN")

  # ...and the wrapper nonetheless returns a result
  res <- suppressWarnings(hdcov_wrapper(X, seed = 1))
  expect_s3_class(res, "ggcpt")
  expect_gte(nrow(res$changepoints), 1L)
  expect_true(all(res$changepoints$cp >= 1 &
                  res$changepoints$cp <= nrow(X) - 1L))

  # across seeds and shapes, no failures at all
  fails <- 0L
  for (cfg in list(c(120, 8), c(200, 10), c(80, 5))) {
    for (sd0 in 1:4) {
      ok <- tryCatch({
        suppressWarnings(hdcov_wrapper(mk(cfg[1], cfg[2], sd0), seed = sd0))
        TRUE
      }, error = function(e) FALSE)
      if (!ok) fails <- fails + 1L
    }
  }
  expect_equal(fails, 0L)

  # a multi-level tree must still go through thresholdBS unchanged: the
  # shared helper has to be identical() to upstream there, not merely
  # equivalent
  Y <- mk(300, 10, 1)
  bs2 <- changepoints::BS.cov(t(Y), 1, nrow(Y))
  expect_gt(length(unique(bs2$Level)), 1L)
  expect_identical(ggchangepoint:::threshold_bs(bs2, 60)$cpt_hat,
                   changepoints::thresholdBS(bs2, 60)$cpt_hat)

  # `network` calls the same upstream function and had the same failure:
  # measured at 10 of 12 runs for a 20-point sequence of 4-node graphs,
  # which is why the route-around is a shared helper rather than a fix in
  # one wrapper.
  mknet <- function(n, p, sd0) {
    set.seed(sd0)
    h <- floor(n / 2)
    rbind(matrix(stats::rbinom(h * p * p, 1, 0.2), nrow = h),
          matrix(stats::rbinom((n - h) * p * p, 1, 0.6), nrow = n - h))
  }
  net_fails <- 0L
  for (sd0 in 1:6) {
    ok <- tryCatch({
      suppressWarnings(network_wrapper(mknet(20, 4, sd0), seed = sd0))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) net_fails <- net_fails + 1L
  }
  expect_equal(net_fails, 0L)
})

test_that("wbsts reports more than one changepoint on modern R", {
  skip_if_not_installed("wbsts")
  # wbsts::wbs.lsw() ends with
  #   suppressWarnings(if (is.na(OUT)) OUT = NULL)
  # and OUT is the post-processed breakpoint set. Since R 4.2 an `if` on a
  # length > 1 condition is an error, not a warning, so that line fails
  # exactly when the method keeps two or more changepoints -- i.e. whenever
  # it would report more than one. Measured before the fix on R 4.4.1: 19
  # of 20 runs failed on a three-changepoint series and on a
  # five-changepoint series, and no successful run ever returned more than
  # one changepoint.
  set.seed(4)
  y3 <- c(rnorm(100), rnorm(100, 5), rnorm(100, -3), rnorm(100, 4))

  fails <- 0L
  counts <- integer(0)
  for (s in 1:8) {
    set.seed(s)
    z <- tryCatch({
      suppressWarnings(utils::capture.output(
        r <- cpt_detect(y3, method = "wbsts")))
      r
    }, error = function(e) {
      if (grepl("the condition has length", conditionMessage(e), fixed = TRUE)) {
        fails <<- fails + 1L
      }
      NULL
    })
    if (!is.null(z)) counts <- c(counts, nrow(z$changepoints))
  }
  expect_equal(fails, 0L)
  # and it can now report more than one, which it could not before
  expect_gt(max(counts), 1L)

  # The replay must be upstream's own computation, not a different one:
  # with `.Random.seed` restored it returns exactly what wbs.lsw() returns
  # on the runs wbs.lsw() can complete.
  replay <- ggchangepoint:::wbs_lsw_replay
  matched <- 0L
  attempted <- 0L
  for (s in 1:6) {
    set.seed(s)
    y <- c(rnorm(60), rnorm(60, 5))
    set.seed(1000 + s)
    st <- get(".Random.seed", envir = globalenv())
    up <- tryCatch(
      suppressWarnings(wbsts::wbs.lsw(y, M = 0, cstar = 0.75, lambda = 0.75)),
      error = function(e) NULL)
    if (is.null(up)) next
    attempted <- attempted + 1L
    assign(".Random.seed", st, envir = globalenv())
    rp <- replay(y, n_intervals = 0, cstar = 0.75, lambda = 0.75,
                 scales = NULL)
    if (identical(up$cp.bef, rp$cp.bef) && identical(up$cp.aft, rp$cp.aft)) {
      matched <- matched + 1L
    }
  }
  expect_gt(attempted, 0L)
  expect_equal(matched, attempted)

  # an unrelated error must still propagate rather than be swallowed by the
  # fallback: one scale is upstream's own refusal
  expect_error(wbsts_wrapper(c(rnorm(60), rnorm(60, 5)), scales = 3),
               "at least two scales")
})

test_that("an engine argument the wrapper renames redirects to the right name", {
  # Most wrappers rename their engine's arguments into this package's
  # snake_case, or derive them from `x`. But `...` is documented on every
  # wrapper as reaching the engine, so the engine's own name is the natural
  # thing for a reader of the upstream help page to pass -- and it collided
  # with the value the wrapper already supplies, giving R's raw "formal
  # argument \"mindist\" matched by multiple actual arguments".
  #
  # Sweeping every wrapper against every argument its engine accepts found
  # **23** such pairs. The earlier managed-argument sweep missed them all
  # because it only looked for `name = <literal>`, and these are
  # `mindist = min_dist` -- a variable, not a literal.
  cases <- list(
    list(m = "ecp",         eng = "ecp",           a = "min.size",      use = "min_size"),
    list(m = "fpop",        eng = "fpop",          a = "lambda",        use = "penalty"),
    list(m = "wbs",         eng = "wbs",           a = "M",             use = "n_intervals"),
    list(m = "wbsts",       eng = "wbsts",         a = "M",             use = "n_intervals"),
    list(m = "cpop",        eng = "cpop",          a = "beta",          use = "penalty"),
    list(m = "decafs",      eng = "DeCAFS",        a = "beta",          use = "penalty"),
    list(m = "bocpd",       eng = "ocp",           a = "hazard_func",   use = "hazard"),
    list(m = "cpm",         eng = "cpm",           a = "cpmType",       use = "cpm_type"),
    list(m = "cpm",         eng = "cpm",           a = "ARL0",          use = "arl0"),
    list(m = "kcp",         eng = "kcpRS",         a = "Kmax",          use = "kmax"),
    list(m = "sn",          eng = "SNSeg",         a = "paras_to_test", use = "parameter"),
    list(m = "ocd",         eng = "ocd",           a = "MC_reps",       use = "mc_reps"),
    list(m = "fabisearch",  eng = "fabisearch",    a = "mindist",       use = "min_dist"),
    list(m = "fabisearch",  eng = "fabisearch",    a = "nruns",         use = "n_runs"),
    list(m = "bfast",       eng = "bfast",         a = "max.iter",      use = "max_iter"),
    list(m = "ocd",         eng = "ocd",           a = "dim",           use = NA),
    list(m = "segmented",   eng = "segmented",     a = "seg.Z",         use = NA)
  )
  # How many of these cases can run here at all. Asserting a fixed minimum
  # instead ("tested > 5") is what broke the R-devel run: against an
  # Imports-only library exactly one engine is present, so the tripwire
  # fired on a sweep that had correctly tested everything available. This
  # is the third time that shape of assertion has misfired, so it is now
  # proportional: test every case whose engine exists, and say so.
  available <- sum(vapply(cases, function(z) engine_installed(z$eng),
                          logical(1)))
  set.seed(7)
  v <- c(rnorm(50), rnorm(50, 5))
  mv <- cbind(a = v, b = rev(v) + rnorm(100, 0, .3))
  tested <- 0L
  for (z in cases) {
    if (!engine_installed(z$eng)) next
    tested <- tested + 1L
    dat <- if (z$m %in% c("ocd")) mv else v
    if (z$m == "fabisearch") dat <- matrix(abs(rnorm(50 * 6)) + 0.5, nrow = 50)
    args <- list(dat, method = z$m); args[[z$a]] <- TRUE
    err <- tryCatch({ suppressWarnings(suppressMessages(
             utils::capture.output(do.call(cpt_detect, args)))); NULL },
             error = function(e) conditionMessage(e))
    expect_true(!is.null(err), info = paste(z$m, z$a, "did not error"))
    # the raw R message must never be what the user sees
    expect_false(grepl("matched by multiple actual arguments", err),
                 info = paste(z$m, z$a))
    if (is.na(z$use)) {
      expect_match(err, "comes from `x` and is not yours to set",
                   info = paste(z$m, z$a))
    } else {
      expect_match(err, paste0("Use `", z$use, "` instead"), fixed = FALSE,
                   info = paste(z$m, z$a))
    }
  }
  expect_equal(tested, available)

  # ...and the wrapper's own argument still reaches the engine
  skip_if_not_installed("wbs")
  expect_s3_class(suppressWarnings(cpt_detect(v, method = "wbs",
                                              n_intervals = 200)), "ggcpt")
  skip_if_not_installed("cpm")
  expect_s3_class(suppressWarnings(cpt_detect(v, method = "cpm",
                                              arl0 = 500)), "ggcpt")
})

test_that("a bad wrapper argument value names the argument, not engine internals", {
  skip_on_cran()
  # Every numeric/logical-defaulted argument of all 50 wrappers -- 64 slots
  # -- probed with NA, Inf, -1, 0, a length-2 vector and a string, asking
  # whether the error names that argument. Twenty-five did not: the value
  # was forwarded and the engine answered from deep inside itself with
  # "missing value where TRUE/FALSE needed", "negative length vectors are
  # not allowed", "NAs in foreign function call", "invalid 'times'
  # argument", "'probs' outside [0,1]", "result would be too long a
  # vector". Each now goes through validate_scalar() with its documented
  # range.
  cases <- list(
    list(m = "bcp",       eng = "bcp",          a = "burnin"),
    list(m = "cpm",       eng = "cpm",          a = "startup"),
    list(m = "kcp",       eng = "kcpRS",        a = "wsize"),
    list(m = "kcp",       eng = "kcpRS",        a = "kmax"),
    list(m = "npmojo",    eng = "CptNonPar",    a = "lag"),
    list(m = "sn",        eng = "SNSeg",        a = "confidence"),
    list(m = "ocd",       eng = "ocd",          a = "patience"),
    list(m = "ocd",       eng = "ocd",          a = "mc_reps"),
    list(m = "segmented", eng = "segmented",    a = "npsi"),
    list(m = "envcpt",    eng = "EnvCpt",       a = "minseglen"),
    list(m = "nsp",       eng = "nsp",          a = "ord"),
    list(m = "esac",      eng = "HDCD",         a = "threshold_d"),
    list(m = "esac",      eng = "HDCD",         a = "N"),
    list(m = "pilliat",   eng = "HDCD",         a = "threshold_d_const"),
    list(m = "pilliat",   eng = "HDCD",         a = "N"),
    list(m = "network",   eng = "changepoints", a = "alpha"),
    list(m = "network",   eng = "changepoints", a = "n_perm"),
    list(m = "fmean",     eng = "fChange",      a = "alpha"),
    list(m = "wbsts",     eng = "wbsts",        a = "cstar"),
    list(m = "wbsts",     eng = "wbsts",        a = "lambda"),
    list(m = "bfast",     eng = "bfast",        a = "frequency"),
    list(m = "taylor",    eng = "ChangePointTaylor", a = "min_conf")
  )
  # the messages these used to give, none of which names an argument
  opaque <- c("missing value where TRUE/FALSE needed",
              "negative length vectors are not allowed",
              "NAs in foreign function call",
              "invalid 'times' argument", "invalid 'nrow' value",
              "'probs' outside [0,1]", "result would be too long a vector",
              "non-numeric argument to binary operator",
              "subscript out of bounds")

  available <- sum(vapply(cases, function(z) engine_installed(z$eng),
                          logical(1)))
  if (available == 0L) skip("none of the engines with a probed argument is installed")

  set.seed(9)
  v <- c(rnorm(100), rnorm(100, 5))
  mv <- cbind(a = v, b = c(rnorm(100), rnorm(100, 4)),
              c = c(rnorm(100), rnorm(100, 3)))
  reg <- builtin_registry()
  tested <- 0L
  for (z in cases) {
    if (!engine_installed(z$eng)) next
    i <- match(z$m, reg$method)
    dat <- if (isTRUE(reg$univariate[i])) v else mv
    if (z$m == "fmean") dat <- matrix(rnorm(100 * 24), nrow = 100)
    if (z$m == "network") {
      dat <- rbind(matrix(stats::rbinom(50 * 16, 1, .2), nrow = 50),
                   matrix(stats::rbinom(50 * 16, 1, .6), nrow = 50))
    }
    if (z$m == "bfast") dat <- stats::ts(v, frequency = 12, start = c(2000, 1))
    tested <- tested + 1L
    for (bad in list(NA_real_, "a", c(1, 2))) {
      args <- list(dat, method = z$m)
      args[[z$a]] <- bad
      err <- tryCatch({
        suppressWarnings(suppressMessages(
          utils::capture.output(do.call(cpt_detect, args))))
        NULL
      }, error = function(e) conditionMessage(e))
      expect_true(!is.null(err),
                  info = paste(z$m, z$a, "accepted", deparse(bad)))
      if (is.null(err)) next
      expect_match(err, z$a, fixed = TRUE, info = paste(z$m, z$a))
      for (o in opaque) {
        expect_false(grepl(o, err, fixed = TRUE),
                     info = paste(z$m, z$a, "->", o))
      }
    }
  }
  expect_equal(tested, available)
})
