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
