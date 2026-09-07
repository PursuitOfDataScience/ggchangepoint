# 0.5.0: visual regression. The package is a visualization package and had
# no visual net at all -- a layer could be dropped, an axis inverted or a
# scale silently changed and every expect_no_error() test would still pass.
#
# These are vdiffr snapshots. They are a *local* regression net and are
# skipped on CRAN and on CI, because an SVG snapshot records the font stack
# and graphics device of the machine that produced it: the same plot renders
# to different SVG on a different runner, so enabling them elsewhere would
# report a failure on every platform except the one the snapshots came from.
# Regenerate them with `testthat::snapshot_accept()` after an intentional
# visual change, and read the diff before accepting it. Run the suite with
# NOT_CRAN=true when you want them exercised -- a plain `test_dir()` skips
# them, and testthat then treats the files as unused.
#
# The `_snaps` directory is `.Rbuildignore`d: CRAN never runs these, so
# shipping a quarter of a megabyte of SVG in the tarball would be dead
# weight. They live in the repository, where the developer running the
# tests is.

skip_if_no_vdiffr <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
}

# Announce a block's snapshot files BEFORE skipping.
#
# testthat prunes snapshot files it did not see used during a run. Skipping
# at the top of a `test_that()` block means `expect_doppelganger()` is never
# reached, nothing is announced, and a plain `test_dir()` deletes the whole
# `_snaps` directory -- after which the next NOT_CRAN=true run recreates it
# from whatever the code does at that moment, so the net always passes and
# never catches anything. Announcing first is what vdiffr does internally
# for the skips it owns; this helper does it for the ones we own.
announce <- function(...) {
  for (title in c(...)) {
    announce_snapshot_file(name = snapshot_name(title))
  }
}

snapshot_name <- function(title) {
  paste0(gsub("(^-|-$)", "",
              gsub("-+", "-", gsub("[^a-z0-9]+", "-", tolower(title)))),
         ".svg")
}

set.seed(2026)
x_step <- c(rnorm(60), rnorm(60, 4))
x_multi <- c(rnorm(50), rnorm(50, 4), rnorm(50, 1))
fit <- cpt_detect(x_step, method = "pelt")

test_that("autoplot renders the core views consistently", {
  announce(
    "autoplot default",
    "autoplot with segments",
    "autoplot without points",
    "autoplot with a date index"
  )
  skip_if_no_vdiffr()
  vdiffr::expect_doppelganger("autoplot default", ggplot2::autoplot(fit))
  vdiffr::expect_doppelganger(
    "autoplot with segments",
    ggplot2::autoplot(fit, show_segments = TRUE)
  )
  vdiffr::expect_doppelganger(
    "autoplot without points",
    ggplot2::autoplot(fit, show_points = FALSE)
  )
  vdiffr::expect_doppelganger(
    "autoplot with a date index",
    ggplot2::autoplot(
      cpt_detect(x_step, method = "pelt",
                 index = as.Date("2020-01-01") + seq_along(x_step) - 1)
    )
  )})

test_that("the theme and the palettes render consistently", {
  announce(
    "theme_ggcpt",
    "scale_colour_cpt"
  )
  skip_if_no_vdiffr()
  vdiffr::expect_doppelganger(
    "theme_ggcpt", ggplot2::autoplot(fit) + theme_ggcpt()
  )
  d <- data.frame(x = 1:8, y = 1:8, g = factor(letters[1:8]))
  vdiffr::expect_doppelganger(
    "scale_colour_cpt",
    ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point(size = 4) + scale_colour_cpt() + theme_ggcpt()
  )})

test_that("the new layers render consistently", {
  announce(
    "geom_cpt_region",
    "geom_cpt_label",
    "geom_cpt_event"
  )
  skip_if_no_vdiffr()
  d <- data.frame(t = seq_along(x_step), y = x_step)
  regions <- data.frame(xmin = 55, xmax = 66)
  vdiffr::expect_doppelganger(
    "geom_cpt_region",
    ggplot2::ggplot(d, ggplot2::aes(t, y)) +
      geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax),
                      data = regions) +
      ggplot2::geom_line()
  )
  labs <- cpt_labels(c(40, 80), c(70, 110), c("change", "no_change"))
  vdiffr::expect_doppelganger(
    "geom_cpt_label",
    ggplot2::ggplot(d, ggplot2::aes(t, y)) +
      geom_cpt_label(ggplot2::aes(xmin = start, xmax = end, fill = change),
                     data = labs) +
      ggplot2::geom_line() + scale_fill_cpt_label()
  )
  ev <- data.frame(x = 60, label = "policy change")
  vdiffr::expect_doppelganger(
    "geom_cpt_event",
    ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line() +
      geom_cpt_event(ggplot2::aes(xintercept = x, label = label),
                     data = ev, repel = FALSE)
  )})

test_that("the comparison and evaluation displays render consistently", {
  announce(
    "ggcpt_compare facet",
    "ggcpt_compare overlay",
    "ggcpt_eval"
  )
  skip_if_no_vdiffr()
  vdiffr::expect_doppelganger(
    "ggcpt_compare facet",
    ggcpt_compare(x_multi, methods = c("pelt", "binseg"))
  )
  vdiffr::expect_doppelganger(
    "ggcpt_compare overlay",
    ggcpt_compare(x_multi, methods = c("pelt", "binseg"),
                  layout = "overlay")
  )
  vdiffr::expect_doppelganger(
    "ggcpt_eval", ggcpt_eval(c(48, 105), c(50, 100), x_multi)
  )})

test_that("the selection and diagnostic displays render consistently", {
  announce(
    "cpt_select criterion",
    "cpt_select ladder",
    "cpt_sensitivity"
  )
  skip_if_no_vdiffr()
  sel <- cpt_select(x_multi, criterion = "bic", k_max = 5)
  vdiffr::expect_doppelganger("cpt_select criterion",
                              ggplot2::autoplot(sel))
  vdiffr::expect_doppelganger(
    "cpt_select ladder",
    ggplot2::autoplot(sel, plot_type = "ladder", max_facets = 4)
  )
  s <- cpt_sensitivity(x_step, method = "pelt",
                       over = list(penalty = c(4, 20)))
  vdiffr::expect_doppelganger("cpt_sensitivity", ggplot2::autoplot(s))})

test_that("the batch, stability and path displays render consistently", {
  announce(
    "cpt_batch",
    "cpt_stability",
    "cpt_crops elbow"
  )
  skip_if_no_vdiffr()
  X <- cbind(a = x_step, b = rev(x_step))
  vdiffr::expect_doppelganger("cpt_batch",
                              ggplot2::autoplot(cpt_batch(X, method = "pelt")))
  st <- cpt_stability(x_step, method = "pelt", B = 10, seed = 1)
  vdiffr::expect_doppelganger("cpt_stability", ggplot2::autoplot(st))
  path <- cpt_crops(x_multi)
  vdiffr::expect_doppelganger("cpt_crops elbow", ggplot2::autoplot(path))})

test_that("the benchmark and power displays render consistently", {
  announce(
    "cpt_benchmark heatmap",
    "cpt_benchmark ranks",
    "cpt_power"
  )
  skip_if_no_vdiffr()
  bm <- cpt_benchmark(cpt_datasets(n = 200, seed = 1,
                                   names = c("step", "teeth")),
                      methods = c("pelt", "amoc"), progress = FALSE)
  vdiffr::expect_doppelganger("cpt_benchmark heatmap",
                              ggplot2::autoplot(bm))
  vdiffr::expect_doppelganger(
    "cpt_benchmark ranks",
    ggplot2::autoplot(bm, plot_type = "ranks")
  )
  pw <- cpt_power(n = 150, jump = c(0.5, 2), n_sim = 10, seed = 1,
                  parallel = FALSE)
  vdiffr::expect_doppelganger("cpt_power", ggplot2::autoplot(pw))})

test_that("the monitoring display renders consistently", {
  announce(
    "cpt_monitor timeline"
  )
  skip_if_no_vdiffr()
  # Seed the block, not the file. `x_step` and `x_multi` at the top are
  # drawn once from a seeded stream, but a block that generates its own
  # data was taking whatever stream state the blocks above it happened to
  # leave -- so the snapshot was a function of test execution order, and
  # stable only by accident. It came apart the moment seeded calls stopped
  # leaking their seed into the caller's stream: `cpt_power(seed = 1)` in
  # the block above used to pin the stream this `rnorm()` drew from.
  set.seed(4001)
  mon <- cpt_replay(c(rnorm(150), rnorm(150, 3)), method = "edetector")
  vdiffr::expect_doppelganger("cpt_monitor timeline",
                              ggplot2::autoplot(mon))})

test_that("engine-dependent displays render consistently", {
  announce(
    "ggcpt_statistic",
    "ggcpt_scale_space",
    "ggcpt_solution_path"
  )
  skip_if_no_vdiffr()
  skip_if_not_installed("mosum")
  # Self-contained, for the reason given in the block above.
  set.seed(4002)
  m <- cpt_detect(c(rnorm(200), rnorm(200, 3)), method = "mosum")
  vdiffr::expect_doppelganger("ggcpt_statistic", ggcpt_statistic(m))
  vdiffr::expect_doppelganger(
    "ggcpt_scale_space",
    ggcpt_scale_space(m, bandwidths = c(20, 40))
  )
  skip_if_not_installed("wbs")
  set.seed(4003)
  w <- cpt_detect(c(rnorm(200), rnorm(200, 3)), method = "wbs")
  vdiffr::expect_doppelganger("ggcpt_solution_path",
                              ggcpt_solution_path(w, max_steps = 15))})

test_that("announced snapshot names match the files vdiffr writes", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("vdiffr")
  # If snapshot_name() and vdiffr's own sanitisation ever drift apart, the
  # announcements would name files that do not exist, testthat would prune
  # the real ones, and the whole visual net would quietly reset itself on
  # the next run. Check the mapping against what is actually on disk.
  expect_equal(snapshot_name("autoplot default"), "autoplot-default.svg")
  expect_equal(snapshot_name("cpt_select ladder"), "cpt-select-ladder.svg")
  expect_equal(snapshot_name("ggcpt_compare facet"),
               "ggcpt-compare-facet.svg")
  dir <- test_path("_snaps", "050-visual")
  skip_if_not(dir.exists(dir), "snapshots not generated yet")
  on_disk <- basename(list.files(dir, pattern = "\\.svg$"))
  expect_true(all(c("autoplot-default.svg", "cpt-select-ladder.svg",
                    "ggcpt-compare-facet.svg") %in% on_disk))
})
