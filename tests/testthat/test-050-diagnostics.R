# 0.5.0: selection, influence, sensitivity, and seeing the statistic.

set.seed(2026)
x_two <- c(rnorm(80), rnorm(80, 4), rnorm(80, 1))
x_step <- c(rnorm(100), rnorm(100, 3))

test_that("cpt_select scores a ladder and picks a K from it", {
  sel <- cpt_select(x_two, criterion = "bic", k_max = 6)
  expect_s3_class(sel, "ggcpt_selection")
  tab <- tidy(sel)
  expect_true(all(c("k", "value", "cost", "chosen") %in% names(tab)))
  expect_equal(sum(tab$chosen), 1)
  expect_equal(sel$k, tab$k[tab$chosen])
  # The candidate ladder must be nested and the cost non-increasing in K.
  ord <- order(tab$k)
  expect_true(all(diff(tab$cost[ord]) <= 1e-8))
  expect_equal(sel$k, 2)
  expect_s3_class(sel$fit, "ggcpt")
  expect_equal(nrow(sel$fit$changepoints), sel$k)
  expect_output(print(sel), "Chosen K")

  for (ty in c("criterion", "segmentation", "ladder")) {
    p <- ggplot2::autoplot(sel, plot_type = ty)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("every selection criterion runs and agrees on an easy series", {
  for (cr in c("bic", "mbic", "crops_elbow")) {
    sel <- cpt_select(x_two, criterion = cr, k_max = 6)
    expect_equal(sel$k, 2, info = cr)
  }
  # AIC is deliberately not in that list: its 2k penalty does not grow with
  # n, so it over-selects changepoints, and on this series it takes every
  # rung of the ladder. That is the criterion behaving as documented, not a
  # failure, so the test asserts only that it finds at least the real ones.
  aic <- cpt_select(x_two, criterion = "aic", k_max = 6)
  expect_gte(aic$k, 2)
  st <- cpt_select(x_two, criterion = "stability", k_max = 3, B = 5)
  expect_true(st$k %in% 0:3)
  skip_if_not_installed("crossvalidationCP")
  cv <- cpt_select(x_two, criterion = "cv", k_max = 6)
  expect_equal(cv$k, 2)
})

test_that("the Zhang-Siegmund mBIC reads the segment lengths", {
  n <- 200
  # Two segmentations with the same K but different lengths must score
  # differently -- which is exactly what cpt_penalty()'s "MBIC" cannot do.
  a <- ggchangepoint:::zhang_siegmund_penalty(100, n)
  b <- ggchangepoint:::zhang_siegmund_penalty(10, n)
  expect_false(isTRUE(all.equal(a, b)))
  expect_equal(ggchangepoint:::zhang_siegmund_penalty(integer(0), n), 0)
})

test_that("the knee rule is scale-free", {
  k <- 0:10
  cost <- c(100, 40, 20, 18, 17, 16.5, 16.2, 16, 15.9, 15.85, 15.8)
  j <- ggchangepoint:::knee_point(k, cost)
  expect_equal(ggchangepoint:::knee_point(k, cost * 1000), j)
  expect_equal(ggchangepoint:::knee_point(k, cost + 500), j)
})

test_that("cpt_influence works through both engines and agrees on shape", {
  fit <- cpt_detect(x_step, method = "pelt")
  inf <- cpt_influence(fit, engine = "recompute",
                       subset = seq(1, 200, by = 20))
  expect_s3_class(inf, "ggcpt_influence")
  expect_equal(nrow(inf$influence), 10)
  expect_true(all(c("index", "n_cp", "delta_n_cp", "max_shift",
                    "param_shift", "cpts") %in% names(inf$influence)))
  expect_equal(inf$engine, "recompute")
  expect_output(print(inf), "ggcpt_influence")

  lev <- cpt_leverage(inf)
  expect_true(all(diff(lev$leverage) <= 1e-8))   # sorted, most influential first
  expect_equal(nrow(lev), nrow(inf$influence))

  skip_if_not_installed("changepoint.influence")
  native <- cpt_influence(fit)
  expect_equal(native$engine, "changepoint.influence")
  expect_equal(nrow(native$influence), length(x_step))
  for (ty in c("overview", "location", "parameter", "map")) {
    p <- ggplot2::autoplot(native, plot_type = ty)
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("cpt_influence validates its subset and refuses tiny series", {
  fit <- cpt_detect(x_step, method = "pelt")
  expect_error(cpt_influence(fit, subset = c(1, 5000)), "index observations")
  tiny <- as_ggcpt(integer(0), rnorm(3))
  expect_error(cpt_influence(tiny), "at least 4 observations")
  expect_error(cpt_influence(1:10), "must be a ggcpt object")
})

test_that("cpt_sensitivity sweeps a grid and reports every setting", {
  s <- cpt_sensitivity(x_step, method = "pelt",
                       over = list(penalty = c(2, 10, 40)))
  expect_s3_class(s, "ggcpt_sensitivity")
  expect_equal(nrow(s$grid), 3)
  expect_true(all(c("penalty", "n_cp", "cpts", "error") %in% names(s$grid)))
  # A weaker penalty can never find fewer changepoints than a stronger one.
  expect_true(all(diff(s$grid$n_cp[order(s$grid$penalty)]) <= 0))
  expect_output(print(s), "ggcpt_sensitivity")
  expect_true(all(c("penalty", "cp") %in% names(tidy(s))))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(s)))
  expect_error(cpt_sensitivity(x_step, over = list()), "non-empty named list")
})

test_that("cpt_statistic returns a per-location statistic where one exists", {
  skip_if_not_installed("mosum")
  fit <- cpt_detect(x_step, method = "mosum")
  st <- cpt_statistic(fit)
  expect_equal(nrow(st), length(x_step))
  expect_true(all(c("index", "statistic", "threshold", "label") %in%
                    names(st)))
  expect_true(is.finite(st$threshold[1]))
  expect_no_error(ggplot2::ggplot_build(ggcpt_statistic(fit)))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(fit, type = "statistic")))
})

test_that("the AMOC statistic peaks at the changepoint", {
  fit <- cpt_detect(x_step, method = "amoc")
  st <- cpt_statistic(fit)
  expect_equal(which.max(st$statistic[!is.na(st$statistic)]),
               fit$changepoints$cp[1])
})

test_that("an engine without internals says which ones have them", {
  fit <- cpt_detect(x_step, method = "pelt")
  expect_error(cpt_statistic(fit), "does not expose a per-location statistic")
  expect_error(cpt_statistic(fit), "These do")
  expect_error(cpt_solution_path(fit), "does not expose a solution path")
  expect_error(cpt_solution_path(fit), "cpt_crops")
})

test_that("solution paths come back ordered with the selected set flagged", {
  fit <- cpt_detect(x_step, method = "binseg")
  path <- cpt_solution_path(fit)
  expect_true(all(c("step", "cp", "contrast", "selected") %in% names(path)))
  expect_equal(path$step, seq_len(nrow(path)))
  expect_true(any(path$selected))
  expect_setequal(path$cp[path$selected], fit$changepoints$cp)
  expect_no_error(ggplot2::ggplot_build(ggcpt_solution_path(fit)))

  skip_if_not_installed("wbs")
  wfit <- cpt_detect(x_step, method = "wbs")
  wpath <- cpt_solution_path(wfit)
  # An interval-based search reports the interval that proposed each split,
  # and the contrast must be non-increasing down the ranking.
  expect_false(all(is.na(wpath$start)))
  expect_true(all(diff(wpath$contrast) <= 1e-8))
})

test_that("scale space sweeps bandwidths and marks the detections", {
  skip_if_not_installed("mosum")
  ss <- cpt_scale_space(x_step, bandwidths = c(20, 40))
  expect_true(all(c("index", "bandwidth", "statistic", "threshold",
                    "significant", "detected") %in% names(ss)))
  expect_setequal(unique(ss$bandwidth), c(20L, 40L))
  expect_equal(nrow(ss), 2 * length(x_step))
  expect_no_error(ggplot2::ggplot_build(
    ggcpt_scale_space(x_step, bandwidths = c(20, 40))))
  expect_error(cpt_scale_space(x_step, bandwidths = 500), "No usable bandwidth")
})

test_that("cpt_select says when the method gives it no ladder", {
  skip_if_not_installed("wbs")
  # wbs tunes itself by sSIC and ignores `penalty`, so the candidate sweep
  # returns the same segmentation at every rung. Reporting a "chosen" K off
  # a one-point curve would be a fabricated decision.
  expect_warning(cpt_select(x_step, method = "wbs", k_max = 5),
                 "distinct segmentation")
})

test_that("scale space works for the multivariate multiscale engine", {
  skip_if_not_installed("CptNonPar")
  set.seed(41)
  X <- cbind(c(rnorm(150), rnorm(150, 3)), c(rnorm(150), rnorm(150, -2)))
  ss <- cpt_scale_space(X, bandwidths = c(30, 60), method = "npmojo")
  expect_equal(nrow(ss), 2 * nrow(X))
  # np.mojo names its per-location statistic `test.stat`, and `threshold`
  # holds the RULE ("bootstrap") while `threshold.val` holds the number.
  # Reading the wrong fields left every statistic NA and every cell
  # insignificant -- a heatmap of uniform grey that looked like a result.
  expect_true(all(is.finite(ss$statistic)))
  expect_true(is.finite(ss$threshold[1]))
  expect_gt(sum(ss$significant), 0)
  expect_true(any(ss$detected))
  # A ggcpt from npmojo keeps its coordinates rather than being flattened.
  fit <- cpt_detect(X, method = "npmojo")
  expect_equal(nrow(cpt_scale_space(fit, bandwidths = c(30, 60))),
               2 * nrow(X))
  expect_no_error(ggplot2::ggplot_build(
    ggcpt_scale_space(fit, bandwidths = c(30, 60))))
  # mosum is univariate and must still refuse a matrix.
  expect_error(cpt_scale_space(X, bandwidths = c(30, 60), method = "mosum"),
               "univariate")
})
