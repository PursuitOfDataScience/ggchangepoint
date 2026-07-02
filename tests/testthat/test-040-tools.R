# Tests for the 0.4.0 non-detector tools: CROPS, batch, stability, cite,
# posterior plots, interactive rendering, and the autoplot extensions.

set.seed(2026)
x_step <- c(rnorm(100), rnorm(100, 4))
x_multi <- c(rnorm(100), rnorm(100, 5), rnorm(100, 1))

test_that("cpt_crops returns the penalty path and all three plots build", {
  path <- cpt_crops(x_multi)
  expect_s3_class(path, "ggcpt_path")
  expect_true(all(c("penalty", "n_cpts", "cost", "cpts") %in%
                  names(path$solutions)))
  expect_gte(nrow(path$solutions), 2)
  # Cost must be non-increasing in the number of changepoints
  sol <- path$solutions[order(path$solutions$n_cpts), ]
  expect_true(all(diff(sol$cost) <= 1e-8))
  expect_identical(tidy(path), path$solutions)
  expect_output(print(path), "ggcpt_path")
  for (ty in c("elbow", "path", "segmentations")) {
    p <- ggplot2::autoplot(path, type = ty)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("cpt_crops validates its penalty range", {
  expect_error(cpt_crops(x_step, pen_min = 10, pen_max = 5), "strictly smaller")
})

test_that("cpt_batch runs over matrix columns and named lists", {
  X <- cbind(s1 = x_step, s2 = rnorm(200))
  b <- cpt_batch(X, method = "pelt")
  expect_s3_class(b, "ggcpt_batch")
  expect_equal(b$series, c("s1", "s2"))
  expect_gte(b$n_changepoints[1], 1)
  expect_s3_class(b$result[[1]], "ggcpt")

  b2 <- cpt_batch(list(a = x_step, b = rnorm(150)), method = "pelt")
  expect_equal(b2$series, c("a", "b"))

  td <- tidy(b)
  expect_true(all(c("series", "cp", "cp_value") %in% names(td)))
  expect_output(print(b), "ggcpt_batch")
  p <- ggplot2::autoplot(b)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("cpt_stability reports high frequency at the true changepoint", {
  st <- cpt_stability(x_step, method = "pelt", B = 20, seed = 1)
  expect_s3_class(st, "ggcpt_stability")
  expect_gte(st$frequency$freq[100], 0.9)
  expect_lte(max(st$frequency$freq), 1)
  expect_output(print(st), "stability")
  p <- ggplot2::autoplot(st)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("cpt_cite covers every wired method and dispatches on ggcpt", {
  refs <- capture.output(tbl <- cpt_cite())
  wired <- cpt_methods()
  wired <- wired$method[wired$status == "available"]
  # binseg/segneigh/amoc/hsmuce share family references and have their own rows
  expect_true(all(wired %in% tbl$method))
  res <- cpt_detect(x_step, method = "pelt")
  out <- capture.output(one <- cpt_cite(res))
  expect_equal(one$method, "pelt")
  expect_error(suppressWarnings(cpt_cite("nonexistent-method")),
               "No reference")
})

test_that("ggcpt_posterior draws the two-panel Bayesian display", {
  skip_if_not_installed("bcp")
  res <- bcp_wrapper(x_step, seed = 1)
  p <- ggcpt_posterior(res)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  plain <- cpt_detect(x_step, method = "pelt")
  expect_error(ggcpt_posterior(plain), "No posterior probability")
})

test_that("autoplot show_ci draws intervals and warns when absent", {
  skip_if_not_installed("stepR")
  res <- smuce_wrapper(x_step)
  p <- ggplot2::autoplot(res, show_ci = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
  plain <- cpt_detect(x_step, method = "pelt")
  expect_warning(ggplot2::autoplot(plain, show_ci = TRUE), "no ci_lower")
})

test_that("autoplot show_fit draws the engine signal and warns when absent", {
  skip_if_not_installed("DeCAFS")
  res <- decafs_wrapper(x_step)
  p <- ggplot2::autoplot(res, show_fit = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
  plain <- cpt_detect(x_step, method = "pelt")
  expect_warning(ggplot2::autoplot(plain, show_fit = TRUE), "no fitted")
})

test_that("autoplot renders multivariate results as facets", {
  skip_if_not_installed("InspectChangepoint")
  X <- cbind(a = c(rnorm(80), rnorm(80, 3)), b = c(rnorm(80), rnorm(80, -2)))
  res <- inspect_wrapper(X)
  p <- ggplot2::autoplot(res)
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$layout$layout$PANEL)), 2)
})

test_that("ggcpt_interactive returns a plotly widget", {
  skip_if_not_installed("plotly")
  res <- cpt_detect(x_step, method = "pelt")
  w <- ggcpt_interactive(res)
  expect_s3_class(w, "plotly")
  expect_error(ggcpt_interactive(42), "must be a ggcpt object or a ggplot")
})

test_that("autoplot warns on unknown styling arguments", {
  res <- cpt_detect(x_step, method = "pelt")
  expect_warning(ggplot2::autoplot(res, cptline_colour = "red"),
                 "Ignoring unknown argument")
})

test_that("autoplot on an empty ggcpt errors cleanly", {
  expect_error(ggplot2::autoplot(new_ggcpt()), "empty ggcpt")
})
