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
