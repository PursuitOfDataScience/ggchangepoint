test_that("ggcpt class works", {
  res <- new_ggcpt()
  expect_s3_class(res, "ggcpt")
  expect_true(is_ggcpt(res))
  expect_true("cp_convention" %in% names(res))

  # With data
  res2 <- new_ggcpt(
    changepoints = tibble::tibble(cp = 100L, cp_value = 5.0),
    segments = tibble::tibble(seg_id = 1:2, start = c(1, 101),
                               end = c(100, 200), n = c(100, 100),
                               param_estimate = c(0, 5)),
    data = tibble::tibble(index = 1:200, value = rnorm(200)),
    method = "pelt",
    change_in = "mean",
    penalty = list(type = "MBIC", value = 3.0)
  )
  expect_s3_class(res2, "ggcpt")
  expect_true(is_ggcpt(res2))
  expect_equal(res2$cp_convention, "left")
})

test_that("broom methods work", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  res <- cpt_detect(x, method = "pelt", change_in = "mean")

  td <- generics::tidy(res)
  expect_s3_class(td, "tbl_df")
  expect_true("cp" %in% names(td))

  gl <- generics::glance(res)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1)
  expect_true("n_changepoints" %in% names(gl))
  expect_true("cp_convention" %in% names(gl))

  au <- generics::augment(res)
  expect_s3_class(au, "tbl_df")
  expect_true("is_changepoint" %in% names(au))
  expect_true(".fitted" %in% names(au))
  expect_true(".resid" %in% names(au))
})

test_that("autoplot.ggcpt works", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  res <- cpt_detect(x, method = "pelt", change_in = "mean")
  p <- ggplot2::autoplot(res)
  expect_s3_class(p, "ggplot")
})

test_that("cpt_detect dispatches methods", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))

  res <- cpt_detect(x, method = "pelt", change_in = "mean")
  expect_s3_class(res, "ggcpt")
  expect_gt(nrow(res$changepoints), 0)
  expect_equal(res$method, "pelt")

  res2 <- cpt_detect(x, method = "binseg", change_in = "mean")
  expect_s3_class(res2, "ggcpt")

  res3 <- cpt_detect(x, method = "segneigh", change_in = "mean", penalty = "BIC")
  expect_s3_class(res3, "ggcpt")

  res4 <- cpt_detect(x, method = "np", change_in = "distribution")
  expect_s3_class(res4, "ggcpt")
  expect_equal(res4$method, "np")

  res5 <- cpt_detect(x, method = "ecp", change_in = "distribution")
  expect_s3_class(res5, "ggcpt")
})

test_that("cpt_detect uses penalty parameter", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- cpt_detect(x, method = "pelt", change_in = "mean", penalty = "BIC")
  expect_s3_class(res, "ggcpt")
})

test_that("cpt_penalty works", {
  expect_equal(cpt_penalty("None", n = 100), 0)
  expect_true(is.numeric(cpt_penalty("BIC", n = 100)))
  expect_true(is.numeric(cpt_penalty("AIC", n = 100)))
  expect_true(is.numeric(cpt_penalty("Hannan-Quinn", n = 100)))
  expect_true(is.numeric(cpt_penalty("sSIC", n = 100)))
  expect_equal(cpt_penalty("Manual", value = 5), 5)
  expect_error(cpt_penalty("Manual"))
})

test_that("cpt_metrics works", {
  res <- cpt_metrics(c(100, 200), c(100, 200), n = 300)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$precision, 1)
  expect_equal(res$recall, 1)
  expect_equal(res$f1, 1)
  expect_true("rand_index" %in% names(res))
  expect_true("rmse_matched" %in% names(res))
  expect_equal(res$annotation_error, 0)

  res2 <- cpt_metrics(c(105, 205), c(100, 200), n = 300, margin = 10)
  expect_equal(res2$f1, 1)

  # No overlap
  res3 <- cpt_metrics(c(50, 150), c(100, 200), n = 300, margin = 5)
  expect_equal(res3$f1, 0)
})

test_that("cpt_simulate works", {
  set.seed(2022)
  dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
                      params = c(0, 10))
  expect_s3_class(dat, "tbl_df")
  expect_equal(nrow(dat), 200)
  expect_true("true_changepoints" %in% names(attributes(dat)))
  expect_equal(attr(dat, "true_changepoints"), 100)
})

test_that("rcpt alias works", {
  set.seed(2022)
  dat1 <- cpt_simulate(100, changepoints = c(50), change_in = "mean",
                       params = c(0, 5))
  set.seed(2022)
  dat2 <- rcpt(100, changepoints = c(50), change_in = "mean",
               params = c(0, 5))
  expect_equal(dat1$value, dat2$value)
})

test_that("test signals work", {
  dat <- signal_blocks(100)
  expect_s3_class(dat, "tbl_df")
  expect_equal(nrow(dat), 100)
  expect_true("true_changepoints" %in% names(attributes(dat)))

  dat2 <- signal_fms(200)
  expect_s3_class(dat2, "tbl_df")

  dat3 <- signal_mix(200)
  expect_s3_class(dat3, "tbl_df")

  dat4 <- signal_teeth(200)
  expect_s3_class(dat4, "tbl_df")

  dat5 <- signal_stairs(200)
  expect_s3_class(dat5, "tbl_df")
})

test_that("theme_ggcpt works", {
  th <- theme_ggcpt()
  expect_s3_class(th, "theme")
})

test_that("annotate_segments works", {
  ann <- annotate_segments(c(50, 100), n = 200)
  expect_type(ann, "list")
  expect_length(ann, 3)
})
