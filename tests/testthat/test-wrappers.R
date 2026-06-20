test_that("gfpop_wrapper does not report the series endpoint as a changepoint", {
  skip_if_not_installed("gfpop")
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- gfpop_wrapper(x)
  expect_s3_class(res, "ggcpt")
  # The true change is at 100; n (=200) must never appear as a changepoint.
  expect_false(200L %in% res$changepoints$cp)
  expect_true(all(res$changepoints$cp < length(x)))
  expect_true(100L %in% res$changepoints$cp)
})

test_that("idetect_wrapper reads the cpt field and finds an obvious shift", {
  skip_if_not_installed("IDetect")
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- idetect_wrapper(x)
  expect_s3_class(res, "ggcpt")
  expect_gt(nrow(res$changepoints), 0)
  expect_true(all(res$changepoints$cp > 0 & res$changepoints$cp < length(x)))
})

test_that("cpt_simulate applies per-segment standard deviations", {
  set.seed(1)
  d <- cpt_simulate(400, changepoints = 200, change_in = "var",
                    params = c(1, 5))
  s1 <- sd(d$value[1:200])
  s2 <- sd(d$value[201:400])
  # The second segment should be markedly more variable than the first.
  expect_gt(s2, 3 * s1)

  set.seed(1)
  dmv <- cpt_simulate(400, changepoints = 200, change_in = "meanvar",
                      params = list(list(mean = 0, sd = 1),
                                    list(mean = 5, sd = 4)))
  expect_gt(mean(dmv$value[201:400]) - mean(dmv$value[1:200]), 3)
  expect_gt(sd(dmv$value[201:400]), 2 * sd(dmv$value[1:200]))
})

test_that("cpt_detect segneigh works with the default penalty", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- cpt_detect(x, method = "segneigh")
  expect_s3_class(res, "ggcpt")
  # SegNeigh cannot use MBIC upstream; the dispatcher falls back to SIC.
  expect_equal(res$penalty$type, "SIC")
})

test_that("glance reports the penalty actually used", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  expect_equal(generics::glance(cpt_detect(x, penalty = "BIC"))$penalty_type, "BIC")
  expect_equal(generics::glance(cpt_detect(x, penalty = "AIC"))$penalty_type, "AIC")
})
