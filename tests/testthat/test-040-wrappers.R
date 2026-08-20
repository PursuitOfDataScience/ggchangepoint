# Tests for the 0.4.0 engine wave. Each wrapper test is guarded with
# skip_if_not_installed() so the suite passes on minimal installations.

set.seed(2026)
x_step <- c(rnorm(100), rnorm(100, 4))
x_slope <- cumsum(c(rep(0.4, 100), rep(-0.3, 100))) + rnorm(200)
X_mv <- cbind(a = c(rnorm(80), rnorm(80, 3)),
              b = c(rnorm(80), rnorm(80, -2)),
              c = rnorm(160))

expect_ggcpt_contract <- function(res, method = NULL) {
  expect_s3_class(res, "ggcpt")
  expect_true(all(c("cp", "cp_value") %in% names(res$changepoints)))
  expect_true(all(res$changepoints$cp >= 1))
  expect_true(all(res$changepoints$cp < nrow(res$data)))
  expect_identical(res$cp_convention, "left")
  expect_equal(nrow(res$segments), nrow(res$changepoints) + 1)
  if (!is.null(method)) expect_identical(res$method, method)
  g <- glance(res)
  expect_equal(nrow(g), 1)
}

test_that("smuce_wrapper returns CIs and a fitted signal", {
  skip_if_not_installed("stepR")
  res <- smuce_wrapper(x_step)
  expect_ggcpt_contract(res, "smuce")
  expect_true(all(c("ci_lower", "ci_upper") %in% names(res$changepoints)))
  expect_true(all(res$changepoints$ci_lower <= res$changepoints$cp))
  expect_true(all(res$changepoints$ci_upper >= res$changepoints$cp))
  expect_true("fitted" %in% names(res$data))
  expect_true(any(abs(res$changepoints$cp - 100) <= 5))
})

test_that("hsmuce family works through the dispatcher", {
  skip_if_not_installed("stepR")
  res <- cpt_detect(x_step, method = "hsmuce")
  expect_identical(res$method, "hsmuce")
})

test_that("cpop_wrapper detects a slope change and reports change_in slope", {
  skip_if_not_installed("cpop")
  res <- cpop_wrapper(x_slope)
  expect_ggcpt_contract(res, "cpop")
  expect_identical(res$change_in, "slope")
  expect_true(any(abs(res$changepoints$cp - 100) <= 10))
  expect_true("fitted" %in% names(res$data))
})

test_that("bcp_wrapper reports posterior probabilities", {
  skip_if_not_installed("bcp")
  res <- bcp_wrapper(x_step, seed = 1)
  expect_ggcpt_contract(res, "bcp")
  expect_true("posterior_prob" %in% names(res$changepoints))
  expect_true(all(res$changepoints$posterior_prob >= 0.5))
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
})

test_that("bocpd_wrapper finds the change and supports the run-length plot", {
  skip_if_not_installed("ocp")
  res <- bocpd_wrapper(x_step)
  expect_ggcpt_contract(res, "bocpd")
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
  p <- ggcpt_runlength(res)
  expect_s3_class(p, "ggplot")
})

test_that("beast_wrapper reports posterior probabilities", {
  skip_if_not_installed("Rbeast")
  skip_on_os("windows")  # Rbeast <= 1.0.2 can crash the session on Windows
  res <- beast_wrapper(x_step, seed = 1)
  expect_ggcpt_contract(res, "beast")
  expect_true("posterior_prob" %in% names(res$changepoints))
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
})

test_that("cpm_wrapper reports detection times", {
  skip_if_not_installed("cpm")
  res <- cpm_wrapper(x_step)
  expect_ggcpt_contract(res, "cpm")
  expect_true("detection_time" %in% names(res$changepoints))
  expect_true(all(res$changepoints$detection_time > res$changepoints$cp))
})

test_that("kcp_wrapper runs on running means", {
  skip_if_not_installed("kcpRS")
  res <- kcp_wrapper(x_step, nperm = 100, seed = 1)
  expect_ggcpt_contract(res, "kcp")
  expect_true(any(abs(res$changepoints$cp - 100) <= 15))
})

test_that("npmojo_wrapper runs", {
  skip_if_not_installed("CptNonPar")
  res <- npmojo_wrapper(x_step)
  expect_ggcpt_contract(res, "npmojo")
  expect_true(any(abs(res$changepoints$cp - 100) <= 10))
})

test_that("decafs_wrapper detects the change and carries the signal", {
  skip_if_not_installed("DeCAFS")
  res <- decafs_wrapper(x_step)
  expect_ggcpt_contract(res, "decafs")
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
  expect_true("fitted" %in% names(res$data))
})

test_that("sn_wrapper detects the change", {
  skip_if_not_installed("SNSeg")
  res <- sn_wrapper(x_step)
  expect_ggcpt_contract(res, "sn")
  expect_true(any(abs(res$changepoints$cp - 100) <= 10))
})

test_that("inspect_wrapper handles multivariate input", {
  skip_if_not_installed("InspectChangepoint")
  res <- inspect_wrapper(X_mv)
  expect_ggcpt_contract(res, "inspect")
  expect_true("strength" %in% names(res$changepoints))
  expect_true(any(abs(res$changepoints$cp - 80) <= 5))
  expect_false(is.null(res$data_wide))
})

test_that("ocd_wrapper declares the change shortly after it happens", {
  skip_if_not_installed("ocd")
  set.seed(1)
  X_strong <- cbind(a = c(rnorm(80), rnorm(80, 5)),
                    b = c(rnorm(80), rnorm(80, -5)),
                    c = c(rnorm(80), rnorm(80, 3)))
  # mc_reps only calibrates the detection threshold, and the change here is
  # far too large for that calibration to matter: 10 reps give the same
  # declaration as 50 and take 7 s instead of 36.
  res <- ocd_wrapper(X_strong, mc_reps = 10)
  expect_ggcpt_contract(res, "ocd")
  expect_true("declared_at" %in% names(res$changepoints))
  expect_lte(nrow(res$changepoints), 3)
  expect_true(res$changepoints$cp[1] >= 80 && res$changepoints$cp[1] <= 110)
})

test_that("geomcp_wrapper labels distance/angle mappings", {
  skip_if_not_installed("changepoint.geo")
  set.seed(1)
  X_strong <- cbind(a = c(rnorm(80), rnorm(80, 5)),
                    b = c(rnorm(80), rnorm(80, -5)),
                    c = c(rnorm(80), rnorm(80, 3)))
  res <- geomcp_wrapper(X_strong)
  expect_ggcpt_contract(res, "geomcp")
  expect_true("mapping" %in% names(res$changepoints))
  expect_true(all(res$changepoints$mapping %in% c("distance", "angle", "both")))
  expect_true(any(abs(res$changepoints$cp - 80) <= 5))
})

test_that("strucchange_wrapper dates mean shifts with CIs", {
  skip_if_not_installed("strucchange")
  res <- strucchange_wrapper(x_step)
  expect_ggcpt_contract(res, "strucchange")
  expect_true(all(c("ci_lower", "ci_upper") %in% names(res$changepoints)))
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
})

test_that("strucchange_wrapper accepts a formula", {
  skip_if_not_installed("strucchange")
  df <- data.frame(y = x_step, t = seq_along(x_step))
  res <- strucchange_wrapper(y ~ 1, data = df)
  expect_s3_class(res, "ggcpt")
  expect_error(strucchange_wrapper(y ~ 1), "`data` must be supplied")
})

test_that("segmented_wrapper fits a broken line with CIs", {
  skip_if_not_installed("segmented")
  res <- segmented_wrapper(x_slope, npsi = 1, seed = 1)
  expect_ggcpt_contract(res, "segmented")
  expect_identical(res$change_in, "slope")
  expect_true("fitted" %in% names(res$data))
  expect_true(any(abs(res$changepoints$cp - 100) <= 10))
})

test_that("envcpt_wrapper picks a changepoint model when one exists", {
  skip_if_not_installed("EnvCpt")
  res <- suppressWarnings(envcpt_wrapper(x_step,
    models = c("mean", "meancpt", "trendcpt")))
  expect_ggcpt_contract(res, "envcpt")
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
  expect_match(res$penalty$type, "cpt")
})

test_that("fastcpd_wrapper detects mean changes", {
  skip_if_not_installed("fastcpd")
  res <- fastcpd_wrapper(x_step)
  expect_ggcpt_contract(res, "fastcpd")
  expect_true(any(abs(res$changepoints$cp - 100) <= 3))
})

test_that("dispatcher errors cleanly for univariate methods on wide input", {
  expect_error(cpt_detect(X_mv, method = "pelt"), "univariate")
})

test_that("dispatcher change_in validation errors instead of mislabelling", {
  expect_error(cpt_detect(x_step, method = "fpop", change_in = "var"),
               "not supported")
  skip_if_not_installed("not")
  res <- cpt_detect(c(rnorm(100, 0, 1), rnorm(100, 0, 6)),
                    method = "not", change_in = "var")
  expect_identical(res$change_in, "meanvar")
})

test_that("dispatcher routes slope requests to capable engines", {
  skip_if_not_installed("not")
  res <- cpt_detect(x_slope, method = "not", change_in = "slope")
  expect_identical(res$change_in, "slope")
  skip_if_not_installed("cpop")
  res2 <- cpt_detect(x_slope, method = "cpop", change_in = "slope")
  expect_identical(res2$change_in, "slope")
})

test_that("cpt_methods reports 31 available methods and NA for planned", {
  m <- cpt_methods()
  expect_gte(sum(m$status == "available"), 31)
  expect_true(all(is.na(m$installed[m$status == "planned"])))
  expect_false(any(is.na(m$installed[m$status == "available"])))
})
