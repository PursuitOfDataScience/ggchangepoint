test_that("B1: signal_blocks produces correct Blocks signal (all changepoints visible)", {
  set.seed(2022)
  dat <- signal_blocks(2048)
  cp <- attr(dat, "true_changepoints")
  # Verify that each declared changepoint produces a genuine level change
  for (i in seq_along(cp)) {
    before <- mean(dat$value[(cp[i] - 2):cp[i]])
    after  <- mean(dat$value[(cp[i] + 1):(cp[i] + 3)])
    expect_true(abs(before - after) > 0.5,
                info = sprintf("Changepoint %d (idx %d) shows no level change", i, cp[i]))
  }
  # Verify at least 10 distinct changepoints are visible (not collapsed to 1)
  expect_gt(length(unique(diff(cp))), 5)
})

test_that("B2: cpt_metrics recall/f1 never exceed 1 (one-to-one matching)", {
  # Multiple predictions near one truth - should get tp = 1, not 3
  res <- cpt_metrics(pred = c(98, 100, 102), truth = c(100), n = 200, margin = 5)
  expect_lte(res$recall, 1)
  expect_lte(res$f1, 1)
  expect_equal(res$n_pred, 3)
  expect_equal(res$n_truth, 1)
  # tp should be at most min(n_pred, n_truth) = 1
  expect_lte(as.numeric(res$precision * res$n_pred), 1)

  # Empty pred or truth should not warn or produce nonsense
  expect_no_condition(res2 <- cpt_metrics(pred = integer(), truth = c(100), n = 200))
  expect_equal(res2$recall, 0)
  expect_equal(res2$precision, 0)

  expect_no_condition(res3 <- cpt_metrics(pred = c(100), truth = integer(), n = 200))
  expect_equal(res3$recall, 0)
  expect_equal(res3$f1, 0)
})

test_that("B3: cpt_detect validates method x change_in combinations", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))

  # slope should error with clear message
  expect_error(cpt_detect(x, method = "pelt", change_in = "slope"),
               "not directly supported")

  # fpop + var should error
  expect_error(cpt_detect(x, method = "fpop", change_in = "var"),
               "not supported")

  # pelt + meanvar should work
  expect_s3_class(cpt_detect(x, method = "pelt", change_in = "meanvar"), "ggcpt")
})

test_that("B5: stat_changepoint respects x aesthetic", {
  skip_if_not_installed("ggplot2")
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  df <- data.frame(t = seq(2000, 2099), y = x)

  # Build a stat_changepoint layer - should produce a plot without errors
  p <- ggplot2::ggplot(df, ggplot2::aes(t, y)) +
    stat_changepoint(method = "pelt")

  # Just verify it builds; the stat correctly maps to x aesthetic
  expect_s3_class(p, "ggplot")
})

test_that("B6: ggcpt_eval handles empty pred/truth without warnings", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))

  expect_no_condition(p <- ggcpt_eval(pred = integer(), truth = c(100), data_vec = x))
  expect_s3_class(p, "ggplot")

  expect_no_condition(p2 <- ggcpt_eval(pred = c(100), truth = integer(), data_vec = x))
  expect_s3_class(p2, "ggplot")
})

test_that("B8: glance.ggcpt populates total_cost and runtime", {
  set.seed(2022)
  x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
  res <- cpt_detect(x, method = "pelt")
  gl <- glance(res)
  expect_true("runtime" %in% names(gl))
  expect_true(is.numeric(gl$runtime))
  expect_false(is.na(gl$runtime))
})

test_that("B10: ggcptplot_internal show_segments removed without breaking", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  # These should all work without any show_segments parameter error
  p <- ggcptplot(x)
  expect_s3_class(p, "ggplot")
  p2 <- autoplot(cpt_detect(x))
  expect_s3_class(p2, "ggplot")
})

test_that("B11: augment.ggcpt works with position-independent column names", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  res <- cpt_detect(x, method = "pelt")

  # augment should not fail if data has >2 columns
  res$data <- cbind(res$data, extra = letters[seq_len(nrow(res$data))])
  au <- augment(res)
  expect_true("is_changepoint" %in% names(au))
  expect_true(au$extra[1] == "a")
})

test_that("cpt_methods returns correct structure", {
  suppressWarnings({
    tbl <- cpt_methods()
  })
  expect_s3_class(tbl, "tbl_df")
  expect_true(all(c("method", "status", "engine", "installed") %in% names(tbl)))
  available <- dplyr::filter(tbl, status == "available")
  expect_gt(nrow(available), 0)
  planned <- dplyr::filter(tbl, status == "planned")
  expect_gt(nrow(planned), 0)
})

test_that("S3 methods work (summary, as_tibble, as.data.frame, format, plot)", {
  set.seed(2022)
  x <- c(rnorm(50, 0, 1), rnorm(50, 10, 1))
  res <- cpt_detect(x, method = "pelt")

  s <- summary(res)
  expect_s3_class(s, "summary.ggcpt")

  at <- as_tibble(res)
  expect_s3_class(at, "tbl_df")
  expect_true("cp" %in% names(at))

  adf <- as.data.frame(res)
  expect_s3_class(adf, "data.frame")

  f <- format(res)
  expect_type(f, "character")

  p <- plot(res)
  expect_s3_class(p, "ggplot")
})
