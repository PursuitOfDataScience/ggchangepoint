# 0.5.0: supervised changepoint detection -- labels, label errors, and
# penalties learned from them.

set.seed(2026)
x_step <- c(rnorm(60), rnorm(60, 4))

test_that("cpt_labels builds and validates the label set", {
  labs <- cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
  expect_s3_class(labs, "cpt_labels")
  expect_equal(nrow(labs), 2)
  expect_true(all(c("label_id", "series", "start", "end", "change") %in%
                    names(labs)))
  expect_equal(labs$change, c("change", "no_change"))
  # A single `change` value is recycled.
  expect_equal(cpt_labels(c(1, 10), c(5, 20))$change, c("change", "change"))
  expect_error(cpt_labels(1:2, 1:3), "same length")
  expect_error(cpt_labels(10, 5), "end >= start")
  expect_error(cpt_labels(1, 5, "maybe"), "Unknown label")
  empty <- cpt_labels(integer(0), integer(0))
  expect_equal(nrow(empty), 0)
  expect_s3_class(empty, "cpt_labels")
})

test_that("as_cpt_labels turns a ground-truth set into labelled regions", {
  labs <- as_cpt_labels(c(50, 120), n = 200, margin = 5)
  pos <- labs[labs$change == "one_change", ]
  neg <- labs[labs$change == "no_change", ]
  expect_equal(nrow(pos), 2)
  expect_true(all(pos$start <= c(50, 120) & pos$end >= c(50, 120)))
  expect_gt(nrow(neg), 0)
  # Positive and negative regions must not overlap, or a detection would be
  # scored twice.
  for (i in seq_len(nrow(pos))) {
    expect_false(any(neg$start <= pos$end[i] & neg$end >= pos$start[i]))
  }
  expect_equal(nrow(as_cpt_labels(50, n = 200, negatives = FALSE)), 1)
})

test_that("cpt_label_error scores each of the three label kinds", {
  labs <- cpt_labels(c(1, 40, 100), c(30, 80, 120),
                     c("no_change", "one_change", "change"))
  # One detection inside label 2 only.
  err <- cpt_label_error(60L, labs)
  expect_equal(err$status, c("correct", "correct", "false_negative"))
  expect_equal(err$n_changes, c(0L, 1L, 0L))
  expect_equal(attr(err, "errors")[["total_errors"]], 1)

  # Two detections inside a one_change label is a false positive.
  err2 <- cpt_label_error(c(50L, 60L), labs)
  expect_equal(err2$status[2], "false_positive")
  # A detection inside a no_change label is a false positive.
  err3 <- cpt_label_error(c(10L, 60L), labs)
  expect_equal(err3$status[1], "false_positive")
  expect_output(print(err3), "cpt_label_error")

  fit <- cpt_detect(x_step, method = "pelt")
  expect_s3_class(cpt_label_error(fit, labs), "cpt_label_error")
  expect_error(cpt_label_error(fit, data.frame(a = 1)), "needs column")
})

test_that("the label error curve finds a target interval", {
  labs <- as_cpt_labels(60, n = 120)
  curve <- cpt_label_error_curve(x_step, labs,
                                 penalties = c(0.5, 2, 8, 32, 128))
  expect_s3_class(curve, "ggcpt_label_curve")
  expect_true(all(c("penalty", "n_cp", "errors", "false_positive",
                    "false_negative") %in% names(curve)))
  # More penalty can never give more changepoints.
  expect_true(all(diff(curve$n_cp) <= 0))
  tg <- attr(curve, "target")
  expect_length(tg, 2)
  expect_true(tg[1] < tg[2])
  expect_output(print(curve), "Target log-penalty interval")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(curve)))
  expect_error(cpt_label_error_curve(x_step, labs, penalties = c(-1, 1)),
               "must be positive")
})

test_that("cpt_learn_penalty fits, predicts and plugs into cpt_detect", {
  series <- list(a = c(rnorm(60), rnorm(60, 4)),
                 b = c(rnorm(80), rnorm(80, 2)),
                 c = c(rnorm(70), rnorm(70, 6)))
  labels <- list(a = as_cpt_labels(60, n = 120),
                 b = as_cpt_labels(80, n = 160),
                 c = as_cpt_labels(70, n = 140))
  model <- cpt_learn_penalty(series, labels, engine = "native",
                             penalties = c(1, 4, 16, 64, 256))
  expect_s3_class(model, "ggcpt_penalty_model")
  expect_equal(model$n_series, 3)
  expect_output(print(model), "ggcpt_penalty_model")
  expect_true(is.numeric(stats::coef(model)))

  pen <- stats::predict(model, series$a)
  expect_length(pen, 1)
  expect_gt(pen, 0)

  # The whole point: the model is usable wherever a penalty is.
  fit <- cpt_detect(series$a, method = "pelt", penalty = model)
  expect_s3_class(fit, "ggcpt")
  expect_equal(fit$penalty$type, "Manual")
  expect_equal(fit$penalty$value, unname(pen), tolerance = 1e-8)
  expect_equal(cpt_penalty(model, series = series$a), unname(pen))
  expect_error(cpt_penalty(model), "`series` must be supplied")
  # The wrappers are exported, so a model handed straight to one must be
  # resolved there too rather than reaching the engine as a list.
  skip_if_not_installed("fpop")
  direct <- fpop_wrapper(series$a, penalty = model)
  expect_equal(direct$penalty$value, unname(pen), tolerance = 1e-8)
  via_cpt <- cpt_wrapper(series$a, change_in = "mean", penalty = model)
  expect_s3_class(via_cpt, "tbl_df")
})

test_that("cpt_learn_penalty reports an unlearnable training set", {
  series <- list(a = rnorm(80), b = rnorm(80))
  labels <- list(a = cpt_labels(1, 79, "no_change"),
                 b = cpt_labels(1, 79, "no_change"))
  # Every penalty satisfies "no change here", so the target intervals are
  # unbounded on both sides and there is nothing to regress on.
  expect_error(
    cpt_learn_penalty(series, labels, penalties = c(10, 100, 1000)),
    "nothing to learn"
  )
})

test_that("label geoms and their scale build", {
  labs <- cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
  d <- data.frame(t = seq_along(x_step), y = x_step)
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_label(ggplot2::aes(xmin = start, xmax = end, fill = change),
                   data = labs) +
    ggplot2::geom_line() +
    scale_fill_cpt_label()
  expect_no_error(ggplot2::ggplot_build(p))

  err <- cpt_label_error(60L, labs)
  p2 <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_label(ggplot2::aes(xmin = start, xmax = end, fill = status),
                   data = err) +
    ggplot2::geom_line() +
    scale_fill_cpt_label()
  expect_no_error(ggplot2::ggplot_build(p2))
})

test_that("autoplot shades label outcomes behind the series", {
  labs <- cpt_labels(c(40, 80), c(70, 110), c("one_change", "no_change"))
  fit <- cpt_detect(x_step, method = "pelt")
  p <- ggplot2::autoplot(fit, labels = labs)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # The label rectangles are drawn first, beneath the series.
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  expect_no_error(built)
  # The fill is the scored status, not the raw assertion.
  fills <- unique(built$data[[1]]$fill)
  pal <- ggchangepoint:::cpt_label_palette()
  expect_true(all(fills %in% unname(pal)))
})
