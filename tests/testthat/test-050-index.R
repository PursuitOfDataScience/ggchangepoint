# 0.5.0: time indices and data structures. Detection stays on positions;
# everything reported gains the user's own scale.

set.seed(2026)
x_step <- c(rnorm(60), rnorm(60, 4))
dates <- as.Date("2020-01-01") + seq_along(x_step) - 1

test_that("a date index is carried through the whole result", {
  fit <- cpt_detect(x_step, method = "pelt", index = dates)
  expect_s3_class(fit$index, "Date")
  expect_equal(fit$data$index, seq_along(x_step))       # positions unchanged
  expect_true("index_value" %in% names(fit$data))
  td <- tidy(fit)
  expect_true("cp_index" %in% names(td))
  expect_s3_class(td$cp_index, "Date")
  expect_equal(td$cp_index, dates[td$cp])
  # cp_index sits next to cp, not after the engine extras.
  expect_equal(names(td)[1:2], c("cp", "cp_index"))
  expect_output(print(fit), "Index:")

  p <- ggplot2::autoplot(fit)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # The axis label follows the index rather than saying "Index".
  expect_true(p$labels$x %in% c("Index", "Time"))
})

test_that("index length and ordering are validated", {
  expect_error(cpt_detect(x_step, method = "pelt", index = dates[1:10]),
               "one value per observation")
  expect_error(cpt_detect(x_step, method = "pelt",
                          index = rev(dates)), "non-decreasing")
  expect_error(as_cpt_series(x_step, index = c(dates[-1], NA)), "must not contain NA")
  irregular <- cumsum(c(1, sample(c(1, 5), length(x_step) - 1, TRUE)))
  expect_warning(cpt_detect(x_step, method = "pelt", index = irregular),
                 "not equally spaced")
  expect_silent(as_cpt_series(x_step, index = irregular,
                              check_regular = FALSE))
})

test_that("ts, xts, zoo and tsibble input carry their own index", {
  tt <- stats::ts(x_step, frequency = 12, start = c(2000, 1))
  fit <- cpt_detect(tt, method = "pelt")
  expect_false(is.null(fit$index))
  expect_equal(length(fit$index), length(x_step))

  skip_if_not_installed("zoo")
  z <- zoo::zoo(x_step, order.by = dates)
  fz <- cpt_detect(z, method = "pelt")
  expect_s3_class(fz$index, "Date")
  expect_equal(fz$changepoints$cp, fit$changepoints$cp)

  skip_if_not_installed("xts")
  xx <- xts::xts(x_step, order.by = dates)
  fx <- cpt_detect(xx, method = "pelt")
  expect_equal(fx$changepoints$cp, fit$changepoints$cp)

  skip_if_not_installed("tsibble")
  tb <- tsibble::tsibble(day = dates, y = x_step, index = day)
  ft <- cpt_detect(tb, method = "pelt")
  expect_s3_class(ft$index, "Date")
  expect_equal(ft$changepoints$cp, fit$changepoints$cp)
})

test_that("the data-frame interface selects columns three ways", {
  df <- data.frame(day = dates, value = x_step, other = rnorm(length(x_step)))
  bare <- cpt_detect(df, y = value, index = day, method = "pelt")
  by_name <- cpt_detect(df, y = "value", index = "day", method = "pelt")
  by_pos <- cpt_detect(df, y = 2, index = 1, method = "pelt")
  expect_equal(bare$changepoints$cp, by_name$changepoints$cp)
  expect_equal(bare$changepoints$cp, by_pos$changepoints$cp)
  expect_s3_class(bare$changepoints$cp_index, "Date")

  expect_error(cpt_detect(df, y = nope, method = "pelt"), "no column called")
  expect_error(cpt_detect(df, y = day, method = "pelt"), "numeric column")
  expect_error(cpt_detect(x_step, y = value, method = "pelt"),
               "only meaningful when")
})

test_that("a bare data frame keeps its 0.4.0 meaning", {
  X <- data.frame(a = c(rnorm(60), rnorm(60, 3)), b = rnorm(120))
  # Two columns, no `y`: still a multivariate input, not a mis-parsed frame.
  expect_error(cpt_detect(X, method = "pelt"), "univariate")
  fit <- cpt_detect(X, method = "ecp")
  expect_s3_class(fit, "ggcpt")
  expect_false(is.null(fit$data_wide))
})

test_that("a one-coordinate result still plots as a single panel", {
  X <- matrix(x_step, ncol = 1, dimnames = list(NULL, "a"))
  fit <- cpt_detect(X, method = "ecp", index = dates)
  expect_equal(ggchangepoint:::n_coordinates(fit), 1L)
  p <- ggplot2::autoplot(fit)
  expect_no_error(ggplot2::ggplot_build(p))
  # index_value must not be mistaken for a coordinate.
  aug <- augment(fit)
  expect_true("index_value" %in% names(aug))
  expect_false(all(is.na(aug$.fitted)))
})

test_that("as_cpt_series returns values, index and a label", {
  s <- as_cpt_series(1:10)
  expect_equal(s$values, as.numeric(1:10))
  expect_null(s$index)
  s2 <- as_cpt_series(1:10, index = as.Date("2020-01-01") + 0:9)
  expect_s3_class(s2$index, "Date")
  expect_type(s2$index_label, "character")
})

test_that("a keyed tsibble is refused with an actionable message", {
  skip_if_not_installed("tsibble")
  tb <- tsibble::tsibble(
    day = rep(dates[1:10], 2), g = rep(c("a", "b"), each = 10),
    y = rnorm(20), index = day, key = g
  )
  expect_error(cpt_detect(tb, method = "pelt"), "keyed tsibble")
})

test_that("cpt_batch carries a time index onto every series", {
  X <- cbind(a = x_step, b = rev(x_step))
  b <- cpt_batch(X, method = "pelt", index = dates)
  expect_true(all(vapply(b$result, function(r) !is.null(r$index),
                         logical(1))))
  td <- tidy(b)
  expect_true("cp_index" %in% names(td))
  expect_s3_class(td$cp_index, "Date")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(b)))

  # A named list gives one index per series, matched by name.
  b2 <- cpt_batch(list(a = x_step, b = x_step[1:100]), method = "pelt",
                  index = list(a = dates, b = dates[1:100]))
  expect_equal(length(b2$result[["b"]]$index), 100)

  # No index: the 0.4.0 shape is unchanged.
  b3 <- cpt_batch(X, method = "pelt")
  expect_false("cp_index" %in% names(tidy(b3)))
})

test_that("a one-row data frame is refused as a series, not read as a column", {
  df1 <- data.frame(day = dates[1], value = 2)
  expect_error(cpt_detect(df1, y = value, method = "pelt"),
               "at least 3 observations")
})

test_that("cpt_select carries the time index onto the fit it chooses", {
  set.seed(91)
  n <- 200
  x <- c(stats::rnorm(n / 2), stats::rnorm(n / 2, 5))
  d <- as.Date("2020-01-01") + seq_len(n) - 1

  # supplied directly
  s <- cpt_select(x, method = "pelt", index = d)
  expect_s3_class(s$fit$index, "Date")
  expect_s3_class(tidy(s$fit)$cp_index, "Date")
  expect_equal(s$index, d)

  # inherited from an indexed ggcpt, which is the common path: reading only
  # x$data$value used to drop the index, so a selection made from a dated
  # fit came back plotted in positions
  f <- cpt_detect(x, method = "pelt", index = d)
  s2 <- cpt_select(f)
  expect_s3_class(s2$fit$index, "Date")
  expect_equal(tidy(s2$fit)$cp_index, tidy(f)$cp_index)

  # an explicit index still wins over the inherited one
  d2 <- d + 1000
  expect_equal(cpt_select(f, index = d2)$index, d2)

  # absent when not asked for, and checked when it is
  expect_null(cpt_select(x, method = "pelt")$fit$index)
  expect_error(cpt_select(x, method = "pelt", index = d[1:10]),
               "one value per observation|one entry per observation")

  # a ts carries its own index in
  st <- cpt_select(stats::ts(x, start = c(2020, 1), frequency = 12),
                   method = "pelt")
  expect_false(is.null(st$fit$index))
  expect_equal(length(st$fit$index), n)

  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(s)))
})

test_that("a factor index is read as labels, not as level codes", {
  # as.numeric() on a factor returns LEVEL positions -- alphabetical unless
  # the caller sets `levels` -- so the ordering check ran on the wrong
  # numbers: factor(month.abb) has codes 5, 4, 8, 1, 9, ... and was refused
  # as "not non-decreasing" while the identical labels as a character
  # vector were accepted.
  set.seed(31)
  x <- c(stats::rnorm(6), stats::rnorm(6, 4))
  labs <- month.abb

  chr <- cpt_detect(x, method = "pelt", index = labs)
  fct <- cpt_detect(x, method = "pelt", index = factor(labs))
  lvl <- cpt_detect(x, method = "pelt", index = factor(labs, levels = labs))
  # all three describe the same series, so they must agree
  expect_equal(tidy(fct)$cp, tidy(chr)$cp)
  expect_equal(as.character(tidy(fct)$cp_index),
               as.character(tidy(chr)$cp_index))
  expect_equal(as.character(tidy(lvl)$cp_index),
               as.character(tidy(chr)$cp_index))

  # An ORDERED factor does carry its order in its codes, so it keeps the
  # check: increasing codes are accepted, scrambled ones refused.
  expect_no_error(cpt_detect(x, method = "pelt",
                             index = factor(labs, levels = labs,
                                            ordered = TRUE)))
  expect_error(cpt_detect(x, method = "pelt",
                          index = factor(labs, levels = sort(labs),
                                         ordered = TRUE)),
               "non-decreasing")

  # and a missing label is still refused
  expect_error(cpt_detect(x, method = "pelt",
                          index = factor(replace(labs, 3, NA))),
               "must not contain NA")
})

test_that("every plot drawn against series position honours the index", {
  # autoplot(ggcpt_stability) was the one that did not: a dated series came
  # back in positions there while autoplot(fit), ggcpt_statistic(),
  # ggcpt_scale_space(), ggcpt_solution_path() and the influence and events
  # plots all showed dates.
  set.seed(81)
  x <- c(stats::rnorm(120), stats::rnorm(120, 4))
  d <- as.Date("2020-01-01") + seq_along(x) - 1
  xscale <- function(p) {
    class(ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]])[1]
  }

  plots <- list(
    ggcpt = ggplot2::autoplot(cpt_detect(x, method = "pelt", index = d)),
    stability = ggplot2::autoplot(
      cpt_stability(x, method = "pelt", B = 10, seed = 1, index = d)),
    influence = ggplot2::autoplot(
      cpt_influence(cpt_detect(x, method = "pelt", index = d), seed = 1)),
    events = ggplot2::autoplot(cpt_annotate_events(
      cpt_detect(x, method = "pelt", index = d),
      data.frame(cp = 120, label = "e")))
  )
  if (requireNamespace("mosum", quietly = TRUE)) {
    plots$scale_space <- ggcpt_scale_space(
      cpt_detect(x, method = "pelt", index = d), bandwidths = c(20, 40))
  }
  for (nm in names(plots)) {
    expect_equal(xscale(plots[[nm]]), "ScaleContinuousDate", info = nm)
  }

  # and without an index they all stay in positions
  plain <- list(
    ggcpt = ggplot2::autoplot(cpt_detect(x, method = "pelt")),
    stability = ggplot2::autoplot(
      cpt_stability(x, method = "pelt", B = 10, seed = 1))
  )
  for (nm in names(plain)) {
    expect_equal(xscale(plain[[nm]]), "ScaleContinuousPosition", info = nm)
  }
})
