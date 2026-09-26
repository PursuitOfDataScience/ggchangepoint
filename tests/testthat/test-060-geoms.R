# 0.6.0: the layers are ggproto extensions with their own glyphs (§111
# item on the grammar-of-graphics layer; §141 found three exported layers
# no test had ever called directly).

layer_data_of <- function(p, i = 1L) ggplot2::layer_data(p, i)

test_that("every layer is built on the package's own ggproto object", {
  expect_s3_class(GeomChangepoint, "Geom")
  expect_s3_class(GeomCptSegment, "Geom")
  expect_s3_class(GeomCptCi, "Geom")
  expect_s3_class(GeomCptRegion, "Geom")
  expect_s3_class(GeomCptLabel, "Geom")
  expect_s3_class(GeomCptEvent, "Geom")
  expect_s3_class(StatChangepoint, "Stat")
  expect_s3_class(StatCptRegion, "Stat")
  expect_s3_class(geom_changepoint(xintercept = 5)$geom, "GeomChangepoint")
  expect_s3_class(geom_cpt_segment()$geom, "GeomCptSegment")
  expect_s3_class(geom_cpt_ci()$geom, "GeomCptCi")
  expect_s3_class(geom_cpt_region()$geom, "GeomCptRegion")
  expect_s3_class(stat_cpt_region()$stat, "StatCptRegion")
})

test_that("geom_changepoint() draws a rule per changepoint", {
  d <- data.frame(t = 1:100, y = c(rep(0, 50), rep(4, 50)))
  cp <- data.frame(cp = c(30, 50))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line() +
    geom_changepoint(ggplot2::aes(xintercept = cp), data = cp,
                     colour = "blue")
  ld <- layer_data_of(p, 2)
  expect_equal(ld$xintercept, c(30, 50))
  expect_true(all(ld$colour == "blue"))
  expect_silent(ggplot2::ggplotGrob(p))
  # geom_vline()'s shorthand still works.
  p2 <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + geom_changepoint(xintercept = 50)
  expect_equal(layer_data_of(p2)$xintercept, 50)
  # inherit.aes through `...` reaches layer(), not the geom's parameters.
  expect_false(geom_changepoint(ggplot2::aes(xintercept = cp), data = cp,
                                inherit.aes = FALSE)$inherit.aes)
})

test_that("geom_cpt_segment() and geom_cpt_ci() draw levels and intervals", {
  set.seed(90)
  fit <- cpt_detect(c(stats::rnorm(50), stats::rnorm(50, 4)), method = "pelt")
  p <- ggplot2::ggplot(fit$data, ggplot2::aes(index, value)) +
    ggplot2::geom_line() +
    geom_cpt_segment(ggplot2::aes(x = start, xend = end, y = param_estimate,
                                  yend = param_estimate),
                     data = fit$segments, colour = "red")
  ld <- layer_data_of(p, 2)
  expect_equal(nrow(ld), nrow(fit$segments))
  expect_equal(ld$x, fit$segments$start)
  ci <- data.frame(xmin = c(45, 70), xmax = c(56, 75), y = 0)
  p2 <- ggplot2::ggplot(fit$data, ggplot2::aes(index, value)) +
    geom_cpt_ci(ggplot2::aes(xmin = xmin, xmax = xmax, y = y), data = ci,
                inherit.aes = FALSE)
  ld2 <- layer_data_of(p2)
  expect_equal(ld2$xmin, c(45, 70))
  expect_equal(ld2$xmax, c(56, 75))
  expect_silent(ggplot2::ggplotGrob(p2))
})

test_that("geom_cpt_region() spans the panel unless a height is mapped", {
  d <- data.frame(t = 1:100, y = stats::rnorm(100))
  reg <- data.frame(xmin = c(20, 60), xmax = c(30, 70))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax), data = reg) +
    ggplot2::geom_line()
  ld <- layer_data_of(p)
  expect_true(all(ld$ymin == -Inf & ld$ymax == Inf))
  expect_true(all(ld$fill == "steelblue"))
  # A mapped fill wins over the default.
  reg$kind <- c("a", "b")
  p2 <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax, fill = kind),
                    data = reg)
  expect_equal(length(unique(layer_data_of(p2)$fill)), 2L)
  # A mapped height is honoured.
  reg$lo <- -1
  reg$hi <- 1
  expect_no_warning(p3 <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = lo,
                                 ymax = hi), data = reg))
  expect_equal(layer_data_of(p3)$ymin, c(-1, -1))
  expect_silent(ggplot2::ggplotGrob(p3))
})

test_that("geom_cpt_event() draws a rule and its label in one layer", {
  d <- data.frame(t = 1:100, y = stats::rnorm(100))
  ev <- data.frame(x = c(30, 70), label = c("launch", "recall"))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line() +
    geom_cpt_event(ggplot2::aes(xintercept = x, label = label), data = ev,
                   repel = FALSE)
  ld <- layer_data_of(p, 2)
  expect_equal(ld$xintercept, c(30, 70))
  expect_equal(ld$label, c("launch", "recall"))
  g <- ggplot2::ggplotGrob(p)
  expect_s3_class(g, "gtable")
  expect_error(geom_cpt_event(), class = "ggchangepoint_bad_argument")
})

test_that("each layer has its own legend glyph", {
  data <- data.frame(colour = "red", linewidth = 0.5, linetype = 1,
                     alpha = NA, fill = "steelblue")
  for (key in list(ggchangepoint:::draw_key_cpt_rule,
                   ggchangepoint:::draw_key_cpt_ci,
                   ggchangepoint:::draw_key_cpt_region,
                   ggchangepoint:::draw_key_cpt_event)) {
    expect_s3_class(key(data, list(), c(1, 1)), "grob")
  }
  own <- function(g) get("draw_key", envir = g, inherits = FALSE)
  expect_identical(own(GeomChangepoint), ggchangepoint:::draw_key_cpt_rule)
  expect_identical(own(GeomCptCi), ggchangepoint:::draw_key_cpt_ci)
  expect_identical(own(GeomCptRegion), ggchangepoint:::draw_key_cpt_region)
  expect_identical(own(GeomCptEvent), ggchangepoint:::draw_key_cpt_event)
  # A legend built from a mapped colour uses the rule glyph.
  d <- data.frame(t = 1:50, y = stats::rnorm(50))
  cp <- data.frame(cp = c(10, 30), kind = c("a", "b"))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
    geom_changepoint(ggplot2::aes(xintercept = cp, colour = kind), data = cp)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("stat_cpt_region() detects inside the layer", {
  skip_on_cran()
  skip_if_not(engine_usable("stepR"))
  set.seed(91)
  d <- data.frame(t = 101:300, y = c(stats::rnorm(100), stats::rnorm(100, 3)))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line() +
    stat_cpt_region(method = "smuce")
  ld <- layer_data_of(p, 2)
  expect_equal(nrow(ld), 1L)
  # In the plot's x units, not positions.
  expect_true(ld$xmin <= 200 && ld$xmax >= 200)
  expect_true(ld$xmin > 150)
  # A method with neither regions nor intervals says so.
  p2 <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + stat_cpt_region(method = "pelt")
  expect_error(ggplot2::ggplot_build(p2), "regions")
})

test_that("stat_changepoint() puts rules at the detected changes", {
  skip_on_cran()
  set.seed(92)
  d <- data.frame(t = 1:100, y = c(stats::rnorm(50), stats::rnorm(50, 5)))
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + stat_changepoint()
  ld <- layer_data_of(p)
  expect_true(any(abs(ld$xintercept - 50) <= 2))
})
