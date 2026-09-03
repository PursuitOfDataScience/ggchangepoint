# plot() is the reflex before anyone reads the documentation. Every result
# class with an autoplot() method must answer it, and must answer it with
# ITS OWN figure -- a plot method that hard-calls autoplot.ggcpt() looks
# fine on a ggcpt and silently draws the wrong plot for a subclass.

set.seed(2026)
x_step <- c(rnorm(80), rnorm(80, 4))

test_that("every class with an autoplot method has a plot method", {
  ns_path <- system.file("NAMESPACE", package = "ggchangepoint")
  skip_if(!nzchar(ns_path))
  ns <- readLines(ns_path)

  classes_for <- function(generic) {
    pat <- sprintf("^S3method\\(%s,(.+)\\)$", generic)
    sub(pat, "\\1", grep(pat, ns, value = TRUE))
  }
  autoplotted <- classes_for("autoplot")
  plotted <- classes_for("base::plot")

  expect_gt(length(autoplotted), 10)
  expect_equal(setdiff(autoplotted, plotted), character(0))
})

test_that("plot() draws the class's own autoplot and returns it invisibly", {
  ds <- cpt_datasets(n = 200, seed = 1, names = c("step", "teeth"))
  labs <- as_cpt_labels(80, n = length(x_step))
  fit <- cpt_detect(x_step, method = "pelt")
  mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")

  objs <- list(
    ggcpt = fit,
    ggcpt_selection = cpt_select(x_step, method = "pelt", criterion = "bic",
                                 k_max = 4),
    ggcpt_stability = cpt_stability(x_step, method = "pelt", B = 20, seed = 1),
    ggcpt_sensitivity = cpt_sensitivity(x_step, method = "pelt",
                                        over = list(penalty = c(5, 10, 20))),
    ggcpt_influence = cpt_influence(fit),
    ggcpt_batch = cpt_batch(cbind(a = ds$step$series, b = ds$teeth$series),
                            method = "pelt"),
    ggcpt_benchmark = cpt_benchmark(ds, methods = c("pelt", "amoc"),
                                    progress = FALSE),
    ggcpt_consensus = cpt_consensus(x_step, methods = c("pelt", "binseg",
                                                        "amoc")),
    ggcpt_monitor = mon,
    ggcpt_delay = cpt_delay(mon, truth = 200),
    ggcpt_path = cpt_crops(x_step, pen_min = 1, pen_max = 30),
    ggcpt_power = cpt_power(n = 80, jump = 2, n_sim = 5, parallel = FALSE,
                            seed = 1),
    ggcpt_events = cpt_annotate_events(
      fit, events = data.frame(cp = 80, label = "shift")),
    ggcpt_label_curve = cpt_label_error_curve(x_step, labs,
                                              penalties = c(0.5, 2, 8, 32, 128))
  )

  for (cls in names(objs)) {
    obj <- objs[[cls]]
    expect_s3_class(obj, cls)

    # Rendering, not just building: a geom that draws nothing complains at
    # draw time, so a test that stops at ggplot_build() never sees it.
    # cpt_power(jump = <one value>) used to render an empty line layer and
    # advise adjusting the group aesthetic.
    expect_no_message(plot(obj))
    expect_no_warning(plot(obj))

    res <- withVisible(plot(obj))
    expect_s3_class(res$value, "ggplot", exact = FALSE)
    # Drawn as a side effect, so plot() works in a loop; returned so that
    # p <- plot(x) still gives something to add layers to.
    expect_false(res$visible)

    # The figure must be the one autoplot() gives for THIS class, not the
    # one a parent class would give.
    expect_equal(res$value$labels, ggplot2::autoplot(obj)$labels, info = cls)
    expect_no_error(ggplot2::ggplot_build(res$value))
  }
})
