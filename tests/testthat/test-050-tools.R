# 0.5.0: consensus and recommendation, events and reports, benchmarking,
# monitoring, and study design.

set.seed(2026)
x_step <- c(rnorm(80), rnorm(80, 4))
dates <- as.Date("2020-01-01") + seq_along(x_step) - 1

test_that("cpt_consensus resolves agreement into one segmentation", {
  cons <- cpt_consensus(x_step, methods = c("pelt", "binseg", "amoc"))
  expect_s3_class(cons, "ggcpt_consensus")
  expect_s3_class(cons, "ggcpt")
  cp <- tidy(cons)
  expect_true(all(c("cp", "votes", "methods", "spread") %in% names(cp)))
  expect_true(all(cp$votes >= 2))
  expect_true(all(cp$votes <= 3))
  expect_true(any(abs(cp$cp - 80) <= 5))
  expect_output(print(cons), "not a significance test")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(cons)))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(cons, plot_type = "agreement")))
})

test_that("consensus honours the vote threshold and the tolerance", {
  strict <- cpt_consensus(x_step, methods = c("pelt", "binseg", "amoc"),
                          min_votes = 3)
  loose <- cpt_consensus(x_step, methods = c("pelt", "binseg", "amoc"),
                         min_votes = 1)
  expect_lte(nrow(strict$changepoints), nrow(loose$changepoints))
  # A fraction is read as a proportion of the methods that ran.
  frac <- cpt_consensus(x_step, methods = c("pelt", "binseg", "amoc"),
                        min_votes = 2 / 3)
  expect_equal(frac$penalty$value, 2)
  expect_error(cpt_consensus(x_step, methods = "pelt"),
               "at least two detectors")
})

test_that("consensus clustering matches the metric's tolerance semantics", {
  cl <- ggchangepoint:::cluster_changepoints(
    list(a = c(10L, 50L), b = c(12L, 90L), c = 11L), tolerance = 5
  )
  expect_equal(nrow(cl), 3)
  expect_equal(cl$cp[1], 11L)
  expect_equal(cl$votes[1], 3L)
  expect_equal(cl$spread[1], 2L)
})

test_that("cpt_recommend is a decision table with reasons", {
  r <- cpt_recommend(noise = "autocorrelated")
  expect_s3_class(r, "ggcpt_recommendation")
  expect_true(all(c("method", "engine", "installed", "score", "why",
                    "caveat") %in% names(r)))
  expect_true(all(diff(r$score) <= 1e-8))
  expect_true(all(c("decafs", "envcpt") %in% utils::head(r$method, 5)))
  # The engines that assume iid noise must be flagged, not silently ranked.
  expect_true(!is.na(r$caveat[r$method == "pelt"]))
  expect_output(print(r), "Recommended methods")

  expect_true(all(cpt_recommend(dimension = "multivariate")$method %in%
                    subset(cpt_methods(), multivariate %in% TRUE)$method))
  expect_true(all(cpt_recommend(online = TRUE)$method %in%
                    subset(cpt_methods(), online %in% TRUE)$method))
  expect_true(all(cpt_recommend(need_uncertainty = TRUE)$method %in%
                    subset(cpt_methods(), ci %in% TRUE | posterior %in% TRUE)$method))
  expect_error(cpt_recommend(dimension = "univariate", change_in = "network"),
               "No wired method matches")
})

test_that("cpt_annotate_events reports all three outcomes", {
  fit <- cpt_detect(x_step, method = "pelt", index = dates)
  ev <- data.frame(when = as.Date(c("2020-03-20", "2020-05-01")),
                   what = c("policy change", "supply shock"))
  a <- cpt_annotate_events(fit, ev)
  expect_s3_class(a, "ggcpt_events")
  expect_equal(nrow(a$matched) + nrow(a$unexplained),
               nrow(fit$changepoints))
  td <- tidy(a)
  expect_true(all(td$status %in% c("matched", "unexplained_changepoint",
                                   "undetected_event")))
  expect_output(print(a), "Events with no changepoint")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(a)))

  # A position-scale event table works on a result with no index.
  plain <- cpt_detect(x_step, method = "pelt")
  a2 <- cpt_annotate_events(plain,
                            data.frame(pos = 80, what = "the change"))
  expect_equal(nrow(a2$matched), 1)
  expect_equal(a2$matched$distance, 0L)
  expect_error(cpt_annotate_events(plain, data.frame(a = "x")),
               "Could not find a location column")
})

test_that("cpt_report assembles a reproducible artifact", {
  fit <- cpt_detect(x_step, method = "pelt", index = dates)
  rep_md <- cpt_report(fit, session = FALSE)
  expect_type(rep_md, "character")
  expect_true(any(grepl("^# Changepoint analysis report", rep_md)))
  expect_true(any(grepl("Method: `pelt`", rep_md, fixed = TRUE)))
  expect_true(any(grepl("## Segments", rep_md, fixed = TRUE)))
  expect_true(any(grepl("Killick", rep_md)))          # the citation is in it
  expect_true(any(grepl("Index:", rep_md, fixed = TRUE)))

  txt <- cpt_report(fit, format = "text", session = FALSE)
  expect_false(any(grepl("^```$", txt)))

  f <- tempfile(fileext = ".md")
  on.exit(unlink(f), add = TRUE)
  invisible(cpt_report(fit, file = f, session = FALSE))
  expect_true(file.exists(f))
  expect_gt(length(readLines(f)), 10)

  st <- cpt_stability(x_step, method = "pelt", B = 5, seed = 1)
  with_st <- cpt_report(fit, stability = st, session = FALSE)
  expect_true(any(grepl("## Stability", with_st, fixed = TRUE)))
  expect_error(cpt_report(fit, stability = 1), "cpt_stability")
})

test_that("cpt_gt builds a table with or without gt", {
  fit <- cpt_detect(x_step, method = "pelt", index = dates)
  out <- cpt_gt(fit)
  if (requireNamespace("gt", quietly = TRUE)) {
    expect_s3_class(out, "gt_tbl")
  } else {
    expect_s3_class(out, "tbl_df")
  }
})

test_that("cpt_benchmark scores a grid and survives a failing engine", {
  ds <- cpt_datasets(n = 200, seed = 1, names = c("step", "teeth"))
  bm <- cpt_benchmark(ds, methods = c("pelt", "amoc"), progress = FALSE)
  expect_s3_class(bm, "ggcpt_benchmark")
  expect_equal(nrow(bm), 4)
  expect_true(all(c("dataset", "method", "covering", "f1", "n_cp",
                    "runtime", "error") %in% names(bm)))
  expect_true(all(bm$covering >= 0 & bm$covering <= 1))
  expect_true(all(is.na(bm$error)))
  expect_output(print(bm), "Mean rank")
  long <- tidy(bm)
  expect_equal(nrow(long), 8)
  for (ty in c("heatmap", "ranks", "critical_difference")) {
    expect_no_error(ggplot2::ggplot_build(
      ggplot2::autoplot(bm, plot_type = ty)))
  }

  # An engine that errors records the message instead of killing the grid.
  on.exit(try(cpt_unregister_method("boom"), silent = TRUE), add = TRUE)
  cpt_register_method("boom", function(x, ...) stop("engine exploded"))
  bad <- cpt_benchmark(ds, methods = c("pelt", "boom"), progress = FALSE)
  expect_equal(sum(!is.na(bad$error)), 2)
  expect_true(all(grepl("exploded", bad$error[!is.na(bad$error)])))
  expect_true(all(is.na(bad$covering[!is.na(bad$error)]) |
                    bad$covering[!is.na(bad$error)] >= 0))
})

test_that("multi-annotator datasets are scored as such", {
  ds <- list(a = list(series = c(rnorm(60), rnorm(60, 4)),
                      annotations = list(c(58), c(60), c(62))))
  bm <- cpt_benchmark(ds, methods = "pelt", progress = FALSE)
  expect_equal(bm$n_annotators, 3L)
  ann <- cpt_annotations(ds)
  expect_equal(nrow(ann), 3)
  expect_equal(sort(unique(ann$annotator)), c("1", "2", "3"))
})

test_that("cpt_datasets is offline, named and annotated", {
  ds <- cpt_datasets(n = 200, seed = 1)
  expect_true(all(vapply(ds, function(d) length(d$annotations) == 1,
                         logical(1))))
  expect_true(all(vapply(ds, function(d) length(d$series) >= 200,
                         logical(1))))
  expect_error(cpt_datasets(names = "nope"), "Unknown dataset")
})

test_that("the Nemenyi critical distance is the textbook one", {
  # Demsar (2006) Table 5: q_0.05 is 2.343 for k = 3 and 2.569 for k = 4,
  # so CD at k = 4, N = 10 is 2.569 * sqrt(4 * 5 / 60) = 1.4832.
  expect_equal(ggchangepoint:::nemenyi_cd(3, 10, 0.05),
               2.343 * sqrt(3 * 4 / 60), tolerance = 1e-3)
  expect_equal(ggchangepoint:::nemenyi_cd(4, 10, 0.05), 1.4832,
               tolerance = 1e-3)
  expect_lt(ggchangepoint:::nemenyi_cd(4, 100, 0.05),
            ggchangepoint:::nemenyi_cd(4, 10, 0.05))
})

test_that("the e-detector monitors, alarms and re-learns", {
  mon <- cpt_monitor("edetector", baseline = rnorm(200), alpha = 0.005)
  expect_s3_class(mon, "ggcpt_monitor")
  mon <- cpt_update(mon, rnorm(50))
  quiet_alarms <- nrow(alarms(mon))
  mon <- cpt_update(mon, rnorm(50, 5))
  expect_gt(nrow(alarms(mon)), quiet_alarms)
  expect_output(print(mon), "ggcpt_monitor")
  expect_true(all(c("time", "statistic", "threshold") %in%
                    names(alarms(mon))))
  # Re-learning stops one persistent change being reported as fifty.
  expect_lt(nrow(alarms(mon)), 10)
  expect_error(cpt_monitor("edetector", baseline = rnorm(3)),
               "at least 5 pre-change observations")
  expect_error(cpt_monitor("edetector", baseline = rep(1, 50)),
               "zero variability")
})

test_that("cpt_replay and cpt_delay account for delay and false alarms", {
  mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")
  al <- alarms(mon)
  expect_gt(nrow(al), 0)
  d <- cpt_delay(mon, truth = 200)
  expect_s3_class(d, "ggcpt_delay")
  expect_equal(d$n_changes, 1)
  expect_true(d$per_change$detected[1])
  expect_gte(d$per_change$delay[1], 0)
  expect_lt(d$per_change$delay[1], 60)
  expect_output(print(d), "Average run length")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(d)))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(mon)))
  expect_error(cpt_replay(rnorm(5)), "at least 10 observations")
})

test_that("an alarm before the change is a false alarm, not a detection", {
  al <- tibble::tibble(time = c(10L, 150L), statistic = c(1, 1),
                       threshold = c(1, 1))
  d <- cpt_delay(al, truth = 100)
  expect_equal(d$n_false_alarms, 1)
  expect_equal(d$per_change$alarm, 150L)
  expect_equal(d$per_change$delay, 50)
})

test_that("cpt_power reports power with a Monte Carlo error", {
  pw <- cpt_power(n = 200, jump = c(0.2, 3), n_sim = 20, seed = 1,
                  parallel = FALSE)
  expect_s3_class(pw, "ggcpt_power")
  expect_equal(nrow(pw), 2)
  expect_true(all(c("power", "mc_se", "mean_abs_error",
                    "false_positive_rate") %in% names(pw)))
  expect_true(all(pw$power >= 0 & pw$power <= 1))
  # Power must rise with the size of the change.
  expect_gt(pw$power[pw$jump == 3], pw$power[pw$jump == 0.2])
  expect_output(print(pw), "Monte Carlo standard errors")
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(pw)))
})

test_that("cpt_min_detectable brackets the target power", {
  md <- cpt_min_detectable(n = 200, n_sim = 20, range = c(0.2, 4),
                           max_iter = 4, seed = 1)
  expect_s3_class(md, "ggcpt_min_detectable")
  expect_true(is.na(md$jump) || (md$jump >= 0.2 && md$jump <= 4))
  expect_output(print(md), "Smallest detectable change")
  expect_error(cpt_min_detectable(n = 200, range = c(4, 1)),
               "increasing positive")

  # An unreachable target is reported, not silently returned as the maximum.
  hard <- cpt_min_detectable(n = 60, power = 0.999, range = c(0.05, 0.06),
                             n_sim = 10, max_iter = 2, seed = 1)
  expect_true(is.na(hard$jump))
  expect_match(hard$note, "Widen `range`")
})

test_that("cpt_scenarios builds a reproducible grid", {
  tab <- cpt_scenarios(n = 200, jump = c(1, 3), as_datasets = FALSE)
  expect_equal(nrow(tab), 2)
  expect_true(all(c("n", "jump", "location", "noise", "change_in",
                    "scenario") %in% names(tab)))
  ds <- cpt_scenarios(n = 200, jump = c(1, 3))
  expect_length(ds, 2)
  expect_equal(ds[[1]]$annotations[[1]], 100L)
  again <- cpt_scenarios(n = 200, jump = c(1, 3))
  expect_equal(ds[[1]]$series, again[[1]]$series)
  reps <- cpt_scenarios(n = 200, jump = 1, n_rep = 3)
  expect_length(reps, 3)
  expect_false(identical(reps[[1]]$series, reps[[2]]$series))
})

test_that("cpt_simulate gained seasonality and a smooth variance trend", {
  d <- cpt_simulate(240, changepoints = 120, params = c(0, 3),
                    seasonality = list(period = 12, amplitude = 2), seed = 1)
  expect_equal(attr(d, "true_changepoints"), 120L)
  plain <- cpt_simulate(240, changepoints = 120, params = c(0, 3), seed = 1)
  # The seasonal component is deterministic and added to the signal, so the
  # difference between the two series is exactly one sine wave.
  diff_series <- d$value - plain$value
  expect_equal(diff_series,
               2 * sin(2 * pi * (seq_len(240) - 1) / 12), tolerance = 1e-10)

  saw <- cpt_simulate(240, changepoints = 120, params = c(0, 3),
                      seasonality = list(period = 12, amplitude = 2,
                                         shape = "sawtooth"), seed = 1)
  expect_false(isTRUE(all.equal(saw$value, d$value)))

  dr <- cpt_simulate(400, changepoints = 200, params = c(0, 3),
                     sd_trend = c(0.5, 4), seed = 1)
  expect_lt(stats::sd(dr$value[1:100]), stats::sd(dr$value[301:400]))
  expect_error(cpt_simulate(100, sd_trend = c(-1, 2)), "two positive finite")
  expect_error(cpt_simulate(100, seasonality = list(amplitude = 1)),
               "seasonality\\$period")
  expect_warning(cpt_simulate(20, seasonality = list(period = 50,
                                                     amplitude = 1)),
                 "exceeds the series length")
})

test_that("geom_cpt_event maps x from xintercept and does not inherit", {
  d <- data.frame(t = seq_along(x_step), y = x_step)
  ev <- data.frame(x = 80, label = "policy change")
  p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line() +
    geom_cpt_event(ggplot2::aes(xintercept = x, label = label), data = ev,
                   repel = FALSE)
  # Two regressions in one plot: `$x` partially matches `xintercept` on a
  # mapping, so the alias was silently skipped and the text layer went out
  # with no x; and inheriting the series' aesthetics looks for columns the
  # event frame does not have.
  built <- ggplot2::ggplot_build(p)
  txt <- built$data[[3]]
  expect_equal(txt$x, 80)
  expect_equal(txt$label, "policy change")
  expect_error(geom_cpt_event(data = ev), "needs a mapping")
  # `repel = TRUE` is only buildable where ggrepel is installed; assert the
  # right behaviour in both worlds rather than skipping one of them.
  repel_layer <- function() {
    geom_cpt_event(ggplot2::aes(xintercept = x, label = label), data = ev,
                   repel = TRUE)
  }
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    expect_no_error(repel_layer())
  } else {
    expect_error(repel_layer(), "needs the ggrepel package")
  }
  # and the default resolves itself either way
  expect_no_error(
    geom_cpt_event(ggplot2::aes(xintercept = x, label = label), data = ev))
})

test_that("cpt_load_tcpd needs the network and is not run here", {
  skip_on_cran()
  skip_if_offline <- function() {
    skip_if_not_installed("jsonlite")
    skip("TCPD download is not exercised in the test suite")
  }
  skip_if_offline()
})

test_that("ggcpt_interactive offers both renderers", {
  fit <- cpt_detect(x_step, method = "pelt")
  skip_if_not_installed("ggiraph")
  expect_s3_class(ggcpt_interactive(fit, engine = "ggiraph"), "girafe")
  expect_error(ggcpt_interactive(1:10, engine = "ggiraph"),
               "must be a ggcpt object or a ggplot")
})

test_that("the progressr path works with and without a handler", {
  skip_if_not_installed("progressr")
  ds <- cpt_datasets(n = 200, seed = 1, names = c("step", "teeth"))
  # progress = TRUE is the default and was otherwise never exercised:
  # progressr::progressor() has context requirements that only bite when it
  # is actually called.
  expect_no_error(cpt_benchmark(ds, methods = c("pelt", "amoc")))
  expect_no_error(
    progressr::with_progress(
      cpt_benchmark(ds, methods = c("pelt", "amoc"))
    )
  )
})

test_that("cpt_power refuses an unsupported method/change combination", {
  # Each replicate would error, each error would be caught, and the run
  # would report NaN power -- which reads as "no power" rather than
  # "impossible request".
  expect_error(cpt_power(n = 100, jump = 1, method = "pelt",
                         change_in = "slope", n_sim = 2),
               "not supported for method")
  expect_error(cpt_power(n = 100, jump = 1, change_in = "distribution",
                         n_sim = 2), "should be one of")
})

test_that("the e-detector respects its average-run-length bound", {
  skip_on_cran()
  # The whole claim of cpt_monitor("edetector") is E_inf[tau] >= 1/alpha,
  # which rests on M_t - t being a mean-zero martingale under the null.
  # Averaging the per-shift statistics preserves that; taking their MAXIMUM
  # does not, and an earlier version did, which put the in-control alarm
  # rate at roughly 1.8x the bound. Measure it.
  set.seed(3)
  n <- 3000
  in_control_alarms <- function(alpha, reps = 6) {
    mean(vapply(seq_len(reps), function(i) {
      mon <- cpt_monitor("edetector", baseline = stats::rnorm(300),
                         alpha = alpha, relearn = 0)
      nrow(alarms(cpt_update(mon, stats::rnorm(n))))
    }, numeric(1)))
  }
  # ARL >= 1/alpha means at most n * alpha alarms in n in-control
  # observations; allow a little Monte Carlo slack on 6 replicates.
  expect_lte(in_control_alarms(0.01), n * 0.01 * 1.35)
  expect_lte(in_control_alarms(0.001), n * 0.001 * 2)

  # And it must still detect a real change quickly.
  mon <- cpt_replay(c(stats::rnorm(300), stats::rnorm(300, 3)),
                    method = "edetector")
  d <- cpt_delay(mon, truth = 300)
  expect_equal(d$n_detected, 1)
  expect_lt(d$per_change$delay[1], 30)
})

test_that("cpt_confint reports an un-re-runnable result in its own terms", {
  # A result assembled from outside cannot be bootstrapped, because the
  # bootstrap re-runs the detector by name. Saying so in terms of
  # `method = "auto"` beats complaining about a bootstrap the caller never
  # asked for.
  fit <- as_ggcpt(60, c(rnorm(60), rnorm(60, 4)), method = "somebody_elses")
  expect_error(cpt_confint(fit), "has nothing to fall back on")
  expect_error(cpt_confint(fit, method = "bootstrap"),
               "is not a method cpt_detect\\(\\) knows")
  # With an interval supplied it works.
  with_ci <- as_ggcpt(60, c(rnorm(60), rnorm(60, 4)), method = "somebody_elses",
                      ci = cbind(55, 65))
  expect_equal(cpt_confint(with_ci)$source, "native")

  # Registering the detector makes the bootstrap route available again.
  on.exit(try(cpt_unregister_method("somebody_elses"), silent = TRUE),
          add = TRUE)
  cpt_register_method("somebody_elses", function(x, ...) 60)
  expect_s3_class(cpt_confint(fit, method = "bootstrap", B = 5, seed = 1),
                  "tbl_df")
})

test_that("cpt_cite points a user-supplied result at the right fix", {
  fit <- as_ggcpt(60, c(rnorm(60), rnorm(60, 4)), method = "somebody_elses")
  expect_error(cpt_cite(fit), "cpt_register_method")
})

test_that("consensus clustering anchors on the first member", {
  # Anchoring on the PREVIOUS detection would chain 1-6-11-16 into a single
  # cluster of width 15 at tolerance 5, which is not what "within 5" means.
  chain <- list(a = 1L, b = 6L, c = 11L, d = 16L)
  cl <- ggchangepoint:::cluster_changepoints(chain, tolerance = 5)
  expect_gt(nrow(cl), 1)
  expect_true(all(cl$spread <= 5))
})

test_that("subsetting a result tibble drops the class when it must", {
  set.seed(1)
  ds <- list(a = list(series = c(rnorm(60), rnorm(60, 4)),
                      annotations = list(60)),
             b = list(series = c(rnorm(60), rnorm(60, 2)),
                      annotations = list(60)))
  bm <- cpt_benchmark(ds, methods = c("pelt", "amoc"), progress = FALSE)

  # Rows only: the invariants hold, so the class survives and print() works.
  expect_s3_class(bm[1, ], "ggcpt_benchmark")
  expect_s3_class(bm[, names(bm)], "ggcpt_benchmark")

  # Columns away: the class must go, or print.ggcpt_benchmark() reaches for
  # an `error` column that is no longer there and warns "Unknown or
  # uninitialised column" on what looks like an ordinary select.
  sub <- bm[, c("dataset", "method", "covering")]
  expect_false(inherits(sub, "ggcpt_benchmark"))
  expect_s3_class(sub, "tbl_df")
  # A tibble prints, of course; what it must not do is warn.
  expect_warning(print(sub), NA)

  labs <- cpt_labels(c(10, 40), c(30, 60))
  err <- cpt_label_error(cpt_detect(ds$a$series, method = "pelt"), labs)
  b <- cpt_batch(cbind(a = ds$a$series, b = ds$b$series), method = "pelt")
  r <- cpt_recommend()
  cv <- cpt_label_error_curve(ds$a$series, labs, penalties = c(2, 20, 200))
  for (obj in list(labs, err, b, r, cv)) {
    keep <- class(obj)[1]
    trimmed <- obj[, 1, drop = FALSE]
    expect_false(inherits(trimmed, keep), info = keep)
    expect_warning(print(trimmed), NA, info = keep)
  }
})

test_that("a monitor refuses a feed of the wrong width", {
  set.seed(1)
  uni <- cpt_monitor("edetector", baseline = rnorm(80))
  expect_error(cpt_update(uni, cbind(rnorm(20), rnorm(20))),
               "1 coordinate")
  # and still accepts the shape it was built for
  expect_s3_class(cpt_update(uni, rnorm(20)), "ggcpt_monitor")
  expect_s3_class(cpt_update(uni, matrix(rnorm(20), ncol = 1)),
                  "ggcpt_monitor")

  skip_if_not_installed("ocd")
  # ocd is dimensioned at construction; a narrower feed used to be consumed
  # silently, which means it was reading the wrong coordinates. An explicit
  # `thresh` keeps the test about the width check: the default "MC" rule
  # calibrates by Monte Carlo and takes over a minute.
  mv <- cpt_monitor("ocd", baseline = matrix(rnorm(240), ncol = 3),
                    thresh = c(10, 10, 10))
  expect_error(cpt_update(mv, cbind(rnorm(10), rnorm(10))),
               "3 coordinate")
  expect_s3_class(cpt_update(mv, matrix(rnorm(30), ncol = 3)),
                  "ggcpt_monitor")
})

# ---------------------------------------------------------------------------
# Parallel execution: a method registered in this session has to survive the
# trip to a future worker. Workers load the package fresh, so the registry
# environment inside the namespace starts empty there.
# ---------------------------------------------------------------------------

test_that("session-registered methods work under a multisession plan", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  # A worker loads the *installed* package, not the sources devtools might
  # have loaded here, so the test is only meaningful when the two agree.
  skip_if_not(nzchar(system.file(package = "ggchangepoint")))
  skip_if_not(identical(
    as.character(utils::packageVersion("ggchangepoint")),
    as.character(utils::packageDescription("ggchangepoint")$Version)))
  skip_if(is.null(suppressWarnings(
    tryCatch(future::plan(future::multisession, workers = 2),
             error = function(e) NULL))))
  withr::defer(future::plan(future::sequential))

  set.seed(11)
  X <- cbind(a = c(stats::rnorm(60), stats::rnorm(60, 5)),
             b = c(stats::rnorm(60), stats::rnorm(60, -5)))
  cpt_register_method("wt_parallel",
                      function(x, ...) which.max(abs(diff(as.numeric(x)))),
                      engine = "stats")
  withr::defer(cpt_unregister_method("wt_parallel"))

  b <- cpt_batch(X, method = "wt_parallel", seed = 1)
  expect_s3_class(b, "ggcpt_batch")
  expect_equal(nrow(b), 2L)
  expect_true(all(b$n_changepoints == 1L))

  cons <- cpt_consensus(X[, 1], methods = c("pelt", "wt_parallel"),
                        min_votes = 1, seed = 1)
  expect_s3_class(cons, "ggcpt_consensus")
  expect_true(nrow(cons$changepoints) >= 1L)
  # the registered method has to have voted, not merely not crashed
  expect_true("wt_parallel" %in% attr(cons, "consensus")$methods)
})

test_that("the registry snapshot round-trips without touching the parent", {
  cpt_register_method("wt_snapshot", function(x, ...) 5L, engine = "stats")
  withr::defer(try(cpt_unregister_method("wt_snapshot"), silent = TRUE))

  snap <- ggchangepoint:::registry_snapshot()
  expect_true("wt_snapshot" %in% names(snap))
  cpt_unregister_method("wt_snapshot")
  expect_false("wt_snapshot" %in% cpt_registered_methods()$method)
  ggchangepoint:::registry_restore(snap)
  expect_true("wt_snapshot" %in% cpt_registered_methods()$method)

  # With nothing registered the wrapper is a no-op, so parallel calls keep
  # exactly the globals they had before.
  cpt_unregister_method("wt_snapshot")
  f <- function(i) i
  expect_identical(ggchangepoint:::with_session_registry(f), f)
})

test_that("cpt_benchmark accepts `changepoints` and warns when truth is missing", {
  set.seed(12)
  x <- c(stats::rnorm(80), stats::rnorm(80, 5))
  b <- cpt_benchmark(list(s = list(series = x, changepoints = 80L)),
                     methods = "pelt", progress = FALSE, parallel = FALSE)
  expect_equal(b$n_annotators, 1L)
  expect_false(is.na(b$covering))

  # several annotators supplied through `truth`
  b2 <- cpt_benchmark(list(s = list(series = x, truth = list(78L, 82L))),
                      methods = "pelt", progress = FALSE, parallel = FALSE)
  expect_equal(b2$n_annotators, 2L)

  expect_warning(
    cpt_benchmark(list(s = list(series = x, cps = 80L)), methods = "pelt",
                  progress = FALSE, parallel = FALSE),
    "no ground truth"
  )
  # a bare vector is legitimately unlabelled and must stay quiet
  expect_silent(cpt_benchmark(list(s = x), methods = "pelt", progress = FALSE,
                              parallel = FALSE))
})

test_that("cpt_batch(keep_fit = FALSE) drops the engine fits and nothing else", {
  set.seed(41)
  X <- cbind(a = c(stats::rnorm(60), stats::rnorm(60, 5)),
             b = c(stats::rnorm(60), stats::rnorm(60, -5)))
  full <- cpt_batch(X, method = "pelt")
  lean <- cpt_batch(X, method = "pelt", keep_fit = FALSE)

  expect_false(is.null(full$result[[1]]$fit))
  expect_true(all(vapply(lean$result, function(r) is.null(r$fit), logical(1))))
  # everything the object is for is unchanged
  expect_identical(full$series, lean$series)
  expect_identical(full$n_changepoints, lean$n_changepoints)
  expect_equal(tidy(full), tidy(lean))
  expect_lt(as.numeric(utils::object.size(lean)),
            as.numeric(utils::object.size(full)))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(lean)))
  expect_error(cpt_batch(X, method = "pelt", keep_fit = "yes"), "keep_fit")
})

test_that("cpt_recommend flags the engines with a real practical limit", {
  mv <- cpt_recommend(dimension = "multivariate", change_in = "mean")
  pil <- mv[mv$method == "pilliat", ]
  expect_equal(nrow(pil), 1L)
  expect_match(pil$caveat, "power of two")
  # esac is the alternative, and must not carry the same caveat
  expect_true(is.na(mv$caveat[mv$method == "esac"]))

  long <- cpt_recommend(n = 8000)
  expect_match(long$caveat[long$method == "strucchange"], "large raw fit")
  expect_true(is.na(long$caveat[long$method == "pelt"]))
  # and not at a length where it does not matter
  short <- cpt_recommend(n = 200)
  expect_true(is.na(short$caveat[short$method == "strucchange"]))
})

test_that("the monitor API rejects the inputs that would score nonsense", {
  set.seed(71)
  base <- stats::rnorm(200)
  stream <- c(stats::rnorm(100), stats::rnorm(200, 4))

  # an NA baseline must be named as such: sd() of a vector holding an NA is
  # itself NA, so the variability check used to blame a flat baseline
  expect_error(cpt_monitor("edetector", baseline = c(base, NA)),
               "must be finite")
  expect_error(cpt_monitor("edetector", baseline = rep(1, 100)),
               "zero variability")

  mon <- cpt_replay(stream, method = "edetector", baseline = base)
  expect_error(cpt_delay(mon), "`truth` is required")
  expect_error(cpt_delay(mon, truth = 10000), "past the end of the stream")
  expect_error(cpt_delay(mon, truth = integer(0)), "positive changepoint")
  expect_error(cpt_delay(mon, truth = 0), "positive changepoint")

  d <- cpt_delay(mon, truth = 100)
  expect_s3_class(d, "ggcpt_delay")
  expect_equal(d$n_changes, 1L)
  expect_equal(d$n_obs, length(stream))
  expect_true(is.finite(d$mean_delay) || d$n_detected == 0L)

  # an empty batch is an explicit no-op, not a state change
  m0 <- cpt_monitor("edetector", baseline = base)
  expect_identical(cpt_update(m0, numeric(0)), m0)
})

# ---------------------------------------------------------------------------
# tidy() is the package's advertised accessor, but only half the result
# classes had a method: tidy(cpt_power(...)) errored while
# tidy(cpt_benchmark(...)) worked. Every class that holds rows now answers.
# ---------------------------------------------------------------------------

test_that("tidy() works on every result class the package returns", {
  set.seed(81)
  x <- c(stats::rnorm(100), stats::rnorm(100, 5))
  X <- cbind(a = x, b = c(stats::rnorm(100), stats::rnorm(100, -4)))
  fit <- cpt_detect(x, method = "pelt")
  labs <- cpt_labels(c(60, 140), c(90, 190), c("one_change", "no_change"))

  objs <- list(
    ggcpt = fit,
    ggcpt_batch = cpt_batch(X, method = "pelt"),
    ggcpt_benchmark = cpt_benchmark(
      list(s = list(series = x, changepoints = 100L)),
      methods = "pelt", progress = FALSE, parallel = FALSE),
    ggcpt_selection = cpt_select(x, method = "pelt"),
    ggcpt_influence = cpt_influence(fit, subset = seq(1, 200, by = 40)),
    ggcpt_sensitivity = cpt_sensitivity(fit,
                                        over = list(minseglen = c(2L, 10L))),
    ggcpt_consensus = cpt_consensus(x, methods = c("pelt", "binseg", "amoc"),
                                    min_votes = 2),
    ggcpt_power = cpt_power(n = 60, jump = c(1, 3), n_sim = 4, seed = 1,
                            progress = FALSE),
    ggcpt_monitor = cpt_replay(x, method = "edetector",
                               baseline = stats::rnorm(100)),
    ggcpt_recommendation = cpt_recommend(),
    cpt_labels = labs,
    cpt_label_error = cpt_label_error(fit, labs),
    ggcpt_events = cpt_annotate_events(fit, data.frame(when = 100,
                                                       what = "shock"))
  )
  objs$ggcpt_delay <- cpt_delay(objs$ggcpt_monitor, truth = 100)

  for (nm in names(objs)) {
    t <- generics::tidy(objs[[nm]])
    expect_s3_class(t, "data.frame", exact = FALSE)
    expect_false(inherits(t, nm) && nm != "ggcpt",
                 info = paste(nm, "tidy() should not return its own subclass"))
  }

  # glance() on a delay object is a genuine one-row summary
  g <- generics::glance(objs$ggcpt_delay)
  expect_equal(nrow(g), 1L)
  expect_equal(g$n_changes, 1L)
  expect_equal(g$n_obs, length(x))

  # the tibble subclasses hand back a plain tibble with the extra attributes
  # stripped, so printing the result cannot reach for a column that is gone
  tp <- generics::tidy(objs$ggcpt_power)
  expect_identical(class(tp), c("tbl_df", "tbl", "data.frame"))
  expect_null(attr(tp, "method"))
  expect_equal(nrow(tp), nrow(objs$ggcpt_power))
})

test_that("every result class round-trips through saveRDS", {
  set.seed(82)
  x <- c(stats::rnorm(80), stats::rnorm(80, 5))
  fit <- cpt_detect(x, method = "pelt")
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, f)
  back <- readRDS(f)
  expect_equal(fit, back)
  expect_no_error(capture.output(print(back)))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(back)))
  expect_equal(tidy(fit), tidy(back))
})

test_that("detection leaves no global state behind", {
  set.seed(83)
  x <- c(stats::rnorm(80), stats::rnorm(80, 5))
  opts_before <- options()
  search_before <- search()
  invisible(cpt_detect(x, method = "pelt"))
  invisible(suppressWarnings(cpt_methods()))
  opts_after <- options()
  changed <- names(opts_before)[!vapply(names(opts_before), function(k)
    identical(opts_before[[k]], opts_after[[k]]), logical(1))]
  expect_equal(changed, character(0))
  expect_equal(setdiff(names(opts_after), names(opts_before)), character(0))
  # fabisearch attaches NMF for the duration of its call and must detach it
  expect_equal(setdiff(search(), search_before), character(0))
})

test_that("a monitor fed one at a time equals cpt_replay and a batched feed", {
  set.seed(84)
  base <- stats::rnorm(150)
  stream <- c(stats::rnorm(120), stats::rnorm(180, 3.5))
  # "edetector" is native; "cpm" needs its engine, so run it only where the
  # engine is present rather than asserting it unconditionally.
  methods <- c("edetector",
               if (requireNamespace("cpm", quietly = TRUE)) "cpm")
  for (m in methods) {
    replayed <- cpt_replay(stream, method = m, baseline = base)
    one_by_one <- cpt_monitor(m, baseline = base)
    for (v in stream) one_by_one <- cpt_update(one_by_one, v)
    batched <- cpt_monitor(m, baseline = base)
    for (k in split(stream, ceiling(seq_along(stream) / 17))) {
      batched <- cpt_update(batched, k)
    }
    # The state machine must not care how the data is chopped up: a detector
    # whose alarms move with the batch size is carrying per-call state it
    # should not have.
    expect_identical(alarms(replayed)$time, alarms(one_by_one)$time,
                     info = m)
    expect_identical(alarms(one_by_one)$time, alarms(batched)$time, info = m)
    expect_identical(one_by_one$t, batched$t)
  }
})

test_that("cpt_metrics matches values worked out by hand", {
  # perfect agreement
  m <- cpt_metrics(50L, 50L, n = 100, margin = 0)
  expect_equal(m$covering, 1); expect_equal(m$f1, 1)
  expect_equal(m$precision, 1); expect_equal(m$recall, 1)
  expect_equal(m$hausdorff, 0)

  # found nothing where there was a change
  m <- cpt_metrics(integer(0), 50L, n = 100, margin = 0)
  expect_equal(m$recall, 0); expect_equal(m$f1, 0)

  # one true positive inside the margin plus one false positive
  m <- cpt_metrics(c(48L, 90L), 50L, n = 100, margin = 5)
  expect_equal(m$precision, 1 / 2)
  expect_equal(m$recall, 1)
  expect_equal(m$f1, 2 * (0.5 * 1) / (0.5 + 1))

  # covering, computed by hand: truth partitions {1..50},{51..100}; the guess
  # partitions {1..40},{41..100}. |A1| = 50 overlaps {1..40} in 40 of a union
  # of 50; |A2| = 50 overlaps {41..100} in 50 of a union of 60.
  m <- cpt_metrics(40L, 50L, n = 100, margin = 0)
  expect_equal(m$covering, (50 * (40 / 50) + 50 * (50 / 60)) / 100)

  # both sides say "no change": agreement, not vacuity
  m <- cpt_metrics(integer(0), integer(0), n = 100)
  expect_equal(m$covering, 1); expect_equal(m$f1, 1)

  expect_equal(cpt_metrics(30L, 50L, n = 100, margin = 0)$hausdorff, 20)
})

test_that("the Nemenyi critical difference matches the published table", {
  # Demsar (2006) Table 5: q_0.05 for the two-tailed Nemenyi test.
  q05 <- c(`2` = 1.960, `3` = 2.343, `4` = 2.569, `5` = 2.728, `10` = 3.164)
  N <- 20
  for (k in names(q05)) {
    kk <- as.integer(k)
    expect_equal(ggchangepoint:::nemenyi_cd(kk, N, alpha = 0.05),
                 unname(q05[[k]]) * sqrt(kk * (kk + 1) / (6 * N)),
                 tolerance = 5e-4, info = paste("k =", kk))
  }
  # the difference shrinks as the number of datasets grows, and grows with
  # the number of methods compared
  expect_lt(ggchangepoint:::nemenyi_cd(5, 100), ggchangepoint:::nemenyi_cd(5, 20))
  expect_gt(ggchangepoint:::nemenyi_cd(10, 20), ggchangepoint:::nemenyi_cd(5, 20))
  expect_gt(ggchangepoint:::nemenyi_cd(5, 20, alpha = 0.01),
            ggchangepoint:::nemenyi_cd(5, 20, alpha = 0.05))
})

test_that("the selection criteria compute what their references define", {
  set.seed(85)
  n <- 200
  y <- c(stats::rnorm(n / 2), stats::rnorm(n / 2, 4))
  gauss <- ggchangepoint:::gaussian_cost
  zs <- ggchangepoint:::zhang_siegmund_penalty

  # -2 log-likelihood of a Gaussian with a per-segment mean and one common
  # variance, profiled: n log(RSS / n)
  expect_equal(gauss(y, integer(0)),
               n * log(sum((y - mean(y))^2) / n))
  cp <- c(60L, 130L)
  b <- c(0L, cp, n)
  rss <- sum(vapply(seq_len(3), function(i) {
    s <- y[(b[i] + 1):b[i + 1]]; sum((s - mean(s))^2)
  }, numeric(1)))
  expect_equal(gauss(y, cp), n * log(rss / n))
  expect_lt(gauss(y, cp), gauss(y, integer(0)))

  # Killick and Eckley (2014, JSS) state Zhang and Siegmund's penalty as
  # 3k log n + sum log((tau_i - tau_{i-1}) / n).
  expect_equal(zs(cp, n), 3 * 2 * log(n) + sum(log(diff(b) / n)))
  expect_equal(zs(integer(0), n), 0)
  # out-of-range candidates are dropped, not counted
  expect_equal(zs(c(0L, 60L, n), n), zs(60L, n))

  # and the assembled objective agrees with changepoint's own MBIC search:
  # its answer must beat both one more and one fewer changepoint
  skip_if_not_installed("changepoint")
  ref <- as.integer(changepoint::cpts(
    changepoint::cpt.mean(y, method = "PELT", penalty = "MBIC")))
  skip_if(length(ref) == 0)
  obj <- function(k) gauss(y, k) + zs(k, n)
  expect_lt(obj(ref), obj(sort(c(ref, 20L))))
  expect_lt(obj(ref), obj(ref[-1]))
})

test_that("knee_point finds the corner and refuses to invent one", {
  kn <- ggchangepoint:::knee_point
  # an L-shaped cost curve with its elbow at the fourth rung
  expect_equal(kn(1:9, c(100, 55, 30, 18, 15, 14, 13.5, 13.2, 13.1)), 4L)
  # a straight line is equidistant from its own chord everywhere, so the
  # rule must not pretend to have found a corner
  expect_equal(kn(1:9, seq(10, 2, length.out = 9)), 1L)
  expect_equal(kn(1:5, rep(3, 5)), 1L)
  # too few rungs to have an interior point at all
  expect_equal(kn(1:2, c(5, 1)), 2L)
})
