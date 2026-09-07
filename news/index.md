# Changelog

## ggchangepoint 0.5.0

The release that fills in what 0.4.0’s engine wave left open: inference,
selection, diagnostics, supervised detection, time indices, streaming,
benchmarking, and an extension mechanism that makes the
CRAN-availability question stop being a blocker.
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
goes from 31 to 50 wired methods, and the surface around the detectors
roughly doubles.

### The extension mechanism

The highest-leverage addition, and the one everything else leans on.

- New
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  /
  [`cpt_unregister_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  /
  [`cpt_registered_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  teach
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  about a detector this package does not (and often cannot) depend on:
  an engine that is not on CRAN, a Python detector reached through
  `reticulate`, a neural detector, a proprietary in-house method. The
  registered method then works with
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  the geoms,
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`glance()`](https://generics.r-lib.org/reference/glance.html)/[`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  and
  [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).
- New
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  turns any set of changepoints — a published paper’s reported breaks,
  an analyst’s annotations, another package’s output — into a validated
  `ggcpt`, running the same contract checks as every built-in wrapper.
- Registered methods are **visibly** user-supplied:
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  gives them `status = "registered"`,
  [`print()`](https://rdrr.io/r/base/print.html) marks their results,
  and
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
  returns the citation the registration supplied or states plainly that
  none was given.

### The engine registry

- The wired-method table, the capability check and the dispatcher’s
  routing are now all derived from one declarative registry, so a new
  engine declares its capabilities once instead of in three places that
  had to be kept in agreement by hand.
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  gains capability columns: `multivariate`, `univariate`, `online`,
  `ci`, `fitted`, `posterior`, `statistic`, `path`, `scale_space`.
  `subset(cpt_methods(), ci)$method` answers “which methods give me a
  confidence interval?” directly.
- New
  [`cpt_install_engines()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_install_engines.md)
  installs a whole family of engines at once (`"core"`, `"bayesian"`,
  `"nonparametric"`, `"highdim"`, `"functional"`, `"regression"`,
  `"inference"`, `"applied"`, `"time"`, `"reporting"`, or `"all"`), with
  a `dry_run`.

### Time indices and data structures

- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  gains `index`: detection still runs on positions — every wrapped
  engine assumes an equally spaced sequence — but the index is stored on
  the result and threaded through
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) (as
  `cp_index`),
  [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  (axis and labels),
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md),
  [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  and
  [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).
  An index that is not equally spaced warns rather than silently
  mislabelling the axis.
- `ts`, `xts`, `zoo` and (unkeyed) `tsibble` objects are accepted
  directly and their own index is carried through. New
  [`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md)
  is the one place that separates the values from the clock.
- New data-frame interface: `cpt_detect(df, y = value, index = date)`,
  where `y` and `index` accept a bare column name, a string or a
  position. A data frame passed without `y` keeps its 0.4.0 meaning.

### Inference

- New
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
  / `cpt_detect(method = "nsp")` wraps Narrowest Significance Pursuit
  (Fryzlewicz 2024): intervals each guaranteed to contain at least one
  changepoint at a prescribed **global** level, with self-normalised and
  autoregressive variants for heavy tails, heteroscedasticity and serial
  dependence.
- New optional `regions` slot on `ggcpt`, read with
  [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md),
  drawn by the new
  [`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md)
  layer and by `autoplot(show_regions =)` — which is on by default for a
  result that has regions. NSP’s `cp` column is the interval midpoint
  and says so, in the `cp_source` column, in
  [`print()`](https://rdrr.io/r/base/print.html), and in the
  documentation: the region is the inferential object, the midpoint is
  not an estimate.
- New
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  answers “where could this changepoint be?” for any result, behind one
  contract with four provenances — `"native"` (the engine’s own
  interval), `"posterior"`, `"bootstrap"` (within-segment resampling,
  available for every engine) and `"nsp"` — and reports which one it
  used in a `source` column.
- New
  [`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md)
  attaches a test to each changepoint or segment, using the engine’s own
  test where it has one (`strucchange`’s Chow F, `segmented`’s Davies
  test) and an explicitly unadjusted Welch two-sample test where it does
  not. A `selection_adjusted` column and a warning make the difference
  impossible to miss, because a p-value computed at a location chosen
  from the same data is anti-conservative.

### Choosing the number of changepoints

- New
  [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  builds one candidate ladder and scores it by any of six criteria:
  `"bic"`, `"mbic"` (the real Zhang–Siegmund segment-length mBIC, which
  [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  cannot express), `"aic"`, `"crops_elbow"` (the knee rule made explicit
  and citable rather than eyeballed), `"cv"` (order-preserved
  cross-validation via `crossvalidationCP` — the criterion with a
  consistency proof) and `"stability"`.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on the result draws the criterion curve, the chosen segmentation, or —
  the new display — a **ladder** of small multiples showing how the
  segmentation coarsens as K falls.

### Diagnostics

- New
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  implements the Wilms–Killick–Matteson influence family: delete and
  outlier perturbation, re-rendered in ggplot2 with
  `plot_type = "overview" | "location" | "parameter" | "map"`. It uses
  `changepoint.influence` where that applies and a generic recomputation
  everywhere else, so it works for every wired and registered method.
- New
  [`cpt_leverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_leverage.md)
  ranks observations by a composite influence score.
- New
  [`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  sweeps tuning parameters and shows the detected locations across the
  grid — the direct answer to “is this robust to the penalty?”.
- New
  [`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  /
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  return and draw the detector’s criterion as a function of location;
  new
  [`cpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  /
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  return and draw the order in which candidates entered the model; new
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  /
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  sweep a multiscale detector’s bandwidth and draw the
  location-by-bandwidth heatmap.
  `autoplot(fit, type = "statistic" | "path" | "scale_space")` reaches
  all three. An engine that exposes nothing errors with the list of
  engines that do.

### Supervised detection

- New
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md)
  and
  [`as_cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_labels.md)
  build labelled regions — the ground-truth representation shared with
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md),
  so the package has one notion of an annotation rather than two.
- New
  [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  scores a segmentation in label errors;
  [`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md)
  traces them across a penalty grid and reports the target interval.
- New
  [`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
  fits the max-margin interval regression of Hocking et al. (2013),
  delegating to `penaltyLearning` when it is installed and falling back
  to a built-in squared-hinge fit. The result has
  [`predict()`](https://rdrr.io/r/stats/predict.html), and
  `cpt_detect(x, penalty = model)` and `cpt_penalty(model, series = x)`
  accept it directly — as do the wrappers that take a numeric penalty.
- New
  [`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md)
  draws the labels, and
  [`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md)
  colours them by assertion or by correct / false-positive /
  false-negative status.

### Choosing and combining methods

- New
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  runs several detectors and reports the locations they agree on, with a
  vote count and the methods behind each. Matching reuses
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)’s
  tolerance rule, so the package has one notion of “the same
  changepoint”. The documentation and the print method both state that
  agreement is a robustness display and **not** a significance test.
- New
  [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  turns the capability matrix into advice: given the dimension, the
  change type, the noise structure, the series length and whether
  uncertainty or an online alarm is needed, it returns a ranked
  shortlist with a reason and a caveat for each.

### Communication

- New
  [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  matches detected changepoints to a table of known events and reports
  all three outcomes: matched, unexplained changepoints, and undetected
  events. Events may be given on the position scale or on the result’s
  own index. New
  [`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md)
  draws them.
- New
  [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md)
  assembles a reproducible artifact — method, citation, penalty,
  locations with intervals, regions, segments, optional stability and
  events, the call, and
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) — as
  markdown or plain text.
- New
  [`cpt_gt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gt.md)
  renders a publication-ready changepoint table through `gt`, degrading
  to a tibble with a note when `gt` is absent.

### Benchmarking and evaluation

- New
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  runs a method-by-dataset grid, scores every cell with
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  (or
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
  when a dataset has several annotators), and records an engine failure
  as a message instead of losing the run.
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  gives a heatmap, a rank plot, or the Demšar critical-difference
  diagram.
- New
  [`cpt_datasets()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_datasets.md)
  builds an offline, deterministic collection from the package’s own
  canonical signals — so the benchmark runs inside `R CMD check`.
- New
  [`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md)
  downloads and caches the Turing Change Point Dataset under
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), with
  its multi-annotator ground truth intact; new
  [`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md)
  returns the per-annotator sets one row at a time, so the disagreement
  between annotators stays visible.

### Streaming and online monitoring

- New
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  creates a stateful sequential detector, fed by
  [`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md)
  and read with
  [`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md).
  Three methods: `cpm`, `ocd`, and `edetector`.
- Selecting columns off one of the new result tibbles
  (`ggcpt_benchmark`, `ggcpt_batch`, `ggcpt_recommendation`,
  `ggcpt_label_curve`, `cpt_labels`, `cpt_label_error`) now drops the
  class rather than keeping a fragment that its own
  [`print()`](https://rdrr.io/r/base/print.html) method cannot read.
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  on one of these behaves the same way;
  [`filter()`](https://rdrr.io/r/stats/filter.html) and row indexing
  keep the class, as they should.
- `edetector` is a **native** implementation of the mixture
  Shiryaev–Roberts e-detector of Shin, Ramdas and Rinaldo (2023) — a
  deliberate, separately scoped exception to this package’s
  wrap-don’t-implement rule, taken because no R package implements
  e-detectors and the construction is short enough to audit. Under the
  null the mixed statistic `M_t` satisfies `E[M_t] = t`, so optional
  stopping at the alarm time gives a finite-sample lower bound of
  `1 / alpha` on the in-control average run length, with no calibration
  run. The shifts are combined by **averaging**, not by taking a
  maximum: a convex combination of e-detectors is an e-detector and a
  maximum is not, and the test suite measures the in-control alarm rate
  against the bound rather than taking the derivation on trust. It is
  labelled as native wherever it appears.
- A monitor re-learns its baseline after an alarm (`relearn`), so a
  persistent change is reported once rather than on every subsequent
  observation. It is also dimensioned at construction: feeding
  [`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md)
  a different number of coordinates is an error rather than a silent
  coercion.
- New
  [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md)
  runs a whole series through a monitor; new
  [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  scores it the way the sequential literature does — detection delay per
  change, false alarms, and the average run length — instead of asking
  whether a location was recovered, which a sequential procedure never
  claims.

### Simulation, power and study design

- New
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  reports detection probability, location error and false positives
  across a scenario grid, with the Monte Carlo standard error attached
  and drawn as a band.
- New
  [`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md)
  inverts it: the smallest change reaching a target power, for
  pre-registration and study design.
- New
  [`cpt_scenarios()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scenarios.md)
  builds a reproducible grid of simulation settings as data, ready for
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).
- [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  gains `seasonality` (sine or sawtooth) and `sd_trend` (smoothly
  varying noise scale, distinct from the piecewise-constant
  `change_in = "var"`), so the conditions the dependence-aware and
  seasonal engines exist for can actually be simulated.

### Engine wave [\#2](https://github.com/PursuitOfDataScience/ggchangepoint/issues/2) — 19 new methods

[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
reaches 50 wired methods. New `change_in` levels `"covariance"`,
`"network"`, `"regression"` and `"seasonality"` come with them, and the
capability matrix was extended in lockstep.

- **Inference:** `nsp` (`nsp`).
- **Bayesian:** `mcp` (`mcp`) — formula-based multiple-changepoint
  regression with full posteriors. Needs JAGS, a system dependency, and
  says so plainly when it is missing.
- **High-dimensional:** `esac` and `pilliat` (`HDCD`) for
  sparsity-adaptive mean changes; `hdcov`, `network`, `var` and `hdreg`
  (`changepoints`) for changes in covariance, dynamic-network structure,
  VAR(1) dynamics and the coefficients of a sparse high-dimensional
  regression — changes no mean-change engine can see.
- **Functional and network:** `fmean` and `fcov` (`fChange`); `kwc`
  (`KWCChangepoint`), robust depth-rank segmentation; `fabisearch`
  (`fabisearch`), network structure via non-negative matrix
  factorisation.
- **Applied vocabularies:** `pettitt`, `buishand` and `snht` (`trend`) —
  the hydrology and climatology standards, each with a valid p-value
  because the location was not chosen from a model search; `taylor`
  (`ChangePointTaylor`) — the quality-control default, with bootstrap
  confidence per changepoint; `bfast` (`bfast`) — season-and-trend
  breaks for remote sensing.
- **Nonstationary and fast:** `wbsts` (`wbsts`) for second-order
  changes; `binsegrcpp` (`binsegRcpp`) as a fast binary-segmentation
  path across several loss functions.

### Accessibility

- [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
  gains `engine = "ggiraph"` alongside the existing path. ggiraph
  renders the ggplot itself to interactive SVG, so facets and every
  layer survive — which ’s own model does not always manage for a
  faceted multivariate result.

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  gains `labels =`: pass a
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md)
  set and the labelled regions are shaded behind the series and coloured
  by outcome (correct, false positive, false negative), so scoring
  against expert labels is a picture rather than a table.

- New
  [`scale_colour_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  /
  [`scale_fill_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  /
  [`scale_linetype_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  provide an Okabe–Ito palette that stays distinguishable under the
  three common forms of colour-vision deficiency.
  `ggcpt_compare(layout = "overlay")` now maps linetype as well as
  colour, so the panel reads in greyscale.

- Every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a `ggcpt` carries generated alt text, which knitr and Quarto pass
  through to the rendered image.

### Fixes found in the post-implementation audit

- A method registered with
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  is now visible inside `future` workers. The registry lives in the
  package namespace and a worker loads the package fresh, so
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md),
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md),
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md),
  [`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  and
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  used to fail on a registered method under `plan(multisession)` with a
  misleading “‘arg’ should be one of” error. Each now carries a snapshot
  of the registry to the worker.
- [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  accepts `changepoints` as ground truth alongside `truth` and
  `annotations`, treats a list-valued `truth` as several annotators
  rather than flattening it, and **warns** when a list dataset carries
  none of the three — previously it returned a full benchmark table in
  which every metric was silently `NA`.
- [`mcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mcp_wrapper.md)
  works, and says so honestly when it cannot. Two faults, both of which
  had gone unnoticed because the engine needs JAGS and its only test was
  the negative one that skips when *is* installed. First, the default
  segment model is plateau-only (`list(y ~ 1, ~ 1)`), so could not
  derive its x-axis variable from the formulas and stopped with “This is
  a plateau-only model”; the wrapper now names the data frame’s `t`
  column via `par_x` unless the caller supplies their own. Second,
  having the *package* is not the same as being able to *run* it —
  installs on some platforms and only fails when it looks for the JAGS
  library at run time, in which case
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html)
  returns a fit with no posterior samples and a warning, and
  [`summary()`](https://rdrr.io/r/base/summary.html) on that died with
  “subscript out of bounds”. The wrapper checks for the samples and
  reports the real cause. The documented claim that “will not install at
  all” without JAGS was wrong, and is corrected; the example is
  `\dontrun{}` because no test of installed R packages predicts whether
  a system library can be reached.
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  no longer loads every engine to find out which ones are installed. It
  asked [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html),
  which loads the package, so building the table pulled in all
  thirty-five namespaces — including , by way of , which fails outright
  on a machine with no OpenGL.
  [`find.package()`](https://rdrr.io/r/base/find.package.html) answers
  the question without touching anything: the call drops from seconds to
  hundredths of a second and loads nothing. Wrappers still load their
  engine when they actually need it.
- [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  validates the shape of its input before requiring the engine, so
  asking for `method = "mosum"` with a matrix says that mosum is
  univariate rather than telling you to install a package that could not
  have accepted the input anyway.
- [`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md)
  refuses a dimension that is an exact power of two. `HDCD` 1.1’s
  `Pilliat()` builds one fewer partial-sum threshold than it uses at
  those dimensions, so it reported a changepoint at *every* observation
  — on pure noise as readily as on a real change — for p = 2, 4, 8, 16,
  32, 64 and 128. The wrapper now says so and points at `esac`, which is
  unaffected; the refusal lifts automatically once a fixed `HDCD` is
  installed.
- [`fabisearch_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fabisearch_wrapper.md)
  rejects an all-zero time point with a message that names the offending
  rows, instead of letting NMF’s own error surface several layers down.
- [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  reads NSP’s own intervals. NSP reports an interval that provably
  contains a change, under `region_start`/`region_end`;
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  looked only for `ci_lower`/`ci_upper` and so bootstrapped 200 re-runs
  of the detector to produce a weaker statement than the one already on
  the object. The `source` column now distinguishes `"nsp_region"` from
  `"native"`, and reports NSP’s global level.
- `bfast_wrapper(change_in = "seasonality")` works. reports “no
  breakpoints in this component” as a bare `NA` rather than an empty
  `breakpoints` object, so the declared capability errored with
  `$ operator is invalid for atomic vectors` on any series whose
  seasonal amplitude is stable. Asking for seasonal breaks with
  `season = "none"` is now an error rather than a puzzle.
- `binsegrcpp` no longer claims a variance-only change. has no
  variance-only cost, so `change_in = "var"` was mapped to a
  distribution the engine does not have; `"mean"` and `"meanvar"` are
  what it offers.
- [`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md)
  validates `n_bootstraps` against the engine’s real range (100 to
  1,000,000) instead of letting a smaller value fail inside
  `ChangePointTaylor` with a message about its own misspelled argument.
- New `cpt_batch(keep_fit = FALSE)` drops each engine’s raw fit. A few
  engines return fits far larger than the data — measured on a
  2000-point series, `strucchange` costs about 135 MB (a triangular
  O(n^2) RSS matrix), `bfast` 53 MB and `bocpd` 31 MB, while every other
  engine stays under 4 MB — and a panel multiplies that by the number of
  series.
  [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  now carries both this and pilliat’s dimension restriction as caveats.
- [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  gains an `index` argument and inherits one from an indexed `ggcpt`. It
  previously read only the values off its input, so a selection made
  from a dated fit came back reporting positions.
- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) now works
  on every result class the package returns. `ggcpt_influence`,
  `ggcpt_power`, `ggcpt_monitor`, `ggcpt_delay`, `ggcpt_recommendation`,
  `cpt_labels` and `cpt_label_error` had no method, so
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) failed on
  half the surface;
  [`glance()`](https://generics.r-lib.org/reference/glance.html) also
  reports the one-row summary a `ggcpt_delay` already carries.
- [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  names a missing value in the baseline instead of reporting it as zero
  variability, and
  [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  refuses a `truth` that falls past the end of the stream, is empty, or
  is non-positive — each of which used to be scored as a clean miss.
- The e-detector’s average-run-length bound is attributed to optional
  stopping on (M_t - t) everywhere it is described. The README and the
  `alpha` parameter’s documentation still credited Ville’s inequality,
  which is a different statement.

### Fixes found in the pre-submission audit

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) works on
  every result class. Thirteen
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods were
  missing, so [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  on a selection, monitor, batch, benchmark, influence, sensitivity,
  stability, path, power, delay, events or label-curve result fell
  through to
  [`plot.default()`](https://rdrr.io/r/graphics/plot.default.html) and
  failed with base R’s
  `'x' is a list, but does not have components 'x' and 'y'` — a message
  that names neither this package nor
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  arriving at the moment a new user is most likely to type
  `plot(result)`.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a subclass
  now draws the subclass’s figure.
  [`plot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_methods.md)
  called
  [`autoplot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md)
  by name rather than dispatching, so
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  result silently produced the plain changepoint plot instead of the
  consensus one.
- Every [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  now draws as a side effect and returns the `ggplot` invisibly, so
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) works inside
  a loop or a function while `p <- plot(result)` still gives you the
  object to add layers to.
- A **factor** series is refused instead of being detected on its level
  codes.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  coerces the series before validating it, so `cpt_detect(factor(...))`
  ran to completion and reported changepoints in an alphabetical
  ordering of the labels with nothing said about it. **Character** input
  is refused by name too, instead of warning “NAs introduced by
  coercion” from base R and then blaming non-finite data.
- A **logical** series is accepted everywhere.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  coerced one before validating and
  [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  validated before coercing, so a 0/1 series worked in one and was
  refused by the other.
- Empty input is reported by this package rather than by base R:
  `cpt_batch(NULL)` gave `'data' must be of a vector type, was 'NULL'`,
  `cpt_batch(list())` returned a batch of nothing at all, and a
  zero-column data frame reached `X[, 1]` and gave
  `subscript out of bounds`.
- Loading an engine no longer warns about the machine. `mosum` reaches
  `tcltk` through `plot3D` and `misc3d`, so the first
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  or
  [`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  call on any headless box — a server, a container, a CI runner, a
  cluster node — warned `no DISPLAY variable so Tk is not available`.
  `need_pkg()` muffles a load-time warning while still reporting a load
  that fails.
- [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  with a single change size plots something. One scenario is one point
  per curve, so
  [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  drew nothing and advised adjusting the group aesthetic — about a plot
  that was already right — and the ribbon carrying the Monte Carlo
  interval was invisible while the subtitle still announced it. A single
  change size now gets a vertical range and a subtitle that says so; two
  or more are unchanged.
- [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
  and
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  say what is wrong when handed a `ggcpt`. They are the only tools in
  the package that take bare changepoint indices rather than the fit, so
  passing the fit is the obvious mistake, and
  [`as.integer()`](https://rdrr.io/r/base/integer.html) answered it with
  `'list' object cannot be coerced to type 'integer'`. A `ggcpt` is also
  a list, so
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
  read one as a set of annotators and scored its own fields. Each now
  names the argument and the fix (`fit$changepoints$cp`), including for
  a [`tidy()`](https://generics.r-lib.org/reference/tidy.html) table.
- [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  warns when a tuning argument the chosen detector ignores is explicitly
  supplied. The three detectors are calibrated in different currencies —
  `edetector` by `alpha`, `cpm` by `arl0`, `ocd` by `patience` — and
  each ignores the others’, so `cpt_monitor("edetector", arl0 = 5000)`
  changed nothing at all.
  [`?cpt_monitor`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  and the monitoring vignette both said so; now the call does too. The
  knobs that apply, and the defaults, stay silent.
- [`?cpt_monitor`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  marks `method = "ocd"` **multivariate only**, so the requirement is
  visible where the method is chosen rather than only in the error a
  univariate baseline eventually raises.
- [`cpt_scenarios()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scenarios.md)
  warns when a requested `location` is clamped into `2..(n - 2)`. The
  scenario table records the requested fraction, so a clamped row and
  the data generated from it disagreed silently about where the change
  is.
- [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  validates `margin` the way
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  does. A negative margin was accepted and drew its tolerance rectangles
  inside out (`xmin > xmax`) while the metrics function refused the same
  value.
- A registered method must detect on the series it was given. When the
  registered function returned a finished `ggcpt`,
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  took it entirely on trust, so a function that built its result from
  some other series handed back a result whose `$data`, row count and
  `n` described that series instead of `x` — the wrong series to plot,
  the wrong number of rows from
  [`augment()`](https://generics.r-lib.org/reference/augment.html), the
  wrong `n` for every metric. The comment and
  [`?cpt_register_method`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  both claimed the returned object went through “the same contract
  checks as every built-in wrapper”; only the bare-index branch did.
- [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  refuses changepoint locations it cannot read instead of reporting
  none. `cp` was coerced under
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html), so
  `as_ggcpt(c("a", "b"), x)` returned a clean-looking result with zero
  changepoints, and `as_ggcpt(factor(c("60", "90")), x)` returned
  changepoints at **1 and 2** — the factor’s level codes. A logical
  vector is refused too, pointing at `which(cp)`. The documented drops
  (out-of-range, duplicated, missing) and the acceptance of a character
  vector that converts cleanly are unchanged.
- [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  reports a wrong-length `fitted` signal. It was dropped silently, after
  which `autoplot(show_fit = TRUE)` said the result “carries no fitted
  signal” — about a signal the caller had supplied. Every sibling slot
  (`index`, `ci`, `regions`, `extra`) already reported its length
  mismatch.
- [`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md)
  and
  [`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md)
  say what shape they got. Two or three columns satisfy the “needs at
  least two” guard and are still too coarse a grid for ’s basis
  expansion, which stopped with base R’s `subscript out of bounds` —
  naming neither the argument nor the shape. The error now reports the
  time-points-by-grid-points shape and passes the upstream message
  through verbatim, since a coarse grid is the usual cause and not the
  only one.
- A **factor** is refused wherever the package reads changepoint
  locations, instead of being read as its level codes.
  [`as.integer()`](https://rdrr.io/r/base/integer.html) on a factor
  returns level *positions* — alphabetical unless the caller set
  `levels` — so
  `cpt_metrics(factor(c("100", "150")), c(100, 150), n = 200)` scored
  the predictions as 1 and 2 and reported a **recall of 0** for
  predictions that were exactly right. Ten entry points read locations
  through a bare [`as.integer()`](https://rdrr.io/r/base/integer.html):
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  (`pred` and `truth`),
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md),
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md),
  [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md),
  [`as_cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_labels.md),
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md),
  [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md),
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)’s
  dataset annotations and
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md).
  All ten now refuse, each naming the argument the caller passed; a
  character vector that converts cleanly is still accepted everywhere it
  was before.
- A **factor time index** is read as labels rather than as level codes.
  `cpt_detect(x, index = month.abb)` was accepted while
  `cpt_detect(x, index = factor(month.abb))` was refused with “`index`
  must be non-decreasing” — because the codes of an alphabetically
  levelled factor are `5, 4, 8, 1, 9, ...`. An *ordered* factor does
  carry its order in its codes and keeps the ordering and spacing
  checks.
- [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md)
  validates `file` before building the report. A path in a directory
  that does not exist, a directory, `NA` or a two-element vector each
  produced a base-R connection error naming neither the argument nor the
  package, and `file = ""` printed the report to the console and wrote
  no file at all — leaving the caller with a report they believed they
  had saved. `file` remains ignored for `format = "gt"`, as documented.
- Four columns that were returned but never documented are now in their
  `@return`:
  [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md)
  carries through whatever extra columns the engine supplied
  ([`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
  adds `value`, the region’s statistic),
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  returns `detected` alongside `significant` — a location can clear the
  threshold without surviving the engine’s own pruning —
  [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  returns `series`, and
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  returns `n_annotators`.
- [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  adds `cp_index` only when the result carries a time index. It was
  created unconditionally and filled with a bare `NA`, so the same
  column was a `Date` on an indexed fit and a **logical** on an
  unindexed one, and every unindexed result carried a mystery all-`NA`
  column. `attach_index()` and
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  both key on the column’s presence, so this now does too. Its `@return`
  also documents `cp_index` and `event_value`, which it had never named.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  result honours a time index. It was the one plot in the package drawn
  against series position that read the positions directly instead of
  going through the shared index helpers, so a dated series came back in
  positions there while
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on the fit,
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md),
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  and the influence and events plots all showed dates. An unindexed
  result is unchanged.
- A result in which every observation is its own segment now says so.
  `validate_data()` accepts three observations, and at that length seven
  engines (`pelt`, `fpop`, `wbs2`, `tguh`, `smuce`, `decafs`, `nsp`)
  return a changepoint after every one — `k = n - 1`, every segment one
  point long, which is a failure to segment rather than a segmentation;
  at `n = 5`, three of them still do. The threshold is engine-specific,
  so the check is on the result rather than a blanket minimum that would
  refuse calls which work. The message distinguishes the two causes:
  with `penalty = 0` (which is what `penalty = "None"` resolves to for
  the numeric-penalty engines) one segment per observation is the
  correct unpenalised optimum at any series length, and only a
  *positive* penalty reaching the same place means the series is too
  short.
- Vignette figures render at `dpi = 72` rather than rmarkdown’s
  default 96. The source tarball goes from 5.2 MB to 4.2 MB (CRAN’s
  limit is 5 MB) and the installed `doc` directory from 4.9 MB to 3.5
  MB, with no visible change to the figures: `html_vignette` displays
  them at their natural size, so fewer pixels means a smaller file, not
  a smaller picture.

### Fixes found in the final pre-submission sweeps

Further passes, each sweeping a surface rather than re-reading code:
every wrapper against every argument its engine accepts, every method
against every `change_in` value it advertises, every documented claim
against the installed package, and — the ones that found the most —
*invariances*, where the answer is compared against another answer
rather than against a recorded value. Fifty-eight further items.

Five are wrong answers. Four are below; the fifth has its own section
because it is the one that could have reached a publication: a `seed`
argument that reset the caller’s random stream, so a simulation loop
analysed the same dataset six times over.

#### Wrong answers

- **`wbsts` could not report more than one changepoint.**
  [`wbsts::wbs.lsw()`](https://rdrr.io/pkg/wbsts/man/wbs.lsw.html) ends
  in `suppressWarnings(if (is.na(OUT)) OUT = NULL)`, which was a warning
  before R 4.2 and is an error after it — and `OUT` has length \> 1
  exactly when post-processing kept two or more changepoints. So the
  call died precisely when the method would have reported the multiple
  changes it exists to find: 2 of 20 runs failed on a one-changepoint
  series and 19 of 20 on three- and five-changepoint ones, where the
  successes never reported more than one. On that one error the wrapper
  restores `.Random.seed` and replays the engine’s own body with
  `all(is.na(OUT))` in place of `is.na(OUT)` — the reading its
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) shows was
  intended — so the result is upstream’s answer retrieved, not a
  different one.
- **`hdcov` and `network` failed on 44–96% of runs, at random.**
  [`changepoints::thresholdBS()`](https://rdrr.io/pkg/changepoints/man/thresholdBS.html)
  prunes with `for (i in 2:level_length)`, so a binary-segmentation tree
  with one level runs the body at `i = 2`, `table(...)[2]` is `NA`, and
  `1:NA` stops with base R’s `NA/NaN argument`. Binary segmentation
  stops at one level whenever the series is short relative to the
  dimension, and the threshold comes from a permutation draw, so whether
  a given call landed there was random: `hdcov` failed on 23 of 25 runs
  at *n* = 120, *p* = 8 and 24 of 25 at *n* = 400, *p* = 20; `network`
  on 10 of 12 for a 20-point sequence of 4-node graphs. One level means
  one candidate split with no ancestors to prune against, so it is a
  changepoint exactly when its own statistic clears the threshold — a
  rule that reproduces `thresholdBS()`’s output on a multi-level tree,
  which is what makes it safe to apply.
- **[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  detected on a panel it had rewritten.** It reached the engine through
  a bare [`as.numeric()`](https://rdrr.io/r/base/numeric.html), which
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  has refused since 0.4.0: a **factor** member became its level codes —
  an alphabetical ordering of the labels — and a **character** member
  became `NA`s. A **matrix** member was worse, because
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) unrolls it
  column after column: an 80×2 member became a 160-point series and
  reported a changepoint at index 80, the seam where the second column
  was appended. The message names the offending series, because in a
  panel “which one?” is the question.
- **[`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  absorbed non-finite values rather than refusing them,** and what came
  back was wrong rather than merely missing. On a 180-point series with
  one change at 90, twenty `NA`s lost the changepoint entirely, and an
  all-`NA` second half reported two changepoints at 12 and 14 that the
  data does not contain.
  [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  and
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  had always refused this; only the `ecp` route was open.
- [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  and
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  refuse multi-column input instead of concatenating it. The same
  unrolling as above:
  [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  on a 120×2 matrix reported 58, 120 and 180, where only the 58 is real
  and the 120 is the seam.
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  drew 240 points for 120 observations; it now plots the first column
  and says so, the convention
  [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
  already had.
- [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md),
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  and
  [`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md)
  refuse a factor series. Each branch coerced its own way, so a factor
  reached two of the three detectors as level codes — for labels like
  `"10"`, `"2"`, `"30"` that is the order 3, 1, 2 rather than the
  numbers. The series is now normalised once, before the branch.
  `cpt_penalty(model, series =)` is guarded for the same reason: a
  learned penalty predicted from a factor’s level codes is silently just
  a number.
- [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  no longer reports a power figure for a changepoint nobody asked about.
  `location` went through no validation at all, and the scenario loop
  clamps with `max(2, min(cp, n - 2))`, so `location = 1e6` at *n* = 200
  answered “power 0.75” for a change at 198. Out-of-range *positions*
  are now refused; an extreme *fraction* still clamps, which is correct.
- [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  warns instead of returning `power = NaN`. One unlucky replicate may
  legitimately fail, which is why the loop tolerates errors — but when
  *every* replicate fails (most often because an argument forwarded
  through `...` is not one
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  accepts) the rate is `mean(all-NA)`, and a `NaN` is a number the
  caller could plot. The first engine error is now reported with it, and
  [`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md)
  stops on it rather than reaching `if (power < ...)` and answering
  `missing value where TRUE/FALSE needed`.
- `cpt_replay(baseline = 500)` on a 180-point series is refused rather
  than reinterpreted. A lone number is documented as the count of
  leading observations; when it was fractional or out of range it fell
  through to the explicit-series branch, which turned it into a
  **one-point** baseline and then failed with “needs at least 5
  pre-change observations”, never mentioning the 500.
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  no longer advertises two capabilities it could not deliver: `wbsts`
  and `binsegrcpp` were marked `path = TRUE` but expose no solution path
  to
  [`cpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md).

#### Engine arguments, answered by name

A sweep of all 64 wrapper argument slots and of every wrapper against
every argument its engine accepts.

- **Twelve wrapper/argument pairs collided with a value the wrapper pins
  for itself** — SMUCE’s `jumpint`, bocpd’s `getR`, `wbs2`/`tguh`’s
  `solution.path` and `model.selection`, `sn`’s `plot_SN`, `envcpt`’s
  and `beast`’s narration switches, `decafs`’s `warningMessage` — and R
  answered
  `formal argument "verbose" matched by multiple actual arguments`,
  naming neither the wrapper, nor the engine, nor what to do instead.
  Each is now refused with the reason the wrapper sets it.
- **Twenty-three pairs collided with an argument this package renames.**
  `...` is documented as reaching the engine, so the engine’s own name
  is the natural thing to pass — `mindist` for `min_dist`, `M` for
  `n_intervals`, `cpmType` for `cpm_type`, `ARL0` for `arl0`, `lambda`
  for `penalty`, and so on. Each now redirects to the argument that does
  the job, or says the value comes from `x` and is not the caller’s to
  set.
- **Four entry points that are not engine wrappers were never swept, and
  had fifteen collisions between them.**
  [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  pins the two arguments that make its call CROPS at all (`method`,
  `penalty`) and the interval it sweeps (`pen.value`);
  [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  and
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  rename the package’s `method` to `cp_method`, so `method` — the most
  natural name to reach for — was the one that broke; and
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  renames or pins five of `cpm`’s and `ocd`’s (`cpmType`, `ARL0`,
  `MC_reps`, `dim`, `beta`), which
  [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md)
  inherits by forwarding `...` to it. Each now redirects by name, and
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  names itself rather than the function it shares the rename with.
- **Twenty-four arguments were forwarded to the engine unchecked**,
  which reported them from deep inside itself as
  `missing value where TRUE/FALSE needed`,
  `negative length vectors are not allowed` or
  `NAs in foreign function call`. `burnin`, `min_size`, `cstar`,
  `lambda`, `alpha`, `patience`, `mc_reps`, `wsize`, `kmax`, `lag`,
  `npsi`, `minseglen`, `confidence`, `frequency`, `ord`, `N`, the two
  `threshold_*` constants, `n_perm`, `sigma`, `df`, `seed`, `startup`
  and `arl0` are all validated by name now.
- **Seven engines refused a short series in their own vocabulary,** two
  of them with their own typos: `not` reports
  `max.length must satisfy 3 < max.lenght <= n`, `envcpt`
  `Minimum segment legnth is too large`, `wbsts` base R’s
  `subscript out of bounds`, which does not even say the series is the
  problem. None named the method the caller asked for or the length they
  gave it. The engine’s diagnosis is kept — it is the informative half —
  and the method, the observation count and, where the threshold moves
  with an argument, the arithmetic are added: `bfast` needs
  `2 * frequency`, `strucchange` needs `h * n`, `envcpt` more than
  `2 * minseglen`.
- **Five multivariate-only engines failed on a single column with a
  message that named nothing:** `hdcov` `non-conformable arrays`,
  `network` `'x' must be an array of at least two dimensions`, `var`
  `incorrect number of dimensions`, `kwc`
  `dim(X) must have a positive length`, and `geomcp` a clearer line that
  still named neither method nor argument. All five now match the four
  engines that always named the requirement.
- Every refusal of a non-finite series now carries the count — one stray
  `NA` in 10,000 points and a half-missing series are different problems
  with different fixes — from one definition rather than seven copies.
  `network` was the one high-dimensional route with no finiteness check
  at all, and a single `NA` surfaced as `replacement has length zero`
  from inside its random edge-splitting.
- A data frame with one non-numeric column is named rather than blamed
  whole: [`as.matrix()`](https://rdrr.io/r/base/matrix.html) returns an
  all-character matrix, so `x must be numeric` left the reader to find
  which column. The note also says what coercing a factor would have
  given.
- Error messages name the argument the caller actually passed.
  `as_mv_matrix()` said `x` where the argument was `baseline`,
  `new_obs`, `series` or `response`;
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  reported `` Method `scale_space` is univariate ``, naming a method the
  caller had never heard of;
  [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md)
  reported a non-finite value against `baseline` or `new_obs` depending
  on which slice happened to contain it, and counted it against the
  slice rather than the series.
- `cpt_benchmark(methods = character(0))` is refused instead of building
  a zero-row grid and stopping at `attempt to set an attribute on NULL`.
- [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
  validates `width_svg`/`height_svg` on the `ggiraph` path, where
  `girafe()` answered `` `width` must be a scalar positive number ``
  about its own internal argument.
- [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  refuses a multi-column `baseline` for a univariate detector by shape.
  [`length()`](https://rdrr.io/r/base/length.html) on a data frame
  counts its columns, so a 60-row, 2-column baseline was refused for
  having fewer than 5 pre-change observations — naming a count of 2 for
  60 observations.
- [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  on a `bocpd` or `mcp` result names the accessor that does work
  ([`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md),
  and `ci_lower`/`ci_upper` respectively) instead of being a dead end
  reached from
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)’
  own `posterior` column.

#### `cpm`’s two thresholds

- **The documented `arl0` grid was half the real one.** `cpm` ships
  thresholds for 24 average run lengths, not the 12 the help page and
  the error message listed: 300, 800, 900, 3000, 4000, 6000, 7000, 8000,
  9000, 30000, 40000 and 50000 were all refused by this package and
  accepted by the engine. The grid is the same for every `cpm_type`, and
  50000 is the ceiling.
- **`cpm_type = "FET"` needs a `lambda` and had no default.** Without
  one `processStream()` dies inside `cpm` with
  `only 0's may be mixed with negative subscripts`. It is now refused by
  name, with the two values `cpm` ships FET thresholds for (0.1 and
  0.3).
- The “No thresholds available” branch reads the printed line rather
  than assuming: the same message covers `arl0` and `lambda`, and
  blaming `arl0` for a `lambda` failure sent the reader after an
  argument that was already correct.
- `"ExponentialAdjusted"` is offered again. It was withheld alongside
  `"GLRAdjusted"` as one of two types `cpm`’s own dispatch rejects;
  re-measured against `cpm` 2.3, it runs and returns changepoints. Only
  `"GLRAdjusted"` is genuinely rejected upstream.

#### The caller’s session

- **Two engines rewrote the search path and did not put it back.**
  `fabisearch` needs `NMF` *attached* rather than loaded, and attaching
  it brings its `Depends` (Biobase, BiocGenerics) and the
  foreach/doParallel/doRNG stack the engine registers — eight packages
  measured, where the wrapper detached only `NMF`; and the baseline was
  taken *after* `need_pkg()`, which is itself what attaches two of them.
  [`bcp::bcp()`](https://rdrr.io/pkg/bcp/man/bcp.html) calls
  [`require(bcp)`](https://github.com/zhaokg/bcp) in its own body, so
  every call attached `package:bcp` and `package:grid`. Both now restore
  exactly what the call added, leaving a package the user had already
  attached untouched.
- **A detection call no longer announces someone else’s package loads.**
  [`require()`](https://rdrr.io/r/base/library.html) speaks through
  [`packageStartupMessage()`](https://rdrr.io/r/base/message.html), so
  restoring the search path silently was not enough:
  [`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md)
  still printed `Loading required package: bcp` and
  `Loading required package: grid` on stderr. Only package startup
  messages are suppressed, so an engine’s own
  [`message()`](https://rdrr.io/r/base/message.html) and
  [`warning()`](https://rdrr.io/r/base/warning.html) still reach the
  caller.
- **[`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md)
  no longer prints an upstream loading diagnostic.**
  [`InspectChangepoint::inspect()`](https://rdrr.io/pkg/InspectChangepoint/man/inspect.html)
  and `sparse.svd()` both call
  [`requireNamespace("RSpectra")`](https://github.com/yixuan/RSpectra)
  without `quietly = TRUE`, and `RSpectra` is only *suggested* there —
  so on a machine holding the engine and not `RSpectra` every call wrote
  `Loading required namespace` and
  `Failed with error: there is no package called 'RSpectra'`. The engine
  handles the absence itself by falling back to
  [`base::svd`](https://rdrr.io/r/base/svd.html), so it is a diagnostic
  rather than a problem, and thirteen repetitions of it are what
  truncated a CI test log down to nothing else.
  [`suppressMessages()`](https://rdrr.io/r/base/message.html) is not
  enough — [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
  writes the second line straight to stderr — so the message *stream* is
  captured; warning conditions and errors still propagate.

#### `seed` no longer resets the caller’s random stream

- **A `seed` argument silently collapsed simulation studies to a sample
  size of one.** Every one of the thirty-six sites that honoured a
  `seed` did it with `if (!is.null(seed)) set.seed(seed)` in the
  function’s own frame, which does not merely *consume* the caller’s
  random stream — it **resets** it. So the argument whose entire purpose
  is trustworthiness pinned the stream of whatever loop the call sat
  inside:

  ``` r

  set.seed(2026)
  for (i in 1:6) {
    d <- c(rnorm(100), rnorm(100, 3))
    f <- cpt_detect(d, method = "wbs", seed = 1)
  }
  ```

  Iteration 1’s `set.seed(1)` pins the stream, so every later
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html) starts from the same
  place and regenerates the same series: measured with the data built
  outside the call, **6 distinct datasets of 6 without the seed and 2 of
  6 with it.** Nothing warned and no test failed. The same collapse was
  measured through
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md),
  `cpt_select(criterion = "cv")`,
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  and the stochastic engines — which is to say exactly the functions a
  user calls inside a simulation loop.

- **The seed is now scoped to the call.** A new internal helper saves
  `.Random.seed`, sets it, and restores it when the calling function
  exits, at all thirty-six sites. A fresh session that had no
  `.Random.seed` is left without one, rather than acquiring one. Nested
  calls stack correctly: an inner scope restores what the outer one set.

- **Nothing documented changed.** A seeded call is still
  byte-reproducible, and still reproducible across a thousand
  intervening draws or after an unseeded call has moved the stream on —
  all three are now tested, as metamorphic assertions that compare calls
  to each other rather than to a recorded value, which is the only kind
  that could have caught this.

- Every `@param seed` says so: the seed is scoped to the call and does
  not pin the loop’s own stream.

#### Benchmarking

- **The critical-difference diagram named Demšar and cited nobody.**
  `autoplot(plot_type = "critical_difference")` is described as “the
  Demšar diagram … the standard way this literature says method A beats
  method B”, which is a specific methodological claim, and Demšar (2006)
  appeared in neither `inst/REFERENCES.bib` nor any `\insertRef` — on a
  page that already cites van den Burg and Williams for the metrics.
  Added, and the page now carries a “Reading the critical-difference
  diagram” section.
- **The section says three things the diagram cannot.** What rank 1
  means and which direction each metric is ranked in; that an `NA` takes
  the *worst* rank rather than being dropped, so a method that failed on
  a dataset is penalised instead of quietly scoring on a smaller sample;
  and the formula for the bar, `CD = q_α √(k(k+1)/6N)` with `q_α` the
  Studentised range over `√2`.
- **And the caveat it invites.** Nemenyi is a *post-hoc* procedure,
  conventionally run only after a Friedman test rejects equal ranks;
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  draws the diagram it is asked for and does not run that omnibus test.
  With the handful of datasets
  [`cpt_datasets()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_datasets.md)
  supplies *N* is small and *CD* correspondingly wide, so the page now
  says to read such a diagram descriptively.
- The arithmetic behind all of it was checked rather than assumed: the
  critical distance agrees with Demšar’s Table 5 to three decimals for
  *k* = 2…10, the ranks respect each metric’s direction, `NA` takes the
  worst rank in both directions, ties share the average, `mean_rank`
  averages over datasets, and an all-`NA` metric returns `NULL` rather
  than a table of ties.
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)’s
  frequencies are exact multiples of 1/*B* — so `hits` really is a count
  of replicates, which is what the clipping bug fixed earlier in this
  cycle had hidden.

#### Consensus

- **[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  accepted a `min_votes` no location could reach, and returned an empty
  consensus without comment.** `min_votes = 3` against two methods
  resolves to a threshold of 3, which nothing can clear — and an empty
  consensus is indistinguishable from *the methods agreed on nothing*,
  which is a finding rather than an arithmetic mistake. It now warns,
  naming the threshold and the number of methods that actually ran (not
  the number requested, since a method that errors is excluded from the
  vote).
- **The count/proportion boundary falls exactly where a reader would
  write “unanimous”, and
  [`?cpt_consensus`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  now says so.** A value strictly between 0 and 1 is a proportion;
  anything else is a count. So with three methods `min_votes = 0.99`
  requires all three while `min_votes = 1` — and `1.0`, the same number
  — is a count of one, the *least* strict setting there is. The two
  neighbouring values mean opposite things, silently. The parameter now
  spells that out and says to pass the method count, or a fraction just
  below 1, for unanimity; the new warning repeats it, because a count
  above the method total is the likeliest way to arrive there.

#### Choosing K: the criteria say what they compute

- **[`?cpt_select`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  gave the formula for one of its three closed-form criteria and not the
  other two.** `"mbic"` was written out in full (); `"bic"` was
  “Gaussian BIC over the ladder” and `"aic"` “Gaussian AIC over the
  ladder”, which leaves the `value` column a reader cannot reproduce —
  both the cost convention and the changepoint parameter count vary
  between authors. Both are now stated: `n log(RSS/n) + (2K + 1) log n`
  and `n log(RSS/n) + 2(2K + 1)`, with the parameter count spelled out
  as *K* locations plus *K + 1* segment means, and the note that the
  first term is the `cost` column.
- The arithmetic was checked rather than assumed, against reference
  implementations written from the definitions and sharing no code with
  the package: all three criteria agree exactly at every rung of the
  ladder, the `cost` column is the Gaussian profile cost at that rung’s
  own locations, exactly one row is marked `chosen`, and it is the
  argmin of `value`. A test pins the formulas rather than recorded
  numbers, so a change to what a criterion *means* fails instead of
  passing.
- The help page’s warning about `"aic"` is now a tested behaviour rather
  than prose: on a 300-point series with changes at 100 and 200, `"bic"`
  and `"mbic"` both choose *K* = 2 and `"aic"` takes the whole ladder.

#### Cost, where it is large enough to look like a hang

- **`fcov` costs minutes on a hundred-point series, and nothing said
  so.** It is by a wide margin the most expensive engine in the package.
  Timed against
  [`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md)
  on identical input — same package, same data — `fcov` took **316 s**
  at *n* = 60, *p* = 5 and **598 s** at *n* = 120 against `fmean`’s 4.5
  s and 2.9 s: a factor of seventy to two hundred.
  [`?fcov_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md)
  now carries a “How long this takes” section with those numbers, notes
  that the cost is roughly linear in the number of time points and lives
  in the engine’s covariance-operator estimation rather than in the
  wrapper, and says plainly not to put the method in a loop — a
  twelve-replicate study at *n* = 120 is two hours. `taylor` and `ocd`
  already carried cost sections for the same reason; this is the third
  and the most extreme.

#### Simulation, which is the package’s own ground truth

- **[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  dropped a surplus `params` entry silently.** `k` changepoints make
  `k + 1` segments, which is the arithmetic easiest to get wrong, and
  the two directions were treated differently: too *few* entries already
  warned (the last one is recycled, so the trailing changepoints would
  be recorded as ground truth with no change behind them), while too
  *many* used the first `k + 1` and discarded the rest without a word.
  So `cpt_simulate(400, changepoints = 200, params = c(0, 3, 9))`
  returned an ordinary two-segment series and the `9` vanished — from a
  caller who had plainly meant two changepoints, and whose
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  or
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  numbers are then scored against a truth they did not intend. It now
  warns in both directions, naming the changepoint count, the segment
  count and how many entries went unused, for every `change_in` rather
  than only `"mean"`.
- The quantities are now tested rather than assumed, because everything
  downstream of
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  is scored against them: the realised mean jump matches `params` to
  within a standard error at three sizes, the realised sd ratio matches
  at two, `seg_id` increments at exactly the requested changepoints for
  three configurations, `noise = "ar1"` recovers `rho`, and
  `noise = "t"` is materially heavier-tailed than Gaussian.

#### The extension mechanism

- **[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  dropped a changepoint it could not use without saying so.**
  `ggcpt_build()` discards an index that is `NA` or outside
  `1..(n - 1)`, and for a wrapper that is right — the indices come from
  an engine, some of which legitimately emit a boundary value, and
  normalising a machine’s output is the wrapper’s job. But
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  is handed the *caller’s* values, and its documented use cases are a
  published paper’s reported breaks, an analyst’s annotations, another
  package’s output. On a 200-point series:

      as_ggcpt(c(50, 500), x)  ->  1 changepoint at 50
      as_ggcpt(c(0, 50), x)    ->  1 changepoint at 50
      as_ggcpt(c(50, 200), x)  ->  1 changepoint at 50
      as_ggcpt(50.5, x)        ->  1 changepoint at 50   (truncated, not rounded)
      as_ggcpt(c(50, 50), x)   ->  1 changepoint at 50   (deduplicated)

  One mistyped index left a result that looked complete and was short a
  changepoint. **The dropping is unchanged** — it is the documented
  contract and refusing would break working code — but each of the five
  now warns, naming the values, the range they had to fall in, and why
  the convention makes `n` invalid under `"left"` and `1` invalid under
  `"right"`. Sorting still happens silently, because reordering loses
  nothing.

- The report is a **classed** condition (`ggchangepoint_cp_dropped`),
  because one caller was right to be silent:
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  routes a registered method’s bare-vector return through
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
  and there the indices came from the detector rather than from a person
  transcribing them — the wrapper case, where normalising an engine’s
  output is the point. That one call site muffles this condition and
  nothing else, so a registered detector emitting a boundary index does
  not warn on every call while a user’s own transcription still does.

#### Reproducibility under a parallel plan

- **[`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)’s
  seeded answer depends on the
  [`future::plan()`](https://future.futureverse.org/reference/plan.html),
  and
  [`?cpt_power`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  now says so.** Seven exported functions dispatch on the plan, and this
  is the one whose farmed-out tasks consume random numbers: under a
  parallel plan the replicates draw from ’s L’Ecuyer streams (derived
  from `seed`), and sequentially from the calling stream `seed` set.
  Both are deterministic and they are not the same numbers — measured,
  one and the same `seed = 11` gave `power = 0, 1` sequentially and
  `0.125, 0.875` on two workers. The guarantee is *same seed and same
  plan, same answer*, and the new section says how to pin a figure that
  has to be reproducible by someone else. It also notes that the gap is
  Monte Carlo error rather than disagreement, which `mc_se` quantifies.
- The other six that dispatch on the plan —
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md),
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md),
  [`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  and
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  — were **measured** to return identical results under a sequential and
  a two-worker plan, stochastic engines included, because their parallel
  tasks are deterministic given their input. Tests now pin both facts:
  every one of the seven reproduces within a plan, and the six are
  plan-independent.

#### Inference

- **`cpt_confint(method = "auto")` could answer at a different level
  than the one asked for, silently.** An `nsp` result carries
  significance regions, so `"auto"` resolves to `"native"` and reports
  them at the level the engine already used —
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)’s
  `alpha = 0.1`, i.e. 0.9 — so `cpt_confint(res, level = 0.95)` returned
  90% regions. The `level` column said 0.9 throughout, which is honest
  but only if you inspect it, and
  [`?cpt_confint`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  documented `level` as ignored by `"native"` without noting that
  `"auto"` lands there whenever the engine supplied an interval.
  Supplying a `level` the answer does not carry now **warns and names
  the route it took**, the way
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  already warns for a tuning argument that does not affect the chosen
  method. The default never warns, and `"bootstrap"`, `"posterior"` and
  `"nsp"` honour the level as before.
- The bound invariants are now tested rather than assumed: across every
  provenance and every installed univariate engine,
  `ci_lower <= cp <= ci_upper`, both bounds inside `1..(n - 1)`, one row
  per changepoint, and a non-empty `source`. Fifty-nine method/route
  pairs, no violations.

#### `cpt_metrics()` says what its twelve numbers mean

- **The help page listed twelve column names and explained three of
  them.**
  [`?cpt_metrics`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  now describes each, with the direction that is better — the same
  directions
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  ranks by — because a benchmark table of twelve unlabelled columns is
  not readable otherwise.
- **`annotation_error` is a count difference and nothing else**, and
  that is now said where it can be seen. It is `abs(n_pred - n_truth)`,
  so a segmentation with the right *number* of changepoints in entirely
  the wrong *places* scores a perfect 0: on a 100-point series,
  predicting 5 against a truth of 90 gives `annotation_error = 0`
  alongside `hausdorff = 85` and `f1 = 0`. The page now says to read it
  beside a location metric, never alone.
- **The degenerate cases are documented, including the one place the row
  mixes conventions.** With one side empty, `precision`, `recall`, `f1`
  and `rand_index` are `0` rather than `NA`; but `hausdorff`,
  `mae_matched` and `rmse_matched` are `NA`, because they are distances
  with no pair to measure. So an all-wrong answer returns `f1 = 0` and
  `mae_matched = NA` in the same row, and
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  ranks the `NA` last rather than dropping it. Only the both-empty case
  was documented before.
- The numbers themselves were checked rather than assumed: `covering`,
  `hausdorff`, `rand_index` and the matched precision/recall/F1 agree
  exactly with brute-force reference implementations (explicit set
  intersection over every segment pair, a contingency-table ARI, a
  double minimax) on nine hand-built cases and twenty random 400-point
  ones. The [`findInterval()`](https://rdrr.io/r/base/findInterval.html)
  fast path in `calc_covering()` is a performance change to a formula,
  so a test now pins it to the definition.

#### Testing

- **Eleven vignette chunks generated their data without seeding it**, so
  their rendered output was a function of how much randomness every
  chunk above them happened to consume — and several of those chunks are
  conditional on an engine being installed, which means the vignette’s
  numbers and figures already differed between machines with different
  optional packages. Scoping the `seed` argument (above) changed them
  again. Each of the eleven now seeds itself, in
  `vignettes/ggchangepoint.Rmd`, `vignettes/monitoring.Rmd` and
  `vignettes/supervised.Rmd`, so a rebuilt vignette is a function of the
  vignette. No prose claim depended on the old values.
- **Four visual snapshots were only reproducible by accident.** The
  visual-regression file seeds once at the top and draws its shared
  series there, but three blocks generate their own data with a bare
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html) — so those snapshots
  were a function of test *execution order*, stable only because every
  block above them consumed randomness deterministically. Scoping the
  `seed` argument (above) took that away and four snapshots changed; the
  figures were the same layers of a different series, not a broken plot.
  Each of those blocks now seeds itself, and the snapshots reproduce
  from three deliberately different starting RNG states — which they did
  not before.
- **The suite’s Suggests-guard check could name the wrong test.** It
  locates each `test_that()` block, and it used to end one at the line
  before the next `test_that(` — which swept up the section comment
  introducing the *following* test. So a test that touches no optional
  engine was reported as calling one, because the next test’s header
  mentioned it in prose, and the reader was sent to edit a block that
  was never at fault. Block extents now come from R’s parser (`srcref`s
  cover an expression and nothing between expressions), which is also
  exact where brace-counting would not be — several blocks contain a
  brace inside a string, `skip("{mcp} is not installed")` among them. A
  test builds a synthetic file with all five shapes
  (unguarded-and-innocent, guarded, genuinely unguarded,
  brace-in-string, `expect_error`) and asserts the new spans flag
  exactly the one guilty block while the old rule flags two.

#### Printed output

- **Four [`print()`](https://rdrr.io/r/base/print.html) headers padded
  their labels by hand, and all four had drifted.**
  [`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  — the package’s most-seen output — put its values in three different
  columns, because `Changepoints found:` is longer than the pad the
  other five lines use;
  [`print.ggcpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  had five of six lines right and `Average run length:` one column out;
  and
  [`print.ggcpt_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  left `Distinct segmentations:` unpadded entirely.
  [`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  and
  [`print.summary.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/summary.ggcpt.md)
  also indented their values differently, though they are two views of
  one object. One internal helper now emits every field line, so they
  cannot drift apart again.
- Header lines no longer end in a space. `cat(" Label: ", value, "\n")`
  puts the separator between the value and the newline, which left a
  trailing space on 5 of 13 lines of a `ggcpt` and 7 of a summary. (The
  tibble printed below the header pads its own columns; that is tibble’s
  output and is left alone.)

#### Documentation that had drifted from the code

- The engine lists in `autoplot(show_ci =)`, `autoplot(show_fit =)`,
  [`augment.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/augment.ggcpt.md)
  and `cpt_confint(method = "native")` were each short by three to four
  engines. They now name exactly the methods
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  marks in its `ci` and `fitted` columns, and say why `nsp` is marked
  for uncertainty while being drawn by `show_regions` rather than
  `show_ci`.
- [`?cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  documents the six method/`change_in` pairs that are routed to the
  method’s own native change type because the engine has no separate
  estimator — `not`’s `"var"`, `cpm`’s `"mean"` and `"var"`, `kcp`’s
  two, and `wbsts`’s `"mean"`. The routing was never silent (the
  result’s `change_in` records what was detected), but it was never
  written down either. Measured across every method and every value its
  registry entry lists; every other combination returns what was asked
  for.
- [`?cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  names two more engines whose own signature ends in `...`, so a
  misspelt argument is discarded upstream rather than reported:
  `fChange` and `bfast`. Every other wired method rejects an unknown
  argument by name — checked, rather than asserted.
- **`scale_space` is not the capability column the docs said it was, on
  three help pages.**
  [`?cpt_methods`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
  `autoplot(type =)` and
  [`?cpt_scale_space`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  all grouped it with `statistic` and `path` as an engine internal whose
  accessor errors without it — so a reader with a `pelt` fit was told
  the scale-space view was closed to them. It is not: nothing stores a
  scale space on a result at all,
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  computes one by sweeping a multiscale detector over the series, and it
  returns a full sweep for a `pelt` result as readily as for a `mosum`
  one. What the column marks is the two engines the sweep can be run
  *with* — the domain of that function’s own `method` argument.
  `statistic` and `path` do gate their accessors, and still do, with the
  list of supporting engines in the message.
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  documents that `online` describes the *algorithm* and not what
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  accepts. The two sets overlap without coinciding: `bocpd` is an online
  algorithm the monitor does not offer, and `edetector` is native to
  this package and has no row in the table. Also documented: what
  `univariate = FALSE` means (a high-dimensional method, which is what
  [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  filters on — nine of the fourteen do error on one column, five run and
  would still be poor advice), what `ci = TRUE` covers for `nsp`, and
  that `posterior = TRUE` does not imply
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  can draw it.
- Eight wrappers had no reference at all (`fpop`, `wbs`, `wbs2`, `not`,
  `mosum`, `idetect`, `tguh`) and
  [`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md)
  credited the wrong paper — `fChange`’s package citation rather than
  the covariance-change method it wraps.
- The package help page’s bold headings rendered as literal `**`: the
  block is not `@md`, so they are `\strong{}` now.
- [`?ocd_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)’s
  timing note understated Monte Carlo construction by 2–3× and has been
  re-timed, with the machine dependence stated and the linearity in
  `mc_reps` — the part worth planning around — separated from the
  absolute numbers.
- [`?taylor_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md)
  gains a section on series length, because the engine runs where R
  cannot look: a
  [`setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html) of 45 s
  was not honoured after 170, and **Ctrl-C will not stop it either** (R
  checks both at the same points). `n_bootstraps` is the knob, and the
  cost is roughly linear in it.
- [`?wbs2_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md)
  gains a reproducibility section. The engine is not reproducible call
  to call within a session and no argument can make it so — repeated
  identical calls with a byte-identical `.Random.seed` on entry returned
  a last changepoint of either 183 or 188 — because the state that
  varies is not R’s random stream. It reproduces upstream, a fresh
  session is deterministic, and it needs a series whose model selection
  sits near a tie: across the methods swept, `wbs2` on one configuration
  was the only case, and `tguh` — same package — was stable throughout.
- [`?network_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/network_wrapper.md)
  documents that the series the result carries — and so the one
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  draws — is the **mean edge weight** per time point, not a coordinate.
  It is the one multivariate method with no `data_wide` slot, because a
  *p*×*p* network has *p*² entries per time point.
- The `fChange` wrappers’ note on a too-coarse grid said “two or three
  columns”; measured on 60 time points, two fail and three, four and six
  all return a fit, so the guard above it cannot be raised without
  refusing grids the engine handles.
- [`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md)’s
  example says why it is `\dontrun{}` — every call downloads from the
  Turing Change Point Dataset’s repository — rather than leaving a
  reader to guess.
- Seven help topics could not be reached from any other help page;
  `@seealso` links now connect them.
  [`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md),
  [`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
  [`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  and
  [`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md)
  gained a description distinct from their title — roxygen had been
  copying the title into `\description` — and
  [`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  documents its return value, which no `\value` section had stated.
- Comments that justified a workaround with a claim that is no longer
  true were corrected rather than left standing:
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  now survives a facet column named `variable` (the name stays
  `coordinate` because that is what the column holds); `EnvCpt`’s
  [`arima()`](https://rdrr.io/r/stats/arima.html) stderr leak would not
  reproduce on any of eight series chosen to provoke it (the diversion
  is kept as a cheap net, and a genuine convergence warning still
  reaches the caller); `breakfast`’s `"lp"` selector does not misfire on
  constant data, so pinning `"ic"` is for reproducibility against an
  upstream default of `NULL`; and `ocd`’s single-column failure depends
  on `thresh = "MC"`, which is why the shape is checked in the wrapper
  rather than left to the engine.

### Corrections to the roadmap

- `hdbinseg` is **archived on CRAN again**, contrary to the 0.5.0
  roadmap’s note that it was back at 1.0.3. `sbs` therefore stays in the
  planned table, alongside `gfpop`, `robseg`, `FOCuS` and
  `changeforest`, and all five now read “when on CRAN”.
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  is the supported route to any of them today.

### Testing and infrastructure

- New `inst/CITATION`.
- The package opts into testthat edition 3
  (`Config/testthat/edition: 3`). The whole suite passes unchanged under
  it, and it is what makes `announce_snapshot_file()` available —
  without which a plain `test_dir()` deletes every visual snapshot as
  unused and the next run silently regenerates them.
- New `vdiffr` visual-regression snapshots for every
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  type and every new layer — the package had no visual net at all, so a
  dropped layer or an inverted axis could pass every existing test. They
  are a local net: an SVG snapshot records the font stack of the machine
  that made it, so they are skipped on CRAN and on CI rather than
  reporting a failure on every platform but one.
- Around 620 new expectations across seven test files
  (`test-050-registry.R`, `-index`, `-inference`, `-diagnostics`,
  `-supervised`, `-engines`, `-tools`) plus 25 visual snapshots in
  `-visual`, all engine-dependent tests guarded with
  `skip_if_not_installed()`. The suite runs about 2350 assertions with
  the engines installed and about 1470 without them.
- `stats`, `tools` and `utils` are declared in `Imports`; the new
  engines and extras are in `Suggests` behind
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards, as
  before — 35 engines inside a 56-package `Suggests` list, and the
  package still checks clean with none of them installed. `withr` joins
  `Suggests`, which the tests already used, and `rjags`, which
  [`mcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mcp_wrapper.md)
  tests for because having does not imply JAGS can be reached.
- Every parallel entry point is now tested under a real
  `future::plan(multisession)`, and the wrappers that the suite only
  ever reached through
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  —
  [`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md),
  [`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md),
  [`kwc_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kwc_wrapper.md),
  [`not_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/not_wrapper.md),
  [`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
  [`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
  [`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md),
  [`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)
  — are now called directly, so their own argument handling is covered.
- The suite is checked in two environments: the full one, and R 4.6.0
  against a library holding the `Imports` and none of the `Suggests`.
  The second is the only thing that exercises the no-`Suggests` path the
  DESCRIPTION promises, and it caught a test that asserted
  `geom_cpt_event(repel = TRUE)` builds — true only where is installed.
  That assertion now covers both worlds instead of one.
- `tests/testthat/setup.R` sets `rgl.useNULL`. `fabisearch` imports
  `rgl`, which warns twice about the X11 display the moment its
  namespace loads on any headless machine; the option is rgl’s own way
  to say no window is needed, and it keeps the suite’s output about the
  package.

### Backward compatibility

Everything from 0.4.0 keeps working. Almost all the additions are new
functions, new optional arguments with their previous defaults, or new
optional slots on `ggcpt` that are absent unless something supplies them
— `is.null(fit$regions)` remains the test for “this engine does not do
regions”, exactly as `data_wide` has always worked. Three changes are
worth naming rather than leaving to be discovered:

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now prefers a time index carried on the result over the observation
  position. This affects only results built with the new `index`
  argument, which did not exist before.
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  returns nine capability columns by default. Code that reads it by name
  is unaffected; code that reads it by position, or checks
  [`ncol()`](https://rdrr.io/r/base/nrow.html), is not.
  `cpt_methods(capabilities = FALSE)` returns the 0.4.0 shape.
- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  gained `index` and `y` as its fifth and sixth formal arguments, ahead
  of `...`. Named calls are unaffected. A call that passed a wrapper’s
  own argument *positionally* past `penalty` — which no example or
  vignette ever did, because `...` arguments have always had to be named
  to reach the right engine — would now bind it to `index`.

## ggchangepoint 0.4.0

CRAN release: 2026-08-24

### The 0.4.0 engine wave

[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
grows from 13 to 31 wired methods. Eighteen new wrappers, all of whose
engines live on CRAN and enter `Suggests` behind
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards:

- [`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md)
  — SMUCE/HSMUCE multiscale inference (`stepR`), the first engines to
  populate `ci_lower`/`ci_upper` confidence-interval columns.
- [`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md)
  — exact change-in-slope detection (`cpop`);
  `cpt_detect(change_in = "slope")` now routes here or to NOT’s linear
  contrast instead of erroring.
- [`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
  [`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
  [`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md)
  — the Bayesian pillar (`bcp`, `ocp`, `Rbeast`), with `posterior_prob`
  columns and the posterior mean carried as a fitted signal.
- [`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md)
  — sequential distribution-free detection (`cpm`), with a
  `detection_time` column.
- [`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md)
  — kernel change point analysis on running statistics (`kcpRS`; mean,
  variance, autocorrelation, correlation).
- [`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md)
  — nonparametric MOSUM under serial dependence (`CptNonPar`).
- [`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md)
  — abrupt changes amid drift and AR(1) noise (`DeCAFS`).
- [`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md)
  — self-normalised segmentation (`SNSeg`; mean, variance, acf,
  bivariate correlation).
- [`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md),
  [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md),
  [`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md)
  — high-dimensional and multivariate detection (`InspectChangepoint`,
  `ocd`, `changepoint.geo`).
- [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
  — Bai-Perron structural breaks with break-date confidence intervals
  (`strucchange`); accepts a bare series or a regression formula.
- [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md)
  — broken-line regression with kink confidence intervals (`segmented`).
- [`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md)
  — changepoints vs. trends vs. autocorrelation model selection
  (`EnvCpt`).
- [`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md)
  — the modern fastcpd engine (`fastcpd`), covering
  mean/variance/meanvariance plus AR/ARMA/GARCH model changepoints.

### New tools

- New
  [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  computes the full CROPS penalty path and returns a `ggcpt_path` object
  with [`print()`](https://rdrr.io/r/base/print.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
  `autoplot(type = c("elbow", "path", "segmentations"))`.
- New
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  runs one detector over many series (matrix, data frame, or list) with
  optional `future` parallelism; returns a `ggcpt_batch` tibble with
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and a
  faceted
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- New
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  bootstrap stability diagnostic: segment-preserving resampling with a
  detection-frequency profile and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- New Bayesian displays:
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  (posterior mean + per-location changepoint probability) and
  [`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
  (the BOCPD run-length posterior heatmap).
- New
  [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
  renders any result as a `plotly` widget.
- New
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
  returns the verified methodological reference(s) behind a result or
  method name.

### Visualisation

- [`autoplot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md)
  gains `show_ci` (draws changepoint-location confidence intervals from
  `ci_lower`/`ci_upper`) and `show_fit` (overlays the engine’s fitted
  signal), and renders multivariate results as faceted small-multiples.
- [`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  migrated off the deprecated
  [`ggplot2::geom_errorbarh()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  to `geom_errorbar(orientation = "y")`.
- Unknown styling arguments passed through
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  now warn instead of being silently discarded, and plotting an empty
  `ggcpt` errors cleanly instead of producing infinite axis limits.

### Bug fixes (audit items C1-C20; regression-tested)

- `cpt_detect(penalty = <number>)` works for the changepoint-package
  methods (`pelt`, `binseg`, `segneigh`, `amoc`): a numeric penalty is
  now translated to the engine’s
  `penalty = "Manual", pen.value = <number>` instead of erroring with
  “Unknown Penalty”
  ([\#2](https://github.com/PursuitOfDataScience/ggchangepoint/issues/2)).
- `binseg` / `segneigh` no longer crash on short series that pass
  validation; the maximum number of segments `Q` is clamped to a
  length-safe value
  ([\#3](https://github.com/PursuitOfDataScience/ggchangepoint/issues/3)).
- [`augment()`](https://generics.r-lib.org/reference/augment.html) uses
  the engine’s fitted signal when the result carries one, instead of
  always the per-segment mean
  ([\#4](https://github.com/PursuitOfDataScience/ggchangepoint/issues/4)).
- [`augment()`](https://generics.r-lib.org/reference/augment.html) keeps
  all coordinates for a multivariate result instead of dropping
  everything but the first
  ([\#5](https://github.com/PursuitOfDataScience/ggchangepoint/issues/5)).
- The `segments` table’s `start` / `n` columns are integer, matching the
  documented schema
  ([\#6](https://github.com/PursuitOfDataScience/ggchangepoint/issues/6)).
- [`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md)
  gains a minimum-`n` guard and filters its changepoint indices, so
  `true_changepoints` no longer contains 0, `n`, or duplicates for small
  `n`
  ([\#7](https://github.com/PursuitOfDataScience/ggchangepoint/issues/7)).
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  honours the `index` argument for multivariate results
  ([\#8](https://github.com/PursuitOfDataScience/ggchangepoint/issues/8)).
- [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  /
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  no longer crash with “factor level duplicated” when two series share a
  name, or the `methods` vector repeats
  ([\#9](https://github.com/PursuitOfDataScience/ggchangepoint/issues/9)).
- `wbs` returns an empty result instead of erroring when a manual
  `threshold` admits no changepoints
  ([\#10](https://github.com/PursuitOfDataScience/ggchangepoint/issues/10)).
- `idetect` returns an empty result on short series instead of erroring
  with “wrong sign in ‘by’ argument”
  ([\#11](https://github.com/PursuitOfDataScience/ggchangepoint/issues/11)).
- [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  no longer fabricates changepoints on no-change data (the positional
  boundary strip reversed `c(1, n+1)`), and no longer drops genuine
  changepoints in `e.agglo`’s wrap-around case (C1).
- [`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md)
  now returns the sSIC model selection it documents; a manual threshold
  is recorded as the penalty actually used (C2).
- Univariate wrappers and
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  now error on multi-column input instead of silently flattening it
  column-major (C3).
- [`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md)
  returns an empty result on no-change data instead of erroring (C4).
- [`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md)
  pins breakfast’s model selection to “ic”: no more spurious changepoint
  on constant data, no crash on short series, and the scalar-0 “no
  changepoints” sentinel is handled (C5).
- [`glance()`](https://generics.r-lib.org/reference/glance.html) is
  always one row: fpop’s per-position cost vector no longer explodes the
  tibble, and `$` partial matching no longer grabs unrelated fit
  elements (C6).
- [`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md)
  records the numeric threshold as `penalty$value` (was the string
  “critical.value”) and implements its documented `multiscale` argument
  via
  [`mosum::multiscale.localPrune()`](https://rdrr.io/pkg/mosum/man/multiscale.localPrune.html)
  (C7, C8).
- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  forwards `change_in` to NOT via contrast mapping and the result
  reports what actually ran (C9); `penalty = "None"` resolves to 0 for
  numeric-penalty engines (C10).
- `cpt_penalty("sSIC")` implements the strengthened SIC
  `k * log(n)^alpha` (was `0.5 * k * log(n)`, weaker than BIC) (C11).
- Metrics agree with the van den Burg-Williams conventions: an exactly
  correct empty prediction scores precision/recall/F1 = 1 (C12); empty
  predictions score the trivial-partition covering and chance-level ARI
  0 (C13); out-of-range indices are dropped with a warning instead of
  crashing (C14);
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  uses the same one-to-one matching as
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  and its “Miss” legend entry renders (C15).
- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  keeps a facet panel for every method, including those that found
  nothing, and no longer errors when no method finds anything (C16).
- [`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
  sorts by the `x` aesthetic before detecting (results were previously
  row-order dependent) and declares `dropped_aes` so building the plot
  is warning-free (C17).
- [`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
  generates the true Donoho-Johnstone blocks signal (cumulative jumps,
  not absolute levels) (C18); simulated t-noise is rescaled so its
  standard deviation matches `sd` (C19); all signal generators validate
  their minimum lengths (C20).
- `cpt_wrapper(cp_method = "SegNeigh")` falls back to the SIC penalty
  the engine supports instead of always erroring under the default; `np`
  results report `change_in = "distribution"`; `meanvar` results stay
  `"meanvar"` in the user’s vocabulary;
  [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
  handles multivariate input without crashing.

### Bug fixes (pre-release audit; regression-tested)

- Every univariate wrapper now rejects multi-column input instead of
  silently flattening it column-major. The C3 fix had only reached the
  search-based wrappers, so
  [`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md),
  [`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md),
  [`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
  [`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
  [`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
  [`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
  [`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md),
  [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md),
  [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md)
  and
  [`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md)
  turned a 120x2 matrix into a 240-point series. The new
  [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  and
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  entry points guard the same way (R16).
- `segneigh` no longer errors with “subscript out of bounds” on short
  series. The `Q` clamp added for
  [\#3](https://github.com/PursuitOfDataScience/ggchangepoint/issues/3)
  missed the engine’s real constraint: Segment Neighbourhood requires
  `Q >= 3` regardless of length, so the clamped `Q` of 1 or 2 failed for
  every `n < 8`. `Q` is now clamped into the engine’s valid window
  (`3 <= Q <= n - 2` for a mean change, `floor(n / 2) + 1` when a
  variance is estimated per segment), and a series too short to admit
  any valid `Q` gets an actionable message naming the constraint instead
  of the engine’s internal error (R17).
- A multivariate coordinate literally named `index` no longer crashes
  `mv_data_wide()` with “Column name `index` must not be duplicated”; it
  is made unique against the position column, so `ecp`, `inspect`,
  `geomcp`, `ocd`, `npmojo`, `kcp` and `fastcpd` all accept such data
  (R18).
- `NA` changepoint indices from an engine are dropped rather than
  propagating into `build_segments()` as an “NA/NaN argument” error, and
  any engine-supplied extra columns (`ci_lower`, `posterior_prob`, …)
  stay row-aligned through the drop (R19).
- [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)’s
  `"MBIC"` no longer misattributes its formula to Zhang and Siegmund
  (2007), whose modified BIC penalty depends on the segment lengths and
  cannot be written as a function of `n` and `k` alone. The computed
  value is unchanged; the documentation now states what it is (BIC plus
  a combinatorial placement term) and how it differs.
- Passing a wrapper’s own argument through
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  no longer errors with “formal argument … matched by multiple actual
  arguments”. The dispatcher derives some arguments from `change_in` and
  was passing them alongside the caller’s, so the documented `...`
  passthrough was broken for `not`’s `contrast`, `cpm`’s `cpm_type`,
  `kcp`’s `running_stat`, `sn`’s `parameter`, `fastcpd`’s `family` and
  `hsmuce`’s `family`. A value supplied by the caller now wins over the
  derived one (R20).
- Two enumerated engine options that could never succeed were removed
  (R21): `smuce_wrapper(family = "poisson")` — current `stepR` accepts
  no such family, so it always errored — and
  `cpm_wrapper(cpm_type = "GLRAdjusted")`, which
  [`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html)
  rejects by *printing* an error and returning no changepoints, making
  it silently report “no changes” for any input. `cpm_type = "FET"` is
  retained and documented as needing 0/1 data plus a `lambda` value.
- [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
  no longer advertises univariate input: `ocd`’s detector cannot be
  constructed for a single coordinate (it fails with “subscript out of
  bounds”), so a bare vector now gets a message naming the requirement
  instead of the engine’s internal error (R22).
- Degenerate input is handled the way the rest of the package already
  handled it. A constant series now returns the empty result instead of
  an opaque engine error (`sn`, `kcp`, `npmojo`, `inspect`) or, for
  `segmented`, a spurious kink recovered from a singular fit (R23).
- A single constant coordinate no longer kills a multivariate run.
  `inspect`, `npmojo` and `kcp` standardise each coordinate, so one flat
  column (a dead sensor channel, say) made their statistics undefined
  and the whole call failed with “missing value where TRUE/FALSE needed”
  even when the other coordinates carried an obvious change. Flat
  coordinates are now dropped with a warning naming them, detection
  proceeds on the rest, reported locations stay in the original row
  space, and the dropped coordinates are still kept for plotting (R24).
- `kcp` and `sn` explain themselves on series too short for their
  windows, instead of surfacing “wrong sign in ‘by’ argument” and “only
  0’s may be mixed with negative subscripts” (R25).
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) no longer render
  penalties at full double precision or with a placeholder value:
  `Penalty: Manual = 17.8459510605346` is now `Manual = 17.846`, and a
  penalty that carries no numeric value prints as `MBIC` rather than
  `MBIC = NA`.

### Bug fixes (final pre-submission audit; regression-tested)

The whole exported surface was exercised with degenerate,
contract-violating and self-generated input. Items are listed with the
ones that change an answer or end a session first.

- `hsmuce` no longer aborts the R session. When a series carries
  essentially no noise at the per-segment scale, `stepR`’s heterogeneous
  variance estimator does not raise an R error but *terminates the
  session*, so nothing downstream can catch it and the user loses their
  work. It is reachable straight from
  `cpt_detect(x, method = "hsmuce")`. Two regimes were measured as
  fatal: a globally flat series such as
  `rep(4, 300) + rnorm(300, 0, 2e-7)`, and — more dangerous, because it
  looks entirely ordinary — a clean step whose segments are numerically
  constant, `c(rep(0, 150), rep(5, 150)) + rnorm(300, 0, 1e-9)`, which
  is what `cpt_simulate(sd = 0)` produces once any rounding is added.
  Both are refused when the point-to-point variation lies more than
  about seven orders of magnitude below the data’s own scale, with a
  message naming `family = "gauss"`, which handles the whole range. An
  exactly noiseless series is safe upstream and still works (R53).
- `idetect` no longer invents changepoints on a constant series.
  [`IDetect::ID()`](https://rdrr.io/pkg/IDetect/man/ID.html) is erratic
  on flat input — its statistics go to 0/0, and what it returns depends
  on the value and the length: `rep(3, 200)` came back with **126**
  changepoints at 1, 3, 4, 6, 7, …, while `rep(0, 100)` errors and
  `rep(-2.5, 60)` returns a sentinel 0. Every other search wrapper
  reports none, and the 0.4.0 audit fixed exactly this class of bug for
  `segmented`, `sn`, `kcp`, `npmojo` and `inspect` — `idetect` was
  missed. It now short-circuits to the empty result, decided by exact
  equality so a series with tiny but genuine variation still reaches the
  engine (R50).
- [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  reports the quantity it documents. `freq` is described as “the
  proportion of replicates detecting a changepoint within `margin` of
  that index”, but the loop incremented once per *changepoint*, so a
  replicate whose detections had overlapping ±`margin` windows was
  counted twice at the shared indices; `pmin(hits / B, 1)` then hid the
  overflow by clipping it. The effect was to inflate exactly the number
  the function exists to report — in a measured example an index that
  only half the replicates covered was shown as 1.00, “re-detected every
  time”. Each replicate now contributes at most one to any index, so
  `freq` is a genuine proportion and needs no clipping (R38).
- [`glance()`](https://generics.r-lib.org/reference/glance.html) always
  returns the single row it documents.
  [`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  defaulted `method` and `change_in` to `character(0)`, so `tibble()`
  recycled every other column down to zero rows — an empty summary for
  any hand-built result, including the one the README demonstrates.
  Those defaults are now `NA_character_`, and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) coerces
  the metadata fields to length one whatever the object carries (R37).
- The `changepoint` engines (`pelt`, `binseg`, `segneigh`, `amoc`, `np`)
  keep their upstream `cpt` object in `$fit`. It was `NULL`, although
  `$fit` is documented as “the raw upstream object” and every other
  engine stored one — which also left the `inherits(fit, "cpt")` branch
  of [`glance()`](https://generics.r-lib.org/reference/glance.html)
  unreachable, and with it a sign error and a wrong element index that
  had never run. `glance()$total_cost` now reports the unpenalised −2
  log L for those engines where `changepoint` exposes it on that scale,
  and stays `NA` where it does not, rather than mixing two scales in one
  column;
  [`?glance.ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/glance.ggcpt.md)
  spells out which cases are which (R35).
- [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
  works on multivariate results. The faceted small-multiple that
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  builds for them used a facet column named `variable`, which is also
  the name
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  gives a column of its own when it melts the built plot, so every
  multivariate result failed with “Names must be unique”. The column is
  now `coordinate`; the facet strips are unchanged (R36).
- Duplicate multivariate coordinate names no longer abort a run. A
  matrix may legally carry the same colname twice, which made
  `add_column()` reject the wide frame with “must have unique names as
  of tibble 3.0.0”; the R18 fix had only deduplicated a coordinate named
  `index` against the position column, not the coordinates against each
  other. All coordinate names are now made unique in one pass (R34).
- `envcpt` no longer prints its engine’s internal failures as though the
  call had failed. `EnvCpt` fits up to twelve models with
  [`try()`](https://rdrr.io/r/base/try.html), and a non-silent
  [`try()`](https://rdrr.io/r/base/try.html) writes its error straight
  to stderr, so on a degenerate series
  [`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md)
  printed six lines beginning “Error in arima(…): non-stationary AR part
  from CSS” and then returned a perfectly good result. Those failures
  are expected — the criterion ignores the models that did not fit — so
  the message stream is diverted for the duration of the call. Genuine
  warnings are deferred past the diversion and still reach the user, and
  a call that really does fail still errors (R52).
- `cpt_simulate(change_in = "meanvar")` works without `params`. It was
  the one change type with no parameter default, so the call died with
  “replacement has length zero” instead of simulating anything (R32).
- [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  warns about recycled parameters for every change type, not only
  `"mean"`. Supplying fewer parameters than there are segments reuses
  the last one, so the trailing entries of `changepoints` were recorded
  in `true_changepoints` with no actual change behind them — silently
  wrong ground truth for `"var"`, `"meanvar"` and `"slope"` (R32).
- Two more ways to get a silent “no changepoints” are closed. `cpm`
  ships thresholds only for a fixed set of average run lengths; for any
  other `arl0` its `processStream()` *prints* “Error: No thresholds
  available for selected ARL0” and returns an empty result instead of
  raising a condition, so
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) never saw it
  and the wrapper reported zero changepoints on a series with an obvious
  one — the same trap the earlier audit found for
  `cpm_type = "GLRAdjusted"`, on a different argument. And
  [`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md)
  with `nperm` below 2 either reported nothing (0 or negative) or died
  inside the engine with an unreadable `row.names` error (1). Both are
  refused now, with the supported `arl0` values named in the message
  (R61).
- An out-of-range `conf_level` no longer hangs
  [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md).
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html) on a
  breakpoints fit at `level = 2` never returns, and the
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) already around
  that call cannot rescue a call that does not terminate — so the
  session simply locked up. `conf_level` is now required to lie strictly
  between 0 and 1 in both
  [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
  and
  [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md).
  In the same sweep: `bocpd_wrapper(hazard)` and `cpop_wrapper(sd)` must
  be positive, and `wbs_wrapper(n_intervals)` at least 1 — all
  previously accepted meaningless values (R60).
- [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  refuses parameters that made it emit `NaN`. It is where ground truth
  for every benchmark comes from, so a silent series of `NaN` is the
  worst thing it can produce — and `sd = -1`, `sd = NA`, and
  `|rho| >= 1` under the AR(1) model each did exactly that, with no
  error and no warning (`sqrt(1 - rho^2)` is not a number outside the
  stationary range). Non-positive `n` is refused too. `rho` is checked
  only for `noise = "ar1"`, so a stray value the chosen model ignores is
  still accepted (R59).
- The logical switches refuse a non-logical value instead of silently
  doing the opposite. `show_segments`, `show_ci`, `show_fit`,
  `show_line`, `show_points` and `mosum_wrapper(multiscale)` are all
  documented as “Logical” but were read with
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html), which treats
  everything that is not `TRUE` as `FALSE`. So `show_segments = 1`,
  `= "yes"`, `= "TRUE"` or `= NA` quietly drew nothing, and
  `show_line = 1` quietly *removed* the line the user was asking to keep
  — three layers down to one. `show_points = NULL` keeps its documented
  meaning of deciding from the series length (R58).
- The package’s own arguments now enforce the ranges they document. The
  engines police their own — `stepR` refuses an `alpha` outside (0, 1),
  `SNSeg` an unlisted `confidence` — but ggchangepoint’s were taken on
  trust, and out-of-range values returned answers instead of errors:
  `cpt_metrics(margin = -3)` scored a *perfect* segmentation as
  precision 0 and recall 0; `cpt_stability(B = 0)` produced a stability
  profile of `NaN`; `cpt_metrics(n = -10)` a covering metric of −1;
  `bcp_wrapper` and `beast_wrapper` with `prob_threshold = 0` reported
  239 changepoints in a 240-point series; `kcp_wrapper(alpha = 2)` and
  `cpt_crops(pen_min = -5)` ran regardless. All are refused now, with
  the legitimate boundaries (`margin = 0`, `B = 1`, `n = 1`,
  `prob_threshold = 1`) still accepted (R57).
- [`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md)’s
  automatic bandwidth is never 1. `min(n / 10, 100)` rounds to 1 for
  every `n < 20`, and a one-observation window leaves the engine’s
  studentised statistic undefined, so it warned “NaNs produced” and
  returned spurious changepoints rather than failing. The automatic
  bandwidth is floored at 2, and a series too short for any window gets
  an actionable message (R29).
- [`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md)’s
  default bandwidth is capped at `n / 2`, the largest the engine
  accepts. The documented `max(20, 0.1 * n)` exceeded that for every
  series shorter than 40, so the default always failed with “Bandwidth
  is too large for the length of time series”. Series of 40 or more
  observations are unchanged (R30).
- `cpt_wrapper(change_in = "np")` refuses `cp_method` values other than
  `"PELT"` up front.
  [`changepoint.np::cpt.np()`](https://rdrr.io/pkg/changepoint.np/man/cpt.np.html)
  implements PELT only, so `"BinSeg"` and `"SegNeigh"` used to die on
  the internal `Q` clamp with “unused argument (Q = 5)” and `"AMOC"`
  surfaced the engine’s “Invalid Method” (R27).
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  and
  [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
  reject an `index` whose length does not match the series, naming the
  argument at fault, instead of surfacing dplyr’s recycling error (“`x`
  must be size 200 or 1, not 10”), which never mentions `index` (R26).
- [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  enforces the argument ranges it documents (R28): `alpha > 1` for
  `"sSIC"` (at or below 1 it is weaker than BIC, so no longer a
  *strengthened* SIC); `n >= 3` for the log-based penalties (`log(n)` is
  0 at `n = 1` and `log(log(n))` is negative below `n = 3`, so the
  “penalty” rewarded extra changepoints); and `0 <= k <= n` for
  `"MBIC"`, whose `log C(n, k)` term is `-Inf` beyond that. `"AIC"`,
  which does not involve `n`, is exempt.
- [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  names the series that failed. It exists for panels of hundreds of
  series, but an error in any one of them surfaced only as the
  underlying complaint — “`x` must have at least 3 observations” —
  leaving the user to bisect the list to find which. The message is now
  prefixed with the series name and its position,
  e.g. `` Series `short` (2 of 3): `` (R49).
- A result from
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  records the
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  call in `$call`. It previously held the internal helper each branch
  happened to use — e.g.
  `wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)`, an unexported
  function named with the dispatcher’s local symbols, which a reader can
  neither recognise nor re-run. Wrappers called directly still record
  themselves (R33).
- [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
  on a result with no method name says so, instead of surfacing tibble’s
  “Can’t subset rows with `refs$method == method`” (R37).
- [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  no longer warns “No shared levels found …” when there is nothing to
  draw: a run with no predictions and no ground truth is a perfect
  score, not a broken plot (R31).
- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  pads its changepoint rules by a fixed amount on a flat series, as
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  already did; a zero data range would otherwise collapse them to
  invisible zero-height segments.
- [`glance()`](https://generics.r-lib.org/reference/glance.html) no
  longer carries an unreachable branch. It tested
  `inherits(fit, "cptrange")`, but the `changepoint` class is
  `cpt.range` — with a dot — so the branch could never fire, and its
  body used `$` on an S4 object, which would have errored had it ever
  been reached. Removed; the BinSeg/SegNeigh case is handled explicitly
  alongside the other engines whose cost is on a different scale (R46).
- [`?new_ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  and
  [`?ecp_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  explain why `$fit` is `NULL` for `"ecp"` and only for `"ecp"`:
  [`ecp::e.agglo()`](https://rdrr.io/pkg/ecp/man/e.agglo.html) returns a
  cluster-progression matrix that is quadratic in the series length, so
  retaining it by default would make the result object explode on a long
  series — 207 kB of fit for a 1.3 kB series at n = 160 alone (R46).
- The covering metric no longer scales quadratically.
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  compared every truth segment against every prediction segment, so
  scoring a segmentation with many changepoints crawled — 7.5 seconds
  for 3000 of them. Because both partitions tile the series and their
  breakpoints are sorted, only the overlapping prediction segments can
  win, and two
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html) lookups
  locate them; the same case now takes 0.42 seconds. The numbers are
  unchanged: verified identical on 4010 cases (4000 random plus
  adversarial partitions) and pinned in the tests against an independent
  set-based statement of the definition (R42).
- The redundant `.onLoad()` is gone. It re-registered `print`, `plot`,
  `summary`, `tidy`, `glance`, `augment` and `autoplot` at load time —
  writing into `base`‘s and `generics`’ S3 method tables — even though
  NAMESPACE already declares every one of them, and it wrapped the lot
  in [`suppressWarnings()`](https://rdrr.io/r/base/warning.html), so a
  genuine registration failure would have been invisible. It was a
  leftover from before `@exportS3Method base::generic` was adopted in
  0.3.0. Verified redundant before removing: all eleven methods still
  dispatch with and without the package attached, and every declared
  generic/class pair still resolves through
  [`getS3method()`](https://rdrr.io/r/utils/getS3method.html) (R41).
- [`?ocd_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
  says how long it takes. Nearly all of `ocd`’s cost is Monte Carlo
  threshold calibration, which happens before a single observation is
  read: measured at `mc_reps = 5`, construction is about 3 s at p = 3, 9
  s at p = 10 and 55 s at p = 50, and four times that at `mc_reps = 20`
  — so the default `mc_reps = 100` extrapolates to roughly a quarter of
  an hour at p = 50. The help now gives those numbers, notes that
  monitoring the observations afterwards is comparatively free, and
  points at `thresh`, which takes the three thresholds directly and
  skips calibration entirely. That escape hatch had no test; it has one
  now (R56).
- `stats` is declared in `Imports`.
- The documented simulate → detect → evaluate → plot workflow is
  verified end to end. Each piece had its own tests, but not the chain:
  a result’s changepoints feeding
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  and
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md),
  its segments feeding
  [`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
  the object itself feeding
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).
  The chain was run for all 31 methods — it completes for every one, and
  24 of them recover both planted changepoints with precision, recall,
  F1 and covering all exactly 1. The exceptions are all correct by
  construction: `amoc` finds at most one changepoint, `cpop` and
  `segmented` are slope engines being shown a step, `ocd` is online and
  reports declaration times, and `geomcp` unions its distance and angle
  mappings. A six-method version spanning the structural variety is now
  in the suite (R55).
- Every configuration of
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  and every canonical signal was run through all 31 methods to confirm
  none of them can produce input that terminates the session. Three
  configurations do land in the degenerate band and are now refused by
  `hsmuce` rather than crashing it: `sd = 0` and `sd = 1e-9` for a
  change in mean, and — the one the audit turned up —
  `change_in = "slope"` with `sd = 0`, whose consecutive differences are
  a constant slope, so its point-to-point variation is floating-point
  residue of about 1e-14 rather than zero. Nothing else crashes on any
  of them, and the realistic settings and all five canonical signals are
  unaffected (R54).
- The dispatcher’s `change_in` translations are tested.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  derives an engine-specific argument from `change_in` for `not`, `cpm`,
  `kcp`, `sn` and `fastcpd`; the suite covered *overriding* those
  through `...` but never the derivation, so a wrong translation would
  have silently run the wrong analysis. Each is now checked against the
  equivalent explicit call (R51).
- [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)’s
  one-to-one matching is verified to be a genuine maximum matching,
  which is what
  [`?cpt_metrics`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  claims and what precision and recall are derived from — if the greedy
  scan ever fell short, both would be silently understated. Checked
  against an exact maximum bipartite matching on 300 random
  configurations plus seven clustered and interleaved patterns chosen to
  break a greedy rule: it never falls short (R48).
- The Bayesian displays’ remaining documented paths are tested:
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  on a
  [`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md)
  result (the help says it handles both bcp and BEAST, but only the bcp
  branch of the profile extractor was ever run), and every guard on
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)/[`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
  — non-`ggcpt` input, a result with no posterior, and a `prob_floor`
  that leaves nothing to draw (R47).
- Test coverage rose to cover the exported surface that had none. A
  coverage run found two exported functions with no test at all —
  [`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
  and
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
  — alongside a set of documented modes and arguments that nothing
  exercised: `ecp_wrapper(algorithm = "agglo")`,
  `sn_wrapper(parameter = "bivcor")`,
  `cpt_simulate(noise = "ar1" | "rw")`,
  [`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
  `autoplot(show_segments = TRUE)`, the “no changepoints detected” print
  paths, and the `sd`/`breaks`/`model_param`/`lambda`/`threshold`/`G`
  arguments of the cpop, strucchange, DeCAFS, inspect and mosum
  wrappers. All of them worked; none of them was guarded against a
  future refactor (R45).
- The test suite now really does run with none of the Suggests
  installed. Two assertions reached a Suggests-only engine without a
  guard — `expect_error(fpop_wrapper(X), "univariate")` and the fpop
  half of the scale-sensitivity note — so on a machine with no fpop they
  met “Package ‘fpop’ is required” instead of the message under test,
  which is an ERROR rather than a skip on CRAN’s noSuggests flavour. The
  earlier `_R_CHECK_DEPENDS_ONLY_` run had missed both because the
  fallback library it used still exposed part of Suggests; the suite is
  now verified against a library holding the Imports and nothing else.
  Both assertions are guarded and the pelt half of each stayed
  unguarded, so the cases that need no Suggests still run. A static
  sweep of every `test_that()` block for a Suggests package used without
  a matching guard found no others (R62).
- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  and
  [`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
  refuse a multi-column `x` instead of flattening it. Both run
  univariate detectors but took `as.numeric(x)` on trust, so a 160x2
  matrix was unrolled column after column and the join between the
  columns read as a level shift: the table came back with changepoints
  at 80 *and* 160, and 160 is the seam, not a feature of either series.
  Every wrapper already refused wide input through the same check; these
  two entry points were the only ones that did not. Non-numeric input
  now names the argument as well, rather than failing inside
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) with “cannot
  coerce type ‘object’ to vector of type ‘double’”. The message points
  at
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md),
  which is what runs a detector over a panel (R63).
- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  hands `future.apply` a documented `future.seed` value. It passed
  `seed` straight through, and `seed` defaults to `NULL`, which is not
  among the logical/integer/list values `future_lapply()` documents — so
  every parallel comparison run without an explicit seed was outside
  that contract. It now sends `TRUE` in that case, asking for
  parallel-safe L’Ecuyer streams, which is what
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  already did. Sequential runs are unaffected. Found by exercising the
  parallel branch of both functions for the first time: it is documented
  in three vignettes and both help pages, and no test had ever set a
  non-sequential
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  The branch is otherwise correct — same changepoints as the sequential
  path, series names preserved, `...` forwarded, and the “which series
  failed” error still named (R64).
- [`?strucchange_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
  says how large its result is. Measuring
  [`object.size()`](https://rdrr.io/r/utils/object.size.html) for every
  engine on one series turned up a single outlier: a `strucchange`
  result is quadratic in the series length, because `breakpoints()`
  keeps `RSS.triang`, the triangular table of segment residual sums of
  squares that lets it return the optimal segmentation for any number of
  breaks without refitting. On a 3.2 kB series it comes to 1.7 MB at n =
  200, 5.9 MB at n = 400 and 22.6 MB at n = 800 — about four times
  larger per doubling — and the table’s share of that grows from 85% to
  95% over the same range. One fit is nothing; a few hundred from
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  are, so the help now says to keep `$changepoints` rather than the
  whole list of results. Nothing changed in the object: this is the same
  size-versus-usefulness trade-off already documented for `ecp` in the
  opposite direction, and it was simply unstated. Every other engine is
  ordinary — the median result across the other thirty is under ten
  times the size of the series it was given (R65).
- Asking for one of the four planned methods says so.
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  lists `gfpop`, `robust`, `focus` and `sbs` with `status = "planned"`,
  but `cpt_detect(x, method = "gfpop")` went to
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html), whose message
  enumerates the thirty-one wired methods — so it did not contain the
  name the user had just read out of the table. The table said the name
  existed and the dispatcher said it did not. It now reports what the
  method is waiting on and which package it will be built on; an
  outright unknown name still gets the ordinary list. In the same pass,
  `sbs`’s entry was out of date: it said “when on CRAN”, but `hdbinseg`
  returned to CRAN as 1.0.3 in September 2025, so the only thing
  standing between `sbs` and a user is the wrapper. `gfpop` was removed
  from CRAN and `robseg` and `FOCuS` have never been on it, so those
  three still read “when on CRAN” (R66).

### Documentation

- [`?cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  gains a scale-sensitivity section, and the README and the introduction
  vignette repeat it: `pelt`, `binseg`, `segneigh` and `fpop` weigh the
  penalty against a *raw* segment cost when detecting a change in mean,
  because `changepoint`’s Normal cost fixes the noise standard deviation
  at 1 and `fpop`’s `lambda` penalises the residual sum of squares
  directly. Neither rescales the data, so wider noise makes the penalty
  negligible and the segmentation shatters — on one true changepoint
  with a five-sigma jump, `pelt` returns 1 changepoint at sigma = 1, 29
  at sigma = 3 and 138 at sigma = 10. The note gives the three remedies
  (standardise the series, scale the penalty by the noise variance, or
  use `change_in = "meanvar"`) and records that every other engine
  estimates or cancels the noise scale itself; both halves of it are
  pinned by a test (R39).
  [`?cpt_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md),
  [`?fpop_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md)
  and
  [`?cpt_penalty`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  point at it. Behaviour is unchanged; the trap was simply undocumented,
  and the package’s own examples all use unit-variance data, so nothing
  exposed it.
- [`?cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
  [`?fpop_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md),
  [`?cpop_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md)
  and
  [`?decafs_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md)
  now record that the dispatcher and those wrappers do not share a
  default penalty.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  resolves its `"MBIC"` default to a numeric value that is stronger than
  the wrappers’ own `2 * log(n)` — 19.9 against 11.8 at n = 360 — so
  `cpt_detect(x, method = "decafs")` reports 3 changepoints where
  `decafs_wrapper(x)` reports 5 on the same series. Both defaults were
  documented individually; that they differ was not. Passing `penalty`
  explicitly makes the two entry points agree (R40).
- [`?npmojo_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md)
  records that the engine calibrates its detection threshold by
  bootstrap, so the value stored in the penalty descriptor varies
  between runs unless [`set.seed()`](https://rdrr.io/r/base/Random.html)
  is called first (or a manual threshold is passed through `...`).
- [`?cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  warns that a misspelt engine argument can pass unnoticed. `wbs`,
  `not`, `Rbeast`, `strucchange`, `segmented` and `fastcpd` all end
  their own signature in `...`, so an unrecognised name forwarded
  through
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)’s
  `...` is discarded upstream and the engine quietly uses its default.
  Intercepting it here would risk rejecting arguments those engines
  legitimately forward deeper, so the behaviour is unchanged and
  documented instead.
- The README and all three vignettes were reviewed against the source
  and corrected. Notably: a
  [`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md)
  example that could not run (it was given `xintercept`, but the geom
  needs `x`/`xend`/`y`/`yend`); `DeCAFS` and `EnvCpt` filed under
  multivariate methods when both are univariate;
  [`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)
  demonstrated on the input series rather than the result; a claim that
  only three engine packages are required; and a method-family count
  that disagreed between the package help, the README and the vignettes
  (all now six — the feature-tour vignette was the last straggler and
  still said five).
- Figure alt text is now specific per figure instead of one generic
  string for every plot.
- The
  [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
  test uses `mc_reps = 10` rather than 50. Those repetitions only
  calibrate the detection threshold, and the change the test plants is
  far too large for the calibration to matter — 10 reps give the same
  declaration as 50 and take 7 seconds instead of 36, cutting the whole
  test suite from 74 to 42 seconds with the assertions unchanged.
- The
  [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
  example runs in 3.6 seconds instead of 20. It was by far the slowest
  example in the package — `ocd`’s Monte Carlo threshold calibration
  scales with both the number of coordinates and `mc_reps` — and a
  smaller, cleaner problem (100x3 with `mc_reps = 5`) demonstrates the
  wrapper better anyway: it reports one declaration just after the true
  change, where the old example also produced a spurious second one.
- Two citations were wrong, and the package’s three citation sources now
  agree. The TGUH paper was dated 2018 (Annals of Statistics 46(6B),
  3390-3421) by `cpt_cite("tguh")` but 2022 (50(5), 2721-2761) in the
  vignette bibliography — the same paper with two sets of coordinates;
  the bibliography is corrected to match, and its key renamed
  accordingly.
  [`?ecp_wrapper`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  cited the arXiv preprint of the ecp software paper while both
  vignettes cited its published form, so `inst/REFERENCES.bib` now
  carries the Journal of Statistical Software version (62(7), 1-25). A
  new test cross-validates all three sources: shared BibTeX keys must
  describe the same publication, every `\insertRef` key must resolve in
  `inst/REFERENCES.bib`, and every `@key` cited in a vignette must
  resolve in the vignette bibliography (R44).
- [`?stat_changepoint`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
  says which geoms actually work with it. The stat emits one
  `xintercept` per changepoint and drops `x`/`y`, so `"vline"` (the
  default) and `"rug"` fit while `"point"` errors; the help previously
  read as though any geom would do.
- [`?geom_cpt_ci`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  no longer claims an `x` aesthetic is required. The layer is a
  horizontal error bar, so it needs `y`, `xmin` and `xmax`; `x` is
  accepted but unnecessary, and neither of the package’s own call sites
  (`autoplot(show_ci = TRUE)` and the feature-tour vignette) supplies
  it, so the help contradicted the package’s own usage (R43).
- [`?augment.ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/augment.ggcpt.md)
  now says what the columns mean for a multivariate result: every
  coordinate is returned and `seg_id`/`is_changepoint` apply to the
  whole row, but `.fitted` and `.resid` describe the first coordinate
  only — the same one `$segments$param_estimate` summarises.
- The penalty-semantics section of
  [`?cpt_penalty`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  now records the one silent substitution the dispatcher makes:
  `changepoint` does not implement MBIC for Segment Neighbourhood, so
  `cpt_detect(method = "segneigh")` falls back to `"SIC"` on the default
  penalty and its result is therefore not directly penalty-comparable
  with a PELT one.

## ggchangepoint 0.3.0

CRAN release: 2026-06-26

### Documentation and coverage

- The README now introduces every exported function, grouped by role,
  and the over-claimed `gfpop` engine (never wrapped) has been removed
  from it.
- New feature-tour vignette
  ([`vignette("ggchangepoint")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/ggchangepoint.md))
  walking the full exported surface, including the per-engine wrappers,
  [`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md),
  and
  [`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md).
- The package-level help
  ([`?ggchangepoint`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggchangepoint-package.md))
  was rewritten to describe the unified `ggcpt` framework and the
  current 13-method engine list (it previously still claimed “only three
  changepoint packages”).
- New documentation-coverage test asserting every export appears in the
  README.

### New features

- New
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  introspection helper returning a tibble of every known method, its
  engine, availability status, and whether the engine is installed.
- New S3 methods for the `ggcpt` class:
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`format()`](https://rdrr.io/r/base/format.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).
- [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  gained a documented per-engine penalty-semantics section.

### Bug fixes

- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  no longer advertises 13 methods that errored at runtime;
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) now enumerates
  only the wired methods (B7).
- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  validates `method` × `change_in` combinations and errors with a clear
  message instead of silently mislabelling the result (B3).
- [`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
  now produces the correct Blocks signal; the segment levels previously
  collapsed to a single step because the assignment loop ran in reverse
  (B1).
- [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  uses one-to-one matching, so `recall` and `f1` can no longer exceed 1
  (B2), and no longer warns on empty `pred`/`truth` (B6).
- [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  returns a correct per-coordinate `cp_value` for matrix and data.frame
  input instead of a column-major flattened scalar (B4);
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  no longer flattens multivariate input before passing it to `ecp`.
- [`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
  maps detected indices back to the `x` aesthetic so rules land at the
  correct location on non-`1:n` axes (B5).
- [`glance.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/glance.ggcpt.md)
  now reports a measured `runtime` and populates `total_cost` from the
  underlying fit when available (B8).
- [`augment.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/augment.ggcpt.md)
  renames data columns position-independently, so it no longer breaks
  when the data carries more than two columns (B11).
- [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  `@return` now documents the `seg_id` column it actually returns (B9),
  and the dead `show_segments` parameter was removed from the internal
  plot helper (B10).

## ggchangepoint 0.2.0

CRAN release: 2026-06-21

### Major changes

- New `ggcpt` S3 result class with
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html), and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods
- New
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  unified dispatcher for changepoint methods
- New geoms:
  [`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
  [`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
  [`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
  [`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
- New first-wave wrappers: WBS/WBS2, NOT, MOSUM, FPOP, Isolate-Detect,
  TGUH
- New
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  and
  [`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
  for method comparison
- New evaluation module:
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md),
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
- New simulator:
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)/[`rcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  and canonical test signals
- New
  [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  helper
- New
  [`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md)
  and
  [`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md)
  for plot customisation

### Hardening (bug fixes)

- [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  no-change bug fixed: spurious boundary changepoints and NA no longer
  emitted
- `size` → `linewidth` migration: `cptline_linewidth` replaces
  deprecated `cptline_size`
- [`match.arg()`](https://rdrr.io/r/base/match.arg.html) input
  validation added to all wrappers
- Changepoint convention documented and aligned
- “sytle” typo fixed → “style” in documentation
- roxygen modernised to `"_PACKAGE"` sentinel
- `change_in = "np"` alias added (keeps `"cpt_np"` for backward
  compatibility)
- Full-height changepoint rule default; `show_points` auto-off above 500
  obs
- Optional `index` parameter for time-series axes

### Testing

- New `testthat` test suite with coverage for all new and hardened
  functions

## ggchangepoint 0.1.0

CRAN release: 2022-02-24

- Initial release to CRAN.
- Exported functions:
  [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md),
  [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md),
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md),
  [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md).
