# `ggchangepoint` package

Unified tidy changepoint detection with `ggplot2` visualisation.

## Details

`ggchangepoint` provides a consistent S3 result class (`ggcpt`) for
changepoint detection results, `broom`-style methods
([`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`augment()`](https://generics.r-lib.org/reference/augment.html)),
`ggplot2` integration via
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and composable geoms
([`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)),
and a unified dispatcher
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
that reaches fifty methods.

\*\*Detection engines.\*\*
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
dispatches to the methods in
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
across nine families:

- **Penalised/optimal:** PELT, BinSeg, SegNeigh, AMOC (changepoint);
  FPOP (fpop); fast binary segmentation (binsegRcpp); the CROPS penalty
  path
  ([`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md));
  fastcpd (fastcpd, incl. AR/ARMA/GARCH); change-in-slope via CPOP
  (cpop).

- **Multiscale/search:** WBS (wbs), WBS2 and TGUH (breakfast), NOT
  (not), MOSUM incl. multiscale (mosum), Isolate-Detect (IDetect),
  SMUCE/HSMUCE with confidence intervals (stepR), WBS for nonstationary
  series (wbsts).

- **Inference:** Narrowest Significance Pursuit (nsp), which returns
  intervals rather than points.

- **Nonparametric/kernel:** NP (changepoint.np), E-Divisive/E-Agglo
  (ecp), kernel running statistics (kcpRS), NP-MOJO (CptNonPar),
  sequential CPM (cpm), self-normalisation (SNSeg), depth ranks
  (KWCChangepoint).

- **Bayesian:** Barry-Hartigan posterior (bcp), online BOCPD (ocp),
  BEAST model averaging (Rbeast), formula-based regression with
  changepoints (mcp).

- **High-dimensional:** sparse projection (InspectChangepoint), online
  ocd (ocd), geometric mapping (changepoint.geo), sparsity-adaptive ESAC
  and Pilliat (HDCD), and covariance, network, VAR and
  high-dimensional-regression changes (changepoints).

- **Functional and network:** functional mean and covariance (fChange),
  NMF-based network structure (fabisearch).

- **Regression, trend and season:** Bai-Perron breaks with CIs
  (strucchange), broken-line regression (segmented),
  changepoints-vs-autocorrelation model selection (EnvCpt), drift+AR
  robust detection (DeCAFS), BFAST season-and-trend breaks (bfast).

- **Classical single-change tests:** Pettitt, Buishand and SNHT (trend),
  Taylor's analyzer (ChangePointTaylor).

\*\*What surrounds the detectors.\*\* Every detector returns a `ggcpt`
object with a stable `tibble(cp, cp_value)` contract, optionally
carrying a time index, engine confidence intervals, a fitted signal,
significance regions and diagnostics. Around that:

- **Inference:**
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  (four provenances, one contract),
  [`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md),
  [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md).

- **Choosing K:**
  [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  (BIC, Zhang-Siegmund mBIC, AIC, CROPS elbow, cross-validation,
  stability),
  [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md),
  [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md).

- **Diagnostics:**
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md),
  [`cpt_leverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_leverage.md),
  [`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md),
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md),
  [`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`cpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md),
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md).

- **Supervised detection:**
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md),
  [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md),
  [`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md),
  [`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md).

- **Choosing a method:**
  [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md),
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md).

- **Evaluation:**
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
  [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md),
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
  [`cpt_datasets()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_datasets.md),
  [`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md).

- **Streaming:**
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
  [`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md),
  [`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md),
  [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md),
  [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md).

- **Study design:**
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md),
  [`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md),
  [`cpt_scenarios()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scenarios.md).

- **Communication:**
  [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md),
  [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md),
  [`cpt_gt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gt.md),
  [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md),
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).

- **Extension:**
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  and
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  bring detectors this package does not and cannot depend on into the
  same grammar.

## See also

The entry points, by group:

- **Detect:**
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md).

- **Visualise:**
  [`autoplot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md),
  [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md),
  [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md).

- **Inference and selection:**
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md),
  [`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md),
  [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md).

- **Choosing and combining methods:**
  [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
  [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md),
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).

- **Evaluation:**
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md).

- **Streaming:**
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md).

- **Study design:**
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
  [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md).

- **Communication:**
  [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).

Useful links:

- <https://pursuitofdatascience.github.io/ggchangepoint/>

- Report bugs at
  <https://github.com/PursuitOfDataScience/ggchangepoint/issues>

## Author

**Maintainer**: Youzhi Yu <yuyouzhi666@icloud.com>
