# Package index

## Core API

- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  : Unified changepoint detection dispatcher
- [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  : Introspect available changepoint detection methods
- [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  : Construct changepoint penalties
- [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
  : Cite the method behind a result
- [`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  : Create a ggcpt object
- [`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)
  : Test if an object is a ggcpt object
- [`print(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  : Print a ggcpt object

## Original wrappers

- [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  : Changepoint wrapper
- [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  : ecp wrapper

## Search and pruning wrappers

- [`fpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md)
  : FPOP wrapper — Functional Pruning Optimal Partitioning
- [`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md)
  : WBS wrapper — Wild Binary Segmentation
- [`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md)
  : WBS2 wrapper — Wild Binary Segmentation 2
- [`not_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/not_wrapper.md)
  : NOT wrapper — Narrowest-Over-Threshold
- [`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md)
  : MOSUM wrapper — Moving Sum
- [`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md)
  : Isolate-Detect wrapper
- [`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md)
  : TGUH wrapper

## Inference and slope wrappers

- [`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md)
  : SMUCE / HSMUCE wrapper — multiscale changepoint inference
- [`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md)
  : CPOP wrapper — optimal change-in-slope detection
- [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`autoplot(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`print(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`tidy(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  : CROPS — the full penalty path of a penalised changepoint method

## Bayesian wrappers

- [`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md)
  : Bayesian changepoint wrapper (Barry-Hartigan product partition
  model)
- [`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md)
  : Bayesian online changepoint detection wrapper (BOCPD)
- [`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md)
  : BEAST wrapper — Bayesian estimation of abrupt change, seasonality,
  and trend

## Nonparametric and sequential wrappers

- [`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md)
  : Sequential change point model wrapper (CPM)
- [`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md)
  : Kernel changepoint wrapper (KCP on running statistics)
- [`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md)
  : Nonparametric MOSUM wrapper (NP-MOJO)
- [`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md)
  : Self-normalisation wrapper (SNSeg)

## Robust and model-selection wrappers

- [`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md)
  : DeCAFS wrapper — changes amid drift and autocorrelated noise
- [`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md)
  : EnvCpt wrapper — changepoints versus trends versus autocorrelation
- [`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md)
  : fastcpd wrapper — fast changepoint detection via sequential gradient
  descent

## Multivariate and high-dimensional wrappers

- [`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md)
  : inspect wrapper — high-dimensional changepoints via sparse
  projection
- [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
  : ocd wrapper — online high-dimensional changepoint detection
- [`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md)
  : Geometrically-inspired multivariate changepoint wrapper (geomcp)

## Regression-break wrappers

- [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
  : Bai-Perron structural break wrapper (strucchange)
- [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md)
  : Broken-line regression wrapper (segmented)

## broom methods

- [`tidy(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tidy.ggcpt.md)
  : Tidy a ggcpt object
- [`glance(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/glance.ggcpt.md)
  : Glance at a ggcpt object
- [`augment(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/augment.ggcpt.md)
  : Augment a ggcpt object
- [`summary(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/summary.ggcpt.md)
  [`print(`*`<summary.ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/summary.ggcpt.md)
  : Summary of a ggcpt object

## Additional S3 methods

- [`as_tibble(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_methods.md)
  [`as.data.frame(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_methods.md)
  [`format(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_methods.md)
  [`plot(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_methods.md)
  : Coerce, format, and plot ggcpt objects

## Theming and helpers

- [`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md)
  : ggchangepoint theme
- [`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md)
  : Annotate segments with alternating shading

## Visualization

- [`autoplot(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md)
  : Autoplot a ggcpt object
- [`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
  : Plot for the changepoint package
- [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
  : Plot for the ecp package
- [`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md)
  : Changepoint vertical rules geom
- [`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md)
  : Changepoint segment level geom
- [`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  : Changepoint confidence interval geom
- [`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
  : Changepoint detection stat
- [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  : Posterior probability plot for Bayesian results
- [`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
  : Run-length posterior heatmap for Bayesian online results
- [`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
  : Interactive changepoint plot

## Method comparison, batch, and stability

- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  : Compare multiple changepoint detection methods
- [`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
  : Comparison table
- [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  [`print(`*`<ggcpt_batch>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  [`tidy(`*`<ggcpt_batch>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  [`autoplot(`*`<ggcpt_batch>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  : Batch changepoint detection over many series
- [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  [`print(`*`<ggcpt_stability>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  [`autoplot(`*`<ggcpt_stability>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  : Changepoint stability diagnostics via bootstrap

## Evaluation

- [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  : Changepoint accuracy metrics
- [`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
  : Multi-annotator evaluation
- [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  : Evaluation visualization

## Simulation and data

- [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  [`rcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  : Generate simulated changepoint data
- [`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
  : Blocks test signal
- [`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md)
  : FMS (Four-Metric-Segments) test signal
- [`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md)
  : Mix test signal
- [`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)
  : Teeth test signal
- [`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md)
  : Stairs test signal

## Re-exports

- [`reexports`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  [`tidy`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  [`glance`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  [`augment`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  [`autoplot`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  [`as_tibble`](https://pursuitofdatascience.github.io/ggchangepoint/reference/reexports.md)
  : Objects exported from other packages
