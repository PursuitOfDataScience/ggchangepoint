# Package index

## Core API

- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  : Unified changepoint detection dispatcher
- [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  : Construct changepoint penalties
- [`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  : Create a ggcpt object
- [`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)
  : Test if an object is a ggcpt object
- [`print(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  : Print a ggcpt object

## Existing wrappers

- [`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
  : Changepoint wrapper
- [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  : ecp wrapper

## New wrappers

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

## broom methods

- [`tidy(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tidy.ggcpt.md)
  : Tidy a ggcpt object
- [`glance(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/glance.ggcpt.md)
  : Glance at a ggcpt object
- [`augment(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/augment.ggcpt.md)
  : Augment a ggcpt object

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

## Method comparison

- [`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
  : Compare multiple changepoint detection methods
- [`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
  : Comparison table

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
