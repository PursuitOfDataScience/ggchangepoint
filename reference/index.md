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
- [`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md)
  : Coerce a time series object to values plus a time index
- [`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
  : Create a ggcpt object
- [`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)
  : Test if an object is a ggcpt object
- [`print(`*`<ggcpt>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)
  : Print a ggcpt object

## Extending the package

Bring a detector this package does not wrap – a non-CRAN engine, a
Python tool, a neural detector, or your own changepoints – into the same
tidy, plottable grammar.

- [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
  : Turn external changepoints into a ggcpt result
- [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  [`cpt_unregister_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  [`cpt_registered_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
  : Register an external changepoint detector
- [`cpt_install_engines()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_install_engines.md)
  : Install the engines behind a family of methods

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
- [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
  : NSP wrapper — Narrowest Significance Pursuit
- [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`autoplot(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`print(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  [`tidy(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  : CROPS — the full penalty path of a penalised changepoint method

## Inference

Where could this changepoint be, and is it real?

- [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  : Confidence intervals for changepoint locations
- [`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md)
  : Test detected changepoints
- [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md)
  : Tidy the significance regions of a ggcpt object

## Choosing the number of changepoints

- [`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  [`print(`*`<ggcpt_selection>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  [`tidy(`*`<ggcpt_selection>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  [`autoplot(`*`<ggcpt_selection>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)
  : Choose the number of changepoints

## Diagnostics

Which observation is driving this, which setting, and what did the
detector actually compute?

- [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  [`print(`*`<ggcpt_influence>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  [`autoplot(`*`<ggcpt_influence>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  [`tidy(`*`<ggcpt_influence>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  : Influence diagnostics for a changepoint segmentation
- [`cpt_leverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_leverage.md)
  : Rank observations by influence
- [`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  [`print(`*`<ggcpt_sensitivity>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  [`tidy(`*`<ggcpt_sensitivity>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  [`autoplot(`*`<ggcpt_sensitivity>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
  : Sensitivity of a segmentation to its tuning parameters
- [`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  : The detector's statistic as a function of location
- [`cpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  : The solution path of a search-based detector
- [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  : Scale space: the statistic across bandwidths

## Supervised detection

Labelled regions as ground truth, label errors as the accuracy measure,
and a learned penalty.

- [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md)
  : Changepoint labels
- [`as_cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_labels.md)
  : Coerce annotations to changepoint labels
- [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  [`tidy(`*`<cpt_label_error>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  [`print(`*`<cpt_label_error>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  : Score a segmentation against labels
- [`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md)
  [`print(`*`<ggcpt_label_curve>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md)
  [`autoplot(`*`<ggcpt_label_curve>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md)
  : Label error as a function of the penalty
- [`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
  [`print(`*`<ggcpt_penalty_model>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
  [`coef(`*`<ggcpt_penalty_model>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
  [`predict(`*`<ggcpt_penalty_model>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
  : Learn a penalty from labelled series

## Choosing and combining methods

- [`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  [`tidy(`*`<ggcpt_recommendation>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  [`print(`*`<ggcpt_recommendation>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
  : Recommend a detection method
- [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  [`autoplot(`*`<ggcpt_consensus>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  [`print(`*`<ggcpt_consensus>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
  : Consensus changepoints across several detectors

## Communication

- [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  [`print(`*`<ggcpt_events>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  [`tidy(`*`<ggcpt_events>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  [`autoplot(`*`<ggcpt_events>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  : Match detected changepoints to known events
- [`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md)
  : A reproducible report of a changepoint analysis
- [`cpt_gt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gt.md)
  : A publication-ready changepoint table

## Benchmarking

- [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  [`print(`*`<ggcpt_benchmark>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  [`tidy(`*`<ggcpt_benchmark>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  [`autoplot(`*`<ggcpt_benchmark>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  : Benchmark detectors across datasets
- [`cpt_datasets()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_datasets.md)
  : A catalogue of benchmark datasets
- [`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md)
  : Download and cache the Turing Change Point Dataset
- [`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md)
  : Per-annotator ground truth for a benchmark dataset

## Streaming and online monitoring

- [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  [`tidy(`*`<ggcpt_monitor>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  [`print(`*`<ggcpt_monitor>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  [`autoplot(`*`<ggcpt_monitor>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
  : A stateful sequential changepoint monitor
- [`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md)
  : Feed observations to a monitor
- [`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md)
  : The alarm log of a monitor
- [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md)
  : Replay a series through a sequential detector
- [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  [`tidy(`*`<ggcpt_delay>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  [`glance(`*`<ggcpt_delay>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  [`print(`*`<ggcpt_delay>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  [`autoplot(`*`<ggcpt_delay>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  : Detection delay and false-alarm rate

## Power and study design

- [`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  [`tidy(`*`<ggcpt_power>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  [`print(`*`<ggcpt_power>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  [`autoplot(`*`<ggcpt_power>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  : Detection power for a changepoint scenario
- [`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md)
  [`print(`*`<ggcpt_min_detectable>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md)
  : The smallest detectable change
- [`cpt_scenarios()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scenarios.md)
  : A grid of simulation scenarios

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
- [`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md)
  : ESAC wrapper — sparsity-adaptive high-dimensional detection
- [`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md)
  : Pilliat wrapper — high-dimensional detection by three complementary
  tests
- [`hdcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdcov_wrapper.md)
  : High-dimensional covariance changepoints
- [`network_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/network_wrapper.md)
  : Dynamic-network changepoints
- [`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md)
  : VAR(1) changepoints
- [`hdreg_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdreg_wrapper.md)
  : High-dimensional regression changepoints

## Functional and network wrappers

- [`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md)
  : Functional mean changepoints
- [`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md)
  : Functional covariance changepoints
- [`kwc_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kwc_wrapper.md)
  : Robust depth-based changepoints for functional and multivariate data
- [`fabisearch_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fabisearch_wrapper.md)
  : Network-structure changepoints via non-negative matrix factorisation

## Regression-break wrappers

- [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
  : Bai-Perron structural break wrapper (strucchange)
- [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md)
  : Broken-line regression wrapper (segmented)
- [`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md)
  : BFAST wrapper — breaks for additive season and trend

## Applied vocabularies and fast paths

- [`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md)
  : Classical single-changepoint tests (Pettitt, Buishand, SNHT)
- [`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md)
  : Taylor's change point analyzer
- [`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)
  : WBS for nonstationary time series
- [`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md)
  : Fast binary segmentation across loss functions

## Bayesian formula models

- [`mcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mcp_wrapper.md)
  : Bayesian formula-based changepoint regression (mcp)

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
- [`plot(`*`<ggcpt_selection>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_stability>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_sensitivity>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_influence>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_batch>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_benchmark>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_consensus>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_monitor>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_delay>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_path>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_power>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_events>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  [`plot(`*`<ggcpt_label_curve>`*`)`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_plot_methods.md)
  : Base plot() methods for ggchangepoint result objects

## Theming, palettes and accessibility

- [`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md)
  : ggchangepoint theme
- [`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md)
  : Annotate segments with alternating shading
- [`scale_colour_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  [`scale_color_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  [`scale_fill_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  [`scale_linetype_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md)
  : Colour-vision-safe scales for changepoint methods
- [`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md)
  [`scale_colour_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md)
  : Colour scales for changepoint labels and label errors

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
- [`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md)
  : Significance region geom
- [`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md)
  : Changepoint label geom
- [`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md)
  : Event annotation geom
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
