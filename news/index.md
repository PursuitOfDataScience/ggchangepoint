# Changelog

## ggchangepoint 0.2.0

### Major changes

- New `ggcpt` S3 result class with
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html), and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods
- New
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  unified dispatcher for all changepoint methods
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
- vdiffr snapshot tests (where available)

## ggchangepoint 0.1.0

CRAN release: 2022-02-24

- Initial release to CRAN.
