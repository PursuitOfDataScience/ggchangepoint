# Changelog

## ggchangepoint 0.3.0

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
