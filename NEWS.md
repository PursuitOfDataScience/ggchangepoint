# ggchangepoint 0.3.0

## Documentation and coverage
- The README now introduces every exported function, grouped by role, and the
  over-claimed `gfpop` engine (never wrapped) has been removed from it.
- New feature-tour vignette (`vignette("ggchangepoint")`) walking the full
  exported surface, including the per-engine wrappers, `theme_ggcpt()`, and
  `annotate_segments()`.
- The package-level help (`?ggchangepoint`) was rewritten to describe the
  unified `ggcpt` framework and the current 13-method engine list (it previously
  still claimed "only three changepoint packages").
- New documentation-coverage test asserting every export appears in the README.

## New features
- New `cpt_methods()` introspection helper returning a tibble of every known
  method, its engine, availability status, and whether the engine is installed.
- New S3 methods for the `ggcpt` class: `summary()`, `as_tibble()`,
  `as.data.frame()`, `format()`, and `plot()`.
- `cpt_penalty()` gained a documented per-engine penalty-semantics section.

## Bug fixes
- `cpt_detect()` no longer advertises 13 methods that errored at runtime;
  `match.arg()` now enumerates only the wired methods (B7).
- `cpt_detect()` validates `method` × `change_in` combinations and errors with a
  clear message instead of silently mislabelling the result (B3).
- `signal_blocks()` now produces the correct Blocks signal; the segment levels
  previously collapsed to a single step because the assignment loop ran in
  reverse (B1).
- `cpt_metrics()` uses one-to-one matching, so `recall` and `f1` can no longer
  exceed 1 (B2), and no longer warns on empty `pred`/`truth` (B6).
- `ecp_wrapper()` returns a correct per-coordinate `cp_value` for matrix and
  data.frame input instead of a column-major flattened scalar (B4); `cpt_detect()`
  no longer flattens multivariate input before passing it to `ecp`.
- `stat_changepoint()` maps detected indices back to the `x` aesthetic so rules
  land at the correct location on non-`1:n` axes (B5).
- `glance.ggcpt()` now reports a measured `runtime` and populates `total_cost`
  from the underlying fit when available (B8).
- `augment.ggcpt()` renames data columns position-independently, so it no longer
  breaks when the data carries more than two columns (B11).
- `cpt_simulate()` `@return` now documents the `seg_id` column it actually
  returns (B9), and the dead `show_segments` parameter was removed from the
  internal plot helper (B10).

# ggchangepoint 0.2.0

## Major changes
- New `ggcpt` S3 result class with `tidy()`, `glance()`, `augment()`, and `autoplot()` methods
- New `cpt_detect()` unified dispatcher for changepoint methods
- New geoms: `geom_changepoint()`, `geom_cpt_segment()`, `geom_cpt_ci()`, `stat_changepoint()`
- New first-wave wrappers: WBS/WBS2, NOT, MOSUM, FPOP, Isolate-Detect, TGUH
- New `ggcpt_compare()` and `ggcpt_compare_table()` for method comparison
- New evaluation module: `cpt_metrics()`, `cpt_metrics_annotated()`, `ggcpt_eval()`
- New simulator: `cpt_simulate()`/`rcpt()` and canonical test signals
- New `cpt_penalty()` helper
- New `theme_ggcpt()` and `annotate_segments()` for plot customisation

## Hardening (bug fixes)
- `ecp_wrapper()` no-change bug fixed: spurious boundary changepoints and NA no longer emitted
- `size` → `linewidth` migration: `cptline_linewidth` replaces deprecated `cptline_size`
- `match.arg()` input validation added to all wrappers
- Changepoint convention documented and aligned
- "sytle" typo fixed → "style" in documentation
- roxygen modernised to `"_PACKAGE"` sentinel
- `change_in = "np"` alias added (keeps `"cpt_np"` for backward compatibility)
- Full-height changepoint rule default; `show_points` auto-off above 500 obs
- Optional `index` parameter for time-series axes

## Testing
- New `testthat` test suite with coverage for all new and hardened functions

# ggchangepoint 0.1.0

- Initial release to CRAN.
- Exported functions: `cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`, `ggecpplot()`.
