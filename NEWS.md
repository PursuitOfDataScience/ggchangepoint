# ggchangepoint 0.2.0

## Major changes
- New `ggcpt` S3 result class with `tidy()`, `glance()`, `augment()`, and `autoplot()` methods
- New `cpt_detect()` unified dispatcher for all changepoint methods
- New geoms: `geom_changepoint()`, `geom_cpt_segment()`, `geom_cpt_ci()`, `stat_changepoint()`
- New first-wave wrappers: WBS/WBS2, NOT, MOSUM, FPOP, gfpop, Isolate-Detect, TGUH
- New `ggcpt_compare()` and `ggcpt_compare_table()` for method comparison
- New evaluation module: `cpt_metrics()`, `cpt_metrics_annotated()`, `ggcpt_eval()`
- New simulator: `cpt_simulate()`/`rcpt()` and canonical test signals
- New `cpt_penalty()` helper

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
- vdiffr snapshot tests (where available)

# ggchangepoint 0.1.0

- Initial release to CRAN.
