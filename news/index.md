# Changelog

## ggchangepoint 0.4.0

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

### Documentation

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
  (all now six).
- Figure alt text is now specific per figure instead of one generic
  string for every plot.

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
