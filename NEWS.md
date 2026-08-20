# ggchangepoint 0.4.0

## The 0.4.0 engine wave

`cpt_detect()` grows from 13 to 31 wired methods. Eighteen new wrappers, all
of whose engines live on CRAN and enter `Suggests` behind
`requireNamespace()` guards:

- `smuce_wrapper()` — SMUCE/HSMUCE multiscale inference (`stepR`), the first
  engines to populate `ci_lower`/`ci_upper` confidence-interval columns.
- `cpop_wrapper()` — exact change-in-slope detection (`cpop`);
  `cpt_detect(change_in = "slope")` now routes here or to NOT's linear
  contrast instead of erroring.
- `bcp_wrapper()`, `bocpd_wrapper()`, `beast_wrapper()` — the Bayesian
  pillar (`bcp`, `ocp`, `Rbeast`), with `posterior_prob` columns and the
  posterior mean carried as a fitted signal.
- `cpm_wrapper()` — sequential distribution-free detection (`cpm`), with a
  `detection_time` column.
- `kcp_wrapper()` — kernel change point analysis on running statistics
  (`kcpRS`; mean, variance, autocorrelation, correlation).
- `npmojo_wrapper()` — nonparametric MOSUM under serial dependence
  (`CptNonPar`).
- `decafs_wrapper()` — abrupt changes amid drift and AR(1) noise
  (`DeCAFS`).
- `sn_wrapper()` — self-normalised segmentation (`SNSeg`; mean, variance,
  acf, bivariate correlation).
- `inspect_wrapper()`, `ocd_wrapper()`, `geomcp_wrapper()` —
  high-dimensional and multivariate detection (`InspectChangepoint`,
  `ocd`, `changepoint.geo`).
- `strucchange_wrapper()` — Bai-Perron structural breaks with break-date
  confidence intervals (`strucchange`); accepts a bare series or a
  regression formula.
- `segmented_wrapper()` — broken-line regression with kink confidence
  intervals (`segmented`).
- `envcpt_wrapper()` — changepoints vs. trends vs. autocorrelation model
  selection (`EnvCpt`).
- `fastcpd_wrapper()` — the modern fastcpd engine (`fastcpd`), covering
  mean/variance/meanvariance plus AR/ARMA/GARCH model changepoints.

## New tools

- New `cpt_crops()` computes the full CROPS penalty path and returns a
  `ggcpt_path` object with `print()`, `tidy()`, and
  `autoplot(type = c("elbow", "path", "segmentations"))`.
- New `cpt_batch()` runs one detector over many series (matrix, data frame,
  or list) with optional `future` parallelism; returns a `ggcpt_batch`
  tibble with `tidy()` and a faceted `autoplot()`.
- New `cpt_stability()` bootstrap stability diagnostic: segment-preserving
  resampling with a detection-frequency profile and `autoplot()`.
- New Bayesian displays: `ggcpt_posterior()` (posterior mean + per-location
  changepoint probability) and `ggcpt_runlength()` (the BOCPD run-length
  posterior heatmap).
- New `ggcpt_interactive()` renders any result as a `plotly` widget.
- New `cpt_cite()` returns the verified methodological reference(s) behind
  a result or method name.

## Visualisation

- `autoplot.ggcpt()` gains `show_ci` (draws changepoint-location confidence
  intervals from `ci_lower`/`ci_upper`) and `show_fit` (overlays the
  engine's fitted signal), and renders multivariate results as faceted
  small-multiples.
- `geom_cpt_ci()` migrated off the deprecated `ggplot2::geom_errorbarh()`
  to `geom_errorbar(orientation = "y")`.
- Unknown styling arguments passed through `autoplot()`/`ggcptplot()` now
  warn instead of being silently discarded, and plotting an empty `ggcpt`
  errors cleanly instead of producing infinite axis limits.

## Bug fixes (audit items C1-C20; regression-tested)

- `cpt_detect(penalty = <number>)` works for the changepoint-package methods
  (`pelt`, `binseg`, `segneigh`, `amoc`): a numeric penalty is now translated to
  the engine's `penalty = "Manual", pen.value = <number>` instead of erroring
  with "Unknown Penalty" (#2).
- `binseg` / `segneigh` no longer crash on short series that pass validation;
  the maximum number of segments `Q` is clamped to a length-safe value (#3).
- `augment()` uses the engine's fitted signal when the result carries one,
  instead of always the per-segment mean (#4).
- `augment()` keeps all coordinates for a multivariate result instead of
  dropping everything but the first (#5).
- The `segments` table's `start` / `n` columns are integer, matching the
  documented schema (#6).
- `signal_mix()` gains a minimum-`n` guard and filters its changepoint indices,
  so `true_changepoints` no longer contains 0, `n`, or duplicates for small `n`
  (#7).
- `autoplot()` honours the `index` argument for multivariate results (#8).
- `cpt_batch()` / `ggcpt_compare()` no longer crash with "factor level
  duplicated" when two series share a name, or the `methods` vector repeats (#9).
- `wbs` returns an empty result instead of erroring when a manual `threshold`
  admits no changepoints (#10).
- `idetect` returns an empty result on short series instead of erroring with
  "wrong sign in 'by' argument" (#11).
- `ecp_wrapper()` no longer fabricates changepoints on no-change data (the
  positional boundary strip reversed `c(1, n+1)`), and no longer drops
  genuine changepoints in `e.agglo`'s wrap-around case (C1).
- `wbs_wrapper()` now returns the sSIC model selection it documents; a
  manual threshold is recorded as the penalty actually used (C2).
- Univariate wrappers and `cpt_detect()` now error on multi-column input
  instead of silently flattening it column-major (C3).
- `idetect_wrapper()` returns an empty result on no-change data instead of
  erroring (C4).
- `tguh_wrapper()` pins breakfast's model selection to "ic": no more
  spurious changepoint on constant data, no crash on short series, and the
  scalar-0 "no changepoints" sentinel is handled (C5).
- `glance()` is always one row: fpop's per-position cost vector no longer
  explodes the tibble, and `$` partial matching no longer grabs unrelated
  fit elements (C6).
- `mosum_wrapper()` records the numeric threshold as `penalty$value` (was
  the string "critical.value") and implements its documented `multiscale`
  argument via `mosum::multiscale.localPrune()` (C7, C8).
- `cpt_detect()` forwards `change_in` to NOT via contrast mapping and the
  result reports what actually ran (C9); `penalty = "None"` resolves to 0
  for numeric-penalty engines (C10).
- `cpt_penalty("sSIC")` implements the strengthened SIC `k * log(n)^alpha`
  (was `0.5 * k * log(n)`, weaker than BIC) (C11).
- Metrics agree with the van den Burg-Williams conventions: an exactly
  correct empty prediction scores precision/recall/F1 = 1 (C12); empty
  predictions score the trivial-partition covering and chance-level ARI 0
  (C13); out-of-range indices are dropped with a warning instead of
  crashing (C14); `ggcpt_eval()` uses the same one-to-one matching as
  `cpt_metrics()` and its "Miss" legend entry renders (C15).
- `ggcpt_compare()` keeps a facet panel for every method, including those
  that found nothing, and no longer errors when no method finds anything
  (C16).
- `stat_changepoint()` sorts by the `x` aesthetic before detecting (results
  were previously row-order dependent) and declares `dropped_aes` so
  building the plot is warning-free (C17).
- `signal_blocks()` generates the true Donoho-Johnstone blocks signal
  (cumulative jumps, not absolute levels) (C18); simulated t-noise is
  rescaled so its standard deviation matches `sd` (C19); all signal
  generators validate their minimum lengths (C20).
- `cpt_wrapper(cp_method = "SegNeigh")` falls back to the SIC penalty the
  engine supports instead of always erroring under the default; `np`
  results report `change_in = "distribution"`; `meanvar` results stay
  `"meanvar"` in the user's vocabulary; `ggecpplot()` handles multivariate
  input without crashing.

## Bug fixes (pre-release audit; regression-tested)

- Every univariate wrapper now rejects multi-column input instead of
  silently flattening it column-major. The C3 fix had only reached the
  search-based wrappers, so `smuce_wrapper()`, `cpop_wrapper()`,
  `bcp_wrapper()`, `bocpd_wrapper()`, `beast_wrapper()`, `cpm_wrapper()`,
  `decafs_wrapper()`, `strucchange_wrapper()`, `segmented_wrapper()` and
  `envcpt_wrapper()` turned a 120x2 matrix into a 240-point series. The new
  `cpt_crops()` and `cpt_stability()` entry points guard the same way (R16).
- `segneigh` no longer errors with "subscript out of bounds" on short
  series. The `Q` clamp added for #3 missed the engine's real constraint:
  Segment Neighbourhood requires `Q >= 3` regardless of length, so the
  clamped `Q` of 1 or 2 failed for every `n < 8`. `Q` is now clamped into
  the engine's valid window (`3 <= Q <= n - 2` for a mean change,
  `floor(n / 2) + 1` when a variance is estimated per segment), and a series
  too short to admit any valid `Q` gets an actionable message naming the
  constraint instead of the engine's internal error (R17).
- A multivariate coordinate literally named `index` no longer crashes
  `mv_data_wide()` with "Column name `index` must not be duplicated"; it is
  made unique against the position column, so `ecp`, `inspect`, `geomcp`,
  `ocd`, `npmojo`, `kcp` and `fastcpd` all accept such data (R18).
- `NA` changepoint indices from an engine are dropped rather than
  propagating into `build_segments()` as an "NA/NaN argument" error, and any
  engine-supplied extra columns (`ci_lower`, `posterior_prob`, ...) stay
  row-aligned through the drop (R19).
- `cpt_penalty()`'s `"MBIC"` no longer misattributes its formula to Zhang and
  Siegmund (2007), whose modified BIC penalty depends on the segment lengths
  and cannot be written as a function of `n` and `k` alone. The computed
  value is unchanged; the documentation now states what it is (BIC plus a
  combinatorial placement term) and how it differs.
- Passing a wrapper's own argument through `cpt_detect()` no longer errors
  with "formal argument ... matched by multiple actual arguments". The
  dispatcher derives some arguments from `change_in` and was passing them
  alongside the caller's, so the documented `...` passthrough was broken for
  `not`'s `contrast`, `cpm`'s `cpm_type`, `kcp`'s `running_stat`, `sn`'s
  `parameter`, `fastcpd`'s `family` and `hsmuce`'s `family`. A value supplied
  by the caller now wins over the derived one (R20).
- Two enumerated engine options that could never succeed were removed
  (R21): `smuce_wrapper(family = "poisson")` — current `stepR` accepts no
  such family, so it always errored — and
  `cpm_wrapper(cpm_type = "GLRAdjusted")`, which `cpm::processStream()`
  rejects by *printing* an error and returning no changepoints, making it
  silently report "no changes" for any input. `cpm_type = "FET"` is retained
  and documented as needing 0/1 data plus a `lambda` value.
- `ocd_wrapper()` no longer advertises univariate input: `ocd`'s detector
  cannot be constructed for a single coordinate (it fails with "subscript out
  of bounds"), so a bare vector now gets a message naming the requirement
  instead of the engine's internal error (R22).
- Degenerate input is handled the way the rest of the package already handled
  it. A constant series now returns the empty result instead of an opaque
  engine error (`sn`, `kcp`, `npmojo`, `inspect`) or, for `segmented`, a
  spurious kink recovered from a singular fit (R23).
- A single constant coordinate no longer kills a multivariate run. `inspect`,
  `npmojo` and `kcp` standardise each coordinate, so one flat column (a dead
  sensor channel, say) made their statistics undefined and the whole call
  failed with "missing value where TRUE/FALSE needed" even when the other
  coordinates carried an obvious change. Flat coordinates are now dropped
  with a warning naming them, detection proceeds on the rest, reported
  locations stay in the original row space, and the dropped coordinates are
  still kept for plotting (R24).
- `kcp` and `sn` explain themselves on series too short for their windows,
  instead of surfacing "wrong sign in 'by' argument" and "only 0's may be
  mixed with negative subscripts" (R25).
- `print()` and `summary()` no longer render penalties at full double
  precision or with a placeholder value: `Penalty: Manual =
  17.8459510605346` is now `Manual = 17.846`, and a penalty that carries no
  numeric value prints as `MBIC` rather than `MBIC = NA`.

## Bug fixes (final pre-submission audit; regression-tested)

The whole exported surface was exercised with degenerate, contract-violating
and self-generated input. Items are listed with the ones that change an
answer or end a session first.

- `hsmuce` no longer aborts the R session. When a series carries essentially
  no noise at the per-segment scale, `stepR`'s heterogeneous variance
  estimator does not raise an R error but *terminates the session*, so
  nothing downstream can catch it and the user loses their work. It is
  reachable straight from `cpt_detect(x, method = "hsmuce")`. Two regimes
  were measured as fatal: a globally flat series such as
  `rep(4, 300) + rnorm(300, 0, 2e-7)`, and — more dangerous, because it looks
  entirely ordinary — a clean step whose segments are numerically constant,
  `c(rep(0, 150), rep(5, 150)) + rnorm(300, 0, 1e-9)`, which is what
  `cpt_simulate(sd = 0)` produces once any rounding is added. Both are
  refused when the point-to-point variation lies more than about seven orders
  of magnitude below the data's own scale, with a message naming
  `family = "gauss"`, which handles the whole range. An exactly noiseless
  series is safe upstream and still works (R53).
- `idetect` no longer invents changepoints on a constant series.
  `IDetect::ID()` is erratic on flat input — its statistics go to 0/0, and
  what it returns depends on the value and the length: `rep(3, 200)` came
  back with **126** changepoints at 1, 3, 4, 6, 7, ..., while `rep(0, 100)`
  errors and `rep(-2.5, 60)` returns a sentinel 0. Every other search
  wrapper reports none, and the 0.4.0 audit fixed exactly this class of bug
  for `segmented`, `sn`, `kcp`, `npmojo` and `inspect` — `idetect` was
  missed. It now short-circuits to the empty result, decided by exact
  equality so a series with tiny but genuine variation still reaches the
  engine (R50).
- `cpt_stability()` reports the quantity it documents. `freq` is described as
  "the proportion of replicates detecting a changepoint within `margin` of
  that index", but the loop incremented once per *changepoint*, so a
  replicate whose detections had overlapping ±`margin` windows was counted
  twice at the shared indices; `pmin(hits / B, 1)` then hid the overflow by
  clipping it. The effect was to inflate exactly the number the function
  exists to report — in a measured example an index that only half the
  replicates covered was shown as 1.00, "re-detected every time". Each
  replicate now contributes at most one to any index, so `freq` is a genuine
  proportion and needs no clipping (R38).
- `glance()` always returns the single row it documents. `new_ggcpt()`
  defaulted `method` and `change_in` to `character(0)`, so `tibble()`
  recycled every other column down to zero rows — an empty summary for any
  hand-built result, including the one the README demonstrates. Those
  defaults are now `NA_character_`, and `glance()` coerces the metadata
  fields to length one whatever the object carries (R37).
- The `changepoint` engines (`pelt`, `binseg`, `segneigh`, `amoc`, `np`) keep
  their upstream `cpt` object in `$fit`. It was `NULL`, although `$fit` is
  documented as "the raw upstream object" and every other engine stored one —
  which also left the `inherits(fit, "cpt")` branch of `glance()` unreachable,
  and with it a sign error and a wrong element index that had never run.
  `glance()$total_cost` now reports the unpenalised −2 log L for those
  engines where `changepoint` exposes it on that scale, and stays `NA` where
  it does not, rather than mixing two scales in one column; `?glance.ggcpt`
  spells out which cases are which (R35).
- `ggcpt_interactive()` works on multivariate results. The faceted
  small-multiple that `autoplot()` builds for them used a facet column named
  `variable`, which is also the name `plotly::ggplotly()` gives a column of
  its own when it melts the built plot, so every multivariate result failed
  with "Names must be unique". The column is now `coordinate`; the facet
  strips are unchanged (R36).
- Duplicate multivariate coordinate names no longer abort a run. A matrix may
  legally carry the same colname twice, which made `add_column()` reject the
  wide frame with "must have unique names as of tibble 3.0.0"; the R18 fix
  had only deduplicated a coordinate named `index` against the position
  column, not the coordinates against each other. All coordinate names are
  now made unique in one pass (R34).
- `envcpt` no longer prints its engine's internal failures as though the
  call had failed. `EnvCpt` fits up to twelve models with `try()`, and a
  non-silent `try()` writes its error straight to stderr, so on a degenerate
  series `envcpt_wrapper()` printed six lines beginning "Error in arima(...):
  non-stationary AR part from CSS" and then returned a perfectly good result.
  Those failures are expected — the criterion ignores the models that did not
  fit — so the message stream is diverted for the duration of the call.
  Genuine warnings are deferred past the diversion and still reach the user,
  and a call that really does fail still errors (R52).
- `cpt_simulate(change_in = "meanvar")` works without `params`. It was the
  one change type with no parameter default, so the call died with
  "replacement has length zero" instead of simulating anything (R32).
- `cpt_simulate()` warns about recycled parameters for every change type,
  not only `"mean"`. Supplying fewer parameters than there are segments
  reuses the last one, so the trailing entries of `changepoints` were
  recorded in `true_changepoints` with no actual change behind them —
  silently wrong ground truth for `"var"`, `"meanvar"` and `"slope"` (R32).
- Two more ways to get a silent "no changepoints" are closed. `cpm` ships
  thresholds only for a fixed set of average run lengths; for any other
  `arl0` its `processStream()` *prints* "Error: No thresholds available for
  selected ARL0" and returns an empty result instead of raising a condition,
  so `tryCatch()` never saw it and the wrapper reported zero changepoints on
  a series with an obvious one — the same trap the earlier audit found for
  `cpm_type = "GLRAdjusted"`, on a different argument. And `kcp_wrapper()`
  with `nperm` below 2 either reported nothing (0 or negative) or died inside
  the engine with an unreadable `row.names` error (1). Both are refused now,
  with the supported `arl0` values named in the message (R61).
- An out-of-range `conf_level` no longer hangs `strucchange_wrapper()`.
  `stats::confint()` on a breakpoints fit at `level = 2` never returns, and
  the `tryCatch()` already around that call cannot rescue a call that does
  not terminate — so the session simply locked up. `conf_level` is now
  required to lie strictly between 0 and 1 in both `strucchange_wrapper()`
  and `segmented_wrapper()`. In the same sweep: `bocpd_wrapper(hazard)` and
  `cpop_wrapper(sd)` must be positive, and `wbs_wrapper(n_intervals)` at
  least 1 — all previously accepted meaningless values (R60).
- `cpt_simulate()` refuses parameters that made it emit `NaN`. It is where
  ground truth for every benchmark comes from, so a silent series of `NaN` is
  the worst thing it can produce — and `sd = -1`, `sd = NA`, and `|rho| >= 1`
  under the AR(1) model each did exactly that, with no error and no warning
  (`sqrt(1 - rho^2)` is not a number outside the stationary range).
  Non-positive `n` is refused too. `rho` is checked only for
  `noise = "ar1"`, so a stray value the chosen model ignores is still
  accepted (R59).
- The logical switches refuse a non-logical value instead of silently doing
  the opposite. `show_segments`, `show_ci`, `show_fit`, `show_line`,
  `show_points` and `mosum_wrapper(multiscale)` are all documented as
  "Logical" but were read with `isTRUE()`, which treats everything that is
  not `TRUE` as `FALSE`. So `show_segments = 1`, `= "yes"`, `= "TRUE"` or
  `= NA` quietly drew nothing, and `show_line = 1` quietly *removed* the line
  the user was asking to keep — three layers down to one. `show_points = NULL`
  keeps its documented meaning of deciding from the series length (R58).
- The package's own arguments now enforce the ranges they document. The
  engines police their own — `stepR` refuses an `alpha` outside (0, 1),
  `SNSeg` an unlisted `confidence` — but ggchangepoint's were taken on trust,
  and out-of-range values returned answers instead of errors:
  `cpt_metrics(margin = -3)` scored a *perfect* segmentation as precision 0
  and recall 0; `cpt_stability(B = 0)` produced a stability profile of `NaN`;
  `cpt_metrics(n = -10)` a covering metric of −1; `bcp_wrapper` and
  `beast_wrapper` with `prob_threshold = 0` reported 239 changepoints in a
  240-point series; `kcp_wrapper(alpha = 2)` and `cpt_crops(pen_min = -5)`
  ran regardless. All are refused now, with the legitimate boundaries
  (`margin = 0`, `B = 1`, `n = 1`, `prob_threshold = 1`) still accepted
  (R57).
- `mosum_wrapper()`'s automatic bandwidth is never 1. `min(n / 10, 100)`
  rounds to 1 for every `n < 20`, and a one-observation window leaves the
  engine's studentised statistic undefined, so it warned "NaNs produced" and
  returned spurious changepoints rather than failing. The automatic
  bandwidth is floored at 2, and a series too short for any window gets an
  actionable message (R29).
- `npmojo_wrapper()`'s default bandwidth is capped at `n / 2`, the largest
  the engine accepts. The documented `max(20, 0.1 * n)` exceeded that for
  every series shorter than 40, so the default always failed with
  "Bandwidth is too large for the length of time series". Series of 40 or
  more observations are unchanged (R30).
- `cpt_wrapper(change_in = "np")` refuses `cp_method` values other than
  `"PELT"` up front. `changepoint.np::cpt.np()` implements PELT only, so
  `"BinSeg"` and `"SegNeigh"` used to die on the internal `Q` clamp with
  "unused argument (Q = 5)" and `"AMOC"` surfaced the engine's "Invalid
  Method" (R27).
- `autoplot()`, `ggcptplot()` and `ggecpplot()` reject an `index` whose
  length does not match the series, naming the argument at fault, instead of
  surfacing dplyr's recycling error ("`x` must be size 200 or 1, not 10"),
  which never mentions `index` (R26).
- `cpt_penalty()` enforces the argument ranges it documents (R28):
  `alpha > 1` for `"sSIC"` (at or below 1 it is weaker than BIC, so no
  longer a *strengthened* SIC); `n >= 3` for the log-based penalties
  (`log(n)` is 0 at `n = 1` and `log(log(n))` is negative below `n = 3`, so
  the "penalty" rewarded extra changepoints); and `0 <= k <= n` for
  `"MBIC"`, whose `log C(n, k)` term is `-Inf` beyond that. `"AIC"`, which
  does not involve `n`, is exempt.
- `cpt_batch()` names the series that failed. It exists for panels of
  hundreds of series, but an error in any one of them surfaced only as the
  underlying complaint — "`x` must have at least 3 observations" — leaving
  the user to bisect the list to find which. The message is now prefixed
  with the series name and its position, e.g. ``Series `short` (2 of 3):``
  (R49).
- A result from `cpt_detect()` records the `cpt_detect()` call in `$call`.
  It previously held the internal helper each branch happened to use — e.g.
  `wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)`, an unexported
  function named with the dispatcher's local symbols, which a reader can
  neither recognise nor re-run. Wrappers called directly still record
  themselves (R33).
- `cpt_cite()` on a result with no method name says so, instead of surfacing
  tibble's "Can't subset rows with `refs$method == method`" (R37).
- `ggcpt_eval()` no longer warns "No shared levels found ..." when there is
  nothing to draw: a run with no predictions and no ground truth is a
  perfect score, not a broken plot (R31).
- `ggcpt_compare()` pads its changepoint rules by a fixed amount on a flat
  series, as `ggcptplot()` already did; a zero data range would otherwise
  collapse them to invisible zero-height segments.
- `glance()` no longer carries an unreachable branch. It tested
  `inherits(fit, "cptrange")`, but the `changepoint` class is `cpt.range` —
  with a dot — so the branch could never fire, and its body used `$` on an S4
  object, which would have errored had it ever been reached. Removed; the
  BinSeg/SegNeigh case is handled explicitly alongside the other engines
  whose cost is on a different scale (R46).
- `?new_ggcpt` and `?ecp_wrapper` explain why `$fit` is `NULL` for `"ecp"`
  and only for `"ecp"`: `ecp::e.agglo()` returns a cluster-progression matrix
  that is quadratic in the series length, so retaining it by default would
  make the result object explode on a long series — 207 kB of fit for a
  1.3 kB series at n = 160 alone (R46).
- The covering metric no longer scales quadratically. `cpt_metrics()`
  compared every truth segment against every prediction segment, so scoring
  a segmentation with many changepoints crawled — 7.5 seconds for 3000 of
  them. Because both partitions tile the series and their breakpoints are
  sorted, only the overlapping prediction segments can win, and two
  `findInterval()` lookups locate them; the same case now takes 0.42
  seconds. The numbers are unchanged: verified identical on 4010 cases
  (4000 random plus adversarial partitions) and pinned in the tests against
  an independent set-based statement of the definition (R42).
- The redundant `.onLoad()` is gone. It re-registered `print`, `plot`,
  `summary`, `tidy`, `glance`, `augment` and `autoplot` at load time —
  writing into `base`'s and `generics`' S3 method tables — even though
  NAMESPACE already declares every one of them, and it wrapped the lot in
  `suppressWarnings()`, so a genuine registration failure would have been
  invisible. It was a leftover from before `@exportS3Method base::generic`
  was adopted in 0.3.0. Verified redundant before removing: all eleven
  methods still dispatch with and without the package attached, and every
  declared generic/class pair still resolves through `getS3method()` (R41).
- `?ocd_wrapper` says how long it takes. Nearly all of `ocd`'s cost is Monte
  Carlo threshold calibration, which happens before a single observation is
  read: measured at `mc_reps = 5`, construction is about 3 s at p = 3, 9 s at
  p = 10 and 55 s at p = 50, and four times that at `mc_reps = 20` — so the
  default `mc_reps = 100` extrapolates to roughly a quarter of an hour at
  p = 50. The help now gives those numbers, notes that monitoring the
  observations afterwards is comparatively free, and points at `thresh`,
  which takes the three thresholds directly and skips calibration entirely.
  That escape hatch had no test; it has one now (R56).
- `stats` is declared in `Imports`.
- The documented simulate → detect → evaluate → plot workflow is verified
  end to end. Each piece had its own tests, but not the chain: a result's
  changepoints feeding `cpt_metrics()` and `ggcpt_eval()`, its segments
  feeding `geom_cpt_segment()`, the object itself feeding `cpt_cite()`. The
  chain was run for all 31 methods — it completes for every one, and 24 of
  them recover both planted changepoints with precision, recall, F1 and
  covering all exactly 1. The exceptions are all correct by construction:
  `amoc` finds at most one changepoint, `cpop` and `segmented` are slope
  engines being shown a step, `ocd` is online and reports declaration times,
  and `geomcp` unions its distance and angle mappings. A six-method version
  spanning the structural variety is now in the suite (R55).
- Every configuration of `cpt_simulate()` and every canonical signal was run
  through all 31 methods to confirm none of them can produce input that
  terminates the session. Three configurations do land in the degenerate
  band and are now refused by `hsmuce` rather than crashing it: `sd = 0` and
  `sd = 1e-9` for a change in mean, and — the one the audit turned up —
  `change_in = "slope"` with `sd = 0`, whose consecutive differences are a
  constant slope, so its point-to-point variation is floating-point residue
  of about 1e-14 rather than zero. Nothing else crashes on any of them, and
  the realistic settings and all five canonical signals are unaffected
  (R54).
- The dispatcher's `change_in` translations are tested. `cpt_detect()`
  derives an engine-specific argument from `change_in` for `not`, `cpm`,
  `kcp`, `sn` and `fastcpd`; the suite covered *overriding* those through
  `...` but never the derivation, so a wrong translation would have silently
  run the wrong analysis. Each is now checked against the equivalent explicit
  call (R51).
- `cpt_metrics()`'s one-to-one matching is verified to be a genuine maximum
  matching, which is what `?cpt_metrics` claims and what precision and recall
  are derived from — if the greedy scan ever fell short, both would be
  silently understated. Checked against an exact maximum bipartite matching
  on 300 random configurations plus seven clustered and interleaved patterns
  chosen to break a greedy rule: it never falls short (R48).
- The Bayesian displays' remaining documented paths are tested:
  `ggcpt_posterior()` on a `beast_wrapper()` result (the help says it handles
  both bcp and BEAST, but only the bcp branch of the profile extractor was
  ever run), and every guard on `ggcpt_posterior()`/`ggcpt_runlength()` —
  non-`ggcpt` input, a result with no posterior, and a `prob_floor` that
  leaves nothing to draw (R47).
- Test coverage rose to cover the exported surface that had none. A coverage
  run found two exported functions with no test at all —
  `ggcpt_compare_table()` and `cpt_metrics_annotated()` — alongside a set of
  documented modes and arguments that nothing exercised:
  `ecp_wrapper(algorithm = "agglo")`, `sn_wrapper(parameter = "bivcor")`,
  `cpt_simulate(noise = "ar1" | "rw")`, `signal_mix()`,
  `autoplot(show_segments = TRUE)`, the "no changepoints detected" print
  paths, and the `sd`/`breaks`/`model_param`/`lambda`/`threshold`/`G`
  arguments of the cpop, strucchange, DeCAFS, inspect and mosum wrappers.
  All of them worked; none of them was guarded against a future refactor
  (R45).
- The test suite now really does run with none of the Suggests installed.
  Two assertions reached a Suggests-only engine without a guard —
  `expect_error(fpop_wrapper(X), "univariate")` and the fpop half of the
  scale-sensitivity note — so on a machine with no fpop they met
  "Package 'fpop' is required" instead of the message under test, which is
  an ERROR rather than a skip on CRAN's noSuggests flavour. The earlier
  `_R_CHECK_DEPENDS_ONLY_` run had missed both because the fallback library
  it used still exposed part of Suggests; the suite is now verified against
  a library holding the Imports and nothing else. Both assertions are
  guarded and the pelt half of each stayed unguarded, so the cases that need
  no Suggests still run. A static sweep of every `test_that()` block for a
  Suggests package used without a matching guard found no others (R62).
- `ggcpt_compare()` and `ggcpt_compare_table()` refuse a multi-column `x`
  instead of flattening it. Both run univariate detectors but took
  `as.numeric(x)` on trust, so a 160x2 matrix was unrolled column after
  column and the join between the columns read as a level shift: the table
  came back with changepoints at 80 *and* 160, and 160 is the seam, not a
  feature of either series. Every wrapper already refused wide input through
  the same check; these two entry points were the only ones that did not.
  Non-numeric input now names the argument as well, rather than failing
  inside `as.numeric()` with "cannot coerce type 'object' to vector of type
  'double'". The message points at `cpt_batch()`, which is what runs a
  detector over a panel (R63).
- `ggcpt_compare()` hands `future.apply` a documented `future.seed` value.
  It passed `seed` straight through, and `seed` defaults to `NULL`, which is
  not among the logical/integer/list values `future_lapply()` documents — so
  every parallel comparison run without an explicit seed was outside that
  contract. It now sends `TRUE` in that case, asking for parallel-safe
  L'Ecuyer streams, which is what `cpt_batch()` already did. Sequential runs
  are unaffected. Found by exercising the parallel branch of both functions
  for the first time: it is documented in three vignettes and both help
  pages, and no test had ever set a non-sequential `future::plan()`. The
  branch is otherwise correct — same changepoints as the sequential path,
  series names preserved, `...` forwarded, and the "which series failed"
  error still named (R64).
- `?strucchange_wrapper` says how large its result is. Measuring
  `object.size()` for every engine on one series turned up a single outlier:
  a `strucchange` result is quadratic in the series length, because
  `breakpoints()` keeps `RSS.triang`, the triangular table of segment
  residual sums of squares that lets it return the optimal segmentation for
  any number of breaks without refitting. On a 3.2 kB series it comes to
  1.7 MB at n = 200, 5.9 MB at n = 400 and 22.6 MB at n = 800 — about four
  times larger per doubling — and the table's share of that grows from 85%
  to 95% over the same range. One fit is nothing; a few hundred from
  `cpt_batch()` are, so the help now says to keep `$changepoints` rather than
  the whole list of results. Nothing changed in the object: this is the same
  size-versus-usefulness trade-off already documented for `ecp` in the
  opposite direction, and it was simply unstated. Every other engine is
  ordinary — the median result across the other thirty is under ten times
  the size of the series it was given (R65).
- Asking for one of the four planned methods says so. `cpt_methods()` lists
  `gfpop`, `robust`, `focus` and `sbs` with `status = "planned"`, but
  `cpt_detect(x, method = "gfpop")` went to `match.arg()`, whose message
  enumerates the thirty-one wired methods — so it did not contain the name
  the user had just read out of the table. The table said the name existed
  and the dispatcher said it did not. It now reports what the method is
  waiting on and which package it will be built on; an outright unknown name
  still gets the ordinary list. In the same pass, `sbs`'s entry was out of
  date: it said "when on CRAN", but `hdbinseg` returned to CRAN as 1.0.3 in
  September 2025, so the only thing standing between `sbs` and a user is the
  wrapper. `gfpop` was removed from CRAN and `robseg` and `FOCuS` have never
  been on it, so those three still read "when on CRAN" (R66).

## Documentation

- `?cpt_detect` gains a scale-sensitivity section, and the README and the
  introduction vignette repeat it: `pelt`, `binseg`, `segneigh` and `fpop`
  weigh the penalty against a *raw* segment cost when detecting a change in
  mean, because `changepoint`'s Normal cost fixes the noise standard
  deviation at 1 and `fpop`'s `lambda` penalises the residual sum of squares
  directly. Neither rescales the data, so wider noise makes the penalty
  negligible and the segmentation shatters — on one true changepoint with a
  five-sigma jump, `pelt` returns 1 changepoint at sigma = 1, 29 at sigma = 3
  and 138 at sigma = 10. The note gives the three remedies (standardise the
  series, scale the penalty by the noise variance, or use
  `change_in = "meanvar"`) and records that every other engine estimates or
  cancels the noise scale itself; both halves of it are pinned by a test
  (R39). `?cpt_wrapper`, `?fpop_wrapper` and
  `?cpt_penalty` point at it. Behaviour is unchanged; the trap was simply
  undocumented, and the package's own examples all use unit-variance data,
  so nothing exposed it.
- `?cpt_detect`, `?fpop_wrapper`, `?cpop_wrapper` and `?decafs_wrapper` now
  record that the dispatcher and those wrappers do not share a default
  penalty. `cpt_detect()` resolves its `"MBIC"` default to a numeric value
  that is stronger than the wrappers' own `2 * log(n)` — 19.9 against 11.8 at
  n = 360 — so `cpt_detect(x, method = "decafs")` reports 3 changepoints
  where `decafs_wrapper(x)` reports 5 on the same series. Both defaults were
  documented individually; that they differ was not. Passing `penalty`
  explicitly makes the two entry points agree (R40).
- `?npmojo_wrapper` records that the engine calibrates its detection
  threshold by bootstrap, so the value stored in the penalty descriptor
  varies between runs unless `set.seed()` is called first (or a manual
  threshold is passed through `...`).
- `?cpt_detect` warns that a misspelt engine argument can pass unnoticed.
  `wbs`, `not`, `Rbeast`, `strucchange`, `segmented` and `fastcpd` all end
  their own signature in `...`, so an unrecognised name forwarded through
  `cpt_detect()`'s `...` is discarded upstream and the engine quietly uses
  its default. Intercepting it here would risk rejecting
  arguments those engines legitimately forward deeper, so the behaviour is
  unchanged and documented instead.
- The README and all three vignettes were reviewed against the source and
  corrected. Notably: a `geom_cpt_segment()` example that could not run (it
  was given `xintercept`, but the geom needs `x`/`xend`/`y`/`yend`); `DeCAFS`
  and `EnvCpt` filed under multivariate methods when both are univariate;
  `is_ggcpt()` demonstrated on the input series rather than the result; a
  claim that only three engine packages are required; and a method-family
  count that disagreed between the package help, the README and the
  vignettes (all now six — the feature-tour vignette was the last straggler
  and still said five).
- Figure alt text is now specific per figure instead of one generic string
  for every plot.
- The `ocd_wrapper()` test uses `mc_reps = 10` rather than 50. Those
  repetitions only calibrate the detection threshold, and the change the test
  plants is far too large for the calibration to matter — 10 reps give the
  same declaration as 50 and take 7 seconds instead of 36, cutting the whole
  test suite from 74 to 42 seconds with the assertions unchanged.
- The `ocd_wrapper()` example runs in 3.6 seconds instead of 20. It was by
  far the slowest example in the package — `ocd`'s Monte Carlo threshold
  calibration scales with both the number of coordinates and `mc_reps` — and
  a smaller, cleaner problem (100x3 with `mc_reps = 5`) demonstrates the
  wrapper better anyway: it reports one declaration just after the true
  change, where the old example also produced a spurious second one.
- Two citations were wrong, and the package's three citation sources now
  agree. The TGUH paper was dated 2018 (Annals of Statistics 46(6B),
  3390-3421) by `cpt_cite("tguh")` but 2022 (50(5), 2721-2761) in the
  vignette bibliography — the same paper with two sets of coordinates; the
  bibliography is corrected to match, and its key renamed accordingly.
  `?ecp_wrapper` cited the arXiv preprint of the ecp software paper while
  both vignettes cited its published form, so `inst/REFERENCES.bib` now
  carries the Journal of Statistical Software version (62(7), 1-25). A new
  test cross-validates all three sources: shared BibTeX keys must describe
  the same publication, every `\insertRef` key must resolve in
  `inst/REFERENCES.bib`, and every `@key` cited in a vignette must resolve in
  the vignette bibliography (R44).
- `?stat_changepoint` says which geoms actually work with it. The stat emits
  one `xintercept` per changepoint and drops `x`/`y`, so `"vline"` (the
  default) and `"rug"` fit while `"point"` errors; the help previously read
  as though any geom would do.
- `?geom_cpt_ci` no longer claims an `x` aesthetic is required. The layer is
  a horizontal error bar, so it needs `y`, `xmin` and `xmax`; `x` is accepted
  but unnecessary, and neither of the package's own call sites
  (`autoplot(show_ci = TRUE)` and the feature-tour vignette) supplies it, so
  the help contradicted the package's own usage (R43).
- `?augment.ggcpt` now says what the columns mean for a multivariate result:
  every coordinate is returned and `seg_id`/`is_changepoint` apply to the
  whole row, but `.fitted` and `.resid` describe the first coordinate only —
  the same one `$segments$param_estimate` summarises.
- The penalty-semantics section of `?cpt_penalty` now records the one silent
  substitution the dispatcher makes: `changepoint` does not implement MBIC
  for Segment Neighbourhood, so `cpt_detect(method = "segneigh")` falls back
  to `"SIC"` on the default penalty and its result is therefore not directly
  penalty-comparable with a PELT one.

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
