# ggchangepoint: A Roadmap for the Next Release

### Expanding tidy, visualization-first changepoint detection in R

**Author:** Youzhi Yu **Status:** Design document / development roadmap
for `ggchangepoint` 0.4.0 and beyond **Current release:**
`ggchangepoint` 0.3.0 (CRAN)

------------------------------------------------------------------------

## Abstract

`ggchangepoint` 0.3.0 closed the documentation gap of the 0.2.0 cycle:
the README introduces every export, the dispatcher is honest (only wired
methods are offered), a
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
introspection table ships, the `ggcpt` S3 surface is complete
(`summary`/`as_tibble`/`as.data.frame`/`format`/`plot`), and eleven
verified 0.2.0 bugs (B1–B11) were fixed.

This document plans **0.4.0**, whose brief is the opposite of 0.3.0’s:
not documentation, but **methods**. Guided by a fresh
literature-and-CRAN survey (July 2026; §2), 0.4.0 ships the **largest
engine wave in the package’s history** — eighteen new wrappers spanning
multiscale inference with confidence intervals (SMUCE/HSMUCE), exact
change-in-slope (CPOP), the CROPS penalty path, Bayesian detection
offline and online (bcp, BOCPD, BEAST), sequential/nonparametric testing
(CPM, kernel running statistics, NP-MOJO), robustness to drift,
autocorrelation and dependence (DeCAFS, self-normalisation, EnvCpt),
high-dimensional and multivariate detection (inspect, ocd, geomcp),
regression breaks (Bai–Perron/strucchange, segmented), and the modern
fastcpd engine (mean/var/ARMA/GARCH) — taking
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
from 13 to **31 wired methods** (§3). Around the new engines it builds
the supporting features that compound across all of them:
uncertainty-aware plotting (`show_ci`, `show_fit`, posterior and
run-length displays), genuine multivariate input with faceted rendering,
panel/batch detection, bootstrap stability diagnostics, interactive
rendering, and per-method citations (§4). It also fixes **twenty
verified bugs** found by a systematic audit that ran the 0.3.0 code (§6)
— including an `ecp` wrapper that fabricated changepoints on no-change
data, a WBS wrapper that silently discarded the model selection it
claimed to use, and metrics that punished a correct “no changepoints”
answer.

The two commitments that define the package are unchanged: **(i) every
detector returns a tidy tibble inside a structured `ggcpt` object**, and
**(ii) every result is directly renderable with `ggplot2`**. 0.4.0 adds
a third: **(iii) where a method quantifies uncertainty — confidence
intervals, posterior probabilities, run-length distributions — the
`ggcpt` object carries it and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
can draw it.**

------------------------------------------------------------------------

## Contents

1.  [Status: where 0.3.0 landed](#id_1-status-where-030-landed)
2.  [The 2026 method survey: what the field considers
    essential](#id_2-the-2026-method-survey)
3.  [The 0.4.0 engine wave](#id_3-the-040-engine-wave)
4.  [New features beyond detectors](#id_4-new-features-beyond-detectors)
5.  [Deferred engines and the 0.5.0
    horizon](#id_5-deferred-engines-and-the-050-horizon)
6.  [The 0.4.0 bug audit (verified)](#id_6-the-040-bug-audit-verified)
7.  [Architecture, dependencies,
    testing](#id_7-architecture-dependencies-testing)
8.  [Backward compatibility](#id_8-backward-compatibility)
9.  [References](#references)

------------------------------------------------------------------------

## 1. Status: where 0.3.0 landed

0.3.0 delivered documentation parity (README/vignettes/help cover all
~40 exports), the honest dispatcher (13 wired methods, planned ones
listed in
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
instead of erroring at runtime), the completed S3 surface, and fixes
B1–B11 from the 0.2.0 audit (the broken
[`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
loop, `recall > 1` in
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
silent `change_in` mislabelling, flattened multivariate `ecp` input,
misplaced
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
rules, and others).

What 0.3.0 explicitly deferred — and what defines 0.4.0:

- **The method backlog.** Thirteen methods were listed as *planned*:
  `smuce`, `hsmuce`, `kcp`, `cpm`, `robust`, `decafs`, `sn`, `inspect`,
  `sbs`, `bcp`, `bocpd`, `strucchange`, `segmented`.
- **[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  had no producer.** The CI geom shipped in 0.2.0, but no engine emitted
  `ci_lower`/`ci_upper`.
- **Multivariate input was routed but not rendered.** `ecp` stopped
  being flattened, but
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  still drew only the first coordinate.
- **Orchestration.** No batch/panel loop, no stability diagnostics, no
  penalty-path tooling.

## 2. The 2026 method survey

A nine-area literature-and-CRAN sweep (penalised/optimal, multiscale,
nonparametric/kernel, Bayesian, high-dimensional, regression breaks,
online/sequential, robust/dependence, and recent benchmarking work,
2020–2026) was run in July 2026 with every citation and CRAN status
verified. Its actionable conclusions:

1.  **Confidence statements are now table stakes.** SMUCE (Frick, Munk
    and Sieling, 2014) and HSMUCE (Pein, Sieling and Munk, 2017) give
    simultaneous confidence sets; Bai–Perron (`strucchange`) and
    `segmented` give break-date CIs. A visualisation-first package that
    cannot draw an interval around a changepoint is behind the field.
2.  **The Bayesian pillar is mandatory.** Barry–Hartigan (`bcp`), BOCPD
    (Adams and MacKay, 2007; `ocp`), and the widely used BEAST ensemble
    (`Rbeast`) are all on CRAN and produce the field’s signature
    graphics (posterior profiles, run-length heatmaps) — exactly this
    package’s remit.
3.  **Slope changes deserve an exact engine.** `cpop` (Fearnhead,
    Maidstone and Letchford, 2019; JSS software paper 2024) is on CRAN
    and is the canonical answer to the `change_in = "slope"` request
    that 0.3.0 could only route to NOT’s contrast.
4.  **fastcpd is the notable newcomer.** Li and Zhang’s `fastcpd` (2024)
    reached CRAN 1.0.0 in 2026 and covers mean/variance/GLM/ARMA/GARCH
    families under one PELT-style interface — the survey’s
    highest-priority “new package to not miss”.
5.  **Dependence-aware methods prevent the classic false positive.**
    DeCAFS (drift + AR noise), SNSeg (self-normalisation), EnvCpt
    (changepoints vs trends vs memory), and NP-MOJO (`CptNonPar`,
    nonparametric under serial dependence) are all on CRAN and address
    the most common practical failure of naive mean-shift detection.
6.  **CRAN availability rules out some 0.3.0 plans.** `gfpop` was
    removed from CRAN (2024) and remains GitHub-only; `robseg` and
    `FOCuS` were never on CRAN; `hdbinseg` (SBS) was archived. These
    cannot live in `Suggests` of a CRAN package and are deferred (§5) —
    [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
    says so honestly. `bcp` and `cpm` are on CRAN as of mid-2026 (both
    had brief archival episodes historically; pin versions if that
    recurs).
7.  **Evaluation conventions have settled.** van den Burg and Williams
    2020. covering/F1 under one-to-one matching is the benchmark
          standard — which the audit (§6, C12–C15) shows the 0.3.0
          metrics module implemented inconsistently.

## 3. The 0.4.0 engine wave

Eighteen new wrappers, all engines on CRAN, all in `Suggests` behind
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards.
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
grows from 13 to 31 methods; the `method × change_in` capability matrix
is validated centrally and errors — never silently substitutes.

| Family | Method (`cpt_detect` name) | Engine | Wrapper | Distinctive output |
|----|----|----|----|----|
| Multiscale inference | `smuce`, `hsmuce` | `stepR` | [`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md) | **CI columns** + step fit |
| Penalty path | (path object) | `changepoint` | [`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md) | `ggcpt_path`: elbow/path/segmentation plots |
| Slope | `cpop` | `cpop` | [`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md) | broken-line fit in `$data$fitted` |
| Bayesian | `bcp` | `bcp` | [`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md) | `posterior_prob` column, posterior mean |
| Bayesian online | `bocpd` | `ocp` | [`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md) | run-length posterior ([`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)) |
| Bayesian ensemble | `beast` | `Rbeast` | [`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md) | `posterior_prob`, mean trend |
| Sequential | `cpm` | `cpm` | [`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md) | `detection_time` column |
| Kernel | `kcp` | `kcpRS` | [`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md) | running-statistic changes (mean/var/AR/cor) |
| NP + dependence | `npmojo` | `CptNonPar` | [`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md) | distribution changes under serial dependence |
| Drift + AR | `decafs` | `DeCAFS` | [`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md) | estimated signal in `fitted` |
| Self-normalised | `sn` | `SNSeg` | [`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md) | mean/var/acf/correlation changes |
| High-dim | `inspect` | `InspectChangepoint` | [`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md) | `strength` column, multivariate facets |
| High-dim online | `ocd` | `ocd` | [`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md) | `declared_at`, auto baseline handling |
| Multivariate | `geomcp` | `changepoint.geo` | [`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md) | `mapping` column (distance/angle) |
| Regression | `strucchange` | `strucchange` | [`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md) | Bai–Perron breaks + **CI columns**; formula input |
| Broken line | `segmented` | `segmented` | [`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md) | kink CIs + fitted broken line |
| Model selection | `envcpt` | `EnvCpt` | [`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md) | changepoints only if they beat trend/AR models |
| Modern PELT | `fastcpd` | `fastcpd` | [`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md) | mean/var/meanvar + AR/ARMA/GARCH families |

Cross-cutting dispatcher work that shipped with the wave:

- **Central capability validation.** `method_change_in_support()` is the
  single source of truth; `change_in = "slope"` now routes to `cpop` or
  NOT’s linear contrast, `"var"` to NOT’s meanvar contrast, etc. The
  0.3.0 blanket “slope not supported” error is gone.
- **Multivariate routing.** Univariate methods **error** on wide input
  (previously: silent column-major flattening, §6 C3); multivariate
  methods (`ecp`, `inspect`, `geomcp`, `ocd`, `npmojo`, `kcp`,
  `fastcpd`, `sn`) receive the matrix intact and store a `data_wide`
  slot that
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  renders as facets.
- **Penalty resolution.** Character penalties resolve through
  [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  for numeric-penalty engines, including `"None"` → 0 (§6 C10);
  `cpt_penalty("sSIC")` now implements the actual strengthened SIC
  (k(n)^) (§6 C11).
- **A shared `ggcpt_build()` constructor** guarantees the contract
  (sorted, deduplicated, in-range `cp`; aligned extra columns; segments;
  optional `fitted` signal and `data_wide`) for every new wrapper.

## 4. New features beyond detectors

- **Uncertainty-aware
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).**
  `show_ci = TRUE` draws changepoint-location intervals (SMUCE/HSMUCE,
  strucchange, segmented) as whiskers; `show_fit = TRUE` overlays the
  engine’s fitted signal (SMUCE, DeCAFS, CPOP, segmented, bcp, BEAST);
  multivariate results facet automatically.
  [`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  finally has producers — and was migrated off the deprecated
  `geom_errorbarh()`.
- **Bayesian displays.**
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
  (series + posterior mean above, per-location changepoint probability
  below) and
  [`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
  (the BOCPD run-length heatmap).
- **[`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
  and the `ggcpt_path` class.** The CROPS penalty path with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
  `autoplot(type = c("elbow", "path", "segmentations"))` — penalty
  selection as a diagnostic, not a guess.
- **[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md).**
  One detector over many series (matrix/data frame/list), returning a
  `ggcpt_batch` tibble with list-columns,
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), faceted
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  and `future` parallelism with reproducible RNG.
- **[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md).**
  Segment-preserving bootstrap re-detection frequencies — a
  model-agnostic confidence signal for the many engines with no native
  intervals; renders as a frequency profile.
- **[`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md).**
  Any result as a `plotly` widget (`Suggests`); the static path is
  untouched.
- **[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).**
  The verified methodological reference(s) behind any result or method
  name.

## 5. Deferred engines and the 0.5.0 horizon

Deferred for CRAN-availability reasons (tracked in
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
as `planned`): `gfpop` (removed from CRAN 2024), `robseg` and `FOCuS`
(never on CRAN), `hdbinseg`/SBS (archived). If they return to CRAN they
slot into the existing wrapper pattern in an afternoon.

Planned for 0.5.0+: streaming interfaces for the online engines (`cpm`,
`ocd`, BOCPD currently run in batch-replay mode); TCPD-style
multi-annotator benchmark loaders; a benchmark-study harness over
[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
×
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
grids; time/date-index support (`ts`/`xts`/`tsibble`); `cpt_explore()`
(Shiny penalty tuner); solution-path plots for WBS/NOT/MOSUM internals;
API freeze toward 1.0.0 and a JOSS/R Journal software paper.

## 6. The 0.4.0 bug audit (verified)

A six-agent audit ran the 0.3.0 code path by path; every claim below was
reproduced on R 4.4.1 before being fixed, and each fix carries a
regression test (`tests/testthat/test-040-bugfixes.R`).

#### Correctness — wrong results

- **C1 (critical).
  [`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
  fabricated changepoints on no-change data.** With no changepoints,
  `estimates = c(1, n+1)` and the positional strip
  `estimates[2:(length-1)]` evaluated `2:1`, returning the reversed
  boundaries — a ghost changepoint at `n` (with `cp_value = NA`) instead
  of the documented empty tibble. Same root cause silently dropped
  genuine changepoints in `e.agglo`’s wrap-around case. Fixed by
  value-based filtering (`estimates > 1 & estimates <= n`).
- **C2 (critical).
  [`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md)
  discarded the model selection it claimed.** The default branch
  computed the sSIC selection but read `cpt.th[[1]]` — the unrelated
  threshold selection — and labelled the result “sSIC”. Fixed to read
  `cpt.ic$ssic.penalty`; a manual threshold is now recorded as the
  penalty actually used.
- **C3 (major). Univariate wrappers silently flattened matrices.** A
  100×2 matrix became a 200-point series with meaningless changepoints.
  All univariate paths now error with the list of multivariate methods;
  single-column matrices/data frames still work.
- **C9 (major).
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  did not forward `change_in` to NOT.**
  `method = "not", change_in = "var"` ran the mean contrast and reported
  “mean”. Contrasts are now mapped centrally (`var` →
  `pcwsConstMeanVar`, `slope` → `pcwsLinContMean`) and the object
  reports what ran.
- **C10 (major). `penalty = "None"` silently became the default.** For
  numeric-penalty engines, unmapped strings fell through to `NULL` →
  engine default; `"None"` now resolves to 0.
- **C11 (major). `cpt_penalty("sSIC")` returned half of SIC.** The
  “strengthened” criterion was weaker than BIC, over-detecting wherever
  used; now (k(n)^{1.01}).
- **C18 (major).
  [`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
  used absolute levels for the Donoho–Johnstone jumps.** The classic
  signal is the cumulative sum of the jump heights; benchmarks against
  the literature were scored against the wrong signal.
- **C19 (minor). t-noise had sd ≈ 1.73×`sd`.** `rt(n, df) * sd` is not
  sd-`sd` noise; now rescaled by (), with `df <= 2` rejected.

#### Contract violations and crashes

- **C4 (major).**
  [`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md)
  errored on no-change data (“No change-points found…”) instead of
  returning the documented empty result. Normalised.
- **C5 (major).**
  [`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md)
  reported a spurious changepoint on constant data (breakfast’s default
  “lp” selector), crashed on short series (empty `cptmodel.list`), and
  mishandled the scalar-0 sentinel (producing a corrupt
  two-segment/zero-changepoint object). Selector pinned to “ic”; both
  edge cases handled in a shared `breakfast_cpts()`.
- **C6 (major).**
  [`glance()`](https://generics.r-lib.org/reference/glance.html) on an
  fpop result returned **n rows** — `$fit$cost` is a length-n vector and
  tibble recycled every column. Also `$` partial matching grabbed
  DeCAFS’s `costFunction`. Exact `[[` subsetting + terminal-cost
  extraction; glance is one row always.
- **C7/C8 (major).**
  [`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md)
  stored the threshold *type string* as the numeric penalty value
  (breaking `bind_rows` over glances), and its documented `multiscale`
  argument was ignored. Both fixed; `multiscale = TRUE` now calls
  [`mosum::multiscale.localPrune()`](https://rdrr.io/pkg/mosum/man/multiscale.localPrune.html).
- **C14 (minor).**
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  crashed opaquely when an index exceeded `n`; now drops out-of-range
  indices with a warning.
- **C20 (minor).** The signal generators produced nonsense for small `n`
  (negative `length.out`, nine zero changepoints, `n` itself as a
  changepoint); all now validate their minimum sizes.

#### Metric semantics (van den Burg–Williams alignment)

- **C12 (major).** Both-empty pred/truth scored precision = recall = F1
  = 0 — punishing a perfect “no changepoints” answer while covering and
  Rand said 1. Now 1 across the board.
- **C13 (major).** Empty predictions scored covering 0, though the
  induced trivial partition has a well-defined (positive) covering; and
  the ARI guard returned 1 for chance-level agreement
  (`index == expected`). Both corrected.
- **C15 (minor).**
  [`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
  classified many-to-one while
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
  matched one-to-one, so the picture contradicted the numbers; both now
  share `match_changepoints()`, and the “Miss” legend entry actually
  renders.

#### Visualisation

- **C16 (major).** `ggcpt_compare(layout = "facet")` errored when no
  method found changepoints, and silently dropped panels for methods
  that found none. Panels now come from the method list.
- **C17 (major).**
  [`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
  detected on data-frame **row order** (shuffled rows → different
  changepoints) and emitted the “dropped aesthetics” warning on every
  build. Now sorts by `x` and declares `dropped_aes`.
- Also fixed: `show_ci` was documented but unimplemented (§4 makes it
  real);
  [`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
  sat on deprecated `geom_errorbarh()`;
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)’s
  `...` silently swallowed misspelled arguments (now warns); empty
  `ggcpt` objects produced Inf-limit plots (now a clean error);
  [`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
  crashed on data-frame input (now plots the first column with a
  message); `cpt_wrapper(cp_method = "SegNeigh")` always errored under
  the default penalty (now falls back to SIC like the dispatcher);
  `print.ggcpt` double-printed its truncation message; `np` results now
  report `change_in = "distribution"` and `meanvar` stays `"meanvar"` in
  the user’s vocabulary.

## 7. Architecture, dependencies, testing

- **Imports unchanged** (`changepoint`, `changepoint.np`, `ecp` +
  tidyverse/broom/ggplot2 infrastructure). Eighteen engines + `plotly`
  joined `Suggests`. Nothing not-on-CRAN is referenced.
- **File layout:** new `wrap-inference.R` (stepR), `wrap-bayes.R`
  (bcp/ocp/Rbeast), `wrap-nonparam.R` (cpm/kcpRS/CptNonPar),
  `wrap-robust.R` (DeCAFS/SNSeg), `wrap-regression.R`
  (strucchange/segmented/EnvCpt), `wrap-highdim.R` (inspect/ocd/geomcp),
  `wrap-slope.R` (cpop), `wrap-fastcpd.R`, `crops.R`, `batch.R`,
  `stability.R`, `cite.R`, `posterior-plots.R`, and the shared
  `ggcpt-build.R`.
- **Testing:** three new test files — per-wrapper contract tests behind
  `skip_if_not_installed()`, tool tests (CROPS/batch/stability/cite/
  posterior plots/autoplot extensions), and a regression test per audit
  item C1–C20. The suite passes with all engines installed and with
  none.
- **Docs:** every new export has runnable `@examples` (guarded with
  `@examplesIf`), the README and the pkgdown reference index cover the
  full surface (enforced by the existing coverage test), and the
  vignettes are written in research-paper format.

## 8. Backward compatibility

- The full 0.3.0 surface keeps working unchanged; the
  `tibble(cp, cp_value)` contract is untouched (new columns are
  additive).
- Two behaviour changes are deliberate bug fixes, not breaks: univariate
  methods error on wide matrices instead of flattening (C3), and
  `cpt_detect(change_in = ...)` mappings that silently ran something
  else now either run the right thing or error (C9). No correct program
  is affected.
- `validate_method_change_in()` accepts `change_in = "mean"` for every
  method (routing it to the method’s native change type), so all
  existing single-argument calls keep working.
- No exported function is deprecated in this cycle.

------------------------------------------------------------------------

## References

All verified against the published record in July 2026.

1.  Adams, R. P. and MacKay, D. J. C. (2007). *Bayesian online
    changepoint detection.* arXiv:0710.3742. (R package `ocp`.)
2.  Anastasiou, A. and Fryzlewicz, P. (2022). *Detecting multiple
    generalized change-points by isolating single ones.* **Metrika** 85,
    141–174. (R package `IDetect`.)
3.  Arlot, S., Celisse, A. and Harchaoui, Z. (2019). *A kernel multiple
    change-point algorithm via model selection.* **JMLR** 20(162), 1–56.
    (R package `kcpRS` via Cabrieto et al.)
4.  Bai, J. and Perron, P. (1998). *Estimating and testing linear models
    with multiple structural changes.* **Econometrica** 66(1), 47–78.
5.  Bai, J. and Perron, P. (2003). *Computation and analysis of multiple
    structural change models.* **J. Applied Econometrics** 18(1), 1–22.
    (R package `strucchange`.)
6.  Baranowski, R., Chen, Y. and Fryzlewicz, P. (2019).
    *Narrowest-over-threshold detection of multiple change points.*
    **JRSS-B** 81(3), 649–672. (R package `not`.)
7.  Barry, D. and Hartigan, J. A. (1993). *A Bayesian analysis for
    change point problems.* **JASA** 88(421), 309–319. (R package
    `bcp`.)
8.  Beaulieu, C. and Killick, R. (2018). *Distinguishing trends and
    shifts from memory in climate data.* **Journal of Climate** 31(23),
    9519–9543. (R package `EnvCpt`.)
9.  Cabrieto, J., Adolf, J., Tuerlinckx, F., Kuppens, P. and
    Ceulemans, E. (2018). *Detecting long-lived autodependency changes
    in a multivariate system via change point detection and regime
    switching models.* **Scientific Reports** 8, 15637. (R package
    `kcpRS`.)
10. Chen, Y., Wang, T. and Samworth, R. J. (2022). *High-dimensional,
    multiscale online changepoint detection.* **JRSS-B** 84(1), 234–266.
    (R package `ocd`.)
11. Eichinger, B. and Kirch, C. (2018). *A MOSUM procedure for the
    estimation of multiple random change points.* **Bernoulli** 24(1),
    526–564. (R package `mosum`.)
12. Erdman, C. and Emerson, J. W. (2007). *bcp: An R package for
    performing a Bayesian analysis of change point problems.* **JSS**
    23(3), 1–13.
13. Fearnhead, P., Maidstone, R. and Letchford, A. (2019). *Detecting
    changes in slope with an L0 penalty.* **JCGS** 28(2), 265–275.
14. Fearnhead, P. and Grose, D. (2024). *cpop: Detecting changes in
    piecewise-linear signals.* **JSS** 109(7), 1–30. (R package `cpop`.)
15. Frick, K., Munk, A. and Sieling, H. (2014). *Multiscale change point
    inference.* **JRSS-B** 76(3), 495–580. (R package `stepR`.)
16. Fryzlewicz, P. (2014). *Wild binary segmentation for multiple
    change-point detection.* **Annals of Statistics** 42(6), 2243–2281.
    (R package `wbs`.)
17. Fryzlewicz, P. (2018). *Tail-greedy bottom-up data decompositions
    and fast multiple change-point detection.* **Annals of Statistics**
    46(6B), 3390–3421. (R package `breakfast`.)
18. Fryzlewicz, P. (2020). *Detecting possibly frequent change-points:
    Wild Binary Segmentation 2 and steepest-drop model selection.* **J.
    Korean Statistical Society** 49, 1027–1070. (R package `breakfast`.)
19. Grundy, T., Killick, R. and Mihaylov, G. (2020). *High-dimensional
    changepoint detection via a geometrically inspired mapping.*
    **Statistics and Computing** 30, 1155–1166. (R package
    `changepoint.geo`.)
20. Haynes, K., Eckley, I. A. and Fearnhead, P. (2017). *Computationally
    efficient changepoint detection for a range of penalties.* **JCGS**
    26(1), 134–143. (CROPS; `changepoint`.)
21. Haynes, K., Fearnhead, P. and Eckley, I. A. (2017). *A
    computationally efficient nonparametric approach for changepoint
    detection.* **Statistics and Computing** 27(5), 1293–1305. (R
    package `changepoint.np`.)
22. Killick, R., Fearnhead, P. and Eckley, I. A. (2012). *Optimal
    detection of changepoints with a linear computational cost.*
    **JASA** 107(500), 1590–1598. (PELT; R package `changepoint`.)
23. Killick, R. and Eckley, I. A. (2014). *changepoint: An R package for
    changepoint analysis.* **JSS** 58(3), 1–19.
24. Li, X. and Zhang, X. (2024). *fastcpd: Fast change point detection
    in R.* arXiv:2404.05933. (R package `fastcpd`.)
25. Maidstone, R., Hocking, T., Rigaill, G. and Fearnhead, P. (2017).
    *On optimal multiple changepoint algorithms for large data.*
    **Statistics and Computing** 27(2), 519–533. (R package `fpop`.)
26. Matteson, D. S. and James, N. A. (2014). *A nonparametric approach
    for multiple change point analysis of multivariate data.* **JASA**
    109(505), 334–345. (R package `ecp`.)
27. McGonigle, E. T. and Cho, H. (2025). *Nonparametric data
    segmentation in multivariate time series via joint characteristic
    functions.* **Biometrika** 112(2), asaf024. (R package `CptNonPar`.)
28. Muggeo, V. M. R. (2003). *Estimating regression models with unknown
    break-points.* **Statistics in Medicine** 22(19), 3055–3071. (R
    package `segmented`.)
29. Muggeo, V. M. R. (2008). *segmented: An R package to fit regression
    models with broken-line relationships.* **R News** 8(1), 20–25.
30. Pein, F., Sieling, H. and Munk, A. (2017). *Heterogeneous change
    point inference.* **JRSS-B** 79(4), 1207–1227. (HSMUCE; R package
    `stepR`.)
31. Romano, G., Rigaill, G., Runge, V. and Fearnhead, P. (2022).
    *Detecting abrupt changes in the presence of local fluctuations and
    autocorrelated noise.* **JASA** 117(540), 2147–2162. (R package
    `DeCAFS`.)
32. Ross, G. J. (2015). *Parametric and nonparametric sequential change
    detection in R: The cpm package.* **JSS** 66(3), 1–20.
33. van den Burg, G. J. J. and Williams, C. K. I. (2020). *An evaluation
    of change point detection algorithms.* arXiv:2003.06222.
34. Wang, T. and Samworth, R. J. (2018). *High dimensional change point
    estimation via sparse projection.* **JRSS-B** 80(1), 57–83. (R
    package `InspectChangepoint`.)
35. Zeileis, A., Leisch, F., Hornik, K. and Kleiber, C. (2002).
    *strucchange: An R package for testing for structural change in
    linear regression models.* **JSS** 7(2), 1–38.
36. Zhao, K., Wulder, M. A., Hu, T., et al. (2019). *Detecting
    change-point, trend, and seasonality in satellite time series data…*
    (BEAST). **Remote Sensing of Environment** 232, 111181. (R package
    `Rbeast`.)
37. Zhao, Z., Jiang, F. and Shao, X. (2022). *Segmenting time series via
    self-normalisation.* **JRSS-B** 84(5), 1699–1725. (R package
    `SNSeg`.)

------------------------------------------------------------------------

*This document is a living roadmap; signatures and milestones will be
refined as implementation proceeds. Contributions and method suggestions
are welcome via the issue tracker.*
