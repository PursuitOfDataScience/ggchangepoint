# ggchangepoint: A Roadmap for the Next Release

### Expanding tidy, visualization-first changepoint detection in R

**Author:** Youzhi Yu
**Status:** Design document / development roadmap for `ggchangepoint` 0.3.0 and beyond
**Current release:** `ggchangepoint` 0.2.0 (CRAN)

---

## Abstract

`ggchangepoint` 0.2.0 is on CRAN. It turned the three-engine 0.1.0 prototype into
a unified, tidy, `ggplot2`-native framework: a single S3 result class (`ggcpt`)
with `broom`-style `tidy()`/`glance()`/`augment()` methods, an `autoplot()`
method, a family of composable geoms (`geom_changepoint()`,
`geom_cpt_segment()`, `geom_cpt_ci()`, `stat_changepoint()`), a unified
`cpt_detect()` dispatcher, first-wave wrappers for WBS, WBS2, NOT, MOSUM, FPOP,
Isolate-Detect and TGUH, a method-comparison module with optional `future`
parallelism, an accuracy-metrics module, a data simulator with the canonical
benchmark signals, and a thorough hardening of the original four functions.

This document plans the **next** release. It does two things the 0.2.0 cycle did
not finish, and they come first because they matter most for users *today*:

1. **It closes a documentation and coverage gap.** The package now exports ~37
   user-facing functions, but the README, the package-level help, and even the
   vignettes introduce only a subset; some documentation actively *mis*-states
   the surface (the README advertises a `gfpop` engine that is not wrapped; the
   package-level help still says the package "only include[s] three changepoint
   packages"; `cpt_detect()`'s signature advertises 13 methods that error with
   "not yet implemented"). **§2 makes it a first-class, blocking deliverable that
   the next release introduce *every* feature the package actually provides** —
   in the README, in a vignette, and with a runnable example — and reconcile
   every over- and under-claim. A package that ships features its own front door
   never mentions is a package whose features do not exist for most users.

2. **It makes the dispatcher honest and finishes the foundation** (§3): wire or
   stop advertising the stub methods, add a `cpt_methods()` introspection helper,
   deliver genuine multivariate input instead of silently flattening it, and
   extend the opt-in `future` parallelism beyond `ggcpt_compare()`.

On top of that it plans the **next engine wave** (§4: `gfpop`, SMUCE/HSMUCE,
CROPS penalty paths, robust loss, DeCAFS, self-normalisation), surveys the
**remaining method backlog** with mathematics and citations (§5: Bayesian,
high-dimensional/multivariate, regression breaks, online/sequential, kernel),
and — answering the brief to *think harder about what to build* — proposes a set
of **genuinely new, non-detector features** (§6: completed S3 surface,
time/date and `ts`/`xts`/`tsibble` input, panel/batch detection, interactive and
Shiny exploration, `predict()`/segment labelling, stability diagnostics,
reproducible citations, decomposition-then-detect, and interop/export). The
document closes with the visualization, evaluation, and simulation extensions, an
architecture/dependency/testing plan, a phased release schedule, a record of what
0.2.0 fixed, **a verified bug audit reproducing twelve concrete defects found by
actually running 0.2.0** (§11.3 — including a broken canonical test signal, an
accuracy metric that returns `recall > 1`, and a stat that misplaces changepoints
on dated axes), and a complete, verified reference list.

The two commitments that define the package are unchanged: **(i) every detector
returns a tidy tibble inside a structured `ggcpt` object**, and **(ii) every
result is directly renderable with `ggplot2`**.

---

## Contents

1. [Status: where 0.2.0 landed](#1-status-where-020-landed)
2. [The documentation and coverage gap (and the mandate to close it)](#2-the-documentation-and-coverage-gap-and-the-mandate-to-close-it)
3. [Finishing the foundation: an honest dispatcher, multivariate input, parallelism](#3-finishing-the-foundation-an-honest-dispatcher-multivariate-input-parallelism)
4. [The next engine wave (0.3.0)](#4-the-next-engine-wave-030)
   - 4.1 [gfpop — graph-constrained functional pruning](#41-gfpop--graph-constrained-functional-pruning)
   - 4.2 [SMUCE / HSMUCE — multiscale inference with confidence](#42-smuce--hsmuce--multiscale-inference-with-confidence)
   - 4.3 [CROPS — the penalty path](#43-crops--the-penalty-path)
   - 4.4 [Robust loss, DeCAFS, self-normalisation](#44-robust-loss-decafs-self-normalisation)
5. [The remaining method backlog (0.4.0+)](#5-the-remaining-method-backlog-040)
   - 5.1 [Nonparametric and kernel](#51-nonparametric-and-kernel)
   - 5.2 [High-dimensional and multivariate](#52-high-dimensional-and-multivariate)
   - 5.3 [Bayesian](#53-bayesian)
   - 5.4 [Structural breaks in regression](#54-structural-breaks-in-regression)
   - 5.5 [Online and sequential](#55-online-and-sequential)
6. [New features beyond detection methods](#6-new-features-beyond-detection-methods)
7. [The visualization layer: what shipped and what comes next](#7-the-visualization-layer-what-shipped-and-what-comes-next)
8. [Evaluation, simulation, and benchmark data](#8-evaluation-simulation-and-benchmark-data)
9. [Architecture, dependencies, testing, and parallelism](#9-architecture-dependencies-testing-and-parallelism)
10. [Phased release plan](#10-phased-release-plan)
11. [Hardening: what 0.2.0 fixed and what remains](#11-hardening-what-020-fixed-and-what-remains)
    - 11.3 [Bugs and improvements found by testing 0.2.0 (verified)](#113-bugs-and-improvements-found-by-testing-020-verified)
12. [Backward compatibility and deprecation](#12-backward-compatibility-and-deprecation)
13. [Appendix A: Method → package → function map](#appendix-a-method--package--function-map)
14. [Appendix B: API reference (shipped and proposed)](#appendix-b-api-reference-shipped-and-proposed)
15. [References](#references)

---

## 1. Status: where 0.2.0 landed

### 1.1 What 0.2.0 delivered

The current public surface is much larger than 0.1.0's four functions. Grouped
by role:

| Area | Exported surface (0.2.0) | Notes |
|---|---|---|
| Result object | `new_ggcpt()`, `is_ggcpt()`, `print.ggcpt()` | the `ggcpt` S3 class; `cp`/`cp_value` contract preserved |
| broom methods | `tidy()`, `glance()`, `augment()` (`.ggcpt`) | one row per changepoint; one-row summary; data + `seg_id`/`.fitted`/`.resid`/`is_changepoint` |
| Unified front-end | `cpt_detect()`, `cpt_penalty()` | dispatcher + penalty constructor |
| Original wrappers | `cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`, `ggecpplot()` | retained, hardened, unchanged contract |
| New engine wrappers | `fpop_wrapper()`, `wbs_wrapper()`, `wbs2_wrapper()`, `not_wrapper()`, `mosum_wrapper()`, `idetect_wrapper()`, `tguh_wrapper()` | all return `ggcpt`; engines in `Suggests` |
| Visualization | `autoplot.ggcpt()`, `geom_changepoint()`, `geom_cpt_segment()`, `geom_cpt_ci()`, `stat_changepoint()`, `theme_ggcpt()`, `annotate_segments()` | composable layers + theming |
| Comparison | `ggcpt_compare()`, `ggcpt_compare_table()` | facet/overlay; honours `future::plan()` |
| Evaluation | `cpt_metrics()`, `cpt_metrics_annotated()`, `ggcpt_eval()` | precision/recall/F1, covering, Hausdorff, Rand, annotation error |
| Simulation / data | `cpt_simulate()` / `rcpt()`, `signal_blocks()`, `signal_fms()`, `signal_mix()`, `signal_teeth()`, `signal_stairs()` | known-truth generators + canonical signals |

The hardening promised for the 0.1.0 functions shipped too (§11): the
`ecp_wrapper()` no-change bug is fixed, `size` became `linewidth`
(`cptline_linewidth`, with `cptline_size` soft-deprecated), `match.arg()`
validation is in, the changepoint-location convention is reconciled to "left"
(last index of the left segment) with a `cp_convention` field, the plots gained
an `index` argument and a length-aware `show_points`, the `"np"` alias exists,
and a `testthat` suite plus the first snapshot tests are in place.

### 1.2 What 0.2.0 set up but did not finish

Three load-bearing things are partial, and the next release should treat them as
debt, not background:

- **The dispatcher advertises more than it can do.** `cpt_detect()`'s
  `match.arg()` enumerates 26 methods, but only 13 are wired (`pelt`, `binseg`,
  `segneigh`, `amoc`, `np`, `ecp`, `fpop`, `wbs`, `wbs2`, `not`, `mosum`,
  `idetect`, `tguh`). The other 13 — `smuce`, `hsmuce`, `kcp`, `cpm`, `robust`,
  `decafs`, `sn`, `inspect`, `sbs`, `bcp`, `bocpd`, `strucchange`, `segmented` —
  fall through to `stop("... not yet implemented ...")`, yet `cpt_detect.Rd`
  lists all 26 as if they worked. This is both a correctness wart and a
  documentation defect (§2, §3.1).
- **Multivariate detection is a false promise.** `ecp` is the package's
  multivariate engine, but `cpt_detect()` does `data_vec <- as.numeric(x)`, which
  *flattens* a matrix, and `ecp_wrapper()` returns `NA` for `cp_value` on
  matrix/data-frame input. `validate_data()` accepts matrices, so the door is
  open but the room is empty (§3.2).
- **Parallelism reaches one function.** `ggcpt_compare()` honours
  `future::plan()`, but the batch/simulation/metric-grid loops that dominate
  methodological work do not yet (§3.3, §9.4).

### 1.3 Design principles (carried forward, with one addition)

- **P1 — Wrap, don't reinvent.** Bind to peer-reviewed, CRAN-published engines;
  new engines enter as `Suggests`, guarded with `requireNamespace()`.
- **P2 — Tidy in, tidy out.** Every detector returns a `ggcpt`; column names are
  stable across methods.
- **P3 — `ggplot2` all the way down.** Every result renders with `autoplot()`
  and extends with `+`.
- **P4 — One vocabulary.** A consistent argument set (`x`, `method`, `penalty`,
  `min_size`, `n_cpts`, …); method-specific arguments flow through `...`.
- **P5 — Progressive disclosure.** Beginners call `cpt_detect()` + `autoplot()`;
  experts reach the upstream object via `$fit`.
- **P6 — No surprises for existing users.** The 0.1.0 four and now the 0.2.0
  surface keep working unchanged.
- **P7 — Document everything you ship (new).** No feature is "done" until it is
  introduced where users look first — the README and a vignette — with a runnable
  example, and until the package's own help no longer contradicts the package's
  own exports. This principle is what §2 operationalises.

---

## 2. The documentation and coverage gap (and the mandate to close it)

> **This section is the headline deliverable of the next release.** The package
> already provides far more than its public-facing documentation admits, and in a
> few places the documentation states things that are not true. Until that is
> fixed, much of 0.2.0's work is invisible.

### 2.1 The gap, concretely

The README and the package-level help advertise the package; the vignettes teach
it; the man pages document the individual functions. Today these three layers
disagree with each other and with the code. Mapping every export against where it
is actually introduced:

| Export | README | Vignette (narrative) | Man page |
|---|:---:|:---:|:---:|
| `cpt_detect`, `tidy`/`glance`/`augment`, `autoplot`, `ggcpt_compare`, `ggcpt_compare_table`, `cpt_metrics`, `cpt_simulate`, `signal_*`, `cpt_wrapper`, `ggcptplot` | ✅ | ✅ | ✅ |
| `cpt_penalty`, `cpt_metrics_annotated`, `ggcpt_eval`, `geom_changepoint`, `geom_cpt_segment`, `geom_cpt_ci`, `stat_changepoint`, `rcpt`, `ecp_wrapper`/`ggecpplot` | ❌ | ✅ | ✅ |
| `theme_ggcpt`, `annotate_segments` | ❌ | ❌ | ✅ |
| `fpop_wrapper`, `wbs_wrapper`, `wbs2_wrapper`, `not_wrapper`, `mosum_wrapper`, `idetect_wrapper`, `tguh_wrapper` | ❌ | ❌ | ✅ |
| `new_ggcpt`, `is_ggcpt` | ❌ | ❌ | ✅ |

Roughly **half of the ~37 exports never appear in the README**, and a block of
them — `theme_ggcpt()`, `annotate_segments()`, and *all seven* per-engine
wrappers — appear in **neither the README nor the vignettes**, surviving only as
man pages a user must already know to look for. A reader of the CRAN landing page
would not learn that the package can theme its plots, shade segments, or be
driven one engine at a time.

### 2.2 Statements that are actively wrong

Worse than silence is mis-statement. The next release must fix all of these:

1. **The README advertises an engine the package does not have.** `README.Rmd`
   says the package "wraps multiple detection engines (changepoint,
   changepoint.np, ecp, wbs, breakfast, not, mosum, fpop, IDetect, **and
   gfpop**)." There is **no `gfpop` code**, `gfpop` is **not in `Suggests`**, and
   `DESCRIPTION` explicitly **defers `gfpop` to v0.3.0** (`gfpop` is also not
   currently available on CRAN). The claim must be removed until §4.1 actually
   ships it.
2. **The package-level help is stale by a whole release.** `?ggchangepoint`
   still reads: *"For the moment, I only include three changepoint packages
   ('changepoint', 'changepoint.np' and 'ecp'). More changepoint packages will be
   included as time progresses."* The package now wraps eight-plus engines behind
   a unified class. This is the first help page a curious user reads, and it
   describes 0.1.0.
3. **`cpt_detect()`'s documentation over-promises.** Its `@param method`
   enumerates 26 methods, 13 of which `stop()` at runtime (§1.2). The Rd makes no
   distinction between "available now," "available with a `Suggests` package," and
   "planned." A user copying a method name out of the help gets an error.
4. **Convention and multivariate caveats are under-stated.** The `cp_convention`
   normalisation (§11) and the *current absence* of true multivariate support
   (§3.2) are not surfaced where a multivariate user would look (the `ecp_wrapper`
   and `cpt_detect` help, and the README).

### 2.3 The mandate and its acceptance criteria

The next release adopts P7 (§1.3) and treats documentation coverage as a release
gate. Concretely:

- **Every exported function is introduced in the README**, grouped by role
  (detection, result object, broom, visualization, comparison, evaluation,
  simulation, theming), each with a minimal runnable example. The README is
  regenerated from `README.Rmd` so the examples are executed, not asserted.
- **A short "feature tour" vignette** (`vignette("ggchangepoint")`) walks the
  *entire* surface end to end — including the per-engine wrappers, `theme_ggcpt()`,
  `annotate_segments()`, `cpt_penalty()`, `ggcpt_eval()`, and the class
  constructors — so no export is vignette-orphaned. The existing `introduction`
  and `comparison` vignettes stay; the tour is the index to them.
- **The package-level help is rewritten** to describe the unified framework and
  the current engine list, generated so it cannot drift (e.g. an engine table the
  help and the README share).
- **`cpt_detect()`'s help distinguishes three tiers** — wired now / available via
  a named `Suggests` package / planned — and the runtime message for an
  unimplemented method names the package or release that will provide it (§3.1).
  A new `cpt_methods()` returns this table programmatically (§3.1), and the help
  links to it.
- **A `pkgdown` reference audit** ensures every export sits in a titled group
  (the current `_pkgdown.yml` is close; it must stay exhaustive as the surface
  grows), and a CI check (e.g. a `usethis`/`pkgdown`-style "undocumented export"
  test) fails the build if any export is missing from the reference index or
  lacks `@examples`.
- **A coverage test in `testthat`** asserts that the set of exports equals the set
  of functions referenced by the README + tour vignette, so the gap cannot
  silently reopen.
- **`NEWS.md` is audited for accuracy and completeness** — the changelog is a
  documentation surface too. No entry may advertise a feature or test that does
  not exist (the 0.2.0 entry's nonexistent `vdiffr` tests; the "all methods"
  over-claim — §11.3 B13), and the 0.3.0 entry names every new export, including
  the ones 0.2.0's changelog omitted (`theme_ggcpt()`, `annotate_segments()`).

This is deliberately mechanical: the point is that "introduce all features"
becomes a checkable contract, not a good intention.

---

## 3. Finishing the foundation: an honest dispatcher, multivariate input, parallelism

### 3.1 Make `cpt_detect()` honest, and make the method set discoverable

Two acceptable fixes, used together:

- **Stop advertising what does not run.** `match.arg()` should enumerate only
  wired methods; planned methods are documented as planned, not offered as
  choices that error. As each engine lands (§4–§5) its name joins the
  enumeration.
- **Make capability introspectable.** Add `cpt_methods()` returning a tibble:

  ```r
  cpt_methods()
  #> # A tibble: N × 5
  #>   method  change_in            engine        status     installed
  #>   <chr>   <chr>                <chr>         <chr>      <lgl>
  #> 1 pelt    mean,var,meanvar     changepoint   available  TRUE
  #> 2 wbs     mean                 wbs/breakfast available  FALSE      # Suggests not installed
  #> 3 smuce   mean                 stepR         planned    NA
  #> …
  ```

  `status` ∈ {`available`, `planned`}; `installed` reflects whether the
  `Suggests` engine is present. When a wired method's engine is missing, the
  error is actionable ("install `mosum`"); when a method is `planned`, the error
  names the target release. This both fixes the over-promise and gives users (and
  `ggcpt_compare()`) a programmatic way to ask "what can I run here?"

### 3.2 Genuine multivariate input

`ecp`, and several backlog engines (§5.2), exist precisely for multivariate
data. The next release stops flattening it:

- A single input-normalisation step classifies `x` as univariate (vector) or
  multivariate (`n × p` matrix/data-frame) and routes accordingly, instead of
  `as.numeric(x)`.
- For multivariate results, `cp_value` (a scalar) is replaced/augmented by a
  populated `segments` table carrying per-segment, per-coordinate summaries; the
  `ggcpt` object already has a `segments` slot for exactly this. (Today
  `ecp_wrapper()` returns a *silently wrong* `cp_value` for matrix input — a
  column-major-flattened scalar — and `NA` for data-frame input; see §11.3 B4.)
- `autoplot()` dispatches on dimensionality to the multivariate renderers of §7.4
  (faceted small-multiples; heatmap). Univariate behaviour is unchanged.
- The `ecp_wrapper()` and `cpt_detect()` help gain a worked multivariate example,
  removing the §2.2(4) caveat by making the feature real.

### 3.3 Extend opt-in parallelism

Lift the `future` pattern already in `ggcpt_compare()` to the other
orchestration-level loops — the planned `cpt_batch()` panel detector (§6.3),
`cpt_simulate()`-driven studies, and `cpt_metrics()` over a method×penalty grid —
with parallel-safe L'Ecuyer RNG wired to a `seed` argument so results are
identical regardless of worker count (§9.4). Sequential stays the default; no
detector's *result* depends on the plan, only its runtime.

---

## 4. The next engine wave (0.3.0)

Each subsection gives the idea and key mathematics, the package to wrap, the
proposed tidy signature, and the proposed plot. These four were chosen for 0.3.0
because they add the most reach (and, for `gfpop` and SMUCE, the most genuinely
new *output types* — graph-constrained means and confidence bands) for modest
dependency weight, and because `smuce`/`hsmuce` are already named in the
dispatcher and `gfpop` is already (wrongly) named in the README.

### 4.1 gfpop — graph-constrained functional pruning

**Priority engine for 0.3.0**, both because `DESCRIPTION` deferred it here and
because the README already (incorrectly) claims it (§2.2(1)); shipping it
converts a false claim into a true one.

Generalized Functional Pruning Optimal Partitioning (Hocking, Rigaill, Fearnhead
& Bourque, 2022) lets the user encode prior structural constraints on the
sequence of segment means as a *constraint graph*: means that must alternate
up-then-down (peak detection in genomics), monotone-increasing (isotonic),
or with bounded jumps. It minimises the penalised cost subject to the graph with
empirical cost $O(N \log N)$, returning an exact constrained segmentation.

- **Wraps:** `gfpop` (`gfpop()`, `graph()`). *Availability caveat:* `gfpop` is
  not currently on CRAN; the wrapper ships behind `requireNamespace()` and is
  excluded from CRAN examples/tests until the engine is installable, so the claim
  in the README is only restored once the dependency is genuinely available.
- **Proposed:** `gfpop_wrapper(x, graph = NULL, type = c("std","updown","isotonic","relevant"), penalty = "BIC", ...)` → `ggcpt`.
- **Plot:** `autoplot()` overlays the constrained step-mean (`show_segments`);
  for `updown` graphs it shades up/down states.

### 4.2 SMUCE / HSMUCE — multiscale inference with confidence

Already named in `cpt_detect()` (`smuce`, `hsmuce`) but unwired. SMUCE (Frick,
Munk & Sieling, 2014) estimates a step function by minimising the number of
changepoints subject to a *multiscale constraint*: the estimate must pass a
multiscale test at level $\alpha$ on every interval simultaneously. The level
$\alpha$ bounds the probability of over-estimating the number of changepoints,
and the method delivers **confidence intervals for changepoint locations** and
**confidence bands for the signal**. HSMUCE (Pein, Sieling & Munk, 2017) extends
this to heterogeneous (segment-varying) noise.

- **Wraps:** `stepR` (`stepFit()`, `confband()`, `confint()`).
- **Proposed:** `smuce_wrapper(x, alpha = 0.5, family = c("gauss","hsmuce","poisson"), ...)` → `ggcpt` with `ci_lower`/`ci_upper` populated.
- **Plot:** `autoplot()` draws the step estimate, a shaded confidence band for
  the signal, and the changepoint-location CIs via the **already-shipped**
  `geom_cpt_ci()` — which today has no engine producing CIs to consume. SMUCE is
  the feature that finally exercises that geom.

### 4.3 CROPS — the penalty path

The Achilles heel of penalised methods is the penalty $\beta$; `cpt_penalty()`
constructs standard values but cannot tell the user which to pick. CROPS (Haynes,
Eckley & Fearnhead, 2017) computes *all* optimal segmentations as $\beta$ ranges
over $[\beta_{\min}, \beta_{\max}]$, exploiting that the number of distinct
optimal segmentations is small and each is found at PELT cost. This turns penalty
selection from a guess into a diagnostic.

- **Wraps:** `changepoint` (the `CROPS` penalty option).
- **Proposed:** `cpt_crops(x, method, change_in, pen_min, pen_max, ...)` → a
  `ggcpt_path` object holding, per penalty interval, the number of changepoints
  and the total unpenalised cost.
- **Plot:** `autoplot.ggcpt_path()` is the **elbow plot** (cost vs. number of
  changepoints); `ggcpt_pathplot()` is a faceted small-multiple of the actual
  segmentation at each distinct changepoint count, so the analyst *sees* the
  models being chosen among.

### 4.4 Robust loss, DeCAFS, self-normalisation

Three engines that handle the regimes where naïve change-in-mean over-counts;
all three are already named (`robust`, `decafs`, `sn`) in the dispatcher.

- **Robust loss (`robseg`).** Fearnhead & Rigaill (2019): only *bounded* loss is
  robust to arbitrary outliers. The exact DP minimises
  $\mathcal{C}(y_{a:b}) = \min_{\mu}\sum_{i=a}^{b}\gamma\!\big((y_i-\mu)/\sigma\big)$
  with $\gamma(u)=\min(u^2,K^2)$ (truncated quadratic / biweight), plus Huber and
  $L_1$ variants. `robseg_wrapper(x, loss = c("biweight","huber","L1","L2"), lambda = ..., ...)`.
  *Availability caveat:* `robseg` is not on CRAN; ships behind a guard like
  `gfpop`.
- **DeCAFS.** Romano, Rigaill, Runge & Fearnhead (2022): mean *drifts* (random
  walk) between abrupt changes and noise is autocorrelated (AR(1)). DeCAFS
  minimises
  $\sum_t\big[(y_t-\mu_t)^2/\sigma^2 + (\mu_t-\mu_{t-1})^2/\eta^2\cdot\mathbb{1}\{\text{no change}\}\big] + \beta\cdot(\#\text{changes})$.
  `decafs_wrapper(x, beta = ..., model_param = NULL, ...)` (`DeCAFS` on CRAN).
- **Self-normalisation (`SNSeg`).** Zhao, Jiang & Shao (2022): SN-based tests
  with nested local-window segmentation; nonparametric, robust to dependence,
  avoids long-run-variance estimation, detects changes in general parameters
  (mean, variance, quantiles, autocovariance, correlation) for univariate,
  multivariate, and high-dimensional series.
  `sn_wrapper(x, parameter = c("mean","variance","acf","bivcor"), ...)`.

---

## 5. The remaining method backlog (0.4.0+)

The methods below are surveyed with enough mathematics and citation to be
directly actionable, but are scheduled after the 0.3.0 wave. Each is already a
named-but-unwired method in `cpt_detect()` (§1.2), so wiring them is additive.

### 5.1 Nonparametric and kernel

- **KCP (`kcpRS`).** Arlot, Celisse & Harchaoui (2019): map data into an RKHS via
  a Gaussian kernel and minimise a within-phase scatter criterion on running
  statistics (mean, variance, autocorrelation, correlation), with a permutation
  test for existence and model selection for number. Captures *higher-order*
  changes (e.g. correlation) that mean/variance methods miss.
  `kcp_wrapper(x, running_stat = c("mean","var","ar","cor"), wsize = 25, ...)`.
- **Sequential CPM (`cpm`).** Hawkins; Ross (2015): distribution-free sequential
  detection via repeated two-sample tests — Mann–Whitney (location), Mood
  (scale), Lepage (location+scale), Kolmogorov–Smirnov and Cramér–von-Mises
  (general). Also runs in batch (`processStream`).
  `cpm_wrapper(x, cpm_type = "Mann-Whitney", arl0 = 500, startup = 20, ...)`.
- **NMCD / E-Divisive (already shipped via `cpt.np` / `ecp`).** Expose
  `nquantiles` and the CROPS path for `cpt.np`; lift `ecp` to genuine
  multivariate (§3.2) and expose `alpha`, `sig.lvl`, `R`.

### 5.2 High-dimensional and multivariate

- **inspect (`InspectChangepoint`).** Wang & Samworth (2018): for a $p$-variate
  series whose mean changes in an unknown *sparse* subset of coordinates, form the
  CUSUM matrix, find a projection direction by a sparse-SVD convex relaxation,
  project to 1-D and apply a univariate detector, recursing via WBS.
  `inspect_wrapper(X, lambda = NULL, threshold = NULL, ...)`; plot adds the sparse
  projection direction (which coordinates drive the change).
- **SBS / double-CUSUM (`hdbinseg`).** Cho & Fryzlewicz (2015); Cho (2016):
  threshold and aggregate per-coordinate CUSUMs to suppress noise coordinates
  before segmenting. `cpt_detect(method = "sbs")`.
- **The `changepoints` collection.** Xu, Padilla, Wang, Yu & Li: univariate mean
  and covariance, high-dimensional mean, high-dimensional regression (incl. under
  temporal dependence; Xu, Wang, Zhao & Yu, 2024), dynamic networks, and VAR,
  several with confidence procedures.
  `changepoints_wrapper(data, model = c("mean","cov","regression","network","var"), ...)`.

### 5.3 Bayesian

- **bcp.** Erdman & Emerson (2007), Barry & Hartigan (1993): product-partition
  model via MCMC returning a **posterior probability of a changepoint at every
  location** plus posterior segment means with credible intervals.
  `bcp_wrapper(x, ..., prob_threshold = 0.5)`; `changepoints` gains a
  `posterior_prob` column. Plot: posterior means + credible band on top,
  per-location posterior probability below.
- **BOCPD (`ocp`).** Adams & MacKay (2007): online posterior over the **run
  length** $r_t$ via
  $P(r_t, y_{1:t}) = \sum_{r_{t-1}} P(r_t\mid r_{t-1})\,P(y_t\mid r_{t-1}, y^{(r)}_t)\,P(r_{t-1}, y_{1:t-1})$,
  with a hazard $H(\cdot)$ and a conjugate predictive.
  `bocpd_wrapper(x, hazard_lambda = 250, ...)`. Plot: the run-length posterior
  heatmap — the signature BOCPD graphic.

### 5.4 Structural breaks in regression

- **Bai–Perron / fluctuation tests (`strucchange`).** Zeileis et al. (2002):
  generalized fluctuation tests for *detecting* instability and the Bai & Perron
  (1998, 2003) dynamic program (`breakpoints()`) for *dating* multiple breaks in
  $y_t = x_t^\top\beta_j + \varepsilon_t$, with CIs for the break dates.
  `strucchange_wrapper(formula, data, breaks = NULL, ...)`.
- **segmented.** Muggeo (2003, 2008): *continuous* piecewise-linear
  ("broken-line") fits with breakpoint standard errors and per-segment slopes.
  `segmented_wrapper(model, seg_var, npsi = 1, ...)`. Both render as fitted
  regime lines with break CIs rather than vertical rules (§7.3).

### 5.5 Online and sequential

- **FOCuS (`FOCuS`).** Romano, Eckley, Fearnhead & Rigaill (2023): online
  change-in-mean by functional pruning of the CUSUM likelihood — equivalent to
  running CUSUM simultaneously for all magnitudes and window sizes, at amortised
  $\log$ cost. `focus_wrapper(x, threshold = NULL, ...)`; streaming and batch.
  (Not on CRAN; guarded.)
- **OCD (`ocd`).** Chen, Wang & Samworth (2022): high-dimensional online mean
  change, storage and per-observation cost independent of history, guaranteed
  average-run-length. `ocd_wrapper(X, thresh = "MC", ...)`.

---

## 6. New features beyond detection methods

The brief asked specifically for *new* thinking about what to build. Adding more
detectors is the obvious axis; the less-obvious and arguably higher-leverage one
is making the *result object*, the *inputs*, and the *exploration loop*
first-class. These features compound across every detector the package already
has, and several directly relieve friction visible in the current code and
vignettes.

### 6.1 Complete the `ggcpt` S3 surface

The class implements `print`, `tidy`, `glance`, `augment`, `autoplot` — but not
the other generics R users reflexively reach for:

- **`summary.ggcpt()`** — a human-readable digest beyond `print`: segment table
  with levels and lengths, total cost / log-likelihood, penalty, runtime,
  convention.
- **`as_tibble.ggcpt()` / `as.data.frame.ggcpt()`** — explicit coercion to the
  changepoints (or, via an argument, the segments/augmented frame), so the object
  drops cleanly into a pipe.
- **`format.ggcpt()` + `knitr::knit_print.ggcpt()`** — clean rendering inside R
  Markdown / Quarto without forcing the user through `tidy()`.
- **`plot.ggcpt()`** — a base-graphics fallback delegating to `autoplot()` so
  `plot()` "just works" for users who type it out of habit.
- **`c()` / `rbind`-style combination** of results from several methods into the
  tidy union that `ggcpt_compare_table()` builds by hand.

### 6.2 Real time/date indices and time-series input

`autoplot()` and the plots gained an `index` argument in 0.2.0, but detection
still assumes a bare numeric vector. The next release accepts the objects R users
actually hold:

- **`cpt_detect()` methods for `ts`, `zoo`/`xts`, and `tsibble`** that carry the
  time index through to `cp`/`cp_value` and label the axis with real dates, then
  return the index on the `ggcpt` object so `autoplot()` is date-aware by default.
- A `cpt_detect.data.frame(data, y, index = NULL, ...)` form taking column names,
  matching how tidyverse users think.

### 6.3 Panel / batch detection

Methodological and applied work both routinely run *one detector over many
series* (or many coordinates). `cpt_batch()` formalises the loop the comparison
code already hints at:

```r
cpt_batch(X, method = "pelt", change_in = "mean", ..., workers = NULL, seed = 1)
#> a tibble keyed by series id, each row a ggcpt (list-column) or its tidy summary
```

It is embarrassingly parallel (§3.3, §9.4), pairs naturally with `ggcpt_facet()`
(§7.4), and is the substrate for benchmarking studies (§8).

### 6.4 Interactive and exploratory tooling

Static `ggplot`s are the core, but exploration benefits from interactivity:

- **`ggcpt_interactive()`** — render any `autoplot()` result through `ggiraph` or
  `plotly` (in `Suggests`), with changepoint locations, segment levels, and CIs
  on hover. Pure add-on; the static path is untouched.
- **`cpt_explore()`** — a `shiny`/`miniUI` gadget for the single most painful
  decision in the field: the penalty/threshold. A slider over $\beta$ (or the
  CROPS path of §4.3) updates the segmentation live, so the analyst *tunes by
  eye* and exports the chosen `ggcpt`. This turns §4.3's diagnostic into a tool.

### 6.5 Using the result: prediction, labelling, stability

- **`predict()` / `segment_id()`** — assign each observation (or a new
  observation) to its segment and fitted level; the inverse of detection, useful
  for downstream modelling and for colouring raw data by regime.
- **Stability / bootstrap diagnostics** — `cpt_stability(x, method, B = 200, ...)`
  resamples or subsamples and reports, per candidate location, how often it is
  detected; `autoplot()` renders a detection-frequency profile. This gives a
  cheap, model-agnostic confidence signal for the many engines that ship no CIs.
- **Cost / diagnostic plots** — for penalised methods, the elbow of §4.3 and a
  cost-vs-number-of-changepoints curve; a residual/`augment()` diagnostic panel.

### 6.6 Reproducibility, interop, and ergonomics

- **`cpt_cite(x)`** — return the bibliographic reference(s) for the method behind
  a result (the package already carries the references via `Rdpack`), so a user
  writing up an analysis can cite the right paper without leaving R.
- **Interop / export** — coercion of changepoints to a `tsibble` of events or a
  tidy "regimes" frame; `write_cpt()` helpers; an option to emit segment
  annotations for downstream plotting tools.
- **`cli`-quality messaging** — replace bare `stop()`/`message()` with `cli`
  (or `rlang::abort()`) errors that name the offending argument, the legal set,
  and (for a missing `Suggests` engine) the exact `install.packages()` call —
  consistent with the `cpt_methods()` introspection of §3.1.
- **Decomposition-then-detect** — a small `cpt_decompose()` helper that removes a
  trend/seasonal component (via `stats::stl`/`feasts`) before mean-change
  detection, plus documentation distinguishing a *changepoint* (a persistent
  regime shift) from a *point anomaly* (a single outlier) so users pick the right
  tool — a recurring confusion the package is well placed to clarify.

These are scoped so each is independently shippable and most are pure
`Suggests`-guarded add-ons that do not touch the core contract (P1, P6).

---

## 7. The visualization layer: what shipped and what comes next

Visualization is the package's reason to exist. 0.2.0 delivered the composable
core; the next release fills the method-aware gaps.

### 7.1 Shipped in 0.2.0

`autoplot.ggcpt()` (vertical rules by default, optional `show_segments` step
line, `index`/`show_points`/`show_line` controls), the four geoms/stats
(`geom_changepoint()`, `geom_cpt_segment()`, `geom_cpt_ci()`,
`stat_changepoint()`), `theme_ggcpt()`, `annotate_segments()`, and the
`ggcpt_compare()` facet/overlay layouts. Note that `geom_cpt_ci()` ships but has
**no engine feeding it CIs yet** — §4.2 (SMUCE) and §5 (`mosum` CIs, `segmented`,
`strucchange`) are what make it useful; until then it is an orphaned capability,
itself an instance of the §2 coverage problem.

### 7.2 Penalty-path and solution-path plots

The CROPS elbow and path plots (§4.3), plus solution-path plots for the
randomised/multiscale methods already shipped — the ordered CUSUM contrasts of
WBS/WBS2, the narrowest-over-threshold features of NOT, the MOSUM detector curve
with its threshold and bandwidth band, the Isolate-Detect path — each showing
*why* a changepoint was selected, not just where. These engines are already
wrapped, so this is rendering work on existing `$fit` objects.

### 7.3 Trend-change rendering

For slope-change methods (`not` with linear contrasts — already wrapped — and the
backlog `segmented`, `gfpop` isotonic), `autoplot()` draws the **fitted broken
line** with breakpoints marked (and CIs from `segmented`) instead of vertical
rules — the correct idiom for trend changes, and a capability the wrapped `not`
engine can already supply but the renderer does not yet use.

### 7.4 Multivariate and high-dimensional plots

Enabled by §3.2: `ggcpt_facet(X, ...)` (one panel per coordinate, shared
changepoint rules) and `ggcpt_heatmap(X, ...)` (tiles of the data or
per-coordinate CUSUM contributions, with a side panel showing the `inspect`
projection direction). Multivariate `ecp` and `changepoints` results plug
straight in.

### 7.5 Bayesian posterior plots

For §5.3: the bcp two-panel posterior-mean-plus-probability plot and the BOCPD
run-length posterior heatmap.

---

## 8. Evaluation, simulation, and benchmark data

### 8.1 Shipped

`cpt_metrics()` (precision/recall/F1 with margin, covering, Hausdorff, (adjusted)
Rand, annotation error, matched MAE/RMSE), `cpt_metrics_annotated()` for
multi-annotator data, `ggcpt_eval()` for the visual TP/FP/miss overlay,
`cpt_simulate()`/`rcpt()` with known-truth attributes, and the canonical signals
`signal_blocks/fms/mix/teeth/stairs`.

### 8.2 Next

- **Benchmark study harness** — a thin layer over `cpt_batch()` (§6.3) and
  `cpt_metrics()` running a method×penalty×replication grid in parallel and
  returning a tidy results frame, the single largest time sink in methodological
  work.
- **TCPD-style multi-annotator benchmarking** — loaders/recipes for the Turing
  Change Point Dataset conventions (van den Burg & Williams, 2020), scoring with
  the averaged covering/F1 already implemented.
- **Real datasets** — documented loaders for well-log, coal-mining disasters,
  Nile, aCGH, and server/HASC-style streams, with heavy data in `Suggests` or
  external packages to respect CRAN size limits.
- **A simulator `slope` mode and multivariate sparse-mean mode**, closing the gap
  between what `cpt_simulate()` documents and what the high-dimensional and
  trend-change detectors need to be tested against.

---

## 9. Architecture, dependencies, testing, and parallelism

### 9.1 Dependency strategy

Only the original engines stay in `Imports` (`changepoint`, `changepoint.np`,
`ecp`, plus the tidyverse/`broom`/`ggplot2` infrastructure). Every newly wrapped
engine goes in `Suggests`, guarded with `requireNamespace()` and an actionable
message (now routed through the §3.1 `cpt_methods()`/`cli` machinery). Engines not
on CRAN (`gfpop`, `robseg`, `FOCuS`) ship behind an install note and are excluded
from CRAN examples/tests — and, per §2.2, are **not advertised as available**
until installable.

### 9.2 File layout

The existing layout (`ggcpt-class.R`, `broom-methods.R`, `autoplot.R`, `geoms.R`,
`detect.R`, `compare.R`, `metrics.R`, `simulate.R`, `wrap-optpart.R`,
`wrap-search.R`, plus `changepoint.R`/`ecp.R`) extends naturally:
`wrap-multiscale.R` (stepR), `wrap-robust.R` (robseg/DeCAFS/SN),
`wrap-highdim.R`, `wrap-bayes.R`, `wrap-regression.R`, `wrap-online.R`,
`crops.R`, and `batch.R`/`explore.R`/`interop.R` for §6.

### 9.3 Testing and documentation

- `testthat` per wrapper (`skip_if_not_installed()`), asserting the tidy contract
  and `tidy()`/`augment()` round-trips; `vdiffr` snapshots for geoms and
  `autoplot()`.
- The **documentation-coverage tests of §2.3** (exports ⊆ README+tour;
  every export in the `pkgdown` index; every export has `@examples`).
- The feature-tour vignette (§2.3) and updates to `introduction`/`comparison`.

### 9.4 Parallelism and reproducible RNG

Standardise on the `future` ecosystem (`future.apply`/`furrr`, `progressr` for
progress), already used in `ggcpt_compare()`. Parallelise the *outer* loops —
methods, series, coordinates, replications — never the inner DP recurrence or the
online stream updates, which are sequential by construction. Because many engines
are stochastic, use parallel-safe **L'Ecuyer-CMRG** streams (`future.seed`/
`furrr_options(seed = TRUE)`) wired to each wrapper's `seed`, so a study is
reproducible whether it runs on 1 core or 64; this is tested (same `seed` ⇒
identical changepoints under `sequential` and `multisession`). Backends live in
`Suggests`; the sequential default adds no hard dependency.

---

## 10. Phased release plan

### v0.2.0 — Foundations + first engines — **DELIVERED (CRAN)**
The `ggcpt` object, broom methods, `autoplot()`, the geoms, `cpt_detect()`,
`cpt_penalty()`, the WBS/WBS2/NOT/MOSUM/FPOP/IDetect/TGUH wrappers,
`ggcpt_compare()` (+ `future`), the evaluation module, the simulator and signals,
and the 0.1.0 hardening.

### v0.3.0 — Documentation parity, an honest dispatcher, inference & robustness — **NEXT**
- **§2: full documentation coverage** — README introduces every export; feature-
  tour vignette; rewritten package-level help; tiered `cpt_detect()` docs +
  `cpt_methods()`; coverage tests; **remove the gfpop over-claim** (restored only
  when §4.1 ships).
- **§3: finish the foundation** — honest `match.arg()`, genuine multivariate
  input, parallelism beyond `ggcpt_compare()`.
- **§4: next engine wave** — `gfpop` (when installable), SMUCE/HSMUCE (`stepR`,
  wiring the orphaned `geom_cpt_ci()`), CROPS penalty path + plots, robust
  (`robseg`), DeCAFS, self-normalisation (`SNSeg`).
- **§6 (first slice)** — `summary`/`as_tibble`/`format`/`knit_print`/`plot`
  methods; `ts`/`xts`/`tsibble`/`data.frame` input; `cpt_cite()`.

### v0.4.0 — Bayesian, high-dimensional, regression, exploration
- bcp + BOCPD (`ocp`) with posterior plots; `inspect`, SBS (`hdbinseg`), the
  `changepoints` collection + multivariate/HD plots; `strucchange` + `segmented`
  with regression rendering; `cpt_batch()`, `ggcpt_interactive()`,
  `cpt_explore()`, stability diagnostics.

### v0.5.0 / v1.0.0 — Online + consolidation
- CPM streaming, FOCuS, OCD + streaming plots; TCPD multi-annotator benchmarking
  and a benchmarking vignette; API freeze, full `pkgdown` site, JOSS/R-Journal
  software paper.

---

## 11. Hardening: what 0.2.0 fixed and what remains

### 11.1 Fixed in 0.2.0

The 0.1.0 audit items are closed: the `ecp_wrapper()` no-change bug (now returns
0 rows; boundaries stripped unconditionally), the `size`→`linewidth` deprecation
(`cptline_linewidth`, with `cptline_size` soft-deprecated via `lifecycle`),
`match.arg()` input validation across wrappers, the changepoint-location
convention (normalised to "left", exposed as `cp_convention`), full-height rules
with a `show_points` length cutoff and an `index` argument, the `"np"` alias, the
"sytle"→"style" typo, the `"_PACKAGE"` sentinel, and the first `testthat`
tests (24 `test_that` blocks across three files). `RoxygenNote` is current
(7.3.2). Note: the `vdiffr` snapshot tests advertised in `NEWS.md` were **not**
actually added — see §11.3 B13.

### 11.2 Remaining and newly surfaced

- **Documentation drift (now the §2 mandate).** The package-level help is stale
  (claims three engines); the README over-claims `gfpop` and under-documents ~half
  the surface; `cpt_detect()` documents methods that error. These are the
  highest-priority "bugs" of the next cycle — wrong/missing docs are as harmful as
  wrong code.
- **Dispatcher over-promise.** `cpt_detect()` offers 13 methods that `stop()`
  (§1.2, §3.1).
- **Multivariate `ecp` still degraded.** `cpt_detect()` flattens a matrix with
  `as.numeric(x)`, and `ecp_wrapper()` returns a wrong column-major scalar
  `cp_value` for matrix input (and `NA` for a data frame). The validator accepts
  matrices, advertising support the detectors do not honour (§3.2, §11.3 B4).
- **`geom_cpt_ci()` has no producer.** It ships but no shipped engine emits
  `ci_lower`/`ci_upper`; SMUCE/`mosum`-CIs (§4.2, §5) are what make it real.
- **Penalty handling is uneven across engines.** `cpt_penalty()` and the string
  penalties map cleanly onto the `changepoint` family but only partially onto the
  newer wrappers; the CROPS work (§4.3) and a documented penalty-semantics table
  per engine should make this consistent and discoverable.

Each remaining item is either a doc fix (no compatibility risk) or ships behind
an option/`Suggests` guard, consistent with §12.

### 11.3 Bugs and improvements found by testing 0.2.0 (verified)

The following were **reproduced by running 0.2.0** on R 4.4.1 with the declared
dependencies and all `Suggests` engines installed; the existing `testthat` suite
passes, so none of these is currently caught. They are ordered by severity. Each
is a concrete fix for the next release.

#### Correctness — silently wrong results

**B1. `signal_blocks()` does not produce the Blocks signal; its data contradicts
its own `true_changepoints`.** In `simulate.R` the level loop runs *in reverse*
and assigns *absolute* levels with a `>=` mask:

```r
for (i in rev(seq_along(cp_idx))) signal[t >= cp_scaled[i]] <- heights[i]
```

Because the iteration is reversed, the final assignment (`i = 1`,
`t >= 0.10`) overwrites the entire tail, collapsing all 11 blocks into a **single
step** at the first changepoint. Reproduction: of the 11 declared
`true_changepoints`, only **1** shows a real jump in the generated series; the
other ten sit in flat noise. This is the worst kind of bug for a benchmark
package — a "canonical test signal" whose ground truth is wrong — so every
example, test, or evaluation built on `signal_blocks()` is scored against
fiction. *Fix:* iterate forward (`seq_along`), or build cumulative jumps; add a
test asserting each declared changepoint is a genuine level change. (The other
signals — `fms`, `mix`, `teeth`, `stairs` — were checked and are correct.)

**B2. `cpt_metrics()` double-counts true positives; `recall` and `f1` can exceed
1.** The precision/recall loop matches every *prediction* to its nearest truth
independently and increments `tp` each time, so several predictions near one true
changepoint each count as a hit. Reproduction:

```r
cpt_metrics(pred = c(98, 100, 102), truth = c(100), n = 200, margin = 5)
#> precision = 1, recall = 3, f1 = 1.5      # recall and f1 are impossible
```

`recall > 1`, `f1 > 1`, and the implied `fn = length(truth) - tp` goes negative.
*Fix:* one-to-one matching — each truth may be claimed by at most one prediction
(e.g. greedy nearest matching with removal, or a bipartite match) — then
`tp ≤ min(|pred|, |truth|)`. The covering and Rand metrics are unaffected
(partition-based) and stayed in `[0, 1]` in testing.

**B3. `cpt_detect()` silently changes or ignores `change_in`.** Two faces of one
problem, both verified:
- `change_in = "slope"` is accepted by `match.arg()` but `change_in_mapping()`
  maps it to `"mean"`; on a pure linear trend the call runs mean-change detection
  (returning spurious changepoints) and the resulting object reports
  `change_in = "mean"`. The user's request is silently discarded.
- For the search/pruning engines (`fpop`, `wbs`, `wbs2`, `not`, `mosum`,
  `idetect`, `tguh`) `change_in` is not passed through at all, and the object is
  hard-coded to `change_in = "mean"`. `cpt_detect(x, method = "fpop",
  change_in = "var")` reports `mean` and does mean detection.

*Fix:* validate the `method × change_in` combination and **error** (or route to a
capable engine — `not` does support `slope`/`var` contrasts) instead of silently
mislabelling; record the *actual* `change_in` on the object.

**B4. `ecp_wrapper()` returns a wrong `cp_value` for multivariate input, and
`cpt_detect()` flattens it.** Because `is.numeric()` is `TRUE` for a numeric
matrix, the `cp_value <- data[cp]` branch fires and returns a **column-major
flattened scalar** (e.g. `X[101]`), not the per-coordinate change; a `data.frame`
falls to the `else` branch and returns `NA`. Separately, `cpt_detect(X, method =
"ecp")` runs `as.numeric(X)`, so a 200×2 matrix is seen as a length-400 vector.
`ecp` is the package's multivariate engine, so this is its primary use case.
*Fix:* §3.2 (route multivariate input without flattening; populate `segments`
instead of a scalar `cp_value`).

**B5. `stat_changepoint()` ignores the `x` aesthetic and draws lines at raw
indices.** `StatChangepoint$compute_group()` returns `xintercept = cp$cp`
(integer positions) regardless of the panel's `x` scale. On any axis that is not
`1:n` the rules land in the wrong place. Reproduction: with `aes(t, y)` where
`t` spans 2000–2019, a changepoint at observation 100 is drawn at `xintercept =
100` — off the visible axis — and ggplot also warns that the `x`/`y` aesthetics
were dropped. *Fix:* map the detected index back to the `x` aesthetic
(`xintercept = data$x[cp$cp]`) and preserve grouping.

#### Robustness — noisy or fragile behaviour

**B6. `min(numeric(0))` warnings on empty `pred`/`truth`.** `cpt_metrics()` and
`ggcpt_eval()` call `min(abs(p - truth))` inside a loop; when `truth` (or `pred`)
is empty this emits *"no non-missing arguments to min; returning Inf"* for every
element. The numeric result is defensible (no matches) but the warning spam is
not. A no-change series scored against an empty truth set is a normal case. *Fix:*
guard the empty cases explicitly before the loop.

**B7. The dispatcher advertises 13 methods that error at runtime** (`smuce`,
`hsmuce`, `kcp`, `cpm`, `robust`, `decafs`, `sn`, `inspect`, `sbs`, `bcp`,
`bocpd`, `strucchange`, `segmented`) — cross-referenced as the §3.1 fix and the
§2.2(3) documentation defect; listed here for completeness as a verified runtime
behaviour (`cpt_detect(x, method = "smuce")` → `stop("... not yet implemented")`).

#### Improvements and documentation mismatches

**B8. `glance.ggcpt()` always returns `total_cost = NA` and `runtime = NA`.** Both
are hard-coded; the help says `total_cost` is filled "if available," but it never
is, and runtime is never measured. The upstream `changepoint`/`fpop` fits expose a
cost/likelihood. *Fix:* populate `total_cost` from `$fit` where available and time
the detector call.

**B9. `cpt_simulate()` documents return columns `index` and `value`, but returns
`index`, `value`, `seg_id`.** A small `@return` mismatch (verified); reconcile the
doc with the (useful) `seg_id` column.

**B10. `ggcptplot_internal(show_segments = )` is a dead parameter.** It is accepted
but never used; segment drawing actually happens in `autoplot.ggcpt()`. Harmless
today, but a trap for future edits. *Fix:* remove the unused parameter or move the
segment logic into the shared internal.

**B11. `augment.ggcpt()` hard-codes `colnames(data) <- c("index", "value")`.** It
assumes exactly two data columns, so it will break or mislabel once multivariate
data (§3.2) carries more. *Fix:* rename by position-independent logic before
multivariate support lands.

**B12. Penalty semantics differ across engines and are undocumented.**
`cpt_penalty("MBIC", n)` returns `2*log(n)` (for `k = 1`), which is **not** the
`changepoint` package's MBIC; a numeric penalty means different things to `fpop`
(`lambda`) than the string `"MBIC"` does to PELT. Results can therefore differ
across methods for "the same" penalty. *Fix:* the CROPS work (§4.3) plus a
documented per-engine penalty-semantics table (also §11.2).

**B13. `NEWS.md` over-claims and omits — the changelog is itself part of the
documentation gap (§2).** Verified against the repo: (a) the 0.2.0 entry
advertises *"vdiffr snapshot tests (where available)"* but **none exist** — zero
`expect_doppelganger`/`expect_snapshot` calls and no `tests/testthat/_snaps/`
directory (the same inaccuracy previously inherited in §11.1, now corrected);
(b) it calls `cpt_detect()` a *"dispatcher for all changepoint methods,"* echoing
the §2.2(3) over-promise (13 of 26 methods error); (c) it never mentions the
exported, user-facing `theme_ggcpt()` and `annotate_segments()`; and (d) the
0.1.0 entry is a bare *"Initial release to CRAN"* that does not record what 0.1.0
contained (`cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`, `ggecpplot()`). The
shipped 0.2.0 CRAN changelog is frozen, so the fix is *forward*: make the 0.3.0
`NEWS.md` accurate and complete, fold `NEWS.md` into the §2 coverage mandate, and
only print the *"vdiffr"* claim once those snapshot tests actually exist (§9.3).

---

## 12. Backward compatibility and deprecation

- The full 0.2.0 surface keeps working unchanged: signatures, defaults, return
  types, and the `tibble(cp, cp_value)` contract are covered by regression and
  `vdiffr` tests. The 0.1.0 four (`cpt_wrapper()`, `ecp_wrapper()`,
  `ggcptplot()`, `ggecpplot()`) remain exported.
- New behaviour is strictly additive. Tightening `cpt_detect()`'s `match.arg()`
  to wired methods (§3.1) only removes choices that already error, so no working
  call breaks; the removed names reappear as they are implemented.
- Removing `gfpop` from the README (§2.2) corrects an over-claim — it never
  worked — and is therefore compatible; the claim returns when §4.1 ships.
- No exported function is deprecated. Should a wrapper later be subsumed by
  `cpt_detect()`, it goes through the standard `lifecycle` soft-deprecation path
  with at least one minor-version notice.

---

## Appendix A: Method → package → function map

Status as of 0.2.0: ✅ shipped · 🚧 harden/finish · 🆕 planned.

| Method | Reference | Package | Access | Status |
|---|---|---|---|---|
| Optimal Partitioning / SegNeigh / PELT / BinSeg / AMOC | Jackson 2005; Auger–Lawrence 1989; Killick 2012; Scott–Knott 1974 | `changepoint` | `cpt_wrapper()`, `cpt_detect()` | ✅ |
| NMCD (`cpt.np`) | Zou 2014; Haynes 2017 | `changepoint.np` | `cpt_detect("np")` | ✅ |
| E-Divisive / E-Agglo | Matteson–James 2014 | `ecp` | `ecp_wrapper()`, `cpt_detect("ecp")` | ✅ / 🚧 multivariate |
| FPOP | Maidstone 2017 | `fpop` | `fpop_wrapper()` | ✅ |
| WBS / WBS2 / NOT / MOSUM / IDetect / TGUH | Fryzlewicz 2014/2020; Baranowski 2019; Eichinger–Kirch 2018; Anastasiou–Fryzlewicz 2022; Fryzlewicz 2018 | `wbs`/`breakfast`/`not`/`mosum`/`IDetect` | `*_wrapper()`, `cpt_detect()` | ✅ |
| gfpop | Hocking 2022 | `gfpop` | `gfpop_wrapper()` | 🆕 (README over-claims) |
| SMUCE / HSMUCE | Frick 2014; Pein 2017 | `stepR` | `smuce_wrapper()` | 🆕 (named, unwired) |
| CROPS | Haynes 2017 | `changepoint` | `cpt_crops()` | 🆕 |
| Robust loss / DeCAFS / SN | Fearnhead–Rigaill 2019; Romano 2022; Zhao 2022 | `robseg`/`DeCAFS`/`SNSeg` | `*_wrapper()` | 🆕 (named, unwired) |
| KCP / CPM | Arlot 2019; Ross 2015 | `kcpRS`/`cpm` | `kcp_wrapper()`/`cpm_wrapper()` | 🆕 |
| inspect / SBS / changepoints | Wang–Samworth 2018; Cho–Fryzlewicz 2015; Xu et al. | `InspectChangepoint`/`hdbinseg`/`changepoints` | `*_wrapper()` | 🆕 |
| bcp / BOCPD | Erdman–Emerson 2007; Adams–MacKay 2007 | `bcp`/`ocp` | `bcp_wrapper()`/`bocpd_wrapper()` | 🆕 |
| Bai–Perron / segmented | Bai–Perron 1998/2003; Muggeo 2003/2008 | `strucchange`/`segmented` | `*_wrapper()` | 🆕 |
| FOCuS / OCD | Romano 2023; Chen 2022 | `FOCuS`/`ocd` | `*_wrapper()` | 🆕 |

## Appendix B: API reference (shipped and proposed)

```r
## Shipped in 0.2.0 -------------------------------------------------------
## core
cpt_detect(x, method, change_in, penalty, ...) -> ggcpt
cpt_penalty(type, n, k, value)                 -> numeric
new_ggcpt(...); is_ggcpt(x); print(<ggcpt>)
## broom + ggplot2
tidy(x); glance(x); augment(x)                 # x: ggcpt
autoplot(x, show_segments, show_points, show_line, index, ...) -> ggplot
ggcptplot(...); ggecpplot(...)                 # original + hardened
geom_changepoint(); geom_cpt_segment(); geom_cpt_ci(); stat_changepoint()
theme_ggcpt(); annotate_segments()
## wrappers
cpt_wrapper(); ecp_wrapper()
fpop_wrapper(); wbs_wrapper(); wbs2_wrapper(); not_wrapper()
mosum_wrapper(); idetect_wrapper(); tguh_wrapper()
## comparison / evaluation / simulation
ggcpt_compare(x, methods, layout, seed, ...) -> ggplot
ggcpt_compare_table(x, methods, ...)         -> tibble
cpt_metrics(); cpt_metrics_annotated(); ggcpt_eval()
cpt_simulate() / rcpt(); signal_blocks/fms/mix/teeth/stairs()

## Proposed for 0.3.0+ ----------------------------------------------------
cpt_methods()                                  -> tibble (introspection, §3.1)
cpt_detect(<ts|xts|tsibble|data.frame>, ...)   # §6.2
gfpop_wrapper(); smuce_wrapper(); robseg_wrapper(); decafs_wrapper(); sn_wrapper()
cpt_crops(x, method, change_in, pen_min, pen_max, ...) -> ggcpt_path  # §4.3
ggcpt_pathplot(path, ...) -> ggplot
summary(<ggcpt>); as_tibble(<ggcpt>); format(<ggcpt>); plot(<ggcpt>)  # §6.1
cpt_batch(X, method, ..., workers, seed) -> tibble                    # §6.3
ggcpt_interactive(p, ...); cpt_explore(x, ...)                        # §6.4
predict(<ggcpt>); segment_id(<ggcpt>); cpt_stability(x, method, B)   # §6.5
cpt_cite(x); cpt_decompose(x, ...)                                    # §6.6
ggcpt_facet(X, ...); ggcpt_heatmap(X, ...)                           # §7.4
## later: kcp/cpm/inspect/sbs/changepoints/bcp/bocpd/strucchange/segmented/focus/ocd
```

---

## References

The following references are the basis for the shipped and proposed features.
Venues and years have been verified against the published record; arXiv
identifiers are given where useful.

1. Adams, R. P. and MacKay, D. J. C. (2007). *Bayesian Online Changepoint
   Detection.* arXiv:0710.3742.
2. Anastasiou, A. and Fryzlewicz, P. (2022). *Detecting multiple generalized
   change-points by isolating single ones.* **Metrika** 85, 141–174.
   (R packages `IDetect`, `breakfast`.) arXiv:1901.10852.
3. Arlot, S., Celisse, A. and Harchaoui, Z. (2019). *A kernel multiple
   change-point algorithm via model selection.* **Journal of Machine Learning
   Research** 20(162), 1–56.
4. Auger, I. E. and Lawrence, C. E. (1989). *Algorithms for the optimal
   identification of segment neighborhoods.* **Bulletin of Mathematical Biology**
   51(1), 39–54.
5. Bai, J. and Perron, P. (1998). *Estimating and testing linear models with
   multiple structural changes.* **Econometrica** 66(1), 47–78.
6. Bai, J. and Perron, P. (2003). *Computation and analysis of multiple
   structural change models.* **Journal of Applied Econometrics** 18(1), 1–22.
7. Baranowski, R., Chen, Y. and Fryzlewicz, P. (2019). *Narrowest-over-threshold
   detection of multiple change points and change-point-like features.*
   **Journal of the Royal Statistical Society: Series B** 81(3), 649–672.
   (R package `not`.)
8. Barry, D. and Hartigan, J. A. (1993). *A Bayesian analysis for change point
   problems.* **Journal of the American Statistical Association** 88(421),
   309–319.
9. Cho, H. and Fryzlewicz, P. (2015). *Multiple-change-point detection for high
   dimensional time series via sparsified binary segmentation.* **Journal of the
   Royal Statistical Society: Series B** 77(2), 475–507. (R package `hdbinseg`.)
10. Cho, H. (2016). *Change-point detection in panel data via double CUSUM
    statistic.* **Electronic Journal of Statistics** 10(2), 2000–2038.
11. Cho, H. and Kirch, C. (2022). *Two-stage data segmentation permitting
    multiscale change points, heavy tails and dependence.* **Annals of the
    Institute of Statistical Mathematics** 74, 653–684. Companion bootstrap
    location CIs: Cho, H. and Kirch, C. (2022), *Bootstrap confidence intervals
    for multiple change points based on moving sum procedures*, **Computational
    Statistics & Data Analysis** 175, 107552.
12. Chen, Y., Wang, T. and Samworth, R. J. (2022). *High-dimensional, multiscale
    online changepoint detection.* **Journal of the Royal Statistical Society:
    Series B** 84(1), 234–266. (R package `ocd`.) arXiv:2003.03668.
13. Davis, R. A., Lee, T. C. M. and Rodriguez-Yam, G. A. (2006). *Structural
    break estimation for nonstationary time series models (Auto-PARM).*
    **Journal of the American Statistical Association** 101(473), 223–239.
14. Eichinger, B. and Kirch, C. (2018). *A MOSUM procedure for the estimation of
    multiple random change points.* **Bernoulli** 24(1), 526–564.
15. Erdman, C. and Emerson, J. W. (2007). *bcp: An R package for performing a
    Bayesian analysis of change point problems.* **Journal of Statistical
    Software** 23(3), 1–13.
16. Fearnhead, P. (2006). *Exact and efficient Bayesian inference for multiple
    changepoint problems.* **Statistics and Computing** 16, 203–213.
17. Fearnhead, P. and Rigaill, G. (2019). *Changepoint detection in the presence
    of outliers.* **Journal of the American Statistical Association** 114(525),
    169–183. (R package `robseg`.) arXiv:1609.07363.
18. Frick, K., Munk, A. and Sieling, H. (2014). *Multiscale change point
    inference (SMUCE).* **Journal of the Royal Statistical Society: Series B**
    76(3), 495–580. (R package `stepR`.) arXiv:1301.7212.
19. Fryzlewicz, P. (2014). *Wild binary segmentation for multiple change-point
    detection.* **Annals of Statistics** 42(6), 2243–2281. (R packages `wbs`,
    `breakfast`.) arXiv:1411.0858.
20. Fryzlewicz, P. (2018). *Tail-greedy bottom-up data decompositions and fast
    multiple change-point detection (TGUH).* **Annals of Statistics** 46(6B),
    3390–3421. (R package `breakfast`.)
21. Fryzlewicz, P. (2020). *Detecting possibly frequent change-points: Wild
    Binary Segmentation 2 and steepest-drop model selection.* **Journal of the
    Korean Statistical Society** 49, 1027–1070. (R package `breakfast`.)
    arXiv:1812.06880.
22. Hannan, E. J. and Quinn, B. G. (1979). *The determination of the order of an
    autoregression.* **Journal of the Royal Statistical Society: Series B**
    41(2), 190–195.
23. Haynes, K., Eckley, I. A. and Fearnhead, P. (2017). *Computationally
    efficient changepoint detection for a range of penalties (CROPS).* **Journal
    of Computational and Graphical Statistics** 26(1), 134–143.
24. Haynes, K., Fearnhead, P. and Eckley, I. A. (2017). *A computationally
    efficient nonparametric approach for changepoint detection.* **Statistics and
    Computing** 27(5), 1293–1305. (R package `changepoint.np`.) arXiv:1602.01254.
25. Hocking, T. D., Rigaill, G., Fearnhead, P. and Bourque, G. (2022).
    *Generalized functional pruning optimal partitioning (GFPOP) for constrained
    changepoint detection in genomic data.* **Journal of Statistical Software**
    101(10). (R package `gfpop`.) arXiv:1810.00117.
26. Jackson, B., Scargle, J. D., et al. (2005). *An algorithm for optimal
    partitioning of data on an interval.* **IEEE Signal Processing Letters**
    12(2), 105–108.
27. James, N. A. and Matteson, D. S. (2015). *ecp: An R package for
    nonparametric multiple change point analysis of multivariate data.*
    **Journal of Statistical Software** 62(7), 1–25. arXiv:1309.3295.
28. Killick, R., Fearnhead, P. and Eckley, I. A. (2012). *Optimal detection of
    changepoints with a linear computational cost (PELT).* **Journal of the
    American Statistical Association** 107(500), 1590–1598. arXiv:1101.1438.
29. Killick, R. and Eckley, I. A. (2014). *changepoint: An R package for
    changepoint analysis.* **Journal of Statistical Software** 58(3), 1–19.
30. Maidstone, R., Hocking, T., Rigaill, G. and Fearnhead, P. (2017). *On optimal
    multiple changepoint algorithms for large data (FPOP/SNIP).* **Statistics and
    Computing** 27(2), 519–533. (R package `fpop`.)
31. Matteson, D. S. and James, N. A. (2014). *A nonparametric approach for
    multiple change point analysis of multivariate data.* **Journal of the
    American Statistical Association** 109(505), 334–345. (R package `ecp`.)
    arXiv:1306.4933.
32. Meier, A., Kirch, C. and Cho, H. (2021). *mosum: A package for moving sums in
    change-point analysis.* **Journal of Statistical Software** 97(8). (R package
    `mosum`.)
33. Muggeo, V. M. R. (2003). *Estimating regression models with unknown
    break-points.* **Statistics in Medicine** 22(19), 3055–3071.
34. Muggeo, V. M. R. (2008). *segmented: An R package to fit regression models
    with broken-line relationships.* **R News** 8(1), 20–25.
35. Pein, F., Sieling, H. and Munk, A. (2017). *Heterogeneous change point
    inference (HSMUCE).* **Journal of the Royal Statistical Society: Series B**
    79(4), 1207–1227. (R package `stepR`.)
36. Page, E. S. (1954). *Continuous inspection schemes.* **Biometrika** 41(1/2),
    100–115.
37. Romano, G., Rigaill, G., Runge, V. and Fearnhead, P. (2022). *Detecting
    abrupt changes in the presence of local fluctuations and autocorrelated noise
    (DeCAFS).* **Journal of the American Statistical Association** 117(540),
    2147–2162. (R package `DeCAFS`.) arXiv:2005.01379.
38. Romano, G., Eckley, I. A., Fearnhead, P. and Rigaill, G. (2023). *Fast online
    changepoint detection via functional pruning CUSUM statistics (FOCuS).*
    **Journal of Machine Learning Research** 24, 1–36. arXiv:2110.08205.
39. Ross, G. J. (2015). *Parametric and nonparametric sequential change detection
    in R: The cpm package.* **Journal of Statistical Software** 66(3), 1–20.
40. Schwarz, G. (1978). *Estimating the dimension of a model (BIC).* **Annals of
    Statistics** 6(2), 461–464.
41. Scott, A. J. and Knott, M. (1974). *A cluster analysis method for grouping
    means in the analysis of variance (Binary Segmentation).* **Biometrics**
    30(3), 507–512.
42. van den Burg, G. J. J. and Williams, C. K. I. (2020). *An evaluation of
    change point detection algorithms.* arXiv:2003.06222. (Turing Change Point
    Dataset / benchmark.)
43. Wang, T. and Samworth, R. J. (2018). *High dimensional change point
    estimation via sparse projection (inspect).* **Journal of the Royal
    Statistical Society: Series B** 80(1), 57–83. (R package
    `InspectChangepoint`.) arXiv:1606.06246.
44. Xu, H., Wang, D., Zhao, Z. and Yu, Y. (2024). *Change-point inference in
    high-dimensional regression models under temporal dependence.* **Annals of
    Statistics** 52(3). (R package `changepoints`.) arXiv:2207.12453.
45. Yao, Y.-C. (1988). *Estimating the number of change-points via Schwarz's
    criterion.* **Statistics & Probability Letters** 6(3), 181–189.
46. Zeileis, A., Leisch, F., Hornik, K. and Kleiber, C. (2002). *strucchange: An
    R package for testing for structural change in linear regression models.*
    **Journal of Statistical Software** 7(2), 1–38.
47. Zhang, N. R. and Siegmund, D. O. (2007). *A modified Bayes information
    criterion with applications to the analysis of comparative genomic
    hybridization data (mBIC).* **Biometrics** 63(1), 22–32.
48. Zhao, Z., Jiang, F. and Shao, X. (2022). *Segmenting time series via
    self-normalisation.* **Journal of the Royal Statistical Society: Series B**
    84(5), 1699–1725. (R package `SNSeg`.)
49. Zou, C., Yin, G., Feng, L. and Wang, Z. (2014). *Nonparametric maximum
    likelihood approach to multiple change-point problems (NMCD).* **Annals of
    Statistics** 42(3), 970–1002. arXiv:1405.7173.

### Secondary references (surveys and benchmarks)

50. Aminikhanghahi, S. and Cook, D. J. (2017). *A survey of methods for time
    series change point detection.* **Knowledge and Information Systems** 51(2),
    339–367.
51. Niu, Y. S., Hao, N. and Zhang, H. (2016). *Multiple change-point detection: A
    selective overview.* **Statistical Science** 31(4), 611–623.
52. Truong, C., Oudre, L. and Vayatis, N. (2020). *Selective review of offline
    change point detection methods.* **Signal Processing** 167, 107299.
    (Python `ruptures`; useful for cross-language API comparison.)

---

*This document is a living roadmap; signatures and milestones will be refined as
implementation proceeds. Contributions and method suggestions are welcome via the
issue tracker.*
