# ggchangepoint: Release Ledger and Development Roadmap

### Expanding tidy, visualization-first changepoint detection in R

**Author:** Youzhi Yu
**Status:** Release ledger for `ggchangepoint` 0.5.0 (Part 0) and 0.4.0
(Part I), and the residual design roadmap (Part II, now annotated with what
0.5.0 delivered)
**Current release:** 0.4.0 on CRAN; **0.5.0 complete and ready to submit**

---

## Abstract

**0.5.0 is the release that answers Part II.** 0.4.0 closed the "not enough
methods" gap; 0.5.0 closes the three that were left. *Inferential:*
Narrowest Significance Pursuit brings significance **regions** — intervals
each guaranteed to contain a change at a global level — and with them a new
`regions` slot, a new `geom_cpt_region()` layer, a `cpt_confint()` generic
that unifies four provenances behind one contract, and a `cpt_test()` that
refuses to let a selection-unadjusted p-value pass for an adjusted one.
*Grammatical:* the detector's own statistic, its solution path and its scale
space are now first-class objects with accessors and plots, and
`autoplot(type =)` reaches all three. *Boundary-shaped:* time indices
(`ts`/`xts`/`zoo`/`tsibble` and a data-frame interface), streaming monitors
with detection-delay accounting, functional data, dynamic networks,
covariance and VAR changes, real-world event annotation — and, the
highest-leverage item in the whole roadmap, an **extension mechanism**
(`as_ggcpt()`, `cpt_register_method()`) that lets a detector this package
neither wraps nor can depend on join the same grammar. Along the way
`cpt_detect()` goes from 31 to **50 wired methods**, the three duplicated
method tables collapse into one declarative registry, `cpt_select()` gives
the package its first criterion that actually *selects*, supervised
detection arrives with learned penalties, `cpt_benchmark()` runs the grid,
`cpt_power()` answers the question that should precede an analysis, and the
package gets its first visual-regression net. Part 0 is that ledger; Part II
is now annotated with what it delivered and what it did not.

---

`ggchangepoint` 0.3.0 closed the documentation gap of the 0.2.0 cycle: the
README introduces every export, the dispatcher is honest (only wired methods
are offered), a `cpt_methods()` introspection table ships, the `ggcpt` S3
surface is complete (`summary`/`as_tibble`/`as.data.frame`/`format`/`plot`),
and eleven verified 0.2.0 bugs (B1–B11) were fixed.

**0.4.0's brief was the opposite of 0.3.0's: not documentation, but methods.**
Guided by a literature-and-CRAN survey (July 2026; §2), it ships the largest
engine wave in the package's history — eighteen new wrappers spanning
multiscale inference with confidence intervals (SMUCE/HSMUCE), exact
change-in-slope (CPOP), the CROPS penalty path, Bayesian detection offline and
online (bcp, BOCPD, BEAST), sequential/nonparametric testing (CPM, kernel
running statistics, NP-MOJO), robustness to drift, autocorrelation and
dependence (DeCAFS, self-normalisation, EnvCpt), high-dimensional and
multivariate detection (inspect, ocd, geomcp), regression breaks
(Bai–Perron/strucchange, segmented), and the modern fastcpd engine
(mean/var/ARMA/GARCH) — taking `cpt_detect()` from 13 to **31 wired methods**
(§3). Around the new engines it builds the supporting features that compound
across all of them: uncertainty-aware plotting (`show_ci`, `show_fit`,
posterior and run-length displays), genuine multivariate input with faceted
rendering, panel/batch detection, bootstrap stability diagnostics, interactive
rendering, and per-method citations (§4). It also fixes **eighty-six verified
bugs across three audit passes** (§5) — including an `ecp` wrapper that
fabricated changepoints on no-change data, a WBS wrapper that silently
discarded the model selection it claimed to use, metrics that punished a
correct "no changepoints" answer, and an `hsmuce` path that terminated the R
session rather than raising an error.

The two commitments that define the package are unchanged: **(i) every
detector returns a tidy tibble inside a structured `ggcpt` object**, and
**(ii) every result is directly renderable with `ggplot2`**. 0.4.0 adds a
third: **(iii) where a method quantifies uncertainty — confidence intervals,
posterior probabilities, run-length distributions — the `ggcpt` object
carries it and `autoplot()` can draw it.**

**Part II asks what comes after that.** With the "not enough methods" gap
closed, the remaining gaps are inferential (the package can draw a confidence
interval only where an engine supplies one, and has no general answer to "is
this changepoint real?"), grammatical (a visualization-first package that
cannot draw the test statistic, the solution path, the scale space, or a
changepoint-as-interval is leaving its own specialty on the table), and
boundary-shaped (everything outside a bare numeric vector: dates, streams,
functional data, networks, external detectors, real-world events). Part II is
a CRAN-verified survey of those gaps and fourteen themes of API-level
proposals against them, ordered into six waves.

**How to read this document.** Part I is a **ledger** — decisions already made
and code already written. Part II is a **design and literature document**: not
a changelog, and not a commitment.

> **Provenance.** Part II was maintained as a separate `features.md` catalogue
> (July 2026) and was merged here after being re-verified against the 0.4.0
> source. Seven of its proposals turned out to be already implemented, or to
> rest on a claim about the code that is no longer true. Those are recorded in
> **§8.3** and have been removed from the themes rather than left standing as
> work. `features.md` no longer exists; this file is the single roadmap.

---

## Contents

**Part 0 — The ledger: what 0.5.0 shipped**

- [0.1 The shape of the release](#01-the-shape-of-the-release)
- [0.2 The extension mechanism and the registry](#02-the-extension-mechanism-and-the-registry)
- [0.3 Engine wave #2](#03-engine-wave-2)
- [0.4 Inference, selection, diagnostics](#04-inference-selection-diagnostics)
- [0.5 Supervised detection, communication, benchmarking, streaming, power](#05-supervised-detection-communication-benchmarking-streaming-power)
- [0.6 The open questions, answered](#06-the-open-questions-answered)
- [0.7 Corrections found while building](#07-corrections-found-while-building)
- [0.8 Bugs found and fixed during the 0.5.0 build](#08-bugs-found-and-fixed-during-the-050-build)
- [0.9 What Part II still leaves open](#09-what-part-ii-still-leaves-open)

**Part I — The ledger: what 0.4.0 shipped**

1. [Status: where 0.3.0 landed](#1-status-where-030-landed)
2. [The 2026 method survey](#2-the-2026-method-survey)
3. [The 0.4.0 engine wave](#3-the-040-engine-wave)
4. [New features beyond detectors](#4-new-features-beyond-detectors)
5. [The 0.4.0 bug audits (verified)](#5-the-040-bug-audits-verified)
6. [Architecture, dependencies, testing](#6-architecture-dependencies-testing)
7. [Backward compatibility](#7-backward-compatibility)

**Part II — The roadmap: 0.5.0 and beyond**

8. [Where the package stands, and what is left](#8-where-the-package-stands-and-what-is-left)
9. [The survey refreshed and CRAN-verified](#9-the-survey-refreshed-and-cran-verified)
10. [Theme A — Inference: intervals, not just points](#10-theme-a--inference-intervals-not-just-points)
11. [Theme B — Choosing the number of changepoints](#11-theme-b--choosing-the-number-of-changepoints)
12. [Theme C — Influence, stability and robustness diagnostics](#12-theme-c--influence-stability-and-robustness-diagnostics)
13. [Theme D — Supervised changepoint detection](#13-theme-d--supervised-changepoint-detection)
14. [Theme E — Seeing the statistic](#14-theme-e--seeing-the-statistic)
15. [Theme F — Time indices and data structures](#15-theme-f--time-indices-and-data-structures)
16. [Theme G — Streaming and online monitoring](#16-theme-g--streaming-and-online-monitoring)
17. [Theme H — New data types](#17-theme-h--new-data-types)
18. [Theme I — The extension mechanism](#18-theme-i--the-extension-mechanism)
19. [Theme J — Ensembles, consensus and method choice](#19-theme-j--ensembles-consensus-and-method-choice)
20. [Theme K — Benchmarking and evaluation](#20-theme-k--benchmarking-and-evaluation)
21. [Theme L — Communication](#21-theme-l--communication)
22. [Theme M — Simulation, power and study design](#22-theme-m--simulation-power-and-study-design)
23. [Theme N — Performance, architecture, infrastructure](#23-theme-n--performance-architecture-infrastructure)
24. [Prioritisation](#24-prioritisation)
25. [Open questions and decisions needed](#25-open-questions-and-decisions-needed)
26. [References](#26-references)

---

# Part 0 — The ledger: what 0.5.0 shipped

## 0.1 The shape of the release

0.4.0 was an engine wave. 0.5.0 is a **surface** release: the detectors grow
by nineteen, but the code around them roughly doubles, because that is where
Part II said the gaps were.

| | 0.4.0 | 0.5.0 |
|---|---|---|
| Wired methods | 31 | **50** |
| Exported objects | 63 | **130** |
| Registered S3 methods | 19 | **50** |
| Help topics | 66 | **124** |
| `change_in` levels | 5 | **9** (adds covariance, network, regression, seasonality) |
| Test assertions | 695 | **1 235**, plus 25 visual snapshots |
| Composable geoms | 4 | **7** |
| Sources of the method table | 3 | **1** |

The three commitments carry over unchanged — a tidy tibble inside a
structured `ggcpt`, direct `ggplot2` rendering, and uncertainty carried on
the object wherever a method quantifies it. 0.5.0 adds a fourth:
**(iv) the package is extensible from the outside** — a detector it does not
wrap, cannot wrap, or has never heard of can join the grammar in four lines,
and is labelled as user-supplied wherever it appears.

## 0.2 The extension mechanism and the registry

Part II ranked §18 (Theme I) first, and building it first turned out to be
right for a reason the roadmap only half-anticipated: it is not only the
answer to "this engine is not on CRAN", it is also the *test harness* for
the `ggcpt` contract. Every contract check in `as_ggcpt()` is exercised by
the registration tests rather than only by whichever wrapper happens to hit
it.

- `as_ggcpt(cp, x, ...)` — a validating constructor for external
  changepoints, running the same sorting, de-duplication, range and
  alignment checks as every built-in wrapper, and accepting the optional
  extras (`ci`, `regions`, `fitted`, `index`, `extra`).
- `cpt_register_method()` / `cpt_unregister_method()` /
  `cpt_registered_methods()` — session-scoped registration, deliberately not
  persisted. A registered method's `fn` may return bare indices or a
  finished `ggcpt`; either way the registration's name and engine win, so
  `cpt_methods()` and the result agree.
- **Labelling, not endorsement.** `status = "registered"` in
  `cpt_methods()`, a line in `print.ggcpt()`, and a `cpt_cite()` that
  returns the supplied citation or states plainly that none was given. The
  package validates the shape of the output, not the statistics, and says
  so.

**The registry** (§23's refactor, done before the engine wave rather than
after it, exactly as Part II advised). `builtin_registry()` is now the only
place a method's capabilities are declared. `cpt_methods_table()`,
`method_change_in_support()` and `cpt_detect()`'s dispatch all derive from
it, and `cpt_register_method()` appends to the same structure — so a
registered detector travels the identical code path as a built-in one.
`cpt_methods()` gains nine capability columns (`multivariate`,
`univariate`, `online`, `ci`, `fitted`, `posterior`, `statistic`, `path`,
`scale_space`), which is what makes `cpt_recommend()` and the "these engines
do expose it" error messages possible at all.

`cpt_install_engines()` closes the other half of the `Suggests` question:
the long tail stays optional, but installing a family is one call.

## 0.3 Engine wave #2

Nineteen methods, sequenced as Part II proposed — high-dimensional first
(closest to what existed), then Bayesian, then functional, then the applied
vocabularies.

| Method | Engine | What it adds |
|---|---|---|
| `nsp` | `nsp` | significance **regions** with exact global coverage; self-normalised and AR variants |
| `mcp` | `mcp` | Bayesian formula-based regression with full changepoint posteriors (JAGS) |
| `esac`, `pilliat` | `HDCD` | sparsity-adaptive high-dimensional mean changes |
| `hdcov` | `changepoints` | a change in the covariance operator with no change in any margin |
| `network` | `changepoints` | a change in dynamic-network edge structure |
| `var` | `changepoints` | a change in VAR(1) dynamics — invisible to every mean-change engine |
| `hdreg` | `changepoints` | a change in the coefficients of a sparse high-dimensional regression |
| `fmean`, `fcov` | `fChange` | functional mean, covariance, trace and eigenstructure |
| `kwc` | `KWCChangepoint` | robust depth-rank segmentation for functional and multivariate data |
| `fabisearch` | `fabisearch` | network structure via non-negative matrix factorisation |
| `wbsts` | `wbsts` | second-order changes in a nonstationary series |
| `bfast` | `bfast` | season-and-trend breaks (remote sensing) |
| `pettitt`, `buishand`, `snht` | `trend` | the hydrology and climatology single-change tests |
| `taylor` | `ChangePointTaylor` | the quality-control default, with bootstrap confidence per change |
| `binsegrcpp` | `binsegRcpp` | a fast binary-segmentation path across several loss functions |

Three of these needed something the roadmap did not anticipate, and each is
documented where a user will hit it:

- **`network`** takes two independent observations of the sequence, which
  the theory needs for sample splitting. Given one, the wrapper builds the
  second by independent edge thinning and **says so in a message**, rather
  than silently handing the same data to both arguments.
- **`fabisearch`** calls \pkg{NMF}'s multi-run machinery, which resolves
  helpers through the search path and fails with "none of the packages are
  loaded" when NMF is merely loaded. The wrapper attaches NMF for the
  duration of the call and detaches it after. It also refuses negative
  input up front, because NMF is undefined there and the engine's own
  failure is buried inside a factorisation.
- **`taylor`** reports its interval as the string `"(100 - 101)"` and its
  locations on the *right* convention; both are parsed and converted on the
  way in.

## 0.4 Inference, selection, diagnostics

**Theme A.** `nsp_wrapper()` plus the additive `regions` slot,
`cpt_regions()`, `geom_cpt_region()` and `autoplot(show_regions =)` — on by
default for a result that has regions, because for those methods the region
*is* the result. `cpt_confint()` unifies native, posterior, bootstrap and
NSP intervals behind one tibble with a `source` column. `cpt_test()` ships
with the native tests (`strucchange`'s Chow F, `segmented`'s Davies) and an
explicitly unadjusted fallback, with a `selection_adjusted` column and a
warning.

**Theme B.** `cpt_select()` over one candidate ladder, six criteria, and
three plots — including the **ladder** of small multiples, which shows what
each candidate K actually *is* rather than only what it scores. The
`"mbic"` criterion is the genuine Zhang–Siegmund segment-length penalty,
which `cpt_penalty()` documents itself as unable to express; this is where
it lives.

**Theme C.** `cpt_influence()` (delete and outlier perturbation, four
plots, `changepoint.influence` where it applies and a generic recomputation
everywhere else), `cpt_leverage()`, `cpt_sensitivity()`.

**Theme E.** `cpt_statistic()`, `cpt_solution_path()`, `cpt_scale_space()`
and their `ggcpt_*()` plots, reached from `autoplot(type =)`. Support is
recorded in the registry, so an engine that exposes nothing errors with the
list of engines that do — the behaviour Part II specified. The two-panel
layout reuses `ggcpt_posterior()`'s faceted idiom rather than adding a
`patchwork` dependency, following §8.3's correction.

**Theme F.** `index` on `cpt_detect()`, direct `ts`/`xts`/`zoo`/`tsibble`
input, the data-frame interface, and the index threaded through `tidy()`,
`augment()`, `autoplot()`, `cpt_confint()`, `cpt_annotate_events()` and
`cpt_report()`. Detection stays on positions — the correct separation, and
the reason all 50 engines were untouched by this.

## 0.5 Supervised detection, communication, benchmarking, streaming, power

**Theme D.** `cpt_labels()`, `as_cpt_labels()`, `cpt_label_error()`,
`cpt_label_error_curve()`, `cpt_learn_penalty()`, `geom_cpt_label()` and
`scale_fill_cpt_label()`. Open question 5 is answered by construction:
labels and `cpt_metrics_annotated()`'s ground truth are the same shape, and
`as_cpt_labels()` converts between them. A learned penalty is usable
wherever a number is — `cpt_detect()`, `cpt_penalty()`, and the wrappers
that take a numeric penalty directly.

**Theme J.** `cpt_consensus()` (reusing `cpt_metrics()`'s matching rule, so
there is one notion of "the same changepoint") and `cpt_recommend()` (the
capability matrix made printable, with the noise-structure knowledge the
matrix cannot encode added on top). Both state prominently that agreement
is not inference.

**Theme L.** `cpt_annotate_events()` with its three-way output,
`geom_cpt_event()`, `cpt_report()`, `cpt_gt()`, and the accessibility pass:
an Okabe–Ito palette (`scale_colour_cpt()` and friends), redundant linetype
encoding in the overlay comparison, and generated alt text on every
`autoplot()`.

**Theme K.** `cpt_benchmark()` with per-cell failure capture and three
plots including the Demšar critical-difference diagram; `cpt_datasets()`
(offline, deterministic, runs inside `R CMD check`); `cpt_load_tcpd()`
(download-and-cache under `tools::R_user_dir()`, multi-annotator ground
truth intact); `cpt_annotations()`.

**Theme G.** `cpt_monitor()`, `cpt_update()`, `alarms()`, `cpt_replay()`,
`cpt_delay()` — and one **native implementation**, the mixture
Shiryaev–Roberts e-detector of Shin, Ramdas and Rinaldo. See §0.6.

**Theme M.** `cpt_power()` (with Monte Carlo error reported, not hidden),
`cpt_min_detectable()`, `cpt_scenarios()`, and the two `cpt_simulate()`
gaps §8.3 identified: `seasonality` and a smooth `sd_trend`.

## 0.6 The open questions, answered

Part II's §25 listed eight decisions. All eight were taken.

1. **Slots or new classes?** Additive optional slots (`regions`,
   `diagnostics`, `index`) for things that are still "a segmentation of a
   series"; new classes for the things that are not (`ggcpt_monitor`,
   `ggcpt_benchmark`, `ggcpt_selection`, `ggcpt_influence`,
   `ggcpt_sensitivity`, `ggcpt_events`, `ggcpt_power`, `ggcpt_consensus`,
   `ggcpt_label_curve`, `ggcpt_penalty_model`, `ggcpt_delay`). As §8.3
   predicted, the slot pattern was free: `data_wide` had already set the
   precedent.
2. **How honest can `cp` be for NSP?** Part II leaned toward
   `cp = NA_integer_`. **Rejected, and this is a genuine departure.** An NA
   `cp` breaks `augment()`, `cpt_metrics()`, `cpt_consensus()` and every
   plot, and — worse — makes an NSP fit score as "found nothing" in a
   benchmark, which is the opposite of the truth. The midpoint is populated
   and labelled instead: a `cp_source` column reading `"region_midpoint"`,
   a line in `print()`, a paragraph in `?nsp_wrapper`, and `autoplot()`
   drawing the band by default. Labelling beats absence when every
   downstream consumer needs the column.
3. **`Suggests` strategy.** Both routes, not one. The engines that exist on
   CRAN are in `Suggests` (35 engines inside a 53-package `Suggests` list),
   because a wired method with a test-verified wrapper is worth the weight;
   the ones that are not on CRAN are reached through registration, which is
   now a documented, vignetted path rather than a blocker. The prediction in
   §23 that this would "push past 45" was right about the number and wrong
   about the consequence: every one is `requireNamespace()`-guarded, so the
   package still checks clean with none of them installed.
4. **JAGS.** Yes, with a guard. `mcp` is in `Suggests`, `mcp_wrapper()`
   checks for `rjags` and names JAGS in the error, and the examples are
   `@examplesIf`-gated on both.
5. **One annotation representation or two?** One. See Theme D above.
6. **Do we implement anything original?** Yes, once, deliberately, and
   labelled. The e-detector has no R implementation, the construction is a
   dozen lines, and optional stopping on the martingale $M_t - t$ gives
   it a finite-sample lower bound of $1/\alpha$ on the in-control
   average run length with no calibration run — so
   `cpt_monitor("edetector")` is native, says so in `print()`, in
   `?cpt_monitor`, in `cpt_cite("edetector")` and in NEWS. Nothing else in
   the package is. Writing original mathematics also earned an original
   bug, S11 below; the lesson recorded there is that a derivation the
   package relies on needs a test that measures the quantity, not a test
   that the code runs.
7. **Benchmark data policy.** Download-and-cache, with `cpt_datasets()`
   covering every offline example and test, as Part II leaned.
8. **When to freeze.** After this release. See §0.9.

## 0.7 Corrections found while building

- **`hdbinseg` is archived on CRAN again.** Part II's opening correction
  said it was back at 1.0.3 and that `sbs` therefore waited only on a
  wrapper. Checked against the live index during this cycle: the package
  page redirects to the archive, and the newest tarball there is 1.0.2.
  `sbs` moves back to `"when on CRAN"`, and `planned_methods()` now reads
  uniformly — every remaining planned method waits on CRAN, and nothing
  waits on a wrapper. `changeforest` was added to the same table for the
  same reason. This is precisely the situation §18 exists for, and the
  extending vignette gives the recipe.
- **`penaltyLearning::IntervalRegressionCV()` needs ten series.** Below
  that its internal cross-validation cannot form folds. `cpt_learn_penalty()`
  therefore ships a built-in squared-hinge interval regression and uses the
  published implementation when the training set is large enough and the
  package is installed — which also means the function works at all on the
  two- or three-series examples people actually start with.
- **AIC over-selects, visibly.** On the package's own three-segment test
  signal, `criterion = "aic"` takes every rung of the ladder. That is the
  criterion behaving as its definition requires (its penalty does not grow
  with *n*), so it is documented rather than quietly dropped, and the test
  suite asserts the honest thing rather than the convenient one.

## 0.8 Bugs found and fixed during the 0.5.0 build

Thirty-six defects were found and fixed in the same cycle. Three were
introduced by this release's own code; S7–S8 and S12 are the kind that only
surface when someone reads what an engine actually returns rather than what
its documentation implies; S17–S36 came out of the post-implementation audit
passes, which went after the surfaces the tests never reached — parallel
execution, the `ggcpt` contract across every installed engine, degenerate
input, and the claims the prose makes; and S11 is the one that matters most,
because it was in the only piece of mathematics this package derives
itself.

Four of them are worth reading as a set, because none would have been
caught by a test that only asks whether the code runs. S11's alarm rate was
wrong by a factor of two while every call succeeded. S12 rendered a
complete, plausible heatmap in which every value was `NA`. S14's fallback
path worked so smoothly that the primary path was never once taken. And
S15 is the sharpest of the four: the test *infrastructure* was deleting its
own evidence, so the visual suite would have reported green forever. The
tests written for all three now measure the quantity in question — the
in-control alarm rate against $n\alpha$, that the statistics are
finite and something crosses the threshold, and that the target intervals
are bounded on both sides.

The audit passes that produced S17–S22 were organised by *surface never
exercised*, not by module. Four are worth naming because each was a
different kind of blind spot. Parallel execution: every tool that fans out
does so through `future.apply`, and nothing in the suite had ever run one
under a non-sequential plan. The `ggcpt` contract: each engine had its own
test, but nothing asserted the *same* invariants across all of them at
once — one `glance()` row, $n$ `augment()` rows, segments that tile the
series exactly, changepoints sorted, unique, integer and inside
$[1, n)$, an index that survives to `cp_index`, and an `autoplot()` that
builds. Sweeping all 49 installed methods against that list found no
contract violations, which is the strongest single statement available
about the dispatcher. Degenerate input: constant series, $n = 2$, embedded
`NA`/`Inf`, and results with zero changepoints, through every accessor and
print method. The prose: the README and vignettes make claims that code review does not
check, which is how S20 and S21 survived. And the capability table itself:
sweeping all 49 installed methods against all nine `change_in` values —
441 calls — checked both directions of the registry's central claim, that
every declared combination runs and every undeclared one refuses. Three
declared combinations did not run (S25–S27). The twelve undeclared
combinations that *do* run are deliberate: `change_in = "mean"` is accepted
by every method, and each records its own native change type on the result,
so no object claims to be a mean change when it is not — verified, not
assumed. S28 is the reminder that an audit pass can introduce a defect of
its own: it was caused by the fix for S23 and caught by `R CMD check`, not
by the 2341-assertion suite.

S34 is the sharpest lesson of the release, and the one that generalises.
`mcp` is the single engine that cannot be installed on the development
machine, because it imports `rjags` and JAGS is a system library that is not
there. Its example is gated with `@examplesIf`, and its only test asserted
the *missing-dependency* message — a test that skips precisely when the
package is present. So the engine with the least local coverage also had the
least remote coverage, and the wrapper had never once been run end to end
anywhere. It was broken in every call. **An engine that cannot be exercised
locally needs a test that runs where it can be, not a test of what happens
when it is absent.** S36 is its second half: when the dependency is a
*system* library, package presence does not imply it works, so the honest
guard is a post-hoc check of the result, not a pre-hoc check of the library.

| # | Symptom | Cause and fix |
|---|---|---|
| S1 | Every multi-capability method silently lost its extra `change_in` values: `cpt_detect(x, method = "pelt", change_in = "var")` errored with `Supported: c("mean", "var", "meanvar")` | `tibble::tribble()` **deparses** a `list(c("a","b"))` cell into the string `'c("a", "b")'`. The registry's `supports` column is now a comma-separated string split on read, which is also more readable and diffable. Caught by asserting the column's type, not just its content. |
| S2 | `geom_cpt_event()` produced a text layer with no `x`, so every event plot errored | `$` on a mapping does **partial matching**, so `mapping$x` returned the `xintercept` element and the alias was skipped. Fixed with `[[nm, exact = TRUE]]`; regression-tested by building the plot and reading the layer data. |
| S3 | The same layer errored again once `x` was mapped, this time on the *series'* aesthetics | The text layer inherited the plot's mapping, which names columns the event frame does not have. `inherit.aes = FALSE` by default, exposed as an argument. |
| S4 | The e-detector reported one persistent change as 137 alarms | Restarting against a stale pre-change baseline re-fires on the very next observation. Added a `relearn` window: after an alarm the detector adopts the new regime as its baseline and stays silent while it does. |
| S5 | `ocd` monitoring printed an alarm at "statistic 1.10, threshold 16.4" | `normalisedStatistics()` is already divided by the threshold, so the raw thresholds were the wrong comparison point. Report 1. |
| S6 | A one-coordinate `ecp` result with a time index started rendering as a multivariate facet grid | `autoplot()` tested `ncol(data_wide) > 2`, and the new `index_value` column pushed a single-coordinate frame over the line. Counts coordinates now, excluding the index columns. |
| S7 | `fabisearch` returned **every** candidate split the search proposed as a changepoint whenever `alpha` was supplied | `detect.cps()`'s `stat_test` column has two shapes: a permutation *p-value* when `alpha = NULL`, and a *logical verdict* when `alpha` is a number. Reading the logical form as a number turns `FALSE` into 0, which clears any p-value threshold. The wrapper branches on the type; the filter is unit-tested against both shapes without paying for two engine runs. |
| S8 | `fabisearch` could report "no changepoints" for a purely arithmetic reason | A permutation p-value cannot fall below `1 / n_reps`, so `n_reps = 5` with `alpha = 0.05` makes significance unreachable whatever the data. The wrapper now says so. The same guard was added to `hdcov_wrapper()` and `network_wrapper()`, whose permutation threshold is a `1 - alpha` quantile of `n_perm` values and is extrapolated when `n_perm < 1 / alpha`. |
| S9 | `cpt_methods()` printed "RGL: unable to open X11 display" on every headless machine | Asking whether an engine is installed *loads its namespace*, and `fabisearch` pulls in `rgl`, which warns on load. That is information about the display, not about the installation. The status check now swallows it; a test asserts `cpt_methods()` is silent. |
| S10 | `cpt_select()` reported a confidently "chosen" K off a one-point curve | The search-based methods tune themselves by an internal criterion and ignore `penalty`, so sweeping it returns the same segmentation at every rung and there is nothing to choose between. It now warns and names the methods that do give a full ladder. |
| S12 | `cpt_scale_space(method = "npmojo")` refused a matrix, and produced an all-NA heatmap when reached through a `ggcpt` | Two faults in one path: the input was flattened with `as_uni_vector()` even though npmojo is a multivariate engine, and the extractor read `$stat`/`$threshold` when `np.mojo()` names them `test.stat` and `threshold.val` — `$threshold` holds the rule (`"bootstrap"`), not the number. The result was a uniform grey grid with nothing significant, which looks like a finding. The test now asserts the statistics are finite and that something crosses the threshold. |
| S15 | **The visual regression net silently erased itself.** Any plain `test_dir()` deleted all 25 snapshots | The visual tests skip at the top of each block, so `expect_doppelganger()` was never reached, nothing was announced, and testthat pruned the files as unused. The next `NOT_CRAN=true` run then *recreated* them from whatever the code did at that moment — so the net would have passed forever and caught nothing. Each block now calls `announce_snapshot_file()` for its titles before skipping, which needs testthat edition 3; the package opts in (`Config/testthat/edition: 3`), and the whole suite passes unchanged under it. A test asserts the announced names match the files on disk, because a drift there would restore the original failure silently. |
| S16 | A running monitor accepted a feed of the wrong width | `cpt_update()` coerced whatever it was given to a matrix without checking it against the baseline, so an `ocd` monitor built on three coordinates consumed two-column data and the univariate detectors silently read column 1 and dropped the rest. A monitor is dimensioned at construction; it now says so. |
| S13 | Selecting columns off a result tibble kept its class, so printing the fragment warned "Unknown or uninitialised column" | `ggcpt_benchmark`, `ggcpt_batch`, `ggcpt_recommendation`, `ggcpt_label_curve`, `cpt_labels` and `cpt_label_error` are tibbles with a `print()` method that reads named columns. Base `[` preserves the class, so `bm[, c("dataset", "method")]` still claimed to be a benchmark. `[` methods now drop back to a plain tibble the moment a required column is selected away — which is also what makes these objects safe to hand to `dplyr::select()`. |
| S14 | The `penaltyLearning` delegation in `cpt_learn_penalty()` was unreachable, and every target interval was one-sided | The default penalty grid for `cpt_label_error_curve()` was a fixed geometric span topping out near $40\log n$. On a series with a large change the detector still finds it there, so the label-error curve never turns back up, the minimum runs to the edge of the grid, and the target interval comes out unbounded above — which gives interval regression no margin to fit and makes `IntervalRegressionCV()` fail every time. The grid's top end is now found by doubling until the detector reports nothing. All twelve targets in the check series went from one-sided to bounded, and the published estimator is reached instead of the fallback. |
| S11 | **The e-detector's average-run-length guarantee did not hold.** Measured in-control alarm rate was ~1.8x the bound at `alpha = 0.01` and ~3.5x at `alpha = 0.001` | The per-shift Shiryaev–Roberts statistics were combined with `max()`. The ARL bound comes from optional stopping on $M_t - t$, which needs $E_\infty[M_t] = t$; a convex combination of e-detectors preserves that and a maximum does not — a max over $K$ shifts crosses the threshold roughly $K$ times as often under the null. Changed to a uniform average. The regression test now *measures* the in-control alarm rate against $n\alpha$ rather than asserting that the code runs, which is what would have caught it the first time. |

| S17 | **A method registered with `cpt_register_method()` was invisible to every parallel worker.** Under `future::plan(multisession)`, `cpt_batch()` on a registered method died with `'arg' should be one of "pelt", "binseg", ...` | The registry is an environment inside the package namespace, and a `future` worker loads the package fresh, so the parent's registrations do not exist there. The failure was silent about its real cause — it looked like the method name was wrong. Every parallel entry point (`cpt_batch()`, `cpt_benchmark()`, `cpt_consensus()`, `ggcpt_compare()`, `cpt_influence()`, `cpt_sensitivity()`, `cpt_power()`) now wraps its worker with `with_session_registry()`, which snapshots the registry in the parent and re-creates it on the far side. With nothing registered the wrapper returns the function untouched, so the common path keeps exactly the globals it had. |
| S18 | `cpt_benchmark()` scored a labelled dataset as unlabelled, reporting `n_annotators = 0` and `NA` metrics, with no warning | Ground truth was read only from `annotations` or `truth`. A list built with `changepoints` — the name `cpt_simulate()` uses for the same thing — was silently ignored, and the result was a full, plausible benchmark table in which every metric was missing. `changepoints` is now accepted, a list-valued `truth` is treated as several annotators rather than flattened, extraction is exact (`$` partial-matches, so `annotations_raw` would have been read as ground truth), and a list dataset carrying none of the three names now warns and says what it did find. A bare numeric vector is legitimately unlabelled and stays quiet. |
| S19 | `fabisearch_wrapper()` let NMF's own error through for an all-zero time point | The wrapper pre-checks non-negativity but not the other precondition NMF has, so a series with a zero row failed several layers down with a message that named neither the rows nor the fix. Caught at the wrapper now, with the offending row numbers. |
| S20 | The introduction vignette said four engines were planned; there are five | `changeforest` was added to the planned list without the prose being updated. The count and the citation now match `cpt_methods()`. |
| S21 | The README credited the e-detector's average-run-length bound to Ville's inequality | S11 corrected the derivation in the function's Details section but left the same wrong attribution in the README and in the `alpha` parameter's own documentation. The bound is optional stopping on $M_t - t$; Ville's inequality bounds the probability that a non-negative supermartingale ever exceeds a level, which is a different statement. All three places now agree. |
| S22 | Fifteen exported functions appeared nowhere in the README, and the test that checks this only runs off CRAN | `test-doc-coverage.R` asserts every export is mentioned in the README; it skips on CRAN, so a plain `R CMD check` had been passing while it failed. The fifteen — the accessibility scales, `geom_cpt_region()`, `geom_cpt_event()`, `cpt_update()`, `cpt_scenarios()`, `cpt_annotations()`, `cpt_statistic()`, `ggcpt_statistic()`, `cpt_label_error_curve()`, `cpt_registered_methods()`, `as_cpt_series()` — were woven into the sections they belong to rather than listed, and the README was re-knitted. |

| S23 | **`pilliat` reported a changepoint at every observation whenever the number of coordinates was an exact power of two** — on pure noise as readily as on a real change, at $p = 2, 4, 8, 16, 32, 64, 128$ | `HDCD` 1.1's `Pilliat()` builds its partial-sum threshold vector with `t <- 1; repeat { push(...); t <- 2 * t; if (t >= p) break }`, which yields $\lfloor\log_2(p-1)\rfloor + 1$ entries — one short of the $\lfloor\log_2 p\rfloor + 1$ sparsity scales the C routine then indexes. For a power-of-two $p$ the C code reads past the end of the vector, every candidate clears the garbage value, and the engine returns $n-1$ changepoints. A corrected vector cannot be injected (the analytic branch overwrites what is passed, and the empirical branch zeroes the Berk–Jones scale count unless it runs its own calibration), so the wrapper refuses those dimensions, names the engine bug, and points at `esac`, which is unaffected at every $p$. A version guard means the refusal lifts by itself once HDCD fixes it, and a post-hoc check on the returned count catches the fault if a later version moves rather than fixes it. Found by a reproducibility sweep that reported `pilliat` finding 299 changepoints in a 300-point series — the seed check was reproducible, and the *number* was the finding. |

| S24 | `cpt_confint()` bootstrapped an NSP result 200 times instead of reading the interval already on it | `has_native` looked only for `ci_lower`/`ci_upper`. NSP reports an interval that *provably contains a change* — the strongest interval statement in the package — under `region_start`/`region_end`, so `method = "auto"` fell through to the bootstrap and produced a weaker answer slowly. A `native_bounds()` helper now reads either shape and labels the source (`"nsp_region"`), reporting NSP's own global level rather than `NA`. |
| S25 | `bfast(change_in = "seasonality")` errored with `$ operator is invalid for atomic vectors` — i.e. it never worked | \pkg{bfast} reports "no breakpoints in this component" as a bare logical `NA`, not as a `breakpoints` object with an empty slot, so `bp$breakpoints` was `$` applied to an atomic vector. That is the *ordinary* outcome for a seasonal component of stable amplitude, so the declared capability failed on most series. The test now runs all three of `bfast`'s declared `change_in` values. Also added: `change_in = "seasonality"` with `season = "none"` is a contradiction and now says so. |
| S26 | The registry claimed `binsegrcpp` detects a change in variance alone; it cannot | \pkg{binsegRcpp} 2025.5.13 offers `mean_norm`, `meanvar_norm`, `poisson`, `laplace` and `l1` — no variance-only cost. The wrapper mapped `change_in = "var"` to a nonexistent `"var_norm"` and died inside the engine with `unrecognized distribution`. Both the registry row and the wrapper's argument now stop at `mean` and `meanvar`, and the docs name the alternatives. |
| S27 | `taylor` accepted a bootstrap count the engine rejects | `ChangePointTaylor` requires 100 to 1,000,000 resamples; the wrapper validated only `>= 1`, so `n_bootstraps = 60` failed several frames down with a message about `n_bootraps` — the engine's own misspelling of an argument the caller never typed. Validated at the wrapper with the real bounds. |
| S28 | **`pilliat_wrapper()` silently stopped being exported.** `R CMD check` reported `Undocumented code objects: is_power_of_two` | The fix for S23 inserted two internal helpers *between* `pilliat_wrapper`'s roxygen block and its definition, so the `@export` attached to the helper instead. Dispatch through `cpt_detect(method = "pilliat")` kept working, every test kept passing, and the public entry point vanished — nothing in the suite asserted the export list. A new test now asserts that every wrapper named in the registry is exported and has a help page, and that a fixed list of internals is not exported. |


| S29 | A baseline holding an `NA` was reported as a *flat* baseline | `sd()` of a vector with an `NA` is `NA`, and the `!is.finite(sd0) || sd0 <= 0` guard fired on it, so `cpt_monitor("edetector", baseline = <has NA>)` said "`baseline` has zero variability" — a diagnosis that sends the user looking at the wrong thing. Finiteness is checked first now, and says how many values are missing. |
| S30 | `cpt_delay()` scored a true changepoint that occurs after the end of the stream | Nothing checked `truth` against the number of observations, so `cpt_delay(mon, truth = 10000)` on a 300-point stream returned a clean miss — a statement about the argument, not the detector. It now errors, as does a zero-length or non-positive `truth`; a missing `truth` says why it is required instead of emitting R's raw "argument is missing" message. |


| S31 | Two tests asserted Suggests-only behaviour unconditionally | The package is right — `repel = TRUE` without \pkg{ggrepel} stops with an install hint, and the `NULL` default detects the package — but the test asserted only the installed-here branch, so the suite errored under a library holding the Imports and none of the Suggests. It now asserts the correct behaviour in *both* worlds rather than skipping one, which is what a Suggests-gated test should do. The monitor-equivalence test added in the same audit made the same mistake with \pkg{cpm} and was caught the same way. Found by running the check under R 4.6.0 against a minimal library; that environment is the only thing that exercises the no-Suggests path the DESCRIPTION promises, and it is now part of the release procedure rather than an occasional extra. |


| S32 | `cpt_select()` dropped the time index, so a selection made from a dated fit came back in positions | It read `x$data$value` off a `ggcpt` and threw the rest away, and had no `index` argument of its own — an `index =` passed by a hopeful caller went into `...`, was forwarded to `cpt_detect()` at every rung of the ladder, and then vanished because the chosen fit was rebuilt with `as_ggcpt()` on the bare series. It now takes `index` explicitly, inherits it from an indexed `ggcpt`, checks its length, and attaches it to the fit. Found by sweeping the index across every tool that should carry one, not just `cpt_detect()`. |
| S33 | Half the result classes had no `tidy()` method | `tidy(cpt_benchmark(...))` worked while `tidy(cpt_power(...))` failed with "no applicable method" — for an object that is *already* one row per scenario. Seven classes (`ggcpt_influence`, `ggcpt_power`, `ggcpt_monitor`, `ggcpt_delay`, `ggcpt_recommendation`, `cpt_labels`, `cpt_label_error`) now answer, and `glance()` reports the one-row summary a `ggcpt_delay` already holds as scalars. The tibble subclasses hand back a *plain* tibble with the extra attributes stripped, so nothing downstream re-enters a `print()` method that reads columns the caller may have dropped. |


| S34 | **`mcp_wrapper()` never worked.** Every call stopped with `This is a plateau-only model so no x-axis variable could be derived from the segment formulas` | The default model for `change_in = "mean"` is `list(y ~ 1, ~ 1)`, which contains no predictor, so `mcp::mcp()` has nothing to infer its x-axis variable from and requires `par_x` to be named. The wrapper now passes `par_x = "t"` — the column its own data frame always carries — unless the caller supplied one. The reason this survived every local pass is structural: `mcp` imports `rjags`, JAGS is not installable on the development machine, the example is behind `@examplesIf`, and the *only* test was the negative one asserting the missing-dependency message, which skips exactly when `mcp` is installed. So the one engine that could not be run locally also had no test that would run anywhere else. Found by CI, on all five platforms at once, and now covered by a positive test that runs wherever JAGS is. |
| S36 | **Having \pkg{mcp} is not the same as being able to run it.** With the S34 fix in, `mcp_wrapper()` still failed on macOS and Windows with `subscript out of bounds` inside `summary()` | The wrapper guarded on `requireNamespace("mcp")`, and its own documentation asserted that "if JAGS is missing \pkg{mcp} will not install at all" — which is false. \pkg{rjags} installs on Windows and only fails when it looks for the JAGS library at run time; on the macOS ARM runner `brew install jags` did not make it reachable either. In that state `mcp::mcp()` *warns* and returns an `mcpfit` carrying no posterior, so the failure surfaced several frames later. The wrapper now checks the invariant — a fit either has samples or it is not a fit — and names JAGS. The example is `\dontrun{}`, because no test of installed R packages predicts whether a system library can be reached, and the test skips on the same condition rather than asserting a system library is present. |
| S35 | `cpt_scale_space()` demanded an engine before looking at the input | `need_pkg("mosum")` ran before the univariate check, so a matrix passed with `method = "mosum"` produced "install mosum" on a machine without it and "mosum is univariate" on a machine with it — an error that depends on the library rather than the call. Shape is validated first now. Surfaced as a macOS CI failure, where `mosum` happened not to be installed. |


## 0.9 What Part II still leaves open

Part II's six waves are complete except for six items, five deliberately
left and one waiting on an upstream fix:

1. **`cpt_test()` for engines with no native test** still has no
   selection-adjusted route in-package, because `ChangepointInference` is
   GitHub-only. §18's registration mechanism reaches it; the vignette says
   so.
2. **The performance table** (§23) — benchmarking all 50 methods at
   n = 10⁴/10⁵/10⁶ and publishing the runtimes — is measurement work, not
   code, and belongs with the software paper. **Chunked detection for very
   long series**, proposed in the same section, is also not here: nothing in
   0.5.0 needed it, and doing it without the runtime table would be guessing
   at where the cliff is.
3. **A scheduled CI job installing every suggested engine** (§23). Worth
   doing and not done here.
4. **The remaining §17 engines** — `jointseg`, `segMGarch`, `offlineChange`,
   `onlineCOV`, `BayesProject`, `mbsts`, `bcpa`, `VARcpDetectOnline` — are
   domain-specific and were sequenced last; none is a capability class the
   package now lacks. Likewise §13's peak-detection engines
   (`PeakSegOptimal`, `PeakSegDisk`, `FLOPART`): the supervised *framework*
   is here, and those three would extend it to a new audience rather than
   to a new idea.
5. **The `covr` badge** (§23) needs a coverage service configured for the
   repository, which is infrastructure rather than package work.
6. **`pilliat` is unavailable at power-of-two dimensions** for as long as
   `HDCD` 1.1 is the current release (S23). The wrapper refuses those
   dimensions rather than returning the engine's degenerate answer, and the
   refusal lifts by itself once a fixed `HDCD` is installed — but the fix
   has to happen upstream, and reporting it there is the outstanding
   action. `esac`, the other `HDCD` method, is unaffected.

**The recommendation for 0.6.0 is Part II's own open question 8: freeze.**
0.4.0 and 0.5.0 together took the API from 39 exported objects to 130 in two
cycles. A release that adds no engines and only stabilises — the performance
table, the scheduled CI matrix, the software paper, `inst/CITATION` already
in place, a deprecation policy, and the 1.0 contract freeze including the
`regions` and `diagnostics` slots — is worth more than a third wave.

---

# Part I — The ledger: what 0.4.0 shipped

## 1. Status: where 0.3.0 landed

0.3.0 delivered documentation parity (README/vignettes/help cover all ~40
exports), the honest dispatcher (13 wired methods, planned ones listed in
`cpt_methods()` instead of erroring at runtime), the completed S3 surface,
and fixes B1–B11 from the 0.2.0 audit (the broken `signal_blocks()` loop,
`recall > 1` in `cpt_metrics()`, silent `change_in` mislabelling, flattened
multivariate `ecp` input, misplaced `stat_changepoint()` rules, and others).

What 0.3.0 explicitly deferred — and what defined 0.4.0:

- **The method backlog.** Thirteen methods were listed as *planned*:
  `smuce`, `hsmuce`, `kcp`, `cpm`, `robust`, `decafs`, `sn`, `inspect`,
  `sbs`, `bcp`, `bocpd`, `strucchange`, `segmented`.
- **`geom_cpt_ci()` had no producer.** The CI geom shipped in 0.2.0, but no
  engine emitted `ci_lower`/`ci_upper`.
- **Multivariate input was routed but not rendered.** `ecp` stopped being
  flattened, but `autoplot()` still drew only the first coordinate.
- **Orchestration.** No batch/panel loop, no stability diagnostics, no
  penalty-path tooling.

**Where 0.4.0 landed.** Eleven of the thirteen planned methods are wired. The
two that are not are `robust`, whose engine has never been on CRAN, and `sbs`,
which now waits only on a wrapper (§5.4). Otherwise every deferral is
closed: `geom_cpt_ci()` has four producers, multivariate results
facet, and the orchestration layer exists. `R CMD check --as-cran` is clean on
R 4.4.1 (x86_64 Linux), on R 4.6.0 against a library holding the Imports and
**none** of the Suggests, and on GitHub Actions across ubuntu (devel, release,
oldrel-1), macOS and Windows. `cran-comments.md` is written; the tree is
submission-ready.

## 2. The 2026 method survey

A nine-area literature-and-CRAN sweep (penalised/optimal, multiscale,
nonparametric/kernel, Bayesian, high-dimensional, regression breaks,
online/sequential, robust/dependence, and recent benchmarking work,
2020–2026) was run in July 2026 with every citation and CRAN status
verified. Its actionable conclusions:

1. **Confidence statements are now table stakes.** SMUCE (Frick, Munk and
   Sieling, 2014) and HSMUCE (Pein, Sieling and Munk, 2017) give
   simultaneous confidence sets; Bai–Perron (`strucchange`) and `segmented`
   give break-date CIs. A visualisation-first package that cannot draw an
   interval around a changepoint is behind the field.
2. **The Bayesian pillar is mandatory.** Barry–Hartigan (`bcp`), BOCPD
   (Adams and MacKay, 2007; `ocp`), and the widely used BEAST ensemble
   (`Rbeast`) are all on CRAN and produce the field's signature graphics
   (posterior profiles, run-length heatmaps) — exactly this package's remit.
3. **Slope changes deserve an exact engine.** `cpop` (Fearnhead, Maidstone
   and Letchford, 2019; JSS software paper 2024) is on CRAN and is the
   canonical answer to the `change_in = "slope"` request that 0.3.0 could
   only route to NOT's contrast.
4. **fastcpd is the notable newcomer.** Li and Zhang's `fastcpd` (2024)
   reached CRAN 1.0.0 in 2026 and covers mean/variance/GLM/ARMA/GARCH
   families under one PELT-style interface — the survey's highest-priority
   "new package to not miss".
5. **Dependence-aware methods prevent the classic false positive.** DeCAFS
   (drift + AR noise), SNSeg (self-normalisation), EnvCpt (changepoints vs
   trends vs memory), and NP-MOJO (`CptNonPar`, nonparametric under serial
   dependence) are all on CRAN and address the most common practical failure
   of naive mean-shift detection.
6. **CRAN availability rules out some 0.3.0 plans — but fewer than first
   thought.** `gfpop` was removed from CRAN (2024) and remains GitHub-only;
   `robseg` and `FOCuS` were never on CRAN. None of those three can live in
   the `Suggests` of a CRAN package, so they are deferred and `cpt_methods()`
   says so honestly. **`hdbinseg` is the exception**: it was archived when the
   0.3.0 plan was written, but the refresh found 1.0.3 (Cho) live on CRAN
   again, so `sbs` waits only on a wrapper and not on the archive —
   `planned_methods()` records exactly that distinction (`"next release"`
   versus `"when on CRAN"`). `bcp` and `cpm` are on CRAN as of mid-2026 (both
   had brief archival episodes historically; pin versions if that recurs).
7. **Evaluation conventions have settled.** van den Burg and Williams
   (2020) covering/F1 under one-to-one matching is the benchmark standard —
   which the audit (§5.1, C12–C15) shows the 0.3.0 metrics module implemented
   inconsistently.

## 3. The 0.4.0 engine wave

Eighteen new wrappers, all engines on CRAN, all in `Suggests` behind
`requireNamespace()` guards. `cpt_detect()` grows from 13 to 31 methods; the
`method × change_in` capability matrix is validated centrally and errors —
never silently substitutes.

| Family | Method (`cpt_detect` name) | Engine | Wrapper | Distinctive output |
|---|---|---|---|---|
| Multiscale inference | `smuce`, `hsmuce` | `stepR` | `smuce_wrapper()` | **CI columns** + step fit |
| Penalty path | (path object) | `changepoint` | `cpt_crops()` | `ggcpt_path`: elbow/path/segmentation plots |
| Slope | `cpop` | `cpop` | `cpop_wrapper()` | broken-line fit in `$data$fitted` |
| Bayesian | `bcp` | `bcp` | `bcp_wrapper()` | `posterior_prob` column, posterior mean |
| Bayesian online | `bocpd` | `ocp` | `bocpd_wrapper()` | run-length posterior (`ggcpt_runlength()`) |
| Bayesian ensemble | `beast` | `Rbeast` | `beast_wrapper()` | `posterior_prob`, mean trend |
| Sequential | `cpm` | `cpm` | `cpm_wrapper()` | `detection_time` column |
| Kernel | `kcp` | `kcpRS` | `kcp_wrapper()` | running-statistic changes (mean/var/AR/cor) |
| NP + dependence | `npmojo` | `CptNonPar` | `npmojo_wrapper()` | distribution changes under serial dependence |
| Drift + AR | `decafs` | `DeCAFS` | `decafs_wrapper()` | estimated signal in `fitted` |
| Self-normalised | `sn` | `SNSeg` | `sn_wrapper()` | mean/var/acf/correlation changes |
| High-dim | `inspect` | `InspectChangepoint` | `inspect_wrapper()` | `strength` column, multivariate facets |
| High-dim online | `ocd` | `ocd` | `ocd_wrapper()` | `declared_at`, auto baseline handling |
| Multivariate | `geomcp` | `changepoint.geo` | `geomcp_wrapper()` | `mapping` column (distance/angle) |
| Regression | `strucchange` | `strucchange` | `strucchange_wrapper()` | Bai–Perron breaks + **CI columns**; formula input |
| Broken line | `segmented` | `segmented` | `segmented_wrapper()` | kink CIs + fitted broken line |
| Model selection | `envcpt` | `EnvCpt` | `envcpt_wrapper()` | changepoints only if they beat trend/AR models |
| Modern PELT | `fastcpd` | `fastcpd` | `fastcpd_wrapper()` | mean/var/meanvar + AR/ARMA/GARCH families |

Cross-cutting dispatcher work that shipped with the wave:

- **Central capability validation.** `method_change_in_support()` is the
  single source of truth; `change_in = "slope"` now routes to `cpop` or
  NOT's linear contrast, `"var"` to NOT's meanvar contrast, etc. The 0.3.0
  blanket "slope not supported" error is gone.
- **Multivariate routing.** Univariate methods **error** on wide input
  (previously: silent column-major flattening, §5.1 C3); multivariate methods
  (`ecp`, `inspect`, `geomcp`, `ocd`, `npmojo`, `kcp`, `fastcpd`, `sn`)
  receive the matrix intact and store a `data_wide` slot that `autoplot()`
  renders as facets.
- **Penalty resolution.** Character penalties resolve through
  `cpt_penalty()` for numeric-penalty engines, including `"None"` → 0
  (§5.1 C10); `cpt_penalty("sSIC")` now implements the actual strengthened
  SIC \(k(\log n)^\alpha\) (§5.1 C11).
- **A shared `ggcpt_build()` constructor** guarantees the contract (sorted,
  deduplicated, in-range `cp`; aligned extra columns; segments; optional
  `fitted` signal and `data_wide`) for every new wrapper.

## 4. New features beyond detectors

- **Uncertainty-aware `autoplot()`.** `show_ci = TRUE` draws
  changepoint-location intervals (SMUCE/HSMUCE, strucchange, segmented) as
  whiskers; `show_fit = TRUE` overlays the engine's fitted signal (SMUCE,
  DeCAFS, CPOP, segmented, bcp, BEAST); multivariate results facet
  automatically. `geom_cpt_ci()` finally has producers — and was migrated
  off the deprecated `geom_errorbarh()`.
- **Bayesian displays.** `ggcpt_posterior()` (series + posterior mean above,
  per-location changepoint probability below) and `ggcpt_runlength()` (the
  BOCPD run-length heatmap). Both are a single `ggplot` faceted on a `panel`
  column — the package's two-panel idiom needs no `patchwork` dependency.
- **`cpt_crops()` and the `ggcpt_path` class.** The CROPS penalty path with
  `print()`, `tidy()`, and `autoplot(type = c("elbow", "path",
  "segmentations"))` — penalty selection as a diagnostic, not a guess.
- **`cpt_batch()`.** One detector over many series (matrix/data frame/list),
  returning a `ggcpt_batch` tibble with list-columns, `tidy()`, faceted
  `autoplot()`, and `future` parallelism with reproducible RNG.
- **`cpt_stability()`.** Segment-preserving bootstrap re-detection
  frequencies — a model-agnostic confidence signal for the many engines with
  no native intervals; renders as a frequency profile.
- **`ggcpt_interactive()`.** Any result as a `plotly` widget (`Suggests`);
  the static path is untouched.
- **`cpt_cite()`.** The verified methodological reference(s) behind any
  result or method name, for every wired method (test-enforced).

## 5. The 0.4.0 bug audits (verified)

Three audit passes preceded submission. Every claim was reproduced on
R 4.4.1 before being fixed, and every fix carries a regression test:
**C1–C20** from the first pass (`tests/testthat/test-040-bugfixes.R`) and
**R1–R66** from the pre-release and final pre-submission passes
(`test-040-polish.R`, `test-040-tools.R`, `test-040-wrappers.R`). NEWS.md
itemises all eighty-six. §5.1 records the first pass in full because it is
what shaped the release; §5.2 and §5.3 summarise the two later passes, whose
detail lives in NEWS.md rather than being duplicated here.

### 5.1 First pass: C1–C20

#### Correctness — wrong results

- **C1 (critical). `ecp_wrapper()` fabricated changepoints on no-change
  data.** With no changepoints, `estimates = c(1, n+1)` and the positional
  strip `estimates[2:(length-1)]` evaluated `2:1`, returning the reversed
  boundaries — a ghost changepoint at `n` (with `cp_value = NA`) instead of
  the documented empty tibble. Same root cause silently dropped genuine
  changepoints in `e.agglo`'s wrap-around case. Fixed by value-based
  filtering (`estimates > 1 & estimates <= n`).
- **C2 (critical). `wbs_wrapper()` discarded the model selection it
  claimed.** The default branch computed the sSIC selection but read
  `cpt.th[[1]]` — the unrelated threshold selection — and labelled the
  result "sSIC". Fixed to read `cpt.ic$ssic.penalty`; a manual threshold is
  now recorded as the penalty actually used.
- **C3 (major). Univariate wrappers silently flattened matrices.**
  A 100×2 matrix became a 200-point series with meaningless changepoints.
  All univariate paths now error with the list of multivariate methods;
  single-column matrices/data frames still work.
- **C9 (major). `cpt_detect()` did not forward `change_in` to NOT.**
  `method = "not", change_in = "var"` ran the mean contrast and reported
  "mean". Contrasts are now mapped centrally (`var` → `pcwsConstMeanVar`,
  `slope` → `pcwsLinContMean`) and the object reports what ran.
- **C10 (major). `penalty = "None"` silently became the default.** For
  numeric-penalty engines, unmapped strings fell through to `NULL` → engine
  default; `"None"` now resolves to 0.
- **C11 (major). `cpt_penalty("sSIC")` returned half of SIC.** The
  "strengthened" criterion was weaker than BIC, over-detecting wherever
  used; now \(k(\log n)^{1.01}\).
- **C18 (major). `signal_blocks()` used absolute levels for the
  Donoho–Johnstone jumps.** The classic signal is the cumulative sum of the
  jump heights; benchmarks against the literature were scored against the
  wrong signal.
- **C19 (minor). t-noise had sd ≈ 1.73×`sd`.** `rt(n, df) * sd` is not
  sd-`sd` noise; now rescaled by \(\sqrt{df/(df-2)}\), with `df <= 2`
  rejected.

#### Contract violations and crashes

- **C4 (major).** `idetect_wrapper()` errored on no-change data
  ("No change-points found...") instead of returning the documented empty
  result. Normalised.
- **C5 (major).** `tguh_wrapper()` reported a spurious changepoint on
  constant data (breakfast's default "lp" selector), crashed on short
  series (empty `cptmodel.list`), and mishandled the scalar-0 sentinel
  (producing a corrupt two-segment/zero-changepoint object). Selector pinned
  to "ic"; both edge cases handled in a shared `breakfast_cpts()`.
- **C6 (major).** `glance()` on an fpop result returned **n rows** —
  `$fit$cost` is a length-n vector and tibble recycled every column. Also
  `$` partial matching grabbed DeCAFS's `costFunction`. Exact `[[`
  subsetting + terminal-cost extraction; glance is one row always.
- **C7/C8 (major).** `mosum_wrapper()` stored the threshold *type string*
  as the numeric penalty value (breaking `bind_rows` over glances), and its
  documented `multiscale` argument was ignored. Both fixed;
  `multiscale = TRUE` now calls `mosum::multiscale.localPrune()`.
- **C14 (minor).** `cpt_metrics()` crashed opaquely when an index exceeded
  `n`; now drops out-of-range indices with a warning.
- **C20 (minor).** The signal generators produced nonsense for small `n`
  (negative `length.out`, nine zero changepoints, `n` itself as a
  changepoint); all now validate their minimum sizes.

#### Metric semantics (van den Burg–Williams alignment)

- **C12 (major).** Both-empty pred/truth scored precision = recall = F1 = 0
  — punishing a perfect "no changepoints" answer while covering and Rand
  said 1. Now 1 across the board.
- **C13 (major).** Empty predictions scored covering 0, though the induced
  trivial partition has a well-defined (positive) covering; and the ARI
  guard returned 1 for chance-level agreement (`index == expected`). Both
  corrected.
- **C15 (minor).** `ggcpt_eval()` classified many-to-one while
  `cpt_metrics()` matched one-to-one, so the picture contradicted the
  numbers; both now share `match_changepoints()`, and the "Miss" legend
  entry actually renders.

#### Visualisation

- **C16 (major).** `ggcpt_compare(layout = "facet")` errored when no method
  found changepoints, and silently dropped panels for methods that found
  none. Panels now come from the method list.
- **C17 (major).** `stat_changepoint()` detected on data-frame **row
  order** (shuffled rows → different changepoints) and emitted the
  "dropped aesthetics" warning on every build. Now sorts by `x` and
  declares `dropped_aes`.
- Also fixed: `show_ci` was documented but unimplemented (§4 makes it
  real); `geom_cpt_ci()` sat on deprecated `geom_errorbarh()`;
  `autoplot()`'s `...` silently swallowed misspelled arguments (now warns);
  empty `ggcpt` objects produced Inf-limit plots (now a clean error);
  `ggecpplot()` crashed on data-frame input (now plots the first column
  with a message); `cpt_wrapper(cp_method = "SegNeigh")` always errored
  under the default penalty (now falls back to SIC like the dispatcher);
  `print.ggcpt` double-printed its truncation message; `np` results now
  report `change_in = "distribution"` and `meanvar` stays `"meanvar"` in
  the user's vocabulary.

### 5.2 Second pass: the pre-release audit (R16–R25 and unnumbered items)

The C3 multivariate-flattening fix had only reached the search-based
wrappers, so ten of the new univariate wrappers — `smuce`, `cpop`, `bcp`,
`bocpd`, `beast`, `cpm`, `decafs`, `strucchange`, `segmented`, `envcpt` —
still turned a 120×2 matrix into a 240-point series. The rejection now lives
in the shared coercion helper, which is the only place that can go stale
once. Twelve other items were documentation corrections and
edge-case guards.

### 5.3 Third pass: the final pre-submission audit (R26–R66)

The whole exported surface was re-exercised with degenerate,
contract-violating and self-generated input. The items that change an answer
or end a session:

- **`hsmuce` terminated the R session** on series with essentially no
  per-segment noise: `stepR`'s heterogeneous variance estimator does not
  raise a catchable R error there, it aborts the process. Guarded upstream of
  the call.
- **`cpt_stability()` reported an inflated frequency** — it counted
  changepoints rather than replicates and then clipped the overflow, so an
  index covered by half the replicates was shown as 1.00.
- **`ggcpt_interactive()` failed for every multivariate result**, because the
  faceted plot used a column name `plotly` also generates.
- **`glance()` returned zero rows** instead of one for a hand-built result.
- **`$fit` was NULL for the five `changepoint`/`changepoint.np` engines**,
  although it is documented to hold the upstream object.
- **The covering metric was quadratic** in the number of changepoints.
- A redundant `.onLoad()` that re-registered S3 methods into the method
  tables of `base` and `generics` was removed, after verifying that the
  NAMESPACE declarations alone suffice.
- **One documented trap worth repeating here**: `?cpt_detect` now records
  that `changepoint`'s Normal cost and `fpop`'s lambda are compared against
  a *raw* segment cost for a change in mean, so those four engines
  over-segment badly on data whose noise is much wider than one unit unless
  the series is standardised. Behaviour is unchanged; the trap was simply
  undocumented.

Two test-hygiene lessons from the R-devel run are worth carrying forward:
assert the **mechanism, not a platform-dependent number** (R65 pinned
"`RSS.triang` is the largest component of a `strucchange` fit" after the
80%-share version failed on R-devel's different object accounting), and keep
example runtimes clear of the CRAN timing check even when the time is the
engine attaching `doParallel` rather than the example doing work.

### 5.4 Deferred engines

Deferred purely for CRAN availability, and tracked in `cpt_methods()` as
`planned` with a `target_release` that says which kind of wait it is:

| Method | Engine | Waiting on |
|---|---|---|
| `gfpop` | `gfpop` | CRAN (removed 2024, GitHub-only) |
| `robust` | `robseg` | CRAN (never published there) |
| `focus` | `FOCuS` | CRAN (never published there) |
| `sbs` | `hdbinseg` | **a wrapper, not CRAN** — 1.0.3 is live |

If the first three return to CRAN they slot into the existing wrapper pattern
in an afternoon. `sbs`/`dcbs` is now ordinary roadmap work (§17), and §18's
registration mechanism would make the first three reachable without a
`Suggests` entry at all.

## 6. Architecture, dependencies, testing

- **Imports unchanged** (`changepoint`, `changepoint.np`, `ecp` +
  tidyverse/broom/ggplot2 infrastructure). Eighteen engines + `plotly`,
  `future` and `future.apply` joined `Suggests`. Nothing not-on-CRAN is
  referenced.
- **File layout:** new `wrap-inference.R` (stepR), `wrap-bayes.R`
  (bcp/ocp/Rbeast), `wrap-nonparam.R` (cpm/kcpRS/CptNonPar),
  `wrap-robust.R` (DeCAFS/SNSeg), `wrap-regression.R`
  (strucchange/segmented/EnvCpt), `wrap-highdim.R` (inspect/ocd/geomcp),
  `wrap-slope.R` (cpop), `wrap-fastcpd.R`, `crops.R`, `batch.R`,
  `stability.R`, `cite.R`, `posterior-plots.R`, and the shared
  `ggcpt-build.R`. Twenty-six wrapper functions now live across twelve
  files.
- **Testing:** nine test files — per-wrapper contract tests behind
  `skip_if_not_installed()`, tool tests (CROPS/batch/stability/cite/
  posterior plots/autoplot extensions), and a regression test per audit item
  (C1–C20, R1–R66). The suite passes with all engines installed and with
  none; it also passes under `LC_ALL=C`, against `ggplot2` 3.5.2 and 4.0.3
  (both ends of the declared `>= 3.4.0` floor), and on R 4.6.0.
- **Docs:** every new export has runnable `@examples` (guarded with
  `@examplesIf`), the README and the pkgdown reference index cover the full
  surface (enforced by the existing coverage test), and the vignettes are
  written in research-paper format.
- **Citation hygiene is test-enforced, not aspirational.** `cpt_cite()` must
  cover every method `cpt_methods()` reports as `available`; every
  `\insertRef` key in `R/` must resolve in `inst/REFERENCES.bib`; and a key
  defined in both `inst/REFERENCES.bib` and `vignettes/vignette_reference.bib`
  must describe the same publication (year and journal), which is how the
  TGUH 2018/2022 and ecp preprint/JSS discrepancies were caught.

## 7. Backward compatibility

- The full 0.3.0 surface keeps working unchanged; the
  `tibble(cp, cp_value)` contract is untouched (new columns are additive).
- Two behaviour changes are deliberate bug fixes, not breaks: univariate
  methods error on wide matrices instead of flattening (C3), and
  `cpt_detect(change_in = ...)` mappings that silently ran something else
  now either run the right thing or error (C9). No correct program is
  affected.
- `validate_method_change_in()` accepts `change_in = "mean"` for every
  method (routing it to the method's native change type), so all existing
  single-argument calls keep working.
- No exported function is deprecated in this cycle.

---

# Part II — The roadmap: 0.5.0 and beyond

*A research- and CRAN-grounded feature catalogue for the releases after 0.4.0.
This is a design and literature document, not a changelog and not a
commitment. §9 is a method-and-package survey with CRAN availability verified
against the live CRAN index (July 2026), §10–§23 turn it into concrete,
API-level proposals, and §24 prioritises them. No package code is changed by
this part.*

## 8. Where the package stands, and what is left

### 8.1 The surface today

0.4.0 is a large surface: **31 wired detection methods** behind
`cpt_detect()`, the `ggcpt` S3 class with `broom` methods
(`tidy`/`glance`/`augment`) plus `summary`/`as_tibble`/`as.data.frame`/
`format`/`print`/`plot`, `autoplot()` with `show_ci`/`show_fit`/multivariate
facets, four composable layers (`geom_changepoint()`, `geom_cpt_segment()`,
`geom_cpt_ci()`, `stat_changepoint()`), and the tooling around detection:
`cpt_crops()` (penalty path + `ggcpt_path`), `cpt_batch()`,
`cpt_stability()`, `cpt_metrics()`/`cpt_metrics_annotated()`, the
`ggcpt_compare()` family (`ggcpt_compare()`, `ggcpt_compare_table()`,
`ggcpt_eval()`), `cpt_simulate()` with five canonical signals and four noise
models, `ggcpt_posterior()`, `ggcpt_runlength()`, `ggcpt_interactive()`,
`cpt_cite()` and `cpt_methods()` introspection.

### 8.2 What that means for this part

The obvious gap ("not enough methods") is closed. The remaining gaps are of
three kinds, and the proposals below are organised around them:

1. **Inferential gaps** — the package can draw a CI for the four methods whose
   engines supply one (SMUCE/HSMUCE via `stepR`, `strucchange`, `segmented`)
   but has no *general* answer to "is this changepoint real?" (§10, §11, §12).
2. **Grammar gaps** — a visualization-first package that cannot draw the
   *test statistic*, the *solution path*, the *scale space*, or a
   changepoint-as-interval is leaving its own specialty on the table (§10,
   §14).
3. **Boundary gaps** — everything outside a bare numeric vector: dates,
   streams, functional data, networks, external detectors, real-world events
   (§15–§18, §21).

### 8.3 Verified against the source: proposals that are already done

Every proposal in this part was re-checked against the 0.4.0 code before the
merge. Seven were already implemented, or rested on a description of the code
that is no longer accurate. They are recorded here and **removed from the
themes** so nobody schedules them twice.

| Proposal as written | What the source actually says | Where |
|---|---|---|
| "Correct `cpt_methods()`'s `planned` status for SBS — `hdbinseg` is live on CRAN, not archived" | **Already corrected.** `planned_methods()` gives `sbs` a `target_release` of `"next release"`, against `"when on CRAN"` for `gfpop`/`robseg`/`focus`, and the comment above it records that 1.0.3 is back. Only the wrapper is outstanding, and that is §17's work. | `R/detect.R:225–237` |
| "Add non-iid noise to `cpt_simulate()`; the package ships dependence-aware engines but simulates only the conditions where they are unnecessary" | **Already shipped and the premise is wrong.** `noise = c("gauss", "t", "ar1", "rw")` covers heavy tails (with the variance rescaled so the sd is exactly `sd`, and `df <= 2` rejected), AR(1) with a `rho` guard, and a random walk; piecewise-constant heteroscedasticity comes from `change_in = "var"`/`"meanvar"`. What is genuinely missing is narrower: a `seasonality` argument, *smoothly varying* (non-piecewise) variance, and the power functions. §22 asks only for those. | `R/simulate.R:39–47, 127–142` |
| "Add a CI check that every wired method has a bib entry" | **Already two tests.** One asserts `cpt_cite()` covers every method `cpt_methods()` calls `available`; the other (R44) asserts every `\insertRef` key in `R/` resolves in `inst/REFERENCES.bib` and that keys shared with the vignette bibliography describe the same publication. | `tests/testthat/test-040-tools.R:58`; `test-040-polish.R:628` |
| "Compose the statistic panel below the series with `patchwork`, or keep `ggcpt_posterior()`'s manual grob approach" | **Neither is the status quo.** `ggcpt_posterior()` is a single `ggplot` over a long tibble with a `panel` column, faceted with `facet_grid(panel ~ ., scales = "free_y")`. §14 can reuse that idiom for zero new dependencies, and `patchwork` drops out of the plan. | `R/posterior-plots.R:38–55` |
| "A `regions` slot would be the first change to the `ggcpt` contract since `ggcpt_build()` was introduced" | **The precedent already exists.** `ggcpt_build()` attaches an optional `data_wide` slot (and an optional `fitted` column) only when the engine supplies one, and `autoplot()` tests for it before using it. An additive, defaulted-off `regions` slot is the same pattern, which lowers §10's risk and largely answers open question 1 (§25). | `R/ggcpt-build.R:13, 46, 61`; `R/autoplot.R:61` |
| "`Suggests: future.apply`" for the benchmark harness (§20) | Already in `Suggests` alongside `future`; only `progressr` would be new. | `DESCRIPTION` |
| "26 wrapper files with a shared shape" | Twenty-six wrapper *functions* across twelve files. The duplication argument (§23) stands unchanged — the count was of the wrong noun. | `R/wrap-*.R`, `R/changepoint.R`, `R/ecp.R` |

Two smaller corrections carried into the text below: there is no
`cpt_compare()` (the exports are `ggcpt_compare()`, `ggcpt_compare_table()`
and `ggcpt_eval()`), and `ggcpt_build()` is internal while the exported
`new_ggcpt()` is a bare, non-validating constructor — which is exactly why
§18's `as_ggcpt()` has to be a distinct, validating entry point rather than an
alias.

### 8.4 Constraints to preserve

Engines live in `Suggests` behind `requireNamespace()`; `ggcpt_build()` is the
single constructor enforcing the result contract; the capability matrix errors
rather than silently substituting; `{Rdpack}`-backed citations for every
method, test-enforced; all 31 methods keep working.

## 9. The survey refreshed and CRAN-verified

The 0.4.0 survey (§2) covered nine method areas. This refresh does two things
it did not: it checks **CRAN availability against the live index** rather than
secondary sources, and it looks specifically for capability *classes* the
package cannot express at all. Versions are as of July 2026.

### 9.1 CRAN-live engines that are not yet wired

| Package | Version | What it adds that 0.4.0 cannot do | Theme |
|---|---|---|---|
| **`nsp`** | 1.0.0 | Narrowest Significance Pursuit (Fryzlewicz, *JASA* 2024): **intervals each guaranteed to contain ≥1 changepoint** at a global significance level; works under heavy tails, heteroscedasticity (self-normalisation) and autoregression; general linear models | §10 |
| **`changepoint.influence`** | 1.0.2 | Wilms, Killick & Matteson (*JCGS* 2022) influence diagnostics: delete/outlier perturbation, location & parameter stability, influence maps | §12 |
| **`crossvalidationCP`** | 1.1 | Order-preserved cross-validation (COPPS) for **choosing the number of changepoints** — Zou, Wang & Li (*AoS* 2020). (Note: `cpss`, the authors' own package, was **removed from CRAN**; this is the live route) | §11 |
| **`penaltyLearning`** (+ `PeakSegOptimal`, `PeakSegDisk`, `FLOPART`) | 2024.9.3 / 2024.10.1 / 2024.10.1 / 2024.6.19 | **Supervised** changepoint detection: labelled regions, max-margin interval regression for penalty learning, label-error curves, constrained peak models | §13 |
| **`hdbinseg`** | 1.0.3 | Cho's SBS/DCBS high-dimensional binary segmentation — live on CRAN, which is why `sbs` is a wrapper task and not an archive wait (§5.4) | §17 |
| **`HDCD`** | 1.1 | ESAC (Moen, Glad & Tveten 2023) sparsity-adaptive high-dimensional detection, plus Pilliat et al. (2023); a genuinely different regime from `inspect` | §17 |
| **`changepoints`** | 1.1.0 | Xu/Wang/Yu/Rinaldo collection: high-dimensional **covariance**, **networks (incl. missing values)**, high-dimensional **regression**, and **VAR1** changepoints, offline and online | §17 |
| **`fChange`** | 2.1.0 | **Functional** time series changepoints: mean, covariance, eigen-structure, trace, projections, robust means | §17 |
| **`KWCChangepoint`** | 0.2.3 | Robust changepoint detection for functional *and* multivariate data, incl. covariance structure | §17 |
| **`fabisearch`** | 0.0.4.5 | Changepoints in the **network structure** of high-dimensional series (NMF-based); network-graph outputs | §17 |
| **`VARcpDetectOnline`** | 0.2.1 | Sequential detection for high-dimensional **VAR** models | §16, §17 |
| **`mcp`** | 0.3.4 | Bayesian **formula-based** multiple-changepoint regression (JAGS): posterior distributions on changepoint locations, changes in mean/variance/autocorrelation, Bayes factors, `loo` model comparison | §10, §17 |
| **`bfast`** | 1.7.2 | Breaks for Additive Season and Trend — the standard in remote sensing / land-cover monitoring | §17, §21 |
| **`trend`** | 1.1.7 | Classical single-change tests (Pettitt, Buishand, SNHT, Lanzante) — the hydrology/climatology vocabulary | §17 |
| **`ChangePointTaylor`** | 0.3 | Taylor's change-point analyzer — the quality-control/Six-Sigma audience's default | §17 |
| **`binsegRcpp`** | 2025.5.13 | Fast binary segmentation across many loss functions; useful as a *performance* path | §23 |
| **`wbsts`** | 2.1 | WBS for **nonstationary** time series | §17 |
| **`jointseg`**, **`segMGarch`**, **`offlineChange`**, **`onlineCOV`**, **`BayesProject`**, **`mbsts`**, **`bcpa`** | — | Domain engines (copy-number, GARCH panels, online covariance, Bayesian projection, multivariate BSTS, movement ecology) | §17 |

### 9.2 Not on CRAN — confirmed, and why it matters less than it did

`gfpop` (**removed** from CRAN), `robseg`, `FOCuS`, `cpss` (**removed**),
`Segmentor3IsBack` (**removed**), `changepointsHD`, `ChangepointInference`
(GitHub only — Jewell, Fearnhead & Witten's post-selection inference),
`changeforest` (conda-forge only — Londschien, Bühlmann & Kovács, *JMLR* 2023,
random-forest nonparametric detection with strong benchmark results).

Two of these are *methodologically important and unavailable*, which is the
argument for §18's extension mechanism: `ChangepointInference` is the canonical
implementation of post-detection significance testing, and `changeforest`
reports the best empirical results in the multivariate nonparametric
simulation literature. A package that can only wrap CRAN engines will never
reach them; a package with a registration hook reaches them the moment a user
installs them.

### 9.3 Methodological currents 0.4.0 does not yet reflect

1. **"Post-inference selection" has arrived.** Fryzlewicz's NSP inverts the
   post-selection-inference framing: instead of testing changepoints you
   already chose, it returns **intervals that must each contain a change** at a
   prescribed *global* significance level, with exact coverage regardless of the
   number of regressors, and variants for heavy tails and autoregression.
   Visually this is a different object — a **region**, not a point — and no R
   plotting package draws it. → §10.
2. **Post-detection testing is standard practice.** Jewell, Fearnhead & Witten
   (*JRSSB* 2022) give p-values for a change in mean after binary segmentation,
   ℓ0 segmentation or the fused lasso, conditioning on less information than
   earlier approaches and therefore with more power. → §10.
3. **Selecting K is its own literature.** CROPS (wired) is one answer;
   sample-splitting / order-preserved cross-validation (Zou, Wang & Li, *AoS*
   2020) is another, with consistency guarantees. → §11.
4. **Influence diagnostics exist and are graphical.** Wilms, Killick & Matteson
   (*JCGS* 2022) build exactly the kind of diagnostic plots this package should
   own: which single observations destabilise a segmentation, and where. → §12.
5. **Supervised detection is a whole paradigm we ignore.** Hocking's line of
   work (ICML 2013 penalty learning; constrained DP for peak detection; FLOPART,
   *JCGS* 2024) treats *labelled regions* as the ground truth and *learns* the
   penalty. Labels are drawn as rectangles over the series — a ggplot2-native
   idea with no ggplot2-native implementation. → §13.
6. **Deep learning entered the field.** Automatic changepoint detection via
   deep learning (*JRSSB* 2024) and the 2025 *Frontiers of Engineering
   Management* review establish neural detectors as a real family. We should
   **not** implement them; we should be able to *plot and score* their output.
   → §18.
7. **Anytime-valid sequential detection.** E-detectors (Shin, Ramdas & Rinaldo,
   *NEJSDS* 2023) give nonparametric sequential detection with non-asymptotic
   average-run-length control — the right modern framing for the online engines
   the package runs in batch-replay mode. No R package. → §16 (exploratory).
8. **Evaluation conventions are settled but under-served.** van den Burg &
   Williams (2020) covering/F1 under one-to-one matching plus the **Turing
   Change Point Dataset** are the benchmark standard; `cpt_metrics()` implements
   the metrics, but nothing loads the data or runs the grid. → §20.

---

## 10. Theme A — Inference: intervals, not just points

> **Delivered in 0.5.0** (§0.4): `nsp_wrapper()`, the additive `regions`
> slot, `cpt_regions()`, `geom_cpt_region()`, `autoplot(show_regions =)`,
> `cpt_confint()` with four provenances, and `cpt_test()` with a
> `selection_adjusted` column. Open question 2 was decided against this
> section's `cp = NA` proposal — see §0.6. Not delivered: a
> selection-adjusted test for engines with no native one, which needs
> `ChangepointInference` and therefore §18.

**Problem.** `show_ci = TRUE` works for exactly four methods across three
engines (`smuce`/`hsmuce` via `stepR`, `strucchange`, `segmented`). For the
other 27 the honest answer to "how sure are we about this location?" is
`cpt_stability()` (a bootstrap heuristic) or nothing. Two CRAN-and-literature
routes close this properly, and one of them needs **a new visual grammar**.

**Features.**

```r
# 1. NSP: significance regions, each guaranteed to contain a changepoint
nsp_wrapper(x, alpha = 0.1, method = c("nsp", "nsp_poly", "nsp_selfnorm",
                                       "nsp_ar"), formula = NULL)
cpt_detect(x, method = "nsp", alpha = 0.1)

# 2. the new layer: a changepoint that IS an interval
geom_cpt_region(mapping = NULL, data = NULL, alpha = 0.2, ...)
autoplot(fit, show_regions = TRUE)

# 3. a single generic for "where could this changepoint be?"
cpt_confint(object, level = 0.95, method = c("native", "nsp", "bootstrap",
                                             "posterior"))
#> tibble: cp, ci_lower, ci_upper, source  — one contract, four provenances

# 4. post-detection significance, where an engine supports it
cpt_test(object, type = c("jump", "segment"), correction = "none")
#> tibble: cp, estimate, statistic, p_value, method
```

**Design notes.**

- **`geom_cpt_region()` is the interesting part.** NSP's output is a set of
  intervals, each of which *must* contain at least one change at global level
  α — that is neither a point estimate nor a confidence interval around a point,
  and squeezing it into `ci_lower`/`ci_upper` around a fabricated `cp` would
  misrepresent it. Proposal: add an optional `regions` slot to `ggcpt`
  (`start`, `end`, `alpha`), render it as shaded vertical bands, and have
  `tidy()` return it as a separate element or a distinguishable row type. The
  `cp` column for an NSP fit should either be `NA` or documented as the
  interval midpoint *flagged as such* — never silently presented as an estimate.
- **The slot is additive, and there is precedent.** `ggcpt_build()` already
  attaches `data_wide` and a `fitted` column only when the engine supplies
  them, and `autoplot()` tests for their presence (§8.3). `regions` follows the
  same pattern: optional, defaulted-off, invisible to every existing method.
- **`cpt_confint()` unifies four provenances behind one contract** and must
  report which one it used in a `source` column. Users comparing methods
  currently cannot tell a SMUCE simultaneous confidence set from a bootstrap
  frequency band; the `source` column makes the difference visible in the data,
  not just the docs.
- **`cpt_test()` is where CRAN availability bites.** The canonical
  implementation (`ChangepointInference`) is GitHub-only, so this either waits
  for §18's registration hook or ships only for engines with native tests
  (`strucchange`'s `sctest`, `segmented`'s Davies test, `mcp`'s Bayes factors).
  Ship the generic with the native cases and let §18 supply the rest.
- **`mcp`** deserves a wrapper here as well as in §17: it gives a full
  *posterior distribution* over each changepoint location from a formula
  specification, which `ggcpt_posterior()` already knows how to draw. It is a
  JAGS dependency (system-level), so guard it carefully and keep it out of
  examples that must run on CRAN.

**Deps** `Suggests: nsp`, `mcp`. **Effort** M (`nsp_wrapper`, `cpt_confint`),
M (`geom_cpt_region` + slot), L (`cpt_test` in general). **Risk** medium — the
`regions` slot changes the `ggcpt` contract, so it must be additive and
defaulted-off, and `tidy()`'s row semantics need deciding before it ships.

---

## 11. Theme B — Choosing the number of changepoints

> **Delivered in 0.5.0** (§0.4): `cpt_select()` with six criteria — including
> the genuine Zhang–Siegmund mBIC and COPPS cross-validation — plus the
> criterion, segmentation and **ladder** plots.

**Problem.** Penalty choice is the single most consequential decision a user
makes, and 0.4.0's answers are `cpt_penalty()` (a formula) and `cpt_crops()` (a
path plot). Neither *selects*. The consistency literature has a selection
procedure with guarantees, and it is on CRAN.

**Features.**

```r
cpt_select(x, method = "pelt", criterion = c("crops_elbow", "cv", "bic",
                                             "mbic", "stability"),
           k_max = 20, folds = 2)
#> ggcpt_selection: criterion value per K, chosen K, the fitted result at K̂

autoplot(sel, type = c("criterion", "segmentation", "ladder"))
#> "ladder" = small multiples of the segmentation at each candidate K
```

**Design notes.**

- `criterion = "cv"` wraps `crossvalidationCP` (COPPS: order-preserved
  sample-splitting cross-validation, Zou/Wang/Li). Note that the authors' own
  `cpss` package **was removed from CRAN**, so `crossvalidationCP` is the
  supportable route — and note the removal in `cpt_methods()` so nobody
  re-proposes `cpss`.
- `"crops_elbow"` should reuse `cpt_crops()` and make the elbow *rule* explicit
  and citable rather than eyeballed (maximum-curvature / knee-point on the
  cost-vs-K curve), with the caveat printed.
- **The `"ladder"` plot is the visualization contribution**: a column of small
  multiples showing how the segmentation coarsens as K falls is far more
  informative than a criterion curve, and it is exactly the kind of thing a
  ggplot2-native package should offer. `cpt_crops()` already computes the
  segmentations, so this is a rendering feature over existing data.
- Cross-check with §12: a K chosen by CV but unstable under `cpt_influence()`
  should be reported as such. A combined "selection report" is a natural
  §21 `cpt_report()` section.
- **Mind the cost-scale trap** documented in `?cpt_detect` (§5.3): for the
  `changepoint` and `fpop` engines a mean-change penalty is compared against a
  raw segment cost, so any automatic selection over those engines must either
  standardise the series or report that it did not.

**Deps** `Suggests: crossvalidationCP`. **Effort** M. **Risk** low.

---

## 12. Theme C — Influence, stability and robustness diagnostics

> **Delivered in 0.5.0** (§0.4): `cpt_influence()` with four renderings,
> `cpt_leverage()`, `cpt_sensitivity()`, and both the
> `changepoint.influence` and the generic-recomputation routes.

**Problem.** `cpt_stability()` resamples within segments and reports detection
frequencies. That answers "would I find this again?" but not "**which
observation is driving this?**" — and the second question has a published,
graphical, CRAN-available answer that lands squarely in this package's remit.

**Features.**

```r
cpt_influence(object, type = c("delete", "outlier"), engine = NULL)
#> ggcpt_influence: per-observation effect on the number, locations and
#> segment parameters of the segmentation

autoplot(inf, type = c("overview", "location", "parameter", "map"))
#> ggplot2 renderings of the Wilms/Killick/Matteson diagnostic family

cpt_leverage(object)      # rank observations by influence, tidy tibble
cpt_sensitivity(x, method, over = list(penalty = ..., minseglen = ...))
#> grid over tuning parameters -> heatmap of detected locations
```

**Design notes.**

- Wrap `changepoint.influence` where its interface allows and **re-render its
  diagnostics in ggplot2**. Re-rendering rather than re-exporting is the value
  add: the outputs then compose with the package's own facet idiom, inherit
  `theme_ggcpt()`, and can be laid out alongside `autoplot.ggcpt()`. Cite
  Wilms, Killick & Matteson (2022) via `cpt_cite()`.
- `cpt_sensitivity()` is the parameter analogue of `cpt_influence()`'s data
  perturbation, and it is cheap: `cpt_batch()` already runs a detector many
  times, so this is a grid plus a heatmap. It directly addresses the most common
  reviewer question ("is this robust to the penalty?").
- **Applicability limits matter.** Influence-by-deletion needs a detector that
  can be re-run n times; for expensive engines this is O(n) fits. Provide a
  `subset`/`sample` argument, warn on large n with an estimated runtime, and
  parallelise through the existing `future` support.
- **Some engines cannot be re-run n times safely.** The `hsmuce` session-abort
  case (§5.3) is the warning: a delete-one loop over a guarded engine must
  route through the same guard, not call the engine directly.
- Cross-reference `envcpt` (wired): "are these changepoints or is it a trend /
  autocorrelation?" is a *different* robustness question with a different
  answer, and the docs should route users between the two.

**Deps** `Suggests: changepoint.influence`. **Effort** M–L. **Risk** low
methodologically, medium on runtime ergonomics.

---

## 13. Theme D — Supervised changepoint detection

> **Delivered in 0.5.0** (§0.5): `cpt_labels()`, `as_cpt_labels()`,
> `cpt_label_error()`, `cpt_label_error_curve()`, `cpt_learn_penalty()`,
> `geom_cpt_label()`, `scale_fill_cpt_label()`, penalty-as-model in
> `cpt_detect()`/`cpt_penalty()`/the wrappers, and a dedicated vignette.
> Open question 5 is answered by construction. Not delivered: the
> peak-detection engines (`PeakSegOptimal`, `PeakSegDisk`, `FLOPART`).

**Problem.** An entire paradigm is missing. Hocking's work treats changepoint
detection as **supervised learning from labelled regions**: an expert marks
intervals as "contains a change" / "contains no change", the penalty is
*learned* by max-margin interval regression, and accuracy is measured in
label errors rather than by an information criterion. It is on CRAN
(`penaltyLearning`, `PeakSegOptimal`, `PeakSegDisk`, `FLOPART`), it consistently
beats unsupervised penalties on labelled data, and — decisively for this
package — **its central object is a rectangle drawn over a time series.**

**Features.**

```r
# labels as first-class data
cpt_labels(start, end, change = c("change", "no_change", "one_change"))
geom_cpt_label(mapping, data, ...)      # shaded label regions behind the series
autoplot(fit, labels = my_labels)       # correct / false-positive / false-negative shading

# error accounting against labels
cpt_label_error(object, labels)
#> tibble: label, status (correct | fp | fn), n_changes_in_label
cpt_label_error_curve(x, method, labels, penalties)
#> error vs penalty, with the ROC-style curve penaltyLearning produces

# learned penalties
cpt_learn_penalty(series_list, labels_list, features = NULL)
#> a model mapping series features -> penalty; predict() gives per-series penalties
cpt_detect(x, method = "pelt", penalty = learned_model)
```

**Design notes.**

- **`geom_cpt_label()` may be the single most distinctive new geom available to
  this package.** Nothing in the R visualization ecosystem draws changepoint
  labels, and the three-colour status shading (correct / false positive / false
  negative) turns model evaluation into a picture. It composes with the existing
  `geom_changepoint()` / `geom_cpt_segment()` layers.
- `cpt_learn_penalty()` needs the per-series *feature* step (`penaltyLearning`'s
  feature matrix) and a model; keep it thin, delegate the interval-regression
  fitting, and return an object with a `predict()` method so
  `cpt_detect(penalty = model)` just works. That requires `cpt_penalty()` to
  accept a model object as well as a string or number — a small, additive
  contract change.
- This theme also opens a **new audience**: genomics/epigenomics peak detection
  (`PeakSegOptimal`/`PeakSegDisk`/`FLOPART`) is a large applied community that
  uses changepoint models daily and has no tidy/ggplot2 interface.
- Labels are also *annotations*, which means `cpt_metrics_annotated()` (already
  shipped, van den Burg-aligned) and this theme share a data structure. Unify
  them: one `cpt_labels`/annotation representation used by metrics, plotting and
  penalty learning.

**Deps** `Suggests: penaltyLearning`, optionally `PeakSegOptimal`/`FLOPART`.
**Effort** L. **Risk** medium — the biggest conceptual addition here, and it
needs a vignette of its own to land.

---

## 14. Theme E — Seeing the statistic

> **Delivered in 0.5.0** (§0.4): `cpt_statistic()`, `cpt_solution_path()`,
> `cpt_scale_space()` and their plots, reached from `autoplot(type =)`,
> with per-engine support recorded in the registry and an error naming the
> engines that do expose each one. The faceted-panel idiom was reused, so
> `patchwork` stayed out of `Imports`.

**Problem.** Every detector computes something — a CUSUM path, a MOSUM
statistic at each bandwidth, a set of random intervals with contrast values, a
cost-vs-K curve — and 0.4.0 throws almost all of it away, keeping only the
argmax. For a package whose thesis is "changepoint analysis should be
visual", this is the largest remaining grammar gap.

**Features.**

```r
autoplot(fit, type = c("series", "statistic", "path", "scale_space"))

ggcpt_statistic(object)     # the detector's criterion as a function of location
ggcpt_solution_path(object) # WBS/WBS2/NOT random intervals, binary-segmentation
                            # split order, IDetect expansion
ggcpt_scale_space(object)   # MOSUM/multiscale: statistic × bandwidth heatmap,
                            # with the accepted changepoints overlaid
```

**Design notes.**

- **Scale space is the standout.** MOSUM and the multiscale methods sweep a
  bandwidth; the natural display is a location × bandwidth heatmap with the
  significant regions marked — a picture that immediately explains *why* a
  method found a change at one resolution and not another. This is a genuinely
  novel plot for R and directly serves the "which bandwidth?" question.
- **Feasibility is per-engine and must be checked before promising.** Some
  engines return their internals (`wbs`/`not` expose the interval/contrast
  tables; `mosum` exposes statistics; `changepoint` gives cost via CROPS;
  `bcp`/`beast` give posteriors, already handled). Others return only
  locations. Proposal: add a `diagnostics` slot to `ggcpt`, populate it only
  where the engine supports it, have `ggcpt_statistic()` etc. error with a
  clear "engine X does not expose Y; these do: …" message, and record support
  in `cpt_methods()` as new capability columns. Every wired wrapper now stores
  the upstream object in `$fit` (an R26–R66 fix), so the internals are already
  reachable without changing a single wrapper's return.
- **The name `ggcpt_path` is taken** by `cpt_crops()`. Use
  `ggcpt_solution_path` for the detector's search path; do not overload the
  penalty-path class silently.
- **No new dependency is needed for the two-panel layout.** The
  `"statistic"` panel should compose below the series panel the way
  `ggcpt_posterior()` already does it: one long tibble with a `panel` column
  and `facet_grid(panel ~ ., scales = "free_y")`. One layout language, zero
  added `Suggests`.
- **Test the way R65 taught us to.** Engine internals are the most
  upstream-fragile thing in the package, so assert structure (a table with
  these columns exists; the accepted changepoints appear in it) rather than
  numbers, and skip rather than fail when an engine's shape is unexpected.

**Deps** none. **Effort** M per engine family, L in total. **Risk**
medium — depends on engine internals, which can change upstream.

---

## 15. Theme F — Time indices and data structures

> **Delivered in 0.5.0** (§0.4): `index` on `cpt_detect()`,
> `as_cpt_series()`, direct `ts`/`xts`/`zoo`/`tsibble` input, the
> data-frame interface, an irregular-spacing warning, and the index
> threaded through every output.

*Named as a 0.5.0 item in the 0.4.0 release notes.*

**Problem.** `cpt_detect()` takes a numeric vector and returns integer indices.
Real series have dates. Users currently post-process indices back to dates by
hand, and every plot has an "index" x-axis where it should have years.

**Features.**

```r
cpt_detect(x, index = dates)          # or accept ts / xts / zoo / tsibble directly
tidy(fit)                             # gains cp_index (original scale) alongside cp
autoplot(fit)                         # date axis, correct scale, sensible breaks
cpt_detect(df, y = value, index = date, method = "pelt")   # data-frame interface
```

**Design notes.**

- Store the index once in the `ggcpt` object and thread it through `tidy()`,
  `augment()`, `autoplot()`, `cpt_metrics()` and the `ggcpt_compare()` family.
  Detection itself stays on positions — that is the correct separation, and it
  keeps all 31 engines untouched.
- `autoplot()` already takes an `index` argument for *labelling* (including for
  multivariate results, fix #8), so this theme is about making the index a
  property of the *object* rather than of the plot call, and about accepting it
  at detection time.
- Accept `ts`/`xts`/`zoo`/`tsibble` via a small `as_cpt_series()` coercion with
  `Suggests`-guarded methods (`{tsbox}` can do the conversions if we would
  rather not write them). Handle irregular spacing by warning: most engines
  assume equal spacing, and silently ignoring that is a wrong-answer bug of the
  kind the 0.4.0 audits were full of.
- A **data-frame-first interface** (`cpt_detect(df, y, index)`) is worth
  considering for tidyverse consistency, kept as an additional method rather
  than a breaking change to the vector interface. Note the collision to
  resolve: a bare data frame *already* means "multivariate wide input" to
  `cpt_detect()`, so the column-selecting form has to be distinguished by the
  presence of `y`/`index` rather than by the class of the first argument.
- `cpt_batch()` should carry per-series indices too, so a faceted plot of 50
  series shows dates.

**Deps** `Suggests: xts`, `zoo`, `tsibble`, `tsbox`. **Effort** M. **Risk** low,
and it is one of the most-requested things in every changepoint package's issue
tracker.

---

## 16. Theme G — Streaming and online monitoring

> **Delivered in 0.5.0** (§0.5): `cpt_monitor()`, `cpt_update()`,
> `alarms()`, `cpt_replay()`, `cpt_delay()`, and the native e-detector —
> the deliberate exception to wrap-don't-implement, decided in §0.6.
> Not delivered: `VARcpDetectOnline` and `onlineCOV`.

*Named as a 0.5.0 item in the 0.4.0 release notes.*

**Problem.** `cpm`, `ocd` and BOCPD are *online* methods run in batch-replay
mode. The output the online literature cares about — detection delay, average
run length, the alarm sequence — is not first-class, and there is no way to feed
new observations to an existing fit.

**Features.**

```r
mon <- cpt_monitor(method = "ocd", baseline = x0, ...)   # a stateful detector
mon <- cpt_update(mon, new_obs)                          # push observations
alarms(mon)                                              # tidy alarm log

cpt_replay(x, method = "cpm", ...)     # explicit batch replay -> alarm timeline
autoplot(mon, type = c("timeline", "delay", "arl"))
cpt_delay(object, truth)               # detection-delay distribution
```

**Design notes.**

- A stateful `ggcpt_monitor` object is a genuinely new class, not a variation on
  `ggcpt`; keep it separate with its own `print`/`autoplot`/`tidy`.
- **Detection delay and ARL are the field's currency** and belong next to
  `cpt_metrics()`: for online methods, "did you find the location?" is the wrong
  question and "how long did you take, and how often do you false-alarm?" is
  the right one. `cpt_metrics()` should refuse (or warn) on online results and
  point at `cpt_delay()`. The raw material is already there: `cpm` results
  carry `detection_time` and `ocd` results carry `declared_at`.
- `VARcpDetectOnline` and the `changepoints` package's online functions extend
  this to high dimensions; `onlineCOV` covers online covariance.
- **Exploratory:** e-detectors (Shin, Ramdas & Rinaldo) give nonparametric
  sequential detection with non-asymptotic ARL bounds and have **no R
  implementation**. A small native implementation (Shiryaev–Roberts / CUSUM
  e-detectors with mixtures) would be the package's first original method
  contribution and is publishable alongside the software paper — but it breaks
  the "wrap, don't implement" rule, so treat it as a deliberate, separately
  scoped decision, not a drive-by addition.

**Deps** `Suggests: VARcpDetectOnline`, `onlineCOV`. **Effort** L. **Risk**
medium (statefulness is a testing burden; the e-detector option is a research
task).

---

## 17. Theme H — New data types

> **Delivered in 0.5.0** (§0.3): fourteen of this section's engines, plus
> the four new `change_in` levels and the capability-matrix entries that go
> with them. Not delivered: `sbs`/`dcbs` (hdbinseg is archived again — see
> §0.7) and the domain engines listed in §0.9.

**Problem.** The 31 wired methods cover univariate and (some) multivariate
numeric series. Three data types with active literatures and live CRAN engines
are entirely absent, and two applied vocabularies are missing.

**Features (engine wave #2).**

| Proposed method name | Engine | Adds |
|---|---|---|
| `sbs`, `dcbs` | `hdbinseg` | high-dimensional binary segmentation — **already promised** in `cpt_methods()` as a wrapper task (§5.4), so it is the first item, not a new one |
| `functional`, `fmean`, `fcov` | `fChange` | changepoints in functional time series: mean, covariance, eigenstructure, projections |
| `kwc` | `KWCChangepoint` | robust functional/multivariate covariance changepoints |
| `esac`, `pilliat` | `HDCD` | sparsity-adaptive high-dimensional detection |
| `hdcov`, `network`, `hdreg`, `var` | `changepoints` | high-dim covariance, dynamic networks (incl. missing data), high-dim regression, VAR |
| `fabisearch` | `fabisearch` | changepoints in network *structure* of high-dim series |
| `mcp` | `mcp` | Bayesian formula-based multiple-changepoint regression (see also §10) |
| `bfast` | `bfast` | seasonal + trend break decomposition (remote sensing) |
| `pettitt`, `buishand`, `snht` | `trend` | classical single-change tests (hydrology/climate vocabulary) |
| `taylor` | `ChangePointTaylor` | Taylor's analyzer (quality-control audience) |
| `wbsts` | `wbsts` | WBS for nonstationary series |

**New visual grammar these require.**

- **Functional data**: the series is a curve per time point. The natural display
  is a spaghetti/heatmap of curves coloured by estimated segment, plus the
  segment-mean functions — a new `autoplot()` type, not a variation on the
  existing one.
- **Networks**: `changepoints`/`fabisearch` estimate a graph per segment. Small
  multiples of the segment graphs (via `{ggraph}`/`{tidygraph}` in `Suggests`)
  with an adjacency-difference panel is the display; `fabisearch` has its own
  and it can be improved on.
- **Seasonal/trend (`bfast`)**: the decomposition panels are the point, and the
  `panel`-column facet idiom behind `ggcpt_posterior()` generalises to them
  directly.

**Design notes.**

- **Do not add all of these at once.** The 0.4.0 wave added 18 engines and the
  audits found eighty-six real bugs, a third of them in the new code; a second
  wave of the same size invites the same outcome. Sequence by audience size:
  `hdbinseg` (already promised) → `HDCD`/`changepoints` (high-dimensional,
  closest to what exists) → `mcp` (Bayesian, reuses `ggcpt_posterior()`) →
  `fChange`/`KWCChangepoint` (functional, needs new plots) → applied
  vocabularies (`trend`, `bfast`, `ChangePointTaylor`).
- `change_in` needs new levels (`"covariance"`, `"network"`, `"eigen"`,
  `"seasonality"`) and the capability matrix must be extended in lockstep — it
  is the single source of truth and the reason 0.4.0's routing is honest.
- The applied-vocabulary engines (`trend`, `bfast`, `ChangePointTaylor`) are
  small wrappers with disproportionate reach: they bring hydrologists,
  remote-sensing analysts and quality engineers into a tidy interface using the
  method *names they already know*. Cheap, and good for citations.
- **Every new wrapper inherits the audit checklist**, not just the happy path:
  no-change data returns the documented empty result; multi-column input is
  accepted or rejected deliberately (never flattened); the `$fit` slot holds
  the upstream object; `glance()` returns exactly one row; and any engine that
  can abort rather than error is guarded before the call.

**Effort** M per engine, L for the new plot types. **Risk** medium — each new
data type is a new contract question (what is `augment()` for functional data?),
so each needs a decision, not a default.

---

## 18. Theme I — The extension mechanism

> **Delivered in 0.5.0** (§0.2), and built first as this section argued:
> `as_ggcpt()`, `cpt_register_method()`, `cpt_unregister_method()`,
> `cpt_registered_methods()`, visible labelling everywhere a registered
> method appears, and the `reticulate`/`ruptures` recipe in
> `vignette("extending")`.

**This is the highest-leverage feature in the roadmap.** Three separate
problems have one solution:

1. Methodologically important engines are **not on CRAN** and never will be
   (`gfpop` removed, `robseg`, `FOCuS`, `changeforest` conda-only,
   `ChangepointInference` GitHub-only).
2. **Deep-learning detectors** (*JRSSB* 2024; the 2025 review) and Python tools
   (`ruptures`) produce changepoints that users want plotted and scored with the
   same machinery.
3. Users have **hand-curated or domain-specific changepoints** (an analyst's
   annotations, a proprietary detector's output, a published paper's reported
   breaks) and no way into the `ggcpt` ecosystem.

**Features.**

```r
# turn any set of changepoints into a first-class result object
as_ggcpt(cp, x, fitted = NULL, method = "custom", ci = NULL, ...)

# register an external detector so cpt_detect() can dispatch to it
cpt_register_method(name, fn, change_in, engine, citation, capabilities = list())
cpt_methods()          # shows registered methods, flagged as user-supplied

# example: a GitHub-only engine, or a Python detector via reticulate
cpt_register_method("changeforest",
                    fn = function(x, ...) changeforest::changeforest(x, ...)$split_points,
                    change_in = "distribution", engine = "changeforest")
```

**Why this matters more than another wrapper.** It converts ggchangepoint from
*a collection of 31 wrappers* into **the common representation and visual
grammar for changepoint results in R**. Everything the package already
built — `autoplot()`, the four geoms, `tidy`/`glance`/`augment`,
`cpt_metrics()`, the `ggcpt_compare()` family, `cpt_stability()`,
`ggcpt_interactive()`, `cpt_report()` (§21) — becomes available to detectors
the package does not and cannot depend on. It also permanently defuses the
CRAN-availability problem that §9.2 documents and that the 0.4.0 ledger
(§5.4) has to treat as a hard blocker.

**Design notes.**

- `as_ggcpt()` must run the same `ggcpt_build()` validation as every wrapper
  (sorted, deduplicated, in-range `cp`; aligned extra columns; segments
  derived), so external results cannot violate the contract the audits worked
  to establish. Note that the exported `new_ggcpt()` is a *bare* constructor
  that validates nothing, and `ggcpt_build()` is internal: `as_ggcpt()` is
  therefore a new, validating public entry point, not an alias for either. It
  should also produce the `$fit` and `segments` structure that `glance()` and
  `augment()` assume, since a hand-built object was exactly the case that made
  `glance()` return zero rows (R37).
- Registered methods must be **visibly** user-supplied in `cpt_methods()` and in
  `print.ggcpt()`, and `cpt_cite()` must return the user-supplied citation or
  say plainly that none was given. The package's credibility rests on not
  blurring "we validated this" with "someone plugged this in". The existing
  `status` column already distinguishes `available` from `planned`; a third
  value (`registered`) keeps the introspection table the single source of
  truth, and the test that asserts `cpt_cite()` covers every `available`
  method must be extended, not bypassed.
- Registration is session-scoped state; keep it in an internal environment, make
  it inspectable, and provide `cpt_unregister_method()`. Do not persist it to
  disk.
- Document the `{reticulate}` → `ruptures` recipe in a vignette rather than
  depending on it.

**Deps** none. **Effort** M. **Risk** low technically; the risk is
*reputational* and is handled by labelling.

---

## 19. Theme J — Ensembles, consensus and method choice

> **Delivered in 0.5.0** (§0.5): `cpt_consensus()` reusing
> `cpt_metrics()`'s matching rule, with the agreement matrix, and
> `cpt_recommend()` as a printable decision table.

**Problem.** With 31 methods (and more coming), the user's real question is
"which one?" — and 0.4.0's answer is `ggcpt_compare()`, which shows the
disagreement without resolving it.

**Features.**

```r
cpt_consensus(x, methods = c("pelt", "wbs", "not", "ecp"), tolerance = 5,
              min_votes = 2)
#> ggcpt: consensus changepoints + a votes column + which methods agreed

autoplot(cons, type = c("series", "agreement"))
#> "agreement" = method × location dot matrix / heatmap

cpt_recommend(x, questions = NULL)
#> a guided recommendation: dimension, change type, noise structure, n,
#> need for CIs, need for online -> a shortlist with reasons and citations
```

**Design notes.**

- **Consensus needs a defensible matching rule**, and one already exists in the
  package: `match_changepoints()`, the one-to-one matching under a tolerance
  window shared by `cpt_metrics()` and `ggcpt_eval()` since C15. Reuse it
  rather than inventing a second notion of "same changepoint" — two
  incompatible matching rules in one package would be a correctness bug waiting
  to happen, and C15 is the record of what that costs.
- Be explicit that consensus across methods is **not** an inferential
  procedure: agreement among correlated detectors is not a p-value. Document it
  as a robustness display, cross-referenced to §10 for actual inference.
- `cpt_recommend()` should be a *decision table*, not a model: the `change_in` ×
  dimension × noise-structure × inference-need grid is already implicit in
  `cpt_methods()`, and making it explicit and printable is a documentation
  feature with code-sized value. Pair it with a decision-tree diagram in the
  vignette. This is also the honest place to say "if your noise is
  autocorrelated, `envcpt`/`decafs`/`npmojo` first, not `pelt`" — and to
  surface the standardisation trap for the `changepoint`/`fpop` engines.

**Deps** none. **Effort** M. **Risk** low, provided the "not inference"
framing is prominent.

---

## 20. Theme K — Benchmarking and evaluation

> **Delivered in 0.5.0** (§0.5): `cpt_benchmark()` with per-cell failure
> capture, `cpt_datasets()`, `cpt_load_tcpd()`, `cpt_annotations()`, and
> the critical-difference diagram.

*Named as a 0.5.0 item in the 0.4.0 release notes.*

**Problem.** `cpt_metrics()` implements the right metrics; nothing supplies the
right data or runs the grid. The field has a standard benchmark (TCPD) with
**multiple human annotators per series**, which is exactly the structure
`cpt_metrics_annotated()` was built for.

**Features.**

```r
cpt_benchmark(datasets, methods, metrics = c("covering", "f1"),
              tolerance = 5, parallel = TRUE)
#> ggcpt_benchmark: method × dataset × metric tibble, tidy() + autoplot()

autoplot(bm, type = c("heatmap", "ranks", "critical_difference"))

cpt_datasets()                        # catalogue of available benchmark series
cpt_load_tcpd(...)                    # download + cache the Turing Change Point Dataset
cpt_annotations(dataset)              # per-annotator ground truth, tidy
```

**Design notes.**

- **TCPD must be downloaded, not bundled** (size and licence), cached under
  `tools::R_user_dir()`, and every example/test must run offline against
  `cpt_simulate()` signals instead. Same discipline as any data-fetching
  package.
- `cpt_benchmark()` is mostly composition: `cpt_batch()` × `cpt_metrics()` over
  a grid, with `future` parallelism that already exists. The new parts are the
  result class, the failure handling (an engine that errors on one dataset must
  not kill the grid — record it as `NA` with the message) and the plots. The
  covering metric is no longer the bottleneck it was: it was quadratic in the
  number of changepoints until the final audit pass.
- **Multi-annotator ground truth is the subtle part.** van den Burg &
  Williams's covering metric is defined against a set of annotations; reporting
  a single number against a single "true" set silently discards the
  disagreement. `cpt_metrics_annotated()` handles this — the benchmark harness
  must not regress it.
- A **`critical_difference` plot** (post-hoc ranks across datasets) is the
  standard way this literature summarises "method A beats method B", and it
  would be the package's most quotable figure.

**Deps** `Suggests: progressr` (`future`/`future.apply` are already there).
**Effort** L. **Risk** medium (network + long runtimes; keep it out of
`R CMD check`).

---

## 21. Theme L — Communication

> **Delivered in 0.5.0** (§0.5): `cpt_annotate_events()`,
> `geom_cpt_event()`, `cpt_report()`, `cpt_gt()`, and the accessibility
> pass (Okabe–Ito palette, redundant linetype, generated alt text).

**Problem.** The last mile — turning a detected changepoint into something a
domain reader understands — is unsupported. The one thing every applied user
does by hand is *label the changepoint with what happened*.

**Features.**

```r
cpt_annotate_events(object, events, tolerance = 5)
#> join a data frame of known events (date, label) to detected changepoints;
#> reports matched / unexplained changepoints and unmatched events
geom_cpt_event(...)                   # labelled event markers (ggrepel-aware)

cpt_report(object, format = c("md", "html", "gt"))
#> a reproducible summary: method + citation, penalty, K, locations with CIs,
#> segment table, stability, diagnostics, sessionInfo

cpt_gt(object)                        # publication-ready segment table
```

**Design notes.**

- **`cpt_annotate_events()` is the applied-user feature.** "Changepoint at index
  147" is not a finding; "changepoint at 2020-03-11, matching the WHO pandemic
  declaration" is. The three-way output (matched, unexplained changepoint,
  undetected event) is also a *validation* device: unexplained changepoints are
  where the interesting analysis starts. It reuses `match_changepoints()`
  (§19) and it wants §15's index support to be genuinely useful, so sequence it
  after that.
- `cpt_report()` should lean on `{gt}`/`{rmarkdown}` in `Suggests` and degrade
  to plain markdown. Include `cpt_cite()` output automatically — reproducibility
  and correct attribution in one artifact.
- **Accessibility and defaults deserve a pass**: `theme_ggcpt()` and the default
  changepoint styling (currently a solid blue vertical line, i.e. a
  colour-only distinction from the series) should be checked for colour-vision
  deficiency, `autoplot()` should support pattern/linetype redundancy rather
  than colour-only encoding, and every vignette figure needs alt text. Cheap,
  and the right thing for a visualization package.
- `{ggiraph}` alongside the existing `{plotly}` path would give tooltip-level
  interactivity that composes with faceted and multi-panel plots more
  predictably than `plotly` does — the multivariate `ggcpt_interactive()` bug
  in the final audit (a column name `plotly` also generates) is a symptom of
  how thin that layer's contract is.

**Deps** `Suggests: gt`, `ggrepel`, `ggiraph`. **Effort** M. **Risk** low.

---

## 22. Theme M — Simulation, power and study design

> **Delivered in 0.5.0** (§0.5): `cpt_power()`, `cpt_min_detectable()`,
> `cpt_scenarios()`, and `cpt_simulate()`'s `seasonality` and `sd_trend`.

**Problem.** `cpt_simulate()` ships five canonical signals (blocks, fms, mix,
stairs, teeth — the Donoho–Johnstone/Fryzlewicz test set) and four noise models
(Gaussian, sd-exact Student-t, AR(1), random walk), which is enough to
demonstrate the dependence-aware engines honestly. What it cannot do is answer
"would I even be able to detect a change this small?", which is the question
that should precede an analysis.

**Features.**

```r
cpt_power(n, jump, sigma, method, n_sim = 200, ...)
#> detection probability, location error and false-positive rate by scenario
autoplot(pow)                       # power curves: jump size × segment length

cpt_scenarios(...)                  # a grid of simulation settings as data
cpt_min_detectable(n, sigma, method, power = 0.8)

cpt_simulate(..., seasonality = NULL, sd = <vector or function>)
#> the two simulation gaps that remain: a seasonal component, and variance
#> that varies smoothly rather than piecewise (`change_in = "var"` covers the
#> piecewise case already)
```

**Design notes.**

- `cpt_power()` is `cpt_batch()` over simulated replicates plus `cpt_metrics()`;
  the work is the result class, sensible defaults and honest reporting of Monte
  Carlo error. Reproducible RNG through `future` already exists.
- `cpt_min_detectable()` is the *pre-registration* / study-design version and is
  the kind of function that gets cited in applied papers.
- **A power study over the `changepoint`/`fpop` engines has to standardise or
  say so.** Their mean-change penalties are compared against a raw segment
  cost, so a power curve swept over `sigma` measures the interaction of jump
  size with the penalty scale unless the series is standardised first. This is
  the documented trap from §5.3, and it is exactly where it would bite hardest.
- Keep the noise vocabulary the one that already ships (`"gauss"`, `"t"`,
  `"ar1"`, `"rw"`) rather than introducing a parallel
  `"iid"`/`"heavy"`/`"heteroscedastic"` naming — a second vocabulary for the
  same thing is a documentation bug in waiting.

**Deps** none. **Effort** M. **Risk** low.

---

## 23. Theme N — Performance, architecture, infrastructure

> **Delivered in 0.5.0** (§0.2, §0.5): the declarative registry — done
> *before* the engine wave, as this section advised — `binsegRcpp` as the
> fast path, `progressr` support in `cpt_benchmark()`, `inst/CITATION`,
> and `vdiffr` snapshots. Not delivered: the published performance table
> and the scheduled all-engines CI job (§0.9).

- **Wrapper-code duplication.** Twenty-six wrapper functions across twelve
  files share one shape (validate → guard `requireNamespace()` → call →
  extract → `ggcpt_build()`). A declarative wrapper registry (a table of
  engine metadata + an extractor function per engine) would shrink the
  surface, make `cpt_methods()` derive from one source, and make §18's
  registration mechanism the *same* code path as the built-ins. Do this
  before, not after, engine wave #2 (§17). The audits are the argument: the
  same C3 flattening bug had to be fixed twice, in two waves of wrappers,
  because the guard lived in each wrapper rather than in one shared path.
- **`{Rdpack}` and citation hygiene.** `inst/REFERENCES.bib` is the single
  source for `cpt_cite()` and roxygen references, and three tests already hold
  that line (§6). Keep it that way; the missing piece is `inst/CITATION` for
  the package itself, which the software paper needs anyway.
- **Performance.** `binsegRcpp` as a fast binary-segmentation path; chunked
  detection for very long series; benchmark the 31 methods at n = 10⁴/10⁵/10⁶
  and publish the table (it is *also* useful documentation — users pick methods
  partly on runtime). `progressr` for long `cpt_batch()`/`cpt_benchmark()` runs.
- **Testing.** `vdiffr` snapshots for every `autoplot()` type (the package is
  visual and currently has no visual regression net — the one genuinely
  missing piece of the test infrastructure); `covr` badge; snapshot tests for
  printed output; the existing `skip_on_cran()`/`skip_if_not_installed()`
  discipline for the 29 `Suggests`; and a scheduled CI job that installs *all*
  suggested engines and runs the full matrix, so upstream breakage is caught by
  us rather than by CRAN. Two rules the R-devel failure earned: assert
  mechanisms rather than platform-dependent numbers, and keep every example
  clear of the timing check.
- **`Suggests` weight.** Already 29 packages, and §10/§17 would push past 45.
  That is a real cost: slow CI, frequent upstream breakage, and a confusing
  install story. Mitigations to decide between: (a) an `Additional_repositories`-
  free "engine bundles" install helper (`cpt_install_engines("bayesian")`);
  (b) moving the long tail to §18 registration instead of `Suggests`;
  (c) accepting the weight and investing in the scheduled CI job. **(b) is
  the strategically right answer** and is another argument for building §18
  early.
- **Software paper.** A JOSS or R Journal paper once §10 (inference) and §15
  (time indices) land — those two are what reviewers of a changepoint
  *visualization* package will ask about first. `inst/CITATION` should exist
  before then.
- **API freeze / 1.0.** After one release with no new engines: freeze the
  `ggcpt` contract (including any `regions`/`diagnostics` slots), document the
  extension points, and commit to deprecation cycles.

---

## 24. Prioritisation

Ordered by (value × confidence) ÷ effort, respecting blockers.

**Wave 1 — foundations that everything else leans on.**
1. **§18 extension mechanism** (`as_ggcpt()`, `cpt_register_method()`) — unlocks
   non-CRAN engines, deep-learning output, and user changepoints; caps
   `Suggests` growth; smallest code footprint of any item here.
2. **§15 time indices** — the most-requested ergonomics gap; touches every
   output but breaks nothing.
3. **§23 wrapper registry refactor** — do it *before* engine wave #2.
4. **Wire `sbs`/`dcbs` (`hdbinseg`)** — the one method `cpt_methods()` already
   promises for "next release" (§5.4). Shipping 0.5.0 without it would make the
   introspection table dishonest, which is the one thing this package does not
   do.
5. `inst/CITATION` and `vdiffr` snapshots for the existing `autoplot()` types —
   the two remaining gaps in an otherwise test-enforced infrastructure.

**Wave 2 — inference, which is the package's biggest credibility gap.**
`nsp_wrapper()` + `geom_cpt_region()` + the `regions` slot; `cpt_confint()`;
`cpt_select()` with CV (§11); `cpt_influence()`/`cpt_sensitivity()` (§12);
`cpt_test()` for engines with native tests.

**Wave 3 — the visual specialty.** `autoplot(type = "statistic" | "path" |
"scale_space")` (§14) with per-engine capability flags; `cpt_consensus()` +
agreement plot and `cpt_recommend()` (§19); `cpt_annotate_events()` +
`geom_cpt_event()` (§21); accessibility pass.

**Wave 4 — supervised detection.** `cpt_labels()`, `geom_cpt_label()`,
`cpt_label_error()`, `cpt_learn_penalty()`, penalty-as-model in `cpt_penalty()`
(§13), plus the peak-detection engines and a dedicated vignette. Largest new
audience per unit of work, but wants Wave 1's refactor first.

**Wave 5 — engine wave #2 and new data types.** High-dimensional
(`HDCD`, `changepoints`) → `mcp` → functional (`fChange`, `KWCChangepoint`) →
applied vocabularies (`trend`, `bfast`, `ChangePointTaylor`) → networks
(`fabisearch`), each with its `change_in` levels, capability-matrix entries and
plot types (§17).

**Wave 6 — evaluation, streaming, study design, 1.0.** `cpt_benchmark()` +
TCPD loaders (§20); `cpt_monitor()`/`cpt_delay()` (§16); `cpt_power()` and the
two remaining `cpt_simulate()` gaps (§22); performance table; API freeze and
the software paper (§23).

**Explicitly not planned.** Implementing detection algorithms from scratch
(except possibly the §16 e-detector, as a separately scoped decision); training
neural detectors in-package; a general time-series modelling framework
(`{fable}`/`{modeltime}` own that); a Shiny app as a hard dependency
(`cpt_explore()` stays a `Suggests`-gated extra or an external app).

---

## 25. Open questions and decisions needed

> **All eight were decided during the 0.5.0 cycle; the answers, and the
> one place where the answer departs from the proposal below, are in
> §0.6.**

1. **Does `ggcpt` grow slots, or do new result kinds get new classes?** NSP
   regions (§10), solution-path diagnostics (§14) and functional/network results
   (§17) all want to attach something. **Largely settled by precedent:**
   `ggcpt_build()` already attaches `data_wide` and `fitted` only when an engine
   supplies them, so additive optional slots are the established pattern for
   things that are still "a segmentation of a series". Reserve **new classes**
   for monitors (§16) and benchmarks (§20), which are not. What remains open is
   narrower: what `tidy()` returns when regions and points coexist.
2. **How honest can `cp` be for interval-valued methods?** For NSP there is no
   point estimate. Returning the midpoint risks a user plotting it as one.
   Proposal: `cp = NA_integer_` with populated `regions`, and an `autoplot()`
   that draws bands; document loudly. Note this makes NSP the first result that
   is non-empty yet has no plottable `cp`, and `autoplot()` currently errors on
   an empty object — that guard needs to distinguish "no data" from "no points".
3. **`Suggests` strategy.** Accept 45+ suggested engines, or push the long tail
   to §18 registration? This is the single most consequential architectural
   decision in this part and it should be made *before* Wave 5.
4. **JAGS/system dependencies.** `mcp` needs JAGS. Is a `Suggests` engine with a
   system dependency acceptable, given examples must skip cleanly? (Probably
   yes, with `skip_if_not_installed()` plus a runtime check, but decide once.)
5. **One annotation representation or two?** §13's supervised labels and
   `cpt_metrics_annotated()`'s ground truth are the same shape. Unify them
   before building §13, or they will diverge permanently.
6. **Do we implement anything original?** The §16 e-detector is the tempting
   case: no R implementation exists, the theory is clean, and it would
   strengthen a software paper. It also breaks the wrap-don't-implement rule
   that has kept the package's correctness surface manageable. Decide
   deliberately.
7. **Benchmark data policy.** TCPD download-and-cache vs. bundling a tiny
   curated subset vs. simulation only. Leaning: download-and-cache, with
   `cpt_simulate()` covering all offline examples.
8. **When to freeze.** 0.4.0 grew the API enormously and needed three audit
   passes and eighty-six fixes to settle it. A release that adds *nothing* and
   only stabilises (tests, docs, performance table, deprecation policy) may be
   worth more than another engine wave. If that is the choice, Wave 1's items 4
   and 5 are the smallest honest content for it.

---

## 26. References

### 26.1 Wired engines and methods (as of 0.4.0)

All verified against the published record in July 2026.

1. Adams, R. P. and MacKay, D. J. C. (2007). *Bayesian online changepoint
   detection.* arXiv:0710.3742. (R package `ocp`.)
2. Anastasiou, A. and Fryzlewicz, P. (2022). *Detecting multiple generalized
   change-points by isolating single ones.* **Metrika** 85, 141–174.
   (R package `IDetect`.)
3. Arlot, S., Celisse, A. and Harchaoui, Z. (2019). *A kernel multiple
   change-point algorithm via model selection.* **JMLR** 20(162), 1–56.
   (R package `kcpRS` via Cabrieto et al.)
4. Bai, J. and Perron, P. (1998). *Estimating and testing linear models with
   multiple structural changes.* **Econometrica** 66(1), 47–78.
5. Bai, J. and Perron, P. (2003). *Computation and analysis of multiple
   structural change models.* **J. Applied Econometrics** 18(1), 1–22.
   (R package `strucchange`.)
6. Baranowski, R., Chen, Y. and Fryzlewicz, P. (2019).
   *Narrowest-over-threshold detection of multiple change points.* **JRSS-B**
   81(3), 649–672. (R package `not`.)
7. Barry, D. and Hartigan, J. A. (1993). *A Bayesian analysis for change
   point problems.* **JASA** 88(421), 309–319. (R package `bcp`.)
8. Beaulieu, C. and Killick, R. (2018). *Distinguishing trends and shifts
   from memory in climate data.* **Journal of Climate** 31(23), 9519–9543.
   (R package `EnvCpt`.)
9. Cabrieto, J., Adolf, J., Tuerlinckx, F., Kuppens, P. and Ceulemans, E.
   (2018). *Detecting long-lived autodependency changes in a multivariate
   system via change point detection and regime switching models.*
   **Scientific Reports** 8, 15637. (R package `kcpRS`.)
10. Chen, Y., Wang, T. and Samworth, R. J. (2022). *High-dimensional,
    multiscale online changepoint detection.* **JRSS-B** 84(1), 234–266.
    (R package `ocd`.)
11. Eichinger, B. and Kirch, C. (2018). *A MOSUM procedure for the
    estimation of multiple random change points.* **Bernoulli** 24(1),
    526–564. (R package `mosum`.)
12. Erdman, C. and Emerson, J. W. (2007). *bcp: An R package for performing
    a Bayesian analysis of change point problems.* **JSS** 23(3), 1–13.
13. Fearnhead, P., Maidstone, R. and Letchford, A. (2019). *Detecting
    changes in slope with an L0 penalty.* **JCGS** 28(2), 265–275.
14. Fearnhead, P. and Grose, D. (2024). *cpop: Detecting changes in
    piecewise-linear signals.* **JSS** 109(7), 1–30. (R package `cpop`.)
15. Frick, K., Munk, A. and Sieling, H. (2014). *Multiscale change point
    inference.* **JRSS-B** 76(3), 495–580. (R package `stepR`.)
16. Fryzlewicz, P. (2014). *Wild binary segmentation for multiple
    change-point detection.* **Annals of Statistics** 42(6), 2243–2281.
    (R package `wbs`.)
17. Fryzlewicz, P. (2018). *Tail-greedy bottom-up data decompositions and
    fast multiple change-point detection.* **Annals of Statistics** 46(6B),
    3390–3421. (R package `breakfast`.)
18. Fryzlewicz, P. (2020). *Detecting possibly frequent change-points: Wild
    Binary Segmentation 2 and steepest-drop model selection.* **J. Korean
    Statistical Society** 49, 1027–1070. (R package `breakfast`.)
19. Grundy, T., Killick, R. and Mihaylov, G. (2020). *High-dimensional
    changepoint detection via a geometrically inspired mapping.*
    **Statistics and Computing** 30, 1155–1166. (R package
    `changepoint.geo`.)
20. Haynes, K., Eckley, I. A. and Fearnhead, P. (2017). *Computationally
    efficient changepoint detection for a range of penalties.* **JCGS**
    26(1), 134–143. (CROPS; `changepoint`.)
21. Haynes, K., Fearnhead, P. and Eckley, I. A. (2017). *A computationally
    efficient nonparametric approach for changepoint detection.*
    **Statistics and Computing** 27(5), 1293–1305. (R package
    `changepoint.np`.)
22. Killick, R., Fearnhead, P. and Eckley, I. A. (2012). *Optimal detection
    of changepoints with a linear computational cost.* **JASA** 107(500),
    1590–1598. (PELT; R package `changepoint`.)
23. Killick, R. and Eckley, I. A. (2014). *changepoint: An R package for
    changepoint analysis.* **JSS** 58(3), 1–19.
24. Li, X. and Zhang, X. (2024). *fastcpd: Fast change point detection in
    R.* arXiv:2404.05933. (R package `fastcpd`.)
25. Maidstone, R., Hocking, T., Rigaill, G. and Fearnhead, P. (2017). *On
    optimal multiple changepoint algorithms for large data.* **Statistics
    and Computing** 27(2), 519–533. (R package `fpop`.)
26. Matteson, D. S. and James, N. A. (2014). *A nonparametric approach for
    multiple change point analysis of multivariate data.* **JASA** 109(505),
    334–345. (R package `ecp`.)
27. McGonigle, E. T. and Cho, H. (2025). *Nonparametric data segmentation in
    multivariate time series via joint characteristic functions.*
    **Biometrika** 112(2), asaf024. (R package `CptNonPar`.)
28. Muggeo, V. M. R. (2003). *Estimating regression models with unknown
    break-points.* **Statistics in Medicine** 22(19), 3055–3071.
    (R package `segmented`.)
29. Muggeo, V. M. R. (2008). *segmented: An R package to fit regression
    models with broken-line relationships.* **R News** 8(1), 20–25.
30. Pein, F., Sieling, H. and Munk, A. (2017). *Heterogeneous change point
    inference.* **JRSS-B** 79(4), 1207–1227. (HSMUCE; R package `stepR`.)
31. Romano, G., Rigaill, G., Runge, V. and Fearnhead, P. (2022). *Detecting
    abrupt changes in the presence of local fluctuations and autocorrelated
    noise.* **JASA** 117(540), 2147–2162. (R package `DeCAFS`.)
32. Ross, G. J. (2015). *Parametric and nonparametric sequential change
    detection in R: The cpm package.* **JSS** 66(3), 1–20.
33. van den Burg, G. J. J. and Williams, C. K. I. (2020). *An evaluation of
    change point detection algorithms.* arXiv:2003.06222. (Turing Change
    Point Dataset: <https://github.com/alan-turing-institute/TCPD>.)
34. Wang, T. and Samworth, R. J. (2018). *High dimensional change point
    estimation via sparse projection.* **JRSS-B** 80(1), 57–83.
    (R package `InspectChangepoint`.)
35. Zeileis, A., Leisch, F., Hornik, K. and Kleiber, C. (2002).
    *strucchange: An R package for testing for structural change in linear
    regression models.* **JSS** 7(2), 1–38.
36. Zhao, K., Wulder, M. A., Hu, T., et al. (2019). *Detecting change-point,
    trend, and seasonality in satellite time series data...* (BEAST).
    **Remote Sensing of Environment** 232, 111181. (R package `Rbeast`.)
37. Zhao, Z., Jiang, F. and Shao, X. (2022). *Segmenting time series via
    self-normalisation.* **JRSS-B** 84(5), 1699–1725. (R package `SNSeg`.)
38. Zhao, Z., Jiang, F. and Shao, X. (2024). *SNSeg: An R package for time
    series segmentation via self-normalization.* **R Journal**.
    <https://journal.r-project.org/articles/RJ-2024-029/>

### 26.2 Roadmap literature (not yet wired)

Engines already wired are in §26.1 and are not repeated here.

**Inference and post-selection**

- Fryzlewicz P (2024). Narrowest Significance Pursuit: Inference for Multiple
  Change-Points in Linear Models. *JASA* 119(546): 1633–1646.
  <https://doi.org/10.1080/01621459.2023.2211733> · preprint
  <https://arxiv.org/abs/2009.05431> · R package `nsp`
  <https://cran.r-project.org/package=nsp>
- Jewell S, Fearnhead P, Witten D (2022). Testing for a Change in Mean After
  Changepoint Detection. *JRSS-B* 84(4): 1082–1104.
  <https://doi.org/10.1111/rssb.12501> · software
  <https://jewellsean.github.io/changepoint-inference/>

**Selecting the number of changepoints**

- Zou C, Wang G, Li R (2020). Consistent selection of the number of
  change-points via sample-splitting. *Annals of Statistics* 48(1): 413–439.
  <https://pmc.ncbi.nlm.nih.gov/articles/PMC7397423/> · R package
  `crossvalidationCP` <https://cran.r-project.org/package=crossvalidationCP>
  (the authors' `cpss` was removed from CRAN)

**Influence and stability**

- Wilms I, Killick R, Matteson DS (2022). Graphical Influence Diagnostics for
  Changepoint Models. *JCGS*. · R package `changepoint.influence`
  <https://cran.r-project.org/package=changepoint.influence>

**Supervised changepoint detection**

- Hocking TD, Rigaill G, Vert J-P, Bach F (2013). Learning Sparse Penalties for
  Change-point Detection using Max Margin Interval Regression. *ICML*. · R
  package `penaltyLearning`
  <https://cran.r-project.org/package=penaltyLearning>
- Hocking TD et al. Constrained Dynamic Programming and Supervised Penalty
  Learning Algorithms for Peak Detection. · `PeakSegOptimal`, `PeakSegDisk`
- Hocking TD (2024). Functional Labeled Optimal Partitioning (FLOPART). *JCGS*.
  <https://arxiv.org/pdf/2210.02580>

**High-dimensional, functional, network**

- Moen PAH, Glad IK, Tveten M (2023). Efficient sparsity adaptive changepoint
  estimation (ESAC). <https://arxiv.org/abs/2306.04702> · R package `HDCD`
  <https://cran.r-project.org/package=HDCD>
- Cho H, Fryzlewicz P (2015). Multiple-change-point detection for high
  dimensional time series via sparsified binary segmentation. *JRSS-B*. · R
  package `hdbinseg` (live on CRAN, 1.0.3)
  <https://cran.r-project.org/package=hdbinseg>
- Xu H, Wang D, Yu Z, Rinaldo A. `changepoints`: A Collection of Change-Point
  Detection Methods. <https://cran.r-project.org/package=changepoints> ·
  <https://github.com/HaotianXu/changepoints>
- `fChange`: Functional Change Point Detection and Analysis.
  <https://cran.r-project.org/package=fChange>
- `KWCChangepoint`: Robust Changepoint Detection for Functional and
  Multivariate Data. <https://cran.r-project.org/package=KWCChangepoint>
- Ondrus M, Olds E, Cribben I (2024). `fabisearch`: change point detection in
  and visualization of the network structure of multivariate high-dimensional
  time series. *Neurocomputing*.
  <https://www.sciencedirect.com/science/article/pii/S0925231224000924>
- `VARcpDetectOnline`: Sequential Change Point Detection for High-Dimensional
  VAR Models. <https://cran.r-project.org/package=VARcpDetectOnline>
- Londschien M, Bühlmann P, Kovács S (2023). Random Forests for Change Point
  Detection. *JMLR* 24(216): 1–45. <https://www.jmlr.org/papers/v24/22-0512.html>
  (`changeforest` — conda-forge only, not CRAN)

**Bayesian and model-based**

- Lindeløv JK. `mcp`: An R Package for Regression With Multiple Change Points.
  <https://lindeloev.github.io/mcp/> · <https://cran.r-project.org/package=mcp>

**Sequential / online**

- Shin J, Ramdas A, Rinaldo A (2023). E-detectors: A Nonparametric Framework
  for Sequential Change Detection. *NEJSDS*.
  <https://arxiv.org/abs/2203.03532> ·
  <https://nejsds.nestat.org/journal/NEJSDS/article/59/info>

**Deep learning and reviews**

- Li J, Fearnhead P, Fryzlewicz P, Wang T (2024). Automatic change-point
  detection in time series via deep learning. *JRSS-B* 86(2): 273–.
  <https://academic.oup.com/jrsssb/article/86/2/273/7517020>
- Xu R, Song Z, Wu J, Wang C, Zhou S (2025). Change-point detection with deep
  learning: A review. *Frontiers of Engineering Management*.
  <https://link.springer.com/article/10.1007/s42524-025-4109-z>
- Aminikhanghahi S, Cook DJ (2017). A Survey of Methods for Time Series Change
  Point Detection. *KAIS*. <https://pmc.ncbi.nlm.nih.gov/articles/PMC5464762/>
- A Survey of Change Point Detection in Dynamic Graphs (2025). *IEEE TKDE*.
  <https://www.computer.org/csdl/journal/tk/2025/03/10817616/231oUDkTj8s>

**Applied vocabularies**

- Verbesselt J et al. `bfast`: Breaks For Additive Season and Trend.
  <https://cran.r-project.org/package=bfast>
- Pohlert T. `trend`: Non-Parametric Trend Tests and Change-Point Detection.
  <https://cran.r-project.org/package=trend>
- `ChangePointTaylor`: Identify Changes in Mean.
  <https://cran.r-project.org/package=ChangePointTaylor>

---

*Part I is a record; Part II is a living roadmap. Milestones will be refined as
implementation proceeds, and contributions and method suggestions are welcome
via the issue tracker.*
