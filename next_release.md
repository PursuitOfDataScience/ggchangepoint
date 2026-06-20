# ggchangepoint: A Roadmap for the Next Release

### Expanding tidy, visualization-first changepoint detection in R

**Author:** Youzhi Yu
**Status:** Design document / development roadmap for `ggchangepoint` 0.2.0 and beyond
**Current release:** `ggchangepoint` 0.1.0 (CRAN)

---

## Abstract

`ggchangepoint` 0.1.0 combines three changepoint engines — `changepoint`,
`changepoint.np`, and `ecp` — behind a small tidyverse-flavoured API
(`cpt_wrapper()`, `ecp_wrapper()`) and renders the detected changepoints with
`ggplot2` (`ggcptplot()`, `ggecpplot()`). This document proposes a substantial
expansion of the package, grounded in the modern changepoint-detection
literature, while preserving the two design commitments that define the package:
**(i) every detector returns a tidy tibble**, and **(ii) every result is
directly renderable with `ggplot2`**.

We survey the impactful methodological literature — penalised optimal
partitioning and pruning (Optimal Partitioning, PELT, FPOP/SNIP, gfpop),
randomised and multiscale search (Binary Segmentation, WBS, WBS2/SDLL, NOT,
MOSUM, Isolate-Detect, TGUH), multiscale inference with confidence
(SMUCE/HSMUCE, FDRSeg), nonparametric and distribution-free detection (NMCD,
energy/E-Divisive, kernel/KCP, sequential CPMs), robustness and dependence
(biweight/robust loss, DeCAFS, self-normalisation), high-dimensional and
multivariate detection (`inspect`, sparsified binary segmentation, the
`changepoints` collection), Bayesian detection (product-partition `bcp`,
Bayesian Online Changepoint Detection), structural breaks in regression
(Bai–Perron, `strucchange`, `segmented`), and online/sequential monitoring
(CPM, FOCuS, OCD). For each family we specify the mathematics, the R package to
wrap, the proposed tidy function signature, and the proposed visualisation.

We then propose: a unified `ggcpt` S3 result object with `broom`-style
`tidy()`/`glance()`/`augment()` methods and a `ggplot2::autoplot()` method; a
family of new geoms/stats (`geom_changepoint()`, `geom_cpt_segment()`,
`geom_cpt_ci()`); a penalty-path / model-selection toolkit (CROPS, elbow and
solution-path plots); an evaluation module implementing the standard accuracy
metrics (precision/recall/F1 with margin, the covering metric, Hausdorff
distance, the (adjusted) Rand index, annotation error) including multi-annotator
support; and a data-generating module with the canonical benchmark signals.
We also **audit the four functions already shipped in 0.1.0** and specify
concrete fixes (§12): a confirmed edge-case bug in `ecp_wrapper()` that emits
spurious boundary "changepoints" and an `NA` on series with no change, a live
`ggplot2` `size`→`linewidth` deprecation in both plot functions, an off-by-one
inconsistency between the two detectors' location conventions, and absent input
validation — each scheduled for v0.2.0. The document closes with a phased
release plan, dependency and architecture notes, and a complete, verified
reference list.

---

## Contents

1. [Introduction](#1-introduction)
2. [Problem formulation and notation](#2-problem-formulation-and-notation)
3. [A taxonomy of methods and the gaps in 0.1.0](#3-a-taxonomy-of-methods-and-the-gaps-in-010)
4. [A unified tidy result object and API](#4-a-unified-tidy-result-object-and-api)
5. [New detection methods](#5-new-detection-methods)
   - 5.1 [Penalised optimal partitioning and functional pruning](#51-penalised-optimal-partitioning-and-functional-pruning)
   - 5.2 [Randomised and multiscale search](#52-randomised-and-multiscale-search)
   - 5.3 [Multiscale inference with confidence statements](#53-multiscale-inference-with-confidence-statements)
   - 5.4 [Nonparametric and distribution-free detection](#54-nonparametric-and-distribution-free-detection)
   - 5.5 [Robustness and dependence](#55-robustness-and-dependence)
   - 5.6 [High-dimensional and multivariate detection](#56-high-dimensional-and-multivariate-detection)
   - 5.7 [Bayesian changepoint detection](#57-bayesian-changepoint-detection)
   - 5.8 [Structural breaks in regression](#58-structural-breaks-in-regression)
   - 5.9 [Online and sequential detection](#59-online-and-sequential-detection)
6. [Penalty and model-selection toolkit](#6-penalty-and-model-selection-toolkit)
7. [The visualization layer](#7-the-visualization-layer)
8. [Evaluation and benchmarking](#8-evaluation-and-benchmarking)
9. [Data simulation and benchmark signals](#9-data-simulation-and-benchmark-signals)
10. [Package architecture, dependencies, and testing](#10-package-architecture-dependencies-and-testing)
11. [Phased release plan](#11-phased-release-plan)
12. [Auditing and hardening the existing functions](#12-auditing-and-hardening-the-existing-functions)
13. [Backward compatibility and deprecation](#13-backward-compatibility-and-deprecation)
14. [Appendix A: Method → package → function map](#appendix-a-method--package--function-map)
15. [Appendix B: Proposed API reference](#appendix-b-proposed-api-reference)
16. [References](#references)

---

## 1. Introduction

### 1.1 Motivation

Changepoint detection — the identification of times at which the statistical
properties of a sequence change — is a mature, fast-moving subfield of
statistics and machine learning with applications spanning genomics, finance,
climatology, network monitoring, neuroscience, industrial quality control, and
condition monitoring. The R ecosystem is unusually rich in high-quality
implementations, but they are fragmented: each package has its own API, its own
return structure, and its own (usually base-graphics) plotting conventions. This
fragmentation makes it hard to (a) move between methods, (b) compare methods on
the same series, and (c) produce publication-quality, customisable graphics.

`ggchangepoint` exists to remove that friction for two specific things: **tidy
output** and **`ggplot2` rendering**. Version 0.1.0 demonstrates the idea on
three engines. This document plans the version that makes the idea
comprehensive.

### 1.2 The current package (0.1.0)

The current public surface is:

| Function | Wraps | Returns |
|---|---|---|
| `cpt_wrapper(data, change_in, cp_method, ...)` | `changepoint::cpt.mean/var/meanvar`, `changepoint.np::cpt.np` | `tibble(cp, cp_value)` |
| `ecp_wrapper(data, algorithm, min_size, ...)` | `ecp::e.divisive`, `ecp::e.agglo` | `tibble(cp, cp_value)` |
| `ggcptplot(...)` | `cpt_wrapper()` | `ggplot` (line + points + vertical cpt lines) |
| `ggecpplot(...)` | `ecp_wrapper()` | `ggplot` (line + points + vertical cpt lines) |

The two return columns are `cp` (the integer index of the changepoint in the
input vector) and `cp_value` (the raw value at that index). The plots draw the
raw series as a line-and-point geometry and overlay each changepoint as a
vertical `geom_linerange`.

These four functions are the foundation the rest of this document builds on, and
they will be retained (principle P6 below). They have, however, accumulated
concrete technical debt — a live `ggplot2` deprecation, an edge-case correctness
bug in `ecp_wrapper()`, an inconsistent changepoint-location convention between
the two engines, and absent input validation. We do not gloss over this: §12
audits all four functions in detail, with empirically reproduced evidence, and
specifies the fixes (slated for v0.2.0).

### 1.3 Design principles for 0.2.0+

We commit to the following principles, which constrain every proposal below:

- **P1 — Wrap, don't reinvent.** We bind to peer-reviewed, CRAN-published
  engines rather than re-implementing algorithms, so that statistical
  correctness is inherited from the upstream maintainers. New algorithms enter
  the package as `Suggests` first (optional), then graduate to `Imports` if they
  become core.
- **P2 — Tidy in, tidy out.** Every detector returns a tibble (and a structured
  `ggcpt` object that contains it). Column names are stable across methods.
- **P3 — `ggplot2` all the way down.** Every result is renderable with a single
  `autoplot()` call and is extensible with `+` like any `ggplot`.
- **P4 — One vocabulary.** A consistent argument vocabulary (`x`, `method`,
  `penalty`, `min_size`, `n_cpts`, …) across all detectors; method-specific
  arguments flow through `...`.
- **P5 — Progressive disclosure.** Beginners call one function
  (`cpt_detect()` + `autoplot()`); experts reach the upstream object via
  `$fit` and tune everything.
- **P6 — No surprises for 0.1.0 users.** `cpt_wrapper()`, `ecp_wrapper()`,
  `ggcptplot()`, and `ggecpplot()` keep working unchanged.

### 1.4 Scope

This is a *planning* document: it specifies what to build and why, with
mathematics, citations, and proposed signatures, at a level of detail intended
to be directly actionable in implementation. It is not itself the
implementation. Function signatures are proposals and may be refined during
development; where an upstream package's argument is passed through verbatim we
say so.

---

## 2. Problem formulation and notation

### 2.1 The offline multiple-changepoint model

Let $y_{1:n} = (y_1, \dots, y_n)$ be an ordered sequence (a time series or any
indexed data) taking values in $\mathbb{R}$ (univariate) or $\mathbb{R}^p$
(multivariate). A *segmentation* with $m$ changepoints is an ordered set

$$
\tau_{0:m+1}, \qquad 0 = \tau_0 < \tau_1 < \tau_2 < \dots < \tau_m < \tau_{m+1} = n,
$$

which partitions the index set into $m+1$ contiguous segments
$y_{(\tau_{j-1}+1):\tau_j}$, $j = 1, \dots, m+1$. The modelling assumption is
that within each segment the data are governed by a single parameter
$\theta_j \in \Theta$ (e.g. a mean, a variance, a mean–variance pair, a
regression coefficient vector, or an entire distribution), and that the
parameter *changes* at each $\tau_j$.

### 2.2 Penalised cost (the optimisation view)

Most offline methods choose the segmentation minimising a penalised cost

$$
\sum_{j=1}^{m+1} \mathcal{C}\!\left(y_{(\tau_{j-1}+1):\tau_j}\right) \;+\; \beta\, f(m),
$$

where $\mathcal{C}(\cdot)$ is a *segment cost* (typically twice the negative
maximised log-likelihood, $\mathcal{C}(y_{a:b}) = -2 \max_{\theta}
\sum_{i=a}^{b} \log p(y_i \mid \theta)$) and $\beta f(m)$ is a penalty that
guards against over-segmentation. For a change in mean with Gaussian noise of
known variance, $\mathcal{C}(y_{a:b}) = \sum_{i=a}^{b} (y_i - \bar y_{a:b})^2$.
Common penalties $\beta f(m)$ include AIC ($2k$), BIC/SIC ($k \log n$), Modified
BIC (mBIC), and Hannan–Quinn, where $k$ is the number of free parameters added
per changepoint. The choice of $\beta$ is the central nuisance of the field and
motivates the penalty-path tooling of §6.

### 2.3 The test-and-segment view

A complementary view treats detection as repeated hypothesis testing. The
canonical statistic is the **CUSUM** contrast on an interval $(s, e]$ with split
$b$:

$$
\mathcal{X}^{\,s,e}_{b}
= \sqrt{\frac{e-b}{(e-s)(b-s)}} \sum_{t=s+1}^{b} y_t
- \sqrt{\frac{b-s}{(e-s)(e-b)}} \sum_{t=b+1}^{e} y_t .
$$

Binary Segmentation, WBS, NOT, and MOSUM are all built on (weighted, windowed,
or randomised) variants of this statistic. Online methods (CUSUM, Page, CPM,
FOCuS, OCD) use sequential analogues.

### 2.4 Axes of variation

A useful taxonomy — which structures §5 — distinguishes methods along these
axes:

- **What changes:** mean / variance / mean+variance / slope (trend) / whole
  distribution / regression coefficients / covariance / network structure.
- **Search strategy:** exact dynamic programming (Optimal Partitioning, Segment
  Neighbourhood) vs. pruned exact (PELT, FPOP) vs. greedy/approximate (Binary
  Segmentation) vs. randomised (WBS, NOT) vs. moving-window (MOSUM) vs.
  isolation (Isolate-Detect).
- **Parametric vs. nonparametric.**
- **Frequentist vs. Bayesian.**
- **Offline (batch) vs. online (sequential).**
- **Univariate vs. multivariate vs. high-dimensional.**
- **Independence vs. dependence/autocorrelation; light- vs. heavy-tailed noise.**

---

## 3. A taxonomy of methods and the gaps in 0.1.0

The table maps the landscape and marks current coverage. ✅ = in 0.1.0;
🆕 = proposed for the next release.

| Family | Representative methods | R package(s) | Status |
|---|---|---|---|
| Exact / pruned optimal partitioning | OP, **PELT**, SegNeigh, **FPOP/SNIP**, **gfpop** | `changepoint`, `fpop`, `gfpop` | PELT/SegNeigh ✅; FPOP/gfpop 🆕 |
| Penalty path | **CROPS** | `changepoint`, `crops` | 🆕 |
| Randomised / greedy search | BinSeg, **WBS**, **WBS2/SDLL**, **NOT**, **TGUH**, **Isolate-Detect** | `wbs`, `breakfast`, `not`, `IDetect` | BinSeg ✅; rest 🆕 |
| Moving window | **MOSUM**, multiscale MOSUM | `mosum` | 🆕 |
| Multiscale inference + CIs | **SMUCE**, **HSMUCE**, FDRSeg | `stepR`, `FDRSeg` | 🆕 |
| Nonparametric (univariate) | **NMCD** (`cpt.np`) | `changepoint.np` | ✅ |
| Nonparametric (multivariate, energy) | **E-Divisive / E-Agglo** | `ecp` | ✅ |
| Kernel | **KCP** | `kcpRS` | 🆕 |
| Sequential CPM | Mann–Whitney, Mood, Lepage, KS, CvM | `cpm` | 🆕 |
| Robust to outliers | **biweight / robust loss** | `robseg` | 🆕 |
| Local fluctuation + autocorrelation | **DeCAFS** | `DeCAFS` | 🆕 |
| Dependence, multi-parameter | **Self-normalisation (SNCP)** | `SNSeg` | 🆕 |
| High-dimensional mean | **inspect**, **SBS** | `InspectChangepoint`, `hdbinseg` | 🆕 |
| Collection (HD mean, regression, network, VAR) | **changepoints** | `changepoints` | 🆕 |
| Bayesian (offline) | **bcp** (product partition) | `bcp` | 🆕 |
| Bayesian (online) | **BOCPD** | `ocp` | 🆕 |
| Structural breaks (regression) | **Bai–Perron**, fluctuation tests | `strucchange` | 🆕 |
| Segmented / broken-line regression | **segmented** | `segmented` | 🆕 |
| Online CUSUM | **FOCuS** | `FOCuS` | 🆕 |
| High-dimensional online | **OCD** | `ocd` | 🆕 |

The next release closes essentially all of these gaps, prioritised in §11.

---

## 4. A unified tidy result object and API

### 4.1 The `ggcpt` object

We introduce a single S3 class returned by every detector:

```r
structure(
  list(
    changepoints = tibble(cp, cp_value, ...),  # one row per detected changepoint
    segments     = tibble(seg_id, start, end, n, param_estimate, ...),
    data         = tibble(index, value),        # tidy long form of the input
    method       = "pelt",                       # canonical method id
    change_in    = "mean",                        # what changed
    penalty      = list(type = "MBIC", value = ...),
    fit          = <upstream object>,             # the raw object from the engine
    call         = <matched call>
  ),
  class = "ggcpt"
)
```

`changepoints` always contains `cp` (integer index) and `cp_value` (the value
at that index) — **identical to the 0.1.0 contract** — plus optional method
specific columns (e.g. `cusum`, `posterior_prob`, `interval_start`,
`interval_end`, `ci_lower`, `ci_upper`, `coordinate`). `segments` is the dual
representation: one row per segment with its location-parameter estimate, which
powers segment-mean overlays and the `augment()` method.

### 4.2 `broom` methods

We implement the `broom` generics so the package composes with the rest of the
tidyverse and with `tidymodels`:

- **`tidy(x)`** → the `changepoints` tibble (one row per changepoint).
- **`glance(x)`** → a one-row summary: `n`, `n_changepoints`, `method`,
  `change_in`, `penalty_type`, `penalty_value`, `total_cost`/`logLik` where
  available, `runtime`.
- **`augment(x)`** → the original data with added columns: `seg_id`, the
  fitted within-segment level (`.fitted`), the residual (`.resid`), and an
  `is_changepoint` flag. This is the natural input to `geom_cpt_segment()`.

### 4.3 The unified front-end and thin wrappers

Two entry points, mirroring how 0.1.0 already feels:

```r
# 1. Unified dispatcher (recommended for users comparing methods)
cpt_detect(x,
           method   = c("pelt", "binseg", "segneigh", "amoc",
                        "wbs", "wbs2", "not", "mosum", "idetect", "tguh",
                        "smuce", "hsmuce", "fpop", "gfpop",
                        "np", "ecp", "kcp", "cpm",
                        "robust", "decafs", "sn",
                        "inspect", "sbs", "bcp", "bocpd",
                        "strucchange", "segmented"),
           change_in = c("mean", "var", "meanvar", "slope", "distribution"),
           penalty   = "MBIC",
           ...)

# 2. Thin per-engine wrappers (stable, discoverable, documented one-to-one)
#    cpt_wrapper(), ecp_wrapper()  [existing]
#    wbs_wrapper(), not_wrapper(), mosum_wrapper(), idetect_wrapper(),
#    fpop_wrapper(), gfpop_wrapper(), smuce_wrapper(), kcp_wrapper(),
#    cpm_wrapper(), robseg_wrapper(), decafs_wrapper(), sn_wrapper(),
#    inspect_wrapper(), changepoints_wrapper(), bcp_wrapper(),
#    bocpd_wrapper(), strucchange_wrapper(), segmented_wrapper()
```

`cpt_detect()` dispatches to the thin wrappers; both return a `ggcpt` object.
The thin wrappers keep one-to-one documentation with their upstream engine so
expert users can map arguments precisely.

### 4.4 Rendering

A single generic renders any result:

```r
autoplot(x, ...)          # S3 method autoplot.ggcpt(); returns a ggplot
ggcptplot(x, ...)         # kept; now also accepts a ggcpt object
```

Details of the visualization layer are in §7.

---

## 5. New detection methods

Each subsection gives: the idea and key mathematics, the R package to wrap, the
proposed tidy signature, and the proposed plot. Citations are collected in the
[References](#references).

### 5.1 Penalised optimal partitioning and functional pruning

#### 5.1.1 Optimal Partitioning and Segment Neighbourhood (context)

Optimal Partitioning (Jackson et al., 2005) solves the penalised-cost problem
exactly by the recursion

$$
F(t) = \min_{0 \le s < t} \Big\{ F(s) + \mathcal{C}(y_{(s+1):t}) + \beta \Big\}, \qquad F(0) = -\beta,
$$

in $O(n^2)$. Segment Neighbourhood (Auger & Lawrence, 1989) instead fixes the
number of segments and is $O(Q n^2)$ for up to $Q$ changepoints. Both are
already reachable through `cpt_wrapper(cp_method = "SegNeigh")`; we document
their relationship to PELT explicitly in the vignette.

#### 5.1.2 PELT (already wrapped — documented more fully)

PELT (Killick, Fearnhead & Eckley, 2012) augments Optimal Partitioning with a
pruning step: a candidate last-changepoint $s$ can be discarded permanently once

$$
F(s) + \mathcal{C}(y_{(s+1):t}) + K \;>\; F(t),
$$

provided there exists a constant $K$ with $\mathcal{C}(y_{(s+1):t}) +
\mathcal{C}(y_{(t+1):T}) + K \le \mathcal{C}(y_{(s+1):T})$ for all $T>t$. Under
the assumption that changepoints are spread throughout the data, PELT achieves
expected $O(n)$ cost while remaining *exact*. This is the default engine of
`cpt_wrapper()`; the next release adds an explanatory derivation and a runtime
comparison plot to the vignette.

#### 5.1.3 FPOP and SNIP

Functional Pruning Optimal Partitioning (Maidstone, Hocking, Rigaill &
Fearnhead, 2017) prunes the *cost functions* rather than candidate indices,
storing $F_t(\theta)$ as a piecewise-quadratic in the segment parameter and
discarding regions of $\Theta$ that can never be optimal. FPOP is exact, returns
the same segmentation as PELT, and is empirically faster — crucially, its speed
is *robust to the number of changepoints*. SNIP is its companion for the
segment-neighbourhood (fixed-$Q$) formulation.

- **Wraps:** `fpop` (`Fpop()`).
- **Proposed:** `fpop_wrapper(x, penalty = "BIC", ...)` → `ggcpt`.

#### 5.1.4 gfpop — graph-constrained functional pruning

Generalized Functional Pruning Optimal Partitioning (Hocking, Rigaill,
Fearnhead & Bourque, 2022) lets the user encode prior structural constraints on
the sequence of segment means as a *constraint graph*: e.g. "means must
alternate up-then-down" (peak detection in genomics), "monotone increasing"
(isotonic), or bounded jumps. The cost minimised is the penalised cost subject
to the graph, solved with empirical cost $O(N \log N)$.

- **Wraps:** `gfpop` (`gfpop()`, `graph()`).
- **Proposed:** `gfpop_wrapper(x, graph = NULL, type = c("std","updown","isotonic","relevant"), ...)`.
- **Plot:** `autoplot()` overlays the constrained step-mean; for `updown`
  graphs it shades up/down states.

### 5.2 Randomised and multiscale search

#### 5.2.1 Binary Segmentation (context)

Standard Binary Segmentation recursively finds the single split $b$ maximising
$|\mathcal{X}^{s,e}_b|$ on the current interval and recurses while the maximum
exceeds a threshold. It is $O(n \log n)$ but inconsistent when changes are
frequent or small. Available now via `cp_method = "BinSeg"`.

#### 5.2.2 WBS — Wild Binary Segmentation

WBS (Fryzlewicz, 2014) draws $M$ random sub-intervals $(s_m, e_m]$, computes the
CUSUM contrast on each, and recurses on the interval achieving the globally
largest contrast — a *random localisation* that detects short, small changes
that standard Binary Segmentation misses, without a span parameter. Model
selection uses either a threshold or the strengthened Schwarz Information
Criterion (sSIC).

- **Wraps:** `wbs` (`wbs()`, `changepoints()`); also `breakfast`.
- **Proposed:** `wbs_wrapper(x, n_intervals = 5000, threshold = NULL, selection = c("ssic","threshold"), ...)`.

#### 5.2.3 WBS2 and the Steepest-Drop (SDLL) model selection

WBS2 (Fryzlewicz, 2020) recomputes random intervals *recursively and locally*,
yielding a complete solution path, paired with "Steepest Drop to Low Levels"
(SDLL) model selection. WBS2.SDLL excels precisely in the frequent-change regime.

- **Wraps:** `breakfast` (`sol.wbs2()`, `model.sdll()`).
- **Proposed:** exposed via `cpt_detect(method = "wbs2")`.

#### 5.2.4 NOT — Narrowest-Over-Threshold

NOT (Baranowski, Chen & Fryzlewicz, 2019) considers, among all random intervals
whose contrast exceeds a threshold $\zeta$, the *narrowest*, isolating a single
feature at a time. It is generic over the contrast, so it handles not only
piecewise-constant **mean** but piecewise-**linear** (continuous or with jumps),
**variance**, and **mean+variance** changes. Threshold via sSIC; the entire
solution path is computable in close-to-linear time.

- **Wraps:** `not` (`not()`, `features()`).
- **Proposed:** `not_wrapper(x, contrast = c("pcwsConstMean","pcwsLinContMean","pcwsLinMean","pcwsConstMeanVar"), ...)`.
- **Plot:** trend contrasts render as fitted broken lines rather than vertical
  rules — a visual NOT cannot currently produce easily.

#### 5.2.5 MOSUM — moving sum

MOSUM (Eichinger & Kirch, 2018) scans a moving-sum detector with bandwidth $G$,

$$
T_k(G) = \frac{1}{\hat\sigma}\sqrt{\frac{1}{2G}} \left| \sum_{i=k+1}^{k+G} y_i - \sum_{i=k-G+1}^{k} y_i \right|,
$$

flagging significant local maxima as changepoints. The `mosum` package
(Meier, Kirch & Cho, 2021) adds a *multiscale* combination over several
bandwidths (Cho & Kirch, 2022) and a bootstrap for **confidence intervals** of
changepoint locations.

- **Wraps:** `mosum` (`mosum()`, `multiscale.localPrune()`, `confint()`).
- **Proposed:** `mosum_wrapper(x, G = NULL, multiscale = FALSE, ci = FALSE, ...)`;
  `ci = TRUE` fills `ci_lower`/`ci_upper`.
- **Plot:** a two-panel `autoplot()` — series with changepoints on top, the
  MOSUM detector statistic with threshold and bandwidth band below.

#### 5.2.6 Isolate-Detect and TGUH

Isolate-Detect (Anastasiou & Fryzlewicz, 2022) expands intervals from the left
and right until a single changepoint is *isolated*, then detected — accurate
under frequent changes of small magnitude, for both piecewise-constant and
piecewise-linear signals, including heavy-tailed noise. TGUH (Fryzlewicz, 2018)
is a tail-greedy, bottom-up unbalanced-Haar decomposition giving fast,
consistent multiple-changepoint estimation.

- **Wraps:** `IDetect` (`ID()`, `pcm_th()`); `breakfast` (`sol.idetect()`,
  `sol.tguh()`).
- **Proposed:** `idetect_wrapper(x, ...)`; TGUH via `cpt_detect(method = "tguh")`.

### 5.3 Multiscale inference with confidence statements

#### 5.3.1 SMUCE / HSMUCE

SMUCE (Frick, Munk & Sieling, 2014) estimates a step function in an
exponential-family model by minimising the number of changepoints subject to a
*multiscale constraint*: the estimate must pass a multiscale test at level
$\alpha$ on every interval simultaneously. The level $\alpha$ has an
interpretation — it bounds the probability of over-estimating the number of
changepoints — and the method delivers **confidence intervals for changepoint
locations** and **confidence bands for the signal**. HSMUCE (Pein, Sieling &
Munk, 2017) extends this to heterogeneous (segment-varying) noise.

- **Wraps:** `stepR` (`stepFit()`, `confband()`, `confint()`).
- **Proposed:** `smuce_wrapper(x, alpha = 0.5, family = c("gauss","hsmuce","poisson"), ...)`.
- **Plot:** `autoplot()` draws the step estimate, a shaded confidence band for
  the signal, and horizontal whiskers for changepoint-location CIs — a
  capability the package entirely lacks today.

### 5.4 Nonparametric and distribution-free detection

#### 5.4.1 NMCD (already wrapped via `cpt.np`)

The nonparametric maximum-likelihood approach of Zou, Yin, Feng & Wang (2014)
defines a segment cost from the empirical distribution function, integrating a
quantile-indexed Bernoulli log-likelihood, and selects the number of
changepoints by BIC. Haynes, Fearnhead & Eckley (2017) make it computationally
efficient by solving it with PELT (and CROPS) — this is what `changepoint.np`
implements, reached today via `cpt_wrapper(change_in = "cpt_np")`. The next
release exposes its `nquantiles` argument and CROPS path explicitly.

#### 5.4.2 E-Divisive / E-Agglomerative (already wrapped)

The energy-distance methods of Matteson & James (2014), implemented in `ecp`,
detect changes in *distribution* for multivariate data of arbitrary dimension
using the divergence

$$
\mathcal{E}(X, Y; \alpha) = \frac{2}{mn}\sum_{i=1}^{m}\sum_{j=1}^{n}\lVert X_i - Y_j\rVert^\alpha
- \binom{m}{2}^{-1}\!\!\sum_{i<k}\lVert X_i - X_k\rVert^\alpha
- \binom{n}{2}^{-1}\!\!\sum_{j<l}\lVert Y_j - Y_l\rVert^\alpha,
\quad \alpha \in (0,2).
$$

`e.divisive` adds changepoints by bisection with a permutation test;
`e.agglo` merges. The next release lifts the current univariate-only treatment
to genuine **multivariate** input (see §5.6 and §7.5) and exposes `alpha`,
`sig.lvl`, and `R` (permutations).

#### 5.4.3 KCP — kernel change point

KCP (Arlot, Celisse & Harchaoui, 2019), as packaged in `kcpRS` (Cabrieto et
al.), maps data into an RKHS via a Gaussian kernel and minimises a within-phase
scatter criterion on running statistics (running mean, variance,
autocorrelation, correlation), with a permutation test for the existence of any
changepoint and model selection for their number. It captures changes in
*higher-order* structure (e.g. correlation) that mean/variance methods miss.

- **Wraps:** `kcpRS` (`kcpRS()`, `kcpRS_workflow()`).
- **Proposed:** `kcp_wrapper(x, running_stat = c("mean","var","ar","cor"), wsize = 25, ...)`.

#### 5.4.4 Sequential change-point models (CPM)

The CPM framework (Hawkins; Ross, 2015, `cpm`) provides distribution-free
*sequential* detection through repeated two-sample tests as data arrive:
Mann–Whitney (location), Mood (scale), Lepage (location+scale),
Kolmogorov–Smirnov and Cramér–von-Mises (general distribution), plus parametric
variants. It also runs in batch mode (`processStream`) to find multiple
changepoints. See also §5.9.

- **Wraps:** `cpm` (`detectChangePoint()`, `processStream()`).
- **Proposed:** `cpm_wrapper(x, cpm_type = "Mann-Whitney", arl0 = 500, startup = 20, ...)`.

### 5.5 Robustness and dependence

#### 5.5.1 Robust loss for outliers

Classical Gaussian costs inflate the changepoint count when the data contain
outliers or heavy tails. Fearnhead & Rigaill (2019) show that only *bounded*
loss functions are robust to arbitrarily extreme outliers, and give an exact
dynamic program minimising a penalised cost built from a bounded segment loss,

$$
\mathcal{C}(y_{a:b}) = \min_{\mu} \sum_{i=a}^{b} \gamma\!\left(\frac{y_i - \mu}{\sigma}\right),
\qquad
\gamma(u) = \min\!\left(u^2,\, K^2\right),
$$

with the truncated-quadratic $\gamma$ shown capping each point's influence at
$K^2$. The `robseg` implementation provides this bounded (biweight-style) loss
together with Huber and $L_1$ variants for heavy-tailed noise, consistently
recovering the number and locations of changes despite contamination.

- **Wraps:** `robseg` (`Rob_seg.std()`).
- **Proposed:** `robseg_wrapper(x, loss = c("biweight","huber","L1","L2"), lambda = ..., ...)`.

#### 5.5.2 DeCAFS — local fluctuations + autocorrelated noise

DeCAFS (Romano, Rigaill, Runge & Fearnhead, 2022) targets the very common
situation in which the mean *drifts* (a random-walk component) between abrupt
changes and the noise is *autocorrelated* (AR(1)). Naïve change-in-mean methods
badly over-count in this regime. DeCAFS minimises a penalised cost of the form

$$
\sum_{t} \Big[ (y_t - \mu_t)^2/\sigma^2 + (\mu_t - \mu_{t-1})^2/\eta^2 \cdot \mathbb{1}\{\text{no change}\} \Big] + \beta \cdot (\text{number of changes}),
$$

with an efficient dynamic program that copes with the cross-segment dependence.

- **Wraps:** `DeCAFS` (`DeCAFS()`, `estimateParameters()`).
- **Proposed:** `decafs_wrapper(x, beta = ..., model_param = NULL, ...)`.

#### 5.5.3 Self-normalisation (SNCP)

The self-normalisation framework of Zhao, Jiang & Shao (2022), implemented in
`SNSeg`, couples SN-based tests with a nested local-window segmentation. It is
nonparametric, robust to temporal dependence, **avoids estimating the long-run
variance**, and detects changes in *general parameters* — mean, variance,
quantiles, autocovariance, correlation — in a unified way, for univariate,
multivariate (`SNSeg_Multi`), and high-dimensional (`SNHD`) series.

- **Wraps:** `SNSeg` (`SNSeg_Uni()`, `SNSeg_Multi()`, `SNSeg_HD()`).
- **Proposed:** `sn_wrapper(x, parameter = c("mean","variance","acf","bivcor"), ...)`.

### 5.6 High-dimensional and multivariate detection

#### 5.6.1 inspect — sparse projection

For a $p$-variate series whose mean changes in an unknown *sparse* subset of
coordinates, `inspect` (Wang & Samworth, 2018) forms the CUSUM *matrix*, finds a
good projection direction as the leading left singular vector of a convex
relaxation (sparse SVD), projects to one dimension, and applies a univariate
detector — repeating via wild binary segmentation for multiple changes. It
comes with strong guarantees on the number and rate of localisation.

- **Wraps:** `InspectChangepoint` (`inspect()`).
- **Proposed:** `inspect_wrapper(X, lambda = NULL, threshold = NULL, ...)` where `X` is $n \times p$.
- **Plot:** see §7.5 (per-coordinate heatmap + projection direction).

#### 5.6.2 Sparsified Binary Segmentation

SBS (Cho & Fryzlewicz, 2015) thresholds and aggregates per-coordinate CUSUMs to
suppress noise coordinates before segmenting — strong when changes are sparse
across coordinates. Available via `hdbinseg` (`sbs.alg()`); also `dcbs.alg()`
(double-CUSUM).

- **Proposed:** `cpt_detect(method = "sbs")`.

#### 5.6.3 The `changepoints` collection

`changepoints` (Xu, Padilla, Wang, Yu & Li) packages a suite of recent,
theoretically-optimal estimators: univariate mean and **covariance**,
high-dimensional mean, **high-dimensional regression** (including under temporal
dependence; Xu, Wang, Zhao & Yu, 2024), **dynamic networks** (dynamic
Erdős–Rényi and random-dot-product graphs), and **VAR** models, several with
companion confidence procedures and a tuning-free variant (e.g.
`changepoints::CV.search.DP`).

- **Wraps:** `changepoints`.
- **Proposed:** `changepoints_wrapper(data, model = c("mean","cov","regression","network","var"), ...)`.

### 5.7 Bayesian changepoint detection

#### 5.7.1 bcp — product partition models

`bcp` (Erdman & Emerson, 2007) implements the Barry & Hartigan (1993) product
partition model via MCMC, returning a **posterior probability of a changepoint
at every location** and posterior segment means with credible intervals — a
fundamentally different (and very visualisable) output from a hard segmentation.
It supports univariate sequences and linear-regression changepoints.

- **Wraps:** `bcp` (`bcp()`).
- **Proposed:** `bcp_wrapper(x, ..., prob_threshold = 0.5)` → `changepoints`
  has a `posterior_prob` column; `segments` carries posterior means + CIs.
- **Plot:** `autoplot()` overlays posterior means with a credible band and a
  lower panel of per-location posterior changepoint probability (§7.7).

#### 5.7.2 BOCPD — Bayesian Online Changepoint Detection

BOCPD (Adams & MacKay, 2007) maintains, online, the posterior over the **run
length** $r_t$ (time since the last changepoint) via the message passing
recursion

$$
P(r_t, y_{1:t}) = \sum_{r_{t-1}} P(r_t \mid r_{t-1})\, P(y_t \mid r_{t-1}, y^{(r)}_{t})\, P(r_{t-1}, y_{1:t-1}),
$$

with a hazard function $H(\cdot)$ governing $P(r_t \mid r_{t-1})$ and a
conjugate predictive for the per-segment model (e.g. Gaussian, Poisson). The
`ocp` package implements it with Gaussian and user-defined predictives.

- **Wraps:** `ocp` (`onlineCPD()`).
- **Proposed:** `bocpd_wrapper(x, hazard_lambda = 250, ...)`.
- **Plot:** the run-length posterior heatmap (§7.7) — the signature BOCPD graphic.

### 5.8 Structural breaks in regression

#### 5.8.1 Bai–Perron and fluctuation tests (`strucchange`)

`strucchange` (Zeileis, Leisch, Hornik & Kleiber, 2002) tests and *dates*
structural change in linear regression. It provides (a) the generalized
fluctuation test framework (CUSUM/MOSUM of recursive or OLS residuals, and
estimates-based processes) for *detecting* instability, and (b) the Bai & Perron
(1998, 2003) dynamic program (`breakpoints()`) for *estimating* multiple breaks
in the coefficients of $y_t = x_t^\top \beta_j + \varepsilon_t$, with BIC/RSS
model selection and confidence intervals for the break dates.

- **Wraps:** `strucchange` (`breakpoints()`, `Fstats()`, `efp()`,
  `confint()`, `sctest()`).
- **Proposed:** `strucchange_wrapper(formula, data, breaks = NULL, ...)`.
- **Plot:** `autoplot()` shows fitted per-regime regression lines, break dates
  with CIs, and (optionally) the empirical fluctuation process with its boundary.

#### 5.8.2 segmented / broken-line regression

`segmented` (Muggeo, 2003, 2008) estimates *continuous* piecewise-linear
("broken-line") relationships: it finds breakpoints $\psi_k$ where the **slope**
changes, by an iterative (non-grid) procedure, returning breakpoint estimates
**with standard errors** and per-segment slopes (`slope()`, `davies.test()` for
existence). This complements jump-based detectors with the *trend-change* case.

- **Wraps:** `segmented` (`segmented()`, `slope()`, `confint.segmented()`).
- **Proposed:** `segmented_wrapper(model, seg_var, npsi = 1, ...)`.

### 5.9 Online and sequential detection

#### 5.9.1 FOCuS — functional online CUSUM

FOCuS (Romano, Eckley, Fearnhead & Rigaill, 2023) solves the online
change-in-mean problem by functional pruning of the CUSUM likelihood, which is
*equivalent to running the CUSUM test simultaneously for all possible
pre-change/post-change magnitudes and window sizes*, at amortised cost
logarithmic in the number of observations — removing the need to pre-specify the
size of change or a window.

- **Wraps:** `FOCuS`.
- **Proposed:** `focus_wrapper(x, threshold = NULL, ...)`; supports
  streaming use (one observation at a time) and batch.

#### 5.9.2 OCD — high-dimensional online detection

OCD (Chen, Wang & Samworth, 2022) detects a change in the mean of a
$p$-dimensional Gaussian stream online, by aggregating likelihood-ratio
statistics across scales and coordinates, with storage and per-observation
computation *independent of the number of past observations* and a guaranteed
average-run-length (patience) under the null.

- **Wraps:** `ocd` (`ChangepointDetector()`, `getStatistics()`).
- **Proposed:** `ocd_wrapper(X, thresh = "MC", ...)`.

#### 5.9.3 Sequential CPM

The `cpm` engine (§5.4.4) is the distribution-free workhorse for univariate
streams and is exposed both as a batch detector and, via a thin streaming
helper, for monitoring.

---

## 6. Penalty and model-selection toolkit

The Achilles heel of penalised methods is the penalty $\beta$. The next release
adds first-class tooling.

### 6.1 Penalty constants

A helper `cpt_penalty()` constructs the standard penalties as functions of
$(n, k)$: `None`, `SIC`/`BIC` ($k \log n$), `MBIC` (Zhang & Siegmund, 2007),
`AIC` ($2k$), `Hannan-Quinn`, `sSIC` (Fryzlewicz, 2014), and `Manual`
(an expression in `n`). These map onto the upstream engines' own penalty
arguments where supported and are applied by the package otherwise.

### 6.2 CROPS — Changepoints for a Range of PenaltieS

CROPS (Haynes, Eckley & Fearnhead, 2017) computes *all* optimal segmentations as
the penalty ranges over a continuous interval $[\beta_{\min}, \beta_{\max}]$,
exploiting that the number of distinct optimal segmentations is small and each is
found at PELT cost. This converts penalty selection from a guess into a
diagnostic: the analyst inspects how the segmentation changes with $\beta$ and
picks the "elbow."

- **Wraps:** `changepoint` (the `CROPS` penalty option) and/or `crops`.
- **Proposed:** `cpt_crops(x, method, change_in, pen_min, pen_max, ...)`
  returns a `ggcpt_path` object holding, for each penalty interval, the number
  of changepoints and the total unpenalised cost.

### 6.3 Diagnostic plots

- `autoplot()` on a `ggcpt_path`: the **elbow plot** — penalised/unpenalised
  cost (or $\beta$) against the number of changepoints — with the candidate
  models annotated.
- `ggcpt_pathplot()`: a faceted small-multiple showing the actual segmentation
  for each distinct number of changepoints along the CROPS path, so the analyst
  *sees* the models being chosen among.

---

## 7. The visualization layer

Visualization is the package's reason to exist; the next release treats it as a
first-class subsystem rather than two bespoke functions.

### 7.1 A single `autoplot()` / refreshed `ggcptplot()`

`autoplot.ggcpt()` renders any result with sensible, method-aware defaults:
the raw series (line + optional points), changepoints as vertical rules (the
0.1.0 look, preserved as default), and — toggled on by `show_segments` — the
fitted within-segment level as a step line drawn from the `segments` tibble.
All aesthetics (`cptline_color`, `cptline_type`, `cptline_size`,
`cptline_alpha`) keep their 0.1.0 names and defaults. The function returns a
plain `ggplot`, so `+ theme_*()`, `+ labs()`, `+ facet_*()` all continue to work
exactly as users expect.

### 7.2 New geoms and stats

To make changepoint layers composable like any other `ggplot2` layer:

- **`geom_changepoint()`** — vertical rules / `geom_vline`-style layer for a set
  of changepoints; the refactored core of the existing plots.
- **`geom_cpt_segment()`** — draws per-segment location estimates as horizontal
  step segments (segment means/medians/levels), from an `augment()`-ed frame.
- **`geom_cpt_ci()`** — horizontal whiskers or shaded x-bands for
  changepoint-location confidence intervals (consumes `ci_lower`/`ci_upper`
  from `mosum`, `stepR`, `strucchange`, `segmented`).
- **`stat_changepoint()`** — a stat that runs `cpt_detect()` inside the `ggplot`
  pipeline: `ggplot(df, aes(t, y)) + geom_line() + stat_changepoint(method = "pelt")`.

### 7.3 Multi-method comparison

`ggcpt_compare(x, methods = c("pelt","wbs","not","ecp"), ...)` runs several
detectors on the same series and renders them either as **facets** (one panel
per method, x-axes aligned) or **overlaid** (one panel, changepoints
colour-coded by method, optionally dodged on a rug). A companion
`ggcpt_compare_table()` returns the tidy union of results for numerical
comparison. This directly serves the original motivation — comparing methods on
the same data — which 0.1.0 can only do by hand.

### 7.4 Penalty-path and solution-path plots

- CROPS elbow + path plots (§6.3).
- **Solution-path plots** for randomised/multiscale methods: the ordered CUSUM
  contrasts of WBS/WBS2, the narrowest-over-threshold features of NOT, the
  detector curve of MOSUM with its threshold and bandwidth band, and the
  Isolate-Detect path — each showing *why* a changepoint was selected, not just
  where.

### 7.5 Multivariate and high-dimensional plots

- `ggcpt_facet(X, ...)` — small multiples: one panel per coordinate of an
  $n\times p$ matrix with shared changepoint rules drawn across all panels.
- `ggcpt_heatmap(X, ...)` — a tile/heatmap of the data (or per-coordinate CUSUM
  contributions) with changepoints as vertical lines; for `inspect`, a
  side panel shows the estimated sparse **projection direction** (which
  coordinates drive the change).
- Multivariate `ecp`/`changepoints` results plug directly into these.

### 7.6 Trend-change rendering

For slope-change methods (`not` with linear contrasts, `segmented`,
`gfpop` isotonic), `autoplot()` draws the **fitted broken line** with breakpoints
marked (and CIs from `segmented`), instead of vertical rules — the correct
visual idiom for trend changes.

### 7.7 Bayesian posterior plots

- **bcp:** a two-panel plot — posterior segment means with a credible ribbon on
  top, and per-location posterior changepoint probability as bars/area below,
  with the decision threshold drawn.
- **BOCPD:** the **run-length posterior heatmap** — time on the x-axis, run
  length on the y-axis, shaded by $P(r_t \mid y_{1:t})$, with the MAP run length
  traced — the canonical online-Bayesian diagnostic.

### 7.8 Theming and scales

A light `theme_ggcpt()` and helper scales/annotation utilities (e.g. shading
alternate segments with `annotate_segments()`, labelling changepoints with their
index/date) keep the output publication-ready while remaining a standard
`ggplot` the user can override.

---

## 8. Evaluation and benchmarking

To support method comparison and method development, the next release adds an
evaluation module implementing the metrics standard in the literature (notably
van den Burg & Williams, 2020, and the WBS/NOT simulation conventions).

### 8.1 Accuracy metrics

Given predicted changepoints $\hat{\mathcal{T}}$, ground truth
$\mathcal{T}^\star$, and series length $n$, `cpt_metrics()` returns a tidy row
with:

- **Precision / Recall / F1 with margin $M$.** A prediction $\hat\tau$ is a true
  positive if $\min_{\tau \in \mathcal{T}^\star} |\hat\tau - \tau| \le M$;
  $F_1 = 2PR/(P+R)$.
- **Covering metric** (van den Burg & Williams, 2020). With $\mathcal{G}$ and
  $\mathcal{P}$ the partitions induced by truth and prediction,

$$
C(\mathcal{G}, \mathcal{P}) = \frac{1}{n} \sum_{A \in \mathcal{G}} |A| \cdot \max_{A' \in \mathcal{P}} J(A, A'), \qquad J(A, A') = \frac{|A \cap A'|}{|A \cup A'|}.
$$

- **Hausdorff distance** between $\hat{\mathcal{T}}$ and $\mathcal{T}^\star$
  (worst-case localisation error).
- **(Adjusted) Rand index** between the induced segment labellings.
- **Annotation error** $|\,|\hat{\mathcal{T}}| - |\mathcal{T}^\star|\,|$ and
  **MAE/RMSE of matched locations**.

### 8.2 Multi-annotator evaluation

Real benchmarks (e.g. the Turing Change Point Dataset, van den Burg & Williams,
2020) carry *several* human annotations per series. `cpt_metrics_annotated()`
accepts a list of annotation sets and reports the averaged covering and F1 as in
that paper, so methods can be scored on TCPD-style data.

### 8.3 Visualization of evaluation

`ggcpt_eval(pred, truth, ...)` overlays predictions and ground truth on the
series with the $\pm M$ tolerance windows shaded, colouring true positives,
false positives, and misses — turning the metric into a picture.

---

## 9. Data simulation and benchmark signals

Reproducible methodology work needs reproducible data. The next release adds:

### 9.1 A general simulator

`cpt_simulate()` (alias `rcpt()`) generates piecewise data with known truth,
attached as an attribute for immediate scoring:

```r
cpt_simulate(n, changepoints, change_in = c("mean","var","meanvar","slope"),
             params, noise = c("gauss","t","ar1","rw"), sd = 1, rho = 0, seed = NULL)
```

It covers the regimes exercised in the literature: change in mean, variance,
mean+variance, and slope; Gaussian, Student-$t$ (heavy-tailed, for robustness
studies, cf. Fearnhead & Rigaill, 2019), AR(1) and random-walk-plus-noise
(for dependence studies, cf. DeCAFS), and multivariate sparse-mean changes
(for high-dimensional studies, cf. `inspect`).

### 9.2 Canonical test signals

The standard piecewise-constant test signals used across the WBS/NOT/SMUCE
literature — **`blocks`, `fms`, `mix`, `teeth`, `stairs`** (after
Donoho–Johnstone and Fryzlewicz) — ship as functions/datasets with documented
true changepoint locations, so examples and unit tests are anchored to the same
benchmarks the methods were validated on.

### 9.3 Real datasets

Beyond the existing genomics (`Lai2005fig4`), wind, and discoveries examples, we
document loaders/recipes for canonical real series used in the field
(well-log, coal-mining disasters, Nile, ACGH, server/HASC-style streams),
keeping heavy data in `Suggests` or external packages to respect CRAN size
limits.

---

## 10. Package architecture, dependencies, and testing

### 10.1 Dependency strategy

To keep installation light and CRAN-friendly, **only the original engines stay
in `Imports`** (`changepoint`, `changepoint.np`, `ecp`, plus the tidyverse
infrastructure). Every newly wrapped engine goes in **`Suggests`**, and each
wrapper guards with `rlang::check_installed()` / `requireNamespace()`, emitting
an actionable message if the engine is absent. This preserves P1 (wrap, don't
reinvent) without forcing a heavy dependency tree on users who want only a
subset.

Proposed `Suggests` additions: `fpop`, `gfpop`, `wbs`, `breakfast`, `not`,
`mosum`, `IDetect`, `stepR`, `kcpRS`, `cpm`, `robseg`, `DeCAFS`, `SNSeg`,
`InspectChangepoint`, `hdbinseg`, `changepoints`, `bcp`, `ocp`, `strucchange`,
`segmented`, `FOCuS`, `ocd`, `broom`, `generics`, `rlang`, `patchwork`
(for multi-panel layouts), `testthat (>= 3.0.0)`, `vdiffr` (for plot snapshot
tests). Some of these are not on CRAN (e.g. `robseg`, `FOCuS`); those wrappers
ship behind an optional install note and are excluded from CRAN examples.

### 10.2 File layout

```
R/
  ggcpt-class.R        # ggcpt constructor, print, format, validators
  broom-methods.R      # tidy/glance/augment
  autoplot.R           # autoplot.ggcpt + refreshed ggcptplot
  geoms.R              # geom_changepoint, geom_cpt_segment, geom_cpt_ci, stat_changepoint
  detect.R             # cpt_detect() dispatcher + cpt_penalty()
  wrap-optpart.R       # fpop, gfpop  (+ existing changepoint in changepoint.R)
  wrap-search.R        # wbs, wbs2, not, mosum, idetect, tguh
  wrap-multiscale.R    # smuce/hsmuce (stepR)
  wrap-nonparam.R      # kcp, cpm   (+ existing np/ecp)
  wrap-robust.R        # robseg, decafs, sn
  wrap-highdim.R       # inspect, sbs, changepoints
  wrap-bayes.R         # bcp, bocpd
  wrap-regression.R    # strucchange, segmented
  wrap-online.R        # focus, ocd, cpm-stream
  crops.R              # cpt_crops + path plots
  compare.R            # ggcpt_compare + table
  metrics.R            # cpt_metrics, cpt_metrics_annotated, ggcpt_eval
  simulate.R           # cpt_simulate / rcpt + test signals
```

### 10.3 Testing and documentation

- `testthat` unit tests per wrapper (skipped when the engine is absent via
  `skip_if_not_installed()`), asserting the tidy contract (column names/types)
  and round-trip with `tidy()`/`augment()`.
- `vdiffr` snapshot tests for the geoms and `autoplot()` methods.
- A second vignette, *"Comparing changepoint methods with ggchangepoint,"*
  written in the arXiv-paper register of the current `introduction` vignette:
  problem setup, the taxonomy, worked comparisons on the canonical signals and
  on real data, an evaluation study using §8, and a penalty-path case study.
- `pkgdown` reference index reorganised into the §3 families; `_pkgdown.yml`
  navbar gains the new vignette.

### 10.4 Parallelism, concurrency, and reproducible RNG

Several workloads here are time-consuming on large series or large studies, and
many are *embarrassingly parallel*. This subsection sketches where concurrency
pays off, the infrastructure to use, and — crucial for a package full of
stochastic methods — how to keep parallel runs reproducible. It is a design
sketch: parallel support is opt-in, ships incrementally (§11), and never changes
results, only wall-clock time.

#### 10.4.1 Where parallelism helps (and where it does not)

We distinguish three regimes:

- **Orchestration-level (embarrassingly parallel; our code; immediate win).**
  Loops the package itself drives are independent and can be farmed out directly:
  - `ggcpt_compare()` running *K* methods on one series (§7.3);
  - batch/panel detection — one univariate detector applied to each of many
    series, or to each of *p* coordinates of a matrix (`ggcpt_facet`, §7.5);
  - the Monte Carlo replications and method×penalty grids of the evaluation and
    simulation modules (§8, §9) — usually the single largest time sink in
    methodological work;
  - a coarse penalty grid (when not using the purpose-built, computation-reusing
    CROPS of §6);
  - permutation/bootstrap *replicates* that we drive ourselves.

- **Engine-internal (needs upstream support or a reimplementation; later).**
  Inside a single detector call there is often parallelism the upstream C/C++
  code does not expose: the per-candidate segment-cost evaluations of the
  PELT/OP/FPOP dynamic programs, the `nquantiles` empirical-CDF terms in the
  nonparametric cost of `cpt.np`, the pairwise energy-distance matrix and the
  permutation loop inside `ecp::e.divisive()`, and the *M* random-interval
  contrasts of WBS/NOT. These are real opportunities but live below our API; we
  treat them as upstream feature requests or, where warranted, as candidates for
  a parallel re-implementation of the hot loop (e.g. via `RcppParallel`).

- **Inherently sequential (do not parallelize).** The dynamic-programming
  recurrence itself — $F(t)$ depends on all $F(s),\,s<t$ — and the online stream
  updates of FOCuS/OCD/BOCPD are sequential by construction. For these the
  parallel axis is *across* problems (many streams, many series), never within
  one.

A separate, free win is **multithreaded linear algebra**: the SVD/eigen work in
`inspect` and several `changepoints` estimators runs through BLAS, so a
multithreaded BLAS (OpenBLAS/MKL — this cluster ships both BLAS-enabled and
`-no-openblas` R modules) speeds them up with no code change.

#### 10.4.2 Opportunity map

| Feature | Parallel unit | Regime | Strategy |
|---|---|---|---|
| `ggcpt_compare()` | method | orchestration | `future_map()` over methods |
| Panel / multi-series detection | series | orchestration | `future_map()` over series |
| Per-coordinate (HD screening, facets) | coordinate | orchestration | `future_map()` over columns |
| Simulation / benchmarking (§8–9) | replication × method × penalty | orchestration | `future_map()` over the grid |
| Permutation / bootstrap CIs we drive | resample | orchestration | chunked `future_map()` + parallel RNG |
| `ecp` distances / permutations | within `e.divisive` | engine-internal | upstream / `RcppParallel` |
| `cpt.np` quantile cost | within cost fn | engine-internal | upstream / `RcppParallel` |
| WBS / NOT interval contrasts | random interval | engine-internal | upstream |
| PELT / FPOP segment costs | candidate $s$ | engine-internal | upstream / hot-loop rewrite |
| `inspect`, `changepoints` (SVD) | matrix op | BLAS threads | multithreaded BLAS |
| DP recurrence; online updates | — | sequential | not parallelizable |

#### 10.4.3 Infrastructure: a backend-agnostic layer over `future`

We standardise on the **`future`** ecosystem — `future.apply::future_lapply()`
and `furrr::future_map()` — rather than calling `parallel::mclapply()` or
`foreach` directly, because it fits this package's users (HPC included — this
environment is itself module/scheduler-based):

- **One API, any backend.** The user chooses *how* to parallelise with a single
  `future::plan()`; our code is written once. `plan(sequential)` (default),
  `plan(multisession)` (PSOCK workers, all platforms), `plan(multicore)` (fork,
  Unix/macOS off-RStudio), `plan(cluster, workers = ...)` (sockets across nodes),
  and `future.batchtools` / `future.callr` for Slurm/SGE schedulers. The package
  never hard-codes a backend or a core count.
- **No platform traps.** With the default `sequential`, nothing forks
  unexpectedly; users opt in. We honour the ambient `plan()` and expose at most a
  light `parallel`/`workers` convenience that sets a plan locally and restores it
  on exit.
- **Progress and interrupts.** `progressr` gives backend-agnostic progress bars
  for long studies; `future` propagates errors and interrupts cleanly.

Backends and `future`/`future.apply`/`furrr`/`progressr` go in **`Suggests`**;
the sequential path adds no hard dependency (P1, §10.1).

#### 10.4.4 Reproducible parallel RNG (non-negotiable here)

Because so many methods are stochastic — `ecp` permutations, WBS/NOT random
intervals, `cpt.np` quantile sampling, `mosum`/`kcpRS` resampling, and the §9
simulator — naïve parallelism would *silently destroy reproducibility*: a single
`set.seed()` does not propagate to workers, and ordinary RNG streams overlap
across processes. The package must therefore use **parallel-safe L'Ecuyer-CMRG
streams**: `future.apply`/`furrr` provide them via `future.seed = TRUE` (and
`furrr_options(seed = TRUE)`), giving results *identical regardless of worker
count or backend*. This wires directly into the `seed` arguments proposed for the
stochastic wrappers in §12 (item 7): passing `seed` seeds the L'Ecuyer stream, so
a study is reproducible whether it runs on 1 core or 64. We will document this
prominently and test it (same `seed` ⇒ identical changepoints under
`plan(sequential)` and `plan(multisession)`).

#### 10.4.5 Design principles for parallelism

- **Opt-in, sequential by default.** Respects CRAN (examples/tests/vignettes
  capped at two cores via `_R_CHECK_LIMIT_CORES_`), keeps casual use simple, and
  avoids nested-parallelism blow-ups when our calls sit inside a user's own
  `future_map()`.
- **Parallelise the outer, coarse loop.** Distributing *methods*, *series*, or
  *replications* gives high compute-to-overhead ratios; we avoid parallelising
  tiny inner loops where process/transfer overhead dominates.
- **Overhead awareness.** Below a problem-size threshold we stay sequential
  automatically; chunking (`future.chunk.size`) and load-balancing absorb the
  heterogeneous per-method runtimes in `ggcpt_compare()` (a fast PELT beside a
  slow permutation test).
- **Thread/process hygiene.** Avoid oversubscription by not pairing a
  multithreaded BLAS with many forked workers unguarded (document
  `RhpcBLASctl::blas_set_num_threads()` for the HD methods): multithreading lives
  in the C++ engines/BLAS, multiprocessing in our `future` layer.

#### 10.4.6 API touchpoints

The parallel surface stays small and uniform: functions that loop —
`ggcpt_compare()`, the planned `cpt_batch()` panel detector, `cpt_simulate()`
studies, `cpt_metrics()` over a grid, and permutation-based wrappers — respect
the ambient `future::plan()` and a `seed`, with an optional `workers`
convenience. No detector's *result* depends on the plan; only its runtime does.

---

## 11. Phased release plan

We sequence the work so each release is independently shippable and CRAN-clean.

### v0.2.0 — Foundations + first new engines
- The `ggcpt` object, `broom` methods, `autoplot()`, the new geoms, and the
  refreshed-but-compatible `ggcptplot()`/`ggecpplot()`.
- `cpt_detect()` dispatcher and `cpt_penalty()`.
- First wave of wrappers where they add the most reach for least dependency
  weight: **WBS/WBS2 (`breakfast`/`wbs`)**, **NOT (`not`)**, **MOSUM
  (`mosum`)**, **FPOP (`fpop`)**.
- `ggcpt_compare()` + comparison vignette (initial).
- Evaluation module §8 (metrics + `ggcpt_eval`) and the simulator §9.1–9.2.
- **Hardening of the existing functions (§12):** the `ecp_wrapper()` no-change
  fix, the `size`→`linewidth` migration, `match.arg()`/input validation, the
  changepoint-convention reconciliation, and the first `testthat`/`vdiffr`
  tests.
- **Opt-in `future` parallelism (§10.4)** for the orchestration-level loops —
  `ggcpt_compare()`, batch/simulation studies, the metrics grid — with
  parallel-safe (L'Ecuyer) RNG wired to the wrappers' `seed`. (Engine-internal
  and BLAS-thread parallelism are explored in later milestones.)

### v0.3.0 — Inference, robustness, dependence
- **SMUCE/HSMUCE (`stepR`)** with confidence bands/intervals and
  `geom_cpt_ci()`.
- **Robust (`robseg`)**, **DeCAFS**, **self-normalisation (`SNSeg`)**.
- **CROPS** penalty-path toolkit and path plots (§6).
- **gfpop** graph-constrained detection and constrained-mean rendering.

### v0.4.0 — Bayesian, high-dimensional, regression
- **bcp** and **BOCPD (`ocp`)** with posterior plots (§7.7).
- **inspect**, **SBS (`hdbinseg`)**, the **`changepoints`** collection, and the
  multivariate/high-dimensional plots (§7.5).
- **`strucchange`** (Bai–Perron) and **`segmented`** with regression rendering
  (§7.6).

### v0.5.0 / v1.0.0 — Online + consolidation
- **CPM streaming**, **FOCuS**, **OCD** online detectors and streaming plots.
- TCPD-style multi-annotator benchmarking (§8.2) and a benchmarking vignette.
- API freeze, full `pkgdown` site, JOSS/R-Journal-style software paper
  describing the unified framework.

---

## 12. Auditing and hardening the existing functions

The four functions shipped in 0.1.0 — `cpt_wrapper()`, `ecp_wrapper()`,
`ggcptplot()`, `ggecpplot()` — will be **retained** (P6), but a close audit
surfaces real defects and design limitations the next release should fix. The
items are grouped into correctness, robustness/API, visualization, and
documentation/tooling. Findings marked **[verified]** were reproduced by running
the current code against `changepoint`, `changepoint.np`, `ecp`, and `ggplot2`
3.5.2 on R 4.4.1.

### 12.1 Correctness

1. **`ecp_wrapper()` reports spurious boundary changepoints (and an `NA`) on a
   series with no change. [verified]** `ecp::e.divisive()`/`e.agglo()` return
   `$estimates` that always include the two segment boundaries, `1` and `n+1`.
   The wrapper strips them only inside `if (length(cp) > 2)`. When no interior
   changepoint is found, `estimates == c(1, n+1)` has length 2, the guard is
   false, and the boundaries are returned as if they were changepoints — with
   `cp_value` for `n+1` indexing past the end of the data and so equal to `NA`.
   On a 300-point homogeneous Gaussian series:

   ```
   ecp_wrapper(x)            # x ~ N(0,1), genuinely no change
   #> # A tibble: 2 × 2
   #>      cp cp_value
   #>   <dbl>    <dbl>
   #> 1     1   -0.626
   #> 2   301   NA          # <- index n+1, out of bounds
   ```

   whereas `cpt_wrapper()` correctly returns **0 rows** on the same data. Fix:
   strip the first and last estimate unconditionally and return an empty tibble
   when no interior changepoint exists; add a regression test on a no-change
   series.

2. **`ecp_wrapper()` corrupts `cp_value` for multivariate / data-frame input —
   the very case `ecp` exists for. [verified]** Detection runs on
   `as.matrix(data)`, but `cp_value = data[cp]` indexes the *original* object.
   For an `n × p` matrix, `data[cp]` is a single column-major element (a
   bivariate change at index 101 silently yields the lone scalar `X[101]`); for a
   multi-column `data.frame`, `data[cp]` selects *columns* and errors:

   ```
   ecp_wrapper(X)                  # X is 200 x 2 -> cp_value is a lone X[101] scalar
   ecp_wrapper(as.data.frame(X))   #> Error: undefined columns selected
   ```

   Only a plain numeric vector is handled correctly. Fix: derive per-segment
   summaries from the matrix actually used for detection and populate a
   `segments` table (§4.1) when `p > 1`, instead of a scalar `cp_value`;
   normalise input shape once at entry. First-class multivariate support is in
   §5.6 and §7.5.

3. **The two engines report locations under different conventions. [verified]**
   For a change between observations 100 and 101, `changepoint::cpts()` reports
   **100** (last index of the left segment) while `ecp::e.divisive()` reports
   **101** (first index of the right segment) — an off-by-one. Since the
   package's raison d'être is *comparing* methods on the same data (§7.3),
   returning locations that are not on a common footing is a latent trap. Fix:
   document the convention, expose a normalised location (e.g. align both to
   "last index of the left segment") and a `cp_convention` attribute, so
   `ggcpt_compare()` and the §8 metrics compare like with like.

### 12.2 Robustness and API

4. **No input validation; enumerations use `==` instead of `match.arg()`.** Both
   wrappers branch on raw string equality (`change_in == "mean"`,
   `algorithm == "divisive"`). A typo such as `change_in = "Mean"` falls through
   to a generic `stop()`; nothing checks that `data` is numeric/finite/long
   enough, nor that the `change_in`×`cp_method` combination is legal (e.g.
   `cpt_np` supports only `PELT`). Adopt `match.arg()` for partial matching and
   accurate auto-generated errors, and validate inputs up front (P4).

5. **The fallback error message in `cpt_wrapper()` is inaccurate.** It reads
   `"Invalid Changepoint Method, must be mean_var, mean or var."` — it omits the
   supported `cpt_np` and names "Method" when the offending argument is
   `change_in`. `match.arg()` removes the hand-rolled message entirely.

6. **`change_in = "cpt_np"` is an inconsistent name.** The other values name
   *what* changes (`mean`, `var`, `mean_var`); `cpt_np` injects an upstream
   function name. Add a clearer alias `change_in = "np"` (and/or
   `"distribution"`) while keeping `"cpt_np"` for compatibility.

7. **Detection is recomputed inside the plot and can diverge for stochastic
   methods.** `ggcptplot()`/`ggecpplot()` re-run the wrapper internally, so a
   user who inspects `ecp_wrapper()` output and then plots runs detection twice;
   because `e.divisive()` uses a permutation test (and `cpt.np()` samples
   quantiles), the plotted changepoints can differ from the inspected ones unless
   the seed is set identically before each call — the fragile dance the vignette
   currently performs. Fix: let the plot functions accept an already-computed
   `ggcpt`/tibble (the `autoplot()` path of §4.4), and expose a `seed` argument on
   the stochastic wrappers for self-contained reproducibility.

### 12.3 Visualization

8. **`size =` is a live `ggplot2` deprecation (≥ 3.4.0). [verified]** Both plot
   functions pass `size = cptline_size` to `geom_linerange()`. Under `ggplot2`
   3.5.2 this emits:

   ```
   Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
   ℹ Please use `linewidth` instead.
   ```

   It warns today and will eventually error. Fix: rename the argument to
   `cptline_linewidth` and pass `linewidth`, keeping `cptline_size` as a
   `lifecycle`-soft-deprecated alias.

9. **Changepoint rules are only partial-height (`ymin = -Inf, ymax = cp_value`).**
   The vertical marker stops at the data value at the changepoint, so a change at
   a low value draws a short line and one at a high value a tall line — visually
   inconsistent and easy to miss. A changepoint is a property of its x-location,
   not of the y-value there. Default to full-height rules (`geom_vline`
   semantics), with the partial-height look available as an option; this becomes
   the refactored `geom_changepoint()` of §7.2.

10. **`geom_point()` is always drawn.** For the long series the package itself
    showcases (genomic aCGH, Irish wind), the point layer becomes an unreadable
    smear and slows rendering, with no toggle. Add `show_points` (auto-off above
    a length threshold) and `show_line`.

11. **The x-axis is hard-wired to `row_number`.** There is no way to supply a
    real time/date index, so timestamps cannot label the axis. Add an optional
    `index`/`x` argument, defaulting to position as today.

### 12.4 Documentation, packaging, tooling

12. **Typo and stale docs. [verified]** "tidyverse sytle" → "style" in the
    `cpt_wrapper()` roxygen; reconcile the `change_in` description and the error
    string with the actually-supported values.

13. **Modernise package-level docs and roxygen.** Replace the deprecated
    `@docType package ... NULL` idiom with the `"_PACKAGE"` sentinel; bump
    `RoxygenNote` (currently 7.1.2) and regenerate; the `globalVariables()` list
    can shrink as NSE is encapsulated inside the new geoms.

14. **No tests.** 0.1.0 ships without a `tests/` directory, so none of the above
    is caught by CI. v0.2.0 adds `testthat` coverage for the tidy contract and
    the edge cases above, plus `vdiffr` snapshots for the plots (§10.3).

**Relationship to backward compatibility.** Items 1, 8, and 12 are *bug fixes* —
they remove output that is already wrong (a spurious `NA` row, a deprecation
warning, a typo) and so are fully consistent with the compatibility commitment of
§13: the documented, intended behaviour is preserved while incorrect behaviour is
corrected. Anything that would alter a currently-"working" call (full-height
rules, the `linewidth` rename, the `np` alias) ships behind an option or a
`lifecycle` soft-deprecation, so existing scripts keep running.

---

## 13. Backward compatibility and deprecation

- `cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`, `ggecpplot()` remain exported
  with unchanged signatures, defaults, and return types. Internally they may be
  re-expressed on top of the new core, but their `tibble(cp, cp_value)` output
  and current plot appearance are covered by regression and `vdiffr` tests.
- New behaviour is strictly additive. The `change_in = "cpt_np"` typo-prone
  value is kept; a clearer alias `change_in = "np"` is added without removing the
  old one.
- No function is deprecated in 0.2.0. Should any wrapper later be subsumed by
  `cpt_detect()`, it will go through the standard `lifecycle` soft-deprecation
  path with at least one minor-version notice.

---

## Appendix A: Method → package → function map

| Method | Reference | Wrapped package | Proposed wrapper / access | Change type |
|---|---|---|---|---|
| Optimal Partitioning | Jackson et al. 2005 | `changepoint` | `cpt_wrapper()` | mean/var/meanvar |
| Segment Neighbourhood | Auger & Lawrence 1989 | `changepoint` | `cpt_wrapper(cp_method="SegNeigh")` | mean/var/meanvar |
| PELT | Killick, Fearnhead & Eckley 2012 | `changepoint` | `cpt_wrapper(cp_method="PELT")` | mean/var/meanvar |
| FPOP / SNIP | Maidstone et al. 2017 | `fpop` | `fpop_wrapper()` | mean |
| gfpop | Hocking et al. 2022 | `gfpop` | `gfpop_wrapper()` | constrained mean |
| CROPS | Haynes, Eckley & Fearnhead 2017 | `changepoint`/`crops` | `cpt_crops()` | any (penalty path) |
| Binary Segmentation | Scott & Knott 1974 | `changepoint` | `cpt_wrapper(cp_method="BinSeg")` | mean/var/meanvar |
| WBS | Fryzlewicz 2014 | `wbs`/`breakfast` | `wbs_wrapper()` | mean |
| WBS2 / SDLL | Fryzlewicz 2020 | `breakfast` | `cpt_detect("wbs2")` | mean |
| NOT | Baranowski, Chen & Fryzlewicz 2019 | `not` | `not_wrapper()` | mean/slope/var/meanvar |
| MOSUM | Eichinger & Kirch 2018; Meier et al. 2021 | `mosum` | `mosum_wrapper()` | mean (+ CIs) |
| Isolate-Detect | Anastasiou & Fryzlewicz 2022 | `IDetect`/`breakfast` | `idetect_wrapper()` | mean/slope |
| TGUH | Fryzlewicz 2018 | `breakfast` | `cpt_detect("tguh")` | mean |
| SMUCE / HSMUCE | Frick et al. 2014; Pein et al. 2017 | `stepR` | `smuce_wrapper()` | mean (+ CIs/bands) |
| NMCD (`cpt.np`) | Zou et al. 2014; Haynes et al. 2017 | `changepoint.np` | `cpt_wrapper(change_in="np")` | distribution |
| E-Divisive / E-Agglo | Matteson & James 2014 | `ecp` | `ecp_wrapper()` | distribution (multiv.) |
| KCP | Arlot et al. 2019; Cabrieto et al. | `kcpRS` | `kcp_wrapper()` | mean/var/ac/cor |
| CPM (sequential) | Hawkins; Ross 2015 | `cpm` | `cpm_wrapper()` | location/scale/dist |
| Robust loss | Fearnhead & Rigaill 2019 | `robseg` | `robseg_wrapper()` | mean (robust) |
| DeCAFS | Romano et al. 2022 | `DeCAFS` | `decafs_wrapper()` | mean + RW + AR(1) |
| Self-normalisation | Zhao, Jiang & Shao 2022 | `SNSeg` | `sn_wrapper()` | mean/var/quantile/acf |
| inspect | Wang & Samworth 2018 | `InspectChangepoint` | `inspect_wrapper()` | HD sparse mean |
| SBS / double-CUSUM | Cho & Fryzlewicz 2015; Cho 2016 | `hdbinseg` | `cpt_detect("sbs")` | HD mean |
| changepoints collection | Xu, Padilla, Wang, Yu, Li | `changepoints` | `changepoints_wrapper()` | HD mean/cov/reg/network/VAR |
| bcp | Erdman & Emerson 2007; Barry & Hartigan 1993 | `bcp` | `bcp_wrapper()` | mean (Bayesian) |
| BOCPD | Adams & MacKay 2007 | `ocp` | `bocpd_wrapper()` | online Bayesian |
| Bai–Perron / fluctuation | Bai & Perron 1998/2003; Zeileis et al. 2002 | `strucchange` | `strucchange_wrapper()` | regression breaks |
| segmented | Muggeo 2003/2008 | `segmented` | `segmented_wrapper()` | slope (continuous) |
| FOCuS | Romano et al. 2023 | `FOCuS` | `focus_wrapper()` | online mean |
| OCD | Chen, Wang & Samworth 2022 | `ocd` | `ocd_wrapper()` | HD online mean |

## Appendix B: Proposed API reference

```r
## Core
cpt_detect(x, method, change_in, penalty, ...) -> ggcpt
cpt_penalty(type, n, k, value = NULL)          -> numeric/function

## broom + ggplot2
tidy(x); glance(x); augment(x)                 # x: ggcpt
autoplot(x, show_segments = FALSE, show_ci = FALSE, ...) -> ggplot
ggcptplot(x, ...); ggecpplot(x, ...)           # accept ggcpt or raw data

## geoms / stats
geom_changepoint(...); geom_cpt_segment(...); geom_cpt_ci(...); stat_changepoint(...)

## thin wrappers (all -> ggcpt)
cpt_wrapper(); ecp_wrapper()                                  # existing
fpop_wrapper(); gfpop_wrapper()
wbs_wrapper(); not_wrapper(); mosum_wrapper(); idetect_wrapper()
smuce_wrapper(); kcp_wrapper(); cpm_wrapper()
robseg_wrapper(); decafs_wrapper(); sn_wrapper()
inspect_wrapper(); changepoints_wrapper()
bcp_wrapper(); bocpd_wrapper()
strucchange_wrapper(); segmented_wrapper()
focus_wrapper(); ocd_wrapper()

## penalty path
cpt_crops(x, method, change_in, pen_min, pen_max, ...) -> ggcpt_path
ggcpt_pathplot(path, ...) -> ggplot

## comparison
ggcpt_compare(x, methods, layout = c("facet","overlay"), ...) -> ggplot
ggcpt_compare_table(x, methods, ...) -> tibble

## multivariate / high-dimensional plots
ggcpt_facet(X, ...); ggcpt_heatmap(X, ...) -> ggplot

## evaluation
cpt_metrics(pred, truth, n, margin = 5) -> tibble
cpt_metrics_annotated(pred, annotations, n, margin = 5) -> tibble
ggcpt_eval(pred, truth, ...) -> ggplot

## simulation / data
cpt_simulate(n, changepoints, change_in, params, noise, sd, rho, seed) -> tibble (+ truth attr)
rcpt(...)  # alias

## parallelism (cross-cutting; see §10.4)
# Looping functions respect the ambient future::plan() and a `seed`:
future::plan(multisession, workers = 8)              # user chooses the backend
ggcpt_compare(x, methods, ..., seed = 1)             # methods fanned out, reproducibly
cpt_simulate(...) |> ... ; cpt_metrics(...)          # studies parallelised over the grid
cpt_batch(X, method, ..., workers = NULL, seed = 1)  # panel/per-series detector
```

---

## References

The following references are the basis for the proposed features. Venues and
years have been verified against the published record; arXiv identifiers are
given where useful.

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
    Institute of Statistical Mathematics** 74, 653–684. (Multiscale `mosum`
    extension.) The companion bootstrap location confidence intervals are Cho,
    H. and Kirch, C. (2022), *Bootstrap confidence intervals for multiple change
    points based on moving sum procedures*, **Computational Statistics & Data
    Analysis** 175, 107552.
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
