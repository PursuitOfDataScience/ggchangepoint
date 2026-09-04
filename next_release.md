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

**Part II — The roadmap that became 0.5.0** *(themes A–N removed as built;
what survives is the part still open)*

9. [The survey refreshed and CRAN-verified](#9-the-survey-refreshed-and-cran-verified)
26. [References](#26-references)

**Part III — The roadmap after 0.5.0**

27. [Where 0.5.0 leaves the package, and who else is in the field](#27-where-050-leaves-the-package-and-who-else-is-in-the-field)
28. [Theme O — Attribution: which coordinate changed?](#28-theme-o--attribution-which-coordinate-changed)
29. [Theme P — Distribution-free confidence sets](#29-theme-p--distribution-free-confidence-sets)
30. [Theme Q — Distributions beyond Gaussian](#30-theme-q--distributions-beyond-gaussian)
31. [Theme R — Genetic and metaheuristic search](#31-theme-r--genetic-and-metaheuristic-search)
32. [Theme S — Neural detectors as first-class citizens](#32-theme-s--neural-detectors-as-first-class-citizens)
33. [Theme T — Spatio-temporal changepoints](#33-theme-t--spatio-temporal-changepoints)
34. [Theme U — Scale: out-of-core and chunked detection](#34-theme-u--scale-out-of-core-and-chunked-detection)
35. [Theme V — Segment models and what comes after detection](#35-theme-v--segment-models-and-what-comes-after-detection)
36. [Theme W — Reporting, reproducibility and teaching](#36-theme-w--reporting-reproducibility-and-teaching)
37. [Theme X — The quality-of-life gaps](#37-theme-x--the-quality-of-life-gaps)
38. [Prioritisation for 0.6.0 and beyond](#38-prioritisation-for-060-and-beyond)
39. [Open questions for the next cycle](#39-open-questions-for-the-next-cycle)
40. [References for Part III](#40-references-for-part-iii)

**Part III (continued) — second pass**

41. [Theme Y — Panel and hierarchical changepoints](#41-theme-y--panel-and-hierarchical-changepoints)
42. [Theme Z — Effect size: how big was the change?](#42-theme-z--effect-size-how-big-was-the-change)
43. [Theme AA — Ecosystem interoperability](#43-theme-aa--ecosystem-interoperability)
44. [Theme AB — Robustness, contamination and breakdown](#44-theme-ab--robustness-contamination-and-breakdown)
45. [What this block changes about §38](#45-what-this-block-changes-about-38)
46. [Further references](#46-further-references)

**Part III (continued) — third pass**

47. [Theme AC — Gradual change](#47-theme-ac--gradual-change-the-assumption-all-fifty-engines-share)
48. [Theme AD — Practical significance, not just statistical](#48-theme-ad--practical-significance-not-just-statistical)
49. [Theme AE — Regime models: the adjacent field we do not speak to](#49-theme-ae--regime-models-the-adjacent-field-we-do-not-speak-to)
50. [Theme AF — Frequency-domain and time–frequency changes](#50-theme-af--frequency-domain-and-timefrequency-changes)
51. [Theme AG — Diagnostics after the segmentation](#51-theme-ag--diagnostics-after-the-segmentation)
52. [Where these sit against §38 and §45](#52-where-these-sit-against-38-and-45)
53. [Still-further references](#53-still-further-references)

**Part III — the map**

54. [The seventeen themes, sorted by what they are for](#54-the-seventeen-themes-sorted-by-what-they-are-actually-for)
55. [The consolidated ordering](#55-the-consolidated-ordering)
56. [The three things worth saying out loud](#56-the-three-things-worth-saying-out-loud)

**Part III (continued) — fourth pass**

57. [Theme AH — Epidemic changepoints: the change that comes back](#57-theme-ah--epidemic-changepoints-the-change-that-comes-back)
58. [Theme AI — Genomics, and the segmentation audience that already exists](#58-theme-ai--genomics-and-the-segmentation-audience-that-already-exists)
59. [Theme AJ — The package ships no data](#59-theme-aj--the-package-ships-no-data)
60. [Theme AK — Screening many series: multiplicity across the panel](#60-theme-ak--screening-many-series-multiplicity-across-the-panel)
61. [What the fourth pass changes](#61-what-the-fourth-pass-changes)
62. [Fourth-pass references](#62-fourth-pass-references)

**Part III (continued) — fifth pass**

63. [Theme AL — Data that is not a numeric vector on a regular grid](#63-theme-al--data-that-is-not-a-numeric-vector-on-a-regular-grid)
64. [Theme AM — Where the package runs: production and observability](#64-theme-am--where-the-package-runs-production-and-observability)
65. [Theme AN — Machine-readable output](#65-theme-an--machine-readable-output-for-the-readers-who-are-not-human)
66. [Two things deliberately *not* proposed, and why](#66-two-things-deliberately-not-proposed-and-why)
67. [What the fifth pass changes](#67-what-the-fifth-pass-changes)
68. [Fifth-pass references](#68-fifth-pass-references)

**Part III (continued) — sixth pass**

69. [Theme AO — Be a real ggplot2 extension](#69-theme-ao--be-a-real-ggplot2-extension-not-a-wrapper-over-one)
70. [Theme AP — The plots that are still missing](#70-theme-ap--the-plots-that-are-still-missing)
71. [Theme AQ — Choosing, explained: the decision surface](#71-theme-aq--choosing-explained-the-decision-surface)
72. [Theme AR — Testing the way the package's own history says to](#72-theme-ar--testing-the-way-the-packages-own-history-says-to)
73. [What the sixth pass changes](#73-what-the-sixth-pass-changes)
74. [Sixth-pass references](#74-sixth-pass-references)

**Part III (continued) — seventh pass**

75. [Theme AS — Statistical process control](#75-theme-as--statistical-process-control-the-same-mathematics-a)
76. [Theme AT — Reproducibility across engine versions](#76-theme-at--reproducibility-across-engine-versions)
77. [Theme AU — Preprocessing is a decision nobody records](#77-theme-au--preprocessing-is-a-decision-nobody-records)
78. [Theme AV — The Python bridge, in the direction nobody built](#78-theme-av--the-python-bridge-in-the-direction-nobody-built)
79. [What the seventh pass changes](#79-what-the-seventh-pass-changes)
80. [Seventh-pass references](#80-seventh-pass-references)

**Part III (continued) — eighth pass**

81. [Theme AW — Errors a program can catch](#81-theme-aw--errors-a-program-can-catch)
82. [Theme AX — The changepoint convention, asserted but never verified](#82-theme-ax--the-changepoint-convention-asserted-but-never-verified)
83. [Theme AY — Growing past one maintainer](#83-theme-ay--growing-past-one-maintainer)
84. [Theme AZ — What the package costs to install](#84-theme-az--what-the-package-costs-to-install)
85. [What the eighth pass changes](#85-what-the-eighth-pass-changes)
86. [Eighth-pass references](#86-eighth-pass-references)

**Part III (continued) — ninth pass**

87. [Theme BA — Testing a date you already have in mind](#87-theme-ba--testing-a-date-you-already-have-in-mind)
88. [Theme BB — Costs are asymmetric and nothing lets you say so](#88-theme-bb--costs-are-asymmetric-and-nothing-lets-you-say-so)
89. [Theme BC — Method shopping, and the warning nobody gives](#89-theme-bc--method-shopping-and-the-warning-nobody-gives)
90. [Theme BD — How many datasets does a benchmark need?](#90-theme-bd--how-many-datasets-does-a-benchmark-need)
91. [What the ninth pass changes](#91-what-the-ninth-pass-changes)
92. [Ninth-pass references](#92-ninth-pass-references)

**Part III (continued) — tenth pass**

93. [Theme BE — "No changepoints detected" is not an answer](#93-theme-be--no-changepoints-detected-is-not-an-answer)
94. [Theme BF — Uncertainty about K](#94-theme-bf--uncertainty-about-k)
95. [Theme BG — Does the package's own inference behave?](#95-theme-bg--does-the-packages-own-inference-behave)
96. [Theme BH — A catalogue of how changepoint analysis goes wrong](#96-theme-bh--a-catalogue-of-how-changepoint-analysis-goes-wrong)
97. [What the tenth pass changes](#97-what-the-tenth-pass-changes)
98. [Tenth-pass references](#98-tenth-pass-references)

**Part III (continued) — eleventh pass**

99. [Theme BI — The same series at two resolutions](#99-theme-bi--the-same-series-at-two-resolutions-is-two-different-questions)
100. [Theme BJ — What happens when an engine leaves CRAN](#100-theme-bj--what-happens-when-an-engine-leaves-cran)
101. [Theme BK — Two reproducibility hazards already met](#101-theme-bk--two-reproducibility-hazards-the-package-has-already-met)
102. [Theme BL — What the package costs to load](#102-theme-bl--what-the-package-costs-to-load)
103. [Theme BM — Automated explanation, treated honestly](#103-theme-bm--automated-explanation-treated-honestly)
104. [Where this document stands after eleven passes](#104-where-this-document-stands-after-eleven-passes)
105. [Eleventh-pass references](#105-eleventh-pass-references)

**Part III (continued) — twelfth pass**

106. [The dependency world, checked against the live CRAN index](#106-the-dependency-world-checked-against-the-live-cran-index)
107. [One genuinely new find: `ChangepointTesting`](#107-one-genuinely-new-find-changepointtesting)
108. [What the twelfth pass changes](#108-what-the-twelfth-pass-changes)
109. [Twelfth-pass references](#109-twelfth-pass-references)

**Part III (continued) — thirteenth pass: feasibility and the 0.6.0 spec**

110. [Feasibility notes: what the top proposals must deal with](#110-feasibility-notes-what-the-top-proposals-actually-have-to-deal-with)
111. [**The consolidated 0.6.0 specification**](#111-the-consolidated-060-specification)
112. [What the thirteenth pass changes](#112-what-the-thirteenth-pass-changes)

**Part III (continued) — fourteenth pass: premises checked**

113. [The premise audit](#113-the-premise-audit)
114. [What is genuinely unverified](#114-what-is-genuinely-unverified-and-stays-that-way-for-now)
115. [The one new proposal, which is not a feature](#115-the-one-new-proposal-which-is-not-a-feature)
116. [What the fourteenth pass changes](#116-what-the-fourteenth-pass-changes)

**Part III (continued) — fifteenth pass: the first evidence**

117. [What people actually download](#117-what-people-actually-download)
118. [**What this says, and it is not comfortable**](#118-what-this-says-and-it-is-not-comfortable)
119. [The revision this forces on §111](#119-the-revision-this-forces-on-111)
120. [What the fifteenth pass changes](#120-what-the-fifteenth-pass-changes)

**Part III (continued) — sixteenth pass: the biggest audience**

121. [**Theme BN — Regression breakpoints as a first-class citizen**](#121-theme-bn--regression-breakpoints-as-a-first-class-citizen)
122. [What the sixteenth pass changes](#122-what-the-sixteenth-pass-changes)

**Part III (continued) — seventeenth pass: the capability-gap sweep**

123. [The capability-gap sweep](#123-the-capability-gap-sweep)
124. [What this implies for the design](#124-what-this-implies-for-the-design)
125. [What the seventeenth pass changes](#125-what-the-seventeenth-pass-changes)

**Part III (continued) — eighteenth pass: the narrowing audit**

126. [**The narrowing audit: `fastcpd`**](#126-the-narrowing-audit-fastcpd)
127. [What this says about how the package was built](#127-what-this-says-about-how-the-package-was-built)
128. [What the eighteenth pass changes](#128-what-the-eighteenth-pass-changes)

**Part III (continued) — nineteenth pass: the discarded-output audit**

129. [The discarded-output audit](#129-the-discarded-output-audit)
130. [What to do about it](#130-what-to-do-about-it)
131. [What the nineteenth pass changes](#131-what-the-nineteenth-pass-changes)

**Part III (continued) — twentieth pass: registry versus reality**

132. [The registry-versus-reality audit](#132-the-registry-versus-reality-audit)
133. [What to fix](#133-what-to-fix)
134. [What the twentieth pass changes](#134-what-the-twentieth-pass-changes)

**Part III (continued) — twenty-first pass: instrumentability**

135. [The coverage number, and why there isn't one](#135-the-coverage-number-and-why-there-isnt-one)
136. [What this re-scopes](#136-what-this-re-scopes)
137. [What the twenty-first pass changes](#137-what-the-twenty-first-pass-changes)

**Part III (continued) — twenty-second pass: coverage measured**

138. [**Coverage, measured**](#138-coverage-measured)
139. [What this changes](#139-what-this-changes)
140. [What the twenty-second pass changes](#140-what-the-twenty-second-pass-changes)

**Part III (continued) — twenty-third pass: exports never executed**

141. [Which exports the suite never runs](#141-which-exports-the-suite-never-runs)
142. [What this adds to the plan](#142-what-this-adds-to-the-plan)

**Part IV — parallel research**

143. [The CRAN blind spot: work that does not say "changepoint"](#143-the-cran-blind-spot-changepoint-work-that-does-not-say-changepoint)
144. [§59 was wrong: the package need not bundle data](#144-59-was-wrong-the-package-does-not-need-to-bundle-data)
145. [What Python and Julia do that R does not](#145-what-the-python-and-julia-ecosystems-do-that-r-does-not)
146. [What Part IV changes so far](#146-what-part-iv-changes-so-far)
147. [**What users actually ask — §115's evidence, finally**](#147-what-users-actually-ask--115s-evidence-finally)
148. [**What four applied fields expect**](#148-what-four-applied-fields-expect-that-a-detector-does-not-give)
149. [GitHub-only implementations](#149-github-only-implementations--the-extension-mechanisms-actual-inventory)
150. [**What Part IV changed, in total**](#150-what-part-iv-changed-in-total)

**Part V — the plan, after the evidence** *(supersedes §111)*

151. [What the evidence actually says](#151-what-the-evidence-actually-says)
152. [**0.6.0 — reframed**](#152-060--reframed)
153. [0.7.0 — the inferential release](#153-070--the-inferential-release-now-applied-justified)
154. [The one structural question to settle first](#154-the-one-structural-question-to-settle-first)
155. [What this plan is not](#155-what-this-plan-is-not)

**Part V (continued) — resolving §154**

156. [The problem the grammar would solve](#156-the-problem-the-grammar-would-solve)
157. [What the grammar would look like in R](#157-what-the-grammar-would-look-like-in-r)
158. [**The recommendation**](#158-the-recommendation)
159. [§154 is now closed](#159-154-is-now-closed)

**Part V (continued) — the test, and a correction**

160. [**The result — objection 1 withdrawn**](#160-the-result)
161. [Does the recommendation change?](#161-does-the-recommendation-change)
162. [What this pass demonstrates](#162-what-this-pass-demonstrates)

**Part V (continued) — the second premise audit**

163. [Eight claims re-checked](#163-eight-claims-re-checked)
164. [§84 measured: right claim, wrong reason](#164-84-measured-the-claim-was-right-the-reason-was-not)
165. [**Running tally of the document's own reliability**](#165-running-tally-of-the-documents-own-reliability)

**Part V (continued) — measuring the top priority**

166. [The Bai–Perron template, item by item](#166-the-baiperron-template-item-by-item)
167. [What the package does with it](#167-what-the-package-does-with-it)
168. [What this changes in the plan](#168-what-this-changes-in-the-plan)
169. [**The dominant defect class is discard, not absence**](#169-and-the-honest-ledger-entry)

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

Thirty-seven defects were found and fixed in the same cycle. Three were
introduced by this release's own code; S7–S8 and S12 are the kind that only
surface when someone reads what an engine actually returns rather than what
its documentation implies; S17–S37 came out of the post-implementation audit
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


| S37 | **`cpt_methods()` loaded all thirty-five engine namespaces to fill in one column**, and on macOS that killed the vignette rebuild with no error to show | Asking `requireNamespace()` whether a package is *installed* answers a different question — it loads it — and loading is not free or safe. Building the status table pulled in every engine, including `fabisearch`, which pulls in `rgl`, which on the macOS runner fails in `dyn.load()` because there is no `libGLU`. The R CMD check step reported `Vignette re-building failed` with no chunk, no line and no message, because the failure was not an R condition: it was a namespace load dying in a subprocess. Four of the six vignettes were affected and the two either side of them survived, which is why no property of the individual files explained it — a `buildVignettes()` probe on the runner itself returned `"ok"` after two vignettes having produced no output at all. `find.package()` answers the actual question and touches nothing: `cpt_methods()` drops from seconds to 0.03s, loads zero namespaces, stays silent, and the rgl `dyn.load` NOTE under "checking dependencies in R code" goes with it. `need_pkg()` still calls `requireNamespace()` at the point a wrapper genuinely needs the engine loaded, which is where that belongs. |


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

*This part was the plan for 0.5.0 and has been carried out. Themes A–N
(§10–§23), the six prioritisation waves (§24) and the eight open questions
(§25) are all removed as built — Part 0 is the record of what they became,
and §0.9 lists the six items they left open. §8 went with them: it described
a 0.4.0 surface and a set of gaps that no longer exist. What survives here is
§9, and only the rows of it that are still unwired.*

## 9. The survey refreshed and CRAN-verified

The 0.4.0 survey (§2) covered nine method areas. This refresh does two things
it did not: it checks **CRAN availability against the live index** rather than
secondary sources, and it looks specifically for capability *classes* the
package cannot express at all. Versions are as of July 2026.

### 9.1 CRAN-live engines that are not yet wired

| Package | Version | What it adds that 0.4.0 cannot do | Theme |
|---|---|---|---|
| **`hdbinseg`** | 1.0.3 | Cho's SBS/DCBS high-dimensional binary segmentation — live on CRAN, which is why `sbs` is a wrapper task and not an archive wait (§5.4) | §17 |
| **`VARcpDetectOnline`** | 0.2.1 | Sequential detection for high-dimensional **VAR** models | §16, §17 |
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

---

# Part III — The roadmap after 0.5.0

*Written 2026-08-29, with 0.5.0 finished and ready to submit. Part II asked
"what methods are we missing?" and answered it: fifty engines. That question
is now the wrong one. This part asks a different one — **what can a person do
with a changepoint that this package still cannot help them do?** — and the
answers are mostly not engines. As before, this is a design and literature
document, not a commitment, and no package code is changed by writing it.*

## 27. Where 0.5.0 leaves the package, and who else is in the field

### 27.1 The shape of the surface now

Fifty wired methods, 130 exported objects, and a result class that carries
regions, diagnostics, a time index and a provenance flag. The engine count is
no longer the interesting number. The interesting number is that of the nine
`change_in` levels, **every one is Gaussian-flavoured**; that of fifty
methods, **not one can say which coordinate moved**; and that of the four
interval provenances in `cpt_confint()`, **only two carry a guarantee**.

### 27.2 A direct competitor now exists, and it is worth being precise about

`tidychangepoint` (Baumer et al., *Computational Statistics* 2026;
arXiv:2407.14369) reached CRAN with the same one-sentence pitch this package
uses: a tidyverse-compliant, `broom`- and `ggplot2`-based common interface to
several changepoint algorithms, with a `tidycpt` S3 class produced by
`segment()`.

Being honest about the overlap is more useful than ignoring it:

| | `ggchangepoint` 0.5.0 | `tidychangepoint` |
|---|---|---|
| Engines | 50, plus registration for anything else | `changepoint`, `wbs`, `strucchange`, `segmented`, `GA`, `changepointGA` |
| Data | univariate, multivariate, high-dimensional, functional, network, regression | **univariate only** |
| Inference | NSP regions, four-provenance `cpt_confint()`, `cpt_test()` | — |
| Supervised | labels, label error, learned penalties | — |
| Streaming | `cpt_monitor()`/`cpt_update()`/`cpt_delay()` | — |
| Benchmarking | `cpt_benchmark()`, TCPD, Nemenyi | — |
| **Segment model fitting** | `param_estimate` only | **`fit_*()` model objects, `fitness()`, penalty tooling** |
| **Genetic search** | — | **GA and island GA** |

Two conclusions. First, **do not chase parity on the tidy interface** — we
are ahead of it, and a second tidy wrapper is not what anyone needs. Second,
the two cells where they are ahead are both real and both worth having: an
explicit **segment-model layer** (§35) and **genetic search** (§31). Neither
is a reaction to a competitor; both are things a user of this package can
reasonably ask for today and not get.

### 27.3 The organising idea for 0.6.0 and after

Part II grew the package *outward* — more engines, more data types. Part III
should grow it *downward*, into the questions that follow a detection:

- **Which** coordinate changed, and can you certify it? (§28)
- **How sure** are you about *where*, without assuming a model? (§29)
- Does any of this work when the data are **counts**? (§30)
- What do I **do** with the segmentation now? (§35)
- Will it run on **my** data, which is 400 million rows and has gaps? (§34, §37)

## 28. Theme O — Attribution: which coordinate changed?

### 28.1 The gap, stated precisely

`cpt_detect()` handles multivariate input through nine engines — `ecp`,
`inspect`, `ocd`, `geomcp`, `npmojo`, `esac`, `pilliat`, `hdcov`, `network`,
`var`, `hdreg`, and the functional trio. Every one of them returns
**locations only**. A user with 200 sensors is told that something changed at
t = 4,181 and is given no help whatsoever with the only question they
actually have, which is *which sensors*.

This is the single largest unanswered question in the package, and it is
independent of the detector — which is exactly what makes it tractable.

### 28.2 The literature is new and fits the architecture

- **ARM — Attribution by Rank Maxima** (arXiv:2608.01691, 2026). A *wrapper*
  that takes a changepoint located by an arbitrary detector and returns the
  set of coordinates certified to have changed, each labelled *location* or
  *scale*. Three finite-sample guarantees: per-coordinate validity **under any
  detector**, exact family-wise error control by permutation, and FDR control
  in high dimensions. "Detector-agnostic" is the property that matters here:
  it composes with all fifty engines and with anything registered.
- **CROC — conformal root cause analysis** (arXiv:2607.26481, 2026). Returns
  a confidence set for the root-cause *stream* with user-specified coverage
  and no parametric assumptions; weighted variants downweight corrupted
  observations.

### 28.3 Proposed API

```r
cpt_attribute(object,
              method  = c("arm", "permutation", "conformal"),
              alpha   = 0.05,
              control = c("fwer", "fdr"),
              B       = 999,
              seed    = NULL)
```

Returns a tibble with one row per (changepoint x coordinate):
`cp`, `cp_index`, `coordinate`, `statistic`, `p_value`, `p_adjusted`,
`changed` (logical), `type` (`"location"` / `"scale"` / `NA`), and `source`.
The `source` column mirrors `cpt_confint()`'s, so provenance is visible in
the same way everywhere.

Display:

```r
autoplot(att)                       # coordinate x changepoint heat map,
                                    # certified cells outlined
geom_cpt_attribution()              # the layer, for composing by hand
autoplot(fit, type = "attribution") # the panel, next to statistic/path/scale_space
```

### 28.4 Why this is the right next big thing

It needs **no new hard dependency** and no engine: ARM is a permutation
argument over the coordinates at a known location, implementable in base R
plus the package's own machinery. It applies to every multivariate result the
package can already produce, including registered ones. It is the natural
companion to `cpt_confint()` — that answers *where*, this answers *what* —
and together they turn a location into a finding. And it is a genuinely novel
plot, which is this package's declared specialty.

**Risk to name:** ARM is a 2026 preprint. The permutation and conformal
routes should be implemented such that the ARM-specific ranking is one
`method` among three, so the theme survives if that paper does not.

## 29. Theme P — Distribution-free confidence sets

### 29.1 What `cpt_confint()` can and cannot promise

0.5.0 unified four provenances behind one contract, and the `source` column
is honest about which is which. But of the fifty methods, only seven declare
`ci = TRUE`. For the other forty-three the auto route is the **bootstrap**,
which re-runs the detector on resampled segments — a procedure with no
finite-sample coverage guarantee, whose interval can be badly wrong exactly
when the segmentation is uncertain, which is when a user most needs it.

### 29.2 CONCH

**Conformal changepoint localisation** (arXiv:2607.26481, 2026) returns a
confidence set containing the true changepoint with user-specified
probability, distribution-free, with weighted variants (W-CONCH) that
downweight corrupted observations while shrinking the set. That is a
strictly stronger statement than the bootstrap gives, for the forty-three
engines that currently get the bootstrap.

Proposed: a fifth provenance.

```r
cpt_confint(fit, method = "conformal", level = 0.95)
#> source = "conch"
```

and `method = "auto"` prefers `native` > `nsp_region` > `posterior` >
**`conformal`** > `bootstrap`, so the bootstrap becomes the last resort it
should always have been.

### 29.3 The measurement that has to come with it

0.5.0's hardest-won lesson (S11: the e-detector's average-run-length bound
was wrong by a factor of two while every call succeeded) says the test must
measure the quantity. So this theme ships with:

```r
cpt_confint_coverage(n_sim = 500, methods = ..., level = 0.95, ...)
```

a Monte Carlo study returning **realised coverage** and mean interval width
per method and provenance — which is both the regression test for this
feature and, published in a vignette, a genuinely useful table that does not
currently exist anywhere for R changepoint packages.

## 30. Theme Q — Distributions beyond Gaussian

### 30.1 A verified gap that locks out whole fields

`cpt_change_in_levels()` has nine entries and every one of them assumes a
Gaussian-flavoured cost. Meanwhile, sitting unreachable inside engines the
package **already depends on**:

| Engine | Cost available | Reachable from `cpt_detect()`? |
|---|---|---|
| `changepoint::cpt.meanvar(test.stat=)` | `"Poisson"`, `"Gamma"`, `"Exponential"` | **no** |
| `binsegRcpp` | `poisson`, `laplace`, `l1` (median) | **no** — only `mean_norm`/`meanvar_norm` are mapped |
| `fastcpd` | binomial, Poisson, negative-binomial families | partially, via `...` only |

So the package can detect a change in the mean of a Gaussian fifty ways and
cannot detect a change in a **rate** at all, through engines it already
imports. That is not a missing engine; it is a missing argument.

> **Correction (§123, seventeenth pass): this is overstated.** The costs are
> reachable *today* through `...` — `cpt_detect(y, method = "pelt",
> change_in = "meanvar", test.stat = "Poisson")` runs and finds the change.
> What is missing is not the capability but the **vocabulary**: no `family`
> argument, no registry column, no validation, no discoverability, and the
> user must know the engine's own argument name. The theme stands; its
> framing changes from "add the capability" to "name what is already there".
> `binsegRcpp`'s `poisson`/`laplace`/`l1` remain genuinely unreachable, since
> that wrapper maps `change_in` to a fixed distribution.

### 30.2 Proposed API

```r
cpt_detect(x, method = "pelt", family = "poisson")
cpt_detect(x, method = "binsegrcpp", family = "l1")     # robust / median cost
```

`family = c("gaussian", "poisson", "gamma", "exponential", "laplace", "l1",
"binomial", "negbin")`, defaulting to `"gaussian"`, mapped per engine in the
registry as a new list column, and refusing — by name, in the registry's
voice — where an engine has no such cost. `cpt_methods()` gains a `families`
column; `cpt_recommend()` gains a `family` filter.

`cpt_simulate(family =)` follows, so power analysis (`cpt_power()`) and
benchmarking (`cpt_benchmark()`) cover counts too. A Poisson signal with a
rate change is a different detection problem from a Gaussian mean shift and
the power curves are not interchangeable.

### 30.3 Who this reaches

Epidemiology (case counts, notification rates), reliability engineering
(inter-failure times — the exponential cost), web and product analytics
(event rates, conversion counts), quality control (defect counts, the
audience `taylor` was wired for), ecology (abundance), and finance
(trade-count intensity). Every one of them currently has to pretend their
counts are Gaussian or leave.

**This is the highest ratio of audience-unlocked to work-required in the
whole of Part III**, and it should be in 0.6.0.

## 31. Theme R — Genetic and metaheuristic search

Five search paradigms are wired: penalised optimal partitioning, greedy and
binary segmentation, multiscale/randomised intervals, Bayesian sampling, and
nonparametric/kernel. **Metaheuristic search is the sixth and it is absent.**

`changepointGA` (CRAN, 2026) implements genetic-algorithm changepoint search
including an island model, and `GA` provides the general machinery.
`tidychangepoint` wraps both, which is the clearest signal that users want
them.

The methodological case is not "another search": it is that GA does not
require the objective to **decompose over segments**. PELT's dynamic
programming needs additivity; the moment a user wants an ARIMA model per
segment, or a penalty that depends on the whole configuration, the optimal
methods cannot help and the greedy ones are all that is left. GA fills
exactly that hole, and it is the hole §35's segment models will open.

Proposed registry entries `ga` and `ga_island`, `change_in` of `"meanvar"`
and a new `"arima"`, with `seed` and generation count exposed and the
population trace available through `cpt_solution_path()` — a genuinely new
kind of solution path to draw.

## 32. Theme S — Neural detectors as first-class citizens

0.5.0's position was: do not implement neural detectors, be able to plot and
score them. That position is right and should be kept. Two things have
changed since:

1. **`scanCP` reached CRAN (2026)** — deep-learning changepoint detection
   fitting localised feed-forward networks to a smooth component and building
   a residual-based detector. It is the first neural detector this package
   can *wrap* rather than merely register. One registry row, one wrapper.
2. **Nothing in the package helps a user evaluate a detector they trained.**
   `cpt_register_method()` gets their model in; `cpt_benchmark()` scores it on
   labelled data. What is missing is the piece between: a train/validate split
   over a *collection* of labelled series, which is how a neural detector is
   actually developed.

Proposed: `cpt_learn()` as the companion to `cpt_learn_penalty()` — same
labelled-series input, same `cpt_label_error()` accounting, but holding out
whole series rather than sweeping a penalty. It costs no new dependency
because the user supplies the learner.

## 33. Theme T — Spatio-temporal changepoints

The 2024–2026 literature has arrived and CRAN has not caught up:

- GAM-based spatio-temporal changepoint detection (*Statistics and
  Computing*, 2024);
- likelihood-based temporal changepoint detection in spatio-temporal
  processes, dropping the independence-across-changepoints assumption
  (*Statistics and Computing*, 2025);
- score-based sequential detection **with region localisation** for
  spatio-temporal point processes, giving a stopping time *and an estimated
  change region* with false-alarm, delay and localisation guarantees (2026).

There is no CRAN engine, so this is a `planned` registry entry plus the
groundwork that makes wrapping one cheap when it appears:

- a `change_in = "spatial"` level;
- extending the `regions` slot — currently a 1-D interval `[start, end]` from
  NSP — to carry a 2-D geometry, which is the same additive-slot move that
  worked for NSP;
- an `sf`-aware `geom_cpt_region()` that shades a *region on a map* rather
  than an interval on an axis.

That last item is the one that makes this worth planning early: it is the
package's specialty (a novel plot for an object nobody draws well) applied to
a literature that is arriving right now.

## 34. Theme U — Scale: out-of-core and chunked detection

§0.9 already owes the performance table. This theme is the design that
should come with it.

Measured in 0.5.0: `strucchange` holds a triangular O(n²) RSS matrix — about
135 MB of `$fit` for a 2,000-point series — and `bfast` and `bocpd` are in
the tens of MB. `cpt_batch(keep_fit = FALSE)` is the tourniquet. The real
answer is to know where each engine's cliff is and to have a path past it.

1. **The published table** (§0.9 item 2): every engine at n = 10⁴, 10⁵, 10⁶,
   with runtime, peak memory and result size, run on CI and rendered in a
   vignette. Nothing like it exists for R changepoint packages, and it would
   be cited.
2. **`cpt_detect_chunked()`**: detection over a series that does not fit in
   memory, reading through `arrow` or `duckdb`, with an explicit overlap
   between chunks and a documented merge rule for changepoints near a
   boundary. The correctness obligation is stated up front — on a series that
   *does* fit, the chunked answer must match the in-memory one — and that is
   the regression test.
3. **`cpt_methods()` gains a `complexity` column** (`"linear"`,
   `"n log n"`, `"quadratic"`, `"sampling"`), so `cpt_recommend(n = 1e6)`
   stops being a hand-maintained list of slow method names.

## 35. Theme V — Segment models and what comes after detection

### 35.1 The gap

`$segments` carries `param_estimate` — a mean, or a variance, whatever the
engine happened to fit. A user who wants *their own* model per segment (a
regression, an AR(1), a GLM with their covariates) has to slice the series
themselves and lose every affordance the package provides. `predict()` exists
only for `ggcpt_penalty_model`; there is no `predict.ggcpt`.

This is the cell where `tidychangepoint` is genuinely ahead: it has explicit
model-fitting for segments, and a `fitness()` layer over them.

### 35.2 Proposed API

```r
cpt_segment_models(fit, model = y ~ x1 + x2, engine = stats::lm)
#> ggcpt_segment_models: one fit per segment, in a list-column
tidy(sm)     # broom over every segment, with a `segment` column
glance(sm)   # one row per segment: r.squared, AIC, sigma, n
augment(fit, models = sm)   # fitted/resid from *the user's* model
```

and the thing practitioners actually want the moment a regime change is
detected:

```r
predict(fit, newdata, segment = "last")
```

Forecasting from the final segment only — because the point of detecting a
regime change is that the earlier regime should not inform the forecast.
That single method turns the package from a description tool into part of a
workflow, and it is a small amount of code over machinery that already
exists.

### 35.3 Model-based selection follows for free

With a segment-model layer, `cpt_select()` gains criteria computed from the
user's own model rather than a Gaussian proxy: `criterion = "model_bic"`,
`"model_aic"`, `"model_loglik"`. That closes the loop between §30's families,
§31's non-decomposable objectives and §35's models — the three are one
design, not three.

## 36. Theme W — Reporting, reproducibility and teaching

### 36.1 The artifact

`cpt_report()` produces markdown, text or a `gt` table. The next step is a
**parameterised Quarto template**:

```r
cpt_report(fit, template = "audit", output = "report.html")
```

carrying the method, the penalty and how it was chosen, the seed, the
session, every interval with its provenance, the attribution table (§28),
the caveats `cpt_recommend()` would have raised, and the citation. That is a
defensible analysis artifact of the kind a regulated or reviewed setting
requires, and it is assembly of things the package already computes.

### 36.2 `cpt_checklist()` — the pre-registration helper

`cpt_power()` and `cpt_min_detectable()` answer "would I even detect it?" but
nothing helps a user *record* that they asked before seeing the data. A small
function that takes the design (n, expected change, noise model, method,
penalty, alpha) and emits a pre-registration block — the number that belongs
in a protocol, and the method fixed in advance — is a few dozen lines and is
exactly the discipline the package's own audit history argues for.

### 36.3 The Shiny explorer, as a sibling package

`cpt_explore()` was ruled out in issue #13's "explicitly not planned" as a
hard dependency, and that ruling should stand. But the reasoning permits a
**separate package** — `ggchangepointExplorer` — that depends on this one:
load a series, sweep methods and penalties interactively, see the statistic
and scale-space panels update, export the call. It reaches an audience that
will never write `cpt_detect()` by hand, and it costs this package's
dependency footprint nothing. Teaching material (a `learnr` tutorial, a
course-ready vignette) belongs in the same sibling.

## 37. Theme X — The quality-of-life gaps

Small, unglamorous, and between them probably worth more to real users than
another engine. Each was verified absent against 0.5.0's source.

1. **Missing data.** `validate_data()` refuses any `NA`. Real series have
   gaps — sensor dropouts, weekends, non-response. Proposal:
   `na_action = c("error", "omit", "interpolate")`, defaulting to `"error"`
   so nothing changes silently, with the index preserved through the
   operation so `cp_index` stays honest about *when* rather than about
   position-after-deletion. This is currently a hard wall for a large class
   of real data.
2. **Grouped data frames.** `cpt_batch()` exists but requires reshaping to a
   matrix or a named list. `cpt_detect()` on a `dplyr::group_by()`-ed frame
   should do the obvious thing and return a `ggcpt_batch`. Panel data arrives
   long and grouped, not wide.
3. **Weights and exposure offsets.** Needed the moment §30's count families
   exist — a rate is counts *per exposure*, and without an offset the
   Poisson cost is answering a different question.
4. **`predict.ggcpt`** (§35).
5. **`cpt_detect()` on a `data.frame` of several series with an id column** —
   the long-format twin of item 2.
6. **A `cpt_diff()` / `cpt_compare_fits()`** for "did the segmentation
   change?" between two fits, two penalties or two vintages of the data — the
   question every re-run raises and nothing answers.
7. **Calendar-aware indices**: business days, irregular-but-known calendars.
   `check_regular` already warns on irregular spacing (0.5.0); the next step
   is to let a user say "this is a business-day series, the gaps are
   expected".

## 38. Prioritisation for 0.6.0 and beyond

**0.6.0 — stabilise, and take the two cheap wins.** Issue #13's open question
8 concluded *freeze after 0.5.0*, and that still holds: the performance table
(§34.1), the scheduled all-engines CI, the software paper, the deprecation
policy and the 1.0 contract freeze including the `regions` and `diagnostics`
slots. Add only the two items whose cost is small and whose reach is large:
**Theme Q families** (§30) and **Theme X's `na_action` and grouped frames**
(§37.1–2). Both unlock users who currently cannot use the package at all;
neither adds an engine.

**0.7.0 — the inferential differentiators.** **Theme O attribution** (§28)
and **Theme P conformal intervals** (§29). Both are pure R, both compose with
all fifty engines, both answer questions no R changepoint package answers,
and together they complete the arc from "where" to "which" to "how sure".

**0.8.0 — what comes after detection.** **Theme V segment models and
`predict()`** (§35), **Theme R genetic search** (§31, which §35 motivates),
and **Theme S `scanCP`** (§32).

**Later, or when the field provides.** **Theme U chunked detection** (§34.2,
after the table says where the cliff is), **Theme T spatio-temporal** (§33,
when an engine exists), **Theme W reporting and the sibling explorer** (§36),
and the remaining §17 domain engines from §0.9.

## 39. Open questions for the next cycle

1. **Does `family` belong on `cpt_detect()`, or is it `change_in`'s job?**
   `change_in = "mean"` with `family = "poisson"` is arguably one concept
   split in two. The counter-argument: `change_in` says *what changed* and
   `family` says *what the data are*, and those are orthogonal — a Poisson
   series can have a change in rate or in dispersion.
2. **Attribution: implement ARM ourselves, or wait for a package?** The
   method is a permutation argument and implementable, but 0.5.0's own record
   (the native e-detector, S11) is a warning about implementing statistics.
   If we do it, the test must measure realised FWER and FDR, not that the
   function returns.
3. **Is a sibling Shiny package worth maintaining?** It reaches a real
   audience and costs this package nothing — but it is a second thing to keep
   green, and 0.5.0's CI history is a reminder that "costs nothing" is never
   quite true.
4. **Missing data: impute or refuse?** Interpolating changes the answer, and
   an interpolated changepoint is an artifact of the interpolation. Refusing
   excludes real users. Probably: offer both, default to refusing, and
   document what interpolation does to the estimate.
5. **Is fifty engines enough?** The honest answer is probably yes, and the
   marginal engine is now worth less than the marginal *capability*. If that
   is right, the registry should stop being a growth target and start being a
   curated one — and §0.9's remaining domain engines may simply never be
   worth the maintenance.
6. **What exactly does the 1.0 contract freeze cover?** The `ggcpt` slots,
   including the optional ones? The registry's column names, now that users
   can register against them? The `source` vocabulary in `cpt_confint()`?
   Each of those is now something a user can build on, and 1.0 should say
   which of them we will not break.

## 40. References for Part III

- Baumer, B. S. et al. (2026). tidychangepoint: a unified framework for
  analyzing changepoint detection in univariate time series. *Computational
  Statistics*. arXiv:2407.14369.
- Conformal Changepoint Localization and Root Cause Analysis with Corrupted
  Observations (2026). arXiv:2607.26481. *(CONCH, CROC, and the weighted
  variants — §28, §29.)*
- ARM: Detector-Agnostic Changepoint Attribution with Finite-Sample Error
  Control (2026). arXiv:2608.01691. *(§28.)*
- Conformal Prediction for Time-series Forecasting with Change Points (2025).
  NeurIPS 2025; arXiv:2509.02844. *(Adjacent: switching-state models plus
  online conformal prediction — relevant if §35's `predict()` grows
  intervals.)*
- Change-point detection with deep learning: a review (2025). *Frontiers of
  Engineering Management*. doi:10.1007/s42524-025-4109-z. *(§32.)*
- Li, J., Fearnhead, P., Fryzlewicz, P. and Wang, T. (2024). Automatic
  change-point detection in time series via deep learning. *JRSS-B* 86(2),
  273–285. *(§32.)*
- Detection of spatiotemporal changepoints: a generalised additive model
  approach (2024). *Statistics and Computing*. doi:10.1007/s11222-024-10478-6.
  *(§33.)*
- Efficient Likelihood-Based Temporal Changepoint Detection in Spatio-Temporal
  Processes (2025). *Statistics and Computing*.
  doi:10.1007/s11222-025-10745-0. *(§33.)*
- Score-Based Change-Point Detection and Region Localization for
  Spatio-Temporal Point Processes (2026). arXiv:2602.04798. *(§33.)*
- CRAN packages surveyed for this part and not yet wired: `tidychangepoint`
  (comparator, not an engine), `changepointGA` (§31), `scanCP` (§32),
  `GA` (§31).

---

# Part III (continued) — second pass, 2026-08-29

*A second sweep of the same question. The first pass (§27–§40) came out of
"what happens after a detection?"; this one came out of two others — "what
shape is the user's data actually in?" and "what does the report they have to
write need?" Four more themes, and a note on what they change about §38.*

## 41. Theme Y — Panel and hierarchical changepoints

### 41.1 The gap

`cpt_batch()` runs N independent detections and stacks the answers. It never
borrows strength across series. But the applied question — five hundred
stores, two hundred sensors, forty hospitals, a thousand A/B cells — is
almost never "when did each of these change independently". It is:

- did they change **together**, and when?
- which ones changed **differently** from the rest?
- is there a small **common** shift that no single series has the power to
  find, but five hundred of them jointly do?

Independent detection is the wrong estimator for all three. It has no power
to find a shared small change, and no way to say that one series is
anomalous relative to its peers.

This is distinct from the multivariate detection 0.5.0 already has. `inspect`
and `esac` treat p coordinates as one object with one changepoint set; panel
methods allow **partially shared** changepoints — some common, some
per-series — which is what real panels look like.

### 41.2 CRAN engines, unwired

| Package | What it does |
|---|---|
| **`changepoint.mv`** | Most Recent Changepoint (MRC) for panel data of many related univariate series — Bardwell, Fearnhead, Eckley, Smith & Spott (2018). Built for exactly the "many related series, when did they last change" question. **Correction (§106): archived, not on CRAN as of 2026-08-30.** So this is a `planned` row or a registration target, not a wrapper task. |
| **`cpcens`** | Changepoints in **censored** panel time series — the case where the sensor floors out or the value is right-censored. **Correction (§106): also archived.** |
| **`mcp`** (already wrapped) | Supports **varying changepoints**: by-group differences in changepoint location while sharing every other parameter. We wrap `mcp` and do not expose this at all, which is the cheapest item in this theme. |

### 41.3 Proposed API

```r
cpt_panel(x, key = NULL, method = "pelt",
          pooling = c("none", "common", "partial"),
          ...)
```

`x` is long-format data or a keyed `tsibble`; `key` names the series
identifier. Returns a `ggcpt_panel` carrying

- `$common` — changepoints shared across the panel, with the number and the
  identity of the series supporting each;
- `$series` — per-series deviations from the common set;
- `$data` — long, keyed, ready to plot.

Display: a raster of **series x time**, each series a row, the common
changepoints ruled vertically across all of them and the per-series ones
marked in place. That is a picture people currently make by hand in every
panel-data changepoint analysis, and it is the natural extension of
`autoplot.ggcpt_batch()`'s small multiples to a scale where small multiples
stop working.

### 41.4 Why it is worth the work

`cpt_batch()` was 0.5.0's answer to "many series" and it is honest about
being N independent runs. Panel methods are a different estimator, not a
faster loop, and the audience — anyone with a fleet, a cohort, a portfolio,
or a store network — is large and currently unserved by every R changepoint
package including this one.

## 42. Theme Z — Effect size: how big was the change?

### 42.1 The gap

`$segments$param_estimate` gives a level per segment. To learn what the
*change* was, a user subtracts two numbers by hand and gets no uncertainty
with it. There is no standardised measure, no ratio, no interval.

The framing that makes this obvious comes from the comparison people draw
between changepoint detection and `CausalImpact`: detection answers **where**
a change occurred; intervention analysis answers **how much** effect it had.
This package does the first half thoroughly and the second half not at all —
and "how much" is the half that goes in the report.

### 42.2 Proposed API

```r
cpt_effect(fit, standardise = TRUE, level = 0.95)
```

One row per changepoint: `cp`, `cp_index`, `before`, `after`, `delta`,
`delta_lower`, `delta_upper`, `delta_std` (pooled-sd standardised),
`pct_change`, and — once §30's families exist — `rate_ratio` for counts and
`hazard_ratio` for exponential waiting times. `autoplot()` gives the
before/after with the interval, which is the figure a report wants and which
currently has to be built by hand.

### 42.3 The honest part, which has to ship with it

**An effect measured at a changepoint the same data selected is biased
upward** — the winner's curse. A naive `delta` at a detected location
overstates the change, and the more marginal the detection the worse it is.
So this theme cannot ship as a subtraction. It ships with at least one of:

- a sample-splitting estimator (locate on one half, estimate on the other);
- a de-biased / conditional estimator, in the same family of reasoning as
  `cpt_test(selection_adjusted = )` already uses;
- failing both, a `selection_adjusted = FALSE` flag on every row and a
  warning that says so in as many words.

The package already has the vocabulary for this distinction from 0.5.0's
`cpt_test()`. Reusing it here is what keeps the two consistent.

## 43. Theme AA — Ecosystem interoperability

0.5.0 accepts `ts`, `xts`, `zoo` and `tsibble` **as input**. Nothing comes
back out in any of their shapes, and one specific thing is dropped on the way
in: a `tsibble` carries a **key**, and `tsibble_parts()` reads the values and
the index and ignores it.

1. **`as_tsibble.ggcpt()`** — the augmented series as a tsibble, index and
   key intact, so a detection composes with `feasts`, `fabletools` and the
   rest of the tidyverts rather than terminating the pipeline.
2. **Key-aware input** — a keyed tsibble should route to `cpt_panel()`
   (§41), not silently collapse. Today it is not clear what it does, and
   whatever that is, it is not what the user meant.
3. **`step_changepoint()`** for `recipes` — segment identity, or distance
   since the last changepoint, as a *feature*. This is a small function that
   puts changepoint detection inside `tidymodels` feature engineering, which
   is where a large audience already works and one that will never call
   `cpt_detect()` directly. A full `parsnip` model spec would be
   over-engineering; a recipe step is not.
4. **An `accuracy()`-style bridge** so `cpt_metrics()` reads naturally to
   someone who learned scoring from `fabletools`.

None of these adds a hard dependency; all four are `Suggests`-gated, in the
pattern the package already uses for thirty-five engines.

## 44. Theme AB — Robustness, contamination and breakdown

The package wraps robust engines (`nsp` self-normalised, `sn`, the heavy-tail
nonparametrics) and `cpt_influence()` answers *delete-one* sensitivity. What
nothing answers is the question a sceptical reader asks: **how much
contamination does this answer survive?**

Proposed: `cpt_contaminate(x, method, fraction = seq(0, 0.1, by = 0.01),
type = c("outlier", "level", "missing"), n_sim = 100)` — inject a controlled
fraction of contamination and report how the segmentation degrades, giving an
*empirical breakdown point* per method.

Two reasons this is worth having. It is measurable and nobody publishes it
per-engine, so the resulting table is a contribution in its own right — the
same argument as the performance table (§34.1) and the coverage study
(§29.3). And it feeds `cpt_recommend()`: the `noise = "heavy"` preference
list is currently hand-maintained from the literature, and it could be
derived from measurement instead.

It also pairs with the weighted conformal variants in §29 (W-CONCH), which
exist precisely to downweight corrupted observations — this theme measures
the problem those methods solve.

## 45. What this block changes about §38

Two adjustments to the prioritisation:

- **Theme Z (effect size) joins 0.6.0's cheap wins.** It is small, it needs
  no engine, and "how much did it change" is a more common question than any
  remaining method gap. The selection-bias caveat is the only real work in
  it, and the package already has the vocabulary.
- **Theme Y (panel) is the largest genuinely new capability in Part III** and
  should be sequenced with the inferential themes rather than after them —
  0.7.0 alongside attribution, or 0.8.0 at the latest. Two of its three
  engines are on CRAN and the third (`mcp` varying changepoints) is a wrapper
  argument we already have the dependency for.

Everything else in §38 stands.

## 46. Further references

- Bardwell, L., Fearnhead, P., Eckley, I. A., Smith, S. and Spott, M. (2018).
  Most recent changepoint detection in panel data. *Technometrics*.
  *(`changepoint.mv`; §41.)*
- `cpcens`: Changepoint Analysis using Censored Panel Time Series Data. CRAN.
  *(§41.)*
- Brodersen, K. H., Gallusser, F., Koehler, J., Remy, N. and Scott, S. L.
  (2015). Inferring causal impact using Bayesian structural time-series
  models. *Annals of Applied Statistics* 9(1), 247–274. *(`CausalImpact`;
  the "where versus how much" distinction in §42.)*
- Wang, E., Cook, D. and Hyndman, R. J. (2020). A new tidy data structure to
  support exploration and modeling of temporal data. *JCGS* 29(3), 466–478.
  *(`tsibble` keys; §43.)*
- Quickest Causal Change Point Detection by Adaptive Intervention (2025).
  arXiv:2506.07760. *(Adjacent to §42's counterfactual direction.)*

---

# Part III (continued) — third pass, 2026-08-29

*Third sweep. The first pass asked what happens after a detection; the second
asked what shape the data is in. This one asks the question the fifty engines
quietly agree on and never state: **they all assume the change is abrupt, and
that "changed" means "changed in a Gaussian moment".** Both assumptions
exclude real users. Four more themes and a diagnostic gap.*

## 47. Theme AC — Gradual change: the assumption all fifty engines share

### 47.1 The gap, and it is a large one

Every wired method looks for a **jump**. `segmented` and `cpop` allow a
change in *slope* with the level continuous, which is the closest the package
comes, but that is still an instantaneous change in a parameter. Nothing here
detects a change that takes place *over a window*: a smooth departure from
one regime into another, with the midpoint of the transition being the thing
to estimate.

The literature is explicit that this is under-served — gradual changepoints
are much less developed than abrupt ones — and equally explicit about who
needs it: climatology and paleoclimatology, ecology and paleobiology,
linguistics, remote sensing and land-cover change. Those are the fields where
"the regime shifted over about a decade" is the honest description and "the
regime shifted at 1997" is an artifact of the method.

A user who runs `pelt` on a gradual transition gets a changepoint. It is
confidently located, it is wrong in a way that no diagnostic in the package
will reveal, and nothing warns them.

### 47.2 What to build

1. **A gradual-change model.** `change_in = "gradual"`, estimating a
   transition *interval* rather than a point — which the `regions` slot
   already exists to carry, so the plumbing is done. Smooth-transition
   (logistic/STAR) and rough-fuzzy formulations are both established.
2. **A diagnostic that catches the mistake, which matters more.**
   `cpt_abruptness(fit)`: given a detected changepoint, is the transition
   consistent with a jump, or is the fitted step a stand-in for a ramp? A
   simple, defensible version compares the fit of a step against a
   logistic transition of estimated width, and reports the width with an
   interval. That belongs in `autoplot(type = "diagnostics")` and is cheap.
3. **`cpt_simulate(shape = c("step", "ramp", "logistic"))`**, so power
   analysis can ask the question that matters: *how badly does my abrupt
   detector mislocate a gradual change, and at what transition width does it
   break?* Nobody publishes that curve per engine. We could.

### 47.3 Why item 2 is the priority

Building a gradual detector adds a capability. Building the diagnostic
prevents a *wrong answer that currently looks right* — and the package's own
0.5.0 audit history is one long argument that the second is worth more.

## 48. Theme AD — Practical significance, not just statistical

Everything in the package answers "is there a change?". Nobody asks "is the
change **big enough to care about**?" — and in an applied setting that is the
only question. A monitoring system that alarms on a 0.4% shift in a series
with n = 2,000,000 is statistically correct and operationally useless.

Recent work formalises exactly this: multiscale detection of *practically
significant* changes in a gradually varying series (arXiv:2504.15872, 2025)
tests against a **relevance threshold** rather than against zero.

Proposed, and it is small:

```r
cpt_detect(x, method = "pelt", min_effect = 0.5)        # in sd units
cpt_test(fit, relevance = 0.5)                          # H0: |delta| <= 0.5
cpt_monitor("edetector", relevance = 1.0)               # don't alarm below this
```

`min_effect` post-filters a segmentation on the standardised effect (§42's
`cpt_effect()` supplies the number); `relevance` changes the null hypothesis
from "no change" to "no change worth acting on". The second is the
methodologically interesting one and the first is what most users want.

This composes with §42 and with §30's families — a relevance threshold on a
rate ratio is a different number from one on a mean shift, and both are more
meaningful to a practitioner than a p-value.

## 49. Theme AE — Regime models: the adjacent field we do not speak to

Markov-switching and hidden Markov models (`depmixS4`, `MSwM`, `hhsmm`) solve
a neighbouring problem with a different estimator: **recurring** regimes with
transition probabilities, rather than a sequence of distinct segments. HMMs
can be applied to changepoint analysis directly — the forward–backward
algorithm subsumes some changepoint inference procedures — and a large
applied audience in finance, ecology and behavioural science reaches for them
first.

The package should not become an HMM package. What it should do:

1. **`as_ggcpt()` from a fitted HMM.** A `depmixS4` posterior state sequence
   has changepoints in it — the transitions. Turning those into a `ggcpt`
   makes every plot, metric and comparison in this package available to the
   HMM audience, for the price of one coercion method. This is precisely what
   the extension mechanism was built for and it has never been pointed at the
   single largest adjacent community.
2. **`cpt_regimes(fit)`** — the reverse direction: label the segments of an
   existing segmentation by clustering their parameters, so "these four
   segments are the same regime, recurring" becomes expressible. Segments and
   regimes are different objects and the package currently only has the
   former.
3. A vignette that states honestly **when a changepoint model is the wrong
   tool** and an HMM is right: recurring states, soft assignment, known
   number of regimes. A package that tells you when not to use it is more
   trustworthy, and `cpt_recommend()` is already the place for that advice.

## 50. Theme AF — Frequency-domain and time–frequency changes

`wbsts` is wired and works on the wavelet periodogram, so the package already
touches this — accidentally, through one engine, with no vocabulary for it.
A change in the **spectrum** with no change in mean or variance is invisible
to every other method here.

- `change_in = "spectrum"` as a level, with the natural display being a
  **spectrogram with the changepoints ruled across it** — a scale-space
  picture in frequency rather than bandwidth, and a direct sibling of
  `ggcpt_scale_space()`.
- Engines to survey: `WaveletComp`, `LSWPlib`, `pdSpecEst` for the estimation
  side; the recent statistically-reliable frequency-domain detection work
  (arXiv:2502.03062, 2025) for the inference side.
- Audience: EEG and neurophysiology, vibration and condition monitoring,
  audio, seismology, and anywhere "the machine started humming differently"
  is the observation.

## 51. Theme AG — Diagnostics after the segmentation

`augment()` gives `.fitted` and `.resid`, and — verified — **for the first
coordinate only**. Beyond that, nothing checks whether the fitted
segmentation is any good. A regression package that gave coefficients and no
residual plots would be considered unfinished; that is the package's current
position.

Proposed `autoplot(fit, type = "diagnostics")`, a four-panel display in the
same faceted idiom `ggcpt_posterior()` already uses:

1. residuals against index, with segment boundaries — does the fit leave
   structure behind?
2. residual ACF per segment — is the within-segment independence assumption
   that most of these engines rest on actually plausible?
3. residual QQ per segment — the Gaussian assumption, made visible, which is
   also the argument for §30's families;
4. segment-wise variance — the constant-variance assumption, ditto.

Plus `cpt_gof(fit)` returning those as numbers: per-segment n, sd, Ljung–Box
p, Shapiro p, and a flag when a segment is too short for any of it to mean
anything.

This is the cheapest theme in Part III — it reuses `augment()`, the
`patchwork`-free faceting idiom and the existing accessibility scales — and
it addresses the most common way a changepoint analysis is wrong in practice,
which is that the model was inappropriate rather than the algorithm faulty.
It should also fix `augment()`'s first-coordinate-only limitation for
multivariate fits while it is in there.

## 52. Where these sit against §38 and §45

- **§51 (diagnostics) belongs in 0.6.0.** It is small, it uses only what
  exists, and "your Gaussian assumption is violated" is a more valuable thing
  to tell a user than a 51st engine.
- **§48 (practical significance) also belongs in 0.6.0**, as `min_effect`
  at least. It is a filter over `cpt_effect()` (§42) and the two should ship
  together.
- **§47 (gradual change)** — the *diagnostic* (`cpt_abruptness()`) in 0.7.0
  with the inferential themes; the gradual *detector* later, or never if no
  CRAN engine appears and the diagnostic turns out to be enough.
- **§49 (HMM coercion)** is one `as_ggcpt()` method and a vignette, and it
  reaches the largest adjacent audience of anything in Part III per line of
  code. It could go in any release.
- **§50 (frequency domain)** is a survey first: find out whether the
  estimation packages are wrappable before promising a `change_in` level.

## 53. Still-further references

- Multiscale detection of practically significant changes in a gradually
  varying time series (2025). arXiv:2504.15872. *(§47, §48.)*
- Rough-Fuzzy CPD: a gradual change point detection algorithm (2022).
  *Journal of Data, Information and Management*. doi:10.1007/s42488-022-00077-3.
  *(§47.)*
- Bastian, P. and Dette, H. (2024). Gradual changes in functional time series.
  arXiv:2407.07996. *(§47, and it composes with the functional engines
  already wired.)*
- Liang, Y. et al. (2021). Gradual variance change point detection with a
  smoothly changing mean trend. *Stat* 10:e327. *(§47.)*
- Visser, I. and Speekenbrink, M. (2010). depmixS4: An R package for hidden
  Markov models. *JSS* 36(7). *(§49.)*
- Hidden Markov Model Applications in Change-Point Analysis (2012).
  arXiv:1212.1778. *(§49 — the formal connection between the two frameworks.)*
- Change Point Detection in the Frequency Domain with Statistical Reliability
  (2025). arXiv:2502.03062. *(§50.)*
- Korkas, K. and Fryzlewicz, P. (2017). Multiple change-point detection for
  non-stationary time series using wild binary segmentation. *Statistica
  Sinica* 27, 287–311. *(`wbsts`, already wired; the precedent for §50.)*

---

# Part III — the shape of it, as of the third pass

*A reader's index to §27–§53, because seventeen themes across three passes is
more than anyone should have to hold in their head. Nothing new is proposed
here; this is the map.*

## 54. The seventeen themes, sorted by what they are actually for

**Making a detection mean something** — the arc from a location to a finding:

| | Theme | The question it answers |
|---|---|---|
| §28 | O — Attribution | *Which* coordinate changed? |
| §29 | P — Conformal intervals | How sure are we *where*, without a model? |
| §42 | Z — Effect size | *How much* did it change? |
| §48 | AD — Practical significance | Is that enough to care about? |
| §51 | AG — Diagnostics | Was this model appropriate at all? |

**Letting more data in the door** — each of these is a class of user who
currently cannot use the package:

| | Theme | Who it lets in |
|---|---|---|
| §30 | Q — Non-Gaussian families | counts, rates, waiting times |
| §37 | X — `na_action`, grouped frames | anyone whose series has gaps |
| §41 | Y — Panel and hierarchical | anyone with a fleet or a cohort |
| §47 | AC — Gradual change | climate, ecology, remote sensing |
| §50 | AF — Frequency domain | EEG, vibration, audio, seismology |
| §33 | T — Spatio-temporal | anything on a map |

**Reaching further than we do** — new search, new engines, new neighbours:

| | Theme | What it adds |
|---|---|---|
| §31 | R — Genetic search | the sixth search paradigm |
| §32 | S — Neural detectors | `scanCP`, and evaluating a learned one |
| §49 | AE — Regime models | the HMM audience, for one coercion method |

**Making it usable at all** — infrastructure and afterlife:

| | Theme | What it fixes |
|---|---|---|
| §34 | U — Scale | the series that does not fit in memory |
| §35 | V — Segment models, `predict()` | what to *do* with the segmentation |
| §36 | W — Reporting, teaching, explorer | the artifact, and the audience who will never type `cpt_detect()` |
| §43 | AA — Ecosystem interop | tsibble keys, `recipes`, tidyverts |

## 55. The consolidated ordering

Superseding the partial orderings in §38, §45 and §52. Nothing here is a
commitment; it is the order that makes each release coherent.

**0.6.0 — freeze, measure, and take what is cheap.** Issue #13's open
question 8 said freeze after 0.5.0 and that still holds: the API freeze, the
deprecation policy, the performance table (§34.1), the scheduled all-engines
CI, the software paper. Add only what is small and unlocks users who cannot
currently run the package at all:

- §30 non-Gaussian families
- §37.1–2 `na_action` and grouped data frames
- §42 effect size, with §48's `min_effect` filter
- §51 residual diagnostics (and fix `augment()`'s first-coordinate limit)

Every one of those is a few hundred lines over machinery that exists, and
none adds an engine.

**0.7.0 — the inferential release.** §28 attribution, §29 conformal
intervals, §47.2 the abruptness diagnostic. This is where the package stops
being the broadest changepoint interface in R and starts being the one that
tells you whether to believe the answer. All three are pure R.

**0.8.0 — panels and afterlife.** §41 panel/hierarchical, §35 segment models
and `predict()`, §31 genetic search (which §35 motivates), §49's HMM
coercion.

**Later, or when the field provides.** §34.2 chunked detection, §33
spatio-temporal, §50 frequency domain, §32 beyond `scanCP`, §36's sibling
explorer, and §0.9's remaining domain engines.

## 56. The three things worth saying out loud

**First: the engine count has stopped being the product.** Fifty engines is
already more than any comparable package and more than any user will try.
Nine of the seventeen themes above add no engine at all, and the four in
0.6.0 add none. If a future pass over this document finds itself proposing a
fifty-first engine before it has proposed a diagnostic, that is the signal to
stop and re-read this paragraph.

**Second: every theme that survived three passes is detector-agnostic.**
Attribution, conformal intervals, effect size, practical significance,
diagnostics, families, panels — each composes with all fifty engines *and*
with anything a user registers. That is the leverage the extension mechanism
bought, and it is the reason the surface is worth more than the count.

**Third: the package's own history is the argument for §51.** Thirty-seven
defects were found in 0.5.0, and the ones that mattered — an average-run-length
bound off by a factor of two, a heatmap where every cell was `NA`, an engine
returning a changepoint at every observation — all shared a shape: *the code
ran and the answer was wrong*. The residual and assumption diagnostics in
§51 are the same discipline pointed at the user's analysis rather than at
ours. A package that learned that lesson internally and does not offer it
outward has only half-learned it.

---

# Part III (continued) — fourth pass, 2026-08-29

*Fourth sweep, from a question the first three did not ask: **what shape of
change can the `ggcpt` contract not even represent?** The answer turns out to
be a large one, and it is next door.*

## 57. Theme AH — Epidemic changepoints: the change that comes back

### 57.1 The contract cannot express it

`ggcpt` says: changepoints partition the series, segments tile it, each
segment has its own parameter. That model has an assumption buried in it —
**every change is permanent**. A series that departs from a baseline and
*returns to it* has to be described as two changepoints and three segments,
of which the first and third happen to agree, and nothing in the object says
they are the same regime.

That is not a corner case. It is the whole of:

- an outbreak that subsides;
- a machine fault that is repaired;
- a fraud campaign that ends;
- a drug effect that washes out;
- a sensor that drifts and is recalibrated;
- a market dislocation that mean-reverts.

Every one of those is "an interval of anomaly against a persistent baseline",
which is a *different object* from a sequence of regimes, and the package has
no way to say it.

### 57.2 The engine is on CRAN, by the same group as `changepoint`

`anomaly` 4.3.0 (updated May 2026) implements exactly this family:

| Method | What it is |
|---|---|
| **CAPA** | Collective And Point Anomalies (Fisch, Eckley & Fearnhead, *SADM* 2022): near-linear detection of *collective* anomalies — intervals differing from baseline in mean, variance or both — distinguished from *point* anomalies, i.e. outliers |
| **MVCAPA** | the multivariate version, with subset selection: which coordinates are anomalous over the interval (*JCGS* 2022) |
| **PASS** | Proportion Adaptive Segment Selection |
| **BARD** | Bayesian Abnormal Region Detector, giving a posterior over anomalous regions |

The methodological connection is direct: CAPA is derived as a corollary of
the consistency of penalised-cost changepoint detection. This is the same
Lancaster line of work that produced `changepoint`, PELT and `nsp` — the
package's own foundations — and it is the most natural neighbour we do not
wrap.

### 57.3 What it needs, and the good news about the plumbing

The `regions` slot already carries "an interval on the series" because NSP
needed it. An epidemic changepoint is the same shape with a different
meaning, so:

- `change_in = "epidemic"` as a level;
- registry entries `capa`, `mvcapa`, `pass`, `bard`;
- `$regions` carrying `start`, `end`, `type` (`"collective"` / `"point"`),
  and for MVCAPA the anomalous **subset of coordinates** — which is §28's
  attribution question arriving from a completely different direction, and
  the two should share a vocabulary;
- `geom_cpt_region()` draws it already; what is new is `type` distinguishing
  a shaded band from a marked point;
- `cpt_metrics()` needs an interval-matching variant, because scoring "did
  you find the anomalous *window*" is not the same as scoring "did you find
  the changepoint".

**MVCAPA's subset selection deserves emphasis.** It answers *which
coordinates* were anomalous over the window, natively, in an engine that is
already on CRAN — while §28 proposes to build attribution for ordinary
changepoints from a 2026 preprint. Wrapping MVCAPA gives the package a
working answer to "which coordinate?" in the epidemic case immediately, and a
reference implementation to validate §28's general one against.

### 57.4 Priority

**This is the strongest single addition left in Part III.** It is a whole
class of applied problem the package cannot currently represent, the engine
is on CRAN and actively maintained, the plumbing exists, and it arrives with
a native answer to the attribution question. It should sit alongside §28 in
0.7.0, not later.

## 58. Theme AI — Genomics, and the segmentation audience that already exists

Changepoint detection has a large installed audience that mostly does not
know it is doing changepoint detection: **copy-number segmentation**.
Circular binary segmentation (CBS) via Bioconductor's `DNAcopy` is the
default in array-CGH and SNP-array CNV calling and is embedded in many
pipelines.

The package should not become a genomics package. Three things it can do
cheaply:

1. **`as_ggcpt()` from a `DNAcopy` object.** A CBS segmentation is a
   changepoint set with a per-segment mean. One coercion method makes every
   plot, metric, comparison and diagnostic here available to that audience —
   the same one-method-reaches-a-community argument as §49's HMM coercion,
   and a bigger community.
2. **A `cbs` wrapper**, if `DNAcopy`'s Bioconductor-only status is acceptable
   in `Suggests`. Worth checking: a Bioconductor package in `Suggests` is
   allowed but complicates CI, and this may be a case where registration
   (§18) is the better answer than a wrapper — which would be a good, honest
   test of whether the extension mechanism really does what it claims.
3. **Genomic conventions in the index layer.** Positions in base pairs,
   chromosome as a grouping (§41's panel key), and the fact that segments do
   not cross chromosome boundaries — a constraint the package has no way to
   express and which is exactly the "must not merge across a boundary" rule
   that §34.2's chunked detection also needs. One mechanism, two uses.

The post-selection inference literature for changepoints was in part
motivated by CNV data, so §29's conformal intervals land in a field that
already wants them.

## 59. Theme AJ — The package ships no data

**Verified: there is no `data/` directory.** Every example, vignette and test
runs on `cpt_simulate()` or on TCPD downloaded at runtime.

That is defensible — nothing is redistributed, and licensing is somebody
else's problem — but it has costs that are easy to underestimate:

- a teacher cannot say `data(x); cpt_detect(x)`, which is the first line of
  every R lesson ever written;
- every vignette figure is of synthetic data, so a reader never sees the
  method meet a real series with real messiness;
- `cpt_benchmark()`'s offline path scores methods on signals the package
  generated itself, which is a weaker demonstration than one real annotated
  series would be;
- newcomers judge a package by whether they can run something interesting in
  the first thirty seconds.

Proposed: **two or three small, permissively licensed real series** shipped
in `data/`, each with documented provenance, a known or annotated changepoint
where one exists, and a licence that survives CRAN review. Classic candidates
in the literature — well-log data, Nile river flows (already in base R's
`datasets`, so free), UK coal-mining disasters, a global temperature series,
a public web-traffic or energy series — chosen so that between them they
cover a mean change, a variance change and a count series (which §30's
families would then have something real to demonstrate on).

`LazyData: true`, a few hundred KB, and it changes the first thirty seconds
of everyone's experience with the package. This is the cheapest item in Part
III and possibly the highest ratio of impression-to-effort.

## 60. Theme AK — Screening many series: multiplicity across the panel

§41 asks when a panel changed *together*. The other panel question is the
screening one: **I have ten thousand series and I want the ones that
changed.** Run `cpt_detect()` ten thousand times and a method calibrated at
α = 0.05 hands back five hundred false positives with no correction anywhere.

`cpt_batch()` does exactly that today, silently.

Proposed `cpt_screen(x, method, alpha, control = c("fdr", "fwer", "none"))`:
one p-value or evidence measure per series (`cpt_test()` already produces
one), Benjamini–Hochberg or Holm across series, and a result ordered by
evidence rather than by input order. Plus, at minimum, a **warning from
`cpt_batch()` when the number of series is large and no correction was
requested** — the same "say what you did not do" discipline that
`cpt_test(selection_adjusted)` and `cpt_benchmark()`'s missing-truth warning
already follow.

The screening framing also gives `cpt_benchmark()` a metric it lacks: at a
fixed FDR across a panel, how many true changes does each method recover?
That is the question a practitioner with a fleet actually optimises, and no
changepoint benchmark reports it.

## 61. What the fourth pass changes

- **§57 (epidemic/CAPA) joins §28 in 0.7.0.** It is the largest
  representational gap left, the engine is on CRAN and maintained by the
  group whose methods the package is already built on, and MVCAPA hands us a
  validated answer to the attribution question while §28 builds the general
  one.
- **§59 (bundled data) goes into 0.6.0**, and arguably should have been in
  0.1.0. It is a few hundred kilobytes and a licence check.
- **§60 (screening)** is small and pairs with §41; either 0.7.0 or 0.8.0.
- **§58 (genomics coercion)** is one `as_ggcpt()` method whenever somebody
  wants it, and a useful test of whether the extension mechanism delivers
  what §18 promised.

## 62. Fourth-pass references

- Fisch, A. T. M., Eckley, I. A. and Fearnhead, P. (2022). A linear time
  method for the detection of collective and point anomalies. *Statistical
  Analysis and Data Mining* 15(4), 494–508. *(CAPA; §57.)*
- Fisch, A. T. M., Eckley, I. A. and Fearnhead, P. (2022). Subset
  multivariate collective and point anomaly detection. *JCGS* 31(2),
  574–585. *(MVCAPA and its subset selection; §57.)*
- Fisch, A. T. M., Grose, D., Eckley, I. A., Fearnhead, P. and Bardwell, L.
  (2024). anomaly: Detection of anomalous structure in time series data.
  *Journal of Statistical Software*. *(The package, CRAN 4.3.0; §57.)*
- Olshen, A. B., Venkatraman, E. S., Lucito, R. and Wigler, M. (2004).
  Circular binary segmentation for the analysis of array-based DNA copy
  number data. *Biostatistics* 5(4), 557–572. *(`DNAcopy`; §58.)*
- Hyun, S., Lin, K. Z., G'Sell, M. and Tibshirani, R. J. (2021).
  Post-selection inference for changepoint detection algorithms with
  application to copy number variation data. *Biometrics* 77(3), 1037–1049.
  *(§58, and the motivation for §29 in a field that wants it.)*
- Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery
  rate. *JRSS-B* 57(1), 289–300. *(§60.)*

---

# Part III (continued) — fifth pass, 2026-08-29

*Fifth sweep, from the narrowest question yet: **what does `cpt_detect()`
require its input to be?** The answer is `as.numeric(x)` on a regularly
spaced vector, and three whole classes of data fall outside it. Plus two
themes about where the package runs rather than what it computes.*

## 63. Theme AL — Data that is not a numeric vector on a regular grid

Verified: `as_uni_vector()` ends in `as.numeric(x)`. Everything the package
does begins from a real-valued, equally spaced sequence. Three kinds of data
that people routinely want segmented are not that, and each has a literature.

### 63.1 Event times — point processes

The input is a list of **when things happened**, not a value per time step:
transactions, earthquakes, clicks, arrivals, neuron spikes, security events.
The question is when the **intensity** changed.

Passing these to `cpt_detect()` requires binning them into counts first,
which throws away resolution, makes the answer depend on the bin width, and
is a choice nobody documents. Multiple changepoint detection for Poisson and
Hawkes(-like) processes is established (arXiv:2302.09103), including
self-exciting processes where the intensity depends on past events — the
usual case for anything that clusters.

Proposed: `cpt_detect(events, type = "events")` taking event times directly,
with `change_in = "intensity"`, and the natural display being an intensity
estimate with the changepoints ruled across it. §30's Poisson family is the
binned approximation of this; this is the honest version.

### 63.2 Categorical and state sequences

The input is a sequence of **labels**: log-message types, DNA bases, weather
states, clinical stages, user journey steps, tokenised text. The question is
when the *distribution over categories* changed.

`as.numeric()` on a factor gives the level codes, which are meaningless as a
mean, so the package will happily return a confident and completely spurious
answer. That is worse than refusing.

Multiple changepoint detection in categorical data streams is a solved
problem (*Statistics and Computing*, 2019) via adaptive monitoring of
multinomial category probabilities with forgetting factors — and it is an
*online* method, so it belongs with `cpt_monitor()` as much as with
`cpt_detect()`.

Proposed: accept a `factor` or `character` input with
`change_in = "categorical"`, a multinomial cost, and — before any of that
— **refuse a factor rather than silently coercing it**, which is a one-line
guard and should not wait for the theme.

### 63.3 Censored and survival data

The input is **times to an event, some of them censored**. The question is
when the *hazard* changed. This has a substantial literature (change-point
hazard models, nonparametric estimation with a partially constant hazard,
changepoints in the Cox model with covariates) and CRAN packages —
`CPsurv` for nonparametric changepoint estimation in survival data, and
`cpsurvsim` for simulating from change-point hazard distributions, which
would give `cpt_simulate()` a survival mode for free.

The audience is clinical trials, reliability and epidemiology, and it is
adjacent to §30's exponential family: a change in a constant hazard *is* a
change in an exponential rate, so part of this arrives with Theme Q.

### 63.4 The common move

All three are the same architectural step: **the input contract widens from
"a numeric vector" to "a thing with a `cpt_series` method"**. `as_cpt_series()`
already exists and already dispatches over `ts`/`xts`/`zoo`/`tsibble`/data
frames. Extending it to event times, factors and `Surv` objects is the same
mechanism, and `change_in` gains three levels. The refusals — factor, `Surv`,
a list of event times — should land first regardless, because silently
coercing them is the package producing a wrong answer with a straight face,
which is the failure mode its own audit history is most allergic to.

## 64. Theme AM — Where the package runs: production and observability

Everything in the package assumes an analyst at a console. `cpt_monitor()`
and `cpt_update()` are a genuine streaming state machine, and nothing helps
anyone *deploy* one.

1. **Persist and resume a monitor.** A `ggcpt_monitor` round-trips through
   `saveRDS` (verified in 0.5.0), which is most of the way there, but nothing
   documents the pattern: build it once, checkpoint it, restore it in the
   next process, keep the run length intact. A vignette and a
   `cpt_monitor_save()`/`cpt_monitor_load()` pair with a version stamp would
   make it a supported workflow rather than a thing that happens to work.
2. **A scoring endpoint.** A `plumber` example — POST a batch of new
   observations, get alarms back — as a vignette, not a dependency. This is
   the shape every production deployment takes and everyone reinvents it.
3. **Metrics for observability.** `alarms()` and `cpt_delay()` already
   compute what a monitoring system wants; emitting them in a
   Prometheus-readable form is a formatting function, not a feature.
4. **Backpressure and drift.** A long-running monitor eventually faces the
   question the batch API never does: the baseline itself has drifted, no
   alarm fired, and the detector is now calibrated against a world that no
   longer exists. `relearn` handles the post-alarm case. Nothing handles the
   slow case, and it is the one that bites in production.

None of this is statistics. All of it is the difference between a method
someone tries and a method someone runs.

## 65. Theme AN — Machine-readable output, for the readers who are not human

A growing share of R calls are made by something that is not a person at a
console — automated pipelines, reporting agents, LLM tool use. Every one of
them has to read `print()` output or reach into the object's internals,
because the package offers no structured export.

`tidy()`/`glance()`/`augment()` are the tidyverse answer and they are already
right. What is missing is one level up:

- **`as_json.ggcpt()`** — the whole result as a documented JSON schema:
  method, engine, penalty, changepoints with indices and intervals and
  provenance, segments with parameters, and the caveats. Nothing here needs
  inventing; it is `unclass()` plus a stable contract, and the contract is
  the point.
- **A stable schema, versioned.** If a pipeline is going to depend on this,
  the field names have to be part of the 1.0 freeze (§39 question 6).
- **`cpt_report(format = "json")`** alongside `md`/`text`/`gt`, so the
  reproducible-report path has a machine-readable branch.

The package already imports `jsonlite`'s functionality indirectly (TCPD
loading uses it in `Suggests`), so the dependency question is settled.

This is a small function with an unusually long shelf life: the fifty engines
will be wrapped once, but a stable serialisation is what lets other software
build on this package instead of around it.

## 66. Two things deliberately *not* proposed, and why

Recording these matters as much as recording the themes, because a roadmap
that only ever grows is not making decisions.

**Differentially private / federated changepoint detection.** Searched, and
the 2025–26 literature on differential privacy in this area is almost
entirely about federated *learning*, not changepoint detection. There is no
settled method and no R implementation to wrap. Proposing it would be
proposing research, and the package's stated position — wrap published,
separately maintained implementations; implement original methods once,
deliberately, and label them — rules that out. **Revisit when a method
exists.**

**A `parsnip` model specification.** §43 proposes a `recipes` step and stops
there on purpose. A changepoint model is not a predictive model with a
`fit`/`predict` contract in the tidymodels sense, and forcing one would
produce an awkward object that satisfies an interface without serving a user.
The recipe step is the honest amount of tidymodels integration.

## 67. What the fifth pass changes

- **§63.2's factor refusal and §63.4's other refusals go into 0.6.0**, with
  the rest of Theme X's input hardening. A factor silently coerced to level
  codes is a wrong answer produced confidently, which is the one category of
  bug this package has spent 37 fixes learning to hate. The guard is one line
  per input type; the detection methods can follow whenever.
- **§65 (JSON export)** is small and belongs with the 1.0 contract freeze,
  because the schema has to be frozen with everything else — so 0.6.0.
- **§63.1 (event times)** and **§63.3 (survival)** are 0.8.0 or later, and
  §63.3 partly arrives free with §30's exponential family.
- **§64 (production)** is mostly vignettes and one save/load pair; it can go
  in any release and would pair well with 0.7.0's inferential work, since a
  deployed monitor is exactly where "how sure are you" gets asked.

## 68. Fifth-pass references

- Multiple change-point detection for some point processes (2023–24).
  arXiv:2302.09103. *(Poisson and Hawkes-like processes; §63.1.)*
- Change Point Detection and Mean-Field Dynamics of Variable Productivity
  Hawkes Processes (2025). arXiv:2512.20068. *(§63.1.)*
- Multiple changepoint detection in categorical data streams (2019).
  *Statistics and Computing* 30, 443–458. doi:10.1007/s11222-019-09858-0.
  *(§63.2 — and it is an online method, so it belongs with `cpt_monitor()`.)*
- Nonparametric change point estimation for survival distributions with a
  partially constant hazard rate (2018). *Lifetime Data Analysis* 24,
  705–731. *(`CPsurv`; §63.3.)*
- Hagar, Y. and Dukic, V. (2022). cpsurvsim: An R package for simulating data
  from change-point hazard distributions. *R Journal* 14(1). *(§63.3, and a
  survival mode for `cpt_simulate()`.)*
- Estimation of a changepoint in the Cox hazard model with covariates (2025).
  *Quality & Quantity*. doi:10.1007/s11135-025-02247-y. *(§63.3.)*

---

# Part III (continued) — sixth pass, 2026-08-29

*Sixth sweep, turned on the package's own declared specialty. Five passes have
been about statistics and data. This one asks: **the package says it is
ggplot2-native — is it?** The honest answer is "at the surface, yes; in the
extension contract, not really", and that is a gap worth naming because it is
the one thing this package claims that nothing else does.*

## 69. Theme AO — Be a real ggplot2 extension, not a wrapper over one

### 69.1 What the code actually is

Verified across `R/geoms.R` and `R/geoms-regions.R`: of the six exported
`geom_*` functions, **none defines a `Geom` ggproto object**. Every one is a
thin function that calls a stock geom:

| Export | What it really is |
|---|---|
| `geom_changepoint()` | `ggplot2::geom_vline()` |
| `geom_cpt_segment()` | `ggplot2::geom_segment()` |
| `geom_cpt_ci()` | `ggplot2::geom_errorbarh()`-shaped call |
| `geom_cpt_region()` | `ggplot2::geom_rect()` + full-height defaults |
| `geom_cpt_label()` | `ggplot2::geom_rect()` |
| `geom_cpt_event()` | `geom_vline()` + a text layer |

Only `StatChangepoint` is a genuine `ggproto` object, and it is the one piece
of the visual surface that behaves like a first-class extension.

This is not a bug — the plots are correct and the S2/S3 fixes in 0.5.0 made
them behave — but it has consequences a user hits:

- **No `draw_key`,** so legend glyphs for these layers are whatever the
  borrowed geom draws. A changepoint rule and a significance region should
  have their own key glyphs; `guide_legend()` now only draws a key when the
  value is in the layer's data, which makes this more visible than it was.
- **Default aesthetics are argument defaults, not `default_aes`,** so they do
  not participate in the scale/guide system the way a real Geom's do.
- **`after_stat()` and the position system** are only as available as the
  borrowed geom allows, and `geom_cpt_event()`'s two-layer construction
  cannot participate at all — which is exactly why it needed
  `inherit.aes = FALSE` bolted on (S3).
- **Nothing extends the `Guide` class,** which ggplot2 rewrote in ggproto
  specifically so extension packages could.

### 69.2 What to build

1. **`GeomChangepoint`, `GeomCptRegion`, `GeomCptCi`, `GeomCptEvent` as real
   ggproto Geoms**, each with `default_aes`, `required_aes` and a
   `draw_key` that draws the right glyph — a vertical rule, a shaded band, a
   horizontal interval with caps, a dotted rule with a flag. The user-facing
   `geom_*()` functions keep their signatures, so nothing breaks; they gain
   `ggplot2::layer(geom = GeomX, ...)` underneath.
2. **A `StatCptRegion`** so a region can be *computed* in the layer rather
   than passed in pre-computed, matching what `stat_changepoint()` already
   does for points. `ggplot(d, aes(t, y)) + geom_line() + stat_cpt_region(method = "nsp")`
   is the one-liner the package's pitch implies and does not currently
   deliver.
3. **A `Guide` for the changepoint legend** once 1 and 2 exist — the piece
   ggplot2's rewrite made possible and nobody has used for this domain.
4. **The visual regression net already exists** (25 vdiffr snapshots), which
   is exactly the safety rail this refactor needs. Doing it without that net
   would be reckless; doing it with the net is a contained change.

### 69.3 Why it matters more than it sounds

The package's one-line claim is that changepoint analysis should be
*ggplot2-native*. Right now it is ggplot2-*shaped*: the outputs are ggplots
and the layers compose, but the extension contract — glyphs, default
aesthetics, stats, guides — is borrowed. Closing that is what makes the claim
literally true, and it is the kind of work no competing package is positioned
to do, because none of them has the layer surface to begin with.

## 70. Theme AP — The plots that are still missing

Seventeen displays exist. Five that people draw by hand, every time, do not.

1. **Changepoint location uncertainty as a distribution.** The Bayesian
   engines produce a posterior over location and `ggcpt_posterior()` draws
   it; the frequentist engines produce an interval and `geom_cpt_ci()` draws
   a bar. Nothing draws **location uncertainty as a density** — the ridge or
   violin at each changepoint that says "it is probably here, possibly
   there". §29's conformal sets and §28's attribution both produce exactly
   this shape of object, so the display should be built once and shared.
2. **Before/after distribution comparison.** The single most common manual
   follow-up to a detection: two densities, or a pair of boxplots, one per
   adjacent segment, side by side at each changepoint. It is what makes an
   abstract "change in mean" concrete for a non-statistical reader, and it is
   four lines of ggplot2 that every user writes themselves.
3. **The segment ridge plot.** For a series with many segments, the
   distribution of values within each, stacked — which answers "are these
   really different regimes or is this one noisy regime cut up?" at a glance.
   Pairs naturally with §49's regime clustering.
4. **Animation over the stream.** `cpt_replay()` produces a monitor state at
   every time step and `autoplot.ggcpt_monitor()` draws the final one. The
   detector's statistic *climbing toward its threshold* is the most
   explanatory picture in the whole online story and it is only available as
   a static end state. A `gganimate`-based `cpt_animate()` in `Suggests`, or
   simply a documented recipe, would carry it.
5. **The small-multiples problem at scale.** `autoplot.ggcpt_batch()` facets,
   which stops working somewhere around fifty series. §41's panel raster is
   the answer for panels; the same display serves any large batch, and it
   should not wait for the panel *methods* to arrive.

None of these needs a new statistical idea. All five are the package's stated
specialty, and each is currently homework it sets its users.

## 71. Theme AQ — Choosing, explained: the decision surface

`cpt_recommend()` scores methods against a query and returns a ranked table
with reasons and caveats. Two things it does not do:

1. **Explain the ranking as a picture.** The scoring is additive over a
   handful of criteria, which makes it exactly the kind of thing a small
   contribution plot explains far better than a table — what pushed this
   method up, what pulled it down, and how close the runner-up was.
2. **Learn from `cpt_benchmark()`.** The `noise_pref` and `noise_warn` lists
   in `cpt_recommend()` are **hand-maintained from the literature**, verified
   in the source. Meanwhile `cpt_benchmark()` can measure exactly what those
   lists assert, and §44's contamination study and §34's performance table
   would produce the rest. The recommendation could be *derived from
   measurement* — shipped as a fitted object, versioned with the package,
   regenerated on CI.

That second item is the more interesting one, and it turns three separate
proposals (§34's timings, §44's breakdown points, §29's coverage study) into
inputs for one thing a user actually consults. It also makes the advice
falsifiable, which hand-maintained advice is not.

A **cheatsheet** and a **decision-tree vignette** belong in the same theme:
"which of these fifty do I want" is the first question every new user has,
and `cpt_recommend()` answers it programmatically for people who already know
to ask.

## 72. Theme AR — Testing the way the package's own history says to

0.5.0's audit found 37 defects, and the pattern in the ones that mattered was
always the same: *the code ran and the answer was wrong*. The suite that
found them is example-based — 2,590 assertions on hand-chosen inputs. Two
techniques would have found several of them earlier and mechanically:

1. **Property-based testing.** The `ggcpt` contract is a list of invariants —
   segments tile the series, `cp` is sorted, unique, integer and in
   `[1, n)`, `glance()` is one row, `augment()` is n rows, the index
   round-trips. Those are properties, not examples, and generating a thousand
   random series and asserting them is a different kind of net from the
   contract sweep run by hand in the audit. `hedgehog` or `quickcheck` in
   `Suggests`, one property test per invariant.
2. **Metamorphic testing** — the technique that fits changepoint detection
   unusually well, because the right answers are unknown but the *relations*
   are known. Shift a series by a constant: the changepoints must not move.
   Scale it: a mean-change detector's answers must not move. Reverse it: they
   must mirror. Duplicate every observation: they must roughly double.
   Concatenate two series with a known gap: the known changepoint must
   appear. Each of those is a test that needs no ground truth and would have
   caught the `pilliat` power-of-two bug (S23) immediately — a detector
   returning n−1 changepoints fails the shift-invariance test on any input.

This is infrastructure, not a feature, and it is the item most directly
argued for by the package's own record.

## 73. What the sixth pass changes

- **§72 (property and metamorphic tests) belongs in 0.6.0**, alongside the
  freeze. A contract that is about to be frozen should be *mechanically*
  checked first, and metamorphic tests are the cheapest defect-finder
  available for this problem.
- **§70's items 1–3 (uncertainty density, before/after, ridges)** are 0.7.0,
  because §28 and §29 produce the objects that item 1 draws.
- **§69 (real ggproto Geoms)** is a contained refactor behind an existing
  visual-regression net, with no user-visible signature change. It could go
  in 0.6.0 with the freeze — arguably it *should*, since freezing a layer API
  that is a wrapper commits us to the wrapper.
- **§71's derived recommendation** waits on §34, §44 and §29.3 producing
  their measurements; the cheatsheet and decision tree do not, and are the
  cheapest documentation win available.

## 74. Sixth-pass references

- Wickham, H., Navarro, D. and Pedersen, T. L. *ggplot2: Elegant Graphics for
  Data Analysis* (3e), ch. 20, "Extending ggplot2".
  https://ggplot2-book.org/extensions.html *(§69.)*
- Extending ggplot2 (package vignette).
  https://ggplot2.tidyverse.org/articles/extending-ggplot2.html *(§69 —
  `ggproto`, `draw_key`, `default_aes`, and the ggproto Guide rewrite that
  opened guides to extension packages.)*
- Chen, T. Y., Cheung, S. C. and Yiu, S. M. (1998). Metamorphic testing: a
  new approach for generating next generation test cases. *(§72; the
  technique, later formalised across scientific software.)*
- Segura, S., Fraser, G., Sanchez, A. B. and Ruiz-Cortés, A. (2016). A survey
  on metamorphic testing. *IEEE Transactions on Software Engineering* 42(9),
  805–824. *(§72.)*

---

# Part III (continued) — seventh pass, 2026-08-29

*Seventh sweep, from two questions the previous six did not ask: **who uses
this mathematics under a different name?** and **is a result from this
package reproducible a year from now?** The first found an audience larger
than any yet identified; the second found a hole in the object itself.*

## 75. Theme AS — Statistical process control: the same mathematics, a
different vocabulary, a much larger audience

### 75.1 The observation

`cpt_monitor()` implements a mixture Shiryaev–Roberts e-detector, `cpm` and
`ocd`. Sequential changepoint detection is, historically and mathematically,
**statistical process control**: CUSUM is a sequential likelihood-ratio
changepoint test, EWMA is its smoothed cousin, and Hawkins, Qiu and Kang's
changepoint model for SPC (*Journal of Quality Technology*, 2003) is the
formal bridge — it is the paper `cpm` implements.

So the package already contains SPC. It just never says the word. Verified:
`grep -i` for "shewhart", "control chart" or "ewma" across `R/` returns
nothing but incidental `cusum` column names inside high-dimensional wrappers.

### 75.2 Why this is the largest unclaimed audience in Part III

Every manufacturing quality department, every clinical-outcomes monitoring
team, every laboratory QC function and a large share of operations analytics
runs control charts, in `qcc`, `spc`, `qicharts2` or Minitab. That population
is enormous, it is doing sequential changepoint detection, and it will never
search for "changepoint" — it searches for "control chart".

Note the package already reached for this audience once: `taylor` is wired
precisely because Taylor's change-point analyzer is "the quality-control /
Six-Sigma audience's default". That was the right instinct applied to one
engine. The audience is much bigger than one wrapper.

### 75.3 What to build

1. **`cpt_monitor(method = c("cusum", "ewma", "shewhart"))`.** These are a
   few dozen lines each, they are textbook, and they slot into the existing
   state machine — the same `cpt_update()`/`alarms()`/`cpt_delay()` contract
   the current three monitors use. Unlike the e-detector this is not
   implementing original methodology; it is implementing 1950s textbook
   statistics whose properties are completely known, and the ARL calibration
   is tabulated.
2. **`autoplot(type = "control_chart")`** — the display that audience
   expects: the statistic, the control limits, the out-of-control points
   flagged, run rules if wanted. It is `ggcpt_monitor`'s existing data in the
   idiom the reader knows.
3. **A translation vignette.** "Control charts and changepoint detection are
   the same thing" — ARL0 is the in-control average run length is
   `1 / alpha`; Phase I is offline detection, Phase II is monitoring; a
   CUSUM's reference value `k` is half the shift you are tuned for. A reader
   who understands one half instantly gains the other, and this package is
   uniquely placed to write that because it already has both.
4. **`as_ggcpt()` from a `qcc` object**, so an existing control chart becomes
   a `ggcpt` and gets every plot and metric here — the one-coercion-reaches-a-
   community move that §49 (HMM) and §58 (genomics) also propose.

### 75.4 Honest caveat

SPC has conventions this package does not model: rational subgrouping,
Western Electric run rules, capability indices, Phase I/Phase II separation.
Implementing CUSUM and EWMA without acknowledging those would produce
something an SPC practitioner recognises as naïve. The translation vignette
has to be honest about which conventions are and are not represented, and
`cpt_recommend()` should say "for formal process control, use `qcc`" where
that is the right answer.

## 76. Theme AT — Reproducibility across engine versions

### 76.1 The hole

A `ggcpt` records the method, the penalty, the call, the runtime and the
convention. Verified: **it does not record which version of the engine
produced it.** `cpt_report()` can append `sessionInfo()`, and that is the
only place any version information appears anywhere — it is not on the
object, not in `glance()`, and not checked on read-back.

The package depends on thirty-five engines maintained by other people. A
result produced today and re-run next year can differ because an upstream
package changed its default, fixed a bug, or altered a tie-breaking rule —
and **nothing in the package will notice or say so**. For an analysis that
gets published, audited or acted on, that is the reproducibility story
failing at the last step.

This is not hypothetical for this package specifically. Its own 0.5.0 audit
found upstream behaviour that was version-specific in three separate places:
`HDCD` 1.1's threshold bug (S23, guarded with an explicit
`packageVersion() <= "1.1"` check), `binsegRcpp` 2025.5.13's available cost
functions (S26), and `ChangePointTaylor`'s bootstrap range (S27). The
package already reasons about engine versions in its *code*; it just never
records them in its *results*.

### 76.2 What to build

1. **Stamp the object.** `ggcpt_build()` records `engine`, its
   `packageVersion()`, the ggchangepoint version and the R version. Costs a
   few bytes, and it is the difference between a result that can be audited
   and one that cannot.
2. **Surface it.** `glance()` gains `engine_version`; `print()` shows it when
   it differs from what is currently installed; `cpt_report()` stops needing
   full `sessionInfo()` to answer the one question that matters.
3. **`cpt_verify(fit)`** — re-run the recorded call against the currently
   installed engines and report whether the answer still holds. That is a
   genuinely useful function for anyone re-opening an analysis, and it is the
   natural home for the version-drift warning.
4. **Regression-detection on CI.** A stored set of (series, method, expected
   changepoints) re-checked on the scheduled all-engines job (§0.9 item 3),
   so *we* find out when an upstream release changes an answer, rather than a
   user finding out silently. This is the item that turns §0.9's CI job from
   "installs everything" into "installs everything and tells us what moved".

Item 4 is the one with the most leverage. The package wraps fifty engines; it
is guaranteed that some of them will change behaviour, and currently the only
detection mechanism is a user noticing.

## 77. Theme AU — Preprocessing is a decision nobody records

Detrending, differencing, smoothing, deseasonalising, log-transforming,
outlier removal — every one of these changes what a detector finds, and every
applied changepoint analysis does at least one of them. The package accepts
whatever vector it is handed and says nothing.

Two things worth having, in ascending order of ambition:

1. **Record it.** If preprocessing happened outside the package it cannot be
   recorded — unless the package offers the step itself. A thin
   `cpt_preprocess(x, detrend =, difference =, log =, deseasonalise =)`
   returning a `cpt_series` that *remembers* what was done, so the
   provenance reaches `cpt_report()` and §76's stamp. Not a statistics
   contribution; a bookkeeping one, and bookkeeping is what makes a report
   defensible.
2. **Measure the sensitivity.** `cpt_preprocess_sensitivity()` in the same
   family as `cpt_sensitivity()` and §44's contamination study: run the
   detection under a grid of preprocessing choices and show how the answer
   moves. "Your changepoint disappears if you difference first" is exactly
   the kind of finding the package's diagnostic philosophy exists to
   surface, and no changepoint package reports it.

There is a specific interaction worth naming: `bfast` and the seasonal
engines do their own decomposition internally, so preprocessing before them
double-counts. A user who deseasonalises and then runs `bfast` gets a subtly
wrong answer with no warning. That is a `cpt_recommend()` caveat waiting to
be written.

## 78. Theme AV — The Python bridge, in the direction nobody built

§18's extension mechanism lets a Python detector reached through `reticulate`
join this package's grammar; the `extending` vignette demonstrates exactly
that with `ruptures`. The reverse direction is empty: **a Python user cannot
reach these fifty engines, the inference, or the plots.**

That asymmetry is worth noticing because the Python changepoint ecosystem is
narrower than R's — `ruptures` and `claspy` between them cover a fraction of
what is wired here — while the Python *user base* for time-series work is
larger. The bridge that does not exist is the more valuable one.

The honest options, in order of cost:

1. **Document the round trip.** A vignette showing `rpy2` calling
   `cpt_detect()` and getting a pandas-shaped result back, built on §65's
   JSON schema. Costs a vignette; requires §65 first.
2. **A thin Python package** that shells to R or uses `rpy2`, exposing
   `detect()` and returning a dataframe. A sibling project, like §36's Shiny
   explorer — outside this package's dependency footprint, inside its
   mission.
3. Nothing beyond that. A full port is not a roadmap item; it is a different
   project.

Option 1 is nearly free once §65 exists and should be sequenced with it.

## 79. What the seventh pass changes

- **§76 (version stamping) belongs in 0.6.0**, and specifically with the 1.0
  contract freeze, because the stamp is part of the object contract and
  adding a field afterwards is the kind of change the freeze is meant to
  prevent. Items 1–2 are small; item 4 folds into §0.9's CI job.
- **§75 (SPC monitors)** is 0.7.0. CUSUM and EWMA are textbook and low-risk,
  the audience is the largest identified in Part III, and the translation
  vignette is the part that actually reaches them.
- **§77 (preprocessing)** — the recording half is small and pairs with §76's
  provenance; the sensitivity half sits with §44 and §51's diagnostics.
- **§78 (Python bridge)** waits on §65 and is then a vignette.

## 80. Seventh-pass references

- Hawkins, D. M., Qiu, P. and Kang, C. W. (2003). The changepoint model for
  statistical process control. *Journal of Quality Technology* 35(4),
  355–366. *(§75 — the formal bridge, and the method `cpm` implements.)*
- Page, E. S. (1954). Continuous inspection schemes. *Biometrika* 41(1/2),
  100–115. *(CUSUM; §75.)*
- Roberts, S. W. (1959). Control chart tests based on geometric moving
  averages. *Technometrics* 1(3), 239–250. *(EWMA; §75.)*
- Scrucca, L. (2004). qcc: an R package for quality control charting and
  statistical process control. *R News* 4/1, 11–17. *(§75.4 and the
  coercion in §75.3.)*
- Lawson, J. *An Introduction to Acceptance Sampling and SPC with R*.
  https://bookdown.org/lawson/an_introduction_to_acceptance_sampling_and_spc_with_r26/
  *(§75.3's translation vignette should not duplicate this; it should point
  at it and translate.)*

---

# Part III (continued) — eighth pass, 2026-08-29

*Eighth sweep, and the questions are getting narrower, which is itself
information. This one asks: **what does a program that calls this package
experience?** Not a person — a program. The answer exposes three gaps, and
one of them is a silent-wrong-answer risk of exactly the kind the 0.5.0 audit
kept finding.*

## 81. Theme AW — Errors a program can catch

### 81.1 The state of it

Verified: **189 `stop()` calls across `R/`, and not one carries a condition
class.** Every failure this package can produce is a bare `simpleError` whose
only distinguishing feature is English text.

That is fine for a person at a console and useless for anything else. A
caller who wants to react differently to "this engine is not installed" than
to "your series is too short" than to "this engine has an upstream bug at
this dimension" has exactly one option: match on the message string. Message
strings are not an API, they get rewritten — this release rewrote several of
them — and matching on them breaks silently.

This bites hardest in the places the package most encourages programmatic
use. `cpt_batch()`, `cpt_benchmark()` and `cpt_consensus()` all catch engine
failures and carry on, and each has to decide *which* failures are tolerable.
They currently decide by catching everything, which means a genuine bug in
the package is recorded in an `error` column and reported as a benchmark
result rather than raised.

### 81.2 What to build

A small condition hierarchy, all inheriting from `ggchangepoint_error`:

| Class | Raised when |
|---|---|
| `ggchangepoint_engine_missing` | the `Suggests` engine is not installed; carries `package` |
| `ggchangepoint_engine_error` | the upstream engine itself failed; carries `engine`, `engine_version` and the original condition |
| `ggchangepoint_input_error` | the data violates a precondition — too short, non-finite, wrong width; carries what was expected and what arrived |
| `ggchangepoint_unsupported` | the method/`change_in`/`family` combination is not offered; carries what is |
| `ggchangepoint_upstream_bug` | a known, version-guarded upstream defect — `pilliat` at power-of-two dimensions is the existing instance; carries `package`, `version` and a pointer |

`rlang::abort(class = , ...)` is the idiomatic route and `rlang` is already
in the dependency tree via `ggplot2`/`dplyr`; a hand-built `errorCondition()`
avoids even that. Either way the user-facing text does not change — this is
purely additive metadata — so it is a no-risk change with a large payoff for
anyone building on the package.

Then the three fan-out functions can be honest: catch
`ggchangepoint_engine_error` and record it, let `ggchangepoint_input_error`
and anything unclassed propagate. A benchmark that silently absorbs our own
bugs into an `error` column is a benchmark that hides exactly the defects
this package spent thirty-seven fixes learning to surface.

### 81.3 The same argument for warnings

`cpt_benchmark()`'s missing-ground-truth warning, `cpt_select()`'s collapsed
ladder, `drop_constant_cols()`, the irregular-index warning — all bare
`warning()`. A pipeline that wants to promote one of these to an error, or
suppress exactly one, cannot. Classed warnings cost the same nothing.

## 82. Theme AX — The changepoint convention, asserted but never verified

### 82.1 A real silent-wrong-answer risk

`ggcpt_build()` hardcodes `cp_convention = "left"`, and the contract says a
changepoint is **the last index of the segment before the change**. Fifty
engines feed that constructor. Each upstream package has its own convention —
some return the last index of the left segment, some the first index of the
right — and the difference is one observation.

Verified: exactly **one** wrapper comments on the adjustment
(`R/wrap-applied.R:150`, for `taylor`). `as_ggcpt()` exposes
`cp_convention = "right"` and subtracts one, so the machinery exists — it is
just that nothing checks whether each of the fifty wrappers used it
correctly.

An off-by-one changepoint is the ideal silent bug. It never errors, the plot
looks right, the segments still tile, every contract test passes, and the
reported location is wrong by one — which matters enormously when the index
is a date and the changepoint is being attributed to an event.

### 82.2 The test that settles it, and it is cheap

Construct a series with a change at a *known, unambiguous* location — a step
from exactly 0 to exactly 10 at a specified index, with no noise — and assert
that every engine capable of finding it reports the same index. Any engine
that disagrees by one has a convention bug in its wrapper, and the test names
it.

That is a single test file, it runs against all fifty engines, and it is
mechanically decisive. It is the same shape as the audit's contract sweep —
assert the *same* invariant across every engine at once — applied to the one
invariant the sweep did not check, because the sweep verified that `cp` was
sorted, unique, integer and in range, but never that it was *correct*.

### 82.3 And then document it per engine

`cpt_methods()` should carry the upstream convention as a column, so a user
comparing this package's answer with the engine's own output can see why they
differ by one. That is a documentation fix that only becomes possible after
the test in §82.2 establishes the truth.

## 83. Theme AY — Growing past one maintainer

Fifty engines, thirty-five `Suggests`, one maintainer, and upstream churn
that this release met three separate times. That arithmetic does not hold
indefinitely, and the registry was built for exactly the escape.

Verified: there is **no `CONTRIBUTING` file** and no issue templates —
`.github/` contains only `workflows/`.

1. **A wrapper cookbook.** The wrappers already share a documented shape:
   validate, guard `requireNamespace()`, call, extract, `ggcpt_build()`.
   Writing that down as a contributor guide — with the registry row, the
   citation, the capability flags and the tests each new engine needs — turns
   "add an engine" from a maintainer task into a pull request anyone can
   make. This is the highest-leverage documentation in the package and it
   does not exist.
2. **A wrapper template and a conformance test.** `use_cpt_wrapper("name")`
   scaffolds the file, the registry row, the Rd skeleton and a test that runs
   the contract sweep against the new engine. Contributors then cannot submit
   something that violates the contract without noticing.
3. **Issue templates**: bug (with a reprex and `cpt_methods()` output),
   engine request (with the CRAN link and the capability class it adds), and
   a question template that routes "which method?" to `cpt_recommend()`.
4. **A deprecation policy, written down.** `lifecycle` is imported and used
   in exactly one place (`ggecpplot(cptline_size)`, deprecated at 0.2.0).
   With the 1.0 freeze coming, the policy — what may change, how long a
   deprecation lives, what a breaking change requires — has to be stated
   before the freeze, not after.

None of this is code the users see. All of it is what determines whether the
package is still maintained in five years, which is the single largest risk
to everything else in Part III.

## 84. Theme AZ — What the package costs to install

The installed-size NOTE (5.4 Mb, of which 4.7 Mb is `doc`) is understood and
accepted, and six vignettes on a plotting package is a defensible reason. But
it is worth stating the trade honestly, because it grows with every theme
here that adds a vignette:

- vignette figures are the bulk; `dpi`, `fig.retina` and switching the
  heaviest figures to SVG would cut it materially with no loss to a reader;
- articles that are *not* vignettes — pkgdown `articles/` excluded from the
  build — carry the same content to the website without shipping in the
  tarball, which is the standard move for exactly this problem and would let
  the package add the translation vignette (§75), the SPC cookbook (§83), the
  case studies and the decision tree (§71) at zero installed cost;
- the split should be principled, not incidental: **vignettes teach the API,
  articles teach the domain.**

That rule would move at least two of the current six to `articles/` and give
every future documentation theme in Part III somewhere to go.

## 85. What the eighth pass changes

- **§81 (condition classes) and §82 (the convention test) both belong in
  0.6.0.** §81 is purely additive metadata and unblocks every programmatic
  caller; §82 is one test file that could reveal a real off-by-one in any of
  fifty wrappers, and it must run *before* the contract freeze, not after.
- **§83 (contributor infrastructure)** is 0.6.0 too, because the deprecation
  policy has to exist before the freeze it governs, and the cookbook is the
  cheapest way to stop the maintainer being the bottleneck.
- **§84 (vignettes versus articles)** is a documentation-architecture
  decision to take once, now, before Part III adds six more documents.

Noting the pattern: this pass produced no new statistics at all. Every item
is about the package being *depended upon* — by programs, by contributors, by
its future self. That is what a mature package's roadmap should start looking
like, and it is a reasonable signal that the method-hunting passes have found
most of what they are going to find.

## 86. Eighth-pass references

- Wickham, H. *Advanced R* (2e), ch. 8, "Conditions", §8.5 custom conditions.
  https://adv-r.hadley.nz/conditions.html *(§81.)*
- `rlang::abort()` reference — condition classes and metadata fields.
  https://rlang.r-lib.org/reference/abort.html *(§81; note the advice to
  prefix condition fields with the package name.)*
- The `lifecycle` package's stages and deprecation tooling. *(§83.4 — already
  imported here and used once.)*

---

# Part III (continued) — ninth pass, 2026-08-30

*Ninth sweep, from the applied side: **what does someone with a real question
actually ask, and can the package answer it?** Three of the four things here
come from questions that arrive before or instead of "find me the
changepoints", which is why eight passes of method-hunting did not surface
them.*

## 87. Theme BA — Testing a date you already have in mind

### 87.1 The question the package cannot answer

Someone has a series and a **specific date**: the policy took effect on 1
March, the plant was retooled in week 14, the drug was approved in Q2, the
outage started at 03:12. Their question is not "where are the changepoints?"
It is:

> **Did anything change at the date I already care about?**

Verified: `cpt_test()` takes a `ggcpt` and tests the changepoints *the
detector chose*. There is no entry point that takes a location and tests it.
A user with a hypothesis has to run a detector, hope it finds something near
their date, and then test that — which is a different and much weaker
question, and one contaminated by exactly the selection effect
`cpt_test(selection_adjusted)` exists to flag.

This is, in applied work, probably the single most common changepoint
question there is. Policy evaluation, incident post-mortems, product
launches, clinical protocol changes, regime dating in economics — all of them
start from a known date.

### 87.2 It is also the *easy* case, statistically

Testing a **pre-specified** location needs no selection adjustment at all.
There is no winner's curse, no post-selection inference, no conditioning: it
is an ordinary two-sample test at a fixed split, and the p-value means what
it says. The package's hardest inferential problem (§28, §29, and
`cpt_test()`'s adjustment machinery) exists precisely *because* the location
was chosen from the data. When it was not, all of that falls away.

So the package is in the odd position of having built the hard version and
not the easy one.

```r
cpt_test_at(x, when = as.Date("2026-03-01"), change_in = "mean")
cpt_test_at(x, when = 147, window = 14)   # allow the change within +/- 14
```

Returning the estimate, the interval, the p-value, and — the honest part —
`selection_adjusted = TRUE`, because for a genuinely pre-specified date it
*is*. The `window` variant is the realistic case (the policy took effect
sometime that fortnight) and does need an adjustment for the search over the
window, which is small and known.

### 87.3 And its companion: was the detected change *the* change?

The mirror question: a detector found a changepoint at 14 March and the user
knows something happened on 1 March. Is that the same event?
`cpt_confint()` already produces the interval that answers it; what is
missing is the framing — a `cpt_attribute_event()` that takes a detection and
a candidate date and reports whether the date falls inside the interval, with
`cpt_annotate_events()` already supplying the events.

## 88. Theme BB — Costs are asymmetric and nothing lets you say so

`cpt_metrics()` gives covering, F1, precision, recall. `cpt_delay()` gives
mean delay and a false-alarm count. Every one of those weights a false
positive and a false negative equally, and **in no real application are they
equal**.

A missed structural break in a risk model and a spurious alarm that halts a
production line are not the same mistake, and the balance differs by orders
of magnitude between domains. The quickest-detection literature is built
around exactly this trade-off — the whole point of an ARL constraint is that
false alarms are the expensive thing and delay is what you minimise subject
to them — and recent work formalises *temporal* weighting, where the cost of
an alarm depends on how close it lands to the event.

Proposed, and it is small:

```r
cpt_metrics(pred, truth, n, cost = c(fp = 1, fn = 10))
cpt_delay(mon, truth, cost = c(false_alarm = 1, per_unit_delay = 0.1))
cpt_benchmark(..., cost = ...)   # rank by expected loss, not by F1
```

The third is the interesting one. A benchmark that ranks by F1 answers "which
method is best in general", which nobody needs. A benchmark that ranks by
*your* loss function answers "which method should I use", which is the only
reason anyone reads a benchmark. `cpt_recommend()` should take the same
argument.

This also gives §29's coverage study and §34's timings a common currency:
expected loss is what lets a user trade accuracy against runtime, and right
now the package reports both and connects them not at all.

## 89. Theme BC — Method shopping, and the warning nobody gives

`ggcpt_compare()` runs several methods on one series and shows the answers
side by side. `cpt_consensus()` votes across them. Both are useful and both
have a failure mode the package never mentions:

> Run ten methods, keep the one that found a change, report it as if it were
> the only method you ran.

That is a garden of forking paths with a `ggplot2` interface, and this
package makes it *easier* than any other. Fifty methods behind one call is a
genuine contribution and also a genuine hazard, and the package's own
`cpt_test(selection_adjusted)` shows it already takes this class of problem
seriously — just not across methods.

Three responses, ascending:

1. **Say it.** A paragraph in `?ggcpt_compare` and the comparison vignette
   stating plainly that choosing the method by its answer invalidates the
   p-value, with the same directness the package already uses for the
   e-detector's guarantee and for `cpt_benchmark()`'s missing truth.
2. **Report it.** `ggcpt_compare()` returning how many methods were tried,
   and `cpt_test()` on a result whose `$call` came from a comparison
   carrying `selection_adjusted = FALSE` with a reason naming the
   multiplicity across methods, not just within one.
3. **Correct it.** A multiplicity adjustment across methods where the methods
   are exchangeable enough for one to mean anything — which is genuinely hard
   because they are not independent (they share engines, costs and
   assumptions, as `cpt_consensus()`'s own documentation already warns). An
   honest permutation-based version is possible and would be novel.

Item 1 costs a paragraph and should not wait. The package's credibility rests
on being the tool that tells you when your answer is weaker than it looks;
this is the largest remaining place where it does not.

## 90. Theme BD — How many datasets does a benchmark need?

`cpt_benchmark()` runs a grid and draws a Nemenyi critical-difference
diagram, so the machinery for "is this difference real?" is there. The
question it cannot answer is the one that comes first:

> I want to show method A beats method B. **How many datasets do I need?**

The critical difference is `q * sqrt(k(k+1)/(6N))` — explicit in N — so
inverting it for a target detectable rank difference is arithmetic the
package can already do. A `cpt_benchmark_power(k, N, ...)` alongside
`cpt_power()` would do for benchmark design what `cpt_power()` does for study
design, and the same argument applies: it is the number that belongs in the
protocol, before the benchmark is run.

This is genuinely novel — benchmark power is not something changepoint papers
report, and a package that both runs benchmarks and computes their power
would be in a position to raise the standard rather than just meet it. It is
also small: the CD formula is already implemented and tested against
Demšar's table.

## 91. What the ninth pass changes

- **§87 (`cpt_test_at()`) goes into 0.6.0.** It answers the most common
  applied question, it is the statistically *easy* case, and the package
  looks strange having built the hard version first. Perhaps a hundred lines.
- **§89.1 (the method-shopping warning)** is a paragraph and belongs in
  0.6.0 with it. §89.2–3 can follow.
- **§88 (asymmetric costs)** is 0.7.0, with the inferential work, because
  cost-weighted ranking is what makes §29's coverage and §34's timings
  comparable to each other.
- **§90 (benchmark power)** is small and can go wherever; it pairs naturally
  with §34's performance table, since both are about making the benchmark
  itself defensible.

## 92. Ninth-pass references

- Tartakovsky, A., Nikiforov, I. and Basseville, M. (2014). *Sequential
  Analysis: Hypothesis Testing and Changepoint Detection.* CRC Press. *(The
  decision-theoretic framing behind §88 — delay minimised subject to a
  false-alarm constraint.)*
- Weighted Score-Oriented Losses for Temporally Localized Event Prediction
  (2026). arXiv:2606.23145. *(§88's temporal weighting — the cost of an alarm
  depends on where it lands relative to the event.)*
- Post-detection inference for sequential changepoint localization (2025).
  arXiv:2502.06096. *(§87.3 and the sequential counterpart of §29.)*
- Demšar, J. (2006). Statistical comparisons of classifiers over multiple
  data sets. *JMLR* 7, 1–30. *(Already cited and implemented; §90 inverts its
  critical-difference formula for N.)*
- Gelman, A. and Loken, E. (2013). The garden of forking paths. *(§89 — the
  framing, and why a fifty-method interface needs to name the hazard it
  creates.)*

---

# Part III (continued) — tenth pass, 2026-08-30

*Tenth sweep. This one comes from the two answers the package gives most
often and explains least: **"here are K changepoints"** — with no statement
about K itself — and **"no changepoints detected"**, which it prints as a
period at the end of a sentence when it is really the beginning of a
question.*

## 93. Theme BE — "No changepoints detected" is not an answer

### 93.1 What the package currently says

Verified, `R/ggcpt-class.R:135`:

```
No changepoints detected.
```

That is the whole of it. A user who gets that line has no way to tell apart
two completely different situations:

- **the series is stable** — there is genuinely nothing there; or
- **the study had no power** — the change is real, and this n, this noise
  level and this penalty could never have found it.

Those call for opposite actions. The first ends the analysis; the second says
collect more data, or use a different method, or lower the penalty and accept
more false positives. The package knows enough to tell them apart and does
not.

The irony is sharp: `cpt_power()` and `cpt_min_detectable()` exist and answer
exactly this — *before* the analysis. Nothing connects them to the moment the
question is actually asked, which is when the answer comes back empty.

### 93.2 What to build

1. **Make the empty answer informative.** When a detection returns zero
   changepoints, `print()` and `glance()` should be able to say: *at this n
   and this residual sd, this method and penalty would have detected a shift
   of about δ with 80% power; a smaller change would probably have been
   missed.* That is `cpt_min_detectable()` called on the observed noise, and
   it turns a full stop into a usable statement.
2. **`cpt_test_null(x, method, ...)`** — an explicit test of the global null
   "there is no changepoint anywhere", which is a different and better-posed
   question than testing a located one. Several wired engines already compute
   a global statistic and threshold (`nsp`, `mosum`, `npmojo`, `hdcov`,
   `network`, the `trend` family are literally single-change *tests*), so for
   a good share of the fifty this is extraction rather than invention.
3. **Say it in `cpt_recommend()`.** "You have n = 60 and expect a half-sigma
   shift; no method here will find that reliably" is more useful advice than
   a ranking, and it is `cpt_power()` run over the recommendation.

None of that is new statistics. It is connecting three things the package
already has to the one moment they matter most.

## 94. Theme BF — Uncertainty about K

### 94.1 The gap

`cpt_select()` chooses a K by one of six criteria and returns it. Verified:
`criterion_table` carries one row per candidate with its criterion value, so
the *information* about neighbouring Ks is right there — and nothing in the
API says how much better the winner was, or what would change if it had been
K−1.

For the Bayesian engines the situation is worse in a different way: `bcp`,
`beast` and `mcp` produce a genuine **posterior over the number of
changepoints**, and the package collapses it to a point estimate on the way
into `$changepoints`. `beast` is described in the package's own documentation
as doing "model averaging", and none of that averaging survives into the
result.

### 94.2 What to build

1. **`cpt_k_uncertainty(sel)`** on a `ggcpt_selection`: the criterion curve
   normalised into weights (Akaike weights are the standard move for
   AIC/BIC-family criteria), so a user sees "K = 3 has 0.55 of the weight,
   K = 4 has 0.31" instead of "K = 3". A curve with a flat bottom is a
   *finding* — it means the data do not determine K — and the current API
   cannot express it.
2. **Preserve the posterior where an engine supplies one.** A `k_posterior`
   slot alongside `regions` and `diagnostics`, populated only by the Bayesian
   engines, with `autoplot(type = "k")` drawing it. Same additive-slot
   pattern that worked for NSP.
3. **Model-averaged changepoint probability.** Given weights over K, the
   probability that *each location* is a changepoint, averaged over models —
   which is the single most informative one-dimensional summary a
   changepoint analysis can produce, and which `ggcpt_posterior()` already
   knows how to draw for the two engines that supply it natively. Extending
   it to the frequentist engines via weights would give every method a
   version of the picture that currently only `bcp` and `beast` get.

Item 3 is the one worth reaching for. It would mean that "where are the
changepoints" is answered by a *curve over the series* rather than a set of
points, for all fifty methods — and the package's whole visual thesis is that
the curve is the more honest object.

## 95. Theme BG — Does the package's own inference behave?

§29.3 proposes measuring the realised coverage of `cpt_confint()`. The same
argument applies to everything else the package asserts, and it is worth
stating as a programme rather than a one-off:

| What is asserted | The measurement that would check it |
|---|---|
| `cpt_confint()` levels | realised coverage by provenance (§29.3) |
| `cpt_test()` p-values | are they uniform under the null? |
| `cpt_monitor(alpha)` | realised in-control ARL vs `1/alpha` — **already done** for the e-detector (S11), and done for *no other monitor* |
| `cpt_attribute()` (§28) | realised FWER and FDR |
| `cpt_select()` | how often is the chosen K the true K, by criterion and by n |
| `cpt_metrics()` | agreement with the reference implementation on the TCPD |

Five of those six are unmeasured. The package has already learned, expensively
and in public, what happens when an asserted guarantee goes unmeasured: the
e-detector's ARL bound was wrong by a factor of two while every call
succeeded, and it was found only because someone measured it.

Proposed: a `calibration/` set of long-running scripts, run on the scheduled
CI job (§0.9 item 3), whose output is a **calibration vignette** — a table of
what the package promises against what it delivers, regenerated each release.
No other changepoint package publishes that, it is the natural extension of
the discipline that produced 0.5.0's audit, and it would be the most
persuasive single document the project could put in front of a sceptical
reader.

## 96. Theme BH — A catalogue of how changepoint analysis goes wrong

The package now knows an unusual amount about failure. Thirty-seven defects
across 0.5.0, and behind them a stock of specific, demonstrable ways that a
changepoint analysis produces a confident wrong answer:

- an abrupt detector on a gradual change, confidently mislocated (§47);
- a factor coerced to level codes, producing a mean shift in nothing (§63.2);
- autocorrelation read as a sequence of changepoints — the single most common
  real-world error, and the reason `decafs` and `envcpt` are wired;
- a penalty chosen after seeing the answer;
- the method chosen after seeing the answer (§89);
- preprocessing that created the change it found (§77);
- an effect size read off at the detected location (§42.3);
- outliers read as changepoints, and changepoints read as outliers (§57);
- a monitor whose baseline drifted (§64.4);
- a benchmark whose ground truth was silently missing (fixed in 0.5.0 as
  S18 — and that it *was* a defect is the point).

Each of those is a few lines of code to demonstrate, a picture, and a
sentence on how to avoid it. Together they are a vignette — *"Ten ways to get
a changepoint wrong"* — that would be the most-read document the package
ships, and quite possibly more useful to the field than any additional
engine.

It is also the honest use of what the audit produced. Thirty-seven fixes are
recorded in §0.8 as a ledger of the package's own mistakes; the same
knowledge, turned outward, is a teaching document. A package that says "here
is how this goes wrong, including in our own code" earns a kind of trust that
no feature list buys.

## 97. What the tenth pass changes

- **§93 (the informative empty answer) belongs in 0.6.0.** It connects three
  existing functions, it costs little, and "no changepoints detected" as a
  bare sentence is the least helpful thing the package says.
- **§96 (the failure catalogue)** is a vignette — or, under §84's rule, a
  pkgdown *article* — and can be written at any time. It needs no code.
- **§94 (uncertainty about K)** is 0.7.0 with the inferential themes; item 3
  in particular is the kind of thing that changes what the package is for.
- **§95 (the calibration programme)** should start as soon as §0.9's CI job
  exists, and grow one row at a time as each guarantee gets measured.

Ten passes in, the shape of the remaining work is clear and it is not more
methods. It is: connect what exists (§93), quantify what is asserted (§95),
name the failure modes (§96), and make the answers honest about their own
uncertainty (§94). If an eleventh pass finds nothing but engines, that is the
signal that this document is finished.

## 98. Tenth-pass references

- Jewell, S., Fearnhead, P. and Witten, D. (2022). Testing for a change in
  mean after changepoint detection. *JRSS-B* 84(4), 1082–1104.
  *(`ChangepointInference`; the conditioning-set argument in §93.2, and the
  observation that power increases as the conditioning set grows.)*
- D'Angelo, N. et al. (2025). Testing for a general changepoint in medical
  and psychometric studies: change detection and sample size planning.
  *Statistics in Medicine*. doi:10.1002/sim.70150. *(§93 — the sample-size
  side of the empty answer.)*
- Burnham, K. P. and Anderson, D. R. (2002). *Model Selection and Multimodel
  Inference*. Springer. *(Akaike weights; §94.1.)*
- Hoeting, J. A., Madigan, D., Raftery, A. E. and Volinsky, C. T. (1999).
  Bayesian model averaging: a tutorial. *Statistical Science* 14(4), 382–401.
  *(§94.3.)*
- Talts, S., Betancourt, M., Simpson, D., Vehtari, A. and Gelman, A. (2018).
  Validating Bayesian inference algorithms with simulation-based calibration.
  arXiv:1804.06788. *(§95 — the discipline, and the name for it.)*

---

# Part III (continued) — eleventh pass, 2026-08-30

*The tenth pass set a stopping rule: if this pass finds nothing but engines,
the document is finished. It found five non-engine themes, so the rule is not
triggered — but three of them are about the package's own long-term
survival rather than its capability, which is the same signal arriving more
slowly. §103 says so plainly.*

## 99. Theme BI — The same series at two resolutions is two different questions

Every detector here runs at whatever resolution the input arrives in. But a
series has no privileged resolution, and the answer changes with it:

- a change that is obvious in **weekly totals** can be invisible in daily
  data, because the daily noise swamps it and aggregation is a low-pass
  filter with exactly the effect on signal-to-noise you would expect;
- a change that is sharp in **hourly** data becomes a gradual ramp in daily
  aggregates — §47's gradual-change problem, manufactured by the analyst's
  choice of resolution rather than by the world;
- a **level shift** at one resolution can present as a **variance change** at
  another.

Nothing in the package says any of this, and the choice of resolution is
usually made before the analyst thinks of it as a choice — the data arrived
that way.

The forecasting literature has been here: multiple temporal aggregation
(Kourentzes' MAPA line) treats the resolution as something to *sweep* rather
than fix, on the argument that series features are enhanced at different
frequencies and different methods win at each. The changepoint analogue is
direct, and the multi-scale detection literature (pyramid RNNs, wavelet
scale-invariance) is the deep-learning version of the same insight.

Proposed:

```r
cpt_multiscale(x, index, aggregate = c("1 day", "1 week", "1 month"),
               fun = sum, method = "pelt")
```

returning one detection per resolution, aligned on the *original* time axis,
with a display that stacks them — the same shape as
`ggcpt_scale_space()`, but sweeping temporal aggregation rather than
bandwidth. A changepoint that survives every resolution is a different kind
of finding from one that appears at exactly one, and the picture says which
immediately.

This is also the honest home for a warning the package should give anyway:
when an index is supplied and the spacing implies an aggregation the user
may not have chosen, say so.

## 100. Theme BJ — What happens when an engine leaves CRAN

This is not hypothetical. **`hdbinseg` has been archived and restored more
than once within this package's own history** — and see §106: as of
2026-08-30 it is *back* on CRAN at 1.0.3, so §9.1's statement is true again
and the claim made here in the eleventh pass (that it had gone) was wrong.
That a maintainer can be wrong about the status of their own dependency
inside a single day is the argument for this theme, not against it. `gfpop`,
`robseg`, `cpss`, `Segmentor3IsBack`, `changepoint.mv` and `cpcens` are all
removed. With thirty-five suggested
engines, archival is not an event; it is a rate.

Verified: there is no policy. `planned_methods()` carries a `target_release`
of `"when on CRAN"` for the five that are currently unavailable, which
handles a method that was *never* wired. Nothing handles a method that **was**
wired and whose engine then disappears.

What is missing:

1. **A stated policy.** When an engine is archived: the registry row moves to
   `status = "archived"` rather than vanishing; `cpt_detect()` reports what
   happened and points at the archive and the nearest wired alternative;
   the wrapper and its tests stay in place, skipped, so the method returns
   the day the engine does. Deleting the wrapper is the wrong move and it is
   the one that happens by default.
2. **Detection.** The scheduled CI job (§0.9 item 3) should check CRAN
   availability for every suggested engine and open an issue when one
   disappears — the maintainer currently finds out when a check breaks or a
   user complains.
3. **A migration note per archived engine.** `sbs` gone means "use `esac` or
   `inspect` for high-dimensional mean changes"; that mapping is knowledge
   the registry already contains, via the capability columns, and could be
   generated rather than written.

This is unglamorous and it is the difference between a package that ages and
one that rots. Thirty-five dependencies is a lot of surface exposed to other
people's decisions.

## 101. Theme BK — Two reproducibility hazards the package has already met

§76 covers *engine versions*. Two other ways the same answer fails to
reproduce, both of which this project has hit in practice:

1. **The numerical environment.** This package's own development notes record
   an OpenBLAS build that segfaulted on the wrong CPU, and a compiler
   toolchain that had to be pinned to build engines at all. Different BLAS
   implementations give different last-digit results; a changepoint is an
   `argmax` over a cost curve, and an `argmax` is exactly where a last-digit
   difference becomes a *different answer by one index*. Nobody has measured
   whether any wired engine is close enough to a tie for that to matter.
   A cheap probe would: run the contract sweep under a reference BLAS and
   again under another, and diff the changepoints. If nothing moves, the
   package can say so, which is worth saying. If something moves, that is a
   finding worth publishing.

2. **RNG streams.** §0.5's parallel work established that `future.seed`
   makes `cpt_batch()` and friends reproducible under a plan — verified in
   the audit. What is not established is reproducibility *across R versions*,
   where the sample-kind default has changed before, or across a change in
   the number of workers. A stated guarantee — "same seed, same answer,
   independent of worker count; not guaranteed across R minor versions" —
   would be honest and is currently absent.

Both are documentation-and-measurement items rather than features, and both
belong with §95's calibration programme, which is the right home for
"measure what we assert".

## 102. Theme BL — What the package costs to load

Measured: `library(ggchangepoint)` takes about **1 second** and pulls in
**38 namespaces** from 12 declared `Imports`.

That is unremarkable for interactive use and it is not free anywhere else. A
Shiny app, a `plumber` endpoint (§64), a scheduled job, or another package
that imports this one pays it on every process start. `ggplot2` and `dplyr`
account for most of it, and both are load-bearing here — this is a plotting
package with tidy verbs, so neither is negotiable.

What *is* negotiable is whether they are needed at load:

- `dplyr` is used in a handful of places and could very plausibly be replaced
  by base equivalents, since the package already avoids the tidy-eval
  machinery almost everywhere;
- `Rdpack` is a documentation-time dependency in `Imports` because of the
  `RdMacros` field, which is standard but worth confirming it costs nothing
  at load;
- a deliberate decision, recorded, that `ggplot2` stays an `Import` rather
  than becoming a `Suggests` behind lazy loading — because the package's
  identity *is* the plotting, and making it optional to save a second would
  be the wrong trade.

The point is not that a second is expensive. It is that this is the kind of
number a package should know about itself before someone else measures it,
and that the answer for each dependency should be a decision rather than an
accident. It also feeds §84: install size and load time are the two costs a
downstream user actually feels.

## 103. Theme BM — Automated explanation, treated honestly

There is 2026 work on LLM-augmented changepoint detection — using a language
model to ensemble detectors and to produce automated *explanations* of what
changed and why (arXiv:2601.02957). Given the direction of the field this
will not be the last such paper, and the package should have a position
rather than acquire one by drift.

The position that follows from everything else in this document:

- **Not as a detector.** The package's rule is to wrap published,
  separately-maintained implementations and to implement original methodology
  once, deliberately, labelled. An LLM ensemble is neither, and its outputs
  carry no guarantee of the kind §28, §29 and §95 are built around.
- **As a consumer of our output, yes, and that is already supported.** §65's
  JSON schema is exactly what such a system needs; `cpt_report()` and
  `cpt_alt_text()` already generate prose from a result deterministically.
  The package's contribution is being *reliably readable*, not being clever.
- **The ensemble question is already answered better.** `cpt_consensus()`
  votes across methods with a stated rule and a visible vote count, and its
  documentation already warns that the methods are not independent. That is a
  more defensible ensemble than an opaque one, and the honest comparison
  belongs in the vignette.

Recording this as a decision, like §66's, matters more than the feature would.
A package whose whole argument is "we tell you when the answer is weaker than
it looks" cannot add an unverifiable explanation layer without contradicting
itself.

## 104. Where this document stands after eleven passes

The tenth pass proposed a stopping rule. Applying it honestly:

**The rule is not triggered** — this pass found five themes and none of them
is an engine. But the *character* has shifted decisively. Of the five, one is
a capability (§99), one is a decision to decline (§103), and three (§100,
§101, §102) are about the package continuing to work: archival, numerical
reproducibility, load cost. That is a maintenance roadmap, not a feature
roadmap.

Read across all eleven passes, the durable content is:

- **four things to build in 0.6.0** that connect existing pieces —
  `cpt_test_at()` (§87), the informative empty answer (§93), families (§30),
  effect size (§42) — plus the freeze-adjacent work: condition classes (§81),
  the convention test (§82), version stamping (§76), contributor
  infrastructure (§83);
- **two inferential differentiators for 0.7.0** — attribution (§28) and
  conformal intervals (§29) — with epidemic changepoints (§57) as the
  largest representational gap;
- **one measurement programme** (§95) that turns every asserted guarantee
  into a published number;
- **two documents** that would probably be read more than any feature: the
  failure catalogue (§96) and the calibration vignette (§95);
- and a long tail of engines and data types that will keep arriving and can
  be taken as they come.

Everything after that is refinement. A twelfth pass should look for what has
*changed in the world* — a new CRAN engine, a new method, a bug report — and
add only that. **The generative phase of this document is over; the
maintenance phase has started.**

## 105. Eleventh-pass references

- Kourentzes, N., Petropoulos, F. and Trapero, J. R. (2014). Improving
  forecasting by estimating time series structural components across multiple
  frequencies. *International Journal of Forecasting* 30(2), 291–302. *(MAPA;
  the temporal-aggregation argument behind §99.)*
- Ebrahimzadeh, Z. et al. (2019). Deep learning for multi-scale changepoint
  detection in multivariate time series. *(Pyramid RNNs and wavelet
  scale-invariance; §99.)*
- Hierarchical Spatio-Temporal Change-Point Detection (2023). *The American
  Statistician*. doi:10.1080/00031305.2023.2191670. *(§99's hierarchical
  cousin, and it also serves §33.)*
- LLM-Augmented Changepoint Detection: A Framework for Ensemble Detection and
  Automated Explanation (2026). arXiv:2601.02957. *(§103 — recorded as a
  decision to decline, not a plan.)*

---

# Part III (continued) — twelfth pass, 2026-08-30

*§104 committed this pass to a narrower job than the eleven before it: look
for **what has changed in the world**, and add only that. So this one is
short, and most of it is a correction rather than an idea. That is the
maintenance phase working as intended.*

## 106. The dependency world, checked against the live CRAN index

Run against `available.packages()` on 2026-08-30 — one line of R, which is
exactly the point of §100.2.

### 106.1 Every declared dependency is currently on CRAN

All 12 `Imports` and all 56 `Suggests` resolve against the live index. The
only names that do not are `stats`, `tools` and `utils`, which are base
packages and are supposed to be absent from it. **No engine this package
depends on has been archived.**

That is a good result and it is also the demonstration that §100.2's proposed
CI check is trivial: the whole thing is
`setdiff(deps, rownames(available.packages()))`. There is no reason for that
not to be in the scheduled job, and no excuse for the maintainer finding out
about an archival from a broken check.

### 106.2 Two corrections to earlier passes

Checking the index found two things this document asserted and got wrong.
Both are recorded here rather than quietly edited away, and both sentences
now point back at this section.

| Claim | Where | The truth on 2026-08-30 |
|---|---|---|
| "`hdbinseg` has been archived … §9.1's sentence is now false" | §100, eleventh pass | **Wrong. `hdbinseg` is on CRAN at 1.0.3.** §9.1 is true again, and `sbs`/`dcbs` are a wrapper task, not an archive wait. |
| "`changepoint.mv` 1.0.2 … `cpcens` …" listed as CRAN engines to wrap | §41, second pass | **Wrong. Both are archived.** Theme Y's panel engines are *not* available; they are registration targets or `planned` rows. |

The `hdbinseg` error is the more interesting one, because it was made *by the
maintainer, about this package's own dependency, on the same day the index
said otherwise* — working from a stale note rather than from the index. That
is precisely the failure §100 exists to prevent, arriving as a demonstration
instead of a hypothetical. It also means Theme Y (§41) is weaker than
written and `sbs` (§9.1) is stronger.

### 106.3 Live status of everything Part III proposes wrapping

| Package | Status | Serves |
|---|---|---|
| `anomaly` 4.3.3 | **on CRAN** | §57 epidemic changepoints — still the strongest remaining addition |
| `changepointGA` 0.1.5 | **on CRAN** | §31 genetic search |
| `GA` 3.2.5 | **on CRAN** | §31 |
| `scanCP` 0.1.0 | **on CRAN** | §32 neural detection |
| `CPsurv` 1.0.0 | **on CRAN** | §63.3 survival/hazard |
| `cpsurvsim` 1.2.2 | **on CRAN** | §63.3, and a survival mode for `cpt_simulate()` |
| `depmixS4` 1.5-4 | **on CRAN** | §49 HMM coercion |
| `qcc` 2.7 | **on CRAN** | §75 SPC coercion |
| `hdbinseg` 1.0.3 | **on CRAN** | §9.1 `sbs`/`dcbs` |
| `DNAcopy` | Bioconductor | §58 — as §58 already said |
| `gfpop`, `robseg`, `FOCuS`, `changeforest`, `cpss`, `Segmentor3IsBack`, `ChangepointInference`, `changepoint.mv`, `cpcens` | **absent** | registration targets (§18), not wrappers |

Nine of the eleven engines Part III proposes wrapping are available today.
That is a better position than the document assumed.

## 107. One genuinely new find: `ChangepointTesting`

`ChangepointTesting` 1.2 (CRAN, May 2025) implements **a multiple testing
procedure for clustered alternative hypotheses**: null p-values are uniform,
alternatives are stochastically smaller, and the method gains power by
averaging over *neighbouring* p-values — so an isolated small p-value is
damped as noise while a run of them reinforces.

That is the structure of a changepoint problem, and it lands squarely on
**§60 (screening many series)**, which was proposed abstractly with
Benjamini–Hochberg as the placeholder. BH treats the series as exchangeable
and ignores the fact that in most panels the ones that changed are *adjacent*
— neighbouring sensors, neighbouring genomic bins, neighbouring stores.
Clustered-alternative testing is the right tool for that and it is on CRAN.

It also touches **§28 (attribution)**: the anomalous coordinates in a
multivariate change are frequently clustered too, and the same argument for
borrowing strength across neighbours applies.

So §60 gains a concrete engine, and its priority rises: it is no longer "we
would have to build a multiplicity correction", it is "there is a better one
than BH already packaged".

## 108. What the twelfth pass changes

- **§106.1's availability check goes into the scheduled CI job** with §0.9
  item 3. One line, and it closes the loop §100 opened.
- **§41 (panel) is downgraded.** Its two named engines are archived. The
  theme survives — `mcp`'s varying changepoints are still one argument away,
  and `cpt_panel()` with `pooling = "none"` is still better than
  `cpt_batch()` — but it is a build, not a wrap, and it should move behind
  §57 and §60.
- **§9.1's `sbs`/`dcbs` is upgraded.** `hdbinseg` is live, so this is an
  ordinary wrapper task again and one of the cheapest engine additions
  available.
- **§60 (screening) is upgraded**, with `ChangepointTesting` as its engine.
- Nothing else changed. No new methodology, no new gap, no new failure mode.

That last line is the finding. Twelve passes in, a deliberate search of the
literature and the package index produced one new engine, two corrections and
zero new themes. §104 predicted this shape and it has arrived on schedule:
**the document is complete as a generative exercise.** What it needs from
here is what this pass did — periodic re-checking against a world that
moves — and the next thing that should happen to it is not another pass but
0.6.0.

## 109. Twelfth-pass references

- `ChangepointTesting` 1.2. CRAN, 2025-05-03. *(§107.)*
- CRAN index queried directly via `available.packages()`, 2026-08-30, 24,795
  packages. *(§106 — the method, not a citation: the index is the source, and
  that is the point of the theme.)*

---

# Part III (continued) — thirteenth pass, 2026-08-30

*§108 said the generative phase is over and the next thing should be 0.6.0,
not another sweep. So this pass does not look for themes. It does the two
things standing between this document and being actionable: it
**feasibility-checks the highest-priority proposals against the real engine
APIs**, which nobody had done, and it **consolidates twelve scattered "what
this pass changes" sections into one specification**.*

## 110. Feasibility notes: what the top proposals actually have to deal with

Every theme in Part III was written from a paper or an index entry. That is
enough to decide *whether* to build something and not enough to estimate it.
These are the API-level facts, checked, for the four highest-priority
engine-wrapping themes.

### 110.1 §57 `anomaly` (CAPA/MVCAPA) — S4, and the extractors do not handle it

`capa()` returns an **S4 object**, with accessors `collective_anomalies()`
(location, lags, mean/variance changes) and `point_anomalies()` (location,
strength).

Three consequences the theme did not anticipate:

1. **The package's field extractors all fail on it.** `fld()`, `pfld()` and
   `fl()` in `R/diagnostics.R`, and `mcp_has_samples()` in `R/wrap-mcp.R`,
   are all written as `if (is.list(fit)) fit[[nm, exact = TRUE]] else NULL`.
   That guard was added in the 0.5.0 audit precisely to survive odd fits —
   and on an S4 object it returns `NULL` silently, which is the wrong answer
   rather than an error. Wrapping `anomaly` means either the wrapper never
   goes through those helpers, or the helpers learn about S4.
   **This is a change to shared infrastructure, not a new file.**
2. **The package already handles S4 fits twice** — `fastcpd` (`fit@cp_set`)
   and `geomcp` (`fit@dist.cpts`, `fit@ang.cpts`) — but by reaching for slots
   directly in the wrapper, with no shared helper. A third S4 engine is the
   point at which a `fit_field()` that dispatches on S3/S4 stops being
   over-engineering.
3. **Point anomalies are not intervals.** The theme assumed the `regions`
   slot carries everything, and it carries *collective* anomalies fine —
   start, end, and the mean/variance change. A **point** anomaly is a single
   flagged observation with a strength: not an interval, not a changepoint,
   and not currently representable. That is a real design decision the theme
   deferred without noticing: either `regions` gains `start == end` rows with
   a `type` column, or point anomalies get their own slot. The first is
   cheaper and probably right, but it needs saying before anyone writes code.

Net: §57 is still the strongest remaining addition, and it is a **larger job
than "one wrapper"** — call it a wrapper plus an extractor refactor plus a
contract decision.

### 110.2 §31 `changepointGA`, §32 `scanCP`, §107 `ChangepointTesting`

All three are on CRAN (§106.3) and all three are new enough (0.1.5, 0.1.0,
1.2) that their APIs should be treated as unstable — which argues for
registering them via §18 first and promoting to wrappers once they settle.
That is exactly the use case the extension mechanism was built for and it has
never been used in anger. **Doing this would test §18's central claim**: that
a registration is a real substitute for a wrapper, not a consolation prize.

A concrete proposal follows from that: before wrapping any of the three,
write the registration in the `extending` vignette, run the contract sweep
against it, and see what breaks. If nothing does, the mechanism is proven and
the wrapper is optional. If something does, that is the most valuable bug
report the extension mechanism will ever get.

### 110.3 The general lesson for the rest of Part III

Nine of the eleven proposed engines are available (§106.3), and this pass
looked closely at one of them and found three unanticipated consequences.
The base rate matters: **assume every engine-wrapping theme in this document
is under-estimated by roughly one shared-infrastructure change.** That is not
an argument against them; it is the correction factor to apply when
sequencing.

## 111. The consolidated 0.6.0 specification

Twelve passes produced twelve "what this pass changes" sections (§38, §45,
§52, §61, §67, §73, §79, §85, §91, §97, §104, §108). Nobody can act on
twelve. This is one.

**0.6.0 is a stabilisation release with a small, deliberate feature set.**
Its organising rule: *nothing that adds an engine, everything that connects
what exists or that must be settled before the contract freezes.*

### 111.1 Must ship before the freeze, because the freeze governs them

| Item | Theme | Why it cannot wait |
|---|---|---|
| Condition classes on all 189 `stop()`s and the warnings | §81 | Error classes are API. Freezing without them means bare `simpleError` forever, or a breaking change later. |
| The changepoint-convention test across all 50 engines | §82 | An off-by-one in any wrapper becomes frozen behaviour. One test file, mechanically decisive. |
| Engine-version stamping on `ggcpt` | §76 | A new slot after the freeze is exactly what a freeze forbids. |
| A written deprecation policy | §83.4 | The policy has to exist before the thing it governs. |
| The `as_json()` schema | §65 | If pipelines depend on it, the field names are part of the contract. |
| Real ggproto Geoms | §69 | Freezing a layer API that is a wrapper commits us to the wrapper. Behind the 25 vdiffr snapshots this is contained. |
| Property and metamorphic tests | §72 | A contract about to be frozen should be *mechanically* checked first. |

### 111.2 Small features that unlock users who cannot currently run the package

| Item | Theme | Size |
|---|---|---|
| `family =` for Poisson/Gamma/exponential/Laplace/L1 | §30 | Argument + registry column; the costs already exist inside `changepoint` and `binsegRcpp` |
| `na_action` and grouped data frames | §37.1–2 | Input hardening |
| Refuse a factor / `Surv` / event-time input instead of coercing | §63.4 | One line per type; a silent wrong answer today |
| `cpt_test_at()` — test a pre-specified date | §87 | ~100 lines; the most common applied question, and the statistically easy case |
| `cpt_effect()` with `min_effect` filtering | §42, §48 | Needs the selection-bias caveat, not just a subtraction |
| Informative "no changepoints detected" | §93 | Connects `cpt_min_detectable()` to the empty answer |
| `autoplot(type = "diagnostics")` + `cpt_gof()` | §51 | Reuses `augment()`; also fixes its first-coordinate-only limit |

### 111.3 Measurement and infrastructure

| Item | Theme |
|---|---|
| The performance table, n = 10⁴/10⁵/10⁶ | §34.1, §0.9 |
| Scheduled all-engines CI, **plus the one-line CRAN-availability check** | §0.9, §100.2, §106.1 |
| The calibration suite, starting with `cpt_confint()` coverage | §95, §29.3 |
| Contributor guide, wrapper cookbook, issue templates | §83.1–3 |
| Bundled datasets in `data/` | §59 |
| The vignettes-versus-articles split | §84 |

### 111.4 Documents, which need no code

- *"Ten ways to get a changepoint wrong"* (§96) — likely the most-read thing
  the package would ship.
- The method-shopping warning in `?ggcpt_compare` (§89.1) — a paragraph.
- The calibration vignette (§95) — grows a row per measured guarantee.
- The decision-tree article and cheatsheet (§71).

### 111.5 Explicitly deferred to 0.7.0 and after

Attribution (§28), conformal intervals (§29), epidemic changepoints (§57 —
and see §110.1 on its true size), SPC monitors (§75), panel methods (§41,
downgraded by §108), segment models and `predict()` (§35), genetic search
(§31), screening (§60, upgraded by §107), gradual-change diagnostics (§47.2),
K-uncertainty (§94).

## 112. What the thirteenth pass changes

Nothing was added to the idea inventory, deliberately. What changed:

- **§57 is re-estimated upward** — S4 extraction, a shared-helper refactor,
  and an unresolved contract question about point anomalies (§110.1).
- **§18's extension mechanism gets a job**: prove itself on the three new,
  unstable-API engines before anyone writes a wrapper for them (§110.2).
- **A correction factor is recorded** for every remaining engine theme
  (§110.3).
- **§111 replaces twelve prioritisation sections with one specification** a
  developer could pick up.

The document has been complete since the twelfth pass. It is now also
*actionable*, which is a different property and the one that was missing.
Further passes should add material only when the world supplies it — a new
engine, a new method, a bug report, an archival — and otherwise leave this
alone. **The work now is 0.6.0, not more of this.**

---

# Part III (continued) — fourteenth pass, 2026-08-30

*Two passes have now concluded that the idea inventory is complete, and this
one does not reopen it. Part III currently makes on the order of a hundred
factual claims about this codebase — every "verified:" in it — and the
twelfth pass caught two that were false. A roadmap whose premises are wrong
sends work in the wrong direction, so this pass **checks the premises**
mechanically and records the result.*

## 113. The premise audit

Twenty of Part III's load-bearing claims about the codebase, re-checked
against the source on 2026-08-30. Each is a claim that, if false, would
change what should be built.

| § | Claim | Checked |
|---|---|---|
| §81 | 189 `stop()` calls in `R/` | **189** ✓ |
| §81 | none carries a condition class | **0** `rlang::abort`/`errorCondition` ✓ |
| §69 | six exported `geom_*` | **6** ✓ |
| §69 | exactly one `ggproto` object in the package | **1** (`StatChangepoint`) ✓ |
| §93 | "No changepoints detected." is the whole of the empty answer | present in `ggcpt-class.R` ✓ |
| §94 | `criterion_table` already holds every candidate K | `k`,`value`,`cost`,`chosen` ✓ |
| §30 | nine `change_in` levels, all Gaussian-flavoured | **9** ✓ |
| §30 | no `family` argument anywhere | absent from `cpt_detect()` ✓ |
| §87 | `cpt_test()` cannot take a location | no `when`/`at`/`location` formal ✓ |
| §82 | `cp_convention` hardcoded `"left"` in the constructor | ✓ |
| §51 | `augment()`'s `.resid` is first-coordinate only | documented as such ✓ |
| §59 | no `data/` directory | ✓ |
| §83 | no `CONTRIBUTING` file | ✓ |
| §76 | no engine version recorded on a result | no version field on `ggcpt` ✓ |
| §63 | `as_uni_vector()` ends in `as.numeric(x)` | ✓ |
| §88 | `cpt_metrics()` has no cost weighting | no `cost` formal ✓ |
| §60 | `cpt_batch()` has no multiplicity control | no `control`/`alpha`/`fdr` formal ✓ |
| §71 | `cpt_recommend()`'s noise lists are hand-maintained | `noise_pref <- switch(...)` ✓ |
| §102 | 12 declared `Imports` | **12** ✓ |
| §57 | `regions` is an optional slot, absent by default | `NULL` on a `pelt` fit ✓ |

**Twenty of twenty hold.** Combined with §106's index check, the document's
factual base is now: every claim about *our own code* that has been tested is
true, and the two claims that were false were both about *the outside world*
— `hdbinseg`'s CRAN status and `changepoint.mv`'s availability.

That asymmetry is worth naming, because it says where the next error will be.
Claims about this repository are cheap to verify and have been verified.
Claims about other people's packages were made from notes and papers, and two
of the handful that were checkable turned out to be stale within days.

### 113.1 The rule that follows

**Any claim in this document about an external package's availability,
version or API is provisional until checked against the live index or the
installed package.** §106.1 showed the availability check is one line. The
API claims are harder and §110 showed why: `anomaly`'s S4 return invalidated
three assumptions in a theme written from its paper.

Concretely, before any engine theme in Part III is scheduled, someone should
install the engine and run the contract sweep against a draft wrapper. That
is an afternoon per engine and it is the difference between an estimate and a
guess.

## 114. What is genuinely unverified, and stays that way for now

Honesty about the other side of the ledger. These claims in Part III are
*not* checked and could not be checked from here:

- **Every API claim about an uninstalled engine.** `anomaly`, `changepointGA`,
  `scanCP`, `ChangepointTesting`, `depmixS4`, `qcc` and `hdbinseg` are none of
  them installed on this machine; §110.1's `anomaly` notes come from its
  documentation, not from running it. The S4 finding is solid because the
  documentation states the class; the column names it returns are not.
- **The audience-size arguments.** "Every manufacturing quality department"
  (§75), "the largest adjacent community" (§49) — these are judgements, and
  they are the weakest kind of claim in the document. No user research
  supports any of them. §71's proposal to derive `cpt_recommend()` from
  measurement has an analogue here that this document has never proposed:
  **ask the users**. A short survey, or mining the issue tracker and reverse
  dependencies, would replace a dozen assertions with evidence.
- **The effort estimates.** "~100 lines" for `cpt_test_at()` (§111.2), "a few
  dozen lines each" for CUSUM/EWMA (§75.3). Nobody has written any of it.
  §110.3's correction factor — assume one shared-infrastructure change per
  engine theme — is itself an estimate derived from a single data point.
- **That the 0.6.0 set fits in one release.** §111 lists seven
  freeze-blocking items, seven features, six infrastructure items and four
  documents. That is plausibly two releases, and calling it one is the kind
  of optimism this document should not indulge in given it has just spent a
  section on unverified premises.

## 115. The one new proposal, which is not a feature

Everything above argues for a single addition to the 0.6.0 plan, and it is
methodological rather than functional:

**Ask the users before building the 0.6.0 feature set.**

The package is on CRAN, it has download counts, an issue tracker and reverse
dependencies. Part III's seven-item feature list for 0.6.0 (§111.2) was
derived entirely from reading the source and the literature — which is how
every one of the thirty-plus themes here was derived. Not one line of this
document rests on a user saying what they wanted.

That is defensible for a package finding its shape and indefensible for one
about to freeze its API. Concretely, before 0.6.0:

1. **Mine what exists** — issues, Stack Overflow questions mentioning the
   package or its engines, the CRAN reverse dependencies, and the download
   trajectory of the fifty engines (which methods do people actually reach
   for?).
2. **Ask directly** — a short, linked-from-the-README survey: what do you use
   it for, what did you have to work around, what did you expect to be there.
3. **Then re-order §111.2.** Some of those seven will be confirmed. At least
   one will turn out to matter to nobody, and something absent from all
   thirteen passes will turn out to matter to many.

This is the only proposal in Part III whose *purpose* is to invalidate the
rest of Part III, which is exactly why it belongs in it.

## 116. What the fourteenth pass changes

- **Twenty premises verified** (§113); the document's claims about its own
  codebase are sound.
- **A provisionality rule recorded** for claims about other people's packages
  (§113.1), which is where both known errors came from.
- **An honest unverified list** (§114), including the admission that §111's
  0.6.0 scope is probably two releases.
- **One new proposal** (§115): user research before the feature set is fixed,
  because thirteen passes of reading source and literature produced a plan
  with no user input in it.

No new themes, again, and that continues to be the correct outcome. The
document is complete, checked, actionable and — as of §115 — honest about the
one kind of evidence it entirely lacks.

---

# Part III (continued) — fifteenth pass, 2026-08-30

*§115 proposed asking the users, and observed that nothing in fourteen passes
rested on evidence of what anyone wants. This pass **does the measurable half
of it** — CRAN download counts for every wired engine — and the result
contradicts the emphasis of the entire document. That is the most useful
thing any pass has produced, and it is uncomfortable.*

## 117. What people actually download

Last 30 days, CRAN logs, queried 2026-08-30. Every engine this package wires,
plus the comparators.

### 117.1 The top of the table

| Package | Downloads / month | Wired as |
|---|---:|---|
| **`strucchange`** | **46,164** | `strucchange` |
| **`segmented`** | **33,681** | `segmented` |
| `changepoint` | 7,258 | `pelt`, `binseg`, `segneigh`, `amoc` |
| `trend` | 4,828 | `pettitt`, `buishand`, `snht` |
| `ecp` | 3,939 | `ecp` |
| `cpm` | 1,052 | `cpm` |
| `bfast` | 728 | `bfast` |
| `bcp` | 640 | `bcp` |
| `mcp` | 519 | `mcp` |
| `binsegRcpp` | 478 | `binsegrcpp` |
| `Rbeast` | 443 | `beast` |
| `anomaly` | 424 | **not wired** (§57) |
| *ggchangepoint* | *359* | — |
| *tidychangepoint* | *251* | — |

### 117.2 The long tail

Everything else sits between 111 and 400: `penaltyLearning` 382,
`changepointGA` 308, `SNSeg` 250, `InspectChangepoint` 248,
`changepoint.geo` 248, `CptNonPar` 241, `changepoints` 232, `HDCD` 231,
`DeCAFS` 224, `EnvCpt` 223, `changepoint.influence` 207, `fpop` 206,
`fabisearch` 204, `ChangePointTaylor` 201, `IDetect` 197, `not` 197,
`kcpRS` 191, `wbsts` 186, `fChange` 181, `crossvalidationCP` 178,
`scanCP` 175, `nsp` 167, `KWCChangepoint` 136, `ocd` 111.

Note that a large share of those numbers is CI, mirroring and dependency
resolution rather than human use, so the *floor* is around 150–200 and
anything near it is indistinguishable from noise. The signal is at the top,
and it is stark.

## 118. What this says, and it is not comfortable

### 118.1 The audience is in regression breakpoints, not changepoint detection

`strucchange` and `segmented` together are **~80,000 downloads a month** —
more than ten times `changepoint`, and more than every other wired engine
combined, several times over. Those two are not classical changepoint
packages. They are **structural-break and broken-line regression** packages,
used by econometricians, epidemiologists fitting segmented dose-response, and
anyone asking "did the *relationship* change" rather than "did the *level*
change".

This package wires both, as two of fifty, behind `change_in = "regression"`
and `"slope"`. Part III has thirty-plus themes and **not one of them is about
regression breakpoints.** Fourteen passes of reading the changepoint
literature produced a roadmap aimed almost entirely at the smaller audience.

What that audience would want, none of which is proposed anywhere above:

- **Covariates as first-class.** `cpt_detect(y ~ x1 + x2, data, method =)`
  — a formula interface, which the package does not have at all. `segmented`
  and `strucchange` users think in models, not vectors.
- **Which coefficient broke**, not just when — §28's attribution question, in
  the form this audience asks it.
- **Segment-wise coefficient tables** with intervals — §35's segment models,
  which was proposed as a nice-to-have and is arguably the single most
  wanted thing in the whole document.
- **Bai–Perron's own vocabulary**: `sctest()`, `breakpoints()`, BIC over the
  number of breaks, the `confint()` these users already know.
- **Panel structural breaks**, which is §41 arriving from the econometrics
  side.

§35 (segment models and `predict()`) was sequenced into 0.8.0. On this
evidence it belongs in 0.6.0, and a **formula interface** belongs with it.

### 118.2 `trend` is the fourth-most-used engine and nothing in Part III mentions it

4,828 downloads a month for classical single-change tests — Pettitt,
Buishand, SNHT — the hydrology and climatology vocabulary. That is more than
`ecp`, `cpm`, `bcp` and `mcp` combined. §47 (gradual change) named climate as
an audience almost in passing; the download data says climate and hydrology
are *already here*, using the simplest methods in the package.

The obvious follow-up nobody proposed: those users have specific downstream
needs — Sen's slope, Mann–Kendall alongside the changepoint test, the
homogenisation workflow that SNHT belongs to. Wiring three tests and stopping
was a wrapper task; serving that audience is a vignette and two accessors.

### 118.3 Engine wave #2 was aimed at the smallest audiences

The high-dimensional, functional and network engines that Part II prioritised
as its fifth wave — `HDCD` 231, `changepoints` 232, `fChange` 181,
`KWCChangepoint` 136, `fabisearch` 204, `ocd` 111 — are all at or barely
above the noise floor. They are methodologically important and they are
almost unused.

That is not an argument that wiring them was wrong: being the only R package
that reaches them *is* a contribution, and the marginal cost was one wrapper
each. It is an argument that **the next wave should not be chosen the same
way**. §57 (`anomaly`, 424 — comparable to `mcp` and above nine wired
engines) and §31 (`changepointGA`, 308) are both better-used than most of
what wave #2 added, which strengthens both.

### 118.4 And a note on this package's own standing

359 downloads a month, against `tidychangepoint`'s 251. Comparable, both
small, both dwarfed by the single-purpose packages they wrap. Nobody is using
either as the front door yet. That is worth knowing before optimising the
fiftieth engine: **the constraint is not capability, it is that the audience
does not know the package exists** — which makes §36's teaching material,
§59's bundled data, §71's cheatsheet and §96's failure catalogue not
"documentation nice-to-haves" but the actual growth path.

## 119. The revision this forces on §111

The 0.6.0 specification was written from source-reading. Applying the
evidence:

**Promoted into 0.6.0:**

- **§35 segment models + a formula interface** — the largest audience in the
  data, currently served by two wrappers and nothing else. This is now the
  single highest-value feature in the document.
- **§28 attribution, in its regression form** — "which coefficient broke" for
  `strucchange`/`segmented` results, which is a much smaller job than the
  general multivariate case and serves the bigger audience.
- **§59 bundled data, §96 the failure catalogue, §71 the cheatsheet** — from
  "cheap wins" to "the growth path", per §118.4.

**Demoted:**

- Further high-dimensional, functional and network work. Wave #2's audiences
  are at the noise floor; §33 (spatio-temporal) and §50 (frequency domain)
  should wait for evidence of demand rather than be built on the same
  reasoning that produced wave #2.

**Unchanged:** the freeze-blocking items (§111.1). Those are obligations, not
bets, and download counts do not bear on them.

## 120. What the fifteenth pass changes

- **The first evidence in this document that is not source-reading or
  literature** (§117), and it contradicts the emphasis of fourteen passes.
- **A whole theme that fourteen passes missed** (§118.1): regression
  breakpoints, a formula interface, and coefficient-level attribution for the
  audience that is an order of magnitude larger than the one Part III was
  written for.
- **§111 re-ordered** on evidence rather than on reading (§119).
- **A recorded correction to how engines get chosen** (§118.3): wave #2 was
  selected by methodological interest and landed at the noise floor. The next
  wave should be selected differently, and §57 and §31 now have data behind
  them.

§115 said its purpose was to invalidate the rest of Part III. It partly has,
on its first application, from the *easiest* half of the evidence. The harder
half — actually asking users — is still undone and is now clearly worth more
than another pass over the literature.

---

# Part III (continued) — sixteenth pass, 2026-08-30

*§118.1 found that the largest audience by an order of magnitude wants
regression breakpoints, and that no theme in Part III addressed them. That
deserved more than five bullets. This pass tests what the package actually
does for that audience, and the answer is worse than "nothing proposed" — the
capability is half-built and unreachable.*

## 121. Theme BN — Regression breakpoints as a first-class citizen

### 121.1 Four tests, run against the source

With `y` whose slope on a covariate `x` changes at t = 100:

| | Call | Result |
|---|---|---|
| A | `strucchange_wrapper(y ~ x, data = d)` | **works** — cp = 98, `change_in = "regression"` |
| B | `cpt_detect(y ~ x, data = d, method = "strucchange")` | **fails**: `'language' object cannot be coerced to type 'double'` |
| C | `"data" %in% names(formals(cpt_detect))` | **FALSE** |
| D | `segmented_wrapper(y ~ x, data = d)` | **fails**: `` `x` must be a numeric vector, matrix, or data.frame `` |

So the position is:

- `strucchange_wrapper()` **has** a full formula interface — it takes
  `y ~ x1 + x2` and `data`, fits Bai–Perron breakpoints in the regression,
  and correctly labels the result `change_in = "regression"`. Verified
  working.
- **`cpt_detect()` cannot reach it.** There is no `data` argument, and the
  formula is fed to `as.numeric()`, which produces an error message about
  `'language' objects` that tells the user nothing about what they did wrong.
- **`segmented_wrapper()` cannot do it at all.** It hardcodes
  `stats::lm(.y ~ .t)` — a broken line in *time* — which throws away the
  entire point of `segmented`, whose purpose is estimating breakpoints in the
  relationship between a response and a covariate.

This is the same failure shape as three of 0.5.0's defects (families
unreachable behind the dispatcher, `pilliat`'s guard, `mcp`'s `par_x`): the
capability is present inside the package and the front door does not open on
it. Except here it applies to the two engines with **80,000 downloads a
month between them**, which is more than every other wired engine combined.

### 121.2 The gap, stated properly

The package's mental model is *a series is a numeric vector, and a
changepoint is a moment when its distribution shifts*. The regression
audience's model is *a model is a formula, and a breakpoint is a moment when
its coefficients shift*. Those are different objects, and this package
supports the first thoroughly and the second by accident, in one wrapper,
unreachably.

Everything downstream inherits the limitation. For a regression fit:

- `$segments$param_estimate` holds a mean, when what matters is a **vector
  of coefficients per segment**;
- `augment()`'s `.fitted` is a segment mean, not the model's fitted values;
- `tidy()` reports `cp` and `cp_value`, and `cp_value` is meaningless for a
  regression break;
- `cpt_effect()` (§42) would report a change in level where the change is in
  a slope;
- `cpt_attribute()` (§28) would ask which *coordinate* changed where this
  audience asks which *coefficient* changed.

### 121.3 What to build

**1. A formula method on `cpt_detect()`.** The signature the audience
expects, and the one `strucchange_wrapper()` already implements:

```r
cpt_detect(y ~ x1 + x2, data = d, method = "strucchange")
cpt_detect(y ~ x1 + x2, data = d, method = "segmented", npsi = 2)
cpt_detect(y ~ 1, data = d, method = "pelt")     # the intercept-only case
```

`cpt_detect.formula()` dispatching on the first argument, with `data`.
Methods whose engine has no regression form refuse by name — the registry
already has the mechanism, and a `formula` capability column is one more
flag alongside `multivariate` and `ci`.

**2. Fix `segmented_wrapper()` to take covariates.** It currently cannot
express its own engine's core use case. `seg.Z` should be settable, `npsi`
per variable, and the time-only path stays as the default so nothing breaks.

**3. Coefficients per segment, in the object.** A `coefficients` slot, or
`$segments` gaining a list-column of coefficient tibbles — the additive
optional-slot pattern that `regions` and `diagnostics` already established.
Then `tidy(fit, "coefficients")` gives one row per (segment, term) with
estimate, se and interval, which is §35's segment-model layer arriving for
the audience that most wants it.

**4. Coefficient-level attribution.** §28 asks which coordinate changed; the
regression form asks **which coefficient changed**, and it is easier — a
Chow-type test per coefficient at a known break, with the multiplicity
correction across coefficients. For a *pre-specified* break this needs no
selection adjustment at all, which connects it to §87.

**5. The vocabulary this audience already has.** `sctest()`-style structural
change tests, BIC over the number of breaks (which `strucchange::breakpoints`
computes and we discard), and `confint()` on breakpoints — which
`strucchange_wrapper()` already extracts. Much of this is exposure of what
the engine returns and we drop.

### 121.4 Why this outranks almost everything else in Part III

- It serves an audience **ten times larger** than the one the rest of the
  document targets (§117).
- Roughly half of it is **exposure, not invention**: the formula interface
  exists in one wrapper, the breakpoint confidence intervals are already
  extracted, and the coefficient tables are sitting in the engine's return
  value.
- It repairs a genuine defect — B and D above are bugs, not gaps. A user
  who reads `?strucchange_wrapper`, sees the formula interface, and then
  tries it through `cpt_detect()` gets an error message about `'language'
  objects`.
- It gives §35, §28, §42 and §87 a concrete, high-demand instance to be
  designed against, rather than being designed in the abstract.

**Recommendation: items 1 and 2 are bug fixes and belong in 0.6.0. Item 3 is
§35 and should move there with them. Items 4 and 5 are 0.7.0.**

## 122. What the sixteenth pass changes

- **Four tests turn §118.1 from an observation into a defect report.** The
  formula interface exists and the dispatcher cannot reach it (B, C);
  `segmented` cannot take a covariate at all (D).
- **Theme BN is now specified**, and on the evidence of §117 it is the
  highest-value theme in Part III.
- **§35 is promoted again** — it was moved into 0.6.0 by §119 on download
  evidence, and §121.3 item 3 shows it is also the natural home for the
  coefficient tables the biggest audience needs.
- **A pattern is confirmed for the third time**: this package's recurring
  defect is not wrong statistics but *capability present and unreachable* —
  `change_in` values lost to a tribble (S1), non-Gaussian costs behind the
  dispatcher (§30), `mcp`'s `par_x` (S34), and now the formula interface.
  A systematic sweep asking "what can each engine do that `cpt_detect()`
  cannot ask it for?" would likely find more, and is worth doing once
  properly rather than discovering one instance per release.

That last item is the only genuinely new *method* this pass produced, and it
is a method for finding work rather than a feature: **audit the gap between
each engine's capability and the dispatcher's vocabulary.** Sixteen passes
found four instances of it by accident. One deliberate sweep would find the
rest.

---

# Part III (continued) — seventeenth pass, 2026-08-30

*§122 proposed one deliberate sweep — "what can each engine do that
`cpt_detect()` cannot ask it for?" — on the grounds that sixteen passes had
found four instances of that defect by accident. This pass ran it across all
49 installed engines. It found more instances, and it **corrected one of the
document's headline claims**.*

## 123. The capability-gap sweep

Method: for each wired engine, extract the arguments of the engine functions
its wrapper actually calls, and classify them against what the wrapper
exposes and what `cpt_detect()` has vocabulary for.

### 123.1 Engine arguments that are outright unreachable

Only **4 of 49** wrappers omit `...`, and three of those block real
arguments:

| Method | Engine | Blocked |
|---|---|---|
| `taylor` | `ChangePointTaylor` | `labels`, `min_tbl_conf`, `CI` |
| `hdcov` | `changepoints` | `level`, plus recursion internals (`s`, `e`, `BS_object`, `tau`) |
| `network` | `changepoints` | `Alpha`, `Beta`, `level`, plus internals |

`taylor`'s `labels` is the one worth having — it names the changepoints in
the engine's own table, which is exactly what the quality-control audience
wants in a report. `level` on the two `changepoints` engines is a
significance level a user might reasonably vary. The recursion internals are
correctly hidden.

**This is a small problem**, and that is itself a finding: the `...`
convention has done its job almost everywhere.

### 123.2 The real gap: modelling choices with no vocabulary

Fifteen engines take an argument that expresses a **modelling choice** —
which family, which cost, which kernel, which test statistic — and in
thirteen of the fifteen it is reachable *only* as an anonymous `...`
argument, under the engine's own name, with no validation and no way to
discover it:

| Choice argument | Engines |
|---|---|
| `type`, `model.selection` | `wbs2`, `tguh` (`breakfast`) |
| `family` | `smuce`, `hsmuce` (`stepR`) — **exposed as a formal** |
| `models` | `envcpt` — **exposed as a formal** |
| `cpmType` | `cpm` |
| `probModel` | `bocpd` |
| `boundaryType` | `bcp` |
| `precPriorType` | `beast` |
| `kernel.f` | `npmojo` |
| `test.stat` | `geomcp` |
| `model` | `segmented` |
| `thresh.type` | `nsp` |
| `algtype`, `testtype` | `fabisearch` |
| `type` | `bfast` |

A user cannot learn any of these from `cpt_methods()`, cannot discover them
from `?cpt_detect`, and gets no error if they misspell one — `...` swallows
it silently. That last point is the sharpest: **a typo in a modelling choice
is currently a silent revert to the default**, which is the package's
signature failure mode arriving in a thirteenth place.

### 123.3 The correction to §30

Tested directly:

```r
cpt_detect(y, method = "pelt", change_in = "meanvar", test.stat = "Poisson")
#> works: cp = 80 on Poisson data with a rate change at 80
```

**§30 said the non-Gaussian costs were unreachable. They are not** — they go
through `...` today. §30 has been corrected in place.

The theme survives and its framing changes: not "add the capability" but
"name what is already there". A `family` argument that validates against a
registry column, appears in `cpt_methods()`, and errors on a typo is now the
whole of the work for `changepoint`'s families — much cheaper than §30
assumed, and more valuable, because it also fixes the silent-typo problem for
the other twelve.

`binsegRcpp` is the exception and remains genuinely unreachable: its wrapper
maps `change_in` to a fixed distribution, so `poisson`, `laplace` and `l1`
cannot be selected at all.

### 123.4 The narrowing case

`smuce_wrapper()` exposes `family` as a formal, and restricts it to
`c("gauss", "hsmuce")`. `stepR` supports more. So this is the inverse defect:
the wrapper does not fail to expose the choice, it **narrows** it — and
because it uses `match.arg()`, the refusal is clear rather than silent, which
is the right behaviour for a deliberate restriction and the wrong behaviour
if the restriction was accidental. Nothing records which it was.

Worth checking for the same pattern elsewhere: any wrapper whose `match.arg()`
set is smaller than the engine's.

## 124. What this implies for the design

The sweep changes the shape of the fix. §30 proposed a `family` argument for
count data. The sweep says the real thing to build is one level up:

**A declared vocabulary for engine-specific modelling choices**, in the
registry, alongside `supports` and the capability flags:

```r
cpt_methods()$choices
#> pelt      : test.stat = Normal | Poisson | Gamma | Exponential | CUSUM
#> cpm       : cpmType   = Mann-Whitney | Mood | Lepage | Kolmogorov-Smirnov | ...
#> npmojo    : kernel.f  = ...
#> bcp       : boundaryType = node | edge
```

with `cpt_detect()` validating against it and erroring on an unknown value
instead of letting `...` swallow it. That single mechanism:

- gives §30's families a home, and the same for twelve other engines;
- makes every modelling choice **discoverable** from `cpt_methods()`, which
  is the introspection function's whole purpose;
- turns thirteen silent-typo paths into thirteen clear errors;
- and gives `cpt_recommend()` something new to reason about — "this method
  can do a Poisson cost, that one cannot".

It is a registry column, a validation step and a documentation generator, and
it is worth more than any single engine in the deferred list.

## 125. What the seventeenth pass changes

- **§122's proposed sweep was run** across 49 engines, which is the first
  time this document proposed a method and then executed it in the next pass.
- **§30 corrected** — the non-Gaussian costs are reachable today; the gap is
  vocabulary, not capability, and the fix is cheaper and broader than
  written.
- **Twelve more instances of the same defect found**, plus 3 wrappers
  blocking real arguments and 1 narrowing an engine's own choice set.
- **A better fix identified than any individual theme proposed** (§124): a
  declared `choices` vocabulary in the registry, which subsumes §30 and
  serves twelve other engines at once.
- **The `...` convention is vindicated** as a forwarding mechanism (only 4 of
  49 wrappers omit it) and **indicted** as a user interface: anonymous,
  undiscoverable, unvalidated, and silent on a typo.

Two passes running now have found real defects by measuring rather than
reading — download counts, then this. That is the pattern worth continuing,
and it argues that the remaining value in this document lies in *auditing
what exists* rather than in proposing what does not.

---

# Part III (continued) — eighteenth pass, 2026-08-30

*§125 said the remaining value is in auditing what exists. §123 audited what
engines can be **told**. This pass ran the two follow-ups it named: the
`match.arg()` narrowing check, and the mirror audit of what engines **return**
that the package throws away. The narrowing check found the largest single
instance of the "capability present and unreachable" pattern in eighteen
passes, and it is one line of code.*

## 126. The narrowing audit: `fastcpd`

### 126.1 What was found

`fastcpd_wrapper()` restricts `family` with `match.arg()` to:

```
mean | variance | meanvariance | ar | arma | garch
```

`fastcpd` itself documents at least:

```
mean, variance, meanvariance, lm, binomial, poisson, lasso,
ar, arma, arima, garch, var, glm, mvtnorm, custom
```

**Six of roughly fifteen.** Verified by running the engine directly:

```r
fastcpd::fastcpd.poisson(cbind(y, 1))   #> cp = 100  (true change at 100)
fastcpd_wrapper(y, family = "poisson")  #> 'arg' should be one of "mean",
                                        #>  "variance", "meanvariance", "ar",
                                        #>  "arma", "garch"
```

The engine finds the rate change exactly. Our wrapper refuses to ask it.

### 126.2 What is behind that `match.arg()`

The nine blocked families are not marginal. They are, between them, several
of the largest themes in Part III:

| Blocked family | The theme it would serve |
|---|---|
| `poisson`, `binomial` | **§30** — the entire non-Gaussian families theme |
| `lm`, `glm` | **§121 / Theme BN** — regression breakpoints, the largest audience (§117) |
| `lasso` | high-dimensional regression, §17's `hdreg` territory |
| `custom` | **§31's** central argument: a cost that does not decompose, user-supplied |
| `arima`, `var` | §31's ARIMA-per-segment case, and time-series structure |
| `mvtnorm` | multivariate Gaussian |

So a single already-wired, already-installed, already-`Suggests`ed engine
would deliver most of §30, a substantial part of Theme BN, and the motivating
case for §31 — and the only thing preventing it is a six-element character
vector in one wrapper's signature.

That is the most cost-effective item in this entire document. It is smaller
than any engine wrapper, smaller than most bug fixes, and it moves three
themes.

### 126.3 The caveat, stated honestly

Widening the set is not free of design work, and pretending otherwise would
repeat §110.3's lesson:

- `family = "lm"` and `"glm"` need **covariates**, so they need Theme BN's
  formula interface to be usable at all — they cannot simply be added to the
  `match.arg()` list and left to a numeric-vector API.
- `family = "custom"` needs a **cost function** argument, which is a new
  concept for `cpt_detect()` and overlaps `cpt_register_method()` in an
  interesting way: it is a user-supplied *cost* inside a package engine,
  rather than a user-supplied *detector*.
- Each family changes what `$segments$param_estimate` means, which is §121.2's
  problem arriving again.
- The registry's `supports` and the proposed `choices` column (§124) both
  need to know about them.

**Realistic sequencing:** `poisson`, `binomial`, `variance`, `mvtnorm` are
drop-ins and belong in 0.6.0 with §124's `choices` mechanism. `lm`, `glm`,
`lasso` follow the formula interface. `custom` is its own small design.

### 126.4 The other narrowing candidates

Twenty-five `match.arg()` choice sets exist across the wrappers. Two more are
known to narrow:

- `smuce`/`hsmuce` restrict `stepR`'s `family` to `gauss | hsmuce` (§123.4);
- `binsegrcpp` restricts `change_in` to `mean | meanvar`, hiding `poisson`,
  `laplace` and `l1` (§123.3).

The remaining twenty-two look like faithful exposures of their engines'
options rather than restrictions, but **that has not been verified
individually**, and §126.1 is the demonstration that the assumption is unsafe.
A one-time check of every `match.arg()` set against its engine's documented
values belongs with §124's work, since both end in the same registry column.

## 127. What this says about how the package was built

Three of the largest gaps in Part III — non-Gaussian families (§30),
regression breakpoints (§121), user-supplied costs (§31) — turn out to be
substantially reachable through engines already wired, and blocked by three
`match.arg()` lists and one missing `data` argument.

That is not a criticism of the original wrapping work: restricting to the
families you have tested is the *right* default when wiring fifty engines
under time pressure, and `match.arg()` failing loudly is far better than
`...` swallowing a typo (§123.2). The wrappers were written conservatively
and that was correct.

What it means is that **the package is substantially more capable than its
own interface admits**, and the cheapest work available is not adding
engines but *unlocking the ones already there*. Eighteen passes of reading
the literature produced thirty-odd themes; two passes of auditing the source
found that three of the biggest are mostly already paid for.

The generalisation, which is the useful part:

> For a wrapper package, the audit that pays is not "what methods exist in
> the literature" but "what can the engines I already depend on do that my
> interface will not let anyone ask for". This document spent fifteen passes
> on the first question and two on the second, and the second was more
> productive.

## 128. What the eighteenth pass changes

- **The single most cost-effective item in Part III identified** (§126):
  widening `fastcpd_wrapper()`'s family set, which moves §30, Theme BN and
  §31 for the cost of a character vector plus the design work in §126.3.
- **§123.4's narrowing check executed**, finding `fastcpd` (6 of ~15) as well
  as the already-known `smuce` and `binsegrcpp`.
- **Twenty-two `match.arg()` sets flagged as unverified** against their
  engines' documented options, to be checked once with §124.
- **A methodological conclusion** (§127): for a wrapper package the
  productive audit is inward, not outward — and this document's own history
  is the evidence.

Nothing new was proposed. Everything here is a way to get more out of what
0.5.0 already ships, which is what §125 predicted the remaining value would
look like.

---

# Part III (continued) — nineteenth pass, 2026-08-30

*The mirror of §123 and §126. Those asked what the engines can be **told**;
this asks what they **return** that the package throws away. It ran across 37
engines. It found real material, and — worth recording — it also produced two
false positives, which says something about how these audits should be read.*

## 129. The discarded-output audit

Method: run each engine through `cpt_detect()`, list the fields on the raw
`$fit`, and compare against everything surfaced on the `ggcpt` — the
changepoints tibble, the segments tibble, `$data`, and the optional slots.

### 129.1 Two false positives, recorded first

The crude name-matching produced two findings that did not survive checking,
and both are instructive:

- **`pettitt`'s p-value is *not* discarded.** The audit flagged it because
  the engine calls it `p.value` and the package surfaces it as `p_value` in
  the changepoints tibble. Verified present, along with `statistic`. The
  audit was matching names, not meanings.
- **`esac`'s `coordinate` is *not* the attribution answer.** It looked like
  the engine natively reporting which coordinates changed — which would have
  been a significant find for §28. Checked on data where only 3 of 6
  coordinates shift: `coordinate` comes back `1,1,1,1,1,1`, length p, and
  does not discriminate the three that moved from the three that did not.
  Whatever it is, it is not "which coordinate changed", and §28's plan stands
  unchanged.

**Any audit of this kind reports on names; only checking reports on
meanings.** §113.1 said that about external packages; it applies to
mechanical audits of our own too.

### 129.2 What is genuinely discarded, and verified

**`strucchange` — the whole model-selection table.** Verified: `$fit$RSS.table`
holds the RSS for every candidate break at every number of breaks, which is
the object you use to choose *how many* breaks there are. The package keeps
the chosen breakpoints and throws the table away. §121.4 predicted this
without checking; it is real.

```
   break1     RSS1 break2 RSS2 ...
45     45 35.09329     NA   NA
46     46 36.43751     NA   NA
```

**`binseg` — the full solution path.** Verified: `fit@cpts.full` is a 5×5
matrix of the nested segmentations and `fit@pen.value.full` the penalty at
each rung. The package reports two changepoints and sets `$diagnostics` to
`NULL`. This is precisely the object `cpt_solution_path()` (§14, shipped in
0.5.0) exists to draw, already computed by the engine, and not connected.
The same holds for `pelt`, `amoc` and `np`, which all discard `pen.value`,
`test.stat` and `cpttype`.

**`fastcpd` — `thetas`.** The per-segment parameter estimates, computed by
the engine and dropped. That is §35's segment-coefficient table, already
paid for, for the engine §126 just showed can also fit `lm`, `glm` and
`poisson`.

**`fpop` — `path` and `cost`.** Again the solution path, again discarded.

**`bcp` — `posterior.mean`, `posterior.var`, `blocks`.** `ggcpt_posterior()`
uses `posterior.prob`; the posterior mean and variance per observation are
right there and would make the Bayesian display considerably richer.

**`envcpt` — the model-comparison table.** `mean`, `meancpt`, `meanar1`,
`meanar2`, `trend`, `trendcpt` … with AIC/BIC for each. The engine's entire
purpose is comparing twelve models, and the package reports the winner and
discards the comparison.

**`mosum`, `npmojo` — `stat`, `rollsums`, `threshold.val`.** Partly surfaced
through `cpt_statistic()` and `cpt_scale_space()`; whether the rest matters
needs a per-engine check rather than an assertion.

### 129.3 The pattern

Three of these — `binseg`'s `cpts.full`, `fpop`'s `path`, `strucchange`'s
`RSS.table` — are **solution paths that 0.5.0 built a display for and never
connected**. `cpt_solution_path()` and `ggcpt_solution_path()` ship, are
documented, and are declared in the registry's `path` capability column for
some engines. The audit says at least three more engines could populate them
and do not.

That is a different class from §126's narrowing. Nothing is restricted here;
the wrapper simply extracts the changepoints and lets the rest go. It is the
cheapest kind of gap to close — an extra `extra_cp_cols` or `diagnostics`
argument in a wrapper that already has the object in hand.

## 130. What to do about it

1. **Check the `path` capability column against reality.** The registry
   declares which methods expose a solution path. The audit suggests
   `binseg`, `fpop` and `strucchange` could and are not marked. One check,
   and it is the same shape as §82's convention test: assert the registry's
   claim against what the engine actually returns, for every capability
   flag, not just `path`.
2. **Connect `strucchange`'s `RSS.table` to `cpt_select()`.** The package has
   six criteria for choosing K and computes them itself; for this engine the
   information to choose is already returned and better-founded. This lands
   directly in Theme BN's audience.
3. **Surface `fastcpd`'s `thetas`** as the first instance of §35's
   segment-coefficient table — a concrete, cheap prototype for a theme that
   is otherwise a design.
4. **Enrich `ggcpt_posterior()` with `bcp`'s posterior mean and variance.**
5. **Expose `envcpt`'s model table**, since comparing models *is* what that
   engine does.

None of these is a new feature. All five are wiring an object the engine
already returned into a display or accessor the package already ships.

## 131. What the nineteenth pass changes

- **The output audit run** across 37 engines, completing the pair with §123
  and §126.
- **Two false positives found and recorded** (§129.1), with the lesson that a
  name-matching audit reports on names, not meanings — and that §28's plan is
  unaffected, which was worth establishing.
- **Six engines confirmed to discard material the package has displays for**,
  three of them solution paths that 0.5.0 built the machinery to draw.
- **A sixth capability-flag check proposed** (§130.1), generalising §82's
  convention test to every column in the registry: *assert the registry's
  claims against what the engines actually do.*

Three consecutive passes have now produced findings by auditing rather than
proposing, and each found something the previous fifteen passes of reading
had not. The audits are close to exhausted too — inputs (§123), narrowing
(§126) and outputs (§129) is most of the surface — and what remains after
that is §130.1's registry-versus-reality check and then building 0.6.0.

---

# Part III (continued) — twentieth pass, 2026-08-30

*§130.1 proposed the last audit: assert the registry's capability claims
against what the engines actually do, for every column rather than just
`path`. It was run across nine capability columns and 37 engines. It found
**three real defects and thirty-three false positives**, and the ratio is
itself the most useful thing in the result.*

## 132. The registry-versus-reality audit

### 132.1 Three claims that are false, verified

**`bocpd` claims `posterior = TRUE`. `ggcpt_posterior()` refuses it.**

```r
subset(cpt_methods(), posterior)$method
#> bcp, bocpd, beast, mcp

ggcpt_posterior(cpt_detect(y, method = "bocpd"))
#> Error: No posterior probability profile found on this object.
#>        ggcpt_posterior() supports results from bcp_wrapper() and
#>        beast_wrapper().
```

The error message names the two engines it supports, and the capability table
names four. `posterior_prob_profile()` branches on `inherits(fit, "bcp")` and
`inherits(fit, "beast")` and has no branch for `ocp` — even though the fit
carries the full run-length matrix `$R`, which `ggcpt_runlength()` uses
successfully on the same object. **The posterior is present; the accessor
does not know how to read it.** A `ocp` branch marginalising `$R` over run
lengths would make the claim true; failing that, the registry flag is wrong.

**`binsegrcpp` and `wbsts` claim `path = TRUE`. `cpt_solution_path()` refuses
them by name.**

```r
cpt_solution_path(cpt_detect(y, method = "binsegrcpp"))
#> Error: Engine `binsegrcpp` does not expose a solution path. These do: ...
```

So the registry's `path` column and `cpt_solution_path()`'s own list of
supporting engines **disagree with each other**, and a user who filters
`subset(cpt_methods(), path)` gets two methods that then error. Note the
irony against §129.2: `binsegRcpp` is one of the engines whose full nested
path the sweep found being *discarded*. The capability is claimed, the object
exists, and the accessor was never wired.

### 132.2 Thirty-three false positives, and why they matter

Every method not already flagged came back as "claims `scale_space = FALSE`,
but `cpt_scale_space()` works". That is a defect in the test, not the
registry:

```r
formals(cpt_scale_space)$method   #> c("mosum", "npmojo")
cpt_scale_space(pelt_fit)         #> 2000 rows — of a *mosum* scale space
```

`cpt_scale_space()` takes a series or a fit and computes a scale space using
`mosum` or `npmojo`, **regardless of which method produced the fit**. So it
"works" on any input, and my test read that as thirty-three registry errors.

That is documented behaviour and not a bug. It is, however, worth one line in
the docs: a user passing a `pelt` fit to `cpt_scale_space()` may reasonably
believe they are seeing the scale space *of their detector*, and they are
seeing a mosum scale space of their data. The distinction matters for
interpretation and nothing states it.

### 132.3 The ratio is the finding

Three real, thirty-three spurious. Across the four audits in this and the
previous two passes:

| Audit | Real findings | False positives |
|---|---|---|
| §123 input gaps | 3 blocked + 13 vocabulary | 0 |
| §126 narrowing | 3 confirmed | 0 (22 unchecked) |
| §129 discarded output | 6 | **2** |
| §132 registry claims | **3** | **33** |

The false-positive rate rose sharply as the audits got more ambitious, and
for the same reason each time: **the test encoded my assumption about what a
function does, and the function did something else.** `pettitt`'s `p.value`
became `p_value`; `esac`'s `coordinate` was not attribution;
`cpt_scale_space()` does not use the fit's method.

Which is the same lesson the package learned internally at S12 and S23 and
paid for in 0.5.0 — *the code ran and the answer was wrong* — arriving now in
the audit tooling rather than the package. An audit is software, and
unverified audit output is not evidence. **Every finding in §123, §126, §129
and §132 that is quoted in this document has been re-checked by hand; the
ones that were not survived only as counts.**

## 133. What to fix

1. **`bocpd`'s posterior** — either add an `ocp` branch to
   `posterior_prob_profile()` (marginalise `$R`, which is present) or set the
   registry flag to `FALSE`. The first is better and small.
2. **`binsegrcpp` and `wbsts`'s `path`** — either wire the paths §129.2 found
   being discarded, or correct the flags. Again the first is better, and
   §129's audit says the object is already in hand for `binsegrcpp`.
3. **Make this audit a test.** All three defects are of the form *the
   registry asserts a capability and the accessor cannot deliver it*, and
   they are mechanically detectable. A test that, for every method and every
   capability column, calls the corresponding accessor and asserts agreement
   would have caught all three and would catch the next one. It belongs with
   §82's convention test and §72's property tests, in 0.6.0, before the
   freeze makes the registry's columns part of the public contract.
4. **Document `cpt_scale_space()`'s independence from the fit's method** —
   one sentence.

Item 3 is the durable one. The registry is the package's single source of
truth (0.5.0's central architectural claim), and nothing currently checks
that what it claims is true. Three of nine columns had a false entry on first
inspection.

## 134. What the twentieth pass changes

- **The last audit §130.1 named has been run**, completing the set: inputs
  (§123), narrowing (§126), outputs (§129), claims (§132).
- **Three verified registry defects** — `bocpd`'s posterior, and `path` for
  `binsegrcpp` and `wbsts` — each a case of the registry promising something
  the accessor cannot deliver.
- **A test proposed that would catch the whole class** (§133.3), which is
  more valuable than the three fixes and belongs before the freeze.
- **The audits' own false-positive rate quantified and explained** (§132.3),
  with the standard applied: every finding quoted in this document has been
  re-checked by hand.

The audit surface is now exhausted. What this document can produce by
inspection has been produced. What remains is 0.6.0 — and §115's user
research, which is the one form of evidence no audit can substitute for.

---

# Part III (continued) — twenty-first pass, 2026-08-30

*§0.9 lists a `covr` badge as deferred infrastructure — "needs a coverage
service configured for the repository". This pass tried to take the
measurement and could not, because **the suite does not run under `covr`**.
That turns out to be a one-line problem with a general lesson, and it
re-scopes the §0.9 item from configuration to a code fix.*

## 135. The coverage number, and why there isn't one

### 135.1 What happened

`covr::package_coverage()` instruments every function in the package — it
rewrites each body to insert execution counters — and then runs the suite
against the rewritten copy. On this package it aborts.

Isolating file by file under instrumentation: **15 of 17 test files pass.**
Two do not, and only one of those is real.

### 135.2 The genuine failure: a test that reads the source

`test-040-polish.R`, R64:

```r
body_txt <- paste(deparse(body(ggcpt_compare)), collapse = " ")
expect_match(body_txt, "future.seed = seed %||% TRUE", fixed = TRUE)
```

The test **deparses the function's own source and greps it for a literal
string**. Under instrumentation the body has counters woven through it, the
literal is no longer present as written, and the assertion fails.

The test's reasoning is sound and is recorded in its own comment: it pins a
0.4.0 bug fix (`ggcpt_compare()` passing `seed = NULL` to `future.seed`,
which is outside that argument's documented contract) *without needing to
spin up a worker*. Reading the source was the cheap way to assert the fix
survived. It works, it is fast, and it makes the package non-instrumentable.

The fix is to assert the **behaviour** rather than the text — capture what
`future.apply::future_lapply()` actually receives, via a local mock or a
one-worker plan, and check the value. That is slower and correct. Two other
places in the suite use the same `deparse(body())` idiom and should be
checked with it.

### 135.3 The other one was my harness

`test-040-bugfixes.R`, R19, errored — and not because of instrumentation.
The test calls `ggcpt_build()`, which is **internal and not exported**. My
scan ran each file with `library(ggchangepoint)` attached rather than through
`test_check()`, so internals were invisible and the call failed with "could
not find function". Under the normal suite R19 passes; it is in every green
run this session, including the 2,590-assertion one.

**Fourth false positive in four audits.** §129 had two, §132 had
thirty-three, and this one. Each time the harness encoded an assumption about
the environment that did not hold. Recorded here for the same reason as the
others: the count of findings from a mechanical audit is not the number of
defects, and the difference has been large every time.

### 135.4 A prediction that was also wrong

Before isolating, I expected the culprit to be in the 0.5.0 test files —
specifically `expect_identical(with_session_registry(f), f)` in
`test-050-tools.R`, which compares two function objects and which
instrumentation would plausibly break. It passes. Both failures were in the
0.4.0-era files.

Worth recording because it is the third time in this document that a
confident structural prediction about this codebase has been wrong (§106.2's
`hdbinseg`, §129.1's `esac`, this). The pattern is consistent: **predictions
about behaviour are unreliable at a rate that makes checking mandatory, even
when the reasoning seems solid.**

## 136. What this re-scopes

**§0.9's covr item is not "configure a service".** It is:

1. rewrite R64 to assert behaviour rather than source text;
2. audit the suite for other `deparse(body())` assertions and do the same;
3. *then* take the measurement, and only then think about a badge.

Step 1 is small. Step 3 is the valuable one and has never been done: nobody
knows what fraction of this package's roughly 5,000 lines has ever been
executed by a test. Every defect the 0.5.0 audit found had the shape *the
code ran and the answer was wrong* — code that has never run at all is
strictly worse, and its extent is currently unknown.

Two setup facts for whoever does it: `covr` is not present in this
development environment and needs `rex`, `xml2` and `httr` installed first;
and `covr` discards its temporary library when a run fails, so the failure
log named in its error message is gone by the time you look for it. Write
diagnostics to a path you control.

## 137. What the twenty-first pass changes

- **One real defect found**: R64 makes the package non-instrumentable, by
  asserting on deparsed source. A test that is correct, deliberate, and has
  a side effect nobody intended.
- **§0.9's covr item re-scoped** from infrastructure configuration to a code
  fix followed by a measurement (§136).
- **The coverage number is still unmeasured**, and is now one small test
  rewrite away. That is the single most informative number nobody has about
  this package.
- **A fourth audit false positive recorded** (§135.3) and a third wrong
  structural prediction (§135.4), both of which reinforce §132.3's rule that
  audit output is not evidence until re-checked.

This is the last of the inspection-based work. Inputs, narrowing, outputs,
registry claims and now instrumentability have all been examined; what
remains is to fix R64, take the coverage measurement, and build 0.6.0.

---

# Part III (continued) — twenty-second pass, 2026-08-30

*§136 said the coverage number was one test rewrite away and was the single
most informative figure nobody had. It can be obtained without rewriting
anything, by excluding the one non-instrumentable file from the run. **It is
85.79%**, and the interesting part is the 14% underneath it.*

## 138. Coverage, measured

`covr::package_coverage()`, instrumented, `test-040-polish.R` excluded (§135.2),
run in an environment parented on the package namespace so internals resolve:

```
TOTAL: 85.79%    49 files    4,388 expressions    647 never executed
files at 100%: 2        files below 70%: 3
```

**85.79% is a respectable number** for a package of this size and it should be
said plainly before the criticism starts: the 0.5.0 audit's ~620 new
expectations did their job. The figure is also a slight *under*-estimate,
since the excluded file is one of the two largest test files and its
assertions do exercise real code.

### 138.1 The three files below 70%

| File | Coverage | What it is |
|---|---|---|
| `R/wrap-mcp.R` | **10.4%** | the `mcp` wrapper |
| `R/geoms.R` | **40.5%** | `geom_changepoint()`, `geom_cpt_segment()`, `geom_cpt_ci()`, `stat_changepoint()` |
| `R/wrap-functional.R` | **61.6%** | `fmean`, `fcov`, `kwc`, `fabisearch` |

`wrap-mcp.R` at 10.4% is expected and already understood — S34's whole lesson
was that `mcp` needs JAGS and cannot run here. It confirms rather than adds.

**`R/geoms.R` at 40.5% is the finding.** These are the package's four
original composable layers, the ones its pitch is built on. §69 argued they
should become real ggproto Geoms; coverage says they are also **the least
tested user-facing code in the package**. Doing §69's refactor on 40%-covered
code, even behind the vdiffr snapshots, is riskier than §69.2 implied. The
sequencing should be: raise the coverage first, then refactor.

### 138.2 Twelve functions have never executed at all

| Function | Expressions | Why it matters |
|---|---|---|
| `cpt_load_tcpd` | 42 | the largest untested function in the package — the TCPD loader, deliberately never run (network), verified offline once by hand during the 0.5.0 audit |
| `confint_nsp` | 19 | **a `cpt_confint()` provenance that no test has ever run** |
| `print.summary.ggcpt` | 15 | a print method, never called |
| `default_penalty_grid` | 12 | S14's adaptive grid — the fix was verified by hand, never pinned by a test |
| `segmented_jump_test` | 11 | a `cpt_test()` branch, never run |
| `ggcpt_compare_table` | 7 | an **exported** function |
| `tcpd_download` | 6 | network, as above |
| `geom_changepoint`, `geom_cpt_ci`, `geom_cpt_segment` | 1 each | the wrapper bodies themselves |

Two of these deserve emphasis.

**`confint_nsp` (19 expressions, 0%).** `cpt_confint()`'s four provenances
were the headline inference feature of 0.5.0, and one of the four has never
been executed by a test. §29 proposes adding a fifth (conformal) and §29.3
proposes measuring realised coverage across all of them — that measurement
would have caught this, and the ordering should be: test what exists before
adding to it.

**`ggcpt_compare_table` (7 expressions, 0%).** An exported function with zero
coverage. §S22's audit found fifteen exports missing from the README and
fixed the documentation; nothing checked whether exports are *executed*. The
"exports never called in tests" sweep from that pass matched on the string
appearing anywhere in `tests/`, which is a weaker condition than being run.

### 138.3 And the partially-covered ones worth naming

`extract_statistic` at 52% (44 expressions) is the function behind
`cpt_statistic()`, whose per-engine branches were the site of defect S12 —
the npmojo scale-space heatmap where every value was `NA`. Half its branches
are still unexercised, which is precisely where S12 lived.

`normalise_regions` at 57% handles the `regions` slot that NSP introduced and
that §33, §57 and §63 all propose extending. `autoplot.ggcpt_monitor` at 50%
is the streaming display.

## 139. What this changes

1. **§111.1's test work gains three specific targets** rather than a general
   aspiration: `R/geoms.R` (40.5%, and §69 wants to refactor it),
   `confint_nsp` (0%, an inference provenance), and `extract_statistic`
   (52%, and S12's old neighbourhood).
2. **§69 is re-sequenced.** Turning the geoms into ggproto objects should
   follow raising their coverage, not precede it. The vdiffr snapshots check
   *appearance*; they do not check the argument handling that 40.5% says is
   unexercised.
3. **§29.3's coverage study is promoted.** It would have found
   `confint_nsp`'s zero coverage as a side effect, and the principle —
   measure what the package asserts before asserting more — applies here
   exactly.
4. **A cheap, mechanical check for 0.6.0**: assert that every *exported*
   function is executed at least once by the suite. `ggcpt_compare_table`
   fails it today. That is stricter than §S22's name-matching sweep and
   catches a different thing.
5. **§0.9's covr item, re-scoped again**: fix R64, add the exports-executed
   check, set a floor (85% is the honest current number, so a floor of 85
   is a real ratchet), then a badge.

## 140. What the twenty-second pass changes

- **The coverage number exists**: 85.79%, 4,388 expressions, 647 never
  executed, and a per-file and per-function breakdown that did not exist
  before this pass.
- **Twelve never-executed functions identified**, two of them significant:
  an inference provenance and an exported function.
- **§69 re-sequenced** on the evidence that the code it wants to refactor is
  the least-tested user-facing code in the package.
- **Four attempts were needed** to obtain the figure, each defeated by a
  different tooling detail (temp cleanup, output suppression, a wrong path,
  invisible internals). That cost is the real content of §0.9's covr item and
  it is now written down.

The measurement §136 asked for has been taken. With it, the inspection-based
work in this document is genuinely finished: the interface has been audited
four ways, the implementation has been measured, and every finding has a home
in §111's specification.

---

# Part III (continued) — twenty-third pass, 2026-08-30

*§139.4 proposed a check — "assert that every exported function is executed
at least once by the suite" — and noted that `ggcpt_compare_table` would fail
it. The coverage data from §138 can answer that question now rather than
after the check is built. Five exports fail it, and three of them are a
coherent group.*

## 141. Which exports the suite never runs

Of 130 exported objects, 124 appear in the coverage data (the remaining six
are aliases or S3 registrations with no body of their own).

| Exported function | Expressions | Coverage |
|---|---:|---:|
| `cpt_load_tcpd` | 42 | **0%** |
| `ggcpt_compare_table` | 7 | **0%** |
| `geom_changepoint` | 1 | **0%** |
| `geom_cpt_ci` | 1 | **0%** |
| `geom_cpt_segment` | 1 | **0%** |
| `mcp_wrapper` | 34 | 5.9% |
| `fabisearch_wrapper` | 36 | 25.0% |

Two of these are understood and defensible. `cpt_load_tcpd` needs the network
and is deliberately never run (verified offline by hand during the 0.5.0
audit). `mcp_wrapper` needs JAGS, which is S34's whole story.

### 141.1 The three geoms are the finding

`geom_changepoint()`, `geom_cpt_ci()` and `geom_cpt_segment()` are **never
called by any test**. Each is a one-expression function, so the number is
unambiguous: the body has never executed.

These are the package's three original composable layers — the 0.1.0/0.4.0
generation, the ones the README leads with. The suite exercises them only
*indirectly*, through `autoplot()`, which builds its own layers rather than
calling these wrappers. The 25 vdiffr snapshots check that plots look right;
they never check that these three exported functions work when a user calls
them the way the README shows.

That is a specific, plausible failure mode: someone follows the README's
"Custom geoms, stats, and theming" section, calls `geom_cpt_ci()` directly,
and hits a defect no test could have caught. It is also exactly the shape of
S2 and S3 — both of which were `geom_cpt_event()` defects found by *building
a plot with the geom*, which is the test these three lack.

It also sharpens §138.1 and §69. `R/geoms.R` at 40.5% is not evenly thin: the
`StatChangepoint` ggproto object is covered and the three exported wrappers
around stock geoms are not. §69 wants to replace exactly those wrappers with
real `Geom` objects, and they are the part with no direct test at all.

### 141.2 `ggcpt_compare_table`

Seven expressions, zero coverage, exported, and part of the `ggcpt_compare()`
family that the 0.4.0 ledger lists as a headline feature. §S22's audit
confirmed it is mentioned in the README; nothing confirms it runs.

### 141.3 `fabisearch_wrapper` at 25%

Its tests are the negativity guard and the all-zero-row guard added as S19 —
both of which error *before* reaching the engine. So a quarter of the wrapper
is covered and the three-quarters that actually calls NMF is not, because
that path is slow (22s) and gated. Understandable, and worth knowing: the
0.5.0 defects S7 and S8 both lived in that uncovered three-quarters.

## 142. What this adds to the plan

- **§139.4's check is now specified with its current failures.** Adding
  "every export executes at least once" to the suite means writing five
  tests, three of which are trivial (build a plot with each geom and assert
  the layer appears) and two of which are the known network/JAGS cases and
  should be `skip`ped explicitly rather than silently absent.
- **§69 gains a precondition.** Before converting the geoms to ggproto
  objects, give the three exported wrappers direct tests. They are currently
  the least-verified user-facing surface in the package and the refactor
  targets them precisely.
- **The distinction worth keeping**: §S22 checked that exports are
  *documented*; this checks that they are *executed*. Both are cheap, they
  catch different things, and the package now has evidence that the second
  finds real gaps the first does not.

---

# Part IV — Parallel research, 2026-08-30

*Six research agents run concurrently on questions twenty-three passes had not
asked. Three have reported; their findings are below, with every CRAN claim
re-verified against the live index and download counts attached. Two of the
three overturn something this document asserted.*

## 143. The CRAN blind spot: changepoint work that does not say "changepoint"

A sweep of the TimeSeries, Econometrics, Environmetrics and AnomalyDetection
task views found **21 packages doing changepoint, structural-break or regime
detection that are not in this document's list of 40**. All twenty checked
are live on CRAN; downloads are last-month.

### 143.1 The econometric structural-break cluster — the important find

| Package | Downloads | What it does |
|---|---:|---|
| **`MSwM`** | **4,406** | Markov-switching models — regime detection |
| **`fxregime`** | **2,440** | Exchange-rate regime analysis in a structural-change framework |
| `strucchangeRcpp` | 558 | **C++ `strucchange`** — same API, faster |
| `pdR` | 420 | Threshold models and unit-root tests |
| `COINT` | 331 | Unit-root tests with structural breaks |
| `MultipleBubbles` | 264 | Phillips–Shi–Yu explosive-behaviour date-stamping |
| `pvars` | 244 | Panel VAR with breaks in deterministic terms |
| `makicoint` | 199 | Maki cointegration test with multiple breaks |

These **detect and date breaks but present as unit-root, cointegration or
threshold testing**, so they never surface in a search for "changepoint".
`MSwM` alone outranks every wired engine except `strucchange`, `segmented`,
`changepoint` and `trend`.

This is the third independent line of evidence for the same conclusion.
§117's download data said the audience is in regression breakpoints; §121
found the formula interface built and unreachable; and now the *packages
themselves* cluster there, invisibly, under econometric vocabulary. §49
proposed an HMM coercion for `depmixS4` (2,406/month by comparison) — `MSwM`
belongs in that theme and is larger.

### 143.2 Three more that matter

- **`surveillance`** (1,466/month) — outbreak detection in **count,
  proportion and categorical** time series. It is the only package appearing
  in three of the four task views, and it sits at the intersection of §30
  (count families), §63.2 (categorical) and §57 (epidemic changepoints). If
  one package were to be added for the applied public-health audience, this
  is it.
- **`rupturesRcpp`** (219) — an **R port of Python's `ruptures`**, which is
  the reference design in §146 below. Its presence on CRAN means several of
  §146's interface ideas are reachable as a wrapper rather than a rewrite.
- **`BayesChange`** (302) — Bayesian change point analysis that **clusters
  series by common structural changes**, which is §41's panel theme, on CRAN,
  after §108 downgraded that theme for want of an available engine. **§41 is
  upgraded again.**

Also found: `trendsegmentR` (point anomalies plus linear trend changes, §47 +
§57), `densratio` (density-ratio changepoint detection), `jumps` and
`StructuralDecompose` (break-aware filtering and decomposition — the same
estimation problem framed as filtering), `changeS`, `pasadr`, `jointseg`.

### 143.3 The registry lesson

`strucchangeRcpp` and `changepoint.np` are siblings of packages already
wired, under different names. A registry keyed on the parent package name
would silently miss them. Worth a check when §124's `choices` column is
built.

## 144. §59 was wrong: the package does not need to bundle data

§59 argued the package ships no data and proposed bundling two or three
permissively licensed series. The research says the premise is right and the
**remedy is unnecessary**, because the packages already in `Suggests` and
`Imports` ship annotated series:

| Dataset | From | Notes |
|---|---|---|
| `Nile` | **base R `datasets`** | n = 100, the canonical changepoint series, CP at 1898 (Aswan dam). Free, always present. |
| `well_log`, `bitcoin`, `occupancy`, `transcriptome` | `fastcpd` (Apache-2.0) | **`well_log` is sourced from TCPD** — direct CRAN precedent that TCPD-derived data is redistributable |
| `wave.c44137`, `HC1`, `Lai2005fig3/4`, `ftse100` | `changepoint` (GPL) | up to n = 63,651 |
| `ACGH`, `DJIA` | `ecp` (GPL) | |
| `coriell`, `lombard`, `QuebecRivers` | `bcp` (GPL) | |
| `HeartRate` | `changepoint.np` (GPL) | n = 1,160 |

**Revised §59:** rather than bundling anything, write the vignette and the
examples against `datasets::Nile` and `fastcpd::well_log`. Zero bytes added,
zero licence risk, and a reader still gets a real annotated series in the
first thirty seconds. Bundling remains an option only if a *count* series is
wanted for §30's families and none of the above serves.

If bundling does happen, the research names the defensible route: the
**TSSB** benchmark (BSD-3-Clause, 75 UEA/UCR series with exact constructed
ground truth, smallest 2.3 KB) following `fastcpd`'s pattern — bundle from a
repo with an explicit permissive licence, cite the original authors in
`@source`, note it in `LICENSE.note`. The caveat that matters: TSSB's BSD-3
covers Ermshaus's *compilation*; the underlying UCR/UEA data carries no
explicit licence, only a citation request.

Also relevant to §20: **NAB** (MIT, 58 series) labels anomaly *windows*, not
changepoints, so it needs a different scoring rule; and **nothing published
in 2024–2026 rivals TCPD** as a general changepoint benchmark.

## 145. What the Python and Julia ecosystems do that R does not

The most valuable of the three reports, because it is about *interface* rather
than methods. Twelve ideas, ranked; the first four are the ones this package
could act on.

### 145.1 Mark the changing parameter in the model, not the function name

`Changepoints.jl`:

```julia
@PELT data Normal(:?, 1.0)    # mean changes, variance fixed
@PELT data Normal(:?, :?)     # both change
@PELT data Exponential(:?)    # rate changes
```

The `:?` marks *which parameter changes*, syntactically, in the notation a
statistician already uses. **One grammar replaces the entire
`cpt.mean`/`cpt.var`/`cpt.meanvar` taxonomy** — and, for this package,
replaces both `change_in` and §30's proposed `family` with a single
expression. R's formula culture makes this unusually natural here, and it
subsumes Theme BN's formula interface (§121) rather than competing with it.

This is the single most expressive idea in the survey.

### 145.2 Sparse/dense duality with public converters

`sktime`'s `BaseDetector` spans changepoints, point anomalies, segment
anomalies and segmentation under one contract, and provides
`sparse_to_dense()` / `dense_to_sparse()` as **public methods**: sparse is
the changepoint indices, dense is a per-timepoint label column *indexed like
the input*.

That dense form is exactly what a `dplyr`/`ggplot2` pipeline wants and what
`augment()` half-provides. It also gives §57's epidemic changepoints and
§28's attribution a shared output shape — the "four related tasks, one base
class" idea directly addresses the representational gap §57 identified.

### 145.3 Choose the stopping rule at predict, not at fit

`ruptures` fits once and then answers `predict(pen=)`, `predict(n_bkps=)` or
`predict(epsilon=)`. This package re-runs the whole detection for each. It is
the same insight as CROPS, generalised — and `cpt_select()`'s six criteria
would become views on one fit rather than six searches.

### 145.4 A documented cost extension point

`ruptures` requires exactly two methods of a custom cost — `fit(signal)` and
`error(start, end)` — after which it works with **every** search method
unchanged. `skchange` generalises further: one `BaseIntervalScorer` unifying
costs, change scores and anomaly scores, with **penalties as objects** and a
tuning module that calibrates a penalty to a target false-alarm rate.

This package has `cpt_register_method()` for whole *detectors*; it has no way
to supply a *cost*. §126.2 found `fastcpd`'s blocked `custom` family wanting
exactly this.

### 145.5 The rest, briefly

- **Streaming as a peer protocol**: `river`'s `update(x)` then
  `drift_detected`, with a **warning tier** distinct from the alarm tier so
  downstream code can buffer before committing — §64.4's baseline-drift
  problem has a standard answer.
- **`NoDrift` / `DummyDriftDetector`** — a null detector with the real
  interface, shipped, making ablation and testing trivial. Cheap and absent
  here.
- **Soft scores as a standard second output** — ClaSP's per-timepoint
  transition profile, BOCPD's full run-length matrix, `predict_scores()`.
  §94.3 argued the curve is the more honest object; three ecosystems already
  return it by default.
- **Detect on a model's residual stream**, not the raw data — `river` is
  explicit that drift detectors monitor a model's error sequence. That is a
  documented *workflow*, and it is §35's segment models pointed the other
  way.
- **Metrics ship with the detector**, margin-tolerant by default.
- **Capability tags and a queryable registry** — `all_estimators(...)`
  filtering by task, multivariate support, supervision. This package's
  `R/registry.R` is the foundation; §124's `choices` column is the next step.
- **Plots for the decision, not the result**: `elbow_plot` draws the
  model-selection problem itself; `plot(gt_cps=)` makes ground truth a
  first-class plotting argument, so "detected vs truth" is the default view.

## 146. What Part IV changes so far

- **§41 (panel) upgraded** — `BayesChange` is on CRAN and clusters series by
  common structural change, which is the engine §108 said did not exist.
- **§59 (bundled data) substantially withdrawn** — `datasets::Nile` and
  `fastcpd::well_log` are already available; bundling is optional, not
  needed.
- **§49 (HMM coercion) enlarged** — `MSwM` at 4,406/month is bigger than
  `depmixS4` and belongs in the same theme.
- **A new candidate engine with a strong case**: `surveillance`, at the
  intersection of three existing themes and the only package in three task
  views.
- **A new class of interface proposals** (§145) that no earlier pass
  produced, because every earlier pass looked at methods rather than at how
  other ecosystems shape their APIs. §145.1 in particular could replace
  `change_in`, §30's `family` and §121's formula interface with one grammar.
- **Confirmation, from a third direction, that the econometric/regression
  audience is the underserved one** (§143.1).

Three agents are still running: user pain points mined from Stack Overflow
and GitHub issues, GitHub-only R implementations, and applied-domain
reporting conventions. Their findings will follow.

## 147. What users actually ask — §115's evidence, finally

§115 observed that not one line of this document rested on a user saying what
they wanted, and called that indefensible for a package about to freeze its
API. This is that evidence: **318 threads** from Stack Overflow and Cross
Validated via the Stack Exchange API, **97 GitHub issues** across
`changepoint`, `mcp` and `bcp`, and Posit Community, ranked by frequency,
views and cross-source repetition.

*(One claim in the report — that `bcp` was removed from CRAN — is **false**;
`bcp` is live at 4.0.4. It came from a GitHub issue written during a
temporary archival. Fifth external claim in this document caught by checking,
and consistent with §113.1's rule.)*

### 147.1 The twelve recurring pain points

| # | What users ask | Already served by 0.5.0? |
|---|---|---|
| 1 | **"What penalty? How many changepoints is right?"** — the single most common question | `cpt_select()`, six criteria — **yes** |
| 2 | **"Is this changepoint significant? Where's the CI?"** | `cpt_confint()`, `cpt_test()` — **yes** |
| 3 | **"Which method do I use, and why do they disagree?"** | `cpt_recommend()`, `ggcpt_compare()`, `cpt_consensus()` — **yes** |
| 4 | **"How do I get the numbers *out* of this object?"** | `tidy()`/`glance()`/`augment()` — **yes** |
| 5 | **"How do I plot this in ggplot?"** | the entire package — **yes** |
| 6 | **"I want a change in trend/slope, not mean"** (highest-view single thread, 7.3k) | `change_in = "slope"`, `cpop`, `segmented` — **yes** |
| 7 | **"Run this over many series, and it's too slow"** (one user: 80,000 series) | `cpt_batch()` + `future` — **partly**; no pooling (§41), no chunking (§34) |
| 8 | **Constrain the fit** — fix a breakpoint, bound its range, force a slope | **no** — see §147.3 |
| 9 | **Cryptic errors, silent `NA`, input-class fussiness** | partly; §81's condition classes are the fix |
| 10 | **Off-by-one and date semantics** — is the index the last old point or the first new one? | `cp_convention` documented — but see §82 |
| 11 | **Assumption violations produce nonsense with no warning** — autocorrelation, seasonality, *and rescaling the data changes the answer* | partly; §51's diagnostics and §77's preprocessing are the fixes |
| 12 | **Bayesian posterior → a decision: what threshold is a changepoint?** | partly; §94's K-uncertainty is adjacent |

### 147.2 The finding that reframes the roadmap

The report separately lists **seven things users repeatedly ask for that "no
R package provides."** Checked against 0.5.0:

| Requested, "unavailable" | Reality |
|---|---|
| Automated, defensible model selection | `cpt_select()` — **shipped** |
| A uniform runner/comparator across methods | `cpt_detect()` over 50 engines, `ggcpt_compare()`, `cpt_benchmark()` — **shipped** |
| Location uncertainty for the fast frequentist packages | `cpt_confint()` with a bootstrap provenance — **shipped** |
| A tidy/broom + ggplot layer, `tidy()`/`augment()` + `geom_changepoint()` | **shipped**, and named almost exactly |
| Grouped/hierarchical detection | `cpt_batch()` — **half shipped** (no pooling: §41) |
| Trend/slope changepoints for a bare series | **shipped** |
| Multivariate detection saying *which variable* changed | **not shipped** — §28 |

**Six of seven are already built.** The most-viewed thread in the entire
corpus (4.3k views) asks for a uniform comparator across
`changepoint`/`strucchange`/`segmented`/`bcp`/`ecp`, and every answer to it
is prose rather than code — while this package has done exactly that for
fifty engines since 0.4.0.

That is the strongest possible confirmation of §118.4's suspicion, arriving
from an independent direction: **the binding constraint on this package is
not capability, it is that nobody knows it exists.** 359 downloads a month
against a demonstrated, documented, recurring demand.

The consequence for §111 is direct and uncomfortable for a roadmap made of
features: the highest-value work is not on the list. It is answering those
threads, writing the comparison the 4.3k-view question asks for, and getting
the package in front of the people already asking. §36's teaching material,
§59's real-data examples, §71's cheatsheet and §96's failure catalogue are
not documentation chores — on this evidence they are **the product**.

### 147.3 The one genuinely new gap: constrained detection

Pain point 8 has no theme anywhere in twenty-three passes:

- **fix a breakpoint** at a known location and estimate the rest around it;
- **bound a breakpoint's range** — "the policy took effect sometime in Q2";
- **force a segment's slope** (e.g. flat before an intervention);
- **set a minimum segment length** uniformly (engines expose this variously
  as `minseglen`, `min_size`, `h`, `min.size` — §123's vocabulary problem).

`segmented` supports fixed `psi` and there are four separate threads asking
how. This is the constrained twin of §87's `cpt_test_at()`: that tests a date
you already have, this *fits around* one. Both come from the same user with
the same domain knowledge, and the package serves neither.

Proposed: `cpt_detect(..., fixed = , within = , min_segment = )` — `fixed`
pins changepoints, `within` restricts the search to intervals, `min_segment`
normalises the minimum-length argument across engines. The last of those is
pure §124 vocabulary work and could ship with it.

### 147.4 Pain points that confirm existing findings

Three of the twelve independently confirm defects this document found by
inspection, which is worth recording because it raises confidence in both:

- **#10, off-by-one and date semantics** — four GitHub issues on
  `changepoint` alone. §82 found the convention is asserted and never
  verified across fifty wrappers. Users are confused about this *in the
  engines*; a package that harmonises fifty of them and gets one wrong would
  be worse than the status quo. §82's test moves up.
- **#11, "rescaling the data changes the changepoint count"** — an open
  `changepoint` issue. That is §77's preprocessing sensitivity, reported as a
  live user surprise rather than a hypothetical.
- **#9, silent `NA` and cryptic errors** — §81's condition classes and
  §37.1's `na_action`, both already in 0.6.0.

## 148. What four applied fields expect that a detector does not give

Clinical/epidemiology, finance/econometrics, environmental remote sensing and
industrial condition monitoring, surveyed for what practitioners do *around*
detection and what they must report.

### 148.1 The cross-cutting finding

**All four fields want the same thing, and it is uncertainty on the
changepoint itself:**

| Field | What it demands |
|---|---|
| Finance | a **confidence interval on the break date**, from Bai–Perron |
| Epidemiology | CIs on **level change and slope change separately** |
| Remote sensing | a CI on the **area affected**, design-based and bias-adjusted |
| Condition monitoring | a calibrated **ARL0**, and ARL1 / detection delay at a target severity |

Four fields, four vocabularies, one requirement. The report notes the methods
literature for this — post-selection inference conditional on the detection
event — "exists but is barely reflected in applied practice."

That is independent applied justification for exactly the three themes
already sequenced into 0.7.0: **§29** (conformal intervals, distribution-free
where forty-three engines currently get a guarantee-free bootstrap), **§42**
(effect size with an interval), and **§28** (attribution). They were proposed
on methodological grounds; four unrelated professions turn out to require
them.

Two further needs recur that no generic package emits:

- **Effect size in domain units** — rate ratio, mm/s, hectares, basis points
  — not a test statistic. §42 proposed `delta`, `delta_std` and
  `pct_change`; the domain evidence says the *unit* matters and argues for
  §30's families (a rate ratio only exists if the model is Poisson).
- **Attribution of the break to a named cause** — an event date, a fault
  mode, a change agent, a station-metadata entry. `cpt_annotate_events()`
  ships and §87.3's `cpt_attribute_event()` was proposed; this says it is
  a reporting *requirement* in three of the four fields, not a nicety.

### 148.2 Preprocessing is a domain standard, not a user whim

§77 argued preprocessing is an unrecorded decision. The survey shows each
field has a *codified* pipeline that runs before any detector:

- **Epidemiology**: deseasonalise first (unmodelled seasonality is the most
  common source of a spurious break), *then* diagnose autocorrelation on
  residuals; convert counts to population-based, age-standardised rates with
  denominator offsets; correct recent points for reporting delay.
- **Finance**: log returns; unit-root pretesting, because a break and a unit
  root are near-observationally-equivalent — Perron (1989) showed ignoring a
  break biases unit-root tests toward non-rejection; an explicit trimming
  fraction (5–25%) bounding breaks from each other and the sample ends.
- **Remote sensing**: QA screening, atmospheric correction, cross-sensor
  harmonisation *so a platform swap is not read as land-surface change*,
  harmonic de-seasonalising, and a spectral index matched to the disturbance.
- **Condition monitoring**: the waveform is **never** what gets tested — it
  is collapsed into scalar health indicators (RMS, kurtosis, crest factor,
  band energies) which are then trended.

Three consequences for the roadmap. §77's `cpt_preprocess()` should carry
**named domain recipes**, not just individual switches. The **offset/exposure**
argument (§37.3) is mandatory in epidemiology, not optional. And the finance
**trimming fraction** is the same concept as §147.3's `within` and
`min_segment` — one argument, three fields.

### 148.3 The reporting standards nobody has read

Each field has formal requirements a changepoint result must satisfy:

- **Clinical**: Cochrane EPOC's ITS guidance (≥3 points per segment) and the
  Bernal–Cummins–Gasparrini tutorial are the de facto standards; ICH E9(R1)
  and SPIRIT require the changepoint definition to be **fixed in the protocol
  pre-unblinding** — post hoc redefinition is a protocol amendment. That is
  §36.2's `cpt_checklist()` promoted from a nice idea to a regulatory fit.
- **Finance**: sequential sup-F(l+1|l) against **Bai–Perron** critical
  values (not Chow), BIC or LWZ for the number of breaks, CIs on each date,
  regime-wise coefficients with standard errors, and corroborating CUSUM /
  Nyblom–Hansen / Quandt–Andrews. Nearly all of that is in `strucchange`'s
  return value and §129.2 found we discard the table.
- **Remote sensing**: Olofsson et al. (2014) is effectively mandatory —
  accuracy on an *independent* probability reference sample, a confusion
  matrix in estimated **proportions of area**, a bias-adjusted area
  estimator with CIs. `cpt_metrics()` computes none of these.
- **Industrial**: ISO 17359 / 13374 / 13379 / 13381 and ISO 10816/20816,
  which specify zones A–D with alert and trip setpoints. §75's SPC theme has
  a standards backbone it did not know about, and ISO 13381 **requires stated
  uncertainty on any prognosis**.

### 148.4 The exhibit every field wants and none of the fifty engines draws

**Observed versus counterfactual** — the series with a "what would have
happened absent the change" overlay. It is epidemiology's primary ITS figure,
it is what finance's out-of-sample forecast comparison shows, and it is the
natural display for §42's effect size.

§70 listed five missing plots and this is a sixth, arguably more important
than any of them: it is the figure that goes in the paper. It needs §35's
segment models to produce the counterfactual, which makes §35 load-bearing
for the applied audience twice over — once for coefficients (§121), once for
this.

## 149. GitHub-only implementations — the extension mechanism's actual inventory

§18 built `cpt_register_method()` for detectors this package "neither wraps
nor can depend on", and §110.2 noted the mechanism has never been used in
anger. A survey of ~50 repos (26 deep-read, every name cross-checked against
the live CRAN index) found **fifteen installable R packages and seven
script-only implementations** on GitHub that are not on CRAN.

### 149.1 The five that unblock existing themes

| Repo | What it does | Theme it unblocks |
|---|---|---|
| **`rachelcarrington/changepointsPSI`** (active, 2026-02) | Post-selection inference for changepoints over **BS/WBS/PELT/seeded-BS/L0**, and for changes in mean, **slope** *and* **variance** | **§0.9 item 1.** That item is recorded as blocked on `ChangepointInference` being GitHub-only. This *imports* ChangepointInference and extends it past L0/fixed-window to PELT and WBS, and to variance — and is actively maintained where the original is not. The block is weaker than recorded. |
| **`mjhollaway/GAM.PELT`** (with Killick) | GAM likelihood over 2D location + time, inside PELT | **§33.** That theme says "no CRAN package yet, so this is a `planned` registry entry". There *is* an R implementation — registration reaches it today. |
| **`gtromano/NUNC`** | Online **nonparametric** changepoint detection, rolling windows | §16/§63.2. FOCuS is parametric and `changepoint.np` is offline; this is neither. |
| **`grundy95/changepoint.forecast`** | Sequential monitoring of **forecast errors** to flag model degradation | **§145.5 and §64.4.** This is `river`'s "detect on the model's residual stream" idea, already implemented in R, and it is the answer to the baseline-drift problem §64.4 said had none. |
| **`Lucas-Prates/blockcpd`** | Regularised likelihood across **multiple aligned signals** with a shared changepoint set, plus a per-index "confidence plot" | **§41.** A second panel engine, after `BayesChange` (§143.2). The theme §108 downgraded now has two. |

### 149.2 The rest, briefly

`grundy95/changepoint.cov` (covariance and **subspace** changepoints — CRAN
has univariate `changepointsVar` and geometric `changepoint.geo`, not this);
`cchen22/PARROT` (**bipartite** network changepoints — CRAN's
`NetworkChange` is unipartite); `haeran-cho/fvarseg` (separates **common**
from **idiosyncratic** changepoints under a factor model);
`gaofengnan/charcoal` (changepoints in high-dimensional **regression
coefficients** via complementary sketching); `jongheepark/BridgeChange`
(sparse Bayesian changepoint regression, panel, p ≫ n);
`lpishchagina/GeomFPOP` (exact **multivariate** FPOP — CRAN's `fpop` is
univariate); `Lujia-Bai/fcpseed` (functional, with a formal existence test);
`vrunge/svpChange2` (validity-test-driven segmentation rather than penalised
cost); `yingboli/BayesMDL`; `rkillick/changepoint.online`.

Script-only but usable: `wcm.gsa` (changepoints under serial dependence),
`TAVC.seg` (robust variance estimation feeding MOSUM/WBS2), `mosum.fts`
(MOSUM under a factor model), `DAIS` (data-adaptive isolation, 2025),
`ocd_CI` (**confidence intervals for online high-dimensional detection** —
directly §29 in the streaming case), `changeAUC` (model-free detection via a
classifier's AUC).

### 149.3 Five more archivals, verified

Checked against the live index:

| Package | Status | What is lost |
|---|---|---|
| `gfpop` | **archived 2024-03-29** | graph-constrained changepoints — already `planned` here |
| `CPAT` | **archived 2025-12-13** | CUSUM, Darling–Erdős, Hidalgo–Seo, **Rényi-type** tests. Rényi-type detection of *early or late* changes exists nowhere else in R |
| `breakpoint` | **archived 2025-06-17** | cross-entropy changepoint search |
| `VARDetect` | **archived** | VAR structural breaks |
| `LinearDetect` | **archived** | linear-model structural breaks |

That is five archivals in the changepoint space in roughly two years, three
of them in the last twelve months. **§100's engine-lifecycle theme was
written from one example (`hdbinseg`) and the real rate is far higher.**
`VARDetect` and `LinearDetect` are also both in the regression/VAR area that
§117 and §143.1 identify as the largest audience — that audience is losing
CRAN packages while its demand grows, which is an argument for this package
covering it rather than against.

### 149.4 A concrete warning for §18

Two of the fifteen (`NUNC`, `GeomFPOP`) ship **unedited `usethis` boilerplate
in `DESCRIPTION`** — Title fields reading "What the Package Does", Author
"Your Name". Any registration path that reads package metadata to populate a
registry row will produce garbage for them. `cpt_register_method()` takes its
metadata from arguments rather than the package, which turns out to be the
right design — worth recording as a validated decision rather than an
accident.

## 150. What Part IV changed, in total

Six agents, six questions no earlier pass asked. The results, consolidated:

**Withdrawn or corrected:**

- **§59** — bundling data is unnecessary; `datasets::Nile` and
  `fastcpd::well_log` are already there.
- **§41** — upgraded twice: `BayesChange` (CRAN) and `blockcpd` (GitHub).
- **§33** — `GAM.PELT` exists; "no implementation" was wrong.
- **§0.9 item 1** — `changepointsPSI` is a live, maintained route to
  selection-adjusted inference, and covers variance changes too.
- **§100** — the archival rate is five packages in two years, not one.

**New and significant:**

- **§147.2**: six of the seven things users say no R package provides are
  already built here. The constraint is discovery, not capability.
- **§148.1**: four applied fields independently demand changepoint-location
  uncertainty, decomposed effects, and calibrated error rates — the exact
  content of §28, §29 and §42.
- **§143.1**: 21 CRAN packages invisible under econometric vocabulary,
  `MSwM` at 4,406/month among them.
- **§145.1**: Changepoints.jl's `:?` grammar, which could unify `change_in`,
  §30's `family` and §121's formula interface.
- **§147.3**: constrained detection — fix a breakpoint, bound its range —
  a real user need with no theme in twenty-three passes.

**Verification record:** six external claims were checked and **two were
false** — that `bcp` had left CRAN (it is at 4.0.4) and, earlier, `hdbinseg`
and `changepoint.mv`. Everything recorded above has been re-checked against
the live CRAN index. §113.1's rule continues to earn its place: claims about
other people's packages are provisional until verified, and the failure rate
is roughly one in three.

---

# Part V — The plan, after the evidence

*§111 consolidated twelve prioritisation sections into one 0.6.0
specification. It was written from source-reading, before Part IV. Five of
its premises have since changed and its central assumption — that the next
release should be about features — is contradicted by the only user evidence
this document has ever had. This supersedes it.*

## 151. What the evidence actually says

Four independent lines, none of them available when §111 was written:

1. **Downloads (§117).** `strucchange` and `segmented` together are ~80,000
   a month; every other wired engine combined is a fraction of that. The
   audience is in **regression breakpoints**.
2. **CRAN structure (§143).** Twenty-one further packages do break detection
   under econometric vocabulary — `MSwM` alone at 4,406. Same conclusion,
   independent route.
3. **User questions (§147).** Six of the seven most-requested capabilities
   are **already built**. The most-viewed question in the corpus asks for
   something this package has shipped since 0.4.0.
4. **Applied requirements (§148).** Four professions demand the same three
   things — location uncertainty, decomposed effects, calibrated error rates.

Taken together they say something §111 does not: **the gap between what this
package does and what people need is much smaller than the gap between what
it does and what people know it does.** A feature-led 0.6.0 would widen the
first gap slightly and leave the second untouched.

## 152. 0.6.0 — reframed

Three tracks, in priority order. The freeze obligations are unchanged from
§111.1 because they are obligations; everything else is re-weighted.

### Track 1 — Be findable (new, and now the highest priority)

Nothing here is a feature. On §147's evidence it is worth more than
everything in Track 3.

| Item | Why |
|---|---|
| **Answer the questions that already exist** — the 4.3k-view "which package do I use" thread, the ggplot threads, the "how many changepoints" threads | Each is a person who wanted this package and did not find it. §147 lists the URLs. |
| **The comparison document** the 4.3k-view thread asks for: `changepoint` vs `strucchange` vs `segmented` vs `bcp` vs `ecp`, one series, one table | Nobody has written it in code. This package can generate it in ten lines. |
| **§96's failure catalogue** — "Ten ways to get a changepoint wrong" | §147's pain points 9, 10 and 11 are three of the ten, reported independently by users |
| **§71's cheatsheet and decision tree** | pain point 3 |
| **Real-data examples using `datasets::Nile` and `fastcpd::well_log`** (§144) | zero bytes, zero licence risk, removes the all-synthetic-figures problem |
| **A JOSS or R Journal paper** | the discoverability instrument the R ecosystem actually has |

### Track 2 — Freeze obligations (unchanged from §111.1)

Condition classes (§81) · the convention test (§82, and §147.4 shows users
hit this in the engines) · engine-version stamping (§76) · deprecation policy
(§83.4) · the `as_json()` schema (§65) · real ggproto Geoms (§69, **after**
raising their 40.5% coverage per §138.1) · property and metamorphic tests
(§72) · fix R64 so the package is instrumentable (§135.2) · the
exports-executed check (§141).

### Track 3 — Features, re-ranked on evidence

| Rank | Item | Evidence |
|---|---|---|
| 1 | **Formula interface + `data` argument** (§121.3) | largest audience; `strucchange_wrapper()` already has it and `cpt_detect()` cannot reach it — **a bug, not a feature** |
| 2 | **Segment coefficient tables** (§35, §121.3.3) | wanted by finance (regime-wise coefficients), epidemiology (level *and* slope), and §129.2 found `fastcpd$thetas` already computed and discarded |
| 3 | **Widen `fastcpd`'s family set** (§126) | one character vector; delivers §30's families, part of §121, and §31's `custom` cost |
| 4 | **`cpt_effect()` with domain units** (§42, §148.1) | all four professions; rate ratio needs #3 |
| 5 | **`cpt_test_at()`** (§87) | the most common applied question, and the statistically easy case |
| 6 | **Constrained detection** — `fixed`, `within`, `min_segment` (§147.3) | user pain point 8, four threads, no theme before Part IV |
| 7 | **`na_action`, grouped frames, factor refusal** (§37, §63.4) | pain points 7 and 9 |
| 8 | **Diagnostics panel + `cpt_gof()`** (§51) | pain point 11, reported as "rescaling changes the answer" |
| 9 | **Informative empty answer** (§93) | connects three existing functions |

Items 1–3 are substantially *unblocking what exists* rather than building
new: a dispatcher fix, a discarded return value, and a `match.arg()` list.

## 153. 0.7.0 — the inferential release, now applied-justified

§28 attribution, §29 conformal intervals, §57 epidemic changepoints
(`anomaly`, and see §110.1 on its real size). §148.1 turns these from
methodological ambitions into requirements four professions already have. Add
**§148.4's observed-versus-counterfactual plot**, which is the primary
exhibit in epidemiology and needs Track 3 item 2 to produce it.

## 154. The one structural question to settle first

§145.1 — Changepoints.jl's `Normal(:?, 1.0)` grammar, marking the changing
parameter syntactically — would subsume `change_in`, §30's `family` and
§121's formula interface into one expression. It is the most elegant idea in
Part IV and it is **incompatible with doing those three separately**.

So it has to be decided before Track 3 items 1 and 3, not after. Either:

- **adopt it**, and design `change_in`/`family`/formula as one grammar from
  the start — larger, cleaner, and a genuine contribution to R's changepoint
  interface; or
- **decline it explicitly**, and record why (probably: it breaks the 0.4.0
  API, and the freeze is the wrong moment for a new notation).

Deciding by default — shipping `family` in 0.6.0 and discovering the grammar
question in 0.7.0 — is the one outcome to avoid, because the freeze will
have closed the door.

## 155. What this plan is not

It is not longer than §111 and deliberately so. Twenty-three passes and six
agents produced roughly forty themes; this ranks nine features, six
documents and nine obligations, and defers the rest explicitly.

The defence for that: §147 says the marginal feature is worth less than the
marginal reader, §138 says the code is 85.79% covered with the geoms at
40.5%, and §132 says three of nine registry capability columns had a false
entry. A release that adds nine engines to a package whose own claims are
partly untrue and whose audience cannot find it would be the wrong release.

**The single sentence version: 0.6.0 should make the package findable,
truthful about its own claims, and reachable through a formula — and should
add almost nothing.**

---

# Part V (continued) — resolving §154

*§154 named one decision as gating Track 3: adopt Changepoints.jl's
parameter-marking grammar, or decline it explicitly. It is the only open
question in this document that blocks other work, so it is worth settling
rather than restating. This works the design through and recommends.*

## 156. The problem the grammar would solve

`cpt_detect()`'s current signature is
`(x, method, change_in, penalty, index, y, ...)`, and `change_in` carries
nine values. Their distribution across the fifty methods is lopsided:

| `change_in` | Methods supporting it |
|---|---:|
| `mean` | 37 |
| `var` | 10 |
| `meanvar` | 6 |
| `slope` | 6 |
| `distribution` | 6 |
| `covariance` | 3 |
| `network` | 2 |
| `regression` | 2 |
| `seasonality` | 1 |

Three observations follow. First, `change_in` is doing at least **three
different jobs**: naming a *parameter* (`mean`, `var`, `meanvar`), naming a
*model class* (`regression`, `seasonality`, `network`), and naming a *degree
of assumption* (`distribution`). Second, `mean`/`var`/`meanvar` is a
combinatorial enumeration that will not extend — a Poisson rate change, a
change in a Gamma shape, a change in two of three parameters all need new
strings. Third, four of the nine values are supported by three methods or
fewer, so the vocabulary is already carrying near-dead weight.

Meanwhile §30 proposes a `family` argument, §121 proposes a formula
interface, and §147.3 proposes `fixed`/`within` constraints. Added
separately, `cpt_detect()` acquires ten arguments and the relationship
between `change_in = "var"` and `family = "gamma"` is left to prose.

## 157. What the grammar would look like in R

Changepoints.jl marks the changing parameter with `:?`. R has no `:?`, but it
has something better suited: **a formula, and `NA` as a placeholder that
already means "unknown"**.

```r
# the parameter that changes is the one left open
cpt_detect(x, model = normal(mean = NA, sd = 1))      # mean only
cpt_detect(x, model = normal(mean = NA, sd = NA))     # both
cpt_detect(x, model = poisson(rate = NA))             # a rate change
cpt_detect(x, model = gamma(shape = NA, rate = 1))    # shape only
cpt_detect(x, model = exponential(rate = NA))         # a hazard change
```

and, unifying §121's formula interface into the same slot rather than beside
it:

```r
cpt_detect(y ~ x1 + x2, data = d, model = normal(coef = NA, sd = 1))
cpt_detect(y ~ x1 + x2, data = d, model = binomial(coef = NA))
```

The old vocabulary becomes a documented shorthand that expands into the
grammar, so nothing breaks:

| Current | Expands to |
|---|---|
| `change_in = "mean"` | `normal(mean = NA, sd = 1)` |
| `change_in = "var"` | `normal(mean = 0, sd = NA)` |
| `change_in = "meanvar"` | `normal(mean = NA, sd = NA)` |
| `change_in = "regression"` | `normal(coef = NA, sd = 1)` with a formula |

### 157.1 What it buys

- **`change_in` and `family` become one thing.** §30's proposal disappears
  into the grammar rather than sitting beside it. A Poisson rate change is
  `poisson(rate = NA)`, not `change_in = "mean", family = "poisson"`, which
  is a combination nobody could have guessed was legal.
- **§121's formula interface is the same call**, differing only in the first
  argument, which is how R users already expect model specification to work.
- **§124's `choices` vocabulary is largely subsumed** — `test.stat`,
  `cpmType`, `probModel` and the rest are mostly families and costs under
  engine-specific names, and the grammar is where they belong.
- **Capability checking gets sharper.** The registry currently answers "does
  this method support `var`?"; it would answer "can this method leave `sd`
  open while `mean` is fixed?", which is the question the engine actually
  has.
- **It extends.** A change in two of three Gamma parameters needs no new
  string.

### 157.2 What it costs

Honestly:

- **It is a second way to say everything**, and two idioms is worse than one
  until the old one is deprecated — which the freeze forbids for at least a
  release cycle. So 0.6.0 would ship both and 1.0 could not remove either.
- **Fifty wrappers translate `change_in` today.** Each would need a mapping
  from the grammar back to its engine's own argument. §123 showed thirteen
  engines express the choice under their own name; the grammar makes that
  translation table explicit, which is good, but somebody has to write fifty
  rows of it.
- **Not every method fits.** What is `network(NA)`? What does
  `distribution` become — a family with *every* parameter open? The
  nonparametric engines (6 methods on `distribution`, plus `ecp`, `np`,
  `npmojo`) have no parametric model to mark, and forcing them into the
  grammar would be dishonest. They need an escape hatch, and an escape hatch
  in a unifying grammar is an admission it does not unify.
- **`covariance`, `network`, `seasonality`** — 6 methods between them — are
  model classes, not parameters. The grammar has nothing natural to say about
  them.

## 158. The recommendation

**Decline it for 0.6.0, and record the reasoning. Do not decide by default.**

Three reasons, in order of weight:

1. ~~**It does not unify what it claims to.** Of the nine `change_in`
   values, the grammar improves four and has nothing to say about four others
   (`distribution`, `covariance`, `network`, `seasonality`) covering thirteen
   methods.~~ **Corrected by §160 — this objection was wrong.** The test in
   §158.2 was run: `covariance` and `seasonality` *are* expressible as
   models, and only 6 of 50 methods resist entirely. See §160; the deferral
   below now rests on objections 2 and 3 alone.
2. **The freeze is the wrong moment.** 0.6.0's defining purpose is to stop
   the API moving (§111.2, §152 Track 2). Introducing a new primary idiom
   *in* the freeze release, unable to remove the old one, is the opposite of
   freezing.
3. **Part IV says the marginal reader beats the marginal feature.** §147's
   evidence is that users cannot find the package. A more elegant model
   specification does not address that, and the effort it would take —
   fifty translation rows plus a parallel documentation set — is precisely
   the effort Track 1 needs.

### 158.1 What to do instead, and it is not nothing

Take the parts of the idea that are compatible with a freeze:

- **Ship `family` as a plain argument** (§30), validated against a registry
  column, and **document the pairing rules explicitly** — which
  `(change_in, family)` combinations are legal per method. That is the
  grammar's main benefit (no unguessable combinations) without a new
  notation.
- **Write the translation table anyway.** The fifty-row mapping from
  `(change_in, family)` to each engine's own argument is needed for §124
  regardless, and it is the same table the grammar would need. Building it
  now means a future grammar is a presentation layer over an existing map
  rather than a rewrite.
- **Record the decision in the 1.0 contract discussion**, with §157's design
  attached, so 1.0 can revisit it as a *replacement* — at the one moment
  when removing `change_in` is permitted.

### 158.2 The condition that would reverse this

If the fifty-row translation table (§158.1) turns out to be mostly families
and costs — that is, if `distribution`, `covariance`, `network` and
`seasonality` can be expressed as *models* rather than needing an escape
hatch — then objection 1 dissolves and the grammar becomes the better design.
**Build the table first; let it decide.** That is a cheap empirical test of a
design question that would otherwise be settled by taste.

## 159. §154 is now closed

The blocking question has an answer: **`family` as an argument in 0.6.0, the
grammar deferred to 1.0 with its design recorded, and the translation table
built now because both paths need it.** Track 3 items 1 and 3 are unblocked
and can proceed.

That leaves this document with no open questions gating any work — which,
after twenty-four passes and six agents, is the state it should be left in.

---

# Part V (continued) — the test §158.2 asked for

*§158 declined the parameter-marking grammar on three objections and then set
a falsifiable condition: if `distribution`, `covariance`, `network` and
`seasonality` can be expressed as models rather than needing an escape hatch,
"the objection dissolves and the grammar becomes the better design. Build the
table first; let it decide." The table has been built. **The first objection
was wrong.***

## 160. The result

Classifying each of the nine `change_in` values by whether it can be written
as *a model with one parameter left free*:

| `change_in` | Methods | As a model |
|---|---:|---|
| `mean` | 37 | `normal(mean = NA, sd = 1)` |
| `var` | 10 | `normal(mean = 0, sd = NA)` |
| `meanvar` | 6 | `normal(mean = NA, sd = NA)` |
| `slope` | 6 | `normal(coef = NA)` + formula |
| `covariance` | 3 | `mvnormal(Sigma = NA)` |
| `regression` | 2 | `normal(coef = NA)` + formula |
| `seasonality` | 1 | `normal(harmonic_coef = NA)` |
| `distribution` | 6 | **resists** |
| `network` | 2 | **resists** |

And the decisive count, over methods rather than values:

```
fully expressible          42 of 50
partly (mixed support)      2        cpm, kwc
resist entirely             6        np, ecp, npmojo, geomcp, network, fabisearch
```

**84% of methods are fully expressible; 12% resist.** §158 claimed the
grammar "has nothing to say about four values covering thirteen methods" and
called that "an escape hatch for a quarter of the surface". Both figures were
wrong. `covariance` is a multivariate normal with `Sigma` free — that *is* a
model. `seasonality` is harmonic regression with the seasonal coefficients
free, which is precisely what `bfast` fits. I had flagged both as open
questions in §157.2 and then counted them against the grammar anyway, which
is the error.

### 160.1 And the six that resist are a principled category

`np`, `ecp`, `npmojo`, `geomcp` are **nonparametric** — they have no
parametric model to mark, by construction. `network` and `fabisearch` operate
on graph structure.

That matters, because an escape hatch for "the nonparametric engines" is not
an ad-hoc exception; it is a named, coherent category that the grammar can
state honestly:

```r
cpt_detect(x, model = nonparametric())          # np, ecp, npmojo, geomcp
cpt_detect(X, model = network_structure())      # network, fabisearch
```

A notation covering 84% cleanly, 4% partly, and naming the remaining 12% as a
category rather than an exception is **a good design**, not a failed
unification. §158's first objection is withdrawn.

## 161. Does the recommendation change?

**The conclusion holds; its basis does not.** §158 rested on three
objections. Objection 1 is withdrawn. Objections 2 and 3 stand entirely:

2. **The freeze is the wrong moment.** 0.6.0 exists to stop the API moving.
   Introducing a new primary idiom inside the freeze release — while unable
   to remove the old one for a full cycle — is the opposite of freezing. This
   is unaffected by how good the design is.
3. **Part IV's evidence.** §147 says users cannot find the package; a better
   model specification does not address that, and the effort competes
   directly with Track 1.

So the deferral is now a **timing** decision rather than a **design**
judgement, and that distinction changes what should be recorded:

- §158 said "revisit at 1.0 as a possible replacement." It should now say
  **1.0 should adopt it**, with the 84/4/12 split and the two escape-hatch
  categories as the design, unless something new argues otherwise.
- The fifty-row translation table (§158.1) is no longer insurance against a
  bad idea — it is **the first half of the implementation**.
- `family` in 0.6.0 (§158.1) should be designed as a *subset* of the eventual
  grammar rather than a parallel argument, so that
  `change_in = "mean", family = "poisson"` and `poisson(rate = NA)` mean the
  same thing internally from the start.

## 162. What this pass demonstrates

Two things worth recording, because they are about method rather than
content.

**A falsifiable condition, set and then tested, changed the answer within one
pass.** §158.2 was written as a hedge; it turned out to be the most useful
sentence in that section. The document has done this once before — §106's
index check corrected §100 — and both times the correction came from checking
something cheap rather than thinking harder.

**The error was a specific kind.** In §157.2 I listed `covariance` and
`seasonality` as open questions ("What is `network(NA)`? What does
`distribution` become?"), and in §158 I converted those open questions into
evidence *against* the proposal without resolving them. That is the fourth
wrong structural claim in this document — after `hdbinseg`'s status,
`esac`'s `coordinate` field, and the prediction about which test file would
break under instrumentation — and it has the same shape as all three:
**a plausible inference, stated with more confidence than the checking
behind it warranted.**

§113.1's rule was written for claims about other people's packages. On this
evidence it should be broader: **any structural claim in this document, about
anything, is provisional until checked — and the checks have been cheap every
single time.**

---

# Part V (continued) — the second premise audit

*§162 broadened §113.1's rule: any structural claim in this document is
provisional until checked, and all four errors so far came from the unchecked
pool. §113 audited twenty claims from Part III's early passes. Parts III–V
added dozens more that were never checked. This audits a sample of them.
**Seven of eight held; one was wrong; and one previously unmeasured claim is
now measured and turns out to be right for a reason I had not stated.***

## 163. Eight claims re-checked

| § | Claim | Result |
|---|---|---|
| §121.2 | `$segments` carries `param_estimate` | ✓ |
| §57.3 | the `regions` slot has `start`/`end` when populated | ✓ (verified on an `nsp` fit) |
| §63.4 | `as_cpt_series()` already dispatches on `ts` | ✓ |
| §63.4 | …and on a data frame | ✓ |
| §75 | `cpt_monitor()` offers exactly `edetector`/`cpm`/`ocd` | ✓ |
| §145.2 | `augment()` returns one row per observation | ✓ |
| §70.1 | `ggcpt_posterior()` refuses a non-Bayesian fit | ✓ |
| §102 | **"`dplyr` is used in a handful of places and could plausibly be replaced by base equivalents"** | **wrong** |

### 163.1 The `dplyr` claim

§102 asserted `dplyr` was used "in a handful of places" and floated dropping
it to cut the ~1-second load time. There are **eight** call sites, and where
they are matters more than how many:

```
R/ggchangepoint.R   dplyr::row_number(), dplyr::mutate()  x4
R/metrics.R         dplyr::mutate()
R/compare.R         dplyr::mutate()  x2
```

All eight are `mutate()` or `row_number()` — trivially replaceable
individually. But five of the eight are in `R/ggchangepoint.R`, which holds
the **0.1.0-era API** (`ggcptplot()`, `ggecpplot()`), and §7 commits to those
functions continuing to work unchanged. Touching them to save a second of
load time trades a backward-compatibility risk against a benefit no user has
asked for.

**Revised §102:** the call sites are few and shallow, but they sit in the
oldest and most compatibility-sensitive file in the package. Removing `dplyr`
is *feasible* and is not *worth it* — and the honest reason is the location of
the calls, not their count. Recorded as a decision rather than an option.

## 164. §84 measured: the claim was right, the reason was not

§84 asserted "vignette figures are the bulk" of the 4.7 Mb `doc` directory
and suggested `dpi`, `fig.retina` and SVG as remedies. That was never
measured. Measured now, from the installed `doc/` of the last check:

| Vignette | Total | Embedded base64 images | Share |
|---|---:|---:|---:|
| `ggchangepoint.html` | 1,636 KB | 1,484 KB | **90%** |
| `introduction.html` | 976 KB | 874 KB | **89%** |
| `comparison.html` | 858 KB | 790 KB | **92%** |
| `inference.html` | 792 KB | 749 KB | **94%** |
| `extending.html` | 239 KB | 198 KB | 82% |
| `supervised.html` | 211 KB | 175 KB | 82% |
| `index.html` | 4.5 KB | 0 | 0% |

**Between 82% and 94% of every vignette is base64-embedded PNG.** The
directory total is 4.8 Mb; roughly 4.3 Mb of it is images inlined into HTML.

So §84's conclusion was right and its proposed remedies are the wrong ones.
`dpi` and `fig.retina` shave a fraction; the structural fact is that
`rmarkdown`'s `self_contained` default **base64-inlines every figure**, which
inflates each image by about a third over its binary size and puts it in the
tarball. The effective levers, in order:

1. **Move the four large vignettes to pkgdown `articles/`** (§84's own
   proposal) — they leave the tarball entirely and the website is where
   people read them anyway. `ggchangepoint.html` and `introduction.html`
   alone are 2.6 Mb of the 4.8.
2. **`self_contained: false`** for any vignette that stays, so figures sit
   beside the HTML as PNG rather than inflated base64.
3. **SVG for the line-and-rule plots** — most figures here are a series, a
   few vertical rules and a legend, which vectorises to a fraction of a
   raster.
4. `dpi`/`fig.retina` last, as the marginal tweak §84 led with.

That reordering is the actionable part, and it only became visible by
measuring rather than reasoning.

## 165. Running tally of the document's own reliability

| Category | Checked | Wrong |
|---|---:|---:|
| Claims about this codebase (§113) | 20 | 0 |
| Claims about this codebase (§163) | 8 | 1 |
| Claims about external packages (agents, §106, §150) | ~9 | 3 |
| Structural predictions (§129.1, §135.4, §158) | 4 | 3 |
| Previously unmeasured quantitative claims (§164) | 1 | 0 (right, wrong reason) |

**Twenty-eight of twenty-nine claims about the package's own source have
held.** Everything else has a materially worse record: three of nine external
claims wrong, and three of four *structural predictions* wrong.

The pattern is now clear enough to state as a rule for whoever picks this up:
**this document is reliable where it describes code someone read, and
unreliable where it reasons about code, packages or behaviour someone
did not run.** The reasoning is not worse than average — it is that
`grep` and `available.packages()` are cheap and were skipped.

§113.1 asked for external claims to be verified. §162 broadened it to all
structural claims. §165 adds the calibration: the failure rate in the
unverified categories is roughly **one in three**, and every check that found
an error took under two minutes.

---

# Part V (continued) — measuring the top priority

*Part V ranks "formula interface + segment coefficient tables" as the highest-
value feature work, on the strength of §117's downloads and §148's applied
requirements. §148.3 asserted that "nearly all" of the finance reporting
template is already in `strucchange`'s return value. Per §165, that is
exactly the kind of unverified claim that fails a third of the time. Measured.*

## 166. The Bai–Perron template, item by item

§148.2 listed what a finance practitioner must report. Run against
`strucchange::breakpoints(y ~ x, data = d)` on a series with a genuine
coefficient break:

| Reporting requirement | In the engine's return? | Where |
|---|---|---|
| Break dates | **yes** | `bp$breakpoints` |
| RSS/BIC over the number of breaks | **yes** | `bp$RSS.table` — a **169 × 10** matrix |
| BIC per number of breaks | **yes** | `summary(bp)$RSS` |
| **Confidence intervals on each break date** | **yes** | `confint(bp)$confint` |
| Regime-wise coefficients | **yes** | `coef(bp)` — 2 × 2 |
| sup-F structural change test | **yes** | `sctest(type = "supF")` |
| Out-of-sample forecast comparison | no | not in `strucchange` |

**Six of seven.** §148.3's claim holds — and it is stronger than "nearly
all", because the one missing item is the only one that is genuinely a
modelling choice rather than an output.

## 167. What the package does with it

The same fit through `strucchange_wrapper()`:

```
changepoints: cp, cp_value, ci_lower, ci_upper
segments    : seg_id, start, end, n, param_estimate
slots       : changepoints, segments, data, method, change_in,
              penalty, fit, call, cp_convention, runtime
```

So of the six items the engine hands over:

| Item | Surfaced? |
|---|---|
| break dates | **yes** — `cp` |
| CIs on break dates | **yes** — `ci_lower`/`ci_upper`. Credit where due: this is one of only seven methods with a native interval, and it is wired |
| regime-wise coefficients | **no** — collapsed to a single `param_estimate` per segment |
| RSS/BIC over the number of breaks | **no** — the 169 × 10 table is discarded |
| BIC per number of breaks | **no** |
| sup-F test | **no** — `cpt_test()` has a `strucchange_jump_test()` branch but the model-level sup-F is not exposed |

**Two of six.** And the four missing are precisely the four that a finance
reader would consider the report.

### 167.1 `param_estimate` is the specific failure

`$segments` carries one `param_estimate` per segment. For a regression break
the estimate is a **vector of coefficients**, and `coef(bp)` returns it as a
2 × 2 matrix. Collapsing that to one number per segment is not a
simplification — it discards the answer. A user asking "did the slope on `x`
change, and by how much" gets a column that cannot express either.

This is the concrete form of §121.2's general observation, and it confirms
Part V's ranking with a measurement rather than an inference: **segment
coefficient tables are not a new feature. They are the un-discarding of
`coef(bp)`.**

## 168. What this changes in the plan

- **Track 3 item 2 (segment coefficient tables) is re-described.** Not
  "build a segment-model layer" but "stop collapsing `coef()` to a scalar,
  for the two engines that already return a matrix." §35's general design —
  fitting a user's model per segment — remains a larger, later thing; this is
  the part that serves the largest audience and is nearly free.
- **A fourth item joins Track 3**: expose the engine's own model-selection
  table. `bp$RSS.table` is 169 × 10 for a 240-point series, and §129.2 found
  `binseg` and `fpop` discard equivalent objects. `cpt_select()` computes six
  criteria itself while three engines hand over better-founded ones. That is
  §130.2, now measured.
- **The sup-F gap is worth naming separately.** `cpt_test()` tests *located*
  changepoints; `sctest()` tests whether the model has a break at all, which
  is §93.2's `cpt_test_null()` — and for `strucchange` it is one function
  call away. §93.2 proposed building that from scratch; for at least this
  engine it is extraction.

## 169. And the honest ledger entry

This is the second measurement in three passes to find that a proposed
"feature" is mostly already computed and thrown away — §164 found the same
shape in the vignette-size question, §129 in the solution paths, §126 in
`fastcpd`'s families.

Four independent instances is a pattern, and it deserves a name in the
roadmap: **the package's dominant defect class is not absence, it is
discard.** Fifty engines are wrapped, each returns more than the `ggcpt`
contract asks for, and the wrapper takes the changepoints and drops the rest.
Every audit that has looked at a return value has found something worth
keeping.

That suggests one more systematic pass, of the kind §130.1 proposed for
capability flags: **for each of the fifty engines, list what its return value
contains and decide, per field, keep or discard — deliberately, once,
recorded in the registry.** §129 did this by sampling and found six engines
with material loss. Doing it exhaustively is an afternoon per ten engines and
would likely close more of Part V's feature list than building anything.

## 170. The capability-flag audit §130.1 asked for, actually run

§130.1 proposed checking the registry's capability flags against what the
engines deliver, and §169 argued that the same pass over return-value
*fields* would close more of Part V than building anything. This section
runs the flag half of it against the released 0.5.0 tree and reports
numbers instead of an intention.

**Method.** For each of the 50 registry rows, detect on a 200-point series
with two mean shifts (a 200x6 matrix for the multivariate-only engines),
then call the accessor each flag promises and ask whether it returns
something usable:

| flag | accessor exercised | "delivers" means |
|---|---|---|
| `ci` | `cpt_confint(fit, method = "native")` | `native_bounds()` non-`NULL`, call returns finite bounds |
| `fitted` | `fit$data$fitted` | present and not all-`NA` |
| `posterior` | `posterior_prob_profile(fit)` | non-`NULL` with finite entries |
| `statistic` | `cpt_statistic(fit)` | non-empty tibble |
| `path` | `cpt_solution_path(fit)` | non-empty tibble |

`scale_space` is deliberately excluded. `cpt_scale_space()` takes a *series*
and a bandwidth vector, not a fit, and never dispatches on the engine, so it
succeeds for any input; the flag is advisory documentation about which
detectors are genuinely multiscale (`mosum` and `npmojo`), and no mechanical
test of a fit can confirm or refute it. Saying so explicitly matters: this
is the trap that produced the 33 false positives recorded at §132.3.

**Coverage.** 47 of 50 engines produced a fit. `mcp` is untestable here
(JAGS absent, and it claims `ci`, `fitted` and `posterior`); `hdreg`
requires a `response`, and `fabisearch` requires a matrix wide enough to
factorise — both correctly refuse plain data, which is behaviour, not
defect.

**Result.** 235 flag cells tested across the 47 engines: 36 positive claims
and 199 negative ones. Three positives are false. **No negative is a false
negative** — nothing is quietly delivering a capability the registry does
not advertise.

| flag | claims tested | correct |
|---|---|---|
| `ci` | 7 | 7 |
| `fitted` | 8 | 8 |
| `statistic` | 10 | 10 |
| `path` | 8 | 6 |
| `posterior` | 3 | 2 |

| method | flag | registry says | reality |
|---|---|---|---|
| `binsegrcpp` | `path` | `TRUE` | `cpt_solution_path()` errors |
| `wbsts` | `path` | `TRUE` | `cpt_solution_path()` errors |
| `bocpd` | `posterior` | `TRUE` | `posterior_prob_profile()` returns `NULL` |

`ci` is exact in both directions: all 7 testable claimers return
`source = "native"` or `"nsp_region"`, and a 12-engine control group of
non-claimers all correctly error under `method = "native"`.

### 170.1 The two `path` failures print a self-contradicting message

`cpt_solution_path()` builds its "these do" list from
`subset(cpt_methods(), path %in% TRUE)$method` — from the same flag that is
wrong. So refusing `binsegrcpp` prints:

> Engine `binsegrcpp` does not expose a solution path. These do: binseg,
> segneigh, wbs, wbs2, not, tguh, **wbsts, binsegrcpp**.

The suggestion names the thing that just refused the user, and names the
other broken engine as the alternative. Both upstream objects hold the path:
`binsegRcpp::binseg()` returns a splits table in the order splits were
taken, and `wbsts::wbs.lsw()` returns its candidate table with entry order.
Two small extractors make the flag true; two logicals make it honest.

### 170.2 `bocpd` is a different defect: the flag conflates two shapes

This one is not a lie about a missing object, and the first draft of this
section got it wrong. `bocpd_wrapper()` calls
`ocp::onlineCPD(data_vec, getR = TRUE, ...)` — it *explicitly asks* for the
run-length posterior — keeps the result on `$fit`, and `ggcpt_runlength()`
renders it. Measured on the test series: `R` is 201x201, one time point per
column (every column sums to exactly 1), rows indexing run length + 1.

What is missing is the *shape* the flag promises. The registry defines
`posterior` as "exposes a per-location posterior probability profile", and
`posterior_prob_profile()` recognises fits by class — `bcp` and `beast` —
returning `NULL` for everything else. A run-length posterior over a 2-D
(time x run length) grid is a different object from a per-location
changepoint probability, so:

- `ggcpt_posterior()` errors, naming only `bcp` and `beast`;
- `cpt_confint(method = "posterior")` errors with the same list;
- `cpt_recommend()` filters on `reg$ci | reg$posterior` (`consensus.R:327`)
  and then scores `+1` with the reason `"; supplies a posterior"`
  (`consensus.R:371-372`).

That last one is the real cost. `bocpd` gets *recommended for uncertainty
work, with a written justification*, and the two functions a user would
then reach for both tell them the engine has no posterior. The flag is not
decoration; it steers advice.

**And the marginalisation is available but is not one line.** The obvious
move — take row 1 as P(run length 0) — fails: `R[1, ]` is a flat `0.01`
everywhere, ocp's floor, not a signal. The informative reset sits at run
length 1 (row 2), which bumps at `t = 73` and `t = 142` against true
changepoints at 70 and 140, and the MAP run length per column climbs
`1, 2, ..., 73` then drops to 4, then climbs again and drops to 2 — the
resets are unmistakable but the index convention and the row-1 floor both
need handling. So this is a genuine small piece of work, not a rename, and
the honest interim state is to split the flag: `posterior_profile` (bcp,
beast, mcp) versus `posterior_runlength` (bocpd), so the recommender stops
promising a shape that does not exist.

### 170.3 The audit's own false positive, and what it proves about `...`

The first run of this sweep reported **39 underclaims** — nearly every
engine appearing to supply a native interval it never claimed. All 39 were
one harness error: it called `cpt_confint(fit, engine = "native")`. The
argument is named `method`. `engine` fell into `...`, `method` kept its
default `"auto"`, `auto` fell back to the bootstrap route, the bootstrap
succeeded, and every engine looked like it had native intervals.

`cpt_confint()` accepted a misspelled argument, silently ignored it, and
answered a different question than the one asked. The roadmap already has an
item objecting that `...` is an undiscoverable, unvalidated interface; this
is that item happening, to the audit written to check the package, in the
package's own inference entry point. It is no longer a stylistic objection
with a hypothetical victim.

**Promote `...` validation from nice-to-have to correctness.** The general
form — warn on any `...` name the callee never reads — catches this at the
call site, and every wrapper that forwards `...` to an engine has the same
exposure: a typo'd engine argument is currently indistinguishable from an
argument the engine ignores.

Tally delta for §165: claims about the package's own source — 3 raised, 3
confirmed. Claims produced by the harness — 42 raised, 39 false, a 93%
false-positive rate from a single argument-name error. That is the strongest
available argument for auditing a measurement's *mechanism* before believing
its count, which is the rule §132.3 already states and which this section
had to learn twice.

### 170.4 What this changes in the plan

1. The flag half of §130.1 is **done**, not proposed: 235 cells checked, 3
   wrong, 0 missed, with a documented reason `scale_space` cannot be checked
   this way. Retire it from the feature list.
2. **Add a test that runs this audit.** ~10 minutes across 47 engines,
   dominated by `ocd` (289s) and `fcov` (297s) — too slow for `R CMD check`,
   right for `tests/manual/` or a scheduled job. Without it the next engine
   wave reintroduces the class silently, which is exactly how these three
   survived a 41-defect and a 33-defect audit.
3. **The field half of §169 is now the highest-value item in Part V**, and
   for a sharper reason than "more data is nice": all three defects found
   here were already visible in §129's field listing, which recorded
   `bocpd (ocp) 16 fields | not surfaced: r, prevr, prevrprod, prevrsum,
   prevdatapt, time, ocpd_settings, threshcps`. Two audits were looking at
   the same facts from opposite sides and neither was joined to the
   registry. **Join them:** the registry row and the field inventory should
   be one table, so a flag cannot claim a capability no surfaced field
   supports.
4. **Audit every function that builds prose from a capability flag.** Grep
   `subset(cpt_methods(), ` — each hit turns a wrong flag into a wrong
   instruction, and `cpt_solution_path()` shows the failure mode: the error
   message recommends the broken engines because it reads the broken flag.
   A flag that only gates behaviour degrades quietly; a flag that generates
   advice degrades loudly and in the user's face.

# Part V (continued) — the input contract, measured at its edges

Every pass so far has looked at what the package does with data it accepts.
This one looks at the boundary: data it refuses, and data it accepts but
should not. Both turned out to be blind spots of this document as much as of
the codebase — "missing value" appears zero times in the 6,900 lines before
this section, and so do "zero variance" and "stuck sensor".

## 171. Missing values: the gap this document never mentioned

**What the package does.** `validate_data()` rejects any non-finite input
outright, for vectors, matrices and data frames alike:

> `x` must be finite (no NA/NaN/Inf).

There is no `na_action` argument anywhere in the package, `as_uni_vector()`
does no NA handling, and the only `na.rm = TRUE` uses in `R/` are in
plotting and summary code — none in a detection path. So the contract is a
blanket refusal, applied uniformly to all 50 engines, documented as a
validation rule rather than as a decision.

**What the engines actually do.** Calling seven engines directly, on a
200-point series with true changepoints at 70 and 140 and five `NA`s
inserted at positions 30, 31, 32, 95 and 150:

| engine | behaviour with `NA`s present |
|---|---|
| `changepoint::cpt.mean` | hard error |
| `wbs::wbs` | hard error — "x vector cannot contain NA's" |
| `trend::pettitt.test` | hard error — "missing values in object" |
| `ecp::e.divisive` | **succeeds, finds nothing** (returns only the trivial endpoints) |
| `strucchange::breakpoints` | **succeeds, indices silently shifted** |
| `bfast::bfast01` | succeeds, index in the compacted space |
| `Rbeast::beast` | succeeds, index in the **original** space |

Four distinct behaviours, not two. The `strucchange` case is worth the
detail because it is the dangerous one: the same series gives breakpoints
`69, 140` clean and `66, 136` with the five `NA`s — each reported index
reduced by exactly the number of `NA`s preceding it, with no warning. A user
who dropped the rows themselves and passed the result would get the same
silently wrong answer. `ecp` is the other failure mode: it returns a
well-formed result with no changepoints in it, which reads as "nothing
happened" rather than "I could not compute".

And `Rbeast` gets it right — 70 and 141 against truth 70 and 140, in the
original index space — because Rbeast was written for series with gaps.
`bfast` succeeds too, which is unsurprising: BFAST exists because Landsat
and MODIS series are full of cloud gaps. **The blanket rejection therefore
removes, from the two engines specifically designed for gappy data, the
capability they were designed for.**

### 171.1 The design this argues for

The current behaviour is a defensible *default* and a poor *only option*. It
is simultaneously protecting users from `strucchange` and `ecp`, and
disabling `bfast` and `Rbeast`. Those need to be separable.

1. **`na_action = c("error", "omit", "engine")` on `cpt_detect()`, default
   `"error"`** — no change for existing code.
   - `"omit"` drops the missing observations, detects, and **maps the
     returned indices back to the original positions**, which is precisely
     what `strucchange` fails to do. The mapping goes in `$diagnostics` so
     the translation is inspectable, and `$data` keeps the original length
     with `NA` values so plots show the gaps.
   - `"engine"` passes the `NA`s through for engines that document handling
     them, and errors for the rest naming the ones that do.
2. **A registry flag, populated by measurement.** Not `na_tolerant` as a
   logical — the table above shows three outcomes, not two. Something like
   `na_handling ∈ {"reject", "native", "compacts", "silent_loss"}`, with
   `ecp` marked `silent_loss` and excluded from `"engine"` on purpose. The
   values must come from running the engines, not from reading their
   documentation; §170.3 is this pass's own evidence for why.
3. **Say it in a vignette,** because every applied field §148 identified has
   gaps for a different reason: satellite reflectance (cloud), clinical
   series (missed visits), streamflow (sensor outage), and finance — where
   "missing" means *no observation exists*, a genuinely different case that
   should not be interpolated over.
4. **Do not offer `na_action = "impute"`.** Imputing and then detecting
   biases the estimated changepoint location toward the imputed stretch,
   and the bias grows with the gap. If it is ever added it needs that
   sentence next to it; the honest move is to leave it to the user and
   document why.

## 172. Degenerate inputs, and the fit-versus-data split

### 172.1 A three-point series gets a changepoint after every observation

`validate_data()` admits `n >= 3`. Measured at that boundary:

| input | `pelt` result |
|---|---|
| `c(1, 5, 9)` | **2 changepoints, at 1 and 2** |
| `c(0, 10, 20)` | **2 changepoints, at 1 and 2** |
| `c(1, 1, 9)` | 1 changepoint, at 2 — correct |
| `c(3, 3, 3)` | 0 changepoints |

Two changepoints in a three-point series means every observation is its own
segment, which is not an estimate of anything. It happens on a monotone
series, where two steps fit better than one, and nothing stops it:
`cpt_wrapper()`'s `minseglen` formal is `NULL`, so no minimum segment length
is imposed and the engine's own default is whatever it happens to be. The
same inputs give 0 from `binseg` and 1 from `amoc` and `fpop`, so this is
not a property of the problem — it is an unguarded interaction between one
engine's default and a series too short to segment.

**Proposal: a floor.** `minseglen` should default to at least 2 for every
engine that accepts one, and `cpt_detect()` should warn below roughly
`n = 10` that the result is not interpretable rather than returning it
straight-faced. Cheap, and it removes a whole class of nonsense output.

### 172.2 A constant series returns "no changepoints" with no comment

`rep(5, 200)` and `rep(0, 200)` both return zero changepoints, silently.
That is the right number and the wrong message: a series with zero variance
is a different situation from a series where the detector looked and found
nothing, and the user with a stuck sensor gets the answer that means
"nothing happened". This is §93 (Theme BE, "'No changepoints detected' is
not an answer") with a concrete trigger to attach it to — check the variance
before reporting the null result, and say which of the two cases it is.

### 172.3 Post-detection functions disagree about what they take

Measured by handing the same `ggcpt` fit to each:

| takes a fit | takes raw data |
|---|---|
| `cpt_test()`, `cpt_confint()`, `cpt_select()`, `cpt_influence()`, `cpt_leverage()`, `cpt_gt()`, `cpt_report()` | `cpt_stability()`, `cpt_crops()` |

Seven to two, with no signposting. And the rejection says only:

> `x` must be a numeric vector, matrix, or data.frame.

which is true, gives no hint that a `ggcpt` object was what arrived, and
does not name the fix. For a user who has just called six functions on the
fit, the seventh refusing it is a surprise and the message does not resolve
it. Either every post-detection function accepts a fit (extracting the
series itself), or the two that cannot say so explicitly — the first is
better, and it is a small change since the fit already carries
`$data$value`.

A related edge, found by misusing the API in the obvious way:
`as_cpt_series()` returns `list(values, index, index_label)`, and feeding
that list to `cpt_detect()` produces `'list' object cannot be coerced to
type 'double'` from base R. That is user error, but the documentation
invites it — "returns the time index alongside them, so `cpt_detect()` can
detect on positions and report on dates" reads like a pipeline. The
supported route is `cpt_detect(x, index = d)`. A typed error naming that
route costs one line and belongs with the §81 (Theme AW) work.

### 172.4 Where the index contract does and does not hold

Good news first, measured on 200 points with an irregular `Date` index:
`cpt_detect(x, index = d)` warns that the spacing is unequal, adds
`cp_index` to `$changepoints` and `index_value` to `$data`, `cpt_report()`
prints dates rather than positions, and `cpt_confint()` carries the index
through. The changepoints came back as `2020-07-23` and `2021-03-06`.

**`cpt_test()` does not.** It returns `cp` with no `cp_index`, so the one
function whose output a user is most likely to paste into a paper is the one
that reports positions when everything around it reports dates. Add
`cp_index` there, and add a test that asserts every post-detection function
returning a `cp` column also returns `cp_index` when the fit has an index —
this is the same "one contract, checked once" pattern §170.4 asked for on
capability flags.

### 172.5 What this pass adds to the plan

1. `na_action` plus a measured `na_handling` registry column (§171.1) —
   this is the largest genuinely-new capability found in several passes,
   because it is the difference between "your data are not acceptable" and
   an analysis, for every applied field in §148.
2. A `minseglen` floor and a small-`n` warning (§172.1).
3. Zero-variance detection before reporting a null result (§172.2).
4. One input contract for post-detection functions (§172.3).
5. `cp_index` everywhere a `cp` appears (§172.4).
6. **A boundary-input test matrix**, which is what would have caught all of
   the above: `n` in {3, 4, 5, 8, 12}, plus constant, near-constant,
   single-spike, all-zero and monotone series, run across every engine, with
   the assertion that no engine may return a changepoint per observation and
   no engine may return a well-formed empty result where it actually failed.
   Note the shape of this list — five findings, none of them a new
   statistical method, all of them at the boundary of the input contract,
   and none of them mentioned in the 170 sections before it. Breadth of
   ideas was never the constraint.

## 173. The argument vocabulary: 96 names for maybe twenty concepts

§158 and §160 debated a unified `:?` grammar for expressing a detection
problem, and §160 measured that 42 of 50 methods are fully expressible in
it. Neither pass measured the thing the grammar would have to sit on top
of: the argument names the 43 wrappers actually use.

**43 wrappers, 96 distinct argument names, and 71 of those appear in
exactly one wrapper.** Three-quarters of the vocabulary is single-use. Only
five names appear in more than four wrappers — `x` (41), `...` (39),
`seed` (19), `alpha` (9) and `change_in` (6) — and after those the
distribution falls straight into a long tail of hapax legomena.

That is partly unavoidable: `hazard` means something specific to BOCPD and
`npsi` to `segmented`, and renaming an engine's own parameter hides the
literature the user needs to read. But a large share of the tail is the same
concept under different spellings, and the spellings are not guessable:

| concept | names in use | owners |
|---|---|---|
| permutations / replicates | `n_perm` / `nperm` / `mc_reps` / `n_bootstraps` / `n_intervals` | hdcov+network / **kcp** / ocd / taylor / wbs+not+idetect |
| threshold | `threshold` / `thresh` / `cstar` / `critical` | wbs+inspect+hdcov+network / **ocd** / wbsts / fmean+fcov |
| confidence | `conf_level` / `confidence` / `alpha` | strucchange+segmented+taylor / **sn** / nine wrappers |
| how many changepoints | `n_changepoints` / `n_segments` / `max_segments` / `kmax` / `breaks` / `npsi` | mcp / binsegrcpp / binsegrcpp / kcp / strucchange / segmented |
| minimum segment length | `minseglen` / `min_segment_length` / `min_size` / `min_dist` | cpt / cpop / fabisearch / taylor |
| model specification | `model` / `models` / `model_param` / `family` / `distribution` / `type` / `variant` / `cpm_type` / `cp_method` / `statistic` / `test` | eleven wrappers, no two agreeing |

**`n_perm` versus `nperm` is the sharpest case:** the same concept, in two
wrappers, differing by one underscore. No user can guess which engine wants
which, and no error message helps — a wrong spelling lands in `...` and is
silently forwarded or ignored, which is §170.3's failure mode again, now
with 71 opportunities to trip over it. `threshold` versus `thresh` and
`conf_level` versus `confidence` are the same problem in milder form.
`binsegrcpp` has both `n_segments` and `max_segments`; `taylor` has
`conf_level`, `min_conf` **and** `min_candidate_conf`.

### 173.1 What this means for the grammar decision

§160's recommendation was that the `:?` grammar is worth building because 42
of 50 methods fit it. This measurement adds a precondition that pass missed:
**a grammar over an inconsistent vocabulary just moves the inconsistency one
level up.** If `n_intervals` and `nperm` and `mc_reps` remain three names,
then `wbs:?` and `kcp:?` and `ocd:?` expand into three different parameter
spellings and the user still has to know which. The grammar is worth
building *after* the vocabulary is consolidated, not instead of it.

### 173.2 The consolidation, and how to do it without breaking anything

1. **Pick one canonical name per concept** and use it as the documented
   argument: `n_perm`, `threshold`, `conf_level`, `min_seg_len`, `max_cp`.
   Prefer the name already most common (`alpha` for significance, with
   `conf_level` reserved for interval width — those really are two
   concepts, and collapsing them would be wrong).
2. **Keep every current name as a deprecated alias**, not as a rename. The
   engine-specific literature names (`cstar`, `npsi`, `hazard`, `lambda`)
   stay, aliased to the canonical one where they mean the same thing, so
   nobody reading Fryzlewicz has to translate.
3. **Reject unknown names.** The alias table makes this possible: once every
   accepted spelling is enumerated, anything else in `...` is a typo and can
   be flagged rather than forwarded. This is §170.3's correctness item and
   this section's usability item closing on the same mechanism — build the
   alias table and both are fixed at once.
4. **A test that enumerates the vocabulary** and fails when a new wrapper
   introduces a name for a concept that already has one. Without it the next
   engine wave adds nineteen more single-use names, which is exactly how the
   current 71 accumulated.
5. **Document the tail honestly.** `cpt_methods()` could gain a
   `parameters` column listing each engine's own arguments, so the long tail
   becomes discoverable instead of something you find by reading 43 help
   pages. That is a smaller and more certain win than the grammar.

## 174. The on-ramp: what a new user can actually run

§141 measured which exports the test suite never runs and §"doc coverage"
asserts that every export is *mentioned* in the README. Neither measures the
thing a new user meets first: an example they can paste, and a vignette that
walks them through it. Measured across all 130 exports, six vignettes and
the full `man/` tree:

| | count |
|---|---|
| exports with at least one runnable example | 94 |
| exports whose `.Rd` has **no `\examples{}` block at all** | **31** |
| exports whose examples are entirely `\dontrun`/`\donttest` | 5 |
| exports mentioned in at least one vignette | 111 |
| exports in no vignette | 19 |

**The 31 with no example are not the obscure corners.** They include seven
engine wrappers users are likely to reach for by name — `wbs_wrapper`,
`wbs2_wrapper`, `not_wrapper`, `mosum_wrapper`, `tguh_wrapper`,
`idetect_wrapper`, `fpop_wrapper` — the whole `signal_*` family
(`signal_blocks`, `signal_fms`, `signal_mix`, `signal_stairs`,
`signal_teeth`, the five standard test signals, which are exactly the things
someone would want a one-liner for), all five tidyverse/broom entry points
(`tidy`, `glance`, `augment`, `autoplot`, `as_tibble`), three geoms
(`geom_changepoint`, `geom_cpt_ci`, `geom_cpt_segment`) plus
`stat_changepoint`, and `is_ggcpt`, `new_ggcpt`, `cpt_update`,
`cpt_leverage`, `ggcpt_eval`, `ggcpt_compare_table`, `annotate_segments`,
`cpt_metrics_annotated`, and the two label scales. Verified by hand:
`man/wbs_wrapper.Rd` is 27 lines with zero `\examples` blocks and is its own
page, not an alias on a shared one.

The five fully-gated ones — `cpt_benchmark`, `cpt_load_tcpd`,
`cpt_min_detectable`, `cpt_power`, `mcp_wrapper` — are each defensible
individually (network, runtime, JAGS), but note what the list is: **the two
power-analysis functions and the benchmark are the package's most
distinctive capabilities and none of them has an example a user can run.**
§3613 already observed the irony that `cpt_power()` and
`cpt_min_detectable()` exist and answer the question nobody asks; part of
the reason nobody asks is that the help page shows them only inside
`\donttest`. A fast variant — `n_sim = 20` with a note that the published
default is 200 — would make both runnable in under a second.

**The vignette gap is mostly defensible and has one real hole.** Of the 19
exports in no vignette, 14 are engine wrappers reached through
`cpt_detect(method = )`, which is the intended route. But
`cpt_registered_methods()` is absent from `extending.Rmd`, which calls
`cpt_register_method()` four times: the vignette teaches registration and
never teaches discovery, so a reader who registers three methods has not
been shown how to list them. `ggcpt_statistic()` and `scale_color_cpt` (the
American spelling alias) are the other two.

### 174.1 What to do

*Items 1 and 3 of this list shipped in the 0.5.0 documentation pass: 125 of
130 exports now carry a runnable example (the five re-exported generics stay
bare, which is correct), and `cpt_registered_methods()` executes in a vignette
chunk. Deleted from the list. What remains:*

1. **Un-gate the power functions** with a small-`n_sim` example, so
   `cpt_power()` and `cpt_min_detectable()` — the package's most distinctive
   capability — have something a reader can run.
2. **Extend the existing doc-coverage test** from "is it mentioned in the
   README" to "does it have a runnable example", which is a stronger and
   equally mechanical assertion. Without it the coverage just gained
   decays on the next engine wave, exactly as §231.2 predicts.

## 175. 220 conditions, none of them typed — and 207 tests that pin the prose

§81 (Theme AW) argued for typed conditions so callers can branch on the
*kind* of failure instead of grepping a message. Here is the size of it.

| | count |
|---|---|
| `stop()` calls in `R/` | 189 |
| `warning()` calls | 23 |
| `message()` calls | 8 |
| **conditions signalled, total** | **220** |
| of those carrying a condition class | **0** |
| `rlang::abort()` / `errorCondition()` / `warningCondition()` | 0 |
| `call. = FALSE` uses (good hygiene) | 207 |

So the package signals 220 conditions and every one of them is a bare
string. A caller wrapping `cpt_detect()` in `tryCatch()` — a Shiny app, a
batch pipeline, another package — cannot tell "this engine is not installed"
from "your series has an NA" from "this method is univariate" except by
matching English. `rlang` is not a dependency and does not need to become
one: base `errorCondition()` has been available since R 3.6, and the package
declares no `Depends: R (>= ...)` floor at all, so adding one is free.

### 175.1 The part that makes this urgent rather than tidy

I expected the test suite to be weak here and it is the opposite: of 213
`expect_error()` calls, **207 check the message text** and only 6 are bare.
21 `expect_warning()` calls, 16 checking text. Zero use `class =`.

That is a thorough suite, and it produces a specific trap: **207 tests pin
user-facing prose, so improving an error message is a test-breaking
change.** Every message improvement proposed in the last four sections runs
into it —

- §170.1: `cpt_solution_path()`'s error should stop naming the broken
  engines.
- §171.1: the NA rejection should say which engines could have handled it.
- §172.3: `cpt_stability()`'s rejection should say a fit arrived and name
  the fix.
- §174: nothing, but the same applies to every "say more" suggestion in the
  document.

Each of those is now a two-part change: edit the message, then find and edit
the test that quotes it. That friction is invisible in a coverage number and
it is exactly the friction that keeps error messages bad. **Typed conditions
dissolve it:** once a test asserts `class = "ggcpt_error_no_solution_path"`,
the prose is free to improve without touching the test, and the assertion
gets *stronger* rather than weaker because it pins the failure's identity
instead of its wording.

### 175.2 The migration, ordered so it never breaks the suite

1. **Add a small internal helper** — `cpt_abort(msg, class, ...)` wrapping
   `stop(errorCondition(msg, class = c(paste0("ggcpt_", class), "ggcpt_error"), ...))`
   — and a `Depends: R (>= 3.6.0)` line. One function, no new dependency.
2. **Convert the taxonomy first, not all 189 call sites.** The classes worth
   having are few: `not_installed`, `bad_input`, `wrong_dimension`,
   `unsupported_change_in`, `capability_absent`, `engine_failed`,
   `no_changepoints`. Seven classes cover the great majority of the 189.
3. **Keep every message identical during the conversion**, so all 207 tests
   keep passing and the change is provably behaviour-preserving.
4. **Then add `class =` to the tests**, in the same commit as each message
   improvement — the test gets a class assertion and loses its prose
   assertion at the moment the prose changes.
5. **Attach data to the conditions**, not just a class: `engine`, `method`,
   `n`, `requested`. That is the thing a Shiny app actually wants — enough
   structure to render its own message — and it costs nothing once
   `errorCondition()` is in place.
6. **Document the classes**, because an unlisted condition class is as
   undiscoverable as an unlisted `...` argument (§173.2). A short table in
   the extending vignette, and `?ggchangepoint-conditions`.

This is the cheapest large-surface improvement identified in this document:
no new statistics, no new engine, no dependency, and it unblocks four other
items that are currently each carrying their own test-editing tax.

## 176. Runtime is a capability too, and the recommender guesses at it

`cpt_recommend()` already knows cost matters. At `consensus.R:374-395` it
penalises a `slow` list by 2 points, rewards a `fast` list by 1, and warns
about a `heavy` list that returns oversized fit objects — all gated on
`n >= 5000`. Good instinct. The lists are hand-written, and this document has
now established twice (§170 for capability flags, §132.3 before it) what
happens to a hand-written claim about fifty engines.

**Measured wall clock, n = 200, two mean shifts, on the machine that runs
the checks:**

| engine | time | in the `slow` list? |
|---|---|---|
| `fcov` | **296.7 s** | no |
| `ocd` | **288.8 s** | no |
| `fmean` | 12.6 s | no |
| `network` | 8.2 s | no |
| `kcp` | 5.4 s | yes |
| `var` | 3.5 s | no |
| `inspect` | 1.3 s | no |
| `ecp` | 0.7 s | yes |
| `bcp` | 0.2 s | yes |
| ~30 others | < 1 s | — |

The spread is roughly **1500×** between the fastest and the slowest, on two
hundred data points.

Two findings, and I want to keep them separate because only one is a defect:

1. **The `slow` list contains `"changepoints"`, which is not a method
   name.** No registry row matches it, so that entry has never done
   anything. `fast` and `heavy` check out — every entry in those matches a
   real method. One dead string out of fifteen.
2. **The four slowest engines measured — `fcov`, `ocd`, `fmean`,
   `network` — appear in none of the three lists.** So
   `cpt_recommend(dimension = "multivariate", n = 1e6)` still returns
   `fmean` and `ocd` in its top five, with no caveat, when `fcov` and `ocd`
   need five minutes at n = 200. Whatever they need at a million points, the
   recommender is not the place to find out.

In fairness the lists encode *asymptotic class* ("quadratic or
sampling-based"), not wall clock at n = 200, and by that criterion including
`ecp` and `bcp` is right even though both finish in under a second here.
The criterion is defensible; the coverage is not, and the same three-line
`c(...)` cannot express both "quadratic" and "constant-factor enormous",
which is what `ocd` and `fcov` are.

### 176.1 What to build

1. **Delete `"changepoints"`** and add a test asserting every name in every
   one of these hand-written lists resolves to a registry method. This is
   the §170.4 pattern a third time: any hardcoded list of method names is a
   claim about the registry and should be checked against it. Grep for
   `c("pelt"` and `%in% c(` across `R/` — each hit is the same exposure.
2. **A measured `cost` column in the registry**, with the two axes kept
   apart: `complexity` (a symbol: `linear`, `n_log_n`, `quadratic`,
   `sampling`) and `constant` (a measured seconds-at-n-1000 figure). The
   first comes from the literature; the second has to be measured, and it is
   the one that catches `ocd` and `fcov`.
3. **Populate it from a benchmark script, not by hand** — the same
   `tests/manual/` slot §170.4 asked for, timing every engine at
   n ∈ {200, 1000, 5000} and writing the table. That run also gives the
   package its first honest answer to "will this finish?", which is the
   question a new user has before any statistical one.
4. **Surface it in `cpt_methods()`**, which today carries nine capability
   columns and nothing about cost, and in `cpt_recommend()`'s output, which
   has `score`, `why` and `caveat` but no time estimate. A predicted
   wall-clock at the user's `n` is more useful than a `-2` on a score.
5. **Warn before a long run, not after.** `cpt_detect()` knows the method
   and `n` before it starts; if the estimated time exceeds a threshold it
   can say so once. That is the difference between a user waiting five
   minutes and a user pressing Ctrl-C at ninety seconds and concluding the
   package is broken.

## 177. The field inventory §169 demanded, run over 45 engines

§169 asked for the pass: "for each of the fifty engines, list what its
return value contains and decide, per field, keep or discard — deliberately,
once, recorded in the registry." §129 did it by sampling. This is the whole
inventory.

**Method.** Detect with every registry method whose engine package is
installed, take the raw engine object off `$fit` (slots for S4, names for
lists), and for each field record its class, its size, and a classification:

- **echo-of-input** — numerically identical to the data passed in;
- **bookkeeping** — a scalar, a string, or a function: settings, not results;
- **minor** — short vectors, single-column frames;
- **substantive** — anything length `n` or longer, or a matrix or list with
  real structure.

Then ask whether the package reads it. Two proxies, and the difference
between them is instructive:

1. the field name appears in the *wrapper's* source;
2. the field name appears anywhere in `R/` in extraction position
   (`$field` or `[["field"]]`).

**Scale.** 45 engines, **390 fields**.

| classification | not read by the wrapper | read |
|---|---|---|
| bookkeeping | 114 | 53 |
| minor | 64 | 54 |
| substantive | **48** | 34 |
| echo-of-input | 8 | 10 |
| empty | 3 | 2 |

Under the stricter, fairer proxy — extraction anywhere in `R/`, not just in
the wrapper — of **82 substantive fields, 37 are consumed and 45 are never
extracted anywhere, across 20 engines.**

### 177.1 First correction: §129's method overcounted, by eight

Eight fields that look discarded from the wrapper are in fact consumed by a
downstream function — `cpt_statistic()`, `cpt_solution_path()` and friends
read them out of `$fit` themselves:

`fpop$path`, `fpop$cost`, `not$solution.path`, `mosum$stat`, `beast$time`,
`segmented$model`, `esac$coordinate`, `bfast$jump`.

So the "not surfaced" lists in §129 were an over-estimate of the loss, and
the correct denominator is 82 substantive fields with 45 genuinely unread.
That is still 55% of everything substantive the engines compute, but the
number to quote is 45, not 48 and not §129's larger sample-based figure.

### 177.2 The discarded substantive fields, largest first

| method | engine | field | class | size |
|---|---|---|---|---|
| `not` | not | `contrasts` | data.frame | 10000x5 |
| `strucchange` | strucchange | `RSS.table` | matrix | 141x10 |
| `bcp` | bcp | `blocks` | numeric | 550 |
| `bocpd` | ocp | `logprobcps` | list | list[202] |
| `bocpd` | ocp | `logprobmaxes` | numeric | 202 |
| `bocpd` | ocp | `prevR`, `prevRprod`, `prevRsum` | numeric | 201 each |
| `bocpd` | ocp | `currmu` | list | list[200] |
| `binsegrcpp` | binsegRcpp | `subtrain.borders` | numeric | 201 |
| `mosum` | mosum | `rollsums`, `var.estimation` | numeric | 200 each |
| `bcp` | bcp | `posterior.var` | matrix | 200x1 |
| `npmojo` | CptNonPar | `test.stat` | numeric | 200 |
| `sn` | SNSeg | `SN_sweep_result` | list | list[200] |
| `geomcp` | changepoint.geo | `angle` | numeric | 200 |
| `segmented` | segmented | `residuals`, `effects`, `fitted.values`, `id.group` | — | 200 each |
| `fastcpd` | fastcpd | `residuals` | matrix | 200x1 |
| `kcp` | kcpRS | `CPs_given_K`, `scree_test` | data.frame | 11x12, 9x2 |
| `strucchange` | strucchange | `RSS.triang` | list | list[171] |
| `envcpt` | EnvCpt | `summary` | matrix | 2x12 |
| `var` | changepoints | `K_hat`, `train_error` | matrix | 3x3 each |
| `decafs` | DeCAFS | `costFunction`, `modelParameters` | — | 4x6, list[3] |

**`strucchange$RSS.table` at 141x10 is the item §166 and §167 spent a whole
pass identifying.** The Bai-Perron reporting template needs RSS and BIC by
number of breaks; `strucchange` computes it, hands it back, and the wrapper
never touches it. That prediction is now confirmed by direct inspection
rather than inferred from the engine's documentation.

### 177.3 Second correction, and it revises §170's headline

§170 reported **0 false negatives** across 199 negative capability claims.
That number is correct for what it measured — whether an *accessor* returns
something — and wrong as a statement about capability, because an accessor
that was never written cannot return anything. Cross-referencing the
discarded fields against the flags finds four capabilities the registry does
not claim and the engine does compute:

| method | discarded field | shape | flag | claimed |
|---|---|---|---|---|
| `npmojo` | `test.stat` | length-`n` per-location statistic | `statistic` | **FALSE** |
| `bocpd` | `currmu` | length-`n` online running mean | `fitted` | **FALSE** |
| `strucchange` | `RSS.table` | RSS by number of breaks | `path` | **FALSE** |
| `envcpt` | `meanar1`, `meanar2`, `trendar1`, `trendar2` | fitted models from the 8-model ensemble | `fitted` | **FALSE** |

So the corrected reading of the two audits together: **the flags are honest
about what the package exposes and silent about what the engines provide.**
§170's three overclaims and these four underclaims are the same defect —
nothing joins the registry row to the returned object — seen from opposite
sides. Neither audit alone could see both.

`bcp$posterior.var` and `bocpd$logprobcps`/`logprobmaxes` are the same story
inside flags that are already `TRUE`: `bcp` supplies a posterior *mean*
profile and a posterior *variance* that nobody reads, and `bocpd`'s log
changepoint probabilities sit next to the `R` matrix that §170.2 found
`ggcpt_runlength()` does render.

### 177.4 The decision, per field, which is what §169 actually asked for

Not everything on that list should be surfaced. Making the judgment is the
point:

**Surface — these answer questions users already ask.**
`strucchange$RSS.table` (the Bai-Perron table, §167);
`npmojo$test.stat` and `mosum$rollsums`/`var.estimation` (statistic
profiles, and `npmojo` gains a capability flag);
`bcp$posterior.var` (per-location posterior uncertainty, which is the
natural companion to a posterior mean);
`bocpd$currmu` (an online fitted signal — the only engine that could give a
*causal* fitted mean, computed from data up to `t` alone);
`kcp$CPs_given_K` and `scree_test` (`cpt_select()` currently recomputes
K-selection that `kcpRS` already did);
`envcpt$summary` (the 8-model comparison is the whole reason to use EnvCpt);
`not$contrasts` and `strucchange$RSS.triang` **on request only** — 10000x5
and list[171] at n = 200, so these must be opt-in, not attached by default.

**Discard, deliberately, and record the decision.**
`segmented`'s `residuals`/`effects`/`fitted.values`/`qr`/`model`/`id.group`
are standard `lm` machinery reachable from `$fit` by anyone who wants them;
`fastcpd$residuals` likewise. `bocpd$prevR`/`prevRprod`/`prevRsum` are
internal recursion state. `binsegrcpp$subtrain.borders` is a
cross-validation artefact, not the splits table §170.1 needs.
`geomcp$angle` is one of the two geometric mappings and belongs with
`distance`, so it is surface-both-or-neither.

**The mechanism, which is the part that lasts.** A `fields` table in the
registry — one row per engine field, with `keep`/`discard` and a one-line
reason — turns all of this from an audit finding into a checked invariant.
Then: a test asserting every field of every fit appears in that table, so a
new engine cannot be wired without someone deciding, once, what its return
value is worth. That is the single change that would have prevented §129,
§170 and this section from being three separate discoveries of the same
thing.

## 178. What this pass changes

Six sections, all measurement, no new statistics:

| § | finding | status |
|---|---|---|
| 171 | `NA` rejected blanketly; four distinct engine behaviours measured, two engines built for gaps disabled | new capability: `na_action` + measured `na_handling` column |
| 172 | 3-point series gets a changepoint per observation; constant series answers silently; 7-vs-2 fit/data API split; `cpt_test()` drops `cp_index` | five small fixes plus a boundary-input test matrix |
| 173 | 96 argument names across 43 wrappers, 71 single-use; `n_perm`/`nperm` | alias table, which also enables `...` validation |
| 174 | 31 exports with no example; the two power functions gated | one runnable example each; extend the doc-coverage test |
| 175 | 220 conditions, 0 typed; 207 tests pin the prose | `cpt_abort()` + 7 classes; unblocks four other items |
| 176 | 1500x runtime spread; `slow` list misses the four slowest and contains a dead name | measured `cost` columns; delete `"changepoints"` |
| 177 | 45 of 82 substantive fields never read; 4 flag underclaims; `RSS.table` confirmed | a `fields` table in the registry, checked by a test |

**The pattern across all six.** Every one was found by running the package
against an input or a question it was not built for, and none required
reading a paper or searching CRAN. §169 said the dominant defect class is
discard rather than absence; this pass says something narrower and more
useful: **the dominant defect class is an unchecked claim.** A capability
flag, a cost list, an argument name, an error message, a `...` argument, a
field decision — each is an assertion the package makes about itself, and
none of the six had anything checking it. The fixes are correspondingly
uniform, and §177.4's mechanism generalises to all of them: enumerate the
claim in a table, and add one test that the table matches reality.

**Two corrections this pass made to earlier passes, recorded so the tally
stays honest:** §129's discard count was 8 fields too high (§177.1), and
§170's "0 false negatives" measured accessor coverage rather than capability
(§177.3). Both were errors of scope, not of fact, and both were found by
running a second measurement against the first — which is now three passes
in a row where the most valuable output was checking the previous pass.

# Part V (continued) — the invariances nobody checked

A detector should give the same answer when the same series arrives in
different units. Nothing in this document, and nothing in the test suite,
had ever asked whether it does.

## 179. Multiply your data by ten and the default method returns 139 changepoints

**Method.** For every registry method whose engine is installed, detect on
the same 200-point series with two mean shifts three times: as `x`, as
`10 * x`, and as `0.1 * x`. A correct mean-change detector must return the
same changepoint set all three times — the shift-to-noise ratio is
identical.

**Result: 42 engines tested, 34 scale-invariant, 8 scale-sensitive.** The
eight, with the actual changepoint counts:

| method | engine | `x` | `10x` | `0.1x` |
|---|---|---|---|---|
| **`pelt`** | changepoint | 70, 140 | **139 changepoints** | **none** |
| **`fpop`** | fpop | 70, 140 | **135 changepoints** | **none** |
| `binseg` | changepoint | 70, 140 | 5 changepoints | none |
| `segneigh` | changepoint | 70, 140 | 70, 120, 129, 140 | none |
| `amoc` | changepoint | 70 | 70 | none |
| `envcpt` | EnvCpt | 70, 140 | 68, 138 | 70, 140 |
| `kwc` | KWCChangepoint | 44, 61, 140 | 61, 140 | 61, 140 |
| `geomcp` | changepoint.geo | 70, 140, 142 | 70, 140, 142 | 70, 140 |

`wbsts` failed at all three scales, which §170 already recorded for other
reasons.

**`pelt` is the package default.** `cpt_detect()`'s signature is
`method = "pelt", change_in = "mean", penalty = "MBIC"`, so the path a first
-time user takes is the worst offender in the table: the same data in
millimetres instead of metres returns 139 changepoints instead of 2, and in
kilometres returns none. Nothing warns. Every vignette example and every
README snippet uses this default.

### 179.1 The mechanism, and the one-line proof

`changepoint::cpt.mean()` with `test.stat = "Normal"` uses a Gaussian cost
with **variance assumed known and equal to 1** — that is upstream's
documented behaviour, and `fpop` makes the same assumption. Scaling the
series by 10 multiplies the cost by 100 while the MBIC penalty stays
proportional to `log n`, so the penalty becomes negligible and the segmenter
splits everywhere; scaling down by 10 makes the penalty dominate and it
splits nowhere.

The proof is that estimating the variance fixes it:

| `change_in` | on `x` | on `10x` |
|---|---|---|
| `mean` | 70, 140 | **139 changepoints** |
| `meanvar` | 70, 140 | **70, 140** |
| `var` | none | none |

So the defect is not in the search algorithm and not in the penalty — it is
that `change_in = "mean"` selects a known-variance cost and the package
neither standardises the input nor says that it matters. Note also the third
row: `change_in = "var"` returns zero changepoints, silently, on a series
with an obvious mean shift and constant variance. That is arguably the right
number, but it is reported the same way as "I looked and found nothing",
which §172.2 already flagged as a distinct failure to communicate.

### 179.2 Why the test suite could never have caught it

Every test and example in the package generates data with `rnorm()`, so the
series always has unit variance, and a known-variance cost is exactly
correct there. The bug is invisible to any test whose data are standard
normal — which is all of them. This is the most instructive property of the
finding: **2,590 passing tests and 85.79% expression coverage do not
constrain the answer's dependence on the units of the input, because
coverage measures which lines ran, not which invariances hold.**

That is the general lesson, and it is the argument for a category of test
the package has none of: **metamorphic tests.** Instead of asserting an
expected output, assert a relationship between two outputs — same data
scaled, shifted, reversed, or duplicated:

| transformation | what must hold | catches |
|---|---|---|
| `x -> a*x`, `a > 0` | identical changepoints for mean/var detectors | this section |
| `x -> x + b` | identical changepoints for every detector | offset bugs in cost functions |
| `x -> rev(x)` | changepoints at `n - cp` | asymmetric search / boundary handling |
| `x -> c(x, x)` | changepoints at `cp` and `cp + n` | penalty scaling with `n` |
| `x -> x[rep(seq_len(n), each = 2)]` | changepoints at `2*cp` | resolution dependence (§99) |

Five properties, checkable across all 50 engines in one test file, each one
a statement a user would assume without being told. §95 (Theme BG) asked
whether the package's own inference behaves; this is the concrete,
mechanical form of that question, and the first transformation already found
eight engines.

### 179.3 What to do about the scale problem itself

Fixing the tests does not fix the default. Four options, and I would take
the third:

1. **Standardise silently** inside the `mean`-cost wrappers — divide by a
   robust sd, detect, report positions. Positions are what the contract
   promises, so this is behaviour-preserving for the user. But it changes
   answers between versions, and it hides a real modelling choice.
2. **Switch the default to `change_in = "meanvar"`.** Scale-invariant and
   correct, but it changes what the flagship default *estimates*, and a
   variance-changing segmentation is not the same question as a
   mean-changing one.
3. **Warn, and record the decision in the registry.** Add a measured
   `scale_invariant` column — populated by the metamorphic test above, not
   by hand, given §176's dead `"changepoints"` entry — and have
   `cpt_detect()` emit one warning when a scale-sensitive engine receives a
   series whose sd is far from 1, naming `change_in = "meanvar"` and
   standardisation as the two fixes. This keeps the default's meaning,
   makes the trap discoverable at the moment it bites, and costs one
   registry column plus one `if`.
4. **Document it only.** Cheapest, and the weakest: the users most exposed
   are the ones least likely to read the "Notes" section of a help page,
   because they are handing the package data in whatever units it came in.

Option 3 also composes with §171's `na_handling` and §176's `cost` columns:
three measured registry columns, each populated by a script rather than by a
hand-written list, each surfaced through `cpt_methods()`. That is the same
mechanism §177.4 proposed for fields, applied a third time, which is now
enough repetition to call it the plan rather than an idea.

## 180. The `seed` argument is reproducible and quietly destroys simulations

**What works.** Seed plumbing is sound. Across 43 engines, every one returns
byte-identical changepoints on two consecutive identical calls — **zero
cases of "same seed, different answer."** Eighteen wrappers take a `seed`
argument and all eighteen honour it with `if (!is.null(seed)) set.seed(seed)`.
A seeded call is reproducible even across a thousand intervening random
draws. Verified.

**What that `set.seed()` costs.** It is called in the wrapper's own frame, so
it mutates the caller's global RNG state — it does not merely consume the
stream, it *resets* it. Measured, with the data pre-generated so that data
generation cannot be mistaken for the effect:

| call | caller's RNG stream preserved? |
|---|---|
| `cpt_detect(X, method = "pelt")` — deterministic, no `seed` formal | **yes** |
| `cpt_detect(X, method = "wbs")` — stochastic, `seed = NULL` | no (engine draws; unavoidable) |
| `cpt_detect(X, method = "wbs", seed = 42)` | **no — stream reset to seed 42** |
| `cpt_detect(X, method = "nsp", seed = 42)` | no |
| `cpt_detect(X, method = "bcp", seed = 42)` | no |
| `cpt_stability(X, B = 20, seed = 7)` | no |
| `cpt_power(n = 80, jump = 2, n_sim = 5)` | no |

Rows 2 and 3 look the same and are not. A stochastic engine consuming random
numbers is correct behaviour. Resetting the user's stream to a value they
supplied for a different purpose is not.

### 180.1 The failure, in six lines

```r
set.seed(2026)
for (i in 1:6) {
  d <- c(rnorm(100), rnorm(100, 3))
  f <- cpt_detect(d, method = "wbs", seed = 1)
}
```

Measured `d[1]` across the six iterations:

```
0.520589, -1.516373, -1.516373, -1.516373, -1.516373, -1.516373
distinct datasets generated: 2 of 6
```

**Iterations 2 through 6 analyse the identical dataset.** Iteration 1's
`set.seed(1)` pins the stream, so every later `rnorm()` call starts from the
same place and regenerates the same series. The same loop without `seed = 1`
produces 6 distinct datasets; wrapped in `withr::with_seed(1, ...)` it
produces 4 of 4 distinct. Nothing warns, no test fails, and the study
silently has a sample size of one.

The exposure is worst exactly where it matters most: `cpt_power()`,
`cpt_stability()`, `cpt_benchmark()` and `cpt_sensitivity()` are the
functions a user calls inside a simulation loop, and `benchmark.R:90` has the
same `set.seed(seed)` in it. A user following the obvious advice — "pass a
seed so it's reproducible" — gets a reproducible answer to a question they
did not ask.

### 180.2 The fix, which is one line per wrapper and no new dependency

`withr` is already in `Suggests` (added during the 0.5.0 audit), and
`withr::with_seed(seed, expr)` saves `.Random.seed`, sets it, evaluates, and
restores. The base-only equivalent is eight lines and worth writing so this
does not depend on a suggested package inside a detection path:

1. **An internal `with_local_seed(seed, expr)`** that saves `.Random.seed`
   (handling the case where it does not yet exist), calls `set.seed(seed)`,
   evaluates, and restores on exit via `on.exit()`. No dependency, works
   when `withr` is absent.
2. **Replace all 35 `if (!is.null(seed)) set.seed(seed)` sites** with it,
   plus `benchmark.R:90` and any other Monte Carlo entry point. Mechanical,
   and grep finds every one.
3. **A test that is the loop above** — assert that N iterations of
   generate-then-detect-with-a-fixed-seed produce N distinct datasets. This
   is a metamorphic test in §179.2's sense: it asserts a relationship
   between calls, not a value, and it is the only kind of test that could
   have caught this.
4. **Say it in the docs.** `@param seed` currently reads as "for
   reproducibility"; it should say the seed is scoped to the call and does
   not affect the caller's stream — which will be true once (1) is done, and
   is a documentation bug until then.
5. **While there: `parallel = TRUE` is the default** on `cpt_power()` and
   `cpt_stability()`. Under `future`, `future.seed` governs worker streams
   and the local `set.seed()` does not reach them, so the sequential and
   parallel paths may not agree even with a seed fixed. §101 flagged
   parallel reproducibility as a hazard; this is the specific pair to test —
   same seed, `parallel = TRUE` versus `FALSE`, assert identical output.

This is the highest severity-to-effort ratio in the document. It is
silent, it corrupts results rather than crashing, it lives in the argument
whose entire purpose is trustworthiness, and the fix is a mechanical
substitution at 35 known sites.

## 181. Three ways the input contract is wider than it claims

### 181.1 `change_in` is accepted but never checked against `supports`

The registry's `supports` column is accurate about what it claims: across 42
engines, **zero** entries claim a `change_in` the engine then fails on. But
the check does not run in the other direction — **eight engines silently
accept `change_in = "mean"` while claiming no support for it:**

| method | claims | also accepts |
|---|---|---|
| `np`, `ecp`, `npmojo`, `geomcp` | `distribution` | `mean` |
| `cpop`, `segmented` | `slope` | `mean` |
| `hdcov` | `covariance` | `mean` |
| `kwc` | `covariance`, `distribution` | `mean` |

A user who writes `cpt_detect(x, method = "ecp", change_in = "mean")` gets a
result, no warning, and a *distribution* changepoint analysis. The argument
was read, validated against the global level list, and then ignored — so the
user's stated intent and the computation disagree, silently, and the result
object records `change_in = "mean"` as though it were honoured. Fix:
validate `change_in` against the engine's own `supports` and error naming
what the engine does support. The registry data already exists; nothing
consults it.

### 181.2 A transposed matrix produces confident nonsense

Multivariate engines expect `n x p` — observations down the rows. Handed the
same data as `p x n` (a 6 x 200 matrix: 200 "variables", 6 "observations"),
here is what nine engines do:

| engine | 6 x 200 input |
|---|---|
| `esac`, `pilliat`, `fastcpd` | **accept silently, report 5 changepoints** |
| `inspect` | **accepts silently, reports 3 changepoints** |
| `geomcp` | **accepts silently, reports 1 changepoint** |
| `ecp`, `npmojo`, `kwc` | accept silently, report 0 |
| `hdcov` | errors — `no applicable method for 'thresholdBS'` |

Seven of nine accept it. Five of those fabricate changepoints in a
six-observation series. The one engine that refuses does so with an upstream
S3-dispatch message that says nothing about orientation.

Transposition is one of the most common data-shaping mistakes in R, and it
is cheap to catch: **warn when `ncol(x) > nrow(x)`** for a multivariate
engine — an honest series almost never has more variables than observations
outside the explicitly high-dimensional engines, and for those the check can
be a note rather than a warning. `validate_data()` already sees the matrix
and already enforces `nrow >= 3`; the orientation heuristic belongs in the
same place. Combined with §172.1's `n >= 10` warning, one function gains two
`if`s and the package stops answering questions about six-point series.

### 181.3 `validate_data()` is bypassed for non-numeric input

Called directly, `validate_data()` correctly rejects a list, a factor and a
character vector: "`x` must be a numeric vector, matrix, or data.frame."
Called through `cpt_detect()`, all three are **accepted**:

| input | `cpt_detect(x, "pelt")` |
|---|---|
| `as.list(x)` | accepted, cp = 70, 140 |
| `as.character(x)` | accepted, cp = 70, 140 |
| `factor(round(x))` | accepted, cp = 70, 140 |

The reason is ordering: `cpt_detect()` routes the input through
`as_cpt_series()` first, which calls `as.numeric()` on the values, so
validation never sees the original class. For a list and a character vector
the coercion is faithful and the answer is right — arguably a convenience.

**For a factor it is not faithful.** `as.numeric()` on a factor returns the
integer *level codes*, not the labels' numeric values. Measured on a factor
with levels `"10", "2", "33"` in that order, present in three blocks:

- reported changepoints: 70, 140 — correct, because a monotone recoding
  preserves the block structure;
- the values the result carries are the codes 1, 2, 3, not 10, 2, 33.

So the locations survive and the magnitudes do not: `cp_value`,
`param_estimate`, the fitted signal and every plot report a change from 1 to
2 where the data say 10 to 2 — including the *direction*, since the codes
are ordered and the labels are not. A user with a factor-encoded
categorical series gets a plausible plot of the wrong quantity.

Fix, in order of preference: **reject factors** with a message naming
`as.numeric(as.character(x))` as the fix — a factor is not a numeric series
and silently guessing is worse than refusing; keep the list and character
coercions but say so in the `@param x` documentation, since they are
currently undocumented behaviour that a future refactor would break without
anyone noticing. And move the `validate_data()` call before the coercion, or
have `as_cpt_series()` validate the class it received, so the two functions
stop disagreeing about the contract.

## 182. What this pass changes

| § | finding | measured how |
|---|---|---|
| 179 | 8 of 42 engines are scale-sensitive; the **default** `pelt`/`mean` returns 139 changepoints on `10x` and none on `0.1x` | detect on `x`, `10x`, `0.1x` |
| 180 | `seed =` resets the caller's RNG; a 6-iteration simulation loop analyses **2 distinct datasets** | pre-generated data, RNG stream compared |
| 181.1 | 8 engines accept a `change_in` they do not support, silently | every `supports` level x every engine |
| 181.2 | 7 of 9 multivariate engines accept a transposed matrix; 5 fabricate changepoints | 6 x 200 input |
| 181.3 | `validate_data()` bypassed; a factor becomes its level codes | direct vs routed validation |

New actions, in severity order: a local-seed helper at 19 sites (§180.2); a
measured `scale_invariant` registry column plus one warning (§179.3); a
metamorphic test file covering five invariances (§179.2); `change_in`
validated against `supports` (§181.1); orientation and small-`n` warnings in
`validate_data()` (§181.2, §172.1); factors rejected (§181.3).

**What the pass says about method.** Four of the five findings are
*invariance* violations — the answer changes when something changed that
should not matter (units, RNG state, argument spelling, matrix orientation).
None is reachable by a test that asserts an expected value, which is why
2,590 passing tests and 85.79% coverage did not constrain any of them.
§178 said the dominant defect class is an unchecked claim; this pass narrows
it further: **the claims that go unchecked longest are the ones nobody
thinks to state.** Nobody wrote "the answer must not depend on the units"
because it is too obvious to write down, and that is exactly why it was
false for the package's default method.

One correction, for the tally. My first RNG measurement reported that
`cpt_detect()` disturbs the caller's stream even with no `seed` argument.
That was wrong: the test generated its data *inside* the timed call, so
`rnorm()` advanced the stream and the detector was blamed. Re-run with the
data pre-generated, deterministic `pelt` preserves the stream exactly. The
real finding is narrower and worse — it is the `set.seed()` that the `seed`
argument triggers. Tally: 5 findings raised this pass, 5 confirmed after
re-measurement, 1 initially mis-attributed. Same lesson as §170.3 — check
the mechanism before believing the count.

# Part V (continued) — can a user find any of this?

## 183. 130 exports, and 45% of them have no way in

§174 measured whether an export has a runnable example and whether a
vignette mentions it. Neither asks the question a user in an R console
actually faces: *given one help page, can I get to the others?* R's help
system is a graph, and `\link{}` is its only edge. Measured across all 124
man pages:

| | count |
|---|---|
| exports | 130 |
| exports with at least one inbound `\link{}` from another help page | 71 |
| **exports with zero inbound links — reachable only by typing the exact name** | **59 (45%)** |
| exports absent from the pkgdown website index | 14 |
| **exports invisible in both surfaces** | **4** |

**The package-level page emits zero links.** `?ggchangepoint` and
`?ggchangepoint-package` are the natural index — the first thing a new user
opens — and between them they contain **not one `\link{}` to any of the 130
exports**. The graph has a hub (`cpt_detect`, 48 inbound links) and no entry
point. The most outbound links on any page is 8, on `new_ggcpt`, which is
class-construction machinery rather than an overview.

**Fair credit where it is due:** the pkgdown reference index lists 116 of
130, so the *website* is navigable. It is the *console and IDE help pane*
path — which is how most R users read documentation — that has no
navigation. Those two surfaces should not disagree, and where they do the
website is right.

### 183.1 The 21 doubly-invisible exports

No inbound link **and** no `\examples{}` block, so a user who has not already
been told the name has no route and no demonstration:

- the **entire test-signal family** — `signal_blocks`, `signal_fms`,
  `signal_mix`, `signal_stairs`, `signal_teeth`. These are the five standard
  benchmark signals from the changepoint literature, they are one-liners,
  and they are the single most obvious thing to hand a newcomer. Nothing
  points at them and nothing shows them running.
- **seven engine wrappers** — `wbs_wrapper`, `wbs2_wrapper`, `not_wrapper`,
  `mosum_wrapper`, `tguh_wrapper`, `idetect_wrapper`, `fpop_wrapper`.
- **the ggplot2 layer set** — `geom_changepoint`, `geom_cpt_segment`,
  `stat_changepoint`, plus `scale_colour_cpt_label`.
- **class and utility machinery** — `is_ggcpt`, `new_ggcpt`,
  `annotate_segments`, `ggcpt_eval`, `ggcpt_compare_table`.

### 183.2 The wrapper policy that is not a policy

The defensible argument for a wrapper having no inbound link is that users
reach engines through `cpt_detect(method = )` and never call the wrapper.
The data says that argument is not what is happening: **43 wrappers, 9 with
any inbound link at all**, and the distribution is `nsp_wrapper` with 10
against 34 with zero. `bfast_wrapper` has 2; `bcp_wrapper`,
`beast_wrapper`, `bocpd_wrapper`, `esac_wrapper`, `hdcov_wrapper`,
`strucchange_wrapper` and `var_wrapper` have 1 each.

That is not a policy, it is an accident of which pages happened to get a
`@seealso` while being written. Either wrappers are internal-by-convention
and none should be linked (and then `nsp_wrapper`'s ten links are the
anomaly), or they are a public surface and all 43 need the same treatment.
The second is right — they are exported, documented, and `cpt_register_method()`
exists precisely so people can write more of them.

### 183.3 The accessibility features are the least accessible thing here

Exactly four exports are invisible in **both** the help graph and the
website index, and all four are the accessibility scales:

| export | inbound links | in website index | has example |
|---|---|---|---|
| `scale_color_cpt` | 0 | no | yes |
| `scale_fill_cpt` | 0 | no | yes |
| `scale_linetype_cpt` | 0 | no | yes |
| `scale_colour_cpt_label` | 0 | no | **no** |

`scale_colour_cpt` (the British spelling) is in the website index, so the
American alias and three siblings fall out of it — which also means
`pkgdown::build_site()` is warning about un-indexed topics and nobody is
reading the warning. A user who needs a colourblind-safe palette, which is
the entire point of these functions, cannot discover that the package has
one. §174 already noted that the accessibility pass shipped in 0.5.0; this
is what shipping without wiring looks like.

### 183.4 What to build

*Items 1 and 3 shipped in the 0.5.0 documentation pass: `?ggchangepoint` now
emits 16 links where it emitted none, and 70 `@family` tags took inbound
coverage from 71 to 124 of 130 exports. Item 2 was withdrawn as a false
positive — see §239. Deleted from the list. What remains:*

1. **A test for the graph, not just for mentions.** `test-doc-coverage.R`
   currently asserts every export is named in the README; extend it to
   assert every export has at least one inbound `\link{}`. That is parseable
   from the built package, it is mechanical, and it is the only thing that
   will keep the 124 from decaying back toward 71 on the next engine wave —
   §231.2 measured that this surface is the one that cannot keep pace by
   effort alone.
5. **Wire the accessibility scales first**, because they are the case where
   invisibility defeats the feature's purpose rather than merely
   inconveniencing someone.

This is the third audit in a row (§174, §183, and §177's field inventory)
whose finding is that something *exists and is not connected*. The pattern
from §178 holds and sharpens: the package's work is done and its wiring is
not, and wiring is checkable by a script in every case.

## 184. The other four invariances, run — and two of them were not invariances

§179.2 proposed five metamorphic transformations and asserted that each
"must hold". §179 ran the first (scale) and found eight engines. This runs
the other four across 42 engines, and the first thing it establishes is that
**my own proposal was wrong about two of them.**

Truth for the test series is changepoints at 70 and 140. Baseline behaviour
worth stating before the transformations: **27 of 42 engines return exactly
those two, 9 return three or more, and 6 return one or none** (the
single-change designs, plus `wbsts` which fails outright per §170).

| transformation | expectation | ok | off by <= 2 | changed materially |
|---|---|---|---|---|
| `x + 100` | identical changepoints | **40** | 1 | **1** |
| `rev(x)` | changepoints at `n - cp` | **36** | 3 | **3** |
| `c(x, x)` | `cp` and `n + cp` | 23 | 6 | 13 |
| each observation twice | `2 * cp` | 9 | 3 | 30 |

### 184.1 Offset: one real violation, and it is `beast`

Shift-invariance is unambiguous — adding a constant to a series cannot
create or move a mean changepoint. 40 of 42 engines hold. One does not:

| engine | on `x` | on `x + 100` |
|---|---|---|
| `beast` (Rbeast) | 70, 140 | **70, 140, 159** |

`Rbeast` fits a Bayesian trend model with priors on the trend's level, so a
large offset changes the posterior and manufactures a third changepoint at
159. `kwc` moves by <= 2. This is an upstream modelling property, not a
wrapper bug, but it is a property a user must know: **`beast`'s answer
depends on where zero is.** The registry should carry it, and the honest
mitigation is to centre the series before handing it to `beast` — which,
unlike §179's scale question, is safe because centring cannot change a mean
changepoint's location for any correct detector.

### 184.2 Reversal: three engines segment the reversed series differently

Time-reversal invariance is the other unambiguous one — offline detection
looks at the whole series, so the direction of the scan must not matter.
36 engines hold exactly. Three do not:

| engine | on `x` | on `rev(x)` | expected |
|---|---|---|---|
| `not` | 70, 140, 158 | 60, 130 | 42, 60, 130 |
| `cpm` | 67, 74, 139 | 60, 126, 130 | 61, 126, 133 |
| `kwc` | 70, 139 | 44, 61, 128 | 61, 130 |

`cpop`, `segmented` and `nsp` move by <= 2, which is boundary bookkeeping
rather than a different answer.

**The honest reading is subtler than "direction-dependent algorithm."** The
detection that flips is `not`'s third changepoint at 158, and the data there
are borderline: the mean of observations 141-158 is -0.692 against +0.284
for 159-200, a difference of about one standard deviation across 18 and 42
points. It is a false positive from sampling noise, and false positives near
the detection boundary are exactly what flip when anything changes. Five
engines — `wbs`, `not`, `smuce`, `segneigh`, `binsegrcpp` — report that same
spurious 158 on the base series and lose it under transformation.

So the reversal test is not primarily a correctness check; **it is a cheap
instability detector.** A changepoint that survives reversal is one the data
support; one that does not is a coin flip the user is being shown as a
finding. `cpt_stability()` exists to say this, and it takes bootstrap
resamples; reversal costs one extra detector call and catches the same
thing. That is worth adding as a `cpt_stability()` option — or as a note on
any changepoint that fails it.

`cpm` and `kwc` are different: their whole segmentations change, not one
borderline point. Both are sequential/streaming detectors by construction
(`cpm` is a sequential change-point model, `kwc` a windowed test), and a
sequential method genuinely has a direction. That should be recorded in the
registry as `sequential`, not filed as a defect — and it means the reversal
test must exempt them rather than fail them.

### 184.3 Concatenation is not a clean invariance, and here is why

23 engines match `{cp, n + cp}` exactly. The 13 that do not decompose into
categories, only some of which are defects:

- **4 are single-change-by-design and cannot possibly pass**: `amoc` is At
  Most One Change; `pettitt`, `buishand` and `snht` are single-change
  hypothesis tests. Each returns `70` on the doubled series, which is the
  correct output of the question they answer. My expectation was simply
  invalid for them.
- **5 lost the spurious 158** (`wbs`, `not`, `smuce`, `segneigh`,
  `binsegrcpp`), i.e. the doubled series gave the *better* answer and the
  test flagged it as a violation.
- **`segmented` is a slope method** applied to a mean-shift series, so its
  base answer (110) was already not meaningful; 85 on the doubled series is
  no more wrong.
- **`np` genuinely dropped one changepoint** (returned 5 of 6 expected), and
  `hdcov` returned a different set. Those two are worth chasing.
- `wbsts` failed, as it does everywhere.

**So the transformation needs a per-engine exemption list before it can be
an assertion**, and the exemption criterion is a registry fact the package
does not currently record: how many changepoints an engine is capable of
returning. `cpt_methods()` has nine capability columns and no `max_cp`.

### 184.4 Up-sampling is a sensitivity probe, not an invariance — and it is violent

Only 9 engines return `2 * cp`. Thirty return something materially
different, and the magnitudes are the point:

| engine | on `x` | each observation duplicated |
|---|---|---|
| `wbs2` | 70, 140 | **199 changepoints** |
| `pilliat` | 70, 140 | **199 changepoints** |
| `hsmuce` | 70, 140 | **183 changepoints** |
| `idetect` | 70, 140 | **168 changepoints** |
| `ecp` | 70, 140 | 9 changepoints |
| `pettitt`, `buishand` | 70 | **none** |

**This is not a bug, and §179.2 was wrong to list it as something that
"must hold."** Duplicating each observation produces a series with near-unit
lag-1 autocorrelation, which violates the independence assumption every one
of these engines is built on. A detector that reports 199 changepoints on
data whose effective sample size is half its nominal length is behaving
correctly given a false premise.

What the measurement *is* good for is a number the package cannot currently
give: **how catastrophically each engine fails when its independence
assumption is violated.** `wbs2` and `pilliat` going from 2 to 199 is a
different risk profile from `ecp` going from 2 to 9, and users with
autocorrelated data — which is most real time series — have no way to tell
those apart. §101 and §99 both circle this; here is the measurement.

The right product is not a test but a documented robustness column:
`ac_robust`, populated by exactly this probe (or better, by AR(1) noise at
several values of rho), so `cpt_recommend(noise = "autocorrelated")` — which
already takes that argument and currently uses a hand-written list — has
measured data behind it. That makes it the fourth registry column this
document has proposed populating by script rather than by hand
(§171 `na_handling`, §176 `cost`, §179 `scale_invariant`, and now
`ac_robust`), plus §184.3's `max_cp` and §184.2's `sequential`.

### 184.5 The corrected metamorphic test design

§179.2's five-row table should be replaced with this, because three of the
five need qualification:

| transformation | status | assertion |
|---|---|---|
| `x -> a*x`, `a > 0` | **invariance** | identical, except engines flagged `scale_invariant = FALSE` |
| `x -> x + b` | **invariance** | identical, except `beast` (record why) |
| `x -> rev(x)` | **invariance** | `n - cp`, exempting engines flagged `sequential` |
| `x -> c(x, x)` | conditional | `cp, n + cp`, only for engines whose `max_cp` allows it |
| duplication / decimation | **probe, not assertion** | record the changepoint count; populates `ac_robust` |

Three genuine invariances, one conditional assertion, one measurement. That
is a test file worth writing, and the exemptions are not escape hatches —
each one is a registry column the package should have anyway, which is why
writing the test forces the metadata into existence. **The test and the
registry are the same work.**

## 185. What this pass changes

| § | finding | measured how |
|---|---|---|
| 183 | 59 of 130 exports have zero inbound `\link{}`; `?ggchangepoint` links nothing; 4 accessibility scales invisible in both help and website | parsed all 124 `.Rd` files + `_pkgdown.yml` |
| 184.1 | `beast` is not shift-invariant — `x + 100` invents a changepoint at 159 | detect on `x` and `x + 100`, 42 engines |
| 184.2 | 3 engines segment `rev(x)` differently; reversal is a cheap instability detector; `cpm`/`kwc` are legitimately sequential | detect on reversed series |
| 184.3 | concatenation needs a `max_cp` registry column before it can be asserted; `np` and `hdcov` genuinely drop changepoints | detect on `c(x, x)` |
| 184.4 | up-sampling is a sensitivity probe, not an invariance; `wbs2` and `pilliat` go from 2 changepoints to **199** | detect on duplicated series |

New actions: make `?ggchangepoint` the index and add `@family` tags
(§183.4); six measured registry columns, now the document's most-repeated
recommendation (§184.4); the corrected metamorphic test file (§184.5);
centre the series for `beast` (§184.1); a reversal-based instability flag on
`cpt_stability()` (§184.2).

**Two corrections to my own earlier passes, recorded so the tally holds.**
§179.2 asserted five invariances; measurement shows three are invariances,
one is conditional on a registry fact the package lacks, and one is not an
invariance at all — duplication legitimately changes the answer because it
changes the model. And §184.3's "13 violations" is really 2 defects, 4
category errors in my own expectation, 5 borderline flips, and 2 engines
answering a different question. The raw violation count would have been a
misleading headline in both cases.

That makes three passes running where the largest correction was to the
previous pass's *framing* rather than to the codebase. §182 said the claims
that go unchecked longest are the ones nobody thinks to state; this pass
adds the counterpart: **once you do state them, the first draft of the
statement is usually too strong.** An invariance worth testing needs its
exemptions measured at the same time, or the test manufactures 30 false
positives — which is exactly the 93% false-positive rate §170.3 recorded,
arrived at from the opposite direction.

# Part V (continued) — the function that answers "which method should I use?"

## 186. Under the default noise setting, `cpt_recommend()` returns alphabetical order

A first-time user's first question is not statistical, it is procedural:
*fifty methods, which one?* `cpt_recommend()` exists to answer it. Measured,
for the most common possible query — univariate, mean change, independent
Gaussian noise, n = 300:

| | |
|---|---|
| candidates returned | 32 |
| **distinct scores among them** | **2 (one engine at 1.0, thirty-one tied at 1.5)** |
| ordering within the tie | **alphabetical** |
| `why` column, for all 31 | `handles change_in = "mean"` — identical |
| **top recommendation** | **`amoc`** |
| rank of `pelt`, the package's own default | **20th of 32** |

`amoc` is At Most One Change. On a series with changepoints at 100 and 200
it returns a single point:

```
amoc on a 2-changepoint series: 201
pelt on the same series       : 100, 201
```

So the function whose entire purpose is to route a newcomer to a method
recommends, for the canonical case, an engine that structurally cannot find
the second changepoint — and buries the package's own default in 20th place
behind `buishand`, `bocpd` and `cpm` for no reason other than that `a`
sorts before `p`.

### 186.1 Why it collapses

The scoring in `consensus.R:340-395` is built entirely from noise-regime
adjustments: `noise_pref` adds 3, `noise_warn` subtracts 2, and both lists
are **empty when `noise = "iid"`**. Nothing else discriminates. Every engine
that supports the requested `change_in` gets the same base score, so under
the default noise setting the score column carries one bit of information
(installed or not) and the sort falls through to alphabetical.

Credit where it is due, and it is worth stating because §176 found the
opposite for the cost lists: **all 47 method names across the six
hand-written noise lists resolve to real registry methods.** No dead
entries. The one cross-regime tension — `not` is preferred for
heteroscedastic noise and warned for autocorrelated noise — is not a
contradiction, just two different regimes, and it is defensible.

The problem is not that the noise lists are wrong. It is that they are the
*only* thing the recommender knows, so the moment a user does not claim
unusual noise, it has nothing to say and says it in alphabetical order.

### 186.2 What the score is missing

Three terms, none of which requires new statistics:

1. **`max_cp` — how many changepoints an engine can return.** §184.3 already
   needed this column to make the concatenation invariance assertable. Here
   it is needed for correctness of advice: `amoc`, `pettitt`, `buishand` and
   `snht` are single-change designs and must not lead a ranking unless the
   user said they expect one change. `cpt_recommend()` has no argument for
   that either — add `n_expected = NULL` and let a user who genuinely has a
   single-change hypothesis say so.
2. **A general-purpose tier.** `pelt`, `binseg`, `fpop`, `wbs`, `wbs2`,
   `not`, `mosum` are the workhorses: mature, fast, widely cited, and
   correct on the common case. Ranking them alongside `bocpd` and
   `fabisearch` under a generic query treats "can this engine answer the
   question" as the only criterion, when "is this what a competent
   statistician would reach for first" is the criterion the user actually
   wants. That is a judgment, it should be recorded as one, and it belongs
   in the registry as a column with a documented rationale rather than
   emerging from `sort()`.
3. **Measured robustness instead of asserted robustness** — see §187. The
   noise lists encode the *literature's* claims about which engines tolerate
   which noise. Nothing has ever checked them against the engines' behaviour.

### 186.3 And when everything genuinely ties, say so

The deeper fix is behavioural. When 31 candidates share a score, presenting
them in alphabetical order implies a ranking that does not exist. Either:

- **report the tie honestly** — group the tied engines and say "these are
  equally suitable on the information supplied; here is what would
  discriminate" (expected number of changepoints, series length, whether
  intervals are needed); or
- **break the tie with data**, using the measured power and false-positive
  rates from §187.

The first is cheap and is the more honest default; the second is better and
needs the benchmark to exist. Both are strictly better than sorting by name.
A recommender that admits it cannot discriminate is useful. One that ranks
`amoc` first is worse than no recommender, because the user has no way to
know the order is meaningless.

## 187. The noise advice, measured against the engines' behaviour

§186 said the recommender's noise lists were the only thing it knows and
that nothing had checked them. This checks them.

**Method.** n = 300, true changepoints at 100 and 200, mean shift of 2
marginal standard deviations, four noise regimes, three replicates, 42
engines — 168 cells. Metrics per cell: *hits* (how many of the two true
changepoints were recovered within +/- 10) and *false positives* (detected
points more than 10 from either truth). Three replicates is thin, so read
these as directional, not as published rates; the effects below are large
enough to survive that caveat.

**The regimes are not equally hard, and only one of them is easy:**

| regime | mean hits (of 2) | mean false positives | engines with >5 false positives |
|---|---|---|---|
| iid Gaussian | 1.69 | **0.2** | **0 of 42** |
| heavy-tailed, `t(3)` | 1.76 | 1.5 | 3 |
| **AR(1), rho = 0.7** | **1.34** | **6.9** | **16** |
| heteroscedastic (sd 0.5 / 1 / 2.5) | 1.69 | 6.3 | 13 |

Under independent Gaussian noise every one of the 42 engines is fine. Under
AR(1) noise, 16 of them emit more than five spurious changepoints, and the
mean detection rate drops by a fifth. **That is the same shape as §179.2's
observation about coverage, one level up: the entire test suite generates
data with `rnorm()`, so it lives permanently in the one regime where nothing
goes wrong.**

### 187.1 Does the advice match?

Comparing the mean false-positive rate of each regime's `noise_pref` list
against its `noise_warn` list:

| regime | preferred | warned | verdict |
|---|---|---|---|
| heavy | 1.08 fp, 2.00 hits | 1.05 fp, 1.86 hits | **inverted — the two sets are indistinguishable** |
| autocorrelated | 2.00 fp, **1.00 hits** | 6.64 fp, **1.50 hits** | directionally right |
| heteroscedastic | 2.61 fp, 1.94 hits | 7.06 fp, 1.78 hits | directionally right |

Two of three are right in direction, which is better than §176's cost lists
managed. But each has a specific problem:

- **Heavy-tailed noise: the lists separate nothing.** 1.08 against 1.05.
  The scoring moves preferred engines +3 and warned engines -2, a five-point
  swing, on a distinction the measurement cannot detect. Four of the seven
  *warned* engines — `binseg`, `amoc`, `fpop`, `cpop` — beat the preferred
  average, and two *preferred* engines, `geomcp` and `nsp`, are worse than
  the warned average.
- **Autocorrelated noise: the preferred set buys its low false-positive
  rate with power.** 2.00 fp looks much better than 6.64, but it comes with
  1.00 hits against 1.50 — the recommended engines miss half the real
  changepoints. A recommender that optimises one error rate without
  reporting the other is giving advice a statistician would not give.
  `pelt`, `binseg`, `amoc` and `fpop` are all *warned* and all beat the
  preferred average.
- **Both non-iid regimes: the worst offenders are in neither list.** Under
  AR(1), ten engines exceed five false positives while being neither
  preferred nor warned, including the worst cell in the whole benchmark.

### 187.2 The worst cells, and none of them is warned about

| engine | regime | hits | false positives |
|---|---|---|---|
| `inspect` | AR(1) | 2 / 2 | **49.3** |
| `inspect` | heteroscedastic | 2 / 2 | **40.3** |
| `bcp` | heteroscedastic | 2 / 2 | 35.0 |
| `wbs2` | heteroscedastic | 2 / 2 | 34.3 |
| `esac` | heteroscedastic | 2 / 2 | 31.3 |
| `esac` | AR(1) | 2 / 2 | 26.7 |
| `wbs2` | AR(1) | 2 / 2 | 26.0 |
| `taylor` | AR(1) | 2 / 2 | 18.7 |

Every one of these finds both true changepoints and then reports twenty to
fifty more. That is the most dangerous failure shape available: the result
looks like a successful detection with extra detail, and a user checking
"did it find my known change?" will answer yes.

### 187.3 Three specific entries that should change

1. **`nsp` is in all three preferred lists and is the worst preferred entry
   in all three** — 5.0 false positives under heavy tails, 10.3 under
   AR(1), 12.0 under heteroscedasticity. Narrowest Significance Pursuit is
   built to control the probability of *any* false positive, so this is
   worth understanding rather than just demoting: the wrapper's default
   significance level, or the automatic threshold, is likely not doing what
   the method promises. That is a wrapper-level question with a real answer,
   and it is the highest-value single item in this section.
2. **`wbsts` is preferred for autocorrelated noise and detects nothing in
   any regime.** Zero hits in all four. §170 recorded that it also claims a
   solution path it cannot produce. It is being actively recommended while
   non-functional.
3. **`segmented` returns zero hits in all four regimes**, which is correct —
   it is a slope method being asked about mean shifts — and is exactly the
   §181.1 finding again: it accepts `change_in = "mean"` without supporting
   it, so it appears in a mean-change benchmark at all.

### 187.4 What to build

1. **Replace the six hand-written lists with a measured table.** The
   benchmark above is 168 cells and about twelve minutes; run it at more
   replicates, at several `rho` values, and at two or three signal
   strengths, and store the result as package data. `cpt_recommend()` then
   ranks by measured false-positive rate and power in the user's stated
   regime instead of by a five-point hand-assigned swing. This is the
   `ac_robust` column §184.4 asked for, generalised to all four regimes.
2. **Report both error rates.** The recommendation table has `score`, `why`
   and `caveat`; it should carry expected power and expected false-positive
   count at the user's `n` and regime. "Recommended: 2.0 spurious
   detections, recovers 1.0 of 2 changes" is advice. "score 4" is not.
3. **Warn on the measured outliers regardless of list membership.** Any
   engine above a threshold of spurious detections in the user's regime
   should carry a caveat automatically, generated from the table rather than
   from a name list — which is how `inspect` at 49 false positives would
   ever get flagged.
4. **Add the non-iid regimes to the test suite.** Not as accuracy
   assertions, which would be flaky, but as a recorded benchmark with a
   tolerance band, so a regression that doubles an engine's false-positive
   rate under AR(1) is visible. The suite currently cannot see any of this
   because it never leaves iid.
5. **Investigate `nsp` first**, since it is both the most-recommended engine
   for difficult noise and the worst-behaved one, and a fix there improves
   advice in three regimes at once.

## 188. What this pass changes

| § | finding | measured how |
|---|---|---|
| 186 | Under `noise = "iid"`, 31 of 32 candidates tie and the ranking is **alphabetical**; `amoc` is recommended first and cannot find a second changepoint; `pelt` ranks 20th | `cpt_recommend()` output inspected |
| 187 | iid is the only easy regime (0 of 42 engines above 5 false positives); AR(1) puts 16 of 42 above it | 42 engines x 4 regimes x 3 reps |
| 187.1 | the heavy-tailed advice separates nothing (1.08 vs 1.05 fp); the autocorrelated advice trades power for precision without saying so (1.00 vs 1.50 hits) | preferred vs warned aggregates |
| 187.2 | `inspect` reports **49 spurious changepoints** under AR(1) while finding both real ones, and is in no list | worst-cell ranking |
| 187.3 | `nsp` is the worst entry in all three preferred lists; `wbsts` is recommended and detects nothing | per-engine cross-reference |

New actions: `max_cp` and a general-purpose tier in the registry, plus an
`n_expected` argument (§186.2); report ties honestly instead of sorting by
name (§186.3); replace the six hand-written noise lists with a measured
table shipped as package data (§187.4); surface power and false-positive
count in the recommendation itself; investigate `nsp`'s thresholding.

**What this pass adds to the method.** The previous three passes found
unchecked claims *about the package's own code*. This one found an unchecked
claim about the *outside world* — the noise lists encode what the literature
says each method tolerates, and two of three are directionally right, which
is a reasonable hit rate for expert judgment and a poor one for something
that moves a score by five points. The pattern completes: **§176 hand-written
cost lists, §184 hand-written robustness assumptions, §187 hand-written
noise preferences — every place the package encodes a judgment as a
character vector, that vector is wrong in a way a twelve-minute script
detects.** The recommendation is now uniform across six columns and needs
saying once, plainly: *no method-name list in this package should be
hand-maintained.* Each should be generated by a script that measures the
property it claims, checked in as data, and regenerated when engines change.

# Part V (continued) — the advice names the engine and not the argument

## 189. Four engines carry the fix for a noise regime behind an argument whose default is wrong

§187.3 flagged `nsp` as the worst entry in all three of the recommender's
preferred lists and called it "a wrapper-level question with a real answer."
Here is the answer, and it generalises past `nsp`.

**Fourteen wrappers take an argument that selects the noise or error model.**
The default is the first element of the `match.arg` vector, and for several
of them that first element is the independent-homoscedastic option:

| method | argument | default | the other options |
|---|---|---|---|
| `nsp` | `variant` | **`"poly"`** (i.i.d.) | `selfnorm`, `ar`, `tvreg` |
| `smuce` | `family` | **`"gauss"`** (homoscedastic) | `hsmuce` |
| `fastcpd` | `family` | `"mean"` | `variance`, `meanvariance`, `ar`, `arma`, `garch` |
| `fmean` | `robust` | **`FALSE`** | `TRUE` |
| `envcpt` | `models` | all eight, AR models included | — |
| `cpm` | `cpm_type` | `"Mann-Whitney"` (nonparametric) | Student, Bartlett, ... |

`envcpt` and `cpm` have sensible defaults, which is why §187 measured them
at 0.00 and 0.33 false positives in their recommended regimes. The others do
not.

### 189.1 `smuce`: one argument, 10.00 false positives to 0.33

Heteroscedastic noise (sd 0.5 / 1 / 2.5), true changepoints at 100 and 200,
six replicates:

| call | hits | false positives |
|---|---|---|
| `cpt_detect(x, method = "smuce")` — default `family = "gauss"` | 2.00 / 2 | **10.00** |
| `cpt_detect(x, method = "smuce", family = "hsmuce")` | 2.00 / 2 | **0.33** |
| `cpt_detect(x, method = "hsmuce")` | 2.00 / 2 | 0.33 |

A thirtyfold reduction in spurious detections at **no cost in power**, from
one argument. And note the third row: the package already ships that
argument as a separate registry entry — `smuce` and `hsmuce` are the same
`stepR` function with different `family` values.

So the recommender's advice here is *correct* (it warns about `smuce` for
heteroscedastic noise and prefers `hsmuce`) and its *explanation* is absent.
A user who has already chosen `smuce` for good reasons is told it is
questionable, and is not told that the fix is one argument on the engine
they already have. That is a different and more useful message than "pick a
different method."

### 189.2 `nsp`: the default variant is the i.i.d. one, and the fix is neither obvious nor the one you would guess

AR(1) with rho = 0.7, same design:

| call | hits | false positives |
|---|---|---|
| `method = "nsp"` — default `variant = "poly"` | 2.00 / 2 | **9.00** |
| `method = "nsp", variant = "ar"` | **0.00 / 2** | 0.17 |
| `method = "nsp", variant = "selfnorm"` | 1.50 / 2 | **0.50** |

Three things follow. First, this is the mechanism behind §187.3: `nsp` is in
`noise_pref` for autocorrelated noise, and `cpt_detect(method = "nsp")`
reaches it through the i.i.d. variant, so the recommended engine arrives
assuming exactly what the user just said is false.

Second, **`variant = "ar"` is not the fix** — it eliminates the false
positives by eliminating detection, 0 of 2 hits. With `ord = 1` and a mean
shift, the AR fit absorbs the change. Anyone reading the argument list and
choosing the obviously-named option gets silence.

Third, **`selfnorm` is the fix**: 1.5 of 2 hits at 0.5 false positives,
against 2.0 hits at 9.0. That is the self-normalised variant, and nothing in
the package, the recommender or the help page points a user with
autocorrelated data toward it.

### 189.3 One correction to §187

§187.3 listed `fastcpd` among the concerns for heteroscedastic noise. It is
not: measured at `family = "mean"` (the default) it returns 2.00 / 2 hits at
**0.17** false positives, the best of anything tested in that regime.
`family = "variance"` drops to 1.00 hits and `family = "meanvariance"` to
0.00, so the default is also the right choice — the only engine here whose
default needs no change. Withdrawn.

### 189.4 What this means for the design

The pattern is §177's thesis moved from return values to arguments. **The
capability to handle the hard regime exists inside the engine, is reachable
by one argument, and is unreachable through the advice.** Three consequences:

1. **A recommendation must be a call, not a name.** `cpt_recommend()` should
   return the arguments its recommendation depends on — a `call` column
   holding `cpt_detect(x, method = "nsp", variant = "selfnorm")` rather than
   `nsp`. Everything needed to populate it is already measured: the noise
   table from §187.4 just needs a second key on the argument setting.
2. **Benchmark over settings, not over methods.** §187's table has one row
   per engine; it should have one row per *engine and noise-model setting*.
   `smuce` at `family = "gauss"` and at `family = "hsmuce"` are two
   detectors with a thirtyfold difference in false-positive rate, and a
   benchmark that reports one number for "smuce" hides that entirely. This
   also removes the duplication where `hsmuce` exists as a registry row only
   because the argument was not discoverable.
3. **Audit every `match.arg` default against the regime it implies.** The
   first element of a `match.arg` vector is a design decision that nobody
   revisited; here, at least two of them silently assume the easy case. The
   defensible default for a package that wraps other people's engines is the
   upstream default — but where it differs from the upstream default, or
   where a strictly-better option exists at no cost (`hsmuce` costs nothing
   in power), that deserves a recorded reason.

This is now the seventh registry column the document has asked to be
generated rather than hand-written (`na_handling`, `cost`,
`scale_invariant`, `ac_robust`, `sequential`, `max_cp`, and now
`noise_model_arg`), and the first one that changes what a *recommendation*
looks like rather than what a table says.

## 190. Empirical size under the global null: the number the package has never reported

Every measurement so far has put a real changepoint in the series and asked
whether it was found. The complementary question is the one a user asks when
a detector *does* report something: **how often does this engine report a
changepoint in a series that has none?** That is the empirical size, and
neither `cpt_methods()`, `cpt_recommend()`, the vignettes nor this document
has ever carried it.

**Method.** n = 300, **no changepoint at all**, 10 replicates, 42 engines,
two noise regimes. `size` is the fraction of replicates reporting at least
one changepoint; `mean cp` is the average number reported. Ten replicates
resolves size to 0.1, so "0.00" means zero of ten and does not distinguish
a true rate of 0 from one of 0.05 — the AR(1) column below is far past
needing that precision.

| regime | median size | size = 0.00 | size <= 0.10 | size = 1.00 | mean spurious cp | worst |
|---|---|---|---|---|---|---|
| iid Gaussian | **0.00** | 28 of 42 | 38 of 42 | 1 | 0.09 | `np` (1.1) |
| **AR(1), rho = 0.7** | **1.00** | 5 of 42 | 6 of 42 | **25 of 42** | **8.47** | `inspect` (52.9) |

### 190.1 Under independent noise the package is well calibrated

28 of 42 engines never raised a single false alarm in ten pure-noise
replicates, and 38 of 42 came in at or under 0.10. That is a genuinely good
result and worth stating plainly, because the rest of this section is not.

Two engines stand out:

- **`segmented` has size 1.00 by construction, and it is not a defect.**
  `npsi = 1` asks for the best single breakpoint in a piecewise-linear fit,
  so it always returns one. It is answering "where is the break" and not "is
  there one." That is a legitimate difference in question, and it is the
  same fact §187.3 and §181.1 keep surfacing from other directions: the
  engine appears in mean-change comparisons only because `change_in` is not
  validated against `supports`.
- **`np` has size 0.60** — `changepoint.np` with the wrapper's default
  penalty reports a changepoint in six of ten pure-noise series, averaging
  1.1 of them. That is a real calibration problem in a default, and `np` is
  in the recommender's preferred list for both heavy-tailed and
  heteroscedastic noise.

### 190.2 Under AR(1) noise, 25 of 42 engines false-alarm in every single replicate

On series containing **no changepoint whatsoever**:

| engine | mean spurious changepoints per replicate |
|---|---|
| `inspect` | **52.9** |
| `esac` | 30.4 |
| `wbs2` | 30.1 |
| `bcp` | 24.5 |
| `taylor` | 23.3 |
| `cpop` | 18.2 |
| `pilliat` | 14.6 |
| `cpm` | 13.1 |
| `smuce` | 13.0 |
| `idetect`, `tguh`, `binsegrcpp` | ~11 each |
| `nsp` | 10.3 |
| `ecp`, `not`, `wbs` | ~10 each |

The median engine reports a changepoint in **every** replicate. `inspect`
partitions a 300-point pure-noise series into roughly 54 segments. This is
the §187 finding without the confound of a real signal to find: there is
nothing there, and most of the package says there is.

**Five engines hold at size 0.00**, and they are exactly the ones whose
model includes the dependence: `decafs` (drift plus AR noise), `envcpt`
(fits AR models as part of its ensemble), `npmojo`, `kcp`, and `wbsts`
(which detects nothing anywhere, per §187.3, so its zero is not evidence).

### 190.3 `nsp` at the nominal level — the cleanest result in the document

NSP's contract is explicit: with `alpha = 0.1`, the probability of reporting
**any** false positive is at most 0.1. That is a testable claim, and the
wrapper's default is `alpha = 0.1`. On pure noise:

| variant | iid: size / mean cp | AR(1): size / mean cp |
|---|---|---|
| `poly` (the default) | 0.00 / 0.0 | **1.00 / 10.3** |
| `selfnorm` | 0.00 / 0.0 | 1.00 / 1.2 |
| `ar` | 0.00 / 0.0 | **0.00 / 0.0** |

Under i.i.d. noise all three honour the level and are conservative — the
guarantee holds where its assumptions hold. Under AR(1) the default variant
violates a nominal 10% family-wise level **in 100% of replicates**, and
reports ten spurious regions per series while doing it.

**And this refines §189.2 rather than repeating it.** There I wrote that
`variant = "ar"` "is not the fix" because it returned 0 of 2 hits against a
real step change. Under the null it is the *only* variant that holds the
level — size 0.00 where the default is 1.00. The correct joint statement is:

- `ar` is **correctly calibrated and underpowered** against a step change at
  `ord = 1` (the AR fit absorbs the shift);
- `selfnorm` **trades a level violation for power** — 1.2 spurious regions
  under the null, 1.5 of 2 real changepoints found;
- `poly`, the default, is **wrong under dependence in both directions** at
  once: 10.3 spurious under the null and the appearance of full power
  because it detects everything.

A user cannot choose between calibration and power without being shown both
numbers, and the package currently shows neither.

### 190.4 What to build

1. **Ship the size table as package data**, keyed by engine, noise regime,
   and noise-model argument (§189.4's second key). It is the single most
   useful number for someone deciding whether to believe a detection, and it
   costs one script.
2. **Surface it in `cpt_detect()`'s own output.** A result carrying "engines
   of this kind report ~8 spurious changepoints per series under
   autocorrelated noise" next to `n_cp = 9` would stop most of the
   misreadings this section describes. A `diagnostics$expected_null_rate`
   slot, populated from the table at the user's `n`, is the minimal version.
3. **Test the level claims of the engines that make them.** `nsp` promises a
   family-wise rate, `cpt_test()` reports p-values, `strucchange`'s
   `sctest()` is a hypothesis test. Each is a checkable assertion of the
   form "size <= alpha under the stated assumptions", and each is a test that
   asserts a relationship rather than a value — §184.5's category, applied to
   inference rather than to invariances. **The package makes level claims and
   tests none of them.**
4. **Warn when an engine's assumptions are checkable and violated.** A
   Ljung-Box test on the residuals of the fitted segmentation costs
   microseconds and would tell a user that the series they just segmented is
   autocorrelated — which, given the table above, is the single fact most
   likely to invalidate their result. This is a better use of the
   `diagnostics` slot than anything currently in it.
5. **Reconsider `np`'s default penalty**, which is the one iid-regime
   calibration problem, in an engine the recommender actively prefers.

## 191. What this pass changes

| § | finding | measured how |
|---|---|---|
| 189 | 14 wrappers have a noise-model argument; `smuce`'s default gives **10.00** false positives where `family = "hsmuce"` gives **0.33** at no power cost; `nsp` defaults to the i.i.d. variant | 6 reps, heteroscedastic and AR(1) |
| 189.3 | **correction:** `fastcpd`'s default is the best performer in its regime, not a concern | same design |
| 190.1 | under iid noise 28 of 42 engines never false-alarm; `np` alarms in 6 of 10 pure-noise series | 10 reps, no changepoint |
| 190.2 | under AR(1), **25 of 42 engines report a changepoint in every pure-noise replicate**, averaging 8.47; `inspect` reports 52.9 | 10 reps, no changepoint |
| 190.3 | `nsp` at `alpha = 0.1` violates its own family-wise level in **100%** of AR(1) replicates on the default variant; `variant = "ar"` holds it exactly | pure null, three variants |

New actions: a size table shipped as data and surfaced in the result
(§190.4); level-claim tests for `nsp`, `cpt_test()` and `sctest()`; a
residual autocorrelation check in `diagnostics`; recommendations expressed as
*calls* rather than method names (§189.4); `np`'s default penalty revisited.

**What this pass adds to the method.** The previous passes asked whether the
package's claims about *itself* were true. This one asked whether the
engines' claims about *their own error rates* are true, which is the first
question a reviewer of an applied paper would ask, and the answer for the one
engine that states a rate explicitly is that it is violated in every
replicate the moment its assumption fails.

There is a sharper way to put the whole sequence. §187 showed the test suite
never leaves iid noise. §190 shows why that matters more than it looked:
**under iid the package is genuinely well calibrated — 28 engines with zero
false alarms — and under mild, extremely common dependence it is not, at
all.** The suite is not merely incomplete; it exercises precisely the one
regime in which there is nothing to find. Every quality signal the project
has — 2,590 passing tests, 85.79% coverage, a clean `R CMD check` on four
platforms — was earned inside that regime, and none of them constrains
behaviour outside it. That is the argument for making the noise-regime
benchmark part of the release process rather than a roadmap item.

# Part V (continued) — the package's own inference, tested against its own claims

§190.4 asked for the level claims to be tested and noted that the package
makes several and checks none. This part tests them. It begins with the one
that is not an engine's claim but the package's own.

## 192. `selection_adjusted = TRUE` is set on p-values that are not selection-adjusted

`cpt_test()` exists partly to prevent a specific mistake. The 0.5.0
submission note describes it exactly: it "reports a `selection_adjusted`
flag so an unadjusted two-sample p-value can never be mistaken for a
selection-adjusted one." The mechanism is right, the warning it emits is
right, and on the fallback route it works — the honest case first:

| engine | route taken | `selection_adjusted` | p on a real changepoint |
|---|---|---|---|
| `pelt`, `wbs`, `smuce` | Welch two-sample t (unadjusted) | `FALSE` | 6.5e-50 |
| `segmented` | Davies test (segmented) | `TRUE` | **NA** |
| `strucchange` | Chow F (strucchange) | `TRUE` | 0 |

The first row is the package behaving well. A Welch t-test between the two
segments either side of a break chosen by minimising cost gives p = 6.5e-50
on a 2-sigma shift — an absurd number, which is precisely what selection
bias looks like — and the flag says `FALSE` and a warning fires. Nobody can
be misled by that.

**The `strucchange` row is the problem.** `strucchange_jump_test()` runs
`sctest(type = "Chow", point = k)` at each break date `k` that the
Bai-Perron dynamic program selected, and hard-codes
`selection_adjusted = TRUE`. The source comment justifies it as convention:
*"running it at the dates the Bai-Perron dynamic program selected is the
standard reporting convention in that literature."* The convention is real.
It does not make the p-value adjusted. A Chow F test at a date chosen by
minimising RSS over all candidate dates is the textbook statement of the
selection problem, not its solution.

### 192.1 What that costs, measured on data with no changepoint

Pure noise, n = 300, 15 replicates, `method = "strucchange"`, then
`cpt_test()` on whatever it found:

| noise | replicates with a detection | spurious cps per replicate | median p | p < 0.05 | p < 0.001 | flag |
|---|---|---|---|---|---|---|
| iid Gaussian | 0 of 15 | — | — | — | — | — |
| **AR(1), rho = 0.7** | **11 of 15** | 2.2 | **0.0031** | **75%** | **46%** | `TRUE` in 24 of 24 rows |

On series containing no changepoint at all, the package reports a
changepoint, tests it, calls it significant at p < 0.001 in nearly half the
cases, and labels the p-value selection-adjusted. Under i.i.d. noise
`strucchange` never false-alarms, so the failure is invisible in exactly the
regime the test suite uses — §191's point, arriving now at the package's own
inference rather than at an engine's.

**Why this is worse than the 6.5e-50 case.** A p-value of 6.5e-50 is
self-evidently broken and is flagged `FALSE`; nobody publishes it. A p-value
of 0.003 flagged `TRUE` is exactly the number a user copies into a
manuscript. The mislabel converts an obviously untrustworthy output into a
plausible one, which is the opposite of what the flag was built to do.

### 192.2 Two compounding causes, and they need separating

1. **Selection.** The Chow test's null distribution assumes the break date
   was fixed in advance. It was not. This holds even under i.i.d. noise; it
   is simply not visible there because `strucchange`'s dynamic program
   reports nothing on clean noise.
2. **Dependence.** Even a genuinely selection-adjusted test assumes
   independent errors. Under AR(1) the Chow F is invalid regardless of how
   the date was chosen.

These want different fixes and should not be conflated. The literature has
answers for both: `strucchange::sctest(type = "supF")` and the Bai-Perron
critical values address the selection problem directly, and HAC-corrected
variants address the dependence.

### 192.3 And `segmented`'s Davies test returns NA while claiming adjustment

On a clean two-segment mean shift, `segmented`'s route returns
`selection_adjusted = TRUE` with **`p_value = NA`**. The Davies test is the
right tool — it is built for the nuisance-parameter-under-the-alternative
problem, so the `TRUE` is defensible in principle — but a missing p-value
carrying an adjustment claim is its own small defect: the flag asserts a
property of a number that does not exist. `cpt_test()` should either produce
the p-value or drop the row, not label an `NA` as adjusted.

### 192.4 What to build

1. **Set `selection_adjusted = FALSE` on the `strucchange` route**, and say
   why in the `method` string: `"Chow F at a data-chosen date (not
   selection-adjusted)"`. This is a one-line change that restores the flag's
   meaning, and it is the highest-priority item in this part — the current
   value is not a limitation, it is an incorrect claim about a number.
2. **Add a genuinely adjusted route.** `sctest(type = "supF")` tests for a
   break *anywhere*, which is the selection-adjusted question, and
   `strucchange` already computes it. That gives `cpt_test()` its first
   honestly-adjusted p-value for a mean-change engine.
3. **Make the flag three-valued or add a second column.** "Adjusted for
   selection" and "valid under the observed dependence" are different
   guarantees, and §192.2 shows a p-value can fail either. A single logical
   cannot carry both; `selection_adjusted` plus `assumptions_checked` can.
4. **Never emit a flagged `NA`.** Drop the row or report the failure.
5. **Test the level claim.** The pure-null experiment above is eight lines
   and belongs in `tests/manual/`: for every route `cpt_test()` can take,
   the empirical rejection rate on data with no changepoint, under both
   noise regimes. Any route claiming adjustment must come in near its
   nominal level under i.i.d. noise, and the AR(1) column is the documented
   caveat rather than an assertion.

## 193. `cpt_confint()` coverage: three provenances are right, one is vacuous

The canonical check on an interval is whether it contains the truth at its
nominal rate. `cpt_confint()` unifies four provenances behind one contract,
and nothing has ever measured any of them.

**Method.** n = 300, one true changepoint at 150, mean shift of 2 sigma,
`level = 0.95`, 20 replicates. Coverage is computed conditional on a
detection within 30 of truth, which is the standard convention — an interval
around a changepoint that was never found is not an interval for that
changepoint.

| provenance | engines | mean coverage | mean width | verdict |
|---|---|---|---|---|
| `native` | 6 | **0.95** | 11.9 | correct |
| `bootstrap` | 3 | **0.95** | **6.2** | correct, and the narrowest |
| `nsp` | 1 | 1.00 | 33.0 | conservative by design |
| `posterior` | 2 | 1.00 | **245.5** | **vacuous** |

Per engine on the native route: `strucchange` 0.95, `bfast` 0.90, `taylor`
0.90, `smuce` 1.00 (width 8.8), `hsmuce` 1.00 (width 29.8). `segmented`
produced no detection within 30 of truth in any replicate, which is §190.1
again — it is a slope method being asked about a mean shift.

**This is the best result in the document and deserves saying plainly: the
package's own bootstrap interval achieves nominal 0.95 coverage at a width
of 6.2 on a 300-point series, beating every engine's native interval.** The
one piece of inference `ggchangepoint` computes itself rather than borrowing
is the one that works best.

### 193.1 The posterior route covers by covering almost everything

A 95% interval of width 245 on a series of length 300 spans 82% of the data.
Coverage of 1.00 is arithmetically true and carries no information. But the
cause is not a bug in the interval code, and the diagnosis is the
interesting part. Measuring `bcp`'s posterior profile directly, four
replicates:

| replicate | posterior mode | mass within +/-10 of truth | 50% HDI width | 80% HDI width | 95% HDI width |
|---|---|---|---|---|---|
| 1 | **150** (truth 150) | 0.62 | **2** | 56 | 146 |
| 2 | **150** | 0.65 | **1** | 43 | 131 |
| 3 | **150** | 0.71 | **1** | 36 | 126 |
| 4 | 151 | 0.63 | **1** | 33 | 128 |

The posterior is **sharply peaked and heavy-tailed**. Its mode is exactly
right, two-thirds of its mass sits within ten observations of truth, and
then the remaining third is spread thinly across the entire series — so
reaching 95% requires collecting nearly half of it.

**So `level = 0.95` is the wrong summary for this posterior shape, and the
default hides an excellent estimate behind a useless interval.** A 50%
credible interval is one or two observations wide. That is a *better*
localisation than any other provenance produces, and it is unreachable
through the default.

### 193.2 What to build for the interval contract

1. **Warn when an interval exceeds a fraction of `n`.** An interval covering
   more than, say, a quarter of the series is not an answer, and
   `cpt_confint()` already knows `n`. One line, and it turns a silently
   vacuous result into a stated one.
2. **Report the mass profile, not only the interval.** For posterior
   provenances, return the mode and the mass within a user-specified window
   alongside the interval. The `source` column names the provenance but says
   nothing about how informative it is, so a 6-point bootstrap interval and
   a 245-point credible interval arrive under the same label.
3. **Let `level` vary by provenance, or document why it should.** The
   package should not silently apply 0.95 to a posterior whose 50% HDI is
   the useful summary. Reporting both — 50% and 95% — costs nothing and
   makes the shape visible.
4. **Publish the coverage table.** Three provenances at nominal coverage is
   a genuine quality claim the package can make and currently does not. It
   belongs in the inference vignette and in `?cpt_confint`.

## 194. `cpt_test()` declares pure autocorrelation significant at p ~ 1e-5

§192 found the `strucchange` route mislabelled. This is the fallback route —
the one that is *honestly* labelled `selection_adjusted = FALSE` — measured
on data with no changepoint at all.

**Method.** Pure AR(1) noise, rho = 0.7, n = 300, no changepoint, 20
replicates. Detect, then `cpt_test()` every changepoint found. Every row is
therefore a p-value for a changepoint that does not exist.

| engine | rows tested | median p | fraction p < 0.05 |
|---|---|---|---|
| `pelt` | 27 | **5.0e-10** | **1.00** |
| `wbs` | 223 | 1.0e-05 | 0.99 |
| `not` | 212 | 1.2e-05 | 1.00 |
| `cpm` | 271 | 2.1e-05 | 1.00 |
| `binsegrcpp` | 238 | 5.7e-05 | 0.98 |
| `smuce` | 257 | 8.4e-05 | 0.85 |

1,228 p-values for changepoints that are not there, and **essentially all of
them are significant, with medians between 1e-5 and 1e-10.** For
calibration: on a *real* 2-sigma changepoint under i.i.d. noise the same
route gives median p = 3.3e-47.

So the Welch two-sample route produces `1e-47` for a real change and `1e-5`
for a pure artifact. Both are "highly significant"; the numbers are
uninformative in both directions, and the difference between a true and a
false detection is 42 orders of magnitude on a scale where 1.3 would be the
honest answer.

**The flag is doing its job and it is not enough.** `selection_adjusted =
FALSE` plus a warning is the correct label, and a user who reads it still
has a number in hand that says p < 0.001. The realistic outcome is that the
number gets reported and the caveat does not. §192.4's proposal for a second
column — "valid under the observed dependence" — is what would actually
speak here, because the failure above is caused by dependence, not by
selection: under i.i.d. noise these engines almost never false-alarm at all
(§190.1).

### 194.1 One correction, and it is mine

An earlier reading of this experiment reported that `smuce` and
`binsegrcpp` return **NA** p-values throughout. They do not: the true rate
is **1 row in about 250 (0.4%)**, and my summary showed `NA` only because
`median()` without `na.rm = TRUE` propagates a single missing value. The
real NA rate is a minor robustness gap — a segment too short for a Welch
test — not a systematic failure. Corrected, and the underlying finding is
unaffected because it concerns the 99.6% of rows that do produce a p-value.

### 194.2 What to build

1. **A second validity column** (§192.4), populated from a residual
   dependence check: a Ljung-Box test on the within-segment residuals costs
   microseconds and would set it automatically.
2. **Refuse, or heavily qualify, the naive route when dependence is
   detected.** A p-value of 1e-5 for an artifact is worse than no p-value.
   The honest output when the residuals are autocorrelated is `NA` with a
   reason, not a number.
3. **Bound the reported precision.** Nothing is gained by printing
   `3.3e-47`; `< 1e-10` conveys the same and does not invite the reader to
   treat the magnitude as evidence strength.
4. **The pure-null rejection rate belongs in the test suite** — for every
   route, under both noise regimes, as a recorded number with a tolerance
   band. That is the same recommendation as §190.4 item 3, now with the
   measurement attached.

## 195. What this pass changes

| § | finding | measured how |
|---|---|---|
| 192 | `selection_adjusted = TRUE` on `strucchange`'s Chow F at a data-chosen date; on pure AR(1) noise it labels artifacts significant at p < 0.001 in **46%** of rows | 15 reps, pure noise, both regimes |
| 192.3 | `segmented`'s route returns `p_value = NA` while claiming adjustment | clean 2-segment series |
| 193 | `native` and `bootstrap` provenances hit **nominal 0.95 coverage**; the package's own bootstrap is the narrowest at width 6.2 | 20 reps, level 0.95 |
| 193.1 | the `posterior` route's 95% interval spans **82% of the series**, because the posterior's 50% HDI is 1-2 wide and its tails are diffuse | HDI widths at 3 levels, 4 reps |
| 194 | 1,228 p-values for changepoints that do not exist; medians 1e-5 to 1e-10; ~100% below 0.05 | pure AR(1), 6 engines |
| 194.1 | **correction:** the NA p-value rate is 0.4%, not universal — my summary lacked `na.rm` | re-measured |

New actions: fix the `strucchange` flag and add a `supF` route (§192.4);
a second `assumptions_checked` column driven by a residual dependence test;
warn on intervals exceeding a fraction of `n` and report posterior mass
alongside the interval (§193.2); publish the coverage table; bound reported
p-value precision; pure-null rejection rates in the suite.

**What this pass adds.** Two of the three findings are the same shape as
everything since §187 — the package is well behaved under i.i.d. noise and
badly behaved under mild dependence — but §193 breaks the pattern in the
useful direction: **the one piece of inference the package computes itself,
the bootstrap interval, is the best-performing thing measured in this whole
document.** Nominal coverage, narrowest width, no caveats. That is worth
knowing for the roadmap's priorities: the gap is not in what the package
builds, it is in what it inherits and labels. `strucchange`'s p-value,
`bcp`'s credible interval and the Welch fallback are all borrowed or
improvised, and all three mislead; the bootstrap is the package's own and it
is correct.

The natural conclusion for 0.6.0 is therefore narrower than "test the
inference": **prefer the package's own bootstrap as the default provenance,
and treat every borrowed inferential quantity as requiring a validity label
before it is surfaced.** `cpt_confint(method = "auto")` currently prefers
`native`, then `posterior`, then `bootstrap` — the measured order is close
to the reverse.

# Part V (continued) — does the rest of the package's own machinery work?

§195 concluded that what the package *borrows* misleads and what it *owns*
(the bootstrap interval) is correct. That is a testable generalisation, and
`cpt_consensus()`, `cpt_select()` and `cpt_stability()` are the other three
things the package owns. This part measures them.

## 196. Consensus voting cannot remove dependence-induced false positives

`cpt_consensus()` runs several detectors and reports the locations they agree
on, with `min_votes = 2` by default. The implicit premise — and the one the
roadmap has been assuming since §190 raised the false-positive problem — is
that agreement is evidence: an artifact of one algorithm will not be
reproduced by another, so voting filters it out.

**Measured.** AR(1) noise, rho = 0.7, n = 300, real changepoints at 100 and
200, 10 replicates, panel `c("pelt", "binseg", "wbs")`, tolerance 5:

| detector | hits (of 2) | false positives |
|---|---|---|
| `pelt` alone | 1.90 | **1.20** |
| `binseg` alone | 1.90 | **1.10** |
| `wbs` alone | 1.90 | 9.70 |
| **consensus, `min_votes = 2` (the default)** | 1.90 | **1.40** |
| consensus, `min_votes = 3` (unanimity) | 1.80 | 0.80 |

**At its default setting the consensus is worse than two of its three
members.** 1.40 false positives against `pelt`'s 1.20 and `binseg`'s 1.10,
with identical power. Unanimity does beat the best member on false positives
(0.80 against 1.10) and pays for it in power (1.80 against 1.90).

### 196.1 Why: the artifacts are shared, not idiosyncratic

On **pure** AR(1) noise — no changepoint anywhere — how much do the
detectors' spurious sets overlap:

| replicate | `pelt` | `binseg` | `wbs` | `pelt` points matched by `wbs` (within 5) | matched by all three |
|---|---|---|---|---|---|
| 1 | 3 | 3 | 7 | **3 of 3** | **3** |
| 2 | 4 | 0 | 18 | **4 of 4** | 0 |
| 3 | 2 | 0 | 5 | **2 of 2** | 0 |
| 4 | 3 | 2 | 10 | **3 of 3** | 2 |
| 5 | 0 | 0 | 5 | — | 0 |

Every single one of `pelt`'s spurious detections is reproduced by `wbs`
within five observations, in every replicate where `pelt` detected anything.
In two of five replicates all three detectors agree on the same artifacts.

The reason is not subtle once stated: **a false positive caused by
autocorrelation is a feature of the realised noise path, not of the
algorithm.** An AR(1) path genuinely contains stretches whose local means
differ; every correct mean-change detector, given that path and a
misspecified independence assumption, will find the same stretches. Voting
removes *algorithmic* idiosyncrasy and this is not algorithmic. Agreement is
evidence about the algorithms, not about the data-generating process.

And the panel composition makes it worse rather than better: adding `wbs`
(9.70 false positives) imports its extras into the vote, which is why the
2-of-3 consensus is worse than the two clean members. **A consensus is
bounded below by the worst member's willingness to agree, not by the best
member's caution.**

### 196.2 What follows

1. **The documentation is honest and the framing needs to be explicit.**
   `?cpt_consensus` says it "reports the locations they agree on" and claims
   nothing more — no false claim to fix. But a user reaching for consensus
   is almost always trying to suppress false positives, and the help page
   should say directly what this measurement shows: *consensus suppresses
   disagreement between algorithms; it does not suppress false positives
   caused by a violated assumption shared by all of them.*
2. **Default `min_votes` should probably be unanimity, or a proportion.**
   `min_votes = 2` out of an arbitrary-length panel is a strange default —
   with three methods it is a bare majority, with ten it is 20%. The
   argument already accepts a fraction; the default should be one (0.5, or
   1.0 for the strict reading), so behaviour does not silently depend on
   panel size.
3. **Report vote counts *and* member disagreement.** The genuinely useful
   signal in a panel is not the consensus set but the *variance*: `wbs`
   finding 18 changepoints where `binseg` finds 0 is the single most
   informative fact available about that series, and it says "your noise
   model is wrong," which is exactly the diagnosis §190 wants surfaced. A
   `disagreement` statistic — say the ratio of union to intersection across
   members — would be a one-number assumption check derived from work the
   function already does.
4. **Panel composition should be measured, not chosen by hand.** Given
   §187's table, a panel of engines with *similar* false-positive profiles
   is safe, and mixing a 1.1 engine with a 9.7 engine is not. This is the
   seventh place a hand-written method list needs a measured basis.

## 197. `cpt_select()`: four criteria work, one always returns `k_max`, and three can never say zero

`cpt_select()` offers six criteria for choosing the number of changepoints,
deliberately so that "BIC says 3, cross-validation says 5" is a comparison a
user can make. Whether any of them recovers the truth has never been
measured.

**Method.** n = 400, jump 2 sigma, i.i.d. noise, `k_max = 8`, five
replicates at each true K in {0, 1, 2, 5}. Five replicates is thin; the
effects below are 0-versus-1 in size.

| criterion | exact-K rate | within +/- 1 | can it return K = 0? |
|---|---|---|---|
| `bic` | **0.90** | **1.00** | yes |
| `mbic` | **0.90** | **1.00** | yes |
| `cv` | 0.85 | 0.85 | yes |
| `crops_elbow` | 0.70 | 0.75 | **no** |
| `stability` | 0.60 | 0.90 | **no** |
| **`aic`** | **0.00** | **0.00** | **no** |

So the headline is positive: **`bic` and `mbic` recover the exact true K in
90% of cases and are never off by more than one**, across true K from 0 to
5. That is another piece of the package's own machinery working (§195's
pattern holds for a third time). But two specific failures matter.

### 197.1 `aic` is pinned at `k_max` at every true K

The picks, replicate by replicate, with `k_max = 8`:

| true K | `aic` picks |
|---|---|
| 0 | 8, 8, 2, 4, 4 |
| 1 | 8, 7, 8, 4, 8 |
| 2 | 7, 8, 8, 8, 8 |
| 5 | 8, 8, 7, 7, 8 |

AIC selects the most complex model available, essentially regardless of the
data. This is the textbook result — AIC's penalty of 2 per parameter is not
consistent for the number of changepoints, where the effective dimension
grows with `log n` — so it is not a bug in the implementation. It is a
problem with offering it as a peer of the others:

**`k_max` defaults to 20.** A user who selects `criterion = "aic"` on a
series with no changepoints will be told there are about twenty. There is no
warning, and the criterion table will show a monotonically improving curve
that looks like a legitimate elbow-free result.

The fix is not to remove it — comparing criteria is the function's stated
purpose, and AIC's inconsistency is worth *showing* — but to say so:
`criterion = "aic"` should warn once that AIC is not consistent for K and
will tend to `k_max`, and the criterion table should carry that note. This
is the same class as §176's cost lists and §187's noise lists: a documented
option whose behaviour nobody had checked.

### 197.2 Three of six criteria cannot return K = 0

`crops_elbow` picked {6, 3, 2, 4, 4} on pure noise; `stability` picked
{1, 1, 2, 1, 1}; `aic` as above. None of the three ever returned zero, in
any replicate, at any true K.

For `crops_elbow` and `stability` this is structural rather than a
calibration miss — an elbow in a penalty path and a bootstrap-frequency
ladder are both defined over K >= 1, so "no changepoints" is not in their
candidate set. That is defensible mathematics and an indefensible silence:
§93 (Theme BE) argued that "no changepoints detected" must be a first-class
answer, and here three of six selectors are constitutionally incapable of
giving it, without saying so.

**Minimum fix:** each criterion's help entry states whether K = 0 is
reachable, and `cpt_select()` warns when a criterion that cannot reach zero
returns its smallest candidate — because that is the only signal the user
will get that the answer may have been forced.

`cv` is worth one note: it is exact at K = 2 and K = 5 (5 of 5 both times)
and weak at K = 1, picking {3, 1, 1, 3, 4}. Small-sample cross-validation
with a single change appears to be its weak spot, which is worth a larger
replicate count before drawing a conclusion.

## 198. `cpt_stability()` scores a dependence artifact as highly as a real changepoint

`cpt_stability()` bootstraps the series, re-detects, and reports how often
each location is found — the natural answer to "is this changepoint real?"

**Method.** AR(1) noise, rho = 0.7, n = 300, real changepoints at 100 and
200, `B = 100`, `margin = 5`, 10 replicates. For every changepoint the
original fit reported, take the maximum bootstrap frequency within five
positions, and compare the real ones against the spurious ones.

| | |
|---|---|
| mean stability at **real** changepoints | **0.991** |
| mean stability at **spurious** changepoints | **0.903** |
| replicates where every real cp outscored every spurious one | **4 of 10** |

And the individual cases are worse than the means suggest:

| replicate | spurious cps | max spurious score | min real score |
|---|---|---|---|
| 4 | 2 | **0.99** | 0.93 |
| 7 | 1 | **1.00** | 1.00 |
| 8 | 1 | **1.00** | 1.00 |
| 9 | 3 | 0.985 | 1.00 |

In replicates 7 and 8 a spurious changepoint achieves a **perfect** 1.00
stability score. In replicate 4 a spurious point outscores a real one. The
statistic separates the two classes in fewer than half the replicates.

### 198.1 This is §196's mechanism again, and together they are the finding

`cpt_stability()` resamples the *data* and re-runs the *same* detector.
`cpt_consensus()` keeps the data and varies the *detector*. Both fail on
dependence-induced false positives, and for one reason:

> **The artifact is a property of the realised noise path. Bootstrap
> resampling preserves that path's local structure, and every detector given
> that path makes the same mistake. So both tools vary something that is not
> the cause.**

A stretch of an AR(1) series genuinely does have a different local mean from
its neighbour. Resample it and the stretch is still there — hence stability
1.00. Hand it to a different algorithm and the stretch is still there —
hence the shared spurious sets in §196.1. The package has two robustness
diagnostics and neither varies the one thing that would expose the problem:
**the noise model.**

### 198.2 What would actually work, and the package already contains it

1. **Vary the noise model, not the data or the algorithm.** §189 measured
   this working: `smuce` at `family = "gauss"` gives 10.00 false positives
   and at `family = "hsmuce"` gives 0.33; `nsp` at `variant = "poly"` gives
   9.00 and at `selfnorm` gives 0.50. **A changepoint that survives a change
   of noise model is evidence; one that survives a bootstrap is not.** That
   is a new diagnostic — call it `cpt_robustness(over = "noise_model")` —
   and it is a direct generalisation of `cpt_sensitivity()`, which already
   sweeps arbitrary parameters and would need only the registry's
   `noise_model_arg` column (§189.4) to know what to sweep.
2. **Test the residuals.** A Ljung-Box test on within-segment residuals is
   microseconds and answers the actual question — is the independence
   assumption violated — rather than a proxy for it. §190.4 and §194.2 both
   arrived here from other directions; this is the third.
3. **Block bootstrap in `cpt_stability()`.** The current resample is i.i.d.,
   which destroys the dependence and then reconstructs it identically in
   every replicate. A block bootstrap with a block length estimated from the
   autocorrelation would at least let the artifact vary between replicates,
   which is the minimum condition for the statistic to be informative under
   dependence.
4. **Say what the score means.** `?cpt_stability` should state that a high
   score means "reproducible under resampling", which is *not* the same as
   "real", and that under dependence the two diverge. As with
   §196.2, the documentation makes no false claim — it just does not warn
   against the inference every user will draw.

## 199. What this pass changes

| § | finding | measured how |
|---|---|---|
| 196 | consensus at the default `min_votes = 2` gives **1.40** false positives against `binseg`'s 1.10 alone; every one of `pelt`'s artifacts is reproduced by `wbs` within 5 positions | 10 reps AR(1), 3-member panel |
| 197 | `bic`/`mbic` recover exact K in **90%** of cases; **`aic` returns `k_max` at every true K**, and `k_max` defaults to 20 | 4 true K x 6 criteria x 5 reps |
| 197.2 | three of six criteria can **never** return K = 0, and none says so | pure-noise replicates |
| 198 | a spurious changepoint reaches stability **1.00**; the statistic separates real from spurious in **4 of 10** replicates | 10 reps AR(1), B = 100 |

New actions: warn on `criterion = "aic"` and document which criteria can
reach K = 0 (§197); a `cpt_robustness(over = "noise_model")` diagnostic built
on `cpt_sensitivity()` plus the `noise_model_arg` column (§198.2); a block
bootstrap option in `cpt_stability()`; residual dependence tests, now
requested by three independent sections; `min_votes` defaulting to a
proportion; a `disagreement` statistic on consensus output.

**What this pass adds.** §195 proposed that what the package owns works and
what it borrows misleads. Three more own-machinery measurements refine that
into something more useful:

- **`cpt_select()` (bic/mbic) and the bootstrap interval work.** Both are
  model-selection or resampling machinery operating *inside* the assumptions
  the engines make.
- **`cpt_consensus()` and `cpt_stability()` fail, and fail identically.**
  Both are robustness tools, and both perturb a dimension that is not the
  source of the error.

So the sharper statement is not "own good, borrowed bad" — it is that
**every diagnostic in the package perturbs the algorithm or the sample, and
the dominant error source is the noise model, which nothing perturbs.**
§189 found that the fix for a violated noise assumption is already sitting
in an argument on the engine; §198 finds that the package's two tools for
deciding whether to trust a changepoint both ignore it. The single most
valuable thing 0.6.0 could add is a diagnostic that sweeps the noise-model
argument — it reuses `cpt_sensitivity()`, needs one registry column, and
addresses the failure mode that §187, §190, §194, §196 and §198 have now all
independently landed on.

# Part V (continued) — testing the document's own top recommendation

§199 named one item as the single most valuable thing 0.6.0 could add: a
diagnostic that sweeps the noise-model argument, built on
`cpt_sensitivity()`. That is a claim about feasibility and a claim about
efficacy, and both are testable before a line is written.

## 200. Noise-model sweeping: already possible, and it removes 80-100% of the artifacts

### 200.1 Feasibility: `cpt_sensitivity()` already does it

No new sweep machinery is needed. `cpt_sensitivity(x, method = m, over =
list(<noise arg> = <values>))` works today for every engine tried, returning
a `ggcpt_sensitivity` object whose `grid` carries one row per setting. On a
single AR(1) replicate with true changepoints at 100 and 200:

| engine | setting | changepoints found |
|---|---|---|
| `smuce` | `family = "gauss"` (default) | 13: 36, 70, 100, 162, 175, 200, 210, 236, 244, 257, 268, 274, 284 |
| `smuce` | `family = "hsmuce"` | 7: 46, 71, 100, 133, 200, 255, 286 |
| `nsp` | `variant = "poly"` (default) | 10: 42, 75, 98, 149, 198, 211, 242, 256, 273, 286 |
| **`nsp`** | **`variant = "selfnorm"`** | **2: 98, 200** |
| `nsp` | `variant = "ar"` | 0 |
| `fastcpd` | `family = "mean"` (default) | 5: 46, 71, 100, 200, 264 |
| `fastcpd` | `family = "variance"` | 0 |
| `fastcpd` | `family = "meanvariance"` | 1: 150 |

`nsp` at `selfnorm` returns exactly `98, 200` against a truth of `100, 200`.
The default returns those two plus eight artifacts. **The correct answer was
one argument away, and the sweep that finds it is a function the package
already exports.**

### 200.2 Efficacy: how many artifacts does the correct noise model remove?

On **pure** AR(1) noise — no changepoint anywhere, so every detection is an
artifact — 10 replicates:

| engine | alternative setting | baseline artifacts | surviving | removed |
|---|---|---|---|---|
| `smuce` | `family = "hsmuce"` | 143 | 28 | **80%** |
| `nsp` | `variant = "selfnorm"` | 116 | 6 | **95%** |
| `nsp` | `variant = "ar"` | 116 | 0 | **100%** |

Set that beside the two robustness tools the package already has, measured
on the same problem in §196 and §198:

| axis varied | tool | artifacts removed | clean per-replicate separation |
|---|---|---|---|
| the **algorithm** | `cpt_consensus()` | ~0% (artifacts are shared) | — |
| the **sample** | `cpt_stability()` | ~0% (artifacts score 1.00) | 4 of 10 |
| **the noise model** | *nothing yet* | **80-100%** | 0-1 of 10 |

**The recommendation survives the test, and by a wide margin on the quantity
that matters.** Varying the noise model is the only one of the three axes
that touches the cause.

### 200.3 But it is a graded signal, not a classifier — and §199 overstated it

Efficacy at removing artifacts is not the same as ability to label an
individual changepoint. Scoring each baseline changepoint by the fraction of
alternative noise-model settings that also find it, over 10 AR(1)
replicates with real changepoints at 100 and 200:

| engine | survival at **real** cps | survival at **spurious** cps | ratio | replicates where every real cp outscored every spurious one |
|---|---|---|---|---|
| `smuce` | 0.667 (n = 27) | 0.190 (n = 116) | 3.5x | **0 of 10** |
| `nsp` | 0.146 (n = 24) | 0.016 (n = 96) | 9.1x | **1 of 10** |

The separation in the mean is large and in the right direction — three-and-a
-half to nine times — and the per-replicate separation is **no better than
`cpt_stability()`'s 4 of 10.** A real changepoint survives the sweep more
often than an artifact, but not reliably enough that any single changepoint
can be classified from its survival score.

`nsp`'s low absolute survival at real changepoints (0.146) has a specific
cause worth recording: one of its two alternatives is `variant = "ar"`,
which finds nothing at all (§189.2), so it drags every survival score down
including the real ones. **A survival score is only as meaningful as the
alternatives in its sweep, and an alternative with no power contributes
noise, not evidence.**

So §199's framing — "the single most valuable thing 0.6.0 could add" —
stands on artifact removal and needs qualifying on interpretation:

> **Noise-model sweeping should be reported as a downweighting signal and a
> disagreement warning, not as a real/spurious verdict.** The right output is
> "your default setting found 13 changepoints; a defensible alternative
> noise model found 7; here are the 6 that disappeared" — which is
> actionable — rather than a per-changepoint probability, which the
> measurement does not support.

### 200.4 What to build, revised

1. **`cpt_robustness(x, method, over = "noise_model")`** — a thin wrapper on
   `cpt_sensitivity()` that looks up the engine's noise-model argument from
   the registry (§189.4's `noise_model_arg` column) and sweeps its levels
   without the user needing to know the argument exists. This is the whole
   feature: one lookup and one existing call.
2. **Exclude powerless alternatives from the score.** A setting that returns
   zero changepoints on a series where the default returns thirteen is not a
   vote against those thirteen; it is a setting with no power at this signal
   strength. Report it, do not average it in.
3. **Report the set difference, not a score.** Which changepoints appear
   under every setting, which under only one, and the union-to-intersection
   ratio §196.2 already proposed for consensus. Same statistic, applied to
   the axis that works.
4. **Default the sweep in `cpt_report()`.** Given §190's numbers, a report
   that does not say how the answer changes under a different noise model is
   omitting the most consequential sensitivity there is.

## 201. The residual dependence check works, and I predicted it would not

Three sections — §190.4, §194.2, §198.2 — independently asked for a residual
autocorrelation test as the cheap way to warn a user that the assumption
underlying their segmentation is violated. Before writing it up again I
tried to falsify it, on the following reasoning: *a detector that responds
to autocorrelation by inserting twelve spurious changepoints is fitting the
noise's wiggles, so the within-segment residuals should come out white, and
the check would be blinded precisely when it is most needed.*

**That prediction is wrong.** Ljung-Box at lag 10 on within-segment
residuals, AR(1) rho = 0.7, n = 300, 12 replicates:

| engine | mean changepoints fitted | median p on fitted residuals | fraction p < 0.05 |
|---|---|---|---|
| `pelt` | 3.1 | 0 | **1.00** |
| `binseg` | 3.1 | 0 | **1.00** |
| `wbs` | 12.3 | 9.5e-12 | **1.00** |
| `smuce` | 14.1 | 2.6e-11 | **1.00** |

Even `smuce`, fitting 14.1 changepoints into 300 observations, leaves
residuals that reject whiteness in every replicate. Over-segmentation eats
some of the dependence — the median p rises from 0 to about 1e-11 — and
nowhere near enough to matter. **The check has 100% power at these settings
across four engines and 48 replicate-engine combinations.**

### 201.1 The false-alarm side needs more replicates before a number is published

On i.i.d. data with the same procedure (where all three engines correctly fit
about 2 changepoints):

| engine | median p | fraction p < 0.05 |
|---|---|---|
| `pelt` | 0.416 | 2 of 12 |
| `wbs` | 0.416 | 2 of 12 |
| `smuce` | 0.481 | 2 of 12 |

The point estimate is 0.17 against a nominal 0.05, which would be a
threefold inflation — plausibly caused by the mild negative autocorrelation
that within-segment centring induces at boundaries. But 2 of 12 cannot be
distinguished from nominal at this sample size, and the honest statement is
that **the power is established and the size is not.** Before shipping the
check, run it at 500 replicates and, if the inflation is real, use
segment-length-aware degrees of freedom or a permutation null rather than
the asymptotic chi-square.

### 201.2 Why this belongs in `diagnostics`, not as a warning

Given 100% power, an unconditional warning would fire on every
autocorrelated series, which is most real time series, and would be tuned
out within a week. The useful form is a recorded number:

1. **`diagnostics$residual_dependence`** — the Ljung-Box p-value and the
   estimated lag-1 autocorrelation of the within-segment residuals,
   populated at detection time for a few microseconds.
2. **`cpt_report()` prints it as a line**, next to the changepoint count,
   because that is where a reader decides whether to believe the count.
3. **`cpt_recommend()` reads it** when handed a fit: a user who has already
   detected once can be told "your residuals are autocorrelated; these
   engines handle that" — turning the recommender from a thing you consult
   before the analysis into a thing that responds to the analysis. That is a
   better answer to §186's collapse than any re-scoring, because it replaces
   a guess about the user's noise with a measurement of it.
4. **It is the natural trigger for §200's sweep.** Dependence detected ->
   run the noise-model sweep -> report the set difference. Three sections'
   recommendations compose into one pipeline, and every piece of it already
   exists.

## 202. What this pass changes

| § | finding | measured how |
|---|---|---|
| 200.1 | `cpt_sensitivity()` already sweeps noise-model arguments; `nsp` at `variant = "selfnorm"` returns exactly the truth where the default adds 8 artifacts | one AR(1) replicate, 3 engines |
| 200.2 | switching to the correct noise model removes **80-100%** of artifacts, against ~0% for consensus and stability | 10 pure-AR(1) replicates |
| 200.3 | **correction to §199:** mean separation is 3.5-9x but per-replicate separation is 0-1 of 10 — a downweighting signal, not a classifier | survival scores, 10 reps |
| 201 | the residual dependence check has **100% power** even at 14 fitted changepoints — my prediction that over-segmentation would blind it was wrong | Ljung-Box, 4 engines x 12 reps |
| 201.1 | its false-alarm rate is 2 of 12 on iid data; power established, size not | iid control |

New actions: `cpt_robustness(over = "noise_model")` as a thin wrapper on
existing machinery (§200.4); exclude zero-power settings from survival
scores; report set differences rather than scores;
`diagnostics$residual_dependence` populated at detection time; a
dependence-triggered sweep in `cpt_report()`; 500-replicate calibration of
the Ljung-Box size before shipping.

**What this pass adds to the method, and it is the most useful thing in it.**
For the first time the document tested *its own top recommendation* before
promoting it, and the test changed the recommendation in both directions: it
confirmed the mechanism far more strongly than expected on artifact removal
(80-100% against ~0%) and demolished the interpretation (0-1 of 10, no
better than the tool it was meant to replace). Either half alone would have
been misleading.

Tally, and it is now a pattern worth stating as a rule. Across §170.3,
§184.5, §194.1, §200.3 and §201, the document has been wrong five times in
the same direction: **the first statement of a finding is too strong, and the
error is always in the interpretation rather than the measurement.** The
numbers have held up every time they were re-measured; the sentences built
on them have not. The practical rule for the remaining passes: *measure the
mechanism, then measure whether the mechanism supports the claim* — those are
two experiments, and only the first one has been getting run.

# Part V (continued) — the metrics every comparison rests on

Every benchmark in this document, and everything `cpt_benchmark()` reports,
is computed by `cpt_metrics()`. If the metric is wrong the comparisons are
wrong, and nothing has checked it.

## 203. `cpt_metrics()` is correct; covering has a floor that depends on K

**Method.** Constructed predictions against a known truth (`100, 200`,
n = 300, `margin = 5`), then a ranking check against the error counts from
the AR(1) benchmark.

### 203.1 The constructed cases behave

| prediction | covering | F1 | precision | recall | Hausdorff | ann. error |
|---|---|---|---|---|---|---|
| = truth | 1.000 | 1.000 | 1.000 | 1.000 | 0 | 0 |
| off by 1 | 0.987 | 1.000 | 1.000 | 1.000 | 1 | 0 |
| off by 5 (= margin) | 0.936 | 1.000 | 1.000 | 1.000 | 5 | 0 |
| **off by 6 (> margin)** | 0.923 | **0.000** | 0.000 | 0.000 | 6 | 0 |
| one of two found | 0.667 | 0.667 | 1.000 | 0.500 | 100 | 1 |
| empty | 0.333 | 0.000 | 0.000 | 0.000 | **NA** | 2 |
| every valid index | 0.010 | 0.013 | 0.007 | 1.000 | 99 | 297 |
| **truth empty, pred empty** | **1.000** | **1.000** | 1.000 | 1.000 | NA | 0 |

The last row matters: the K = 0 case, which §93 and §197.2 both argue must be
a first-class answer, is scored correctly — a detector that finds nothing on
a series with nothing scores a perfect 1.000. Predicting every index is
correctly punished to 0.010. Nothing here is broken.

Two behaviours are worth documenting rather than fixing:

- **The margin is a cliff.** Off by 5 gives F1 = 1.000; off by 6 gives
  F1 = 0.000, while covering moves only from 0.936 to 0.923. Two metrics on
  the same output row disagree by everything on a one-observation change.
  That is what a matching-based F1 does and it is correct; a user reading
  both numbers should be told that one is a threshold and the other is
  continuous.
- **Hausdorff is `NA` whenever either set is empty**, which is
  mathematically right and means the metric is silently absent in exactly
  the K = 0 case. `?cpt_metrics` should say so.

### 203.2 A claim of mine, withdrawn

My first pass at this reported that covering cannot distinguish "found both
true changepoints plus eight artifacts" from "found nothing" — both scored
0.333. That was an artifact of hand-picking the eight artifact positions.
With random placement, covering degrades smoothly and monotonically:

| prediction | covering | F1 |
|---|---|---|
| truth + 0 artifacts | 1.000 | 1.000 |
| truth + 1 | 0.957 | 0.800 |
| truth + 2 | 0.777 | 0.667 |
| truth + 4 | 0.673 | 0.500 |
| truth + 8 | 0.493 | 0.333 |
| truth + 16 | 0.437 | 0.200 |
| truth + 32 | 0.220 | 0.111 |

No degeneracy. **Withdrawn** — and this is the sixth instance of the pattern
§202 named, caught this time within the same pass because the rule was
applied.

### 203.3 The real finding: covering's floor is 1/(K+1)

The empty prediction does not score zero, by deliberate design — the source
says so: *"an empty changepoint set is the trivial single-segment partition,
not a zero score."* What that means quantitatively:

| true K | covering of the empty prediction | covering of a perfect prediction | F1 of the empty prediction |
|---|---|---|---|
| 1 | **0.500** | 1.000 | 0.000 |
| 2 | 0.333 | 1.000 | 0.000 |
| 5 | 0.167 | 1.000 | 0.000 |
| 10 | 0.091 | 1.000 | 0.000 |

**On a single-changepoint problem, a detector that finds nothing scores 0.500
on covering.** That reads as "half right" and is a total failure. The floor
is `1/(K+1)`, so it falls as the problem gets harder — which has a
consequence nobody would guess from the help page:

> **Covering values are not comparable across problems with different
> numbers of changepoints.** A method scoring 0.5 on a K = 1 dataset has
> done nothing; the same 0.5 on a K = 10 dataset is five times the floor.

### 203.4 `cpt_benchmark()` already gets the aggregation right

This is where the section turns positive. The obvious way to combine
covering across datasets — average the values — would be invalidated by a
K-dependent floor. `cpt_benchmark()` does not do that: it computes
`aggregate(rank ~ method, FUN = mean)`, averaging **ranks within each
dataset**. Ranks are floor-invariant, so the aggregation is immune to the
problem by construction. Whether that was deliberate or lucky, it is right,
and it should be documented as the reason rather than left as an
implementation detail somebody later "simplifies" into a mean.

### 203.5 And the metrics do rank engines correctly

The check that matters for every benchmark in this document: do the metrics
order engines the way the underlying error counts do? AR(1), 10 replicates,
six engines:

| method | hits (of 2) | false positives | covering | F1 |
|---|---|---|---|---|
| `binseg` | 1.9 | 1.1 | 0.903 | 0.737 |
| `pelt` | 1.9 | 1.2 | 0.906 | 0.684 |
| `not` | 1.9 | 9.6 | 0.460 | 0.259 |
| `wbs` | 1.9 | 9.7 | 0.493 | 0.271 |
| `cpm` | 1.9 | 11.2 | 0.393 | 0.218 |
| `smuce` | 2.0 | 11.6 | 0.374 | 0.236 |

**Spearman correlation with false-positive count: -0.89 for covering, -0.89
for F1.** Both strongly negative, as they must be. The two rankings differ
only in adjacent swaps where the underlying difference is inside the noise
(1.1 against 1.2 false positives; 9.6 against 9.7). The metrics are
measuring the thing.

### 203.6 What to build

1. **Document the floor.** `?cpt_metrics` should state that covering of an
   empty prediction is `1/(K+1)`, that the floor therefore varies with the
   problem, and that covering should not be averaged across datasets with
   different K. Three sentences.
2. **Report the floor alongside the value.** `cpt_metrics()` knows `truth`,
   so it can return `covering_floor` for free. A covering of 0.5 next to a
   floor of 0.5 is self-explanatory; a covering of 0.5 alone is not.
3. **Document why `cpt_benchmark()` ranks rather than averages** (§203.4),
   so the reason survives future refactoring.
4. **Say which metrics are thresholded and which are continuous**, given the
   margin cliff — a one-line note in the returned tibble's documentation.
5. **Consider a scaled covering** — `(covering - floor) / (1 - floor)` — as
   an optional column, which is comparable across K and is what a user
   averaging over a benchmark suite actually wants.

## 204. What this pass changes

| § | finding | measured how |
|---|---|---|
| 203.1 | all constructed cases behave, including K = 0 scoring a perfect 1.000; the margin is a hard cliff (F1 1.000 -> 0.000 for one observation) | 12 constructed predictions |
| 203.2 | **withdrawn:** covering does not collapse when artifacts are added; it degrades smoothly 1.000 -> 0.220 | random artifact placement |
| 203.3 | covering's floor is **1/(K+1)** — an empty prediction scores **0.500** at K = 1 — so covering is not comparable across K | 4 values of K |
| 203.4 | `cpt_benchmark()` averages **ranks**, which is floor-invariant and already correct | source inspection |
| 203.5 | covering and F1 both correlate at **Spearman -0.89** with false-positive count | 6 engines x 10 AR(1) reps |

New actions: document covering's floor and return it as a column; document
why the benchmark ranks rather than averages; flag thresholded versus
continuous metrics; an optional floor-scaled covering.

**What this pass adds.** This is the fourth piece of the package's own
machinery to be measured and the third to pass: the bootstrap interval
(§193), `bic`/`mbic` selection (§197), and now `cpt_metrics()` and
`cpt_benchmark()`'s aggregation. Against that, the two that failed —
`cpt_consensus()` and `cpt_stability()` (§196, §198) — are both *robustness*
tools, and both fail for the single reason §198.1 identified. The picture
that has emerged over five passes is unusually clean:

> **Everything the package computes is correct. Everything it computes about
> whether to *trust* a result is measuring the wrong thing, and the one axis
> that works (§200) is the one nothing sweeps.**

That is a much more actionable summary for 0.6.0 than a list of features. The
estimation, selection, interval and evaluation machinery needs documentation
and small fixes. The trust machinery needs a new axis, and §200.1 established
that the axis is one registry column and one existing function call away.

# Part V (continued) — the streaming path, never measured

Five passes have measured the offline machinery. `cpt_monitor()`,
`cpt_update()`, `alarms()` and `cpt_delay()` are a separate subsystem with
its own canonical quantities — average run length to a false alarm (ARL0)
and detection delay — and neither has ever been measured. §75 (Theme AS)
argued that statistical process control is the same mathematics under a
different name; this is that theme with numbers attached.

## 205. ARL0 is off by 3x for one monitor, right for another, and `ocd` cannot run at all

**Method.** `cpt_monitor(m, baseline = <200 clean observations>)` at the
package defaults (`alpha = 0.01`, `arl0 = 500`), then a single
`cpt_update()` with a 2000-observation in-control stream. 15 replicates.
With `arl0 = 500`, a correctly calibrated monitor should raise about
`2000 / 500 = 4` false alarms per stream.

| monitor | median index of first false alarm | false alarms per 2000-obs stream | expected at `arl0 = 500` |
|---|---|---|---|
| `edetector` | **61** | **13.0** | 4 |
| `cpm` | 142 | **3.7** | 4 |
| `ocd` | — | **errors** | — |

**`cpm` is well calibrated**: 3.7 alarms where 4.0 are expected. That is a
genuine positive result and the first calibration claim in this document to
come in on target without qualification.

**`edetector` alarms 3.3 times too often** — 13.0 against 4.0 — and its
first false alarm arrives at a median index of 61 on a stream that is in
control throughout. Reading the constructor explains why: `edetector` is
driven by `alpha`, and `2000 x 0.01 = 20` is the order of magnitude
observed. So the behaviour is consistent with `alpha` and inconsistent with
`arl0`, which brings the third finding.

### 205.1 `arl0` is in the signature and not all methods use it

`cpt_monitor(method, baseline, alpha = 0.01, arl0 = 500, ...)` presents both
knobs to every method. `cpm` honours `arl0`; `edetector` is governed by
`alpha`. A user who sets `arl0 = 5000` to make an `edetector` monitor less
trigger-happy will change nothing, silently — and a user who sets
`alpha = 0.001` on a `cpm` monitor is equally in the dark about which knob
bites.

This is the §181.1 and §189 pattern in a third place: an argument accepted,
validated, and then ignored by the method that received it. The fix is the
same shape — either error when a method is handed a parameter it does not
consume, or document per method which of the two governs it, and say in
`?cpt_monitor` that `arl0` and `alpha` are alternative parameterisations
rather than independent controls.

### 205.2 `cpt_monitor("ocd")` cannot accept a univariate stream

`method = c("edetector", "cpm", "ocd")` offers three peers. Handed a
univariate baseline and stream, the third fails:

> Method `ocd` is high-dimensional and needs at least two coordinates.

The error is correct and honest — `ocd` is a high-dimensional online
detector — but it is raised after the user has chosen it from a list of
three that gives no hint that one is unavailable for the commonest input.
`match.arg()` is exactly the wrong mechanism here, because it advertises the
option and defers the refusal. `?cpt_monitor` should mark `ocd` as
multivariate-only in the `@param method` entry, and the constructor should
say so before touching the data.

### 205.3 Detection delay is good, and worth publishing

The other half of the SPC contract, measured on a clean stream with a change
injected at index 300:

| monitor | delta = 1 | delta = 2 | delta = 3 | missed |
|---|---|---|---|---|
| `edetector` | 12 | 4 | **2** | 0-1 of 15 |
| `cpm` | 11 | 5 | 4 | 2 of 15 |

Median delays of two to twelve observations, with almost no misses. That is
a competitive result and the package says nothing about it. `cpt_delay()`
already computes and prints mean delay, median delay, false alarms and
average run length — the machinery to publish this table exists and has
never been pointed at the package's own monitors.

## 206. Under AR(1) the monitors fail the same way the offline detectors do

Same design, but the baseline and the stream are both AR(1) with rho = 0.7
and still in control — no change anywhere.

| monitor | false alarms per stream, iid | false alarms per stream, AR(1) | inflation | median first alarm, AR(1) |
|---|---|---|---|---|
| `edetector` | 13.0 | **50.9** | 3.9x | 40 |
| `cpm` | 3.7 | **37.9** | **10.2x** | **1** |

`cpm` — the one monitor that was well calibrated under independence — raises
ten times too many alarms under mild dependence, and its median first false
alarm arrives at **stream index 1**. The baseline was also AR(1), so the
in-control estimate is not the problem; the first observation of the
monitored stream already trips the threshold.

This is §190 in the streaming setting, and the consequence is sharper
because a monitor is designed to run unattended. An offline detector that
over-segments produces a plot somebody looks at. A monitor that raises 38
false alarms per 2000 observations produces a pager that gets muted, and
then the real change is missed for a reason that never appears in any log.

### 206.1 What to build

1. **Publish the ARL0 and delay tables** (§205, §205.3) as package data and
   in the monitoring vignette. These are the two numbers the SPC literature
   requires of any monitor, `cpt_delay()` already computes both, and no
   user can currently obtain them without writing the experiment above.
2. **Calibrate `edetector`'s threshold against `arl0`**, or rename the knob.
   Being 3.3x off a declared ARL0 is a defect in a stated quantity, unlike
   the dependence failures, which are a stated-assumption problem.
3. **A dependence check on the baseline, at construction time.** The monitor
   is handed a clean baseline before any streaming starts — that is the
   single best moment in the whole API to run §201's Ljung-Box test, because
   it costs nothing, happens once, and the answer determines whether the
   threshold about to be used means anything. If the baseline is
   autocorrelated, say so then, not after 38 alarms.
4. **Offer a dependence-aware monitor.** `decafs` and `envcpt` were the
   engines that held at size 0.00 under AR(1) offline (§190.2); neither is
   available as a monitor. A streaming detector that models AR noise is the
   one obvious gap in the online subsystem, and it is the same
   noise-model-axis conclusion §200 reached offline.
5. **Mark `ocd` multivariate-only in the method list** (§205.2).

## 207. What this pass changes

| § | finding | measured how |
|---|---|---|
| 205 | `cpm` raises **3.7** false alarms per 2000-obs in-control stream against 4.0 expected — correctly calibrated; `edetector` raises **13.0**, 3.3x too many | 15 reps, iid stream |
| 205.1 | `arl0` is offered to every method and only some consume it; `edetector` is governed by `alpha` | constructor inspection + rates |
| 205.2 | `cpt_monitor("ocd")` errors on univariate input, after being offered as one of three peers | univariate stream |
| 205.3 | median detection delay of **2-12 observations** at delta = 1-3, almost no misses — a competitive result the package never reports | change injected at index 300 |
| 206 | under AR(1), `edetector` inflates **3.9x** and `cpm` **10.2x**, with `cpm`'s first false alarm at **stream index 1** | 15 reps, AR(1) baseline and stream |

New actions: publish ARL0 and delay tables from the existing
`cpt_delay()` machinery; calibrate `edetector` against `arl0` or rename the
knob; run the baseline dependence check at construction; mark `ocd`
multivariate-only; add a dependence-aware monitor.

**What this pass adds.** The streaming subsystem turns out to have the same
two-part character as the offline one, which is worth stating because it
means the diagnosis generalises rather than needing to be redone:

- **What it computes is good.** Detection delays of 2-12 observations,
  `cpm` calibrated to within 8% of its declared ARL0, `cpt_delay()`
  computing every quantity the SPC literature asks for.
- **What it assumes is unchecked, and the assumption is the same one.**
  Mild autocorrelation inflates false alarms by 4x to 10x, exactly as it
  does offline.

And it adds one thing the offline analysis could not see. Offline, a
dependence check has to be run on residuals after the fact. **A monitor is
handed a clean baseline before any decision is made** — so the streaming API
has a natural, free, once-per-monitor place to test the assumption that the
offline API lacks. That makes `cpt_monitor()` the best possible first home
for §201's residual check, and it is the only place in the package where the
check can run *before* a wrong answer is produced rather than after.

# Part V (continued) — the performance table, finally measured

§23 asked for runtimes at n = 10^4/10^5/10^6. §0.9 deferred it to "the
software paper". Issue #14 lists it as deferred infrastructure. Three passes
have said that chunked detection cannot be designed without knowing where
the cliff is. Nobody had run it. Here it is at n = 1,000 / 10,000 / 100,000.

## 208. Ten engines handle 100,000 points; six do it in under a quarter of a second

**Method.** One series per length with four true changepoints and 2-sigma
shifts. **Each (engine, n) cell runs in its own R process under an external
`timeout`**, so a hang or a C-level stall kills only that cell. One
replicate per cell — this is a runtime table, not a statistical estimate.

### 208.1 A methodological correction, first, because it changes how to read the table

An earlier batch of this measurement ran 69 cells sequentially while I was
also running foreground probes. Contention inflated the borderline cells,
and two readings I would otherwise have reported were wrong:

- the batch reported `smuce` **timing out at n = 1,000**; re-measured
  serially on an idle machine it takes **0.97 s**;
- an earlier probe reported `smuce` at **61.4 s** for n = 800, from which I
  inferred a 90x cliff between n = 400 and n = 800. Clean re-measurement:
  **0.86 s at n = 800**, and a flat 0.89 / 0.86 / 0.96 / 0.97 s across
  n = 700 / 800 / 900 / 1,000. **There is no cliff. Withdrawn.**

The asymmetry is worth stating as a rule for anyone re-running this:
**contention can only inflate a timing, so a fast reading is a valid upper
bound and a slow reading is not evidence of anything.** Every cell reported
as failing below was therefore re-measured serially at a 120 s cap; the fast
cells are reported as first measured.

### 208.2 The table

Times in seconds, `k` is the number of changepoints reported against a truth
of **4**:

| engine | n = 1,000 | k | n = 10,000 | k | n = 100,000 | k |
|---|---|---|---|---|---|---|
| `amoc` | 0.036 | 1 | 0.038 | 1 | **0.052** | 1 |
| `binseg` | 0.038 | 4 | 0.041 | 4 | **0.073** | 4 |
| `fpop` | 0.036 | 4 | 0.039 | 4 | **0.075** | 4 |
| `pelt` | 0.037 | 4 | 0.040 | 4 | **0.083** | 4 |
| `mosum` | 0.150 | 4 | 0.148 | 4 | **0.179** | 5 |
| `binsegrcpp` | 0.173 | 4 | 0.183 | 4 | **0.216** | 4 |
| `decafs` | 0.239 | 1 | 0.337 | 4 | 0.600 | 4 |
| `cpm` | 0.200 | 5 | 0.201 | **36** | 0.720 | **350** |
| `not` | 0.203 | 4 | 0.369 | 4 | 2.084 | 4 |
| `wbs` | 0.168 | 4 | 0.382 | 4 | 2.448 | 4 |
| `tguh` | 0.265 | 4 | 0.670 | 4 | 4.150 | 4 |
| `idetect` | 0.133 | 4 | 0.375 | 4 | 6.395 | 4 |
| `pettitt` | 0.196 | 1 | 0.425 | 1 | 35.5 | 1 |
| `np` | 0.064 | 4 | 2.073 | 4 | 91.0 | **10** |
| `wbs2` | 1.173 | 4 | 11.8 | 4 | **fails** | — |
| `segneigh` | 0.280 | 4 | 12.9 | 4 | **fails** | — |
| `hsmuce` | 0.224 | 4 | 28.4 | 4 | **fails** | — |
| `smuce` | 0.97 | 4 | 56.5 | 4 | **fails** | — |
| `cpop` | 2.566 | 8 | **>120 s** | — | **fails** | — |
| `strucchange` | 3.006 | 4 | **>120 s** | — | **fails** | — |
| `bocpd` | 4.337 | 4 | **>120 s** | — | **fails** | — |
| `taylor` | 10.5 | 5 | **>120 s** | — | **fails** | — |
| `ecp` | 21.7 | 4 | **>120 s** | — | **fails** | — |

`amoc`, `pettitt` and `decafs`-at-1,000 report one changepoint because they
are single-change designs, not because they failed.

### 208.3 What the table says

**Six engines segment 100,000 observations in under a quarter of a second,
with the correct answer**: `amoc`, `binseg`, `fpop`, `pelt`, `mosum`,
`binsegrcpp`. Four more finish in seconds: `not`, `wbs`, `tguh`, `idetect`.
**Ten engines are usable at n = 100,000, and four of them are essentially
free.** That is a much better answer than the deferrals implied, and it is
the single most useful fact in this table for a user with a long series.

**Five engines cannot reach n = 10,000**: `cpop`, `strucchange`, `bocpd`,
`taylor`, `ecp`, all confirmed over 120 s in a clean serial re-run. For two
of them the mechanism is already documented in this file: `strucchange`
holds a triangular O(n^2) RSS matrix (§1708 measured 135 MB at n = 2,000,
which extrapolates to ~3.4 GB at n = 10,000), and `bocpd` keeps an n x n
run-length matrix (§170.2 measured 201 x 201 at n = 200, which is 8 x 10^10
doubles at n = 100,000).

**`segneigh` used 1,208 MB at n = 10,000**, against 42-62 MB for `pelt`,
`fpop` and `binseg` at *every* size. That is the O(n^2) memory cost showing
up in a third engine, and it is the reason it fails at 100,000 rather than
merely being slow.

### 208.4 The finding that matters most: `cpm` reports 350 changepoints at n = 100,000

| n | truth | `cpm` reports | time |
|---|---|---|---|
| 1,000 | 4 | 5 | 0.200 s |
| 10,000 | 4 | **36** | 0.201 s |
| 100,000 | 4 | **350** | 0.720 s |

The count is about **0.35% of n**, independent of the truth. That is the
signature of a sequential test applied at a fixed per-observation
significance level: the expected number of alarms grows linearly with the
stream length, which is correct behaviour for a monitor and wrong behaviour
for an offline segmenter.

**And it is fast.** 0.72 s at n = 100,000 means nothing about the runtime
warns the user. This is the offline counterpart of §205's ARL0 discussion:
`cpm` is a sequential detector wrapped as an offline one, its false-positive
count scales with `n`, and `cpt_detect()` presents it beside `pelt` as a
peer. §176's cost columns would not catch this because the problem is not
cost; §187's noise table would not catch it because it was measured at
n = 300 where 0.35% is one changepoint.

The fix is a per-observation-rate correction — `cpm`'s `arl0` argument
exists and the wrapper defaults it — and, failing that, a warning when a
method's reported count exceeds a plausible fraction of `n`.

### 208.5 What to build

1. **Ship this table** as package data and in a vignette, keyed by engine
   and `n`. It answers "will this finish?", which is the question that
   precedes every statistical one, and §176's proposed `cost` columns should
   be populated from it rather than from the n = 200 timings that section
   used.
2. **`cpt_recommend()` should refuse to recommend an engine that cannot run
   at the user's `n`.** It already takes `n`. The current `slow` list is a
   hand-written five-name vector (§176) that contains a name matching no
   method and omits every engine in the "fails" rows above.
3. **A count-versus-`n` sanity warning.** Any engine reporting more than
   roughly `n/100` changepoints is almost certainly misconfigured rather than
   informative — that single check catches `cpm` at every scale and would
   have caught §190's AR(1) blow-ups too.
4. **Chunked detection is now designable**, which is what §0.9 said it was
   waiting for. The cliff is between n = 10,000 and n = 100,000 for five
   engines and below n = 10,000 for five more, and the ten engines that
   scale need no chunking at all. So chunking is not a general feature: it
   is a compatibility shim for a specific list of ten engines, and the list
   is now known.
5. **Re-run at n = 10^6 on a machine with more memory**, and serially. The
   1,208 MB at n = 10,000 for `segneigh` suggests memory rather than time is
   the binding constraint at the next decade, which changes what the shim
   has to do.

### 208.6 One cell corrected by the serial re-run, and it proves the rule from §208.1

Every cell the batch reported as failing was re-measured serially at a 120 s
cap. Eleven of twelve confirmed: `bocpd`, `cpop`, `ecp`, `strucchange` and
`taylor` all exceed 120 s at n = 10,000, and `smuce`, `hsmuce`, `wbs2`,
`segneigh` and `cpop` all exceed it at n = 100,000. Two changed:

| cell | batch reading | clean serial reading |
|---|---|---|
| `smuce` at n = 10,000 | > 45 s | **56.5 s** — slow, not impossible |
| **`np` at n = 100,000** | **fails** | **91.0 s, reporting 10 changepoints** |

The `np` row of §208.2 has been corrected accordingly. It matters twice
over: `np` *is* usable at 100,000 if a user will wait a minute and a half,
and it reports **10 changepoints against a truth of 4** — over-segmentation
that only appears at that scale, since `np` is exactly right at 1,000 and
10,000.

So the count-versus-`n` problem of §208.4 is not unique to `cpm`. It is
milder in `np` (10 rather than 350) and it has the same shape: an engine
whose false-positive rate is calibrated at moderate `n` and drifts as `n`
grows. **The sanity check proposed in §208.5 item 3 would flag `cpm` loudly
and `np` quietly, which is the correct ordering.**

And `smuce` at n = 10,000 taking 56.5 s, against 0.97 s at n = 1,000, is a
58x increase for a 10x increase in `n` — an exponent near 1.76, slow but
polynomial. Combined with §208.1's withdrawal of the imagined cliff, the
`smuce` story is simply "consistently slow", with no discontinuity anywhere.

**This is the eighth instance of the §202 pattern, and the first where the
correction was produced by a procedure adopted specifically to catch it.**
The rule from §208.1 — re-measure every slow reading serially, because
contention only inflates — was written before the re-run finished, and the
re-run then found exactly one wrong cell out of twelve. That is the first
time in this document that a stated methodological safeguard has paid for
itself within the same pass.

## 209. What this pass changes

| § | finding | measured how |
|---|---|---|
| 208.2 | **ten engines are usable at n = 100,000**; six finish in under 0.25 s with the correct count | 23 engines x 3 lengths, one process per cell |
| 208.2 | five engines cannot reach n = 10,000: `cpop`, `strucchange`, `bocpd`, `taylor`, `ecp` | confirmed serially at 120 s |
| 208.3 | `segneigh` uses **1,208 MB** at n = 10,000 against 42-62 MB for `pelt`/`fpop`/`binseg` at every size | `gc()` max-used per cell |
| 208.4 | **`cpm` reports 350 changepoints at n = 100,000** where there are 4, in 0.72 s — about 0.35% of `n`, independent of the truth | three lengths |
| 208.6 | `np` at n = 100,000 completes in 91 s reporting **10** — the same drift, milder | serial re-measurement |
| 208.1 | **withdrawn:** the `smuce` "cliff" between n = 400 and n = 800 was machine contention; it is flat at ~0.9 s from n = 700 to 1,000 | clean serial sweep |

New actions: ship the table as package data and populate §176's `cost`
columns from it; make `cpt_recommend()` refuse engines that cannot run at the
user's `n`; add a count-versus-`n` sanity warning at roughly `n/100`; scope
chunked detection as a shim for the ten engines that need it rather than a
general feature; re-run at n = 10^6 with memory as the binding constraint.

**What this pass adds.** It closes a debt the document has carried since §23
and that issue #14 still lists as deferred, and the answer is more
encouraging than the deferrals implied: **the scaling problem is much smaller
than assumed.** Ten of twenty-three engines handle 100,000 observations, four
of them for free, and the ones that cannot are identifiable by a property
already documented in this file — an O(n^2) matrix in the return value
(`strucchange`'s RSS triangle, `bocpd`'s run-length matrix, `segneigh`'s
cost matrix). **Scaling is not a research problem for this package; it is a
routing problem**, and §176's `cost` column plus a hard refusal in
`cpt_recommend()` solves it.

The unexpected finding is the other one. §187 measured false positives at
n = 300 and §190 at n = 300, and both were blind to a failure mode that only
appears at scale: **an engine whose reported count is a fixed fraction of the
series length.** At n = 300 `cpm` looks fine, at n = 100,000 it reports 350
changepoints where there are 4. Every benchmark in this document was run at
n = 200-400. That is the same lesson as §191's "the test suite never leaves
iid noise", one axis over: **the test suite also never leaves n < 1,000**, and
both blind spots hid a defect that a single cheap sweep exposes.

# Part V (continued) — the same measurements, at the sizes real data comes in

§209 named the second blind spot: every benchmark in this document ran at
n = 200-400. §208.4 found a defect that only appears at scale. This pass
re-runs the two measurements that matter at n = 1,000 / 10,000 / 100,000.

## 210. `cpm` is not miscalibrated — `arl0 = 500` is doing exactly what it says, and one argument fixes it

§208.4 reported that `cpm` returns 350 changepoints at n = 100,000 where
there are 4, and called it a penalty-scaling defect. That framing was wrong,
and the correct one is more useful.

`cpm_wrapper()` takes `arl0 = 500` — an *average run length* between false
alarms, the sequential-monitoring parameterisation. Under those semantics the
expected number of false alarms in a series of length `n` is `n / arl0`,
**by construction**. Measured on pure noise:

| n | `arl0` | expected `n/arl0` | observed | ratio |
|---|---|---|---|---|
| 1,000 | 500 | 2.0 | 0 | — |
| 10,000 | 500 | 20.0 | 26 | 1.30 |
| 100,000 | 500 | 200.0 | 336 | 1.68 |
| 10,000 | 50,000 | 0.2 | **0** | — |
| 100,000 | 50,000 | 2.0 | **2** | **1.00** |

The engine honours its declared ARL0 to within a factor of 1.7. It is not
broken; it is a sequential monitor being presented by `cpt_detect()` as an
offline segmenter, with a default tuned for streams rather than for series.

### 210.1 The fix, and it is complete

Setting `arl0` proportional to `n` — about `5n` — on a series that genuinely
has four changepoints:

| n | `arl0` | changepoints reported | of the 4 true ones found |
|---|---|---|---|
| 10,000 | 500 (default) | **36** | 4 / 4 |
| 10,000 | 50,000 | **4** | **4 / 4** |
| 100,000 | 500 (default) | **350** | 4 / 4 |

At `arl0 = 5n` the answer goes from *36 changepoints of which 4 are real* to
*exactly the 4 real ones*, with no loss of power at all. One argument.

This is the third instance of §189's pattern — `smuce`'s `family`, `nsp`'s
`variant`, and now `cpm`'s `arl0` — and the cleanest, because the
relationship is quantitative rather than qualitative: **false positives are
about `1.5 n / arl0`, so `arl0 = 5n` buys an expected 0.3 of them.** A
wrapper that scaled `arl0` with `n` by default would be right for every
offline use, and one upper bound is worth recording: `arl0 = 500,000` errors
out of `cpm`'s internal tables, so the scaling needs a cap.

### 210.2 What this changes in the recommendations

§208.5 proposed a "count exceeds n/100" warning. That is still worth having
as a backstop, but it is the wrong primary fix here — the count is
*predictable from the arguments*, so the package can compute the expected
false-positive count before running anything and either scale the parameter
or say what to expect. Concretely:

1. **Scale `arl0` with `n` in `cpm_wrapper()`**, defaulting to something like
   `min(5 * n, 20000)`, and document that the streaming default is preserved
   only when the user sets it explicitly.
2. **Add an `expected_false_positives` field to `$diagnostics`** for any
   engine whose parameterisation permits the calculation. `cpm` permits it
   exactly; `nsp` permits it approximately through `alpha`. That is a number
   no changepoint package offers and it is free where it exists.
3. **This is a fourth entry for the `noise_model_arg` column** (§189.4) —
   or rather it shows the column needs a sibling: `rate_arg`, the argument
   that governs an engine's false-alarm rate, with a note on whether it
   should scale with `n`.

## 211. Empirical size under the null, at scale: ten engines are perfectly clean at n = 100,000

§190 measured the empirical size at n = 300 and found 28 of 42 engines never
false-alarming under i.i.d. noise. The obvious worry after §208.4 was that
this was an artifact of the small size. It is not.

**Method.** Pure i.i.d. noise, **no changepoint anywhere**, 6 replicates per
cell, one process per cell, run serially. `size` is the fraction of
replicates reporting at least one changepoint.

| engine | n = 1,000 size / mean count | n = 10,000 | n = 100,000 |
|---|---|---|---|
| `amoc` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `binseg` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `fpop` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `pelt` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `binsegrcpp` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `decafs` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `not` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `wbs` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `tguh` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `idetect` | 0.000 / 0.00 | 0.000 / 0.00 | **0.000 / 0.00** |
| `mosum` | 0.000 / 0.00 | 0.167 / 0.17 | 0.000 / 0.00 |
| `segneigh` | 0.000 / 0.00 | — | — |
| `pettitt` | 0.167 / 0.17 | 0.000 / 0.00 | — |
| `wbs2` | 0.167 / 0.17 | 0.167 / 0.17 | — |
| `smuce` | 0.167 / 0.17 | 0.333 / 0.33 | — |
| `hsmuce` | 0.167 / 0.17 | 0.333 / 0.33 | — |
| **`np`** | 0.333 / 1.17 | **1.000 / 6.17** | — |
| **`cpm`** | 0.667 / 1.50 | **1.000 / 33.83** | **1.000 / 365.83** |

**Ten engines raised not one false alarm across six replicates of 100,000
observations each — six hundred thousand observations apiece, zero
detections.** §190's good news survives three orders of magnitude, and that
is worth stating as a positive claim the package can make: under its stated
assumptions the core detectors are extremely conservative, and their
calibration does not decay with series length.

Only two engines drift upward with `n`:

- **`cpm`**: 1.50 -> 33.83 -> 365.83, which §210 now explains completely as
  `arl0` semantics.
- **`np`**: 1.17 -> 6.17, and §208.6 measured 10 at n = 100,000. `changepoint.np`'s
  default penalty is not holding as `n` grows. This one has no `arl0`-style
  explanation and is the residual finding of the pass: **`np` is in the
  recommender's preferred list for both heavy-tailed and heteroscedastic
  noise (§187.1), and at n = 10,000 it reports six changepoints on pure
  noise in every replicate.**

`smuce` and `hsmuce` rising from 0.167 to 0.333 is one extra alarm in six
replicates and is inside the noise at this replicate count.

### 211.1 What to build

1. **Publish this table too.** Together with §208's runtimes it answers the
   two questions a user has before any statistical one: will it finish, and
   will it invent changepoints. Neither is currently answerable from the
   package.
2. **Investigate `np`'s default penalty** as a defect of the same class as
   §210's, but without the excuse: `changepoint.np`'s quantile-based cost
   should be paired with a penalty that scales, and it evidently is not.
3. **Raise the replicate count to 100 before publishing sizes.** Six
   replicates resolves size to 0.167, which is enough to separate 0.00 from
   1.00 and not enough to distinguish 0.167 from 0.05. The zeros are the
   solid part of this table; the middle rows are indicative.
4. **Add n = 10^4 and 10^5 cases to the test suite**, not as accuracy
   assertions but as a recorded null-behaviour benchmark. §191 said the suite
   never leaves i.i.d. noise; §209 said it never leaves n < 1,000. This
   table is the second gap closed with a script, and keeping it closed costs
   one scheduled job.

## 212. What this pass changes

| § | finding | measured how |
|---|---|---|
| 210 | **correction to §208.4:** `cpm` honours its declared ARL0 to within 1.7x; the 350 changepoints are `n/arl0` by construction, not a defect | pure noise at 3 lengths x 3 `arl0` values |
| 210.1 | `arl0 = 5n` turns *36 changepoints of which 4 are real* into *exactly 4*, with no power loss | signal series, n = 10,000 |
| 211 | **ten engines raise zero false alarms across 600,000 pure-noise observations each**; iid calibration does not decay with `n` | 6 reps x 3 lengths, no changepoint |
| 211 | `np` drifts from 1.17 to 6.17 spurious changepoints between n = 1,000 and 10,000, with no parameterisation to explain it | same |

New actions: scale `cpm`'s `arl0` with `n`; an `expected_false_positives`
diagnostic where the parameterisation permits it; a `rate_arg` registry
column beside `noise_model_arg`; investigate `np`'s default penalty; publish
the size table; 100 replicates before quoting sizes; large-`n` null cases in
the suite.

**What this pass adds.** Two things, pulling in opposite directions, which is
why both are worth having.

The correction is the more instructive. §208.4 called `cpm`'s 350
changepoints "the finding that matters most" and diagnosed a penalty-scaling
defect. One cheap experiment — vary the argument and see whether the count
follows `n/arl0` — showed the engine is doing precisely what it was told, and
that the fix is one argument with no cost. **The defect was real and the
diagnosis was wrong**, which is the ninth instance of §202's pattern and the
second in consecutive passes. The rule stated in §202 has now earned a
stronger form: *before calling a number a defect, check whether some argument
predicts it.* Three of the last four "defects" — `smuce`'s false positives,
`nsp`'s level violation, `cpm`'s count — were all arguments doing their job
with a default chosen for a different task.

The positive result is the more useful for the roadmap. **Under i.i.d. noise
the ten scaling engines are perfectly clean at 100,000 observations.** Every
serious problem this document has found in the last ten passes — §179's scale
sensitivity, §190's AR(1) collapse, §196 and §198's failed robustness tools,
§205's monitor inflation — is about a *violated assumption*, never about the
core algorithms. That narrows 0.6.0's brief to a single sentence: **the
estimators are sound and the package tells nobody when their assumptions do
not hold.** Everything measured since §187 points at the same missing
feature, and §200.1 established it is one registry column and one existing
function call away.

# Part V (continued) — the pipeline, end to end, and what it should actually do

Every pass since §187 has converged on one missing feature, and §200.1
established the pieces exist. Before specifying it, the whole pipeline needs
running once: detect, check the assumption, respond to the failure, report.

## 213. The assumption check fires every time — and every automatic correction costs more power than it saves

**Method.** n = 500, real changepoints at 170 and 340, 2-sigma shifts, AR(1)
with rho = 0.7, 12 replicates, tolerance +/-12. Detect with the default,
run §201's Ljung-Box check on the within-segment residuals, then compare
every response available to the package.

**The check fired in 12 of 12 replicates.** §201's 100% power holds at this
size too. Detecting the violated assumption is a solved problem.

The responses are not:

| route | hits (of 2) | false positives | spurious cps on **pure** AR(1) noise |
|---|---|---|---|
| `pelt` (the default) | 1.75 | 1.75 | 1.33 |
| `smuce` (default `family`) | **2.00** | **20.08** | 22.58 |
| `smuce`, `family = "hsmuce"` | 1.75 | 5.17 | 6.00 |
| `decafs` (AR-aware engine) | **0.67** | 0.25 | **0.00** |
| `envcpt` (AR-aware ensemble) | 1.08 | 0.17 | **0.00** |
| `pelt` INTERSECT `decafs` | **0.50** | **0.08** | **0.00** |

### 213.1 Switching to a dependence-aware engine is not a free lunch

§190.2 found `decafs` and `envcpt` at size 0.00 under AR(1), and this pass
confirms it — zero spurious changepoints per replicate on pure noise, the
only two routes that achieve that. But on a series that *does* contain
changepoints, `decafs` recovers **0.67 of 2** where `pelt` recovers 1.75.
It removed 1.50 false positives and lost 1.08 true ones. `envcpt` is better
at 1.08 hits and still misses nearly half.

And the intersection pipeline — take the default's changepoints, keep only
those a dependence-aware engine also finds — is the **worst** option for
recall: 0.50 hits. It inherits `decafs`'s misses and then demands agreement
on top, which is §196's lesson arriving from the other side: an intersection
is bounded above by its weakest member's recall.

**So an automatic correction cannot be the feature.** Every substitution
measured here trades roughly one true changepoint for one-to-two false ones,
and which side of that trade a user wants is not something the package can
decide.

### 213.2 What the measurement does support

Three things, all of them reporting rather than deciding:

1. **`pelt` is already among the most robust defaults.** 1.75 false positives
   at n = 500 under AR(0.7), against `smuce`'s 20.08. §190 measured this at
   n = 300 and it holds at 500. So the first honest response to a failed
   assumption check is often *"your current answer is fine; here is why"* —
   not a substitution.
2. **`smuce` is the disaster case and it has its own fix.** 22.58 spurious
   changepoints per pure-noise replicate at the default `family`, 6.00 at
   `family = "hsmuce"`, with hits only falling 2.00 to 1.75. For engines
   that carry a noise-model argument, sweeping it is a genuine improvement
   with a small, measurable power cost. That is §189 and §200 confirmed a
   third time.
3. **The dependence-aware engines are the right thing to *name*, not to
   substitute.** "Your residuals are autocorrelated; `decafs` and `envcpt`
   model that and report 0.00 spurious changepoints on noise like yours, at
   the cost of roughly half the detection power" is a complete, honest,
   actionable sentence. Running `decafs` silently instead of `pelt` is not.

## 214. The consolidated 0.6.0 specification, derived from measurement

§111 wrote a consolidated 0.6.0 spec at the thirteenth pass and §152 revised
it at the twenty-second. Both predate every measurement from §170 onward,
which is now about thirty experiments. This supersedes them, and every item
below cites the measurement that motivates it. Nothing here is an idea that
has not been tested.

### 214.1 The one feature: an assumption report

**`cpt_assumptions(fit)`** returning a small tibble, and a line in
`cpt_report()`:

| component | what it reports | measured basis |
|---|---|---|
| `residual_dependence` | Ljung-Box p and lag-1 autocorrelation of within-segment residuals | 100% power at rho = 0.7, n = 300 and 500 (§201, §213); size needs 500 reps (§201.1) |
| `scale_sensitivity` | whether this engine's answer depends on the units | 8 of 42 engines are scale-sensitive; `pelt`/`mean` returns 139 changepoints on `10x` (§179) |
| `expected_false_positives` | `n / arl0` for `cpm`, `alpha`-derived for `nsp`, from the measured table otherwise | ARL0 honoured to 1.7x (§210); size table (§211) |
| `count_plausibility` | flag when reported changepoints exceed roughly `n/100` | `cpm` 350 at n = 100,000, `np` 10 (§208.4, §208.6) |
| `alternatives` | engines and arguments that model the violation, **with their power cost** | `hsmuce` 22.6 -> 6.0 spurious at 2.00 -> 1.75 hits; `decafs` 0.00 spurious at 0.67 hits (§213) |

**It reports and it does not act.** §213.1 is the reason: every automatic
correction available costs about one true changepoint per one-to-two false
ones, and §200.3 independently found that noise-model survival is a graded
signal rather than a classifier.

### 214.2 The registry columns, all generated by script

Seven columns have accumulated across the passes, each because a
hand-written character vector turned out to be wrong (§176's dead
`"changepoints"` entry, §187's inverted noise lists, §205.1's unconsumed
`arl0`):

| column | populated by | replaces |
|---|---|---|
| `na_handling` | the four-way NA behaviour probe (§171) | nothing; new capability |
| `scale_invariant` | the `x`/`10x`/`0.1x` sweep (§179) | nothing |
| `sequential` | the `rev(x)` invariance test (§184.2) | nothing |
| `max_cp` | construction, or the `c(x,x)` test (§184.3) | `amoc`/`pettitt` special-casing |
| `noise_model_arg` | formals inspection (§189, §200.1) | the six `noise_pref`/`noise_warn` lists |
| `rate_arg` | formals inspection (§210.2) | nothing |
| `cost` and `max_n` | the runtime table (§208) | the five-name `slow` list |

Plus two data tables: the noise-regime benchmark (§187) and the null-size
table at three lengths (§211).

### 214.3 The small fixes, ranked by measured severity

1. **`with_local_seed()` at 35 sites** — a 6-iteration simulation loop
   analyses 2 distinct datasets (§180.1). Silent, corrupts results, purely
   mechanical to fix.
2. **`selection_adjusted = FALSE` on the `strucchange` route** — currently
   labels artifacts significant at p < 0.001 in 46% of AR(1) replicates
   (§192.1). An incorrect claim about a number, not a limitation.
3. **Scale `cpm`'s `arl0` with `n`** — `arl0 = 5n` turns 36 changepoints
   into exactly the 4 real ones (§210.1).
4. **Validate `change_in` against `supports`** — 8 engines silently accept a
   `change_in` they do not implement (§181.1).
5. **Warn on transposed matrices and tiny `n`** — 7 of 9 multivariate
   engines accept a 6x200 input and 5 fabricate changepoints (§181.2);
   `pelt` returns 2 changepoints for a 3-point series (§172.1).
6. **Fix the three wrong capability flags** and connect the four unclaimed
   ones (§170, §177.3).
7. **`?ggchangepoint` as a real index** — 59 of 130 exports have no inbound
   `\link` and the package page links nothing (§183).
8. **One runnable example per export** — 31 have none (§174).
9. **Typed conditions via `cpt_abort()`** — 220 signalled, 0 typed, and 207
   tests pin the prose so message improvements are test-breaking (§175).

### 214.4 What 0.6.0 should explicitly not do

- **No new engines.** Twelve passes of measurement found no capability gap;
  every problem is a violated assumption or an unwired capability.
- **No automatic robustness correction** (§213.1).
- **No per-changepoint "is this real" probability** — the best signal
  measured separates real from spurious in 0-1 of 10 replicates (§200.3).
- **No averaging of covering across datasets** — the floor is `1/(K+1)`
  (§203.3), and `cpt_benchmark()` already ranks instead (§203.4).

### 214.5 The claim 0.6.0 can make, which no changepoint package currently makes

Every number below is measured and in this document:

> Ten engines segment 100,000 observations, six of them in under a quarter of
> a second, and **raise zero false alarms across 600,000 observations of pure
> noise each** (§208, §211). `cpt_confint()`'s bootstrap achieves nominal
> 0.95 coverage at a width of 6.2 on a 300-point series (§193). `bic` and
> `mbic` recover the exact number of changepoints in 90% of cases from K = 0
> to K = 5 (§197). Detection delay in streaming mode is 2-12 observations
> (§205.3). And when the independence assumption fails, the package says so
> (§201) instead of reporting 366 changepoints where there are none (§211).

The last clause is the only part not yet true, and it is the whole release.

## 215. What this pass changes

| § | finding | measured how |
|---|---|---|
| 213 | the assumption check fires in **12 of 12** replicates at n = 500 | Ljung-Box on fitted residuals |
| 213.1 | **every automatic correction loses power**: `decafs` 0.67 hits vs `pelt`'s 1.75; the intersection pipeline is worst at 0.50 | 6 routes x 12 reps, signal and pure noise |
| 213.2 | `pelt` is already among the most robust (1.75 false positives against `smuce`'s 20.08 at n = 500) | same |
| 214 | a consolidated 0.6.0 spec in which every item cites the experiment that motivates it, superseding §111 and §152 | synthesis of ~30 measurements |

**What this pass adds.** For twelve passes the recommendation has been "build
the noise-model sweep". Running the whole pipeline once shows that the
*detection* half is solved and the *response* half should not be automated at
all — which is a smaller and much more buildable feature than the one the
document has been circling. §200.3 reached the same conclusion about scoring;
§213.1 reaches it about substitution. Two independent measurements now say
the same thing: **the package should tell the user what is wrong and what the
alternatives cost, and then stop.**

That also resolves the tension §214.4 had to state explicitly. The natural
instinct after finding that most engines collapse under dependence is to make
the package correct for it. The measurement says the correction is a trade,
not a fix — 1.50 fewer false positives for 1.08 fewer true ones — and a
package that silently takes that trade on a user's behalf is making a
scientific decision it has no standing to make. Reporting it is both easier
to build and the only defensible option.

**And it is worth recording what twelve passes of measurement did not find:**
no wrong changepoint locations, no broken cost functions, no incorrect
metrics, no failure of the estimators themselves. Every defect was in a
label, a default, an unchecked claim, or an unwired capability. For a package
wrapping fifty third-party engines, that is the good outcome, and it is why
0.6.0 is a wiring release rather than a rewrite.

# Part V (continued) — the package's own premise, tested

Twelve passes have measured statistics. None has measured the thing the
package is named after: whether its results compose with ggplot2. That is
the selling point, the audience is every ggplot2 user, and across 499
sections it had never been checked.

## 216. The composition surface works, and the default plot is colourblind-safe

**Method.** Build a fit, add ordinary ggplot2 layers, and force each result
through `ggplot_build()` — which is what actually catches a broken plot,
since a `ggplot` object constructs lazily and only fails on render.

| layer added to `autoplot(fit)` | result |
|---|---|
| `theme_minimal()`, `labs()`, `theme_ggcpt()`, `scale_colour_cpt()` | OK |
| `coord_flip()` | OK |
| `scale_y_log10()` | OK |
| `scale_x_continuous(limits = )` | OK |
| `geom_smooth(se = FALSE)` | OK |
| `geom_hline()` | OK |
| `facet_wrap()` | OK |

**Twelve for twelve.** And building a plot from scratch with the package's own
layers works too: `ggplot(df, aes(index, value)) + geom_line() +
geom_changepoint(...)` builds, `stat_changepoint()` builds, and
`ggcptplot()`, `ggcpt_compare()` both accept themes and coordinate systems on
top.

Two calls emitted warnings, and both are the package being honest rather than
broken: `autoplot(fit, show_ci = TRUE)` and `show_fit = TRUE` warn that a
`pelt` result carries no interval and no fitted signal, which the registry
says correctly (`ci = FALSE`, `fitted = FALSE`).

### 216.1 The default plot is accessible, measured rather than asserted

§183.3 found the accessibility scales invisible in both documentation
surfaces, which raised the obvious question of whether the *default* plot
needs them. It does not. The built default uses exactly two colours — black
for the series and blue for the changepoint lines — and simulating
colour-vision deficiency with the Machado matrices:

| vision | CIE-Lab distance between the two colours |
|---|---|
| normal | 137.5 |
| deuteranopia | 132.4 |
| protanopia | 133.2 |

A separation under about 15 is hard to distinguish and under 10 is
effectively identical. These are an order of magnitude clear. For
comparison, the package's Okabe-Ito palette (`#0072B2`, `#D55E00`, …) — which
is the standard colourblind-safe set — gives 114.9 / 113.2 / 93.8 for its
first two entries, also comfortably clear.

So the accessibility story is better than §183.3 implied: **the default is
safe by construction because it uses two maximally separated colours, and the
palette that nothing links to is a correct palette.** The §183.3 finding
stands as a discoverability defect and shrinks as a correctness one.

## 217. But `plot()` fails on every result class, with an error from base R

The package declares **14 `autoplot` methods and 19 `print` methods** — a
broad plotting surface, and `autoplot` is re-exported so a user does not need
to attach ggplot2 to reach it. The gap is the other generic.

| result class | `autoplot()` | `plot()` |
|---|---|---|
| `ggcpt` | yes | yes |
| `ggcpt_power` | yes | yes |
| `ggcpt_selection` | yes | **fails** |
| `ggcpt_stability` | yes | **fails** |
| `ggcpt_sensitivity` | yes | **fails** |
| `ggcpt_batch` | yes | **fails** |
| `ggcpt_monitor` | yes | **fails** |
| `ggcpt_delay` | yes | **fails** |
| `ggcpt_influence` | yes | **fails** |
| `ggcpt_benchmark`, `ggcpt_consensus`, `ggcpt_events`, `ggcpt_label_curve`, `ggcpt_path` | yes | (same pattern) |
| **`ggcpt_recommendation`** | **none** | **fails** |

The failure is not a missing-method message. It is `plot.default()` trying
to make sense of a list:

> `'x' is a list, but does not have components 'x' and 'y'`

Nothing in that mentions `ggchangepoint`, `autoplot`, or what to do instead.
And `plot(obj)` is the reflex for most R users — it is what you type before
you have read anything, which is exactly the moment this error arrives.

### 217.1 The fix is fourteen one-line methods

The standard pattern in packages that ship `autoplot` methods is a `plot`
method that delegates:

```r
plot.ggcpt_selection <- function(x, ...) print(autoplot(x, ...))
```

Fourteen of those, plus one for `ggcpt_recommendation` once it has an
`autoplot`. It costs a dozen lines, eleven `@exportS3Method` tags, and it
removes the most likely first error a new user will ever see from this
package. Given §174 found 31 exports with no runnable example and §183 found
59 with no inbound link, this is the third independent finding that the
package's *discovery* surface lags its *capability* surface — and it is the
cheapest of the three to close.

### 217.2 `ggcpt_recommendation` is the one class with no visual at all

It has a `print` method and no `autoplot`. §186 measured that under the
default noise setting `cpt_recommend()` returns 32 candidates of which 31 tie
and the ordering is alphabetical — so this is the object most in need of a
display that shows *why* engines rank as they do, and it is the only
plottable-in-principle result class with nothing.

Given §186.3's recommendation to report ties honestly rather than sort by
name, a plot is the natural form: score on one axis, candidates grouped by
tie, with the caveat text as annotation. That turns "31 things tied, here
they are alphabetically" into a visible statement that the recommender cannot
discriminate on the information supplied — which is the honest message.

### 217.3 A third instance of the `...` problem, in passing

While constructing test objects I called `cpt_crops(x, method = "pelt")`.
`cpt_crops()` has formals `x, change_in, pen_min, pen_max, ...` — there is no
`method` argument — and the result is:

> `formal argument "method" matched by multiple actual argument`

That is my mistake, and the error tells the user nothing about which function
lacks the argument or what it should have been. §170.3 found `cpt_confint()`
silently accepting `engine =` into `...` and answering a different question;
§173 counted 96 argument names of which 71 are single-use. This is the same
defect producing a third distinct symptom — silent acceptance, undiscoverable
vocabulary, and now a confusing downstream collision. The alias-table plus
unknown-name rejection proposed in §173.2 fixes all three at once, which
strengthens the case for doing it early.

## 218. What this pass changes

| § | finding | measured how |
|---|---|---|
| 216 | **12 of 12** ggplot2 layer additions build cleanly; standalone `geom_changepoint()`/`stat_changepoint()` work; `ggcptplot()` and `ggcpt_compare()` accept themes and coords | `ggplot_build()` forced on each |
| 216.1 | the default plot uses two colours at CIE-Lab distance **132-137 under simulated deuteranopia and protanopia** — accessible by construction | Machado CVD matrices |
| 217 | **`plot()` fails on all 14 autoplot-able classes** with `'x' is a list, but does not have components 'x' and 'y'` from base R | every result class constructed and both generics called |
| 217.2 | `ggcpt_recommendation` is the only plottable-in-principle class with **no `autoplot` at all** | S3 method table |
| 217.3 | `cpt_crops(x, method = "pelt")` yields `formal argument "method" matched by multiple actual argument` — a third symptom of the `...` problem | direct call |

New actions: fourteen one-line `plot.*` methods delegating to `autoplot()`; an
`autoplot.ggcpt_recommendation()` showing the tie structure §186.3 asks to be
reported; the §173.2 alias table, now motivated by three distinct symptoms.

**What this pass adds.** It is the first pass to test the premise rather than
the statistics, and the premise holds — which matters for the roadmap because
§214.4 ruled out new engines and this confirms the plotting surface does not
need rebuilding either. The composition works, the geoms are usable
standalone, and the default output is accessible without the user knowing the
accessibility scales exist.

What it found instead is the same shape as §174 and §183: **capability that
works, reachable only if you already know how.** `autoplot()` is implemented
fourteen times and `plot()` — the generic every R user tries first — fails
with an error from base R that names neither the package nor the alternative.
Three passes have now independently located the gap in the same place, and
none of the three fixes is hard: eleven delegating methods, one runnable
example per export, one `@seealso` index. Taken together they are probably
worth more to a new user than any statistical improvement in §214.3, and they
are the cheapest items on it.

# Part V (continued) — the only documentation a stuck user reads

§174, §183 and §217 each found capability that works and cannot be found.
There is a fourth discovery surface, and it is the one a user meets at the
exact moment they need help: **the error message.** §175 counted 220
conditions and found none typed. Nobody has asked whether the text is any
good.

## 219. Twenty likely first mistakes, graded: three name a fix, none does all three

**Method.** Enumerate twenty things a new user plausibly does wrong, trigger
each, capture whether it errors, warns or is silently accepted, and grade the
message on three criteria: does it **name** the offending argument or object,
does it list the **valid** alternatives, does it name a concrete **fix**.

| | count |
|---|---|
| errors | 18 of 20 |
| warnings | 1 |
| **silently accepted** | **1** |
| names the offending thing | 8 of 20 |
| lists the valid values | 7 of 20 |
| **names a concrete fix** | **3 of 20** |
| **all three** | **0 of 20** |

Grading is by regular expression and therefore conservative — two of the six
cases scored as having none of the three are better than the grade implies
(`ggcpt_posterior()` does name the two wrappers it supports;
`cpt_metrics()` does say which indices it dropped). The genuine bottom four
are listed in §219.2.

### 219.1 The package's own messages are good

The best of them do real work:

| mistake | message |
|---|---|
| `change_in` the engine lacks | ``change_in = "covariance"` is not supported for method `pelt`. Supported: mean, var, meanvar. See cpt_methods() for the full capability table.` |
| engine not installed (`mcp`) | `Package 'mcp' is required, and it samples through JAGS — a separate program installed outside R. Install JAGS from ...` |
| univariate engine, 2-column input | ``Method `pelt` is univariate, but `x` has 2 columns. Multivariate methods: ecp, kcp, npmojo, ...`` |
| `cpt_delay()` without `truth` | ``truth` is required: detection delay is measured from the true changepoint(s) ... Pass the location(s) as an integer vector.` |
| `cpt_confint(method = "native")` on an engine without one | `... Engines that supply them: smuce, hsmuce, ... Use method = "bootstrap" for a model-agnostic interval.` |

Those are the messages somebody wrote deliberately, and they are the reason
8 of 20 name the object and 7 list the alternatives.

### 219.2 The bad ones are the ones the package never writes

Four of the twenty messages come from base R or from `match.arg()` — the
package delegates and never gets to speak — and they are the worst four:

| mistake | what the user sees |
|---|---|
| `cpt_detect(x, method = "PELT")` | `'arg' should be one of "pelt", "binseg", "segneigh", ...` (all 50) |
| `cpt_crops(x, method = "pelt")` | `formal argument "method" matched by multiple actual arguments` |
| `plot(cpt_select(...))` | `'x' is a list, but does not have components 'x' and 'y'` |
| `as_ggcpt(x)` with `x` positional | `argument "x" is missing, with no default` |

`'arg' should be one of …` deserves special attention because it is almost
certainly **the single most common first error this package produces**. A
mistyped or miscapitalised method name is the easiest mistake available, and
the message: does not name the argument (`arg` is `match.arg()`'s internal
variable), does not name the function, dumps all fifty valid values, and
offers no correction. **`match.arg()` is reachable from 40 of the 130
exported functions**, so this text is a third of the API's failure mode.

And one case produces no message at all: `cpt_detect(x, method = "pelt",
pen = "MBIC")` — a misspelling of `penalty` — is **silently accepted**, lands
in `...`, and is ignored. That is §170.3's `engine =` for the third time
(§217.3 was the second), now as the most likely spelling slip a user could
make on the most-used function in the package.

## 220. What to build: a `match.arg()` replacement with a suggestion

### 220.1 Did-you-mean is trivially feasible in base R

`utils::adist()` is base R, needs no dependency, and resolves every typo
tried against the 50 method names:

| typed | nearest match (edit distance) | second |
|---|---|---|
| `PELT` | **pelt** (0) | ecp (3) |
| `pelts` | **pelt** (1) | wbsts (3) |
| `binsegs` | **binseg** (1) | inspect (4) |
| `smucee` | **smuce** (1) | hsmuce (2) |
| `wbs3` | **wbs** (1) | wbs2 (1) |
| `cusum` | **mosum** (2) | cpm (3) |

Every one lands on the right answer at distance <= 2. Even `cusum`, which is
not a typo but a name from the literature the package does not use, resolves
to a plausible neighbour — and that is the case where a suggestion is most
valuable, because the user's mental model is wrong rather than their spelling.

### 220.2 The design

1. **An internal `cpt_match_arg(value, choices, arg, fun)`** replacing
   `match.arg()` at the user-facing call sites. On failure it emits:

   > Unknown `method = "PELT"`. Did you mean `"pelt"`? Run `cpt_methods()`
   > for all 50 methods and what each supports.

   Case-insensitive matching should be *suggested*, not silently accepted —
   `"PELT"` at distance 0 is unambiguous but auto-correcting user input hides
   a mistake the user should see once.

2. **Truncate long choice lists.** Fifty names is not a helpful list. Show
   the nearest three and point at `cpt_methods()`, which is the discoverable
   form and already exists.

3. **Reject unknown `...` names**, which kills the `pen =` case and closes
   the third instance of the §170.3 problem. §173.2's alias table is the
   fuller version; a bare "this function does not use `pen`" is the minimum
   and needs no table.

4. **Fourteen `plot.*` methods** (§217.1) and a `stop()` in `as_ggcpt()` when
   the first argument is not a changepoint vector — the other two base-R
   leaks.

5. **Grade the messages in a test.** The audit above is a script: trigger N
   known mistakes, assert each message names the argument and offers either
   valid values or a fix. That turns message quality from a thing nobody
   checks into a thing that cannot regress — and §175.1 explains why it
   matters that this be a *class*-based assertion rather than a prose match,
   since 207 tests currently pin the prose and make improvement expensive.

### 220.3 Why this is worth more than it looks

§214.3 ranked nine small fixes by measured severity, and message quality is
not on the list because nothing had measured it. It should sit near the top,
for a reason the other discovery findings do not share: **§174's missing
examples and §183's missing links cost a user who is browsing; a bad error
message costs a user who is already stuck.** The population that hits
`'arg' should be one of ...` is exactly the population deciding whether this
package is worth the trouble, and the fix — one helper, forty call sites,
base R only — is smaller than any statistical item in §214.

## 221. What this pass changes

| § | finding | measured how |
|---|---|---|
| 219 | of 20 likely first mistakes, **3 messages name a fix and 0 do all three**; 8 name the offending thing, 7 list valid values | each mistake triggered, message graded |
| 219.2 | the four worst messages are base R's or `match.arg()`'s, not the package's; `match.arg()` is reachable from **40 of 130** exports | namespace scan |
| 219.2 | `cpt_detect(x, method = "pelt", pen = "MBIC")` is **silently accepted and ignored** — third instance of the `...` problem | direct call |
| 220.1 | `utils::adist()` resolves every method-name typo tried at edit distance <= 2, with no new dependency | 6 typos against 50 names |

New actions: `cpt_match_arg()` with a did-you-mean suggestion and a truncated
choice list; reject unknown `...` names; the fourteen `plot.*` methods; a
message-quality test that triggers known mistakes and asserts each names the
argument plus a fix.

**What this pass adds.** It completes the discovery-surface picture, and the
four findings now form a single ranked list rather than four separate
complaints:

| surface | measured gap | who it costs |
|---|---|---|
| error messages (§219) | 0 of 20 messages do all three; the commonest one is `match.arg()`'s | a user who is stuck **now** |
| `plot()` (§217) | fails on all 14 plottable classes | a user typing the reflex generic |
| examples (§174) | 31 exports have none | a user reading a help page |
| cross-references (§183) | 59 exports have no inbound link | a user browsing |

**All four are wiring, all four are cheap, and they are ordered by how stuck
the user already is when they hit them.** That ordering is the useful output
of this pass: §214.3 sorted its nine fixes by statistical severity, which put
the `with_local_seed()` bug first — correctly, since it silently corrupts
results. But among the *usability* items, the right order is not "biggest
surface" but "latest in the user's journey", and by that measure the error
messages come first and the cross-reference graph last.

# Part V (continued) — what happens after the analysis: saving, sharing, archiving

Fourteen passes have measured what the package computes and how a user finds
it. Nothing has measured what happens when they save the result, mail it to a
collaborator, or open it in three years. `R/` contains no `saveRDS`, no
`readRDS`, no JSON and no CSV path — the words appear zero times in the source
— so persistence is entirely whatever R's defaults do.

## 222. `$fit` is 1,957 times the size of the answer

**Method.** Build a fit, `saveRDS()` it, then save it again with `$fit`
removed, and difference the file sizes.

| engine | n | total on disk | of which `$fit` | the answer | `$fit` share |
|---|---|---|---|---|---|
| `pelt` | 1,000 | **8.3 KB** | 0.4 KB | 7.9 KB | 4% |
| `binseg` | 1,000 | 8.4 KB | 0.5 KB | 7.9 KB | 6% |
| `smuce` | 1,000 | 8.3 KB | 0.2 KB | 8.1 KB | 3% |
| `fpop` | 1,000 | 15.8 KB | 7.9 KB | 7.9 KB | 50% |
| `wbs` | 1,000 | 25.1 KB | 17.2 KB | 7.9 KB | 68% |
| **`bocpd`** | 1,000 | **3.8 MB** | 3.8 MB | 7.9 KB | **100%** |
| **`strucchange`** | 1,000 | **14.4 MB** | 14.4 MB | 8.0 KB | **100%** |
| `pelt` | 10,000 | 147 KB | 73 KB | 74 KB | 50% |
| `wbs` | 10,000 | 336 KB | 262 KB | 74 KB | 78% |

**A `strucchange` result for a one-thousand-point series writes a 14.4 MB
file, of which 8 KB is the answer.** That is 1,778 times the size of the
`pelt` result on the same data. In memory the ratio is worse — slot by slot,
for a 1,000-point series:

| slot | size |
|---|---|
| `changepoints` | 1.3 KB |
| `segments` | 1.4 KB |
| `data` | 12.7 KB |
| `method`, `change_in`, `penalty`, `call`, `cp_convention`, `runtime` | ~2.5 KB total |
| **`fit`** | **35,038 KB** |

**`$fit` is 1,957x everything else combined.** §1708 predicted this from
`strucchange`'s triangular O(n^2) RSS matrix and §208.3 saw it as a runtime
failure; this is the same object as a disk and memory cost.

### 222.1 The package already knows, and its advice is to use a different function

`keep_fit` exists — in exactly one place. `cpt_batch()` takes
`keep_fit = TRUE` and drops `$fit` when asked. `cpt_detect()` does not have
the argument at all. And `cpt_recommend()` emits this caveat for
`strucchange`, `bfast` and `bocpd` at large `n`:

> `returns a large raw fit at this length; use cpt_batch(keep_fit = FALSE) for a panel`

So the recommender's documented workaround for a 34 MB object is *switch to a
different function*. For a single series there is no way to ask for the answer
without the engine's scratch space attached to it.

**The fix is one argument**: `cpt_detect(x, method, keep_fit = TRUE)`,
defaulting to `TRUE` so nothing changes, and honoured by `ggcpt_build()` which
already receives `fit`. Everything downstream that needs `$fit` already checks
for it — §170 and §177 catalogued those checks — and `cpt_confint()`,
`cpt_solution_path()` and `ggcpt_runlength()` all fail informatively when it
is absent, which is the correct behaviour for a deliberately dropped fit.

## 223. The good news: the answer is portable without the package

**Method.** Read each saved fit in `Rscript --vanilla` with `R_LIBS_USER=""`
and **no `library()` call of any kind** — the situation of a collaborator who
receives an `.rds` and does not have `ggchangepoint`.

| file | changepoints recovered | `$fit` class | warnings |
|---|---|---|---|
| `pelt_1000.rds` | 333, 666 | `cpt` (S4, 12 slots resolved) | **none** |
| `smuce_1000.rds` | 333, 666 | `stepfit` | none |
| `strucchange_1000.rds` | 333, 666 | `breakpointsfull` | none |
| `bocpd_1000.rds` | 333, 666 | `ocp` | none |
| `wbs_1000.rds` | 333, 666 | `wbs` | none |

All five load cleanly, and a bare session can pull out the changepoints, the
`data` tibble (999 rows, `index`/`value`), `method` and `change_in` with no
package present. **The `ggcpt` class is a plain list of plain data, and that
makes the result archival by construction** — a property worth stating as a
design virtue rather than leaving as an accident.

Only `$fit` depends on the engine, and only for S4 classes: `pelt`'s `cpt`
resolved because `changepoint` is installed and R loaded its class registry on
demand. Where the engine is *absent*, `$fit` degrades and the answer does not
— which is the right failure mode and reinforces §222's argument for
`keep_fit = FALSE`.

### 223.1 And a text round trip already works, undocumented

| step | result |
|---|---|
| `tidy(fit)` | columns `cp`, `cp_value` |
| `write.csv()` then `read.csv()` | identical columns, 2 rows |
| `as_ggcpt(cp = back$cp, x = x, method = , change_in = )` | **works** — class `ggcpt`, changepoints 333, 666 |
| slot names vs the original | **identical set**; only `$fit` is `NULL` |

So the package can already export a result to a CSV a Python or Julia
collaborator can read, and reconstruct a fully functional `ggcpt` from it.
Nothing says so: "CSV" appears zero times in this document and the round trip
appears in no vignette.

### 223.2 What to build

1. **`keep_fit` on `cpt_detect()`** (§222.1). One argument, default `TRUE`,
   and it turns a 14.4 MB file into 8 KB.
2. **Warn once when `$fit` exceeds a threshold** — say 10 MB. The object knows
   its own size and the user does not until the `.rds` lands on disk.
3. **Document the archival guarantee.** "A saved `ggcpt` result can be read
   in any R session with no packages installed; the changepoints, the series
   and the metadata are plain data" is a real promise, currently unstated and
   already true. §223's table is the evidence.
4. **`cpt_export(fit, file, format = c("csv", "json"))`** — the round trip in
   §223.1 already exists in pieces; wrapping it makes the cross-language path
   discoverable. §78 (Theme AV) asked for a Python bridge "in the direction
   nobody built"; this is the cheap half of it, and it needs no Python.
5. **A `keep_fit = FALSE` note in `cpt_report()`**, since a report is exactly
   the context where nobody wants the engine's scratch space retained.

## 224. What this pass changes

| § | finding | measured how |
|---|---|---|
| 222 | a `strucchange` fit for n = 1,000 writes **14.4 MB**, 100% of it `$fit`, against `pelt`'s 8.3 KB on the same data | `saveRDS` with and without `$fit` |
| 222 | in memory `$fit` is **1,957x** the size of every other slot combined (35,038 KB against ~17.9 KB) | `object.size()` per slot |
| 222.1 | `keep_fit` exists on `cpt_batch()` and **not on `cpt_detect()`**; the recommender's documented workaround is to switch functions | source scan |
| 223 | all five saved fits load in `--vanilla` with **no packages and no warnings**, changepoints intact — the result is archival by construction | bare-session reads |
| 223.1 | `tidy()` -> CSV -> `as_ggcpt()` round-trips to a working object, and is documented nowhere | full round trip |

New actions: `keep_fit` on `cpt_detect()`; a large-`$fit` warning;
`cpt_export()` wrapping the existing round trip; document the archival
guarantee; drop `$fit` by default in reporting contexts.

**What this pass adds.** It is the first pass to look past the analysis to
what happens afterwards, and it splits cleanly into one defect and one
unadvertised virtue — which is the same shape as §216 and §217 found in the
plotting surface, and it is becoming the dominant pattern of this whole
document:

> **The package's substance is better than its packaging.** The result object
> is portable enough to read with no packages installed and reconstructible
> from a two-column CSV — and it ships with the engine's 34 MB scratch space
> attached and no way to ask it not to.

That also gives §214's spec a fifth cheap item and, more usefully, a claim
worth making in the README. §214.5 listed what 0.6.0 could say about
calibration and speed; §223 adds one more sentence to it, already true:
**a saved `ggchangepoint` result is readable in any R session forever,
because it is plain data.** Very few analysis packages can say that, none of
them says it, and the only thing standing between this package and saying it
is an argument that already exists on the neighbouring function.

# Part V (continued) — the growth path: is a registered method a second-class citizen?

The package's route to being "more expansive" is not fifty-one engines
shipped by the maintainer; it is `cpt_register_method()`, so that other people
add their own. §17 recorded that a registered method was once invisible to
every parallel worker. Nobody has checked whether one is a first-class
citizen anywhere else.

## 225. Twenty-three of twenty-four downstream surfaces work for a registered method

**Method.** Register a genuinely multi-changepoint detector — recursive
CUSUM-style binary segmentation, ~20 lines, no penalty argument — then push it
through every downstream function a built-in supports. Plots are forced
through `ggplot_build()`.

The registered detector found the truth exactly: changepoints at **120 and
240** on a 360-point series with shifts there.

| surface | result |
|---|---|
| `cpt_methods()` row, `cpt_registered_methods()`, `cpt_cite()` | OK |
| `print()`, `summary()`, `autoplot()` | OK |
| `tidy()`, `glance()`, `augment()` | OK |
| `cpt_test()` | OK |
| `cpt_confint(method = "bootstrap")` **and** `method = "auto"` | OK |
| `cpt_report()`, `cpt_gt()` | OK |
| `cpt_batch()`, `cpt_consensus()` mixed with `pelt` | OK |
| `cpt_stability()`, `cpt_sensitivity(over = )`, `cpt_influence()`, `cpt_leverage()` | OK |
| `cpt_benchmark()` mixed with `pelt`, `cpt_metrics()` | OK |
| **`cpt_batch()` under `future::multisession`** | **OK** |
| appears in `cpt_recommend()` (33 candidates), `status = "registered"` | OK |
| `cpt_solution_path()`, `cpt_statistic()`, `ggcpt_posterior()` | refuse, naming which engines do have the capability |
| `cpt_select(criterion = "bic")` | **warns** — see below |

**This is the best result in the document.** A twenty-line function registered
at runtime gets confidence intervals, bootstrap resampling, stability
analysis, sensitivity sweeps, consensus voting with a built-in engine,
benchmarking, a `gt` table, a markdown report, tidyverse tidiers, a plot, and
a citation — and it survives serialisation to a parallel worker. §17's fix
holds: `with_session_registry()` carries the registration across the process
boundary, verified here rather than assumed.

The three capability-gated refusals are correct behaviour: a user-supplied
function has no solution path, so `cpt_solution_path()` says so and names the
engines that do. And the `cpt_select()` warning is honest — my detector has no
penalty knob, so its K-ladder collapses, and the package says exactly that:

> `mydet` produced only 2 distinct segmentation(s) over the ladder

That is the one place where a registered method is genuinely limited, and it
is a property of the detector, not of the mechanism. **Worth documenting in
`extending.Rmd`, though:** a registered method that wants to work with
`cpt_select()` needs a parameter the ladder can vary, and the vignette's own
example (`biggest_jump`) does not have one either.

## 226. The one real defect the sweep found: `cpt_scale_space()` loads Tk

The sweep produced one warning that is not about the registered method at all:

> `no DISPLAY variable so Tk is not available`

**Traced.** `cpt_scale_space()` computes a MOSUM statistic across bandwidths,
so it loads `mosum`, and the chain is a hard `Imports` at every step:

```
cpt_scale_space()  ->  mosum  ->  plot3D  ->  misc3d  ->  tcltk
```

Loading any of `plot3D`, `misc3d` or `mosum` alone brings `tcltk` into the
session, and `tcltk`'s own load hook warns when `DISPLAY` is unset. Measured:
`cpt_scale_space()` newly loads **four** namespaces — `misc3d`, `mosum`,
`plot3D`, `tcltk` — and the warning fires once per session on first load.

**And a vignette calls it.** `vignettes/inference.Rmd:238` runs
`ggcpt_scale_space(res_mosum, bandwidths = c(15, 30, 60, 90))`, so building
the vignettes on any headless machine — a CI runner, a Docker image, a
cluster node, a check farm without X11 — emits this warning.

This is the same family as §170's S37, where `cpt_methods()` loaded 35
namespaces and a broken `rgl` silently killed the macOS vignettes. That was
fixed by not loading namespaces at all. **Here it cannot be: the package
genuinely needs `mosum` to compute the statistic.** So the mitigations are
different:

1. **Suppress the load warning at the call site.**
   `suppressWarnings(loadNamespace("mosum"))` before use — narrow, and it
   silences a warning that is about the user's display, not their data.
2. *(Shipped: `vignettes/inference.Rmd`'s setup chunk now wraps the `mosum`
   `requireNamespace()` in `suppressWarnings()`, and the knitted output
   carries zero `no DISPLAY` occurrences. Deleted from this list.)*
3. **Record it in the registry.** `mosum` is the only engine whose transitive
   dependencies reach a GUI toolkit. That is a fact worth a column — call it
   `heavy_deps` — because §84 measured install cost and §102 load cost, and
   neither noticed that one engine pulls in `tcltk`, `plot3D` and `misc3d`.
4. **Ask upstream.** `misc3d` imports `tcltk` for interactive 3-D rendering
   that `plot3D` does not need for the static case, and `mosum` needs neither.
   That is a three-package chain to carry a GUI dependency for a statistic,
   and it is worth one issue upstream even if nothing changes.

## 227. What this pass changes

| § | finding | measured how |
|---|---|---|
| 225 | **23 of 24 downstream surfaces work** for a runtime-registered 20-line detector, including bootstrap intervals, consensus with a built-in, benchmarking, and `future::multisession` | every surface exercised |
| 225 | the three capability-gated functions refuse correctly and name the engines that qualify | same |
| 225 | `cpt_select()` warns honestly that a detector with no penalty knob yields a collapsed K-ladder — including for the vignette's own example | same |
| 226 | `cpt_scale_space()` loads **four** namespaces and warns `no DISPLAY variable so Tk is not available`; the chain is `mosum -> plot3D -> misc3d -> tcltk`, all hard `Imports` | namespace diff and per-package load test |
| 226 | `vignettes/inference.Rmd:238` calls it, so vignette builds warn on every headless machine | grep plus the trace |

New actions: suppress the `mosum` load warning at the call site; add a
vignette setup guard mirroring `tests/testthat/setup.R`; a `heavy_deps`
registry column; document in `extending.Rmd` that `cpt_select()` needs a
varying parameter; an upstream note about `misc3d`'s `tcltk` import.

**What this pass adds.** For fifteen passes the pattern has been "the
substance is better than the packaging." This pass is the first clean
exception, and in the direction that matters most for the package's future:
**the extension mechanism is not a second-class path, it is the same path.**
Everything the maintainer's fifty engines get, a twenty-line user function
gets too — including the two things that would have been easiest to get wrong,
re-running the detector by name inside a bootstrap and shipping the
registration to a parallel worker.

That changes the argument for how the package grows. §214.4 said 0.6.0 should
add no engines, on the grounds that twelve passes found no capability gap.
§225 gives a second and better reason: **the marginal engine is worth more
registered by the person who needs it than wrapped by the maintainer**, because
it costs the maintainer nothing and loses the user nothing. The right
investment is not the fifty-first wrapper but making `extending.Rmd` the most
prominent document in the package — and §183's finding that
`cpt_registered_methods()` appears nowhere in that vignette is, in that light,
the single most consequential documentation gap found so far.

# Part V (continued) — the fifth discovery surface: what the vignettes actually run

§174 counted exports *mentioned* in a vignette (111 of 130). Mentioning is
not demonstrating. This measures the stronger thing: which exports are
**called inside a runnable chunk** in a vignette or the README — the only
place a user sees a function work before trying it.

## 228. 98 of 130 exports are executed in a user-facing document; the entire streaming API is not

**Method.** Parse only the code inside ```` ```{r} ```` fences (prose and
inline code excluded), and look for `name(` for each export.

| document | KB | chunks | exports called |
|---|---|---|---|
| `ggchangepoint.Rmd` (feature tour) | 26.7 | 78 | **86** |
| `README.Rmd` | 27.8 | 50 | 69 |
| `introduction.Rmd` | 30.5 | 30 | 30 |
| `inference.Rmd` | 8.9 | 24 | 17 |
| `extending.Rmd` | 7.6 | 13 | 15 |
| `comparison.Rmd` | 22.4 | 18 | 14 |
| `supervised.Rmd` | 6.6 | 13 | 13 |

**Union: 98 of 130 exports are actually executed somewhere a user reads.**
Against §174's 111 *mentioned*, thirteen exports appear in prose and never
in code.

Of the 32 never executed, sixteen are engine wrappers reached through
`cpt_detect(method = )`, which is the intended route and not a gap. The rest
are, and one group stands out.

### 228.1 The streaming subsystem is described ten times and never run

`cpt_monitor()` and `cpt_update()` appear in **no runnable chunk in any
vignette or the README**. They are discussed in prose — four mentions in
`ggchangepoint.Rmd`, five in `README.Rmd`, one in `introduction.Rmd` — and
never demonstrated.

That is a whole subsystem: three monitor methods, `alarms()`, `cpt_delay()`,
`cpt_replay()`. §205 measured it in detail and the results were good —
median detection delay of **2 to 12 observations** at shift sizes 1 to 3, and
`cpm` calibrated to within 8% of its declared ARL0. **The package has a
competitive streaming detector and has never shown it working.**

`?cpt_monitor` does carry a four-line example (`mon <- cpt_monitor(...)`,
`cpt_update(...)`, `alarms(mon)`), so it is not undocumented — but a help
page example is not a vignette, and §206.1 already assumed a "monitoring
vignette" that does not exist. **That vignette is the single largest
documentation gap in the package**, and §205.3's delay table plus §206's
AR(1) inflation numbers are most of its content already.

### 228.2 The others, and one correction

Also never executed: `cpt_solution_path()`, `cpt_scale_space()` and
`ggcpt_statistic()` (the three diagnostics — note `ggcpt_scale_space()` *is*
run in `inference.Rmd`, its non-plotting sibling is not); `cpt_gt()`;
`cpt_min_detectable()` and `cpt_scenarios()` (the power family, whose third
member `cpt_power()` §174 found gated behind `\donttest`); `as_cpt_series()`,
`cpt_registered_methods()`, `cpt_load_tcpd()`; and all five accessibility
scales.

**A correction to my own tally.** I was about to report the accessibility
scales as lacking examples too. They do not:
`man/scale_colour_cpt.Rd` carries aliases for `scale_colour_cpt`,
`scale_color_cpt`, `scale_fill_cpt` and `scale_linetype_cpt` and has one
`\examples{}` block covering all four. §174's alias-keyed parser had this
right and my per-file check was wrong. The accurate tally for that family is
three gaps, not four:

| surface | status |
|---|---|
| `\examples{}` | **present** (shared page) |
| inbound `\link{}` from another help page | none (§183) |
| pkgdown reference index | 4 of 5 absent (§183.3) |
| runnable vignette or README chunk | none (this section) |

Which is the tenth instance of the §202 pattern and the third caught inside
the pass that raised it.

## 229. NEWS.md names 118 of 130, and nothing tells a reader where to start

### 229.1 NEWS is thorough

`NEWS.md` is 70 KB and 1,131 lines, with a 0.5.0 section organised under
twelve topical headings — the extension mechanism, the engine registry, time
indices, inference, choosing K, diagnostics, supervised detection, choosing
and combining methods, communication, benchmarking, streaming. It **names 118
of the 130 exports.** For a release that took the API from 63 exported
objects to 130, that is a genuinely complete changelog and worth saying so.

The twelve it does not name: `binsegrcpp_wrapper`, `fcov_wrapper`,
`fmean_wrapper`, `hdcov_wrapper`, `hdreg_wrapper`, `network_wrapper`,
`var_wrapper`, `scale_color_cpt`, `scale_colour_cpt_label`, and
**`signal_fms`, `signal_stairs`, `signal_teeth`** — three of the five standard
test signals, which §174 found have no examples and §183.1 found have no
inbound links. That family is now missing from four surfaces in a row.

**Nine exports are in neither NEWS nor any runnable chunk**:
`binsegrcpp_wrapper`, `fcov_wrapper`, `fmean_wrapper`, `hdcov_wrapper`,
`hdreg_wrapper`, `network_wrapper`, `var_wrapper`, `scale_color_cpt`,
`scale_colour_cpt_label`. Seven are engine wrappers — six of them for the
high-dimensional and functional engines added in 0.5.0, which is to say the
newest and least-known part of the package is the least written about.

### 229.2 There is no reading order

Six vignettes, no explicit ordering anywhere. `_pkgdown.yml` has **no
`articles:` section**, so the website uses its default grouping, and R orders
`browseVignettes()` by the built package's directory order — alphabetical
absent an explicit list. By filename that is:

> comparison, extending, ggchangepoint, inference, **introduction**, supervised

**The entry point is fifth.** A user who runs
`browseVignettes("ggchangepoint")` is offered "Comparing and Evaluating
Changepoint Methods" first and "A Unified Tidy Interface for Changepoint
Analysis in R" fifth. The titles are good; the order is alphabetical by
accident of filename.

Two cheap fixes: an `articles:` section in `_pkgdown.yml` giving the intended
sequence, and a `Getting started` designation so pkgdown puts
`introduction.Rmd` first. Neither changes any content.

### 229.3 What to build

*Items 1, 2, 3 and 5 shipped in the 0.5.0 documentation pass and are deleted
from this list: `vignettes/monitoring.Rmd` exists and renders in 8.8 s, the
six unrun diagnostics and power functions have chunks, one figure exercises
`scale_colour_cpt()`/`scale_fill_cpt()`/`scale_linetype_cpt()` together, and
`_pkgdown.yml` carries an `articles:` order beginning with `introduction`.
What remains:*

1. **NEWS entries for the nine exports named in neither NEWS nor any chunk**
   — `binsegrcpp_wrapper`, `fcov_wrapper`, `fmean_wrapper`, `hdcov_wrapper`,
   `hdreg_wrapper`, `network_wrapper`, `var_wrapper`, `scale_color_cpt`,
   `scale_colour_cpt_label`. Six are the high-dimensional and functional
   wrappers added in 0.5.0, which is to say the newest part of the package is
   still the least written about. Note that the *chunk* half of that
   intersection has since been closed for several of them, so the list needs
   re-deriving before it is acted on.

## 230. What this pass changes

| § | finding | measured how |
|---|---|---|
| 228 | **98 of 130** exports are called in a runnable chunk, against §174's 111 merely mentioned | parsed only ```` ```{r} ```` fences across 6 vignettes + README |
| 228.1 | `cpt_monitor()`/`cpt_update()` appear in **no runnable chunk anywhere**, despite ten prose mentions — a whole subsystem with measured 2-12 observation delay and never shown working | per-document call scan |
| 228.2 | **correction:** the accessibility scales *do* have an example, on a shared alias page; their gaps are links, pkgdown index and vignette chunks — three, not four | `man/scale_colour_cpt.Rd` aliases |
| 229.1 | NEWS.md names **118 of 130** exports across twelve topical headings — a complete changelog for a 39-to-130 release | full-text scan |
| 229.1 | 9 exports are in **neither** NEWS nor any chunk; 6 of those are the high-dimensional and functional wrappers added in 0.5.0 | intersection |
| 229.2 | no reading order exists; alphabetical ordering puts `introduction.Rmd` **fifth** | `_pkgdown.yml` and filenames |

New actions: a monitoring vignette; runnable chunks for six unrun functions;
one chunk exercising the accessibility scales; NEWS entries for nine exports;
an `articles:` order in `_pkgdown.yml`.

**What this pass adds.** It closes the discovery-surface set at five, and the
ranking from §221 extends cleanly:

| surface | measured gap | who it costs |
|---|---|---|
| error messages (§219) | 0 of 20 messages do all three | a user stuck **now** |
| `plot()` (§217) | fails on all 14 plottable classes | a user typing the reflex generic |
| examples (§174) | 31 exports have none | a user reading a help page |
| **runnable chunks (§228)** | **32 exports never execute; the whole streaming API** | **a user deciding whether a capability exists** |
| cross-references (§183) | 59 exports have no inbound link | a user browsing |

The new row belongs fourth, and it is the one that costs a *capability*
rather than a user's patience: someone evaluating whether this package does
streaming detection will find prose saying it does, no code showing it, and
will reasonably conclude the support is notional. §205 measured that it is
not — the delays are competitive and one monitor is correctly calibrated.
**That is the clearest case in the whole document of the package
under-selling something it has already built**, and unlike every statistical
item in §214.3, the fix is a vignette whose content is already written down
in §205 and §206.

# Part V (continued) — how fast the API grew, and which surface could not keep up

§214.4 recommended that 0.6.0 add no engines, on the grounds that twelve
passes of measurement found no capability gap. §225 added a second reason:
the extension mechanism means the marginal engine is better registered by the
person who needs it. This pass supplies a third, and it is the first
quantitative one — measured against every version CRAN has ever published.

## 231. 37 exports to 130 in seventy-one days

**Method.** Fetch every archived tarball from the CRAN source archive and
count. This is not inference from the changelog; it is the released artifacts.

| version | published | exported objects |
|---|---|---|
| 0.1.0 | 2022-02-22 | **4** |
| 0.2.0 | 2026-06-20 | 37 |
| 0.3.0 | 2026-06-25 | 39 |
| 0.4.0 | 2026-08-24 | 63 |
| 0.5.0 | *written by 2026-08-30, unreleased* | **130** |

The cadence is the striking part. 0.1.0 shipped in February 2022 with four
exported objects and then nothing for **four years and four months**. Since
2026-06-20: 0.2.0 to 0.3.0 in **five days**, 0.3.0 to 0.4.0 in sixty, and
0.5.0 written within six days of 0.4.0's publication. **The API went from 37
exported objects to 130 in seventy-one days** — three and a half times, in
ten weeks.

### 231.1 Everything scaled proportionally, which is the good news

Comparing CRAN's 0.4.0 tarball against this repo, component by component:

| | 0.4.0 | 0.5.0 | factor |
|---|---|---|---|
| exported objects | 63 | 130 | **2.1x** |
| `.Rd` pages | 66 | 124 | 1.9x |
| vignettes | 3 | 6 | 2.0x |
| files under `R/` | 27 | 49 | 1.8x |
| test files | 10 | 18 | 1.8x |
| `Suggests` | ~30 | ~56 | 1.9x |
| tarball | 2,312 KB | — | — |

Every surface grew between 1.8x and 2.1x alongside a 2.1x API. **The release
did not outrun its own documentation and tests in volume** — that is a real
discipline result and worth stating, because the obvious failure mode of
doubling an API in ten weeks is a `man/` directory that does not follow, and
that did not happen here.

### 231.2 But one surface does not scale by adding files

Set the growth factors beside the coverage fractions measured in this
document:

| surface | coverage | how it scales |
|---|---|---|
| named in `NEWS.md` (§229.1) | **118 / 130 = 91%** | one line per export — linear |
| has a runnable `\examples{}` (§174) | 99 / 130 = 76% | one block per export — linear |
| executes in a vignette or README chunk (§228) | 98 / 130 = 75% | one chunk per export — linear |
| **has an inbound `\link{}` from another page (§183)** | **71 / 130 = 55%** | **a connection between pairs of pages — not linear** |

The three linear surfaces sit at 75-91%. The one non-linear surface sits at
55%, and it is the worst-covered thing in the package.

**That is the structural explanation for §183, and it is not a discipline
failure.** Adding an export costs one `.Rd` page, one example, one NEWS line —
fixed work, and the record shows it was done. Adding an export to a
*cross-reference graph* means deciding which of the other 129 pages should
point at it and editing those pages. The cost per export grows with the size
of the package, so it is the first thing to fall behind when the API doubles,
and it falls behind silently because nothing checks it.

Two consequences:

1. **`@family` tags, not `@seealso` lists** (§183.4 item 3). roxygen2
   generates reciprocal links from `@family`, which converts a quadratic
   hand-editing problem into a linear tagging one — one tag per export instead
   of one edit per pair. This is the only fix that survives the next doubling.
2. **The doc-coverage test must assert the graph, not just mentions**
   (§183.4 item 4). The three linear surfaces stayed high partly because
   `test-doc-coverage.R` already asserts README mentions; the unchecked
   surface is the one that decayed. That is not a coincidence worth ignoring.

### 231.3 What this adds to the freeze argument

§214.4's case for adding no engines in 0.6.0 was qualitative. Here it is
arithmetically:

- Fifty methods and 130 exports is **2.1x** what CRAN has ever seen from this
  package, and CRAN has seen it for **zero days** — 0.5.0 is unreleased.
- The cross-reference graph is at 55% and structurally cannot catch up by
  the same effort that got the other surfaces to 75-91%.
- Every defect the last sixteen passes found was in a label, a default, an
  unchecked claim, or an unwired capability (§214's summary) — the failure
  modes of fast growth, not of missing features.

**So the freeze is not conservatism, it is the only way the coverage
fractions converge.** A 0.6.0 that adds nothing to the API and spends itself
on `@family` tags, the fourteen `plot.*` methods (§217.1), `cpt_match_arg()`
(§220.2), the monitoring vignette (§228.1) and the nine fixes in §214.3 would
take all five discovery surfaces past 95% while the API stands still. Adding
a fifty-first engine moves every denominator in §231.2 in the wrong
direction.

One caveat on the cadence reading, stated because it cuts against the
argument: five days between 0.2.0 and 0.3.0 and six between 0.4.0 and 0.5.0's
completion are not evidence of haste by themselves — 0.3.0 added two exports
and was plainly a patch in all but name, and 0.5.0's audit trail in this file
runs to thirty-odd measured defects found and fixed before submission. The
growth was fast **and** audited. What it was not, and could not have been at
that speed, was fully wired.

# Part V (continued) — the mechanical CRAN checklist, audited

Sixteen passes have found substantive gaps. None has checked the mechanical
things CRAN itself bounces submissions over, and 0.5.0 is pre-submission. This
pass is that checklist. It is the first pass whose answer is almost entirely
"nothing to fix".

## 232. On CRAN's own documentation requirements, the package is clean

### 232.1 `\value{}` — the most common documentation rejection

"Please add `\value` to `.Rd` files regarding exported methods" is among the
most frequent reasons a submission is returned. Measured across all 124 man
pages:

| | count |
|---|---|
| man pages total | 124 |
| pages documenting at least one export | 116 |
| **pages missing `\value{}`** | **1** |
| pages missing `\description{}` | 0 |
| pages missing `\usage{}` | 1 |

The single exception on both counts is `man/reexports.Rd`, which documents the
five re-exported generics (`as_tibble`, `augment`, `autoplot`, `glance`,
`tidy`). That page is roxygen's `@rdname reexports` boilerplate and CRAN
accepts it without `\value` — it is the standard pattern, not an omission.

So on the requirement that most often returns a submission, the package is at
**115 of 115 applicable pages.**

### 232.2 78% of example code actually runs under check

Examples that never execute are examples nobody has tested. Measured by
stripping `\dontrun{}` and `\donttest{}` blocks and counting what remains:

| | lines |
|---|---|
| example lines across `man/` | 491 |
| inside `\dontrun`/`\donttest` | 108 (22%) |
| **executing under `R CMD check`** | **383 (78%)** |

Four pages are fully gated — `cpt_benchmark`, `cpt_load_tcpd`,
`cpt_min_detectable`, `cpt_power` — and eleven are partly gated. Each of the
four has a defensible reason (a benchmark, a network download, and two
Monte Carlo functions), though §174 already argued the two power functions
would be better with a small-`n_sim` runnable variant, since they are the
package's most distinctive capability and currently nothing demonstrates them
executing.

**And a discrepancy resolved.** §174 reported *five* fully-gated pages,
including `mcp_wrapper`. This parser counts four. The difference is
`man/mcp_wrapper.Rd`, whose `\examples{}` block opens with three comment
lines explaining why JAGS cannot be tested and *then* enters `\dontrun{}`. So
a comment-only remainder made it read as partly gated here and fully gated
there. Both counts are right about the file; **five** is the number that
matters, because comments do not test anything. §174 stands.

### 232.3 Every URL resolves

CRAN's incoming checks flag URLs that 404 or redirect, and a redirect alone
is enough to draw a NOTE. Every distinct URL in `man/*.Rd`, `DESCRIPTION`,
`README.md` and `inst/CITATION`:

| | count |
|---|---|
| distinct URLs | 9 |
| returning 2xx | **9** |
| redirecting (3xx) | 0 |
| failing | 0 |

Clean, including the JAGS installation link that §219.1 singled out as one of
the package's better error messages — a message that points at a URL is only
good while the URL resolves, and this one does.

### 232.4 What this pass does and does not say

It is a narrow clean bill and should not be read as more. What was audited is
CRAN's *mechanical* documentation contract: does every exported page declare a
return value, does the example code run, do the links work. On all three the
answer is yes.

What was not audited here, and what sixteen previous passes found in
abundance, is whether the documentation is *useful*: 59 exports with no
inbound `\link` (§183), 31 with no example at all (§174, distinct from the
gating question above), 32 that never execute in a vignette (§228), 0 of 20
error messages doing all three jobs (§219), and the whole streaming API
described and never shown (§228.1). **CRAN's checklist and a user's experience
are different tests, and this package passes the first while failing parts of
the second.**

That asymmetry is worth stating plainly in the roadmap, because it explains a
pattern that has otherwise looked like carelessness. The surfaces CRAN checks
mechanically are complete — `\value`, `\usage`, `\description`, URLs, and the
78% of examples that run. The surfaces nothing checks are the ones that
decayed. §231.2 found the same thing from the growth side: the linear,
checked surfaces sit at 75-91% and the unchecked non-linear one sits at 55%.

**The actionable form: every documentation improvement in §214.3 should ship
with the test that keeps it true**, because the measured evidence across two
independent passes is that this package maintains exactly what is asserted
and loses exactly what is not. §183.4 item 4 (assert the link graph),
§220.2 item 5 (assert message quality) and §229.3 (assert a chunk per export)
are not gold-plating; they are the only reason the fixes would survive the
next release.

## 233. What this pass changes

| § | finding | measured how |
|---|---|---|
| 232.1 | **115 of 115 applicable man pages carry `\value{}`**; the one exception is the roxygen re-exports boilerplate CRAN accepts | all 124 `.Rd` parsed |
| 232.1 | 0 pages missing `\description{}`; 1 missing `\usage{}` (the same boilerplate page) | same |
| 232.2 | **383 of 491 example lines (78%) execute under `R CMD check`**; 4 pages fully gated, 11 partly | `\dontrun`/`\donttest` stripped |
| 232.2 | **discrepancy resolved:** §174's count of 5 fully-gated pages is the correct one; `mcp_wrapper`'s comment-only remainder made it read as 4 here | direct inspection |
| 232.3 | **all 9 distinct URLs return 2xx**, none redirects | `curl -L` per URL |

No new actions — this pass found nothing to fix. Its contribution is the
conclusion in §232.4: pair every documentation fix with the assertion that
keeps it true, since the measured record is that checked surfaces stay
complete and unchecked ones decay.

Two corrections applied to earlier sections this pass, from checking the
scope of §214.3's items rather than quoting my own summaries:

- **The `seed` bug is 35 call sites, not 19.** I had counted only wrappers
  carrying a `seed` formal; `if (!is.null(seed)) set.seed(seed)` also appears
  in `cpt_batch`, `cpt_benchmark`, `cpt_stability`, `cpt_power`,
  `cpt_select`, `cpt_consensus`, `cpt_influence`, `cpt_simulate` and five
  places in `simulate.R`. §180.2 and §214.3 updated.
- **`NAMESPACE` declares zero `plot` S3 methods, not three.** §217 reported
  `plot()` working for `ggcpt` and `ggcpt_power`; whatever made those succeed,
  it was not a declared method. The fix is **fourteen** one-line delegating
  methods, not eleven. §217, §217.1, §218 and §221 updated.

Eleventh and twelfth instances of the §202 pattern, and both were found the
same way: by measuring the size of a fix before quoting it, rather than
trusting the sentence that described it.

# Part V (continued) — what fifty engines do not cover

Every pass since §170 has measured defects. §214.4 concluded 0.6.0 should add
no engines and §225 gave a second reason. But "no engines in 0.6.0" is not the
same as "no engines ever", and the question of *which* engine would actually
add something has only ever been answered from a wish list (§17, §13, §123).
This answers it from the registry.

## 234. Sixty per cent of the package is one cell of the problem space

**Method.** The registry declares, per engine, which `change_in` levels it
supports, whether it is univariate, multivariate, and whether it is online.
Cross those into a coverage matrix. Engine counts per cell, offline / online:

| `change_in` | univariate | multivariate |
|---|---|---|
| **mean** | **30 / 2** | 6 / 1 |
| var | 9 / 1 | 2 / 0 |
| meanvar | 6 / 0 | 1 / 0 |
| slope | 6 / 0 | **0 / 0** |
| distribution | 2 / 1 | 4 / 0 |
| covariance | 0 / 0 | 3 / 0 |
| network | 0 / 0 | 2 / 0 |
| regression | 0 / 0 | 2 / 0 |
| seasonality | 1 / 0 | **0 / 0** |

Two numbers dominate everything else:

- **Thirty of the fifty engines do univariate offline mean detection.** That
  one cell is 60% of the package.
- **Three of the fifty are online**: `bocpd`, `cpm`, `ocd`. Forty-seven are
  offline. The streaming column is 6% of the engine count.

### 234.1 Nineteen cells are empty, but only some are gaps

The matrix has 19 empty cells. Counting all of them as missing capability
would be exactly the error §202 keeps catching, so they need separating.

**Definitionally empty — not gaps, and should be recorded as such:**
`covariance` and `network` univariate, offline and online. A covariance
changepoint requires at least two series by construction, and a network
changepoint requires a graph. Four cells that can never be filled, and
`cpt_methods()` currently presents them as though they could — the honest fix
is a registry note, not an engine.

**Genuinely empty and meaningful:**

| cell | what is missing |
|---|---|
| `slope`, multivariate, offline **and** online | **no multivariate trend detection at all.** Six univariate slope engines, zero multivariate. |
| `seasonality`, multivariate, offline and online | one seasonality engine in the whole package (`bfast`), univariate only |
| `regression`, univariate, offline and online | `hdreg` covers the high-dimensional case; a single-covariate regression break has no engine |
| the online column, everywhere past mean | `meanvar`, `slope`, `covariance`, `network`, `regression`, `seasonality` have **no** online detector; `var` and `distribution` have one each, multivariate `var` none |

**Sixteen of the nineteen empty cells are online cells.**

### 234.2 What this says about where expansion is worth anything

The package's fifty engines are not fifty capabilities. They are, roughly,
one very heavily covered capability plus a thin scatter over the rest. A
fifty-first univariate offline mean detector would be the thirty-first engine
in a cell that already has thirty and would not change what any user can do.
That is a stronger argument for §214.4's freeze than the qualitative one, and
it survives past 0.6.0: **adding engines is only worth doing in cells that are
empty or nearly so.**

Ranked by what would actually extend the package's reach:

1. **The online column.** 6% of engines, 16 of 19 empty cells, and §205
   measured that the streaming machinery already works — detection delays of
   2 to 12 observations, one monitor calibrated to within 8% of its declared
   ARL0. The infrastructure exists (`cpt_monitor()`, `cpt_update()`,
   `alarms()`, `cpt_delay()`, `cpt_replay()`) and is fed by three engines.
   This is the one place where a new engine multiplies existing investment
   rather than duplicating it — and §228.1 found the whole subsystem has
   never been demonstrated in a runnable chunk, so the documentation and
   capability gaps coincide exactly.
2. **Multivariate slope.** Zero engines, offline or online, against six
   univariate. Panel data with a common trend break is an ordinary applied
   problem (§148's four fields all have it) and the package cannot answer it.
3. **Seasonality beyond `bfast`.** One engine, univariate, offline. Every
   environmental and retail series has seasonality, and §148 identified those
   audiences explicitly.
4. **Univariate regression breaks.** `strucchange` and `segmented` cover
   pieces of this through `slope`, but the `regression` level itself has no
   univariate engine, which is why §181.1 found `segmented` silently
   accepting `change_in = "mean"` — users are routed around a cell that is
   empty.

And a fifth item that is not an engine: **record the four definitionally
empty cells in the registry**, so `cpt_methods()` stops implying that a
univariate covariance detector is merely missing rather than impossible. That
is a one-column change and it makes the matrix above self-documenting.

### 234.3 The sequencing this implies

- **0.6.0**: no engines. §214's spec — the assumption report, seven measured
  registry columns, the nine fixes, and the five discovery surfaces. The
  matrix above does not change.
- **0.7.0**: engines *only* in empty cells, and the online column first,
  because it is the only cell where the surrounding machinery is already
  built and measured. One online `meanvar` detector and one multivariate
  `slope` detector would fill more of the problem space than the last
  nineteen engines did.
- **Never**: another univariate offline mean detector, unless it brings a
  capability the registry can express — an interval, a posterior, a solution
  path — that the existing thirty do not have. §170's capability flags are
  the test for that, and they are now measured rather than asserted.

## 235. What this pass changes

| § | finding | measured how |
|---|---|---|
| 234 | **30 of 50 engines occupy one cell** (univariate offline mean) — 60% of the package | registry `supports` x `univariate` x `online` |
| 234 | **3 of 50 engines are online**; 16 of the 19 empty cells are online cells | same |
| 234.1 | 4 of the 19 empty cells are **definitionally** empty (univariate covariance and network) and should be recorded, not filled | definitional |
| 234.1 | `slope` multivariate is empty offline **and** online, against six univariate slope engines | same |
| 234.1 | one seasonality engine exists in the entire package, univariate offline | same |

New actions: a registry column marking definitionally-impossible cells; and a
0.7.0 engine policy — empty cells only, online column first.

**What this pass adds.** It is the first forward-looking section since the
measurement began, and it replaces the wish lists of §13, §17 and §123 with a
matrix. The useful reframing is that **the package's engine count and its
capability count are different numbers, and only one of them is fifty.**
Thirty engines answer the same question; the streaming column, multivariate
trend, and seasonality answer questions nothing in the package answers at all.

That also resolves a tension the roadmap has carried since §214.4. "Add no
engines" read as a counsel of despair — sixteen passes of defects, so stop
building. The matrix says something better: **stop building in the cell that
is full, and the cells that are empty are exactly where the existing
infrastructure is already strongest.** §205 measured a working streaming
subsystem with three engines feeding it. Filling that column is not new
scaffolding; it is using scaffolding that has already been paid for and never
shown to anyone.

# Part V (continued) — the axis the matrix was missing

§234 built a coverage matrix over `change_in` x dimension x mode and found the
package is 60% one cell. This pass asked which CRAN packages could fill the
empty cells, and the search turned up something more important than a
shopping list: **the matrix has only one axis, and the missing one is the data
type.**

## 236. The package cannot detect a changepoint in binary or count data

### 236.1 The registry describes what changes, never what kind of data

All nine `change_in` levels — `mean`, `var`, `meanvar`, `slope`,
`distribution`, `covariance`, `network`, `regression`, `seasonality` — name a
*parameter*. All sixteen registry columns were checked: **not one names a
distribution family or a data type.** Every engine in the package assumes a
continuous real-valued series, and nothing records that assumption.

Measured, on the two commonest non-continuous data types a user brings:

| series | truth | `cpt_detect(method = "pelt")` returns |
|---|---|---|
| Bernoulli, `p` = 0.2 -> 0.7 at t = 150 | 1 changepoint | **0 changepoints** |
| Poisson, `lambda` = 2 -> 8 at t = 150 | 1 changepoint | **26 changepoints** (149, 151, and 24 spurious) |

**It fails in both directions and warns in neither.** A success probability
that more than triples produces silence; a rate that quadruples produces
twenty-six detections of which two bracket the truth.

Both failures are already explained by measurements in this document:

- **Binary is §179's scale problem.** 0/1 data has a standard deviation near
  0.45, so relative to the unit-variance Gaussian cost it is the `0.1x` case,
  and §179 measured that `pelt` at `0.1x` returns nothing.
- **Counts are §189's heteroscedasticity problem.** Poisson variance scales
  with the mean, so the second segment has variance 8 against 2, and §189
  measured that `smuce`'s Gaussian family gives 10.00 false positives on
  heteroscedastic data where `hsmuce` gives 0.33.

So this is not a new failure mode. It is two known failure modes reached
through a door nobody had opened, and it means the package silently
mis-analyses two extremely common data types rather than refusing them.

### 236.2 The capability partly exists upstream and is unreachable

`changepoint::cpt.meanvar()` documents `test.stat` values beyond `"Normal"`
— Gamma, Exponential and Poisson among them. `cpt_wrapper()`, the function
that wraps four of the package's fifty methods, has formals
`data, change_in, cp_method, ...`. **There is no `test.stat` argument and no
family argument.** The Poisson cost that would have handled the count series
above is reachable only by passing `test.stat` through `...`, which §173
measured is an undiscoverable interface and §219 measured silently swallows
misspellings.

That is §177's discard thesis in a new place: the distributional families are
implemented upstream, paid for, and not exposed. §126's narrowing audit found
the same shape in `fastcpd`'s families; this is the `changepoint` equivalent
and it was missed because the audit asked about `change_in`, not about
`test.stat`.

### 236.3 What to build

1. **A `data_type` axis in the registry** — `continuous`, `count`, `binary`,
   `categorical`, `compositional`, `circular` — defaulting to `continuous`
   for all fifty engines, which is honest and immediately useful because it
   makes the gap visible in `cpt_methods()` instead of invisible.
2. **Expose `test.stat` on `cpt_wrapper()`** as a documented `family`
   argument, and map `change_in = "meanvar"` plus `family = "poisson"` to it.
   That is one argument and it fixes the count case with code that already
   ships.
3. **Refuse, or loudly warn, on obviously non-continuous input.** A series of
   integers taking two distinct values is binary; a non-negative integer
   series whose variance tracks its mean is counts. Both are cheap to detect
   at `validate_data()` time, and either message is better than 0 or 26
   changepoints. This joins §172's small-`n` and orientation warnings — the
   same function, a third `if`.
4. **`MultipleBreakpoints`** (CRAN, v0.1.0) is "Estimating Multiple
   Breakpoints for a Sequence of Realizations of Bernoulli Variables" — a
   ready-made engine for the binary cell, and the only CRAN package found
   that targets it.

## 237. The CRAN scan: seven candidates, two that fill real cells, one competitor

**Method.** Fetch the live CRAN index (24,678 packages) and match
`change[ -]?point|structural break|structural change|regime switch|abrupt
change` against package names and titles, then subtract everything already in
`Imports`/`Suggests` or the registry. This is a name-and-title net, not a
full-text one, so it is a lower bound.

Sixteen changepoint-adjacent packages, nine already known to the package,
**seven not yet in it**:

| package | version | date | title | which §234 cell |
|---|---|---|---|---|
| **`changepointTests`** | 0.1.7 | 2024-09 | Change Point Tests for Joint Distributions and Copulas | **new — copula / dependence structure, between `distribution` and `covariance`** |
| **`MultipleBreakpoints`** | 0.1.0 | 2021-11 | Multiple Breakpoints for Bernoulli Variables | **new — the binary data type (§236)** |
| `ChangepointTesting` | 1.2 | 2025-05 | Change Point Estimation for Clustered Signals | many-series simultaneous inference; already noted at §107 |
| `changepointGA` | 0.1.5 | 2026-05 | Detection via Modified Genetic Algorithms | a *search strategy* in the full cell — marginal |
| `changepointsVar` | 0.1.2 | 2025-07 | Changes in Variance | `var` univariate, which already has nine |
| `BreakPoints` | 1.2 | 2020-06 | Identify Breakpoints in Series of Data | overlaps the `trend` family already wrapped |
| **`tidychangepoint`** | 1.0.5 | 2026-05 | **A Tidy Framework for Changepoint Detection Analysis** | **not a cell — see §237.1** |

Only two fill genuinely empty ground: `changepointTests` for copula-based
dependence change, and `MultipleBreakpoints` for binary series. The other
four land in cells that are full or nearly so, which is exactly what §234.2
predicted would happen to any engine chosen without consulting the matrix.

### 237.1 `tidychangepoint` exists, is active, and states the same premise

`tidychangepoint` 1.0.5, published 2026-05-04, is titled "A Tidy Framework
for Changepoint Detection Analysis." That is this package's premise in the
same words.

**I know its title, version and publication date and nothing else** — I have
not read its documentation or its API, and I am not going to guess at them
here. What matters for the roadmap is that the fact is now recorded, because
it has never appeared in this document across 554 sections, and it changes
two things:

1. **Positioning needs an actual answer.** "One interface, 50 changepoint
   methods" (the README's opening line) may or may not be a differentiator
   against it. That is a question to settle by reading their package, not by
   assertion.
2. **It is a citation obligation.** A software paper that claims a unified
   tidy interface for changepoint analysis and does not discuss a CRAN
   package with that title, published four months earlier, will be asked
   about it in review.

**The action is to read it, once, properly** — its API, its engine count, its
data structures, whether it wraps engines or implements them — and write a
short honest comparison into the roadmap. Not a competitive audit; a paragraph
saying what each does that the other does not. That is a prerequisite for the
software paper §0.9 anticipates, and it is the one item in this pass that
cannot be done by measurement of this repo.

## 238. What this pass changes

| § | finding | measured how |
|---|---|---|
| 236.1 | **`pelt` returns 0 changepoints on a Bernoulli series** whose `p` goes 0.2 -> 0.7, and **26 on a Poisson series** whose rate goes 2 -> 8, truth being 1 in both cases | direct detection on both |
| 236.1 | the registry has **no data-type axis**: all nine `change_in` levels name a parameter, and none of the sixteen columns names a distribution family | registry inspection |
| 236.2 | `changepoint::cpt.meanvar()` supports Poisson/Gamma/Exponential `test.stat`; `cpt_wrapper()` exposes **no** family argument, so they are reachable only through `...` | formals comparison |
| 237 | of 16 changepoint-adjacent CRAN packages, 7 are not in this package, and only **2** fill genuinely empty ground | live CRAN index, 24,678 packages |
| 237.1 | **`tidychangepoint` 1.0.5 (2026-05-04), "A Tidy Framework for Changepoint Detection Analysis"** — same stated premise, never mentioned in 554 sections | CRAN DESCRIPTION |

New actions: a `data_type` registry axis; a documented `family` argument on
`cpt_wrapper()`; non-continuous input detection in `validate_data()`;
`MultipleBreakpoints` and `changepointTests` as the only two engine
candidates worth wrapping; and read `tidychangepoint` and write an honest
comparison.

**What this pass adds.** §234 said the engine count and the capability count
are different numbers. This pass says the capability count itself was measured
along the wrong axis: **the matrix enumerated the parameters the registry
knows about, and the registry does not know about data types at all.** A user
with a binary series or a count series — clinical events, defect counts,
click-throughs, rainfall days, hospital admissions — gets 0 or 26 changepoints
and no warning, from a package with fifty engines.

That is a larger reach problem than any empty cell in §234, and the first two
fixes cost one registry column and one argument. It also reframes the engine
question a second time: **the most valuable engine to add is not one that
detects a new parameter, it is one that accepts a new data type.**

And §237.1 is the first item in this document that requires looking outside
this repository. Sixteen passes of internal measurement have been productive
precisely because everything was checkable here; the existence of an actively
maintained CRAN package with the same one-line premise is the point where that
stops being sufficient.

# Part V (continued) — two published findings, corrected by independent check

Two of this document's measured claims were re-checked from scratch this pass —
one because a delegated worker flagged it, one because that flag made me look
at a neighbouring result. Both were wrong in ways worth recording, and the
sections they live in stay as written with these corrections attached.

## 239. §183.3's "14 exports absent from the pkgdown index" was a category error

**The claim.** §183 reported 14 exports absent from `_pkgdown.yml`'s reference
index, and §183.3 built on it to conclude that four accessibility scales are
"invisible in both surfaces" — neither cross-referenced nor on the website.

**Independently re-checked, and it is a false positive.** pkgdown's reference
index lists **topics**, meaning `.Rd` page names. An alias appears on the
website under its topic's row, rendered as its own linked entry. My check
compared *export names* against the yml's *topic names*, so every alias that
is not its page's primary name counted as missing. Mapping each of the 14 to
the page that documents it:

| export | its `.Rd` topic | topic listed in `_pkgdown.yml`? |
|---|---|---|
| `as_tibble`, `augment`, `autoplot`, `glance`, `tidy` | `reexports` | **yes** |
| `cpt_registered_methods`, `cpt_unregister_method` | `cpt_register_method` | **yes** |
| `ggcpt_scale_space` | `cpt_scale_space` | **yes** |
| `ggcpt_solution_path` | `cpt_solution_path` | **yes** |
| `ggcpt_statistic` | `cpt_statistic` | **yes** |
| `scale_color_cpt`, `scale_fill_cpt`, `scale_linetype_cpt` | `scale_colour_cpt` | **yes** |
| `scale_colour_cpt_label` | `scale_fill_cpt_label` | **yes** |

All fourteen. And `wbs2_wrapper`, which the same check also flagged, is
listed in the file literally. `pkgdown::check_pkgdown()` passed before any of
this, which should have been the tell — that function's whole job is to find
un-indexed topics, and it found none.

**Consequences:**

- **§183.4 item 2 is withdrawn.** There are no missing topics to add, and
  adding the aliases explicitly would duplicate rows on the index page.
- **§183.3's "invisible in both surfaces" is withdrawn.** The four
  accessibility scales are on the website, under `scale_colour_cpt`'s row.
  Their real gaps are two, not three: no inbound `\link{}`, and no runnable
  chunk (§228.2). §232.1 already corrected the third — they do have an
  example, on the shared page.
- **§183's headline stands unchanged**: 59 of 130 exports with no inbound
  `\link{}` was measured against `.Rd` files, not the yml, and that number was
  never in doubt.

The general lesson is narrower than "check your regex". **Two surfaces that
look like lists of the same thing were not**: `NAMESPACE` lists exported
objects, `_pkgdown.yml` lists documentation topics, and the mapping between
them is many-to-one. Every comparison in this document between an export list
and a documentation list should be read with that in mind — §174, §228 and
§229 all keyed by alias and are therefore fine, and this one did not.

## 240. §206's `cpm` alarm at stream index 1 is a baseline bug, not dependence

**The claim.** §206 measured that under AR(1) noise, `cpm`'s median first
false alarm arrives at **stream index 1**, and read that as dependence
tripping the threshold immediately — "the baseline was also AR(1), so the
in-control estimate is not the problem."

**That attribution is wrong.** Re-measured with the baseline path varied:

| setup | `cpm` first false alarm, six replicates |
|---|---|
| iid data, **with** a baseline | 120, **1**, 505, **1**, 125, 540 |
| AR(1) data, **with** a baseline | **1, 1, 1, 1, 1, 1** |
| iid data, **no** baseline | 721, 92, 392, 102 — and only 1-3 alarms in 800 observations |

So the t = 1 alarm is a property of the **baseline path**, not of the noise.
It fires on clean i.i.d. data in two of six replicates, and under dependence it
fires in six of six. Without a baseline, `cpm` behaves sanely and never fires
at t = 1.

The mechanism, which a delegated reviewer of the monitoring API identified
independently: supplying `baseline` to `method = "cpm"` runs the observations
through `cpm::changeDetected()`, which is already `TRUE` when the baseline
loop finishes, so the first monitored observation inherits a tripped state.
`?cpt_monitor` recommends building `cpm` without a baseline, which sidesteps
it — and that recommendation is now known to be load-bearing rather than
stylistic.

**Consequences:**

- **§205's ARL0 finding stands.** `cpm` at 3.7 false alarms against 4.0
  expected was measured with a baseline, and 3.7 is still the right number
  for that configuration. The calibration claim is unaffected.
- **§206's inflation factors stand** — `edetector` 3.9x, `cpm` 10.2x under
  AR(1) — but part of `cpm`'s 10.2x is this bug rather than dependence, so the
  dependence component is smaller than reported and the true figure needs a
  no-baseline re-run.
- **§206.1 gains an item, and it moves to the top**: fix the `cpm` baseline
  handoff, because a monitor that alarms on its first observation is
  unusable, and this is the only defect found in the streaming subsystem that
  is the package's own rather than an assumption violation.
- **§207's summary sentence needs qualifying.** It said the streaming
  subsystem's problem is "what it assumes is unchecked, and the assumption is
  the same one" as offline. That is true of `edetector` and of `cpm` without a
  baseline. With a baseline, `cpm` has a plain bug on top.

And a second defect from the same review, recorded here because it is in
`R/monitor.R` and was not reachable from anything this document had measured:
**`?cpt_delay` documents that `cpt_metrics()` warns when pointed at a
monitor. It has no monitor branch at all** — it fails with `'list' object
cannot be coerced to type 'integer'`. That is §170's pattern once more, a
documented behaviour that does not exist, and it belongs on §214.3's fix list.

## 241. What this pass changes

| § | correction | measured how |
|---|---|---|
| 239 | **§183's "14 exports absent from the pkgdown index" is withdrawn** — all 14 are aliases whose topic page is listed; `check_pkgdown()` was passing all along | every alias mapped to its `.Rd` topic, topics matched against the yml |
| 239 | **§183.3's "4 exports invisible in both surfaces" is withdrawn**; the scales have two real gaps, not three | same |
| 240 | **§206's `cpm` alarm at index 1 is a baseline-path bug**, firing on clean iid data in 2 of 6 replicates and never without a baseline | first-alarm index across three baseline/noise configurations |
| 240 | `cpt_metrics()` has no monitor branch despite `?cpt_delay` documenting one | direct call |

New actions: fix the `cpm` baseline handoff (top of §206.1); add a monitor
branch or correct the doc for `cpt_metrics()`; and re-run §206's `cpm`
inflation figure without a baseline to separate the bug from the dependence.

**What this pass adds.** Thirteenth and fourteenth instances of the §202
pattern, and the first two produced by a *second pair of eyes* rather than by
me re-measuring my own work. That is worth noting as a method result: sixteen
passes of self-checking caught twelve errors, and the first independent review
of the same material caught two more within an hour, both in places I had
re-read several times.

The §239 error is the more instructive of the two. It was not a mistake in a
measurement; it was a mistake about **what two lists mean**. `NAMESPACE`
enumerates exported objects and `_pkgdown.yml` enumerates documentation
topics, the mapping is many-to-one, and comparing their cardinalities produces
a plausible, specific, entirely fictitious gap — which then propagated into
three later sections as an established fact. The safeguard §208.1 introduced
(re-measure before quoting) does not catch this class, because the
measurement was repeatable and wrong. What catches it is the question §202's
rule already implies and I did not ask here: **before believing a difference
between two sets, check that they are sets of the same kind of thing.**

# Part V (continued) — the tarball is 83% base64 PNG

The 0.5.0 documentation pass closed the two worst discovery surfaces, and
building the result surfaced a pre-submission problem nothing had measured:
**the source tarball is 5.16 MB.** CRAN's guidance puts packages under 5 MB
and flags what exceeds it. CRAN's own published 0.4.0 was 2.26 MB.

## 242. Four hundred and thirty megabits of embedded images

**Method.** `R CMD build`, then for each built vignette measure total HTML
size, the bytes matching `data:image/…;base64,…`, and the number of such
images.

| built vignette | HTML | base64 payload | share | images |
|---|---|---|---|---|
| `ggchangepoint.html` | 1,665.6 KB | 1,498.6 KB | **90%** | 26 |
| `introduction.html` | 952.7 KB | 853.8 KB | 90% | 13 |
| `inference.html` | 860.1 KB | 814.6 KB | **95%** | 10 |
| `comparison.html` | 837.4 KB | 771.5 KB | 92% | 9 |
| `extending.html` | 233.4 KB | 193.0 KB | 83% | 2 |
| `supervised.html` | 206.4 KB | 171.2 KB | 83% | 3 |
| `monitoring.html` | 142.9 KB | 92.7 KB | **65%** | 2 |
| **total** | **4.95 MB** | **4.29 MB** | **~89%** | **65** |

**4.29 MB of the 5.16 MB tarball — 83% of everything CRAN would receive — is
base64-encoded PNG.** Sixty-five figures at an average of **67.6 KB each**.
The R source is 0.5 MB, the tests 0.22 MB, `man/figures` 1.24 MB, and
everything else rounds to nothing beside the pictures.

This confirms and sharpens §84, which measured 82-94% from a different angle.
The range is 65-95%, and the outlier is instructive: `monitoring.html` is the
*least* image-dominated vignette at 65%, because it is mostly prose, tables
and printed output with two figures. It is also the newest, and it added only
0.14 MB. **The new vignette is not the problem; the four large pre-existing
ones are.**

### 242.1 What the arithmetic permits

- To get **under 5 MB** the tarball must shed about 160 KB — roughly **three
  figures**, or one modest global setting.
- To get back toward **3 MB**, comfortable rather than borderline, it must
  shed about 2.2 MB — roughly **32 figures**, or halve every figure's byte
  size.
- Base64 inflates binary by about a third, so the underlying PNGs are about
  3.2 MB. Halving the pixel dimensions of a raster figure divides its byte
  size by roughly four, so the levers are strong if they work.

### 242.2 The options, with honest labels on which are measured

**Measured:**

1. **Move `ggchangepoint.Rmd` to `vignettes/articles/`.** Its built HTML is
   1.67 MB, a third of `inst/doc`, and this would take the tarball to about
   **3.5 MB** immediately. The cost is real and specific: §228 measured that
   the feature tour executes **86 of 130 exports across 78 chunks**, more
   than twice any other document, so this removes the package's most
   informative vignette from the CRAN distribution while keeping it on the
   website. A defensible trade for a reference tour, and the one lever whose
   effect is already quantified.
2. **The `monitoring.Rmd` shape works.** 65% base64, two figures, and it
   still demonstrates a whole subsystem — prose, tables and printed output
   carry most of the content. Whatever else is done, new vignettes should be
   written this way.

**Not measured, and I tried:**

3. `fig.retina = 1`, `dpi = 72`, smaller `fig.width`/`fig.height`, and
   `dev = "svg"` are the standard levers, and **I could not measure them.**
   Rendering copies of a vignette from a scratch directory failed at the
   Pandoc stage (`error 99`) for all six configurations — the R chunks ran,
   the conversion did not, almost certainly because an `html_vignette`
   rendered outside `vignettes/` loses resources it expects. The savings are
   therefore *estimated* by the pixel arithmetic above and not established
   here. **Anyone acting on this should measure in place**, by setting the
   option in the real vignette and rebuilding, not by rendering a copy.
   `dev = "svg"` deserves particular attention: these are line-and-point
   plots on plain backgrounds, which is the case where vector output is
   usually smaller than raster, sometimes by an order of magnitude — but
   `ggplot2` output with many geom points can invert that, so it must be
   measured per vignette rather than assumed.

### 242.3 Why this belongs in the roadmap rather than a fix list

Three of the five documentation surfaces are now in good shape, and the
measured cost of getting there was 0.14 MB of tarball for a whole new
subsystem's documentation. The remaining surfaces — 14 `plot.*` methods,
`cpt_match_arg()`, the error messages — add no figures at all. So the size
problem is not a consequence of the documentation work and will not be made
worse by finishing it.

What it does constrain is **anything that adds figures**. §229.3's remaining
item, §214's assumption report, and any future vignette all want plots, and
at 67.6 KB per figure the budget is about **three figures before 5 MB
becomes 5.2 MB.** That is the number to hold in mind, and it argues for the
`vignettes/articles/` split as policy rather than as a one-off rescue:
**vignettes that teach the API ship in the tarball; vignettes that show
results live on the website.** The benchmarks article added in the 0.5.0 pass
is already on that side of the line and costs the tarball nothing, which is
the proof the pattern works.

## 243. What this pass changes

**Deletions — items built in the 0.5.0 documentation pass, verified before
removal:**

| section | item deleted | verified |
|---|---|---|
| §174.1 | one runnable example per export | 125 of 130 exports carry one (was 99) |
| §174.1 | `cpt_registered_methods()` into a vignette | executes in a chunk |
| §183.4 | make `?ggchangepoint` the index | 16 outbound links (was 0) |
| §183.4 | one `@family` tag per group | 70 tags; inbound links 71 -> 124 |
| §183.4 | add the 14 missing pkgdown topics | withdrawn as a false positive (§239) |
| §226.1 | vignette setup guard for the Tk warning | `suppressWarnings()` in place, 0 `no DISPLAY` in output |
| §229.3 | a monitoring vignette | `vignettes/monitoring.Rmd`, renders in 8.8 s |
| §229.3 | chunks for the six unrun diagnostics and power functions | present |
| §229.3 | a chunk exercising the accessibility scales | present |
| §229.3 | an `articles:` order in `_pkgdown.yml` | present, `introduction` first |

**New finding:**

| § | finding | measured how |
|---|---|---|
| 242 | **4.29 MB of the 5.16 MB tarball (83%) is base64-encoded PNG**; 65 figures at 67.6 KB each | `R CMD build`, then base64 bytes per built vignette |
| 242 | `inference.html` is **95%** image payload; `monitoring.html` is 65% and is the model | same |
| 242.2 | moving `ggchangepoint.Rmd` to `vignettes/articles/` takes the tarball to **~3.5 MB**, at the cost of removing the vignette that executes 86 of 130 exports | measured HTML size vs §228's chunk audit |
| 242.2 | **could not measure** the figure-setting levers — six configurations all failed at the Pandoc stage when rendered from a copy | attempted and reported as unmeasured |

New actions: measure `fig.retina`/`dpi`/`dev = "svg"` **in place** rather than
on copies; adopt the tarball/website split as vignette policy; treat 67.6 KB
per figure as the standing budget.

**What this pass adds.** The deletion column is the first substantial one
since Part II, and it is worth noting what made it possible: every item
deleted was an item that had been stated as a *checkable* claim — a count of
exports with examples, a count of inbound links, the existence of a file.
Nothing vague was deleted because nothing vague could be verified. §231.2's
observation that this package keeps what is asserted and loses what is not
now has a corollary for the roadmap itself: **a roadmap item written as a
number can be retired; one written as an intention cannot.**

# Part V (continued) — §242.2's unmeasured levers, measured

§242.2 listed four figure settings as the standard way to shrink a vignette
and reported honestly that **I could not measure them** — rendering copies of
a vignette from a scratch directory failed at the Pandoc stage in all six
configurations. It closed with: *"anyone acting on this should measure in
place."* This does that, without touching the repository: three full copies of
the package tree in scratch, the setting injected into all seven vignettes'
`opts_chunk$set()` calls, and `R CMD build` run on each.

## 244. Two of the three levers fail, and the one with the best tarball is the wrong answer

| configuration | tarball | `inst/doc` | largest HTML |
|---|---|---|---|
| **baseline** (as shipped) | 5.16 MB | 4.95 MB | 1.63 MB |
| `fig.retina = 1` | **5.16 MB** | 4.95 MB | 1.63 MB |
| `fig.retina = 1` + `dpi = 72` | **4.22 MB** | **3.66 MB** | 1.18 MB |
| `dev = "svg"` | **3.58 MB** | **8.73 MB** | 2.80 MB |

Three findings, and the third is the one that matters.

### 244.1 `fig.retina` is already at its efficient setting

`fig.retina = 1` changed **nothing** — byte-identical tarball and `inst/doc`.
None of the seven vignettes sets `fig.retina`, so this was the obvious first
lever, and it does not exist: `rmarkdown::html_vignette` evidently already
renders at retina 1, unlike `html_document` which defaults to 2. **§242.2's
first suggestion is withdrawn.** Worth recording because it is the change
somebody would try first, it takes two minutes to apply, and it would have
produced a diff with no effect and no explanation.

### 244.2 `dpi = 72` is the real lever: -18% and it fixes the problem

Adding `dpi = 72` takes the tarball from 5.16 MB to **4.22 MB** and
`inst/doc` from 4.95 MB to **3.66 MB** — an 18% and 26% reduction, and it puts
the package **under CRAN's 5 MB line** with a megabyte to spare. The largest
vignette drops from 1.63 MB to 1.18 MB.

Since `fig.retina = 1` alone did nothing, the default `dpi` for this path is
above 72, and lowering it is the whole effect. The figures are `8 x 5` inches
in every vignette, so at 72 dpi that is 576 x 360 pixels — adequate for a
vignette read in a browser, and the visual cost should be checked by eye
before adopting, which is the one thing a byte count cannot tell you.

### 244.3 `dev = "svg"` has the best tarball and the worst installed size

This is the finding worth the whole pass. SVG produces the **smallest
tarball** of anything tested — 3.58 MB, a 31% reduction, the best number in
the table — and the **largest `inst/doc` by far at 8.73 MB**, against a
baseline of 4.95 MB. Its biggest vignette is 2.80 MB where the baseline's is
1.63 MB.

The mechanism: the figures are still embedded as base64
(`data:image/svg+xml;base64`, zero inline `<svg>` elements), and SVG is
verbose XML describing every point and line. Uncompressed that is far larger
than a PNG of the same plot. But XML compresses extremely well, and a
`.tar.gz` is compressed — so the tarball shrinks while the thing a user
actually installs nearly doubles.

**CRAN checks both.** "Checking installed package size" is a separate NOTE
from the tarball size, and 8.73 MB of `inst/doc` would draw it. So the
configuration with the best headline number is the one that would fail — and
§242.2, which flagged `dev = "svg"` as deserving "particular attention"
because vector output "is usually smaller than raster" for line plots, was
right about the compressed bytes and wrong about the consequence.

**The general lesson, and it is not about figures.** A single metric picked
the wrong configuration. Tarball size and installed size move in *opposite*
directions across this change, and every earlier discussion in this document —
§84, §242 — reasoned about tarball size alone. Any future size work must
report both columns, because one of them is optimised by making the other
worse.

### 244.4 What to do

1. **Adopt `dpi = 72`** in all seven vignettes' `opts_chunk$set()`, after
   checking a rendered figure by eye. Measured: 5.16 -> 4.22 MB tarball,
   4.95 -> 3.66 MB installed. It is one line per vignette and it resolves the
   submission risk on its own.
2. **Do not adopt `dev = "svg"`** despite its tarball number (§244.3).
3. **Drop `fig.retina` from §242.2's list** — no effect (§244.1).
4. **Keep the `vignettes/articles/` split as policy** (§242.2 item 1). It is
   still the right structural answer, and with `dpi = 72` it becomes optional
   rather than forced, which is a better place to make that decision from.
5. **Report both size columns** in any future measurement, and add both to
   whatever CI check eventually guards package size.

## 245. What this pass changes

Deletion pass: nothing. `HEAD` is unchanged at `da401e2` and only
`next_release.md` is dirty, so no roadmap item has been built since §243.

| § | finding | measured how |
|---|---|---|
| 244.1 | **`fig.retina = 1` has no effect** — `html_vignette` already renders at retina 1; §242.2's first suggestion withdrawn | full build, byte-identical output |
| 244.2 | **`dpi = 72` takes the tarball 5.16 -> 4.22 MB and `inst/doc` 4.95 -> 3.66 MB**, clearing CRAN's 5 MB line | full build |
| 244.3 | **`dev = "svg"` gives the smallest tarball (3.58 MB) and the largest installed size (8.73 MB)** — the best headline number is the failing configuration | full build, both columns |
| 244.3 | SVG figures are still base64-embedded, not inline `<svg>`; XML compresses well and installs badly | grep of the built HTML |

New actions: adopt `dpi = 72` after an eyeball check; reject `dev = "svg"`;
drop `fig.retina`; report tarball **and** installed size in all future size
work.

**What this pass adds.** It converts §242.2's three estimates into one
measured recommendation and two rejections, and the method matters as much as
the result: the earlier attempt failed because it rendered a *copy of a file*,
and this one worked because it built a *copy of the package*. The fix for
"I could not measure that" was to enlarge the unit of copying, not to reason
harder about it.

And §244.3 is the fifteenth instance of the §202 pattern, arriving in a new
form. The previous fourteen were claims that were too strong or attributed to
the wrong cause. This one is a **measurement that was correct and a
recommendation that was backwards**: `dev = "svg"` really does produce the
smallest tarball, exactly as §242.2 predicted, and adopting it would have made
the package worse. The rule §202 has been accumulating needs one more clause:
*measure the mechanism, measure whether the mechanism supports the claim, and
check that the metric you optimised is the one that is checked.*

# Part V (continued) — the data-type axis, measured across every engine

§236 discovered that the registry has no data-type axis and measured `pelt`
returning **0** changepoints on a Bernoulli series and **26** on a Poisson
one. §236.3 asked for a `data_type` registry column. This is the measurement
that would populate it — and it corrects §236's headline.

## 246. Recovery is fine; false positives explode

**Method.** 30 univariate engines, five replicates, n = 400, one changepoint
at 200, four data types at comparable effect sizes: a 1.4 sd Gaussian mean
shift; Bernoulli `p` 0.15 -> 0.75; Poisson rate 2 -> 10; and a proportion
(binomial/20) 0.25 -> 0.6. `hit` is the fraction of replicates recovering the
change within +/-20; `fp` is the mean number of spurious changepoints.

| data type | mean hit rate | mean false positives | engines at hit = 1.00 | engines with > 5 fp |
|---|---|---|---|---|
| Gaussian | 0.89 | **0.12** | 25 of 30 | **0** |
| Bernoulli | 0.87 | **19.86** | 24 of 30 | 6 |
| Poisson | **0.97** | 3.31 | 29 of 30 | 4 |
| proportion | 0.78 | 5.24 | 23 of 30 | 2 |

### 246.1 §236's binary finding was a power artifact, and is corrected

§236 reported that `pelt` "returns 0 changepoints" on a Bernoulli series and
read that as an inability to handle the data type. Measured here at a
stronger signal, `pelt` on binary data recovers the change in **5 of 5
replicates with zero false positives** — the cleanest result in the binary
column.

The difference is signal strength: §236 used `p` 0.2 -> 0.7 at n = 300, this
uses 0.15 -> 0.75 at n = 400. So the earlier finding was **a power problem at
that effect size, not a data-type failure**, and the sentence "the package
cannot detect a changepoint in binary data" is withdrawn. Twenty-four of
thirty engines recover it, and twenty-nine of thirty recover the Poisson
change.

**What survives from §236 is the other half, and it is the real problem.**
Recovery is not where non-continuous data breaks the package; **false
positives are.** The Gaussian column has a mean of 0.12 spurious changepoints
and *no* engine above 5. The Bernoulli column has a mean of 19.86 — a
**165-fold increase** — and six engines above 5.

### 246.2 Four engines detect essentially every 0-to-1 transition

The binary mean is driven by a cluster of identical values:

| engine | spurious changepoints on a 400-point binary series |
|---|---|
| `wbs2`, `smuce`, `decafs`, `nsp` | **118.4** each |
| `idetect` | 110.4 |
| `hsmuce` | 10.2 |

Four engines landing on exactly 118.4 is not coincidence. The expected number
of 0↔1 transitions in this series is `2(n p₁(1-p₁) + n p₂(1-p₂))` ≈ **126**,
and 118.4 is 94% of that. **They are detecting almost every transition.**

That is the structural fact the `data_type` column exists to record: **a
binary series is a step function everywhere**, so a threshold-based detector
without a minimum-segment floor sees a changepoint at every flip. It is the
same missing guard §172.1 found when `pelt` returned a changepoint after
every observation on a three-point series — `minseglen` defaults to `NULL`
and nothing imposes a floor. Binary data makes that omission catastrophic
rather than merely odd.

`decafs` on proportion data is the worst single cell in the table at **143.2**
spurious changepoints, and `hsmuce` produces 12.6 there — so the pathology is
not confined to strict 0/1 data; any series taking few distinct values
triggers it.

### 246.3 Poisson over-segmentation is confirmed and is a different mechanism

| engine | spurious changepoints on Poisson data |
|---|---|
| `pelt` | **34.8** |
| `fpop` | 29.6 |
| `hsmuce` | 13.8 |
| `wbs2` | 8.0 |

`pelt` and `fpop` are precisely the two engines §179 measured as
scale-sensitive and §189 tied to a unit-variance Gaussian cost. Poisson
variance tracks the mean, so segments have variances 2 and 10 — the
heteroscedastic case — and these two over-segment exactly as predicted. This
is not a new mechanism; it is §189's, reached through the data type.

Note the contrast: on binary data `pelt` is **perfect** (0 fp) and `wbs2` is
catastrophic (118.4); on Poisson data `pelt` is catastrophic (34.8) and
`wbs2` is mild (8.0). **The engines that fail differ by data type**, which is
why one `data_type` column cannot be a single flag and must be a table.

### 246.4 What a `data_type` column should actually contain

Engines with **zero** false positives across all four types, and a hit rate of
1.00 on at least three: `sn`, `kcp`, `fastcpd`, `bfast`, `pettitt`,
`buishand`, `snht`, `beast`, `tguh`, `np`. **Ten engines are safe on every
data type tested** — that is the recommendation a user with count or binary
data needs and cannot currently obtain.

So the column is not `data_type` as a property of the engine; it is a
**per-engine, per-type false-positive table**, exactly parallel to §187's
noise-regime table. Concretely:

1. **Ship the table** (this section's data at more replicates) as package
   data, keyed engine x data type.
2. **`cpt_recommend()` gains a `data_type` argument** and refuses or warns
   about the six engines above 5 false positives on binary input.
3. **Detect the data type from the input** and warn: a series taking two
   distinct values is Bernoulli; a non-negative integer series whose variance
   tracks its mean is counts; a series in [0,1] on a coarse grid is a
   proportion. All three are two-line checks at `validate_data()` time, and
   §172/§181's warnings already establish that as the place for them.
4. **Impose a `minseglen` floor** (§172.1's open item), which is the single
   change that would fix the 118.4 cluster — those engines are not wrong
   about the data, they are unconstrained.

## 247. What this pass changes

Deletion pass: nothing. `HEAD` is unchanged at `da401e2`, only
`next_release.md` is dirty.

| § | finding | measured how |
|---|---|---|
| 246 | Gaussian data gives **0.12** mean false positives and no engine above 5; Bernoulli gives **19.86** and six engines above 5 — a 165-fold increase | 30 engines x 4 data types x 5 reps |
| 246.1 | **§236's "cannot detect a changepoint in binary data" is withdrawn** — it was a power artifact; `pelt` recovers it 5 of 5 with zero false positives at a stronger signal | same, at a larger effect size |
| 246.2 | `wbs2`, `smuce`, `decafs` and `nsp` each report **118.4** spurious changepoints on binary data — **94% of the 126 expected 0↔1 transitions** | arithmetic against the transition count |
| 246.3 | `pelt` 34.8 and `fpop` 29.6 on Poisson — §189's heteroscedasticity mechanism, reached through the data type | same |
| 246.4 | **ten engines have zero false positives across all four data types** | same |

New actions: ship a per-engine, per-data-type false-positive table; a
`data_type` argument on `cpt_recommend()`; data-type detection and a warning
in `validate_data()`; and the `minseglen` floor, which is now the fix for two
separate findings.

**What this pass adds.** Sixteenth instance of the §202 pattern, and the first
where the corrected claim was **two passes old rather than fourteen**. §236
measured a real number — `pelt` returned 0 changepoints — and drew a
conclusion about the data type when the cause was the effect size. One
stronger signal reversed it.

The corrected picture is more useful than the original. "The package cannot
handle binary data" implied a missing capability and pointed at wrapping
`MultipleBreakpoints`. What the measurement actually shows is that **the
detection works and the restraint is missing**: engines recover
non-continuous changepoints at nearly the Gaussian hit rate and then report a
hundred more that are not there. That is not a gap to fill with a new engine;
it is a floor to impose on the thirty that already work — and §172.1 asked for
that floor for an unrelated reason two hundred sections ago.

# Part VI — the pass that ships

Sections 244-247 measured and recommended; `HEAD` had not moved and nothing
had been built since §243. This pass takes the recommendations that are
*polish* rather than *features* and applies them, then keeps auditing. Six
defects, five of them shipped fixes and one of them found by a test written
for a different defect.

## 248. `plot()` was broken on 8 of 14 result classes, and wrong on a 9th

§217 counted the missing `plot()` methods. Measured again here, on
constructed objects rather than from the method table:

| class | `plot()` before |
|---|---|
| `ggcpt_selection`, `ggcpt_stability`, `ggcpt_sensitivity`, `ggcpt_influence`, `ggcpt_monitor`, `ggcpt_path`, `ggcpt_events` | `'x' is a list, but does not have components 'x' and 'y'` |
| `ggcpt_batch` | `'list' object cannot be coerced to type 'double'` |
| `ggcpt_power`, `ggcpt_benchmark`, `ggcpt_label_curve` | `plot.data.frame()`'s pairs plot, returning `NULL` |
| `ggcpt_consensus` | **the wrong figure, silently** |

### 248.1 The ninth is the one §217 did not find

`plot.ggcpt()` read

```r
plot.ggcpt <- function(x, ...) autoplot.ggcpt(x, ...)
```

— a **hard call, not a dispatch**. `ggcpt_consensus` inherits `ggcpt`, so
`plot()` on a consensus result reached `plot.ggcpt()` and drew
`autoplot.ggcpt()`'s plain changepoint plot instead of
`autoplot.ggcpt_consensus()`'s. Measured on the labels: the consensus plot
is titled *"Consensus of 2 methods"* with the vote rule as its subtitle; the
`plot()` output was titled *"Changepoint Detection (consensus)"* with no
subtitle. Nothing errored, nothing warned, and the figure was a different
figure.

That is the failure mode §217's method-table survey could not see: it
counted classes without a `plot` method, and this class *had* one. **The
count was right and the diagnosis incomplete.**

Shipped: `R/plot-methods.R`, thirteen methods on one shared
`plot_via_autoplot()` helper that calls `autoplot(x, ...)` — the generic —
then draws and returns invisibly, so `plot()` works inside a loop while
`p <- plot(x)` still yields something to layer on.

## 249. `dpi = 72` adopted, and the measurement reproduced

§244 predicted 5.16 -> 4.22 MB. Built from a copy of the working tree:
**4,430,230 bytes = 4.22 MiB**, and `inst/doc` at 3.5 MB across seven
vignettes (largest `ggchangepoint.html` at 1.18 MB, 26 figures). The
prediction was exact.

The eyeball check §244.4 asked for: figures are 576 x 360 px (8 x 5 in at
72 dpi, down from 768 x 480), and `html_vignette` displays them at natural
size, so the change removes pixels rather than shrinking the picture. Axis
labels, point glyphs and the changepoint rule are all crisp in the rendered
`introduction.html`. Adopted in the seven package vignettes;
`vignettes/articles/benchmarks.Rmd` is web-only and `.Rbuildignore`d, so it
keeps the higher resolution.

## 250. `cpt_detect(factor(...))` returned a result

The worst defect in this pass, and it is a silent wrong answer.

```r
series <- as_cpt_series(x, index = index)
x <- series$values          # <- x is replaced by the coerced vector
idx <- series$index
validate_data(x)            # <- validates the coercion, not the input
```

`as_cpt_series()` ended in a bare `as.numeric(values)`, and `as.numeric()`
on a factor returns its **level codes**. So a factor series was detected on
an alphabetical ordering of its labels, `validate_data()` saw a clean
numeric vector, and the result printed like any other. Measured: no error,
no warning, a populated `ggcpt`.

The same line made character input report the wrong cause — base R's *"NAs
introduced by coercion"*, then `` `x` must be finite (no NA/NaN/Inf) ``,
which blames the data for being non-finite when the problem is that it is
text.

### 250.1 The fix has to preserve everything that already converted

A guard that rejects `!is.numeric(x)` would break `ts`, `zoo`, `xts`,
`table`, `difftime` and `Date` inputs, all of which convert cleanly and
several of which are documented paths. Checked one by one: `is.numeric()`
is TRUE for `ts`, `zoo`, `xts`, `table` and `array`, and **FALSE for
`difftime` and `Date`** — so the type test alone would have silently
removed two working input types.

`coerce_series_values()` therefore names the two traps explicitly and
otherwise refuses only what **R itself** flags: it runs `as.numeric()` under
a calling handler and rejects the input if the conversion warns or errors.
Verified across twelve types: numeric, integer, logical, `ts`, `zoo`,
`table`, `difftime`, `Date` and a numeric list all convert; factor,
character and a list of `NULL`s are refused, each naming the argument and
the package.

### 250.2 And the two ends of the package disagreed about what a series is

`cpt_detect()` coerces then validates; `cpt_select()` validates then
coerces. So a **logical** series — a perfectly ordinary 0/1 series — worked
in the first and was refused by the second. `validate_data()` now accepts
`is.logical()` alongside `is.numeric()`, and its final branch delegates to
`coerce_series_values()` so both routes give the same message for a factor.

## 251. Three empty-input errors came from base R

| call | before |
|---|---|
| `cpt_batch(NULL)` | `'data' must be of a vector type, was 'NULL'` |
| `cpt_batch(list())` | **no error at all** |
| `cpt_consensus(data.frame())` | `subscript out of bounds` |

None names the argument or the package. Guards added at
`as_mv_matrix()`, `as_uni_vector()` (a zero-column frame reaching
`X[, 1]`), and `cpt_batch()`'s list branch.

## 252. §226's first mitigation had never shipped, and the suite proved it

§226 recommended two things and recorded only the second as shipped: the
vignette's `requireNamespace()` was wrapped, the **call site was not**.
Measured in a fresh session with `DISPLAY` unset:
`cpt_scale_space(x, bandwidths = c(20, 40, 80))` emitted exactly one
warning, *"no DISPLAY variable so Tk is not available"*, and newly loaded
`mosum`, `tcltk`, `plot3D` and `misc3d`. Every headless user — server,
container, CI runner, cluster node — saw it.

Fixed in `need_pkg()`, the single funnel every wrapper uses, rather than at
each call site: a load that warns still succeeds, and a load that fails
already returns `FALSE` and is reported. Re-measured: zero warnings, 1,800
statistic rows unchanged, and a missing package still errors with its own
name and the install command.

### 252.1 The suite's copy of the warning is testthat's, not the package's

With the fix in place the warning was still in the test report — attached to
`cpt_statistic`, which loads `mosum` through `need_pkg()` like everything
else. The cause is `skip_if_not_installed("mosum")`: **testthat loads the
package to answer the question**, before any package code runs. So the first
mosum test in the suite pulled `tcltk` in and warned. `setup.R` now does
that load once, with warnings suppressed, which is also why the warning can
no longer be measured from inside the suite.

Which made the obvious test — call `cpt_scale_space()` and assert silence —
skip permanently, because `tcltk` is loaded before it runs. The test that
replaced it reproduces the *mechanism* instead: attach an `onLoad` hook that
warns to whichever suggested package has not been loaded yet, then assert
`need_pkg()` is silent and a bare `requireNamespace()` is not. Measured
contrast, order-independent, and it still fails if the suppression is
removed.

## 253. Every visual test stopped at `ggplot_build()`, so draw-time was untested

Found by the test written for §248, not by looking for it. `plot()` renders,
and `ggplot2` reports some problems only when a layer actually draws:

> `geom_line()`: Each group consists of only one observation.
> i Do you need to adjust the group aesthetic?

`autoplot.ggcpt_power()` on a single change size — `cpt_power(n = 80,
jump = 2)`, which is what a first call looks like — built without a murmur
and **rendered** that. Confirmed the asymmetry directly: `ggplot_build()`
over all fourteen classes emits zero messages; `print()` over the same
fourteen emitted one.

The advice is also wrong: the group aesthetic is right, there is simply one
point per curve. And the same one-row degeneracy made the ribbon invisible,
so the figure's own subtitle — *"Shaded band: 95% Monte Carlo interval"* —
described something that was not drawn. A single change size now gets a
`geom_linerange()` and a subtitle that says *"Vertical range"*; two or more
keep the ribbon and the line, so the `cpt_power` vdiffr snapshot
(`jump = c(0.5, 2)`) is unchanged.

The new test asserts `expect_no_message()` and `expect_no_warning()` around
`plot()` for all fourteen classes. **That is the reusable part: a plot test
that only builds is testing half of the plot.**

## 254. What this pass changes

Deletion pass: §217.1 (thirteen `plot()` methods), §226 item 1 (suppress the
engine load warning at the call site) and §244.4 items 1, 2 and 3 (`dpi =
72`, reject `dev = "svg"`, drop `fig.retina`) are shipped and are struck
from the open list. §217.2 (an `autoplot` for `ggcpt_recommendation`) and
§246.4 (the per-data-type table, a `data_type` argument, a `minseglen`
floor) are **features**, not polish, and stay on the 0.6.0 list untouched.

| § | finding | measured how |
|---|---|---|
| 248 | `plot()` failed on 8 of 14 result classes and gave `plot.data.frame()`'s pairs plot on 3 more | constructed one object per class |
| 248.1 | **`plot.ggcpt()` hard-called `autoplot.ggcpt()`, so `plot()` on a consensus result drew the wrong figure silently** — the case §217's method-table survey could not see | compared `$labels` of `plot()` and `autoplot()` |
| 249 | `dpi = 72` gives **4,430,230 bytes**, matching §244's 4.22 MB exactly; `inst/doc` 3.5 MB; figures 576 x 360 px and legible | full build from a copy of the tree, PNG headers, eyeball |
| 250 | **`cpt_detect(factor(...))` returned a populated result** — detection on the level codes, because `x` is replaced by the coercion before `validate_data()` sees it | no error, no warning, a `ggcpt` |
| 250.1 | a `!is.numeric()` guard would have silently dropped `difftime` and `Date`, which convert cleanly | `is.numeric()` over twelve input types |
| 250.2 | a logical series worked in `cpt_detect()` and was refused by `cpt_select()` | both calls |
| 251 | three empty-input calls answered with base R's message, one with no error at all | degenerate-input sweep, 9 inputs x 7 entry points |
| 252 | **§226's call-site mitigation had never shipped**: `cpt_scale_space()` still warned about `DISPLAY` on every headless machine | fresh session, `DISPLAY` unset |
| 252.1 | the suite's copy of that warning comes from `skip_if_not_installed()`, which loads the package to answer the question | warning attached to a test whose code path was already fixed |
| 253 | **`ggplot_build()` over 14 classes: 0 messages. `print()` over the same 14: 1.** Every visual test in the suite stops at build | both, on the same objects |

New actions: none opened. Five fixes shipped, three new test blocks
(`test-050-plot-methods.R`, plus the `need_pkg` and input-validation blocks
in `test-hardening.R`), and `setup.R` gains a null device so a suite that now
draws leaves no `Rplots.pdf` behind.

**What this pass adds.** Two things worth carrying forward. First, the
seventeenth instance of the §202 pattern, and the first where the earlier
claim was *incomplete rather than wrong*: §217 counted the classes with no
`plot` method and was right about all of them, and the worst case was the
class that had one. A survey of a method table cannot see a method that
dispatches to the wrong thing — only calling it can.

Second, §253 is a **test-shape** finding rather than a code finding, which
is rarer and cheaper to reuse: the suite had a plot test for every result
class and none of them drew anything. `ggplot_build()` is where a test
naturally stops because it returns an inspectable object, and that is
exactly why the draw-time half of `ggplot2`'s diagnostics had never been
exercised. One `print()` inside a `plot()` method changed that for fourteen
classes at once.

## 255. Three functions take a vector where every other tool takes the fit

Found by sweeping a **zero-changepoint** result through 27 downstream tools
(the degenerate case: `cpt_detect(rnorm(200), method = "pelt")` finds
nothing, one segment, and everything downstream must still work). Twenty-two
were clean, three errored correctly and informatively (`cpt_statistic()` and
`cpt_solution_path()` naming the engines that *do* expose one), and two
errored with `'list' object cannot be coerced to type 'integer'`.

The cause is not the zero-changepoint case at all. `cpt_metrics()`,
`cpt_metrics_annotated()` and `ggcpt_eval()` are **the only three tools in
the package whose first argument is a bare index vector** rather than the
`ggcpt` itself — every other tool (`cpt_confint()`, `cpt_influence()`,
`cpt_statistic()`, `cpt_report()`, `tidy()`, `autoplot()`) takes the fit. So
the sweep passed the fit, as any user would, and reached `as.integer()` on a
list.

`cpt_metrics_annotated()` is the worse of the two, because a `ggcpt` **is** a
list: `if (!is.list(annotations)) annotations <- list(annotations)` left it
alone, and the function then iterated over the fit's own fields — method,
data, segments — scoring each as though it were an annotator.

Guarded in `as_cp_indices()`, which names the argument and the fix
(`fit$changepoints$cp` or `tidy(fit)$cp`), and covers a `tidy()` table
(`pred$cp`) as well. Note what was deliberately *not* changed: a
`data.frame` of annotations still iterates **column-wise**, one annotator
per column, which is a sensible reading and is tested.

Accepting a `ggcpt` in these three would be the better API and is a
**feature**, so it stays on the 0.6.0 list; the error that tells the user
what to type is the polish.

## 256. What this pass changes (continued)

| § | finding | measured how |
|---|---|---|
| 255 | 27 downstream tools on a zero-changepoint fit: 22 clean, 3 correct errors, **2 base-R coercion errors** | sweep |
| 255 | **`cpt_metrics_annotated(pred, fit, n)` scored the fit's own fields as annotators** — a `ggcpt` is a list, so the promote-to-list guard let it through | reading the loop |

The zero-changepoint sweep is worth keeping as a routine: it is cheap, it
exercises the branch every metric and plot has for "nothing found", and the
defect it surfaced had nothing to do with zero changepoints — the degenerate
input was just the reason to call twenty-seven functions in a row.

## 257. Argument validation: 94 gaps found statically, 12 of them real

**Method.** For every export, classify each formal by its *deparsed* default
(so the empty-symbol marker is never bound) and grep the function body for
`validate_flag(<name>` / `validate_scalar(<name>`. Then call the flagged
ones with a bad value and see what actually happens.

| | count |
|---|---|
| logical defaults with no inline `validate_flag()` | 12 |
| numeric scalar defaults with no inline `validate_scalar()` | 82 |
| of those, silently accepted a nonsense value | **4** |

**The static sweep over-reports badly, and that is the useful part.** All 12
logical "gaps" are false positives: 10 are ggplot2's own `na.rm` and
`inherit.aes`, which belong to `layer()`, and the two that looked real —
`ggcptplot(show_line =)` and `ggecpplot(show_line =)` — *are* validated, in
a helper the grep could not see. `show_line = 1` and `show_line = "yes"`
both error correctly. So a "which functions call the validator" audit
answers a question about **call sites**, not about **behaviour**, and the
only way to the second is to make the bad call.

Most of the 82 numeric gaps are also fine, for three different reasons:
the argument goes straight to an engine that validates it (`bcp_wrapper`'s
`burnin`, `mcp_wrapper`'s `iter`); it is a ggplot2 aesthetic
(`geom_cpt_region(alpha =)`); or a downstream guard catches it with a good
message (`cpt_power(sigma = -1)` -> `` `sd` must be ... at least 0 ``,
`signal_blocks(n = -5)` -> `` `n` must be at least 100 for the blocks
signal ``, `cpt_power(rho = 1.5)` -> `` greater than -1 and less than 1 ``).

### 257.1 The claim I nearly filed, and why it was wrong

`cpt_power(location = 50)` on a 100-point series returns `power = 1.00`.
That looks like a fraction read as a fraction and silently producing a
confident number — and it is not. Reading the code:

```r
cp <- if (location > 0 && location < 1) round(location * n) else as.integer(location)
```

`location` is **deliberately dual-purpose** — a fraction in \eqn{(0,1)} or
an absolute index — and `@param location` documents exactly that: "as a
fraction of `n` in \eqn{(0,1)} or an integer position". So `location = 50`
means index 50, the power of 1.00 is correct, and the returned tibble's
`location` column reports the *realised* index. Nothing to fix.

Eighteenth instance of the §202 pattern, and the cheapest one yet to
avoid: the measurement was right, the interpretation came from not reading
the parameter's own documentation first.

### 257.2 `cpt_scenarios()` is the one where the same clamp does bite

The sibling function has no such dual reading — `@param location` says
"Change positions, as fractions of `n`" — and it clamps:

| requested `location` | table says | data's changepoint |
|---|---|---|
| 0.5 | 0.5 | 50 |
| 9 | **9** | **98** |
| 0.001 | **0.001** | **2** |
| -0.2 | **-0.2** | **2** |

The scenario table keeps the *request* while the generated series carries
the *clamp*, so `subset(scen, location == 9)` describes data whose change is
at 98% of the series. Now warned, once per call, naming each moved
scenario — and only when the clamp actually bit, which is a tighter
condition than "outside \eqn{(0,1)}" (0.001 x 100 rounds to 0 and is moved
too).

### 257.3 `ggcpt_eval(margin = -1)` drew its tolerance windows inside out

Measured on the built plot: `xmin = 101`, `xmax = 99`. `cpt_metrics()`
validates `margin` and this plot exists to agree with `cpt_metrics()`, so
the two disagreed about what a margin is. One `validate_scalar()` call.

## 258. §205.1 was documented twice and signalled nowhere

§205.1 asked for one of two things: error when a method is handed a
parameter it does not consume, **or** document per method which knob
governs. Both halves of the documentation shipped — `?cpt_monitor`'s
`@param alpha` says "for `\"edetector\"`", `@param arl0` says "for
`\"cpm\"`", and `vignettes/monitoring.Rmd` has a section titled *"`alpha`
and `arl0` are alternative parameterisations, not two knobs"* that
demonstrates `arl0 = 5000` changing nothing. The runtime said nothing.

Which leaves the user who did not read either, set `arl0` on an e-detector
to quiet it down, and is now looking at an unchanged number. So:

```
> cpt_monitor("edetector", baseline = b, arl0 = 5000)
Warning: `arl0` does not affect `method = "edetector"`, which is tuned by
`alpha`, `deltas`. See ?cpt_monitor.
```

**The mechanism matters: `missing()`, not a value comparison.** The
constructor's defaults supply all seven tuning knobs, so testing whether
`arl0 != 500` would both miss a user who sets it to 500 and stay quiet
about a default. Only an argument the caller actually named is reported,
and it survives `cpt_replay()`'s `...`. Verified across eight calls: silent
for defaults, silent for `alpha`/`deltas` on an e-detector and `arl0` on
cpm, warning for `arl0` and `mc_reps` on an e-detector (named together in
one message) and for `alpha` on cpm.

The vignette chunk that demonstrates the no-op now runs with
`warning = TRUE`, so the reader sees the package make the same point the
prose does.

Also from §205.2: `@param method` now marks `"ocd"` **multivariate only**
at the point the method is chosen, rather than leaving the requirement to
the error a univariate baseline eventually raises.

## 259. What this pass changes

| § | finding | measured how |
|---|---|---|
| 257 | 94 static validation "gaps"; **12 real defects at most, 4 silently accepted** — the 12 logical ones are all false positives, `show_line` included | static sweep, then a bad call per candidate |
| 257.1 | **`cpt_power(location = 50)` is correct and documented** — the dual fraction/index reading is in `@param location`; claim withdrawn before filing | read the code and the docs |
| 257.2 | `cpt_scenarios()` records the requested fraction while generating the clamped index: `location = 9` -> a change at 98, table still says 9 | four locations, table vs data |
| 257.3 | `ggcpt_eval(margin = -1)` built rectangles with `xmin = 101 > xmax = 99` while `cpt_metrics()` refused the same value | `ggplot_build()` |
| 258 | §205.1's documentation shipped in **two** places and the runtime signalled in none | `?cpt_monitor`, the vignette, and eight constructor calls |

New actions: none opened. Three fixes shipped plus two documentation
corrections, and one candidate defect withdrawn.

**What this pass adds.** A method note worth keeping: *a "does this function
call the validator" grep measures call sites, not behaviour, and it
over-reports by roughly eight to one here.* Of 94 static hits, 90 were
already handled — by a helper the grep could not see, by the engine
downstream, or by ggplot2. The four that mattered were only visible by
making the bad call. Static sweeps are good at generating candidates and
useless as findings.

# Part VII — the extension mechanism under hostile input

§225 swept twenty-four downstream surfaces for a **well-behaved** registered
method and found one defect. This is the other direction: what the extension
mechanism does when the third party gets it wrong. It is the right question
for 0.5.0's headline feature, because the whole point of
`cpt_register_method()` is code this package has never seen — a Python
detector through \pkg{reticulate}, a neural detector, a paper's reported
breaks — and off-by-one conventions and wrong-shaped returns are the normal
failure mode there, not the exotic one.

## 260. The contract holds, in 26 of 30 cases

**`as_ggcpt()`, 16 `cp` inputs.** The documented contract — "out-of-range,
duplicated and missing values are dropped, and the result is sorted" — is
honoured exactly: `c(90, 30)` -> `30, 90`; `c(60, 60)` -> `60`;
`c(60, NA)` -> `60`; `c(0, -5, 60, 120, 500)` on a 120-point series -> `60`;
`60.5` -> `60`; `integer(0)` and `NULL` -> zero changepoints. Segments still
tile after the drops (`starts 1,31,61,91` / `ends 30,60,90,120`), and when
every candidate is dropped the result is one segment spanning the series.

**The optional slots all police their own shape**, and this is the part
worth recording because it is easy to get wrong: `ci` with the wrong number
of rows, `ci` with one column, `extra` with a wrong-length element, an
unnamed `extra`, a one-column `regions`, an `index` of the wrong length, an
empty or `NA` `method`, an unknown `change_in` — nine distinct errors, each
naming the argument.

### 260.1 `ci` and `extra` survive the drop correctly, which is not obvious

`ci` and `extra` are validated against the **supplied** `cp` vector, and the
dropping happens afterwards inside `ggcpt_build()`. So they could easily end
up describing a different changepoint. Measured on four cases:

| supplied | survives | `score` kept | correct? |
|---|---|---|---|
| `cp = c(0, 60)`, `score = c(111, 222)` | 60 | **222** | yes |
| `cp = c(60, 500)`, `score = c(111, 222)` | 60 | **111** | yes |
| `cp = c(60, 60)`, `score = c(111, 222)` | 60 | 111 | yes |
| `cp = c(90, 30)`, `score = c(999, 111)` | 30, 90 | **111, 999** | yes |

Filtered and reordered in step with `cp` in all four. A genuine positive
result, and now pinned by a test, because a future change to the drop order
would break it silently.

**Dispatch is equally solid on return shape.** Of eighteen registered
functions, the six returning something that is not changepoints — `NULL`, a
character vector, a factor, a logical mask, a list, a data frame — all fail
with "must return a ggcpt object or a numeric vector of changepoint
locations". An error inside the function propagates unchanged; a warning
passes through and the result still builds.

## 261. The four that were wrong

### 261.1 A returned `ggcpt` was taken entirely on trust

```r
out <- entry$fn(x, ...)
if (is_ggcpt(out)) {
  out$method <- entry$method
  out$registered <- TRUE
  return(out)          # <- no check that `out` is about `x`
}
```

A registered function returning `as_ggcpt(20, rnorm(40))` made
`cpt_detect(x, method = "probe")` on a **160**-point series return a result
whose `$data` had **40** rows. No error, no warning. Downstream: `augment()`
gives 40 rows for a 160-point input, the plot shows a series the user never
passed, and `n` is wrong for every metric.

**The claim in the code and the documentation is that this cannot happen.**
The comment above the function says a registration "may return a finished
ggcpt ... or a bare vector of indices; either way the result goes through
the same contract checks as a built-in wrapper", and
`?cpt_register_method` says the shape is validated "through `as_ggcpt()`,
which runs the same contract checks as every built-in wrapper". Only the
bare-vector branch ever reached `as_ggcpt()`. Nineteenth instance of the
§202 pattern, and the first where the false claim was a *comment asserting
a check that was not there*.

Fixed with the one invariant that matters: observations in equals
observations out. Verified that a multivariate return still passes — the
count is observations, not coordinates (120 rows, 2 coordinates, accepted).

### 261.2 `suppressWarnings(as.integer(cp))` turned garbage into "none found"

`as_ggcpt()` read `cp` through `suppressWarnings(as.integer(cp))`. So:

| `cp` | before | why |
|---|---|---|
| `c("a", "b")` | **0 changepoints, no warning** | NAs invented, then dropped as "missing" |
| `c("60", "x")` | 1 changepoint, no warning | half read, half discarded |
| `factor(c("60", "90"))` | **changepoints at 1 and 2** | `as.integer()` on a factor gives level codes |
| `seq_along(x) == 80` | 1 changepoint at index 1 | a mask collapsed to 0/1 |

The factor case is round-one's series defect (§250) in a second argument,
and the mechanism is identical. The distinction the fix needs is between an
NA that **arrived** as an NA — documented, dropped — and one the coercion
**invented**, which is a wrong-type input. Character that converts cleanly
still works, so nothing documented was withdrawn.

### 261.3 A wrong-length `fitted` was the one slot that stayed quiet

`ggcpt_build()` keeps `fitted` only `if (length(fitted) == n)`, silently.
Passing a 10-long fitted signal for a 120-point series therefore produced a
result with no fitted signal at all, and then
`autoplot(show_fit = TRUE)` reported

> `show_fit = TRUE` but this result carries no fitted signal

about a signal the caller had supplied. Every sibling slot errors on a
length mismatch; this one now does too, in `as_ggcpt()` where the user
supplies it rather than in the shared builder.

**Checked before assuming the builder's silent guard was the bug:** all
eight installable engines that declare `fitted` in the registry
(`smuce`, `hsmuce`, `cpop`, `bcp`, `beast`, `decafs`, `segmented`, `bfast`)
return a full-length signal — 300 of 300 in every case. So the guard never
bites for a built-in, the registry's `fitted` flag is accurate, and
`as_ggcpt()` was the only place it could fire. Now pinned by a test, since a
wrapper that started returning a short signal would advertise the capability
in `cpt_methods()` and quietly not have it.

## 262. What this pass changes

| § | finding | measured how |
|---|---|---|
| 260 | 30 hostile inputs across `as_ggcpt()` and dispatch: **26 handled correctly** | one call per case |
| 260.1 | `ci`/`extra` are filtered and reordered in step with `cp` in all four drop/sort cases — validated against the pre-drop vector and still correct | supplied vs surviving values |
| 261.1 | **a returned `ggcpt` was never checked against `x`**: 160 observations in, a 40-row result out, silently — while the comment and the help page both claimed it went through `as_ggcpt()` | `nrow(fit$data)` vs `length(x)` |
| 261.2 | `suppressWarnings(as.integer(cp))`: `c("a","b")` -> 0 changepoints; `factor(c("60","90"))` -> **changepoints at 1 and 2** | four `cp` types |
| 261.3 | a wrong-length `fitted` was dropped silently, then reported downstream as "carries no fitted signal" | `length(fit$fitted)` = 0 |
| 261.3 | all 8 installable engines declaring `fitted` return a full-length signal | 300/300 each |

New actions: none opened. Three fixes shipped.

**What this pass adds.** The reusable idea is *auditing a claim rather than a
function*. §261.1 was not found by reading `run_registered_method()` looking
for bugs; it was found by taking the sentence "either way the result goes
through the same contract checks" and constructing the input that would
prove it. A comment asserting a check is a claim, and claims in this
codebase have a poor record (§202, nineteen instances now) — so the ones
that assert an invariant are worth testing directly, and the ones that turn
out to be true are worth a test so they stay true.

## 263. The contract prose is accurate, in 45 engines and 0 violations

`vignettes/extending.Rmd` ends with a section titled *"What the contract
is"* that enumerates five guarantees and claims "`as_ggcpt()` enforces all
of it". Having just found one such claim false (§261.1), the rest were worth
checking. Every wired method, structure only, n = 160:

| claim | result |
|---|---|
| `$changepoints` has `cp` (integer, sorted, de-duplicated, in `1..n-1`) and `cp_value` | holds, 45/45 |
| `$segments` has `seg_id`, `start`, `end`, `n`, `param_estimate`, and one more row than `$changepoints` | holds, 45/45 |
| `$segments` spans `1..n` | holds, 45/45 |
| `$data` has `index` equal to `1..n` and `value` | holds, 45/45 |
| `$method`, `$change_in`, `$cp_convention` are length one | holds, 45/45 |

**Zero violations.** The five engines that did not run all refused for a
stated reason: `mcp` needs JAGS; `pilliat` refuses exactly two coordinates
(the HDCD 1.1 guard from the 0.5.0 audit); `hdreg` requires `response`;
`fabisearch` refuses negative input. Four correct refusals — and one that
was not.

### 263.1 Enforcement moved into the shared expectation, not a new test

The suite already had **two** `expect_ggcpt_contract()` helpers, one per
file, and they had drifted: `test-050-engines.R`'s checked the integer type,
sortedness, de-duplication and that the segment lengths sum to `n`;
`test-040-wrappers.R`'s, with **17 call sites**, checked none of those.
Neither checked the `$segments` column set, `$data$index == 1..n`, or the
length-one metadata.

Both are now the same body, covering every clause of the vignette's list.
That upgrades 22 existing call sites at no runtime cost, which is a better
outcome than a 45-engine sweep the suite would be too slow to run: the
sweep found the facts, the helper keeps them true.

### 263.2 `fcov` on a two-column matrix: `subscript out of bounds`

`fchange_run()` guards `ncol(X) < 2` with a good message — "needs
functional observations: one row per time point and one column per grid
location" — and then hands `t(X)` to `fChange::fchange()`. Two columns
clears the guard and gives the basis expansion a two-point grid, so it dies
inside upstream with base R's subscript error. `fmean` shares the same
runner, so both engines had it.

**The fix deliberately does not invent a minimum.** A `ncol(X) < 4` guard
would be a number I could not justify — the true requirement depends on the
target, the statistic and the critical-value method, and measuring it took
long enough at p = 8 that the probe was killed rather than left running.
Instead the upstream error is caught and re-raised with the shape as a fact
and the grid as a hint:

> `fcov` failed on 120 time point(s) x 2 grid point(s). fChange reported:
> subscript out of bounds. A functional observation is a curve sampled on a
> grid, so `x` wants one column per grid location; a handful of columns is
> usually too coarse for the basis expansion.

The upstream text is passed through verbatim rather than replaced, because a
coarse grid is the usual cause and not the only one — the §202 lesson
applied to an error message instead of a claim.

## 264. What this pass changes (continued)

| § | finding | measured how |
|---|---|---|
| 263 | the vignette's five-clause contract holds in **45 of 45** engines that ran | structure sweep over the registry |
| 263.1 | the two `expect_ggcpt_contract()` helpers had drifted; the one with 17 call sites checked neither cp's type, order, uniqueness nor the segment lengths | read both |
| 263.2 | `fcov`/`fmean` on a 2-column matrix: `subscript out of bounds` from inside fChange | one call per coordinate count |
| 264.1 | **no dead code**: all 135 non-method internal functions are called somewhere in `R/` | fixed-string hit count over `R/`, S3 methods excluded |

New actions: none opened. One fix shipped, one shared expectation
strengthened across 22 call sites.

### 264.1 A note on how the dead-code scan had to be corrected twice

First attempt regex-escaped the function names and died on `[.ggcpt_batch`.
Second attempt, with fixed matching, reported **56 dead internals** — of
which every one was an S3 method (`autoplot.ggcpt_batch`,
`print.cpt_label_error`, `[.cpt_labels`), reached by dispatch and therefore
never named in the source. Excluding the `S3method()` registrations from
`NAMESPACE` leaves 135 genuine internals and **zero** uncalled. Worth
recording so the scan is not repeated: a name-reference count is the wrong
instrument for anything dispatched.

# Part VIII — one mechanism, eleven doorways

Rounds one and three each fixed an instance of `as.integer()` on a factor
returning level codes: the **series** (§250) and `as_ggcpt()`'s **cp**
(§261.2). Finding the same mechanism twice in three passes is the signal
that it is not two bugs but one, so this pass went looking for the rest of
the doorways instead of waiting for the third.

## 265. The index was the third, and it broke a case that character input handles

`check_index_usable()` converts with `as.numeric(idx)` and treats a failed
conversion as "character labels, cannot be ordered, still fine for an axis".
A factor converts *successfully* — to its codes — so it took the numeric
branch and was order-checked on alphabetical positions:

| index | codes | verdict before |
|---|---|---|
| `month.abb` (character) | — | **accepted** |
| `factor(month.abb)` | `5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3` | **refused**: "`index` must be non-decreasing" |
| `factor(month.abb, levels = month.abb)` | `1..12` | accepted |

The same twelve labels, accepted or refused depending on a level ordering
that has nothing to do with whether the index increases in time — and the
error blames the user's data for it. An unordered factor is now treated as
the label vector it is. **An `ordered` factor keeps the checks**, because
there the codes genuinely carry the order: verified accepted with
increasing codes and refused with scrambled ones.

## 266. Then eight more, and this time the answer was a wrong number

The other two instances produced an error or an empty result. These produce
a *measurement*:

| call | before |
|---|---|
| `cpt_metrics(factor(c("100","150")), c(100,150), n = 200)` | `n_pred = 2`, **`recall = 0`** |
| `cpt_metrics(c(100,150), factor(c("100","150")), n = 200)` | `n_truth = 2`, **`recall = 0`** |
| `cpt_metrics_annotated(c(100,150), factor(...), n = 200)` | **`f1 = 0`** |
| `cpt_benchmark(list(a = list(series = x, annotations = list(factor("100")))))` | **`covering = 0.5`** |
| `ggcpt_eval(c(100,150), factor(...), x)` | a plot with tolerance windows at 1 and 2 |
| `cpt_delay(mon, truth = factor("100"))` | `n_changes = 1`, truth at index 1 |
| `as_cpt_labels(factor(c("100","150")), n = 200)` | `starts = 1, 1, 8` |
| `cpt_labels(factor(...), factor(...))` | `starts = 1, 2` |
| `cpt_simulate(200, changepoints = factor("100"))` | a series with its change at 2 |

**The predictions were exactly right and the recall was reported as zero.**
That is the worst failure mode available to a package whose job is to
measure detection accuracy, and none of it warned.

One entry point already refused: `cpt_annotate_events()` looks for a
location *column* and rejects a factor one ("Could not find a location
column in `events`"), because it tests the column rather than coercing it.

### 266.1 The fix is one helper, and it replaced two of my own

Rounds one and three each added a local guard — `as_cp_input()` in
`R/as-ggcpt.R` and `as_cp_indices()` in `R/metrics.R` — with overlapping
rejections and different sorting behaviour. That is the same drift §263.1
found in the two test helpers, one round after criticising it, so both are
now a single `as_cp_locations(x, arg, sort)` beside the other validators,
refusing: a `ggcpt`, a data frame, a **factor**, a **logical mask**, and an
NA the coercion *invented* (as opposed to one that arrived as NA, which the
documented drop rules cover). Applied at **thirteen call sites** across
ten entry points -- `cpt_metrics()` and `ggcpt_eval()` take two each --
plus one more passed to `lapply()` rather than called directly, in
`cpt_metrics_annotated()`. (Corrected in §305: this said "eleven".)

Verified in both directions: ten factor calls refused, each naming the
caller's own argument (`pred`, `truth`, `annotations`, `start`,
`changepoints`, `cp`), and thirteen honest calls unchanged — including
`cpt_metrics(c("100", "150"), ...)`, since character that converts cleanly
was accepted before and still is.

## 267. What this pass changes

| § | finding | measured how |
|---|---|---|
| 265 | `factor(month.abb)` as an index was **refused** while the identical character labels were accepted | three index forms, codes printed |
| 266 | **eight entry points read a factor's level codes as locations**, and four of them returned a metric: recall 0 for exact predictions, f1 0, covering 0.5 | one factor call per entry point |
| 266.1 | the two local guards from rounds one and three had already drifted apart; now one helper at thirteen call sites | read both, then merged |
| — | 200 of 205 `stop()` calls and 25 of 25 `warning()` calls use `call. = FALSE`; the five exceptions are deliberate `stop(e)` re-raises of an upstream condition | paren-balanced scan of `R/` |
| — | the rest of the index surface is sound: unsorted, decreasing, NA, wrong-length and length-1 indices all refused with clear messages; Date and POSIXct get date/datetime scales; irregular spacing warns; character labels give a discrete axis; `cp_index` and `index_value` are carried through | 15 index forms x tidy/augment/autoplot |

New actions: none opened. Two fixes shipped, two duplicate helpers merged.

**What this pass adds.** The lesson is about *when to generalise*. §250 and
§261.2 were filed as two separate defects in two separate arguments, and
both fixes were local. The third sighting is what made the shape visible:
the question is not "is this argument coerced safely" but "**where does
user input get coerced to a number at all**", and asking it that way found
nine more doorways in one sweep — four of them returning a fabricated
measurement rather than an error. A repeated mechanism deserves one guard
and a list of call sites, not a third local patch.

# Part IX — two clean audits and one fix

The yield is falling, which is the point of the exercise. Two of this
pass's three sweeps found nothing, and recording that is worth as much as a
fix: it stops a later pass from spending the same time.

## 268. Every `seed` argument reproduces

**Method.** 38 exports carry a `seed` formal. For each, call it twice with
`seed = 1` and compare the result; the sharp question is not whether the
seed *changes* anything (many wrappers are deterministic and take `seed`
only for uniformity) but whether the **same seed reproduces**. This is the
§205.1 shape — an argument accepted and then ignored — asked of the one
argument whose failure would be invisible and would destroy every benchmark
in the package.

35 ran (two need input this probe did not build, `mcp` needs JAGS).
**All 35 reproduce exactly**, with one qualification that took two rounds of
measurement to state correctly:

| function | naive verdict | after excluding timing |
|---|---|---|
| `cpt_benchmark` | not reproducible | reproducible; only `runtime` differs |
| `cpt_influence` | not reproducible | reproducible; only `$original$runtime` differs |
| `cpt_batch` | not reproducible | reproducible; only each `$result[[i]]$runtime` differs |

**The first reading of my own probe was wrong**, and in the direction that
would have produced three phantom bug reports. `identical()` on two runs of
a fully deterministic computation is `FALSE` because the result records how
long it took. Field-by-field diffing was what settled it: for `cpt_batch`
the differing values were `0.00999` against `0.00499` seconds and nothing
else. Twentieth instance of the §202 pattern, this time in the
instrument rather than in the code.

So: no defect, and a note for whoever writes a reproducibility test —
compare the parts, not the object.

## 269. `stop()` and `warning()` provenance is already right

200 of 205 `stop()` calls and **25 of 25** `warning()` calls pass
`call. = FALSE`, so a user sees the message rather than the name of an
internal helper. The five exceptions are all `stop(e)` re-raising a caught
upstream condition — `wbs`, `not` and `sn` re-throwing anything that is not
their known "constant input" or "no changepoints" case — where preserving
the upstream call is the correct behaviour. Nothing to change.

(Method note: a line-based grep is not enough, because these calls span
lines; the scan balanced parentheses from each `stop(`/`warning(` to find
the whole call.)

## 270. `cpt_report(file = )` was the one gap, and one case was silent

Everything else about `cpt_report()` and `cpt_gt()` is guarded: `object`
must be a `ggcpt`, `stability` must be a `cpt_stability()` result, `events`
a `cpt_annotate_events()` result, `confint` and `session` are flags,
`digits` is a non-negative scalar, and the zero-changepoint report is
honest ("Changepoints found: 0", an empty changepoints table, one segment
spanning the series, no confidence-interval section invented).

`file` went straight to `writeLines()`:

| `file` | before |
|---|---|
| path in a missing directory | `cannot open the connection` |
| a directory | `cannot open the connection` |
| `NA` | `'con' is not a connection` |
| `c(a, b)` | `invalid 'description' argument` |
| `""` | **the report on the console, no file, a warning** |

The last is the one that matters: no error, no file, and a returned report,
so the caller believes the write succeeded. All five are now checked before
any of the report is built, so a bad path fails immediately rather than
after the work.

**And the check is placed after the `format = "gt"` early return**, because
`@param file` says it is ignored for that format — validating it there
would refuse a call the help page calls fine. Verified both ways.

## 271. What this pass changes

| § | finding | measured how |
|---|---|---|
| 268 | **all 35 runnable `seed`-taking functions reproduce**; the three apparent failures differ only in a recorded `runtime` | two identical-seed calls each, then field-by-field |
| 269 | `call. = FALSE` on 200/205 `stop()` and 25/25 `warning()`; the five exceptions are deliberate re-raises | paren-balanced scan |
| 270 | `cpt_report(file = "")` wrote nothing, warned, and returned the report | five bad paths |

New actions: none opened. One fix shipped, two audits closed as clean.

# Part X — auditing the `@return` prose

§261.1 found a false claim in a code comment and §263 found five true ones
in a vignette. The `@return` sections are the same kind of surface: prose
that enumerates columns, that `R CMD check` cannot verify, and that a reader
takes literally. Twenty of them name three or more columns.

## 272. Every documented column exists — and four undocumented ones did not

**Method.** Extract each topic's `\value{}` text from `man/*.Rd`, run the
function, and compare in both directions: documented-but-absent, and
returned-but-unnamed. Forty-one functions ran, none skipped.

**Documented but absent: zero.** Four looked like misses and were all
precisely worded on inspection — `cpt_confint()`'s
`cp_index`/`ci_lower_index`/`ci_upper_index` and `cpt_regions()`'s
`start_index`/`end_index` are each qualified "when the result carries a
time index"; `cpt_power()`'s `tolerance` appears only inside the
*description* of `power` ("within `tolerance`"), not as a claimed column;
and `cpt_label_error()`'s `errors` is explicitly "carrying the totals in an
`errors` attribute". The prose is more careful than a keyword scan.

**Returned but unnamed: four.**

| topic | column | what it is |
|---|---|---|
| `cpt_scale_space()` | `detected` | the engine reported a changepoint there at that bandwidth |
| `cpt_regions()` | `value` | the region's statistic, carried through from `nsp_wrapper()` |
| `cpt_label_error()` | `series` | the label set's series identifier |
| `cpt_benchmark()` | `n_annotators` | how many ground-truth sets the dataset supplied |

`detected` is the one that matters, because the tibble also has
`significant` and the two are **not the same**: `significant` is "the
statistic at this location and bandwidth exceeds the threshold", `detected`
is "the engine accepted a changepoint here". A location can clear the
threshold without surviving the engine's own pruning, and a reader of
`?cpt_scale_space` had no way to know the second column existed, let alone
which one to trust.

All four documented. `cpt_regions()`'s wording now says extra engine
columns are carried through rather than enumerating an engine-specific
name, because that is what the code does — it puts
`start`/`end`/`length`/`*_index` first and keeps everything else.

### 272.1 The one remaining "gap" is not one

`cpt_benchmark()` still returns `covering` and `f1` without naming them,
and correctly: they are whatever the `metrics` argument asked for, and the
`\value` says "the requested metrics". Enumerating them would make the
documentation wrong for `metrics = "f1"`.

### 272.2 A guard, because prose rots and code does not

`test-doc-coverage.R` gains a test that reads each topic's `\value` from the
Rd database and asserts that every column of the returned tibble appears in
it, for twelve result tibbles. It is the cheapest possible defence against
the same drift: a new column added to a result now fails a test rather than
quietly going undocumented.

## 273. What this pass changes

| § | finding | measured how |
|---|---|---|
| 272 | **no documented return column is missing** from any of 41 functions; the four apparent misses are each correctly qualified in the prose | Rd `\value` vs the actual result, both directions |
| 272 | **four columns were returned and never documented**, `cpt_scale_space()`'s `detected` most consequentially -- it sits beside `significant` and means something different | same |
| 272.1 | `cpt_benchmark()`'s unnamed `covering`/`f1` are correct: they are the requested metrics | read the wording |

New actions: none opened. Four documentation fixes and one regression guard.

**What this pass adds.** The two-directional check is the transferable part.
Asking "is everything documented actually there" found nothing, because that
is the direction a maintainer naturally proofreads. Asking "is everything
there actually documented" found four, because nothing prompts you to
re-read the `@return` when you add a column. The second direction is the one
worth automating, and it now is.

# Part XI — the print surface, and the citations

Two surfaces nothing had swept: what the nineteen `print()` methods actually
put on screen, and whether every method the package wires has a citation to
give.

## 274. Citations are complete

All **50** wired methods return a citation from `cpt_cite()` that is
non-empty, longer than forty characters and carries a parenthesised
four-digit year; none falls back to "no citation given". `inst/CITATION`
parses under `readCitationFile()` with one entry. Nothing to change.

## 275. Twenty-seven print cases, no errors and one real defect

**Method.** Print every result class twice where a degenerate form exists —
a fit with zero changepoints, a monitor that has been fed nothing, a
consensus that agreed on nothing, an events object with no matches — and
scan the captured output for `NA`, `NULL`, `character(0)`, `NaN` and `Inf`.

**Zero errors, zero warnings, zero messages across all 27.** Four flagged
spots, three of them honest:

- `cpt_influence()` on a zero-changepoint fit shows `max_shift = NA`. There
  is no changepoint to shift, so the column has nothing to report and says
  so; `param_shift` is populated in the same rows, which is the right
  distinction.
- `cpt_labels()`/`cpt_label_error()` show `<NA>` under `series`. That is
  the documented default for a single unnamed series — and the column
  itself was one of §272's four undocumented ones, now named.
- `cpt_label_error_curve()` prints `Target log-penalty interval: (-Inf, 0)`
  on a coarse grid. An unbounded end is the honest report when the grid
  never reached the error it is looking for, and the notation says so.

## 276. `cpt_annotate_events()` gave the same column two different types

The fourth was real. `$matched` and `$unexplained` were built with

```r
cp_index = if (is.null(idx)) NA else idx[m$pred[i]]
```

so the column **always existed** and, on a fit with no time index, was
filled with a bare logical `NA`:

| fit | `cp_index` |
|---|---|
| indexed by `Date` | `Date`, `2020-04-29` |
| no index | **`logical`, `NA`** |

Two problems in one line. The type of a column depended on the input, so
binding an indexed and an unindexed result puts a `Date` and a `logical` in
the same place. And the package's convention everywhere else is that an
index column is **absent** unless there is an index — `attach_index()` adds
`cp_index` to `$changepoints` only then, `cpt_confint()` adds its three
`*_index` columns only then and documents exactly that, and three call
sites (`R/batch.R`, `R/accessibility.R`, `R/cpt_gt()`) test
`"cp_index" %in% names(x)`. Against `cpt_annotate_events()` that test was
useless, and the user got an all-`NA` column with no explanation.

Now conditional, and placed next to `cp` as it is on a `ggcpt`. Verified in
four configurations: unindexed (absent from both tables), indexed
(`Date`, second column), zero-match indexed (present, zero rows, type
preserved), and `print()`/`autoplot()` working in each.

`event_value` and `cp_index` are also now in the `\value` — §272's sweep
covered top-level tibbles and missed these two, because they live in a
nested table.

## 277. What this pass changes

| § | finding | measured how |
|---|---|---|
| 274 | all 50 wired methods have a dated, non-trivial citation; `inst/CITATION` parses | `cpt_cite()` per method |
| 275 | **27 print cases: no errors, no warnings, no messages**; three flagged `NA`s are each the honest report | captured output, scanned |
| 276 | **`cpt_annotate_events()` typed `cp_index` as `logical` on an unindexed fit and `Date` on an indexed one**, and created it either way | both fits, `class()` of the column |
| 276 | two more undocumented columns, in a nested table §272's sweep did not reach | Rd `\value` vs `names()` |

New actions: none opened. One fix and two documentation additions.

**What this pass adds.** §272's guard checks top-level columns, and this
found its blind spot one pass later: `cpt_annotate_events()` returns a
*list of* tibbles, so the columns that matter are one level down and the
sweep never saw them. The print sweep is what surfaced it — not because
printing is where the bug lived, but because printing is the one operation
that touches every column of every table a result contains. It is a cheap
way to reach nested structure that a names-based audit walks past.

# Part XII — following the blind spot down a level

§276 found a defect in a nested table by accident, through the print sweep.
This pass looked for the same shape deliberately: every table inside every
result class, built twice — once from an indexed fit and once from an
unindexed one — comparing column sets, column classes, and all-NA columns.

## 278. Eighteen nested tables, and the convention now holds

| result class | table | verdict |
|---|---|---|
| `ggcpt`, `ggcpt_consensus` | `changepoints`, `data` | `cp_index`/`index_value` **index-only**, correct |
| `ggcpt_events` | `matched`, `unexplained` | `cp_index` index-only — §276's fix, confirmed |
| `ggcpt_selection` | `criterion_table` | identical both ways |
| `ggcpt_stability` | `frequency` | identical both ways |
| `ggcpt_influence` | `influence` | identical both ways |
| `ggcpt_sensitivity` | `grid` | identical; `error` is all-NA in both, which is the success case |
| `ggcpt_delay` | `per_change`, `false_alarms` | identical both ways |
| `ggcpt_batch`, `ggcpt_label_curve` | (self) | identical both ways |

No type instability anywhere, and no column that appears on one path and
not the other except the index columns that are meant to. The `error`
all-NA column is the same convention `cpt_benchmark()` uses — NA means the
cell did not fail — and its own test asserts exactly that.

## 279. But one plot was still reading positions

The tables were clean, so the sweep moved to what is drawn from them.
Thirteen files define an `autoplot` method or a `ggcpt_*` plot function, and
the package has shared helpers — `plot_index()` and `plot_index_label()` —
that return the time index when there is one and positions when there is
not. Which plots use them, and which are right not to:

| plot | x axis | uses the helpers |
|---|---|---|
| `autoplot(ggcpt)`, influence, events, consensus, batch, statistic, scale space, solution path | series position | yes |
| benchmark (method x dataset), path (penalty), power (jump size), selection (K), label curve (penalty), monitor, delay | not time | not applicable |
| **`autoplot(ggcpt_stability)`** | **series position** | **no** |

Measured on the same dated series: `autoplot(fit)`, `ggcpt_statistic()`,
`ggcpt_scale_space()`, `ggcpt_solution_path()` and the influence and events
plots all render a `ScaleContinuousDate`; `autoplot(cpt_stability(...))`
rendered a `ScaleContinuousPosition`, with its changepoint rules at 120
rather than at 2020-04-29. A user looking at the fit and its stability
side by side saw the same series on two different axes.

`cpt_stability()` has no `index` formal of its own, but it passes `...` to
`cpt_detect()`, so `$original` carries the index — the plot simply never
asked. Now it goes through the helpers like the other eight.

**And the vdiffr snapshot is untouched**, which is the point of using the
helpers rather than a special case: `plot_index()` returns positions when
there is no index, the snapshot is built from an unindexed fit, and the
rendered SVG is byte-identical. Verified both ways — positions and a vline
at 120 without an index, dates and a vline at 2020-04-29 with one.

### 279.1 Two non-findings worth recording

`cpt_crops()` and `cpt_stability()` both refuse a `ggcpt` — and their
`@param x` both say "a numeric vector", so that is documented behaviour,
not the §S33 defect. (`cpt_stability()`'s refusal now reads "not ggcpt"
rather than the generic message, a side effect of §250's guard.) Accepting
a fit there would be new capability, so it stays off this list.

## 280. What this pass changes

| § | finding | measured how |
|---|---|---|
| 278 | **18 nested tables, no type instability and no stray index column**; §276's fix confirmed from the other direction | each result built indexed and unindexed, columns and classes compared |
| 279 | **`autoplot(ggcpt_stability)` was the only position-axis plot ignoring the index**: dates on eight plots, positions on the ninth | `panel_scales_x` class on the same dated series |
| 279 | the vdiffr snapshot is byte-identical, because the shared helper degrades to positions | the visual suite |

New actions: none opened. One fix.

**What this pass adds.** The generalisation worked, but not where it was
aimed. Sweeping the nested *tables* found nothing — §276 had already been
the only instance — and the defect was one level further out, in the code
that *reads* those tables to draw. The lesson is that "same mechanism,
other places" needs to include the consumers, not just the producers: the
tables all carried the index correctly and one plot threw it away.

# Part XIII — three clean sweeps, then §172.1 finally measured

## 281. The composable geoms hold up, on both axis types

**Method.** Seven layers x two x-axis types x standalone or layered onto
`autoplot()`. `geom_changepoint()`, `stat_changepoint()`,
`geom_cpt_segment()`, `geom_cpt_region()`, `geom_cpt_label()`,
`geom_cpt_event()` and `geom_cpt_ci()` all build on a numeric axis and on a
`Date` axis, and all three tested compositions with `autoplot()` build.
`geom_cpt_event()` with no mapping raises its own message
("needs a mapping with at least `xintercept`"); `geom_cpt_region()` with
only `xmax` gets ggplot2's, which names the missing aesthetics.

One non-finding worth recording. `geom_cpt_region(aes(xmin = 90, xmax = 110))`
with no `data` warns "All aesthetics have length 1, but the data has 200
rows" — because with `inherit.aes = FALSE` and no `data` the layer still
inherits the plot's. That is ggplot2's warning about a call the help page
does not use: `@param data` says "A data frame of regions, e.g.
`cpt_regions()` output" and the example passes one. Defaulting `data` to a
one-row frame would silence it and **break** the legitimate
`ggplot(regions, aes(...)) + geom_cpt_region(aes(xmin = xmin, xmax = xmax))`
form, which inherits a multi-row frame on purpose. Left alone.

## 282. Coverage is 129 of 130

Every export except `cpt_load_tcpd()` is called somewhere in `tests/`, and
that one is the network-gated download the suite skips by design. Exactly
one example is `interactive()`-gated (`ggcpt_interactive()`), and that
function *is* called in the suite — so the §S34 shape ("the engine with the
least local coverage had the least remote coverage too") does not repeat
anywhere.

## 283. §172.1's three-point series, measured across every engine

§172.1 recorded that `pelt` "returned a changepoint after every
observation" on a three-point series and left a `minseglen` floor as an open
item. Measured across all univariate engines:

| n | engines returning a changepoint after **every** observation |
|---|---|
| 3 | `pelt`, `fpop`, `wbs2`, `tguh`, `smuce`, `decafs`, `nsp` — **seven** |
| 5 | `wbs2`, `decafs`, `nsp` — three |
| 10 | none |

So it is not one engine, and the threshold is **engine-specific** — which
is exactly why raising `validate_data()`'s minimum of three would be the
wrong fix: it would refuse `amoc`, which returns a sensible single
changepoint on the same three points. §246.4's `minseglen` floor is a
feature; the polish is to stop returning `k = n - 1` without comment, so
`ggcpt_build()` — the one funnel every engine and `as_ggcpt()` passes
through — now says when every segment is one point long.

### 283.1 And the warning immediately caught a case whose cause is different

The first full-suite run with the check produced one new warning, from a
test that had nothing to do with short series:

> `fpop` put a changepoint after every observation: **199 changepoint(s) on
> 200 observation(s)** ... the series is too short for this engine.

`penalty = "None"` resolves to 0 for the numeric-penalty engines, and with
no penalty term **one segment per observation is the correct unpenalised
optimum** at any length. The detection was right and the attributed cause
was wrong — twenty-first instance of the §202 pattern, and the first I
introduced myself in the same pass that found the defect.

The descriptor separates the two cleanly (`value = 0` for the zero-penalty
case, `NA` or positive otherwise), so the message now branches:

- penalty 0: "With a penalty of 0 that is the unpenalised optimum, not a
  segmentation -- give `penalty` a positive value."
- otherwise: "That is a failure to segment rather than a segmentation --
  the series is too short for this engine."

The C10 test now **asserts** that warning rather than letting it leak into
the suite report, which also pins the `penalty = "None"` behaviour: 199
changepoints on 200 observations, by design, with an explanation.

## 284. What this pass changes

| § | finding | measured how |
|---|---|---|
| 281 | all seven geoms build on numeric and Date axes, standalone and layered; the one warning is ggplot2's, on a call form the docs do not use | 24 layer builds |
| 282 | 129 of 130 exports are exercised in `tests/`; the exception is the network download | name scan of the suite |
| 283 | **seven engines return a changepoint after every observation at n = 3, three at n = 5** — engine-specific, so a blanket minimum would be wrong | every univariate engine at three lengths |
| 283.1 | **the new check's first catch was a zero-penalty fit on 200 observations**, where the same result is correct and the message's cause was not | full suite |

New actions: none opened. One fix, and one message corrected before release.

**What this pass adds.** A check that reports a *result* rather than an
input found, on its first run, a case its own explanation got wrong. Worth
generalising: a diagnostic that names a cause is itself a claim, and it
needs the same treatment as any other claim in this document — state the
observation, and branch on what the code actually knows rather than on the
situation you had in mind when you wrote it.

# Part XIV — the prose and the DESCRIPTION, checked against the package

A read-only pass, run while the pushed tree was being validated by CI, on
the two surfaces where a claim can rot without any test noticing: the
sentences in `README.md` and the vignettes, and the dependency lists in
`DESCRIPTION`. The 0.5.0 audit found a stale planned-engine count and a
misattributed ARL bound in exactly this surface (S30), so it is worth
re-checking rather than assuming.

## 285. Every name in the prose exists, and every count is right

**127 package-looking names** — `cpt_*`, `ggcpt_*`, `*_wrapper`, the geoms,
the scales, the signal generators — appear across `README.md`, `README.Rmd`
and the eight vignettes. **All 127 are exported.** No stale reference to a
function that was renamed or removed.

The countable claims, against the live package:

| claim | where | live value |
|---|---|---|
| "50 methods" | `README.md`, `introduction.Rmd` | 50 rows with `status == "available"` |
| "50 wired" | `comparison.Rmd` | same |
| "fifty detectors" | `extending.Rmd` | same |
| "fifty methods" | `DESCRIPTION` | same |
| "five canonical test signals" | `introduction.Rmd` | `signal_blocks`, `signal_fms`, `signal_teeth`, `signal_stairs`, `signal_mix` |
| "six families ... plus two concerns" | `introduction.Rmd` | all eight sections present and named as listed |

### 285.1 The one claim that needed running, not reading

Four documents tell the reader to use `subset(cpt_methods(), status ==
"registered")`. Called on a clean session, `cpt_methods()` returns only
`available` (50) and `planned` (5) — no `registered` row exists, so the
documented idiom cannot be verified by reading the table. Registering a
method and re-calling it:

```
status values: available=50  planned=5  registered=1
the registered row: proseprobe | user | registered | NA
subset(cpt_methods(), status == "registered")$method -> proseprobe
```

The claim holds exactly. Worth recording because a *reader* of the live
table would conclude the documentation was wrong.

## 286. `DESCRIPTION` is tight in both directions

| direction | result |
|---|---|
| all **56** `Suggests` referenced somewhere in `R/`, `tests/` or `vignettes/` | yes, 0 unused |
| every `pkg::` call in `R/` declared in `Imports` or `Suggests` | yes, 0 undeclared |

Neither direction is checked by `R CMD check` — an unused `Suggests` entry
passes silently and costs every checking machine an install — so this is
worth the one command it takes.

## 287. What this pass changes

Nothing in the package: three audits, no findings.

| § | audit | result |
|---|---|---|
| 285 | 127 package names in the prose | all exported |
| 285 | six countable claims in the prose | all match the live package |
| 285.1 | `status == "registered"`, which the live table cannot show | verified by registering a method |
| 286 | 56 `Suggests` used / every `::` declared | clean both ways |

**What this pass adds.** §285.1 is the reusable bit: a documented idiom that
queries a *state the package is not in by default* cannot be checked by
inspecting the default state, and reading the table would have made the
documentation look wrong. The check has to put the package into the state
the sentence describes.

## 288. CI is green, and the cover letter had gone stale

`b94c612` pushed to `master`. GitHub Actions R-CMD-check:

| job | result |
|---|---|
| ubuntu-latest devel / release / oldrel-1 | `Status: OK` |
| windows-latest release | `Status: OK` |
| macos-latest release | 1 NOTE |

pkgdown and the pages deployment are green too. The test suite reports
**0 failures on every platform** — 3119 passing on Ubuntu, 3079 on Windows,
3065 on macOS.

The macOS note is `checking dependencies in R code`, and its whole body is
`rgl.so` failing to `dlopen` for want of `/opt/X11/lib/libGLU.1.dylib`.
That is S37's chain seen from the other end: `fabisearch` -> `NMF` /
`plot3D` -> `misc3d` -> `rgl`, on a runner with no XQuartz. S37 was the
*package* loading that chain through `cpt_methods()`; this is `R CMD
check`'s own dependency step doing it, which nothing in this package can
prevent short of dropping a suggested engine. Every other macOS line,
examples and tests and vignette rebuild included, is OK.

### 288.1 The submission note the fix invalidated

`cran-comments.md` still said:

> 0 errors | 0 warnings | 1 note. ... `installed size is 5.4Mb` /
> `doc 4.7Mb`

**That note no longer exists.** §249's `dpi = 72` removed it, and the
cover letter is the one document in the repository that no check reads, so
nothing caught the contradiction: the file would have told CRAN to expect a
note the package does not raise, alongside numbers from before the change.
Rewritten to state what the checks now report, plus the macOS `rgl`
explanation above.

One claim was narrowed while writing it. The draft said the installed
`doc` directory drops "to under 4 MB", from §249's local measurement of
3.66 MB — but CI reports `doc` at 4.0-4.4 Mb, because `R CMD check`'s size
accounting and `du` do not agree and platforms differ. A cover letter is
the worst place for a number that holds on one machine, so it now says
"cuts the installed `doc` directory by about a quarter" and cites only the
tarball, which was measured directly: 4,445,958 bytes.

**What this adds.** `cran-comments.md` is `.Rbuildignore`d and therefore
outside every audit this document has run: the `@return` sweep (§272), the
prose sweep (§285) and the two-directional Rd checks all stop at files the
package ships. The one file written *for* the reviewer is the one file
nothing verifies. Worth re-reading against a fresh check log before every
submission, because it goes stale exactly when a check result improves.

# Part XV — the guard for the defect CI cannot see

## 289. Four more clean audits

| audit | result |
|---|---|
| `README.md` in sync with `README.Rmd` | yes -- the `.md` was rebuilt one commit *after* the last `.Rmd` edit (`abf04c6` after `ae4cf68`), and nothing in this loop's nine rounds changed any output the README prints: it shows no `cpt_annotate_events()` output, no `plot()` on a result, and no stability plot |
| vdiffr snapshots orphaned or missing | none: 25 `expect_doppelganger()` calls, 25 distinct standardised names, 25 files |
| GitHub Actions action versions | all current: `actions/checkout@v4`, `r-lib/actions/*@v2`, `github-pages-deploy-action@v4.5.0` |
| the failure-only vignette diagnostic step | present and valid -- `tools::buildVignettes(dir = , ser_elibs = NULL)` looked like a typo and **is a real argument** in R 4.4.1, verified by calling it |

The snapshot audit gave a false alarm first: comparing file names against
test titles reported 19 orphans, because vdiffr's `str_standardise()` maps
*any* non-alphanumeric run to `-`, and my first pass only mapped
underscores. Second instrument error in three rounds (§268 was the first),
same shape: the sweep was wrong before the package was.

## 290. CI cannot catch the mistake that has been made three times running

The DESCRIPTION promises the package works with its Imports alone. **CI
installs every suggested package**, so a test that calls a Suggests engine
without a guard passes all five jobs and fails only in an Imports-only
check. That has now happened three times: `ggrepel` (S31), then `cpm`, then
this loop's own `fpop` -- which passed ubuntu devel/release/oldrel-1,
windows and macOS, and was caught by the local R 4.6.0 run minutes before
the push.

So the guard belongs in the suite, where it runs everywhere. It is exact in
three ways, each of which a naive version got wrong:

1. **The engine comes from the registry.** A method whose engine is an
   Imports package needs no guard; 6 of the 50 qualify (`pelt`, `binseg`,
   `segneigh`, `amoc`, `np`, `ecp`), and the other 44 need one.
2. **Only call forms count**, not the bare name. The first version matched
   any `"<method>"` string and flagged twelve blocks, of which ten were
   noise: `"var"` is a method *and* the `change_in = "var"` value, and
   several tests list method names as data (`test-050-engines.R:275`
   "used" eighteen engines it was only enumerating).
3. **A call inside `expect_error()` is exempt.** Two blocks survived the
   tightening and both are correct as they stand:
   `cpt_detect(<matrix>, method = "sn")` asserting "univariate" and
   `cpt_detect(x, method = "fpop", change_in = "var")` asserting "not
   supported". Both refusals fire *before* the engine is needed -- that is
   the shape-first ordering the 0.5.0 audit fixed -- so they have to work
   without it, and guarding them would delete the coverage that matters
   most on a minimal installation. My `fpop` slip was inside
   `expect_warning()`, which is the opposite: it needs the engine to run.

**Validated in both directions before being trusted**, which is the part
worth copying: the rule reports 0 blocks on the current suite, and
re-injecting the `fpop` line reports exactly `test-hardening.R:334 -> fpop`.
A guard that has not been shown to fire on the bug it exists for is a guess.

## 291. What this pass changes

| § | finding | measured how |
|---|---|---|
| 289 | README in sync, snapshots in sync, actions current, the CI diagnostic valid | four checks, one of which needed the argument list read rather than guessed |
| 290 | **CI structurally cannot see an unguarded Suggests call**; three occurrences to date | the `fpop` slip passed 5 CI jobs |
| 290 | the guard now catches it, with 0 false positives on 25 test files | run clean, then run against the re-injected slip |

New actions: none opened. One test added; no package code touched.

# Part XVI — twenty figures, one description between them

## 292. The hypothesis was backwards, and measuring said so twice

`autoplot()` attaches a real description to every plot it draws --
`cpt_alt_text()` builds it and `with_alt()` applies it as `labs(alt = )`:

> "Line chart of a time series of 120 observations ranging from -2.21 to
> 6.4, with 1 changepoint at 60 marked by vertical rules. Detected with the
> pelt method on a change in mean."

Each vignette also sets a single generic `fig.alt` in `opts_chunk$set()`, so
the working hypothesis was that the vignettes were overriding the good text
with a placeholder, and the fix was to delete the global option and let each
plot speak for itself. **Both halves of that were wrong.**

**First measurement -- the scale of the problem is real.** Counting `<img>`
alt attributes in the built `inst/doc`: **29 of 67 figures carried the
generic per-vignette string**, and `ggchangepoint.html` was the worst at
**20 of 26** -- twenty different figures all announced as "ggchangepoint
feature tour plot", which for a screen-reader user is indistinguishable
from no alt text at all.

**Second measurement -- the proposed fix makes it worse.** Rendering the
feature tour with the global `fig.alt` removed produced 26 `<img>` tags of
which only 6 had an `alt` attribute; the other **20 had none at all**.
knitr 1.50 does not read a ggplot's `alt` label: with `fig.alt` unset the
attribute is simply absent, for a package plot and a bare `ggplot2` plot
alike (`knitr:::get_alt_text` does not exist in this version). The global
option is not overriding good text -- it is the only thing standing between
those figures and nothing.

**And the first count of the removal was wrong too.** The regex matched
`<img ... alt="...">`, so an image with *no* alt attribute did not match,
and the first reading was "0 empty alts" -- exactly backwards. Counting
`<img` tags and alt attributes separately is what showed the gap. Third
instrument error in four rounds; the pattern is always the same, a sweep
that cannot see the thing it is looking for.

## 293. So the fix is the work the vignettes were already half doing

38 of the 67 figures already had hand-written per-chunk `fig.alt` --
"Series with shaded label regions behind it, coloured by what each label
asserts", and so on. The remaining 29 just had not been done. Written, one
per figure, describing what that figure shows:

| vignette | chunks given specific alt text |
|---|---|
| `ggchangepoint.Rmd` | 20 |
| `introduction.Rmd` | 5 |
| `comparison.Rmd` | 3 |
| `extending.Rmd` | 1 |

Existing chunk options are preserved (`eval = has_stepR`,
`fig.height = 6`), and the global `fig.alt` **stays** as the fallback for
any figure chunk added later without one -- which is the opposite of where
this section started.

The twentieth was found only by re-rendering: the scan looked for
`autoplot`/`ggcpt_*`/`ggplot` and missed the chunk that calls bare
`plot(res)` -- the one demonstrating the `plot()` methods added in §248.
After adding it, the feature tour renders **26 images: 0 without alt, 0
generic, 26 specific**. The only chunks still on the fallback across all
eight vignettes are three that draw nothing: two `ggcpt_compare_table()`
calls and the `ggcpt_interactive()` class check.

## 294. What this pass changes

| § | finding | measured how |
|---|---|---|
| 292 | **29 of 67 vignette figures shared a generic alt string**, 20 of them in one document | `<img alt>` count in the built `inst/doc` |
| 292 | knitr 1.50 does not use a ggplot's `alt` label, so removing the global fallback leaves **no** alt attribute | render with it removed, counting `<img` against `alt=` |
| 293 | 29 figures now carry a description of what they show; 0 without alt, 0 generic | re-render |

New actions: none opened. No package code touched -- 29 chunk options.

**What this pass adds.** The measurement order mattered more than the fix.
Had I edited first, I would have deleted the only alt text twenty figures
had, on the theory that something better would take its place -- and the
first count would have appeared to confirm it, because the regex could not
represent the failure. Two different measurements of the same change
disagreed, and the one that could see absence was the right one.

## 295. The web-only article: three checks, one gap

`vignettes/articles/benchmarks.Rmd` had never been looked at. It is
`.Rbuildignore`d, so it ships on the pkgdown site and not in the tarball,
which puts it outside `R CMD check` entirely -- the same blind spot
`cran-comments.md` sat in (§288.1).

| check | result |
|---|---|
| does §292's alt-text gap apply to it? | **no figures at all** -- the article has exactly one chunk, the `include = FALSE` setup, and every number in it is hard-coded prose |
| did §249's `dpi = 72` invalidate its "Size" section? | no -- "size" there is *empirical size* under the null, a false-positive rate, and the memory figures (`segneigh` at 1,208 MB) are unaffected |
| do its four `cpt_confint()` provenances still exist? | yes, and the shipped documentation is the more careful of the two |

### 295.1 A near-miss worth recording

The coverage table lists the provenances as `native`, `bootstrap`, `nsp`,
`posterior`, and `cpt_confint()` reports `source = "nsp_region"` -- not
`nsp`. That looked like §285.1's shape: a token a reader would try and
find missing. It is not a defect: the table names the values of the
`method` argument, which are exactly those four, and `?cpt_confint`
already spells out that a changepoint inside a region gets
`source = "nsp_region"`. The Rd is precise where the article is loose, and
the article is loose about the right thing.

### 295.2 The gap: measurements with no date

The provenance paragraph said the numbers came from "a single Linux
x86\_64 machine running R 4.4.1, with the engine versions current at the
time of measurement" -- honest about *what* they are and silent about
*when*. For a published page of measured numbers that is the one piece of
provenance a reader cannot reconstruct: nothing on the page distinguishes
0.4.0's engine wave from 0.5.0's. Now says "measured against
ggchangepoint 0.5.0 in August 2026".

## 296. What this pass changes

| § | finding | measured how |
|---|---|---|
| 295 | the web-only article has no figures, so §292 does not reach it; its "Size" section is statistical size, not file size | read it |
| 295.1 | the `nsp` / `nsp_region` mismatch is a documented distinction, not staleness | `cpt_confint()` on four fits, against the Rd |
| 295.2 | a page of measured numbers with no version or date on it | read the provenance paragraph |

New actions: none opened. One provenance line.

**What this pass adds.** Two files now have the same story: `cran-comments.md`
(§288.1) and `benchmarks.Rmd` are both `.Rbuildignore`d, both written for a
human reader, and both outside every automated check this document has run.
That is the whole category -- the files the package does not ship are the
files nothing verifies, and both of them had drifted. The other members of
that set (`README.Rmd`, `_pkgdown.yml`, the workflows) have now been checked
too (§285, §289), so the category is covered rather than merely noticed.

# Part XVII — the help system as a graph

## 297. Seven topics could not be reached from any other help page

The commit before this loop closed a documentation gap it described as
"inbound links 71->124". Measured again, as a graph over `man/*.Rd` --
resolve every `\link{}` through the alias table and count arrivals per
topic, ignoring self-links:

| | count |
|---|---|
| topics | 125 |
| with at least one inbound link | 116 |
| **unreachable** (non-internal, zero inbound) | **7** |

The seven: `tidy.ggcpt`, `glance.ggcpt`, `augment.ggcpt`, `summary.ggcpt`,
`print.ggcpt`, `cpt_install_engines`, and `ggcpt_plot_methods`.

**The cause is one omission repeated four times: none of the package's four
hub pages had a `@seealso` at all.** `cpt_detect()` -- the central function,
the page a reader lands on first -- had none, and its prose mentions
`\code{tidy()}` and `\code{augment()}` in code font without linking either.
So the five accessors that exist to get a result *out* of `cpt_detect()`
were unreachable from the page that produces it. Same for
`autoplot.ggcpt`, `ggcpt_methods` and `cpt_methods`.

`ggcpt_plot_methods` is mine, from §248: I gave it a `@seealso` pointing
*out* to `autoplot.ggcpt` and `ggcpt_methods` and never made either point
back. A new page is a leaf until something links it.

Fixed by adding the four missing `@seealso` blocks, each pointing where a
reader would actually want to go next -- `cpt_detect()` to the five
accessors, the plot method and `cpt_penalty()`; `cpt_methods()` to the
installer that fills its `installed` column; `autoplot.ggcpt` and
`ggcpt_methods` to each other's plot documentation. Re-measured: **123 of
123 non-internal topics reachable, 0 unreachable.**

### 297.1 Two verifications, one of them of a false alarm

Every new `\link{}` target resolves to a documented alias -- checked by
running the same alias table over the four edited files: 0 unresolved.

`tools::checkRd()` on those files then reported twelve issues, all
"Non-ASCII contents without declared encoding" on em-dashes in prose I did
not touch. Not defects: `checkRd()` called on a bare file cannot see
`Encoding: UTF-8` in the DESCRIPTION, and both full `R CMD check` logs
contain **zero** `Non-ASCII` lines. Worth recording because the natural
reaction to twelve new-looking issues is to start replacing em-dashes.

## 298. What this pass changes

| § | finding | measured how |
|---|---|---|
| 297 | **7 topics unreachable from any other help page**, because all four hub pages lacked a `@seealso` | link graph over `man/*.Rd`, resolved through aliases |
| 297 | now 0 unreachable | re-measured after the edit |
| 297.1 | `checkRd()`'s twelve encoding issues are an artifact of calling it standalone | `grep -c Non-ASCII` over both full check logs; `Encoding: UTF-8` in DESCRIPTION |

New actions: none opened. Four `@seealso` blocks; no package code touched.

**What this pass adds.** Reachability is a property of the *graph*, not of
any page, so it cannot be seen while reading pages one at a time -- which is
how documentation gets reviewed. Every one of the seven topics is well
written and correctly cross-referenced *outward*; they were invisible
because nothing pointed in. The measurement is four lines of alias
resolution and it is the only way this gap shows up at all.

# Part XVIII — the `...` surface, and the example gating

## 299. Every `...` goes where its documentation says

86 exports take `...`. Six never reference it in their body -- `alarms`,
`as_tibble`, `augment`, `autoplot`, `glance`, `tidy` -- and all six are
**generics**, whose body is `UseMethod()`: `...` travels by dispatch, so not
naming it is correct. That leaves 80 functions that actually forward it, and
**all 80 document it**; none has an undocumented `...`.

Then the harder question: does the documented destination match the actual
one? Extracting the `\arguments` entry for `...` from each Rd and the calls
that receive `...` from each body flagged **16 apparent mismatches** -- and
all sixteen were the extractor's fault. The receiver pattern required `...`
to sit inside a call with no nested parentheses before it, which misses
every real idiom in this package:

```r
ocp::onlineCPD(data_vec, getR = TRUE, <a multi-line list>, ...)
do.call(ggplot2::geom_rect, c(<...>, full_height_params(mapping), list(...)))
ggplot2::discrete_scale("colour", palette = cpt_pal(), na.value = na.value, ...)
```

Read directly, all three send `...` exactly where the documentation says.
**Fourth instrument error in five rounds** (§268, §289, §292, now this), and
the same shape every time: a sweep that cannot represent the thing it is
looking for reports its own blindness as a finding. The habit that keeps
catching it is reading the code before filing, not a better regex.

## 300. Example gating is already textbook-correct, with one omission

CRAN reviewers ask specifically about `\dontrun`, so it is worth knowing
exactly where it is used:

| mechanism | count | why |
|---|---|---|
| `@examplesIf` | 46 | the engine may not be installed -- the right tool, and the one used most |
| `\donttest` | 13 topics | slow but runnable; CI runs them with `--run-donttest` |
| `\dontrun` | **2 topics** | genuinely cannot run |

Both `\dontrun` uses are the textbook-correct cases: `mcp_wrapper()` needs
JAGS, a *system* library that no R-package check predicts, and
`cpt_load_tcpd()` downloads from the network, which an example must not do.

`mcp_wrapper()` explains itself above the block:

> Not run by R CMD check: whether this works depends on a *system* library,
> and no test of installed R packages predicts that reliably.

`cpt_load_tcpd()` had **no explanation at all** -- a reviewer would meet a
bare `\dontrun{}` and have to infer the reason. It now says the same kind of
thing in the same place: the calls download from the Turing Change Point
Dataset's repository, and an example must not require network access.

## 301. What this pass changes

| § | finding | measured how |
|---|---|---|
| 299 | all 80 forwarding functions document `...`; the 6 that ignore it are generics | formals and bodies over every export |
| 299 | the 16 "documented destination not used" hits were all extractor false negatives | read three of them in the source |
| 300 | `\dontrun` is used twice, both correctly; one had no justification | grep over `man/`, then read both blocks |

New actions: none opened. One example comment; no package code touched.

**What this pass adds.** §300 is a reminder that an audit can also confirm a
*good* state precisely, and that precision is worth having: "we use
`\dontrun` twice, here is each reason" is a sentence that answers a
reviewer's question directly, and it took a grep to be able to say it.

# Part XIX — guarding the fix that cost five CI rounds

## 302. Load-time behaviour, measured

S37 is the worst bug in this package's history to diagnose: `cpt_methods()`
filled its `installed` column with `requireNamespace()`, which **loads** the
package, so building a table loaded all 35 engine namespaces -- including
`fabisearch` -> `rgl`, which dies in `dyn.load()` on macOS for want of
`libGLU`. `R CMD check` reported it as `Vignette re-building failed` with no
chunk, no line and no message, and it took five CI rounds to find. The fix
was `find.package()`.

Measured now, in a fresh session:

| | value |
|---|---|
| `library(ggchangepoint)` | 1.09s, 30 namespaces -- the Imports and their transitive dependencies, no Suggests engine |
| `cpt_methods()` | **0.030s, 55 rows, and zero further namespaces loaded** |
| engine namespaces loaded after both | **3 of 38**, and all three are the Imports engines (`changepoint`, `changepoint.np`, `ecp`) |

The fix holds exactly. Nothing in the package loads a suggested engine until
a wrapper actually needs it.

### 302.1 Nothing guarded it, so now something does

The suite had no assertion on this at all -- the only uses of
`loadedNamespaces()` were in §290's `need_pkg` test. The new test is
order-independent, which matters because by the time it runs most engines
are already loaded by earlier files: it picks an engine that is **installed
and not yet loaded**, calls `cpt_methods()`, and asserts both that the row
says `installed = TRUE` and that the engine is *still* not loaded. That is
the S37 lesson stated as an executable property -- answering "is it
installed" must not load it.

**Proved to fire before being trusted**, the same way as §290. Reverting
`engine_installed()` to the pre-S37 `requireNamespace()` form makes the
hardening suite fail, and the side effect is visible in the very same run:

```
hardening: ..........Registered S3 methods overwritten by 'strucchangeRcpp':
══ Failed ══
```

Building the table loaded `strucchange` on the way past. Restored
immediately; `find.package()` is back at `R/detect.R:457`.

## 303. And the cross-references from §297 validate

The four `@seealso` blocks added last pass introduced eleven new `\link{}`
targets, which only `R CMD check` verifies -- and getting one wrong is
exactly the mistake of §248 (`\link[base]{plot.default}`, which lives in
**graphics**). Full check on the current tree:

```
* checking Rd cross-references ... OK
```

Status `1 ERROR, 2 WARNINGs, 3 NOTEs` -- the documented environmental
baseline -- with examples, `--run-donttest`, tests, the vignette rebuild and
installed size all OK.

## 304. What this pass changes

| § | finding | measured how |
|---|---|---|
| 302 | `cpt_methods()` runs in 0.030s and loads **nothing**; 3 of 38 engine namespaces loaded after startup, all of them Imports | fresh session, `loadedNamespaces()` before and after |
| 302.1 | no test guarded the S37 property; one now does, and it fails when the fix is reverted | temporary revert, then restore |
| 303 | §297's eleven new `\link{}` targets all resolve | `checking Rd cross-references ... OK` |

New actions: none opened. One test; no package code changed.

**What this pass adds.** The regression guards worth writing are for the
bugs that were *hard to see*, not the ones that were hard to fix. S37 was a
one-line fix after five rounds of looking in the wrong place, and the reason
it hid so well is that its symptom appeared in a different subsystem
entirely -- a vignette failing to rebuild on one platform. A test that
states the property directly ("asking must not load") would have turned
five rounds into one.

# Part XX — auditing my own release notes

## 305. All 25 NEWS claims hold, and one ledger number did not

`NEWS.md`'s "Fixes found in the pre-submission audit" section now carries
**25 bullets**, written across fifteen passes from memory of what had just
been done. That makes them the newest and least-verified claims in the
repository -- and the release notes are what a user and a CRAN reviewer
actually read. So each bullet was turned into a live assertion:

| | result |
|---|---|
| bullets asserted | 25 |
| **PASS** | **25** |
| FAIL | 0 |
| skipped for a missing engine | 0 |

Every one holds. A sample of what was checked rather than taken on trust:
`plot()` on a `ggcpt_consensus` produces the same `$labels` as
`autoplot()` on it (bullet 2); `withVisible(plot(fit))$visible` is `FALSE`
(3); `validate_data()` accepts a logical vector *and* `cpt_select()` runs on
one (5); `?cpt_monitor`'s Rd contains "Multivariate only" (11);
`cpt_annotate_events()` omits `cp_index` without an index and types it
`Date` with one (22); `autoplot()` on an indexed stability result renders a
`ScaleContinuousDate` (23); and `dpi = 72` appears in exactly seven vignette
files (25).

### 305.1 The one wrong number was in this document

Bullet 18 says "Ten entry points read locations through a bare
`as.integer()`" and lists them. Counted from the code: `as_cp_locations()`
is called at **thirteen** places -- `cpt_metrics()` and `ggcpt_eval()` take
two each, for `pred` and `truth` -- plus a fourteenth use where it is passed
to `lapply()` inside `cpt_metrics_annotated()` rather than called directly.
Those thirteen sites sit in exactly **ten** distinct entry-point functions,
so the NEWS bullet is right.

§266.1 of this ledger said "applied at eleven sites", which is neither
number. Corrected in both places it appeared. The error is small and the
direction is worth noting: the shipped document was accurate and the
working notes were not, because the working notes were written while the
edits were still in flight and never recounted afterwards.

## 306. What this pass changes

| § | finding | measured how |
|---|---|---|
| 305 | **25 of 25 NEWS claims verified against the running package** | one live assertion per bullet |
| 305.1 | `as_cp_locations()` is at 13 call sites in 10 entry points; the ledger said 11 | `grep -c` over `R/`, then reading each site |

New actions: none opened. One number corrected in this document; no code
and no NEWS text changed.

**What this pass adds.** Fifteen passes of this loop have applied one rule to
the codebase -- a claim deserves a measurement -- and had never applied it to
the loop's own output. Doing so found the release notes clean and the
ledger's arithmetic wrong, which is the right way round but not a
coincidence: NEWS was written last, after the code settled, while §266.1 was
written mid-change. **A claim made while the thing it describes is still
moving is the one to re-check.**

# Part XXI — do the tests assert anything?

§305.1 found one of my own guards passing under a filter and **skipping** in
the full suite. That is a failure mode, not an accident, so the question
generalises: how many of the 327 test blocks run without asserting anything?

## 307. None of them

Measured by collecting per-test results from `test_local()` rather than
reading the files -- the count that matters is expectations that *executed*,
not `expect_*` calls that appear in the source:

| | value |
|---|---|
| `test_that` blocks | 327 |
| passing expectations | 3,234 |
| failed | 0 |
| skipped blocks | 2 (`mcp` absent, the TCPD download) |
| **blocks that ran and asserted nothing** | **0** |

The thinnest blocks assert one expectation each, and all six are
single-fact regressions from the 0.4.0 audit (`C4`, `C8`, `C11`, `C16`,
the SegNeigh penalty fallback, `np`'s `change_in`). One assertion is thin;
zero would have been the defect, and there are none.

## 308. Three of the five recurring warnings were the suite's own noise

Every pass of this loop has reported "5 upstream warnings" as a constant.
Looked at properly, they are two different things.

**Three were escaping because the test asserts a *different* warning.**
`expect_warning()` captures the one that matches its pattern and lets the
rest bubble up to the report:

| site | escaping warning | now |
|---|---|---|
| `test-new-infrastructure.R:65` | `SegNeigh is computationally slow, use PELT instead` | asserted |
| `test-wrappers.R:31` | same | asserted |
| `test-040-bugfixes.R:495` | `Very low number of permuted data sets` from kcpRS at `nperm = 20` | muffled **by message text** |

`changepoint` advises PELT on every `segneigh` call, so asserting it both
documents the upstream behaviour and takes it out of the report. The kcpRS
complaint is about a parameter the *test* chose for speed, and the block
already asserts a different warning it must keep, so a text-scoped
`withCallingHandlers()` muffle is the precise instrument -- nothing else is
hidden. (The first attempt nested `expect_warning(..., NA)` around it, which
fails: the second warning only occurs for one of the three engines the loop
covers.)

**Two are staying, because the existing test already argues for them.**
`R52` deliberately does not assert `envcpt`'s convergence warning, and says
why:

> whether a given series also triggers an upstream convergence *warning* is
> data-dependent, so it is not asserted here; warnings are deferred past the
> diversion by construction and reach the user unchanged.

That is a substantive point about what the test is checking -- the test
diverts the *message* stream and the comment records that warnings survive
the diversion. Muffling them for a tidier report would overrule a
documented decision to gain nothing.

Suite report: **FAIL 0 | WARN 2 | SKIP 2**, down from WARN 5, and both
remaining warnings are the ones a reader is meant to see.

## 309. What this pass changes

| § | finding | measured how |
|---|---|---|
| 307 | **0 of 327 blocks assert nothing**; 3,234 expectations execute | per-test results from `test_local()` |
| 308 | 3 of the 5 standing warnings were the suite's own, escaping past an `expect_warning()` for a different pattern | read each site |
| 308 | the other 2 are deliberate and documented in the test | read the comment |

New actions: none opened. Three test sites; no package code touched.

**What this pass adds.** A standing number in a status report stops being
information once you repeat it -- "5 upstream warnings" was in every pass of
this document and nobody, including me, had opened them. Three were noise
the suite made itself. The other two were load-bearing, and the difference
was only visible by reading the test that emits them.

# Part XXII — what a detection call does to your session

## 310. Two engines were rewriting the caller's search path

Instrumenting a full suite run for global state -- options, attached
packages, the registry, the RNG -- turned up four packages on the search
path afterwards that had not been there before. Traced per wrapper, in a
fresh session each time:

| wrapper | attached and left behind |
|---|---|
| `bcp_wrapper()` | `package:bcp`, `package:grid` |
| `fabisearch_wrapper()` | `doRNG`, `rngtools`, `doParallel`, `parallel`, `iterators`, `foreach`, `Biobase`, `BiocGenerics` -- **eight** |
| `bocpd_wrapper()`, `beast_wrapper()` | none |

Nothing in this package calls `library()`. `bcp` attaches itself and
`grid` when its namespace loads, and `fabisearch` needs NMF *attached*
rather than loaded -- which the wrapper does deliberately, and documents --
so NMF's Depends and the engine's own `foreach`/`doParallel`/`doRNG`
registration come with it.

**The 0.5.0 audit checked this and reported it clean.** Its claim was "no
option, no attached package, no RNG leak -- the fabisearch NMF attach does
detach", and the last clause is exactly what was verified: the wrapper's
`on.exit` detached `package:NMF`. What it never checked was what came *with*
NMF. One of eight is a pass if you look only at the package you named.

Both wrappers now record `search()` and give back whatever the call added,
detaching in `search()` order (most recently attached first, which is the
only order that works). Measured after: **`bcp` 2 -> 0, `fabisearch`
8 -> 0**, with the same results as before in each case.

### 310.1 The first attempt fixed six of eight, and the reason is instructive

`fabisearch` went from eight to two: `Biobase` and `BiocGenerics` survived,
and the `try()` around the detach swallowed whatever refused. In isolation
all three of `NMF`, `Biobase`, `BiocGenerics` detach cleanly -- so it was
not a refusal at all. The baseline was in the wrong place: I recorded
`search()` after `need_pkg("fabisearch")`, and **loading fabisearch is
itself what attaches Biobase**, so those two were already in the baseline
and were never candidates for removal. Moving the record above `need_pkg()`
takes it to zero.

The `try()` is what made this look like a permissions problem instead of an
arithmetic one. A swallowed error is a bad place to keep a diagnosis.

### 310.2 What was deliberately not done

Loading `bcp` also prints "Loading required package: bcp" and "Loading
required package: grid", because it attaches them itself. Wrapping
`requireNamespace()` in `suppressPackageStartupMessages()` does **not**
silence them -- they are plain messages from `library()` calls inside bcp's
own load -- and the next tool up, `suppressMessages()`, would hide anything
an engine says at load time for a cosmetic gain. Reverted. §252's fix
targeted a *warning*, which is louder, appears in `R CMD check` output, and
was about the machine; conventional load chatter is neither, and escalating
the instrument to win the point would have been the wrong trade.

## 311. The rest of the suite's global state is clean

| property | after a full suite run |
|---|---|
| registered methods left behind | **0** |
| options changed | 45 -- all created by loading engines (`bfast.*`, `datatable.*`, `BioC`), none set by this package |
| RNG seed | unchanged |

And the timing, since check time matters at submission: 249.6s across 327
blocks. The three slowest are `ocd` at ~21s each, a quarter of the suite --
and they already pass `mc_reps = 10` with a comment explaining that
`mc_reps` only calibrates the threshold. The cost is the engine's
per-observation streaming loop, not a test parameter, so there is nothing to
trim without shortening the series the assertions depend on. Worth knowing
before optimising it.

## 312. What this pass changes

| § | finding | measured how |
|---|---|---|
| 310 | **`bcp_wrapper()` left 2 packages attached, `fabisearch_wrapper()` left 8**; the 0.5.0 audit verified only that NMF itself detached | search path before and after, per wrapper, fresh session |
| 310.1 | the first fix reached six of eight because the baseline was recorded after the load that attaches the other two | detaching them by hand, which worked |
| 311 | 0 registered methods leak, 0 options set by this package, RNG unchanged; the suite's slowest tests are already minimal | instrumented full run |

New actions: none opened. Two wrappers and one test.

# Part XXIII — the vignettes: citations, and a count that contradicted the table below it

A new standing instruction: verify the vignettes every round -- prose,
citations, claims, cross-references. This is the first pass.

## 313. Citations resolve, and the bibliography had nine dead entries

**61 distinct citation keys** are used across the eight vignettes.

| check | result |
|---|---|
| keys that resolve in `vignette_reference.bib` | **61 of 61 -- 0 broken** |
| `\insertRef` keys in `R/*.R` present in `inst/REFERENCES.bib` | **50 of 50 -- 0 missing** |
| entries missing author, title, year or venue | 0 (`rcore` is a `@Manual`, which carries `organization`) |
| prose years contradicting the `year` field | **0** |
| multi-word surnames and LaTeX escapes | render correctly -- `van den Burg GJJ`, `Demšar`, `Lindeløv` |

Two things looked like defects and were not. `{van den Burg}` is braced in
one entry and unbraced in another; both parse to `family = "van den Burg"`
through `rbibutils` and render identically, so the unbraced form needed no
"fix". And 29 of 77 rendered entries repeat the DOI URL
(`doi:10.4855... <https://doi.org/...>, <https://doi.org/...>`) -- that is
`rbibutils`' own formatting of a `doi` field, not something in this
bibliography.

**What was real: nine entries nothing cites anywhere.** Eight are cited by
no vignette, no Rd and no code -- `chen2009information`,
`gneiting2006geostatistical`, `hariz2007classification`,
`lai2005comparative`, `meinshausen2006estimating`, `olshausen1997sparse`,
`rigaill2020fpop`, `wang2022overview`. Sparse coding and geostatistical
space-time models are not changepoint references; these look like the
residue of a broader library. The ninth is `vandenburg2020evaluation`, a
**duplicate of `van2020evaluation`** -- the same paper, same DOI, in the
same file under two keys, one for the vignettes and one for the Rd (which
reads `inst/REFERENCES.bib`, where it also lives, so the Rd path is
unaffected).

Removed: 77 entries -> **68**, still 61 cited, still 0 broken. The seven
that remain uncited by a vignette are all `\insertRef`'d from the Rd, so
the vignette bibliography being a superset of the Rd's is a maintenance
choice rather than dead weight.

`cpt_cite()` deserves a note: the Demšar (2006) reference behind the
critical-difference diagram is uncited in the vignette bib and looked like
an attribution gap, but it is named in `?cpt_benchmark`'s prose *and*
returned in full by `cpt_cite("critical_difference")`. The package's own
citation mechanism covers it.

## 314. "Four rows carry status planned" -- the table below says five

`vignettes/ggchangepoint.Rmd` claimed:

> `cpt_methods()` is the live capability table ... **Four rows** carry
> status `"planned"` rather than `"available"`: their engines (`gfpop`,
> `robseg`, `FOCuS`, `hdbinseg`) are not on CRAN

`planned_methods()` has **five** rows: those four engines plus
`changeforest`. And the chunk on the very next line is
`print(cpt_methods(), n = Inf)` -- so a reader is told "four" and then
shown five, on the same screen.

`vignettes/introduction.Rmd` gets it right ("**Five** more, whose engines
are not currently on CRAN", listing all five including random-forest
classification), so the two vignettes contradicted each other. Corrected.

**This is the second time this exact claim has gone stale**: the 0.5.0
audit's S30 recorded "a stale planned-engine count" as one of its findings.
So it is now guarded -- a test extracts the engines named in that sentence
and the number word in front of it, and compares both to
`planned_methods()`. Proved to fire: changing "Five" back to "Four" fails
the suite.

### 314.1 A check I designed badly, recorded so it is not repeated

Before that, I compared per-capability engine lists (`ci`, `fitted`,
`posterior`, `statistic`, `path`) against the registry -- using a list I
had **invented**, not one taken from the vignettes. The mismatches it
reported said nothing about the package. What the exercise did establish,
by accident, is the useful fact: the vignettes do not hard-code capability
lists at all. They write `subset(cpt_methods(), ci)$method` and let the
chunk execute, so there is nothing there to go stale. A claim generated by
running code cannot rot; that is why §314's hand-written count is the one
that did.

## 315. What this pass changes

| § | finding | measured how |
|---|---|---|
| 313 | 61 of 61 vignette citations resolve; 50 of 50 Rd `\insertRef` keys resolve; 0 prose years wrong | key extraction against both bibs |
| 313 | **9 bibliography entries cited nowhere**, one of them a duplicate of an entry in the same file | literal search for each key across every non-bib file |
| 314 | **the feature-tour vignette said "four planned rows" where the table it prints shows five** | `planned_methods()` |
| 314.1 | the vignettes query capabilities rather than hard-coding them | grep for `subset(cpt_methods(), ...)` |

New actions: none opened. One prose correction, nine bib entries removed,
one guard added.

# Part XXIV — a whole vignette nothing linked to

## 316. The same defect as §314, one sentence away

The feature tour's opening reads:

> **Four companion vignettes** go deeper: `vignette("introduction")` ...
> `vignette("comparison")` ... `vignette("inference")` ...
> `vignette("supervised")` ... and `vignette("extending")` ...

Three numbers, none of which agree: the sentence says **four**, it lists
**five**, and there are **six** companion vignettes. The one it omits is
`monitoring` -- and it was the only vignette in the package that **no other
vignette referenced at all**:

| vignette | referenced by (before) |
|---|---|
| comparison, extending, inference, introduction, supervised | one or two others |
| ggchangepoint | comparison |
| **monitoring** | **nothing** |

The closing "next steps" paragraph omits it too, so streaming and online
monitoring -- an entire 0.5.0 feature area, with its own `cpt_monitor()`,
`cpt_update()`, `alarms()`, `cpt_replay()` and `cpt_delay()` -- was
unreachable by a reader working through the vignettes in R. `_pkgdown.yml`
lists it, so the website was fine; the offline path was not.

Corrected in both places, and the count now reads six. Re-measured: all
seven vignettes are referenced by at least one other.

## 317. Guarded, because this is now twice in two rounds

§314's guard was specific to the planned-engine sentence. This one
generalises the class: **every vignette must be referenced by another
vignette**, and the tour's own count of its companions must equal the
number of companions that exist.

Both halves proved to fire before being trusted:

- changing "Six" to "Five" -> the suite fails
- replacing both `vignette("monitoring")` references -> the suite fails,
  and the failure prints `"monitoring"` by name

The first attempt at the second half did **not** fire, because I removed
the reference from the opening sentence only and the closing paragraph
still had one -- so the assertion was right and my test of it was wrong.
Worth recording: a guard that "does not fire" needs the same scepticism as
one that fires.

## 318. The rest of this round's vignette pass

| check | result |
|---|---|
| `%\VignetteIndexEntry{}` vs the YAML `title` | **7 of 7 match** (a mismatch is an `R CMD check` warning) |
| `vignette("...")` targets that do not exist | **0** |
| citations, after §313's cleanup | 61 cited, 68 entries, 0 broken |

## 319. What this pass changes

| § | finding | measured how |
|---|---|---|
| 316 | **the tour said four companions, listed five, and six exist**; `monitoring` was linked from nothing | reachability graph over `vignette("...")` calls |
| 317 | the class is now guarded, both halves proved to fire | deliberate breakage, then restore |
| 318 | index entries, cross-reference targets and citations all clean | direct comparison |

New actions: none opened. Two prose corrections and one guard.

**What this pass adds.** Both §314 and §316 are the same failure: a number
written by hand next to a list maintained by hand, in a document nothing
checks. The vignettes' *executed* claims cannot drift -- §314.1 established
that they query `cpt_methods()` rather than hard-coding it -- so every
remaining risk in them is prose of exactly this shape. Enumerating those
sentences is now a cheap, finite job, and two of them have been wrong.

# Part XXV — the census, and the 0.4.0 number in the opening sentence

§316 said enumerating the vignettes' hand-written numbers was "a cheap,
finite job". Done: every number word or numeral in vignette *prose*
(chunks excluded) followed within three words by a package noun --
methods, engines, vignettes, rows, families, provenances, geoms, criteria.
**50 claims.** Most are structural and cannot rot ("one row per
changepoint", "one more row than `$changepoints`"). Six state a total, and
one of those was wrong.

## 320. The introduction advertised 31 methods

`vignettes/introduction.Rmd`, third line of the opening paragraph:

> a central dispatcher `cpt_detect()` covering **31** detection methods
> across six algorithmic families

**31 is the 0.4.0 number.** It is 50 -- and 31 -> 50 is the headline of the
release this vignette introduces. The first sentence of the first vignette a
reader opens advertised the previous version's capability.

Corrected. The "six algorithmic families" beside it stays: the vignette's
own §325 sentence enumerates six and §285 verified all eight sections it
names, so that number is internally consistent even though the feature tour
counts nine methodological families -- the two are different groupings, one
of the introduction's sections and one of the registry's.

Also corrected, and self-inflicted: the tour's 0.5.0 section said "**three**
companion vignettes develop these properly" while the closing paragraph --
which §316 had just changed -- lists four. Fixing one count exposed the
other.

## 321. Guarded, after five wrong versions of the guard

The total-method count changes every release, so it is worth a test rather
than a proofread. Getting the test right took more attempts than the fix:

1. **Too broad.** "Any number before `methods`" flagged the benchmarks
   article's "**Ten** methods raise **zero** false alarms" -- a measured
   subset, not a claim about the package. Narrowed to the five *totalising*
   constructions the vignettes actually use: `covering N methods`,
   `all N wired methods`, `wraps N detectors`, `N methods share one
   interface`, `N methods in this`.
2. **`[[` on a missing name errors** rather than returning `NULL`, so
   `words[[tok]] %||% NA` threw on "wrapped methods" instead of skipping it.
3. **The guard silently never fired.** `regmatches(m, regexpr(pat, sub(...)))`
   applies the offsets from the *substituted* string to the *original*, so
   it extracted `"co"` from `"covering 31 detection methods"` -- never a
   number, always skipped. Replaced with plain `sub()` calls.
4. **My verification of it was also wrong**: `sed 's/covering 50 detection/.../'`
   matched nothing, because the vignette has "covering 50" and "detection
   methods" on different lines. The test reads the file joined with spaces;
   `sed` works line by line. Two clean runs looked like proof and were
   proof of nothing.

Final state, verified in both directions: the clean tree passes, and
breaking the count in **each of the four vignettes that state a total**
fails the suite -- `introduction.Rmd`, `comparison.Rmd`, `extending.Rmd`
and `benchmarks.Rmd` -- while the benchmarks article's subset counts stay
quiet.

## 322. What this pass changes

| § | finding | measured how |
|---|---|---|
| 320 | **the introduction's opening paragraph claimed 31 methods**; it is 50 | census of 50 prose numbers, then the registry |
| 320 | the tour said three 0.5.0 companion vignettes where its own closing list now names four | reading both after §316's edit |
| 321 | the guard was wrong four different ways before it worked, including one version that could never fire | deliberate breakage in four files |

New actions: none opened. Two prose corrections and one guard.

**What this pass adds.** Fifth instrument error in this document, and the
most instructive: version 3 of the guard *passed on a clean tree and passed
on a broken one*. A test that cannot fail is worse than no test, because it
reports safety. The only thing that caught it was breaking the file on
purpose -- which is now the standing rule for every guard added here, and it
has paid for itself four times (§290, §302.1, §314, §321).

# Part XXVI — attribution: are the citations pointing at the right work?

The standing instruction asks whether citations are "attributed to the right
work". Keys resolving (§313) is a weaker property than that. The package
carries its own answer: `cpt_cite(method)` returns, per method, the paper
the package says introduced it. So the two can be checked against each
other.

## 323. A check that had to be redesigned before it said anything

**First attempt: co-location.** For each method, collect the `@keys` on
prose lines that mention the method's name, and compare their first author
with `cpt_cite()`'s. It paired only **4 of 50** methods -- method names
mostly appear inside chunks, and citations mostly sit in different
sentences -- and its single flag was `ecp` "citing" `@grundy2020geomcp`,
which is one sentence mentioning two engines. Useless in both directions.

**Second attempt, well-defined:** does the paper `cpt_cite()` names exist
*anywhere* in the two bibliographies -- matching on first-author surname and
year across all 123 pooled entries? That question has an answer, and it
found four.

## 324. Two founding papers were named in prose and in no bibliography

| method | `cpt_cite()` says | status |
|---|---|---|
| `segneigh` | Auger and Lawrence (1989) | **absent from both bibs** |
| `amoc` | Hinkley (1970) | **absent from both bibs** |
| `mcp` | Lindelov (2020) | present as `lindelov2020mcp`; the matcher cannot equate `Lindel{\o}v` with ASCII |
| `fcov` | Aue, Rice and Sonmez (2020) | absent, and correctly so |

`introduction.Rmd` names the first two **as bare parentheticals** --
"segment neighbourhoods (Auger and Lawrence, 1989), and at-most-one-change
(AMOC; Hinkley, 1970)" -- in a sentence where every other method carries a
real citation (`[@killick2012pelt]`, `[@scott1974cluster]`,
`[@maidstone2017optimal]`). So two of the four `changepoint` methods the
package has wrapped since 0.1.0 appeared in the text and **never in the
References**.

Fixed: both added to `vignette_reference.bib` with the metadata
`cpt_cite()` already carries -- volume, issue, pages -- and **no invented
DOIs** -- and the parentheticals replaced with `[@auger1989segment]` and
`[@hinkley1970inference]`. Verified by rendering: **0 unresolved citation
markers**, each new key appearing twice in the HTML, once as "(Auger and
Lawrence 1989)" in the text and once in the bibliography block.

`fcov` is the interesting non-fix. `cpt_cite("fcov")` cites Aue, Rice and
Sönmez (2020) on covariance operators and adds "See also ... (2018)", while
`fmean` cites the 2018 functional-mean paper -- two different papers,
correctly distinguished. The 2020 one is in no bibliography because **no
vignette discusses `fcov` at all**: every "functional" in the vignettes
means functional *pruning* (`fpop`, `cpop`, `decafs`), a different idea.
Adding an entry nothing cites would just recreate §313's dead weight, and
`cpt_cite()` already gives the reference in full -- the same conclusion
Demšar reached in §313.

## 325. What this pass changes

| § | finding | measured how |
|---|---|---|
| 323 | the co-location design paired 4 of 50 methods and its one flag was spurious; withdrawn | ran it |
| 324 | **`segneigh` and `amoc` were cited in prose and in neither bibliography** | 50 methods' `cpt_cite()` authors and years against 123 pooled bib entries |
| 324 | 0 unresolved citations after the fix; both render in text and bibliography | `rmarkdown::render()`, counting `???` |

New actions: none opened. Two bibliography entries, two prose citations.

**What this pass adds.** `cpt_cite()` turned out to be the right oracle for
a question the bibliography cannot answer about itself. "Does every key
resolve" is a closed loop -- the bibliography checked against the text that
cites it. "Does the package's own record of who invented each method appear
in the bibliography" compares two independently maintained lists, and that
is where the gap was.

# Part XXVII — the help pages and `cpt_cite()` disagreed about seven methods

§324 compared `cpt_cite()` against the *bibliographies*. The same oracle
compared against the *help pages* -- for each of the 50 methods, the
`\insertRef` keys in its wrapper's roxygen block versus the author and year
`cpt_cite()` gives -- flags **15**.

## 326. Eight of the fifteen are correct by design

| pattern | methods | why it is right |
|---|---|---|
| one Rd covers five methods | `pelt`, `binseg`, `segneigh`, `amoc`, `np` | `cpt_wrapper()` documents all five, so its `@references` cites the **software** paper (`killick2014changepoint`) while `cpt_cite()` resolves each **method** paper -- Killick 2012, Scott and Knott 1974, Auger and Lawrence 1989, Hinkley 1970, Haynes 2017 |
| software paper on the Rd | `ecp` | Rd cites `james2014ecp`, `cpt_cite()` gives Matteson and James 2014 |
| LaTeX escape | `mcp` | `Lindel{\o}v` versus ASCII `Lindelov` |

That is the division of labour the package intends: the help page names the
package you are calling, `cpt_cite()` names the paper you should cite.

## 327. Seven wrappers cited nothing at all

`fpop_wrapper()`, `wbs_wrapper()`, `wbs2_wrapper()`, `not_wrapper()`,
`mosum_wrapper()`, `idetect_wrapper()` and `tguh_wrapper()` had **no
`@references` block and no `\insertRef`** -- seven method help pages with no
reference, where the other forty-three have one.

The cause is upstream of the roxygen: none of the seven keys existed in
`inst/REFERENCES.bib`. They were in `vignettes/vignette_reference.bib`
only, and `\insertRef{key}{ggchangepoint}` reads the *installed* bibliography,
so the reference could not have been inserted even if someone had tried.
Copied across -- `maidstone2017optimal`, `fryzlewicz2014wild`,
`fryzlewicz2020detecting`, `baranowski2019narrowest`,
`eichinger2018mosum`, `anastasiou2022idetect`, `fryzlewicz2018tail` -- and
each wrapper given the `@references` block its siblings have.

## 328. And one help page credited the wrong paper

`fcov_wrapper()` detects changes in the **covariance operator** of a
functional series. Its Rd cited `aue2018fchange` -- "Detecting and dating
structural breaks in functional data without dimension reduction" -- which
is `fmean_wrapper()`'s paper, cited from the same file eleven lines above.
`cpt_cite("fcov")` has the right one and always did: Aue, Rice and Sönmez
(2020), "Structural break analysis for spectrum and trace of covariance
operators", *Environmetrics* 31(1), e2617.

So a reader of `?fcov_wrapper` was sent to a paper about the functional
*mean* for a method about *covariance*. Added `aue2020covariance` to
`inst/REFERENCES.bib` and repointed the reference.

This is the entry §324 deliberately did **not** add to the vignette
bibliography, on the grounds that nothing cited it. That reasoning still
holds and this is not a reversal: the entry belongs in the bibliography
where it is cited, and it is cited from an Rd, so it goes in the Rd's
bibliography.

Re-measured: **15 flags -> 7**, all seven the design cases above. All **58**
`\insertRef` keys across `R/` resolve, 0 missing.

## 329. What this pass changes

| § | finding | measured how |
|---|---|---|
| 326 | 8 of 15 flags are the intended software-versus-method split | read each |
| 327 | **7 wrappers had no reference at all**, because their keys were only in the vignette bibliography and `\insertRef` cannot see that file | roxygen blocks against `inst/REFERENCES.bib` |
| 328 | **`?fcov_wrapper` cited the functional-mean paper for a covariance method** | `cpt_cite("fcov")` against the Rd |

New actions: none opened. Eight bibliography entries, seven `@references`
blocks, one corrected reference.

**What this pass adds.** §327's cause is worth the note: the seven missing
references were not an oversight in the roxygen, they were *impossible*.
`\insertRef` reads `inst/REFERENCES.bib`; the keys lived in
`vignettes/vignette_reference.bib`; two bibliographies that look
interchangeable are not, and the failure is silent -- no warning, no check
note, just a help page with no reference. Which bibliography a citation
mechanism reads is worth knowing before wondering why nothing renders.

# Part XXVIII — two bibliographies, one paper, two author lists

## 330. 47 shared keys, one divergence

The two bibliographies overlap by **47 keys**, and a key present in both
with different content renders one way in a vignette's References and
another in a help page. Compared entry by entry -- author list plus title,
journal, volume, number, pages, year, doi, publisher:

**One divergence.** `zhao2019beast`:

| file | authors |
|---|---|
| `vignette_reference.bib` | `Zhao, Wulder, Hu and others` -- three named, then `et al.` |
| `inst/REFERENCES.bib` | all **eleven**: Zhao, Wulder, Hu, Bright, Wu, Qin, Li, Toman, Mallick, Zhang, Brown |

Everything else about the entry is identical, so the same paper printed a
three-author list in the vignette bibliography and an eleven-author list in
the help pages. The full list is the accurate one; the vignette bib now
carries it too. Re-measured: 47 shared keys, **0** with differing author
lists.

`cpt_cite("beast")` renders "Zhao, K., Wulder, M. A., Hu, T., et al.
(2019)", which is a citation *string* and correctly abbreviates -- that is a
different artifact from a bibliography entry and needs no change.

## 331. `inst/CITATION` hard-coded the version twice

`citation("ggchangepoint")` is reader-visible and the file agreed with
`DESCRIPTION` on every field -- title, author, URL, and version 0.5.0. But
the version was **written out twice as a literal**, in `note` and again in
`textVersion`, so the next bump would leave `citation()` advertising the
previous release. That is §320's defect (the introduction's stale "31
methods") waiting to happen in the citation a user pastes into a paper.

Both now read `meta$Version`, which is what R passes to a CITATION file and
what *Writing R Extensions* recommends. Verified by bumping `DESCRIPTION`
to 0.6.0 in place: both fields report 0.6.0 and **no 0.5.0 remains**
anywhere in the rendered entry. Restored.

## 332. What this pass changes

| § | finding | measured how |
|---|---|---|
| 330 | **the same paper carried a 3-author list in one bibliography and 11 in the other** | 47 shared keys x 9 fields, both files parsed with `rbibutils` |
| 331 | `inst/CITATION` hard-coded the version in two places | read it against `DESCRIPTION`, then bumped the version to see what moved |

New actions: none opened. One author list, one self-updating version.

**What this pass adds.** §331 is a defect that does not exist yet, which is
a category this document has not knowingly worked in before: every claim
audited so far was already true or already false. A literal that must be
edited in lock-step with another file is *guaranteed* to go stale, and the
cost of finding it later is a wrong citation in someone's paper. Worth
looking for the rest of them -- the version string is the obvious one, and
the year `2026` in the same file is the next.

# Part XXIX — the lock-step literals, and a guard debugged three ways

§331 ended by asking where the rest of the "must be edited in lock-step"
literals are. Answer: two, both unavoidable, now guarded.

## 333. The own-version thread closes clean

Every `0.5.0` in `R/` and `tests/` is a **historical comment** -- "0.5.0:
diagnostics, selection and influence", "0.5.0 layers: the objects the
package could not draw before" -- which stays true forever. The only code
that reads a version uses `packageVersion()`, and the one version
*comparison* (`packageVersion("HDCD") <= "1.1"`, the Pilliat guard) is about
an upstream package and documented. Nothing to fix.

## 334. Two files state the method total and cannot compute it

| file | text |
|---|---|
| `DESCRIPTION` | "a 'cpt_detect()' dispatcher covering fifty methods" |
| `R/ggchangepoint.R` -> `man/ggchangepoint-package.Rd` | "dispatcher \code{cpt_detect()} that reaches fifty methods" |

Both are correct today. Neither can be computed -- `DESCRIPTION` and Rd are
static text -- so the literal has to stay, which makes them precisely the
shape that goes stale on the next engine wave, exactly as §320's "31
detection methods" did in a vignette. §321's guard now covers them
alongside the four vignettes that state a total.

## 335. Three bugs between writing the guard and having one

Extending a working guard to two more files took three fixes, and all three
presented as "the guard does not fire":

1. **Indented continuation lines.** `DESCRIPTION` wraps as
   "dispatcher covering\n    fifty methods", so joining lines with a single
   space leaves a *run* of blanks and a pattern written with literal spaces
   never matches. Fixed by normalising whitespace before matching -- which
   also retires the line-wrap trap that bit §321's verification.
2. **My verification, again.** Breaking the Rd with a plain string replace
   looked like it had worked and the guard stayed silent, so I concluded the
   path was unresolved. The path was fine.
3. **The real bug: a half-added pattern.** The new
   `reaches ([A-Za-z0-9]+) methods` pattern was added to the pattern list
   but `reaches` was never added to the alternation that strips the leading
   word before reading the number. So `tok` came out as `"reaches"`, which
   is not a numeral and not a number word, and every match was silently
   skipped. A pattern and its parser are two edits, and only one of them was
   made.

Final state: clean tree passes; breaking the count fires in
`DESCRIPTION`, `man/ggchangepoint-package.Rd` and the four vignettes, with
the offending phrase quoted -- `"ggchangepoint-package.Rd: 'reaches thirty
methods' but the registry has 50"`.

## 336. Standing vignette check

| | |
|---|---|
| bibliography entries / cited / broken | 70 / 63 / **0** |
| uncited (all `\insertRef`'d from Rd) | 7 |
| `\insertRef` keys in `R/` / unresolved | 58 / **0** |
| available / planned methods, against the guarded prose | 50 / 5 |

## 337. What this pass changes

| § | finding | measured how |
|---|---|---|
| 333 | no rotting own-version literal; every `0.5.0` in code is a historical comment | grep over `R/`, `tests/`, `inst/`, `.github/` |
| 334 | **`DESCRIPTION` and the package Rd state the method total as a literal**, uncomputable and unguarded | grep, then the registry |
| 335 | the extended guard failed three different ways, twice looking like a path problem | deliberate breakage per file |

New actions: none opened. One guard extended; no package behaviour changed.

**What this pass adds.** §335.3 is the sharpest version of a lesson this
document keeps relearning: **a guard that reports "pass" is making a claim,
and it is the claim least likely to be checked.** Three of the four guards
added in this loop needed a deliberate breakage to reveal that they could
not fail. The discipline is cheap -- break the input, watch the failure name
the input -- and it has now caught more defects in the guards than the
guards have caught in the package.
