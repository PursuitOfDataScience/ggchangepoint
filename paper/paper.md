---
title: "ggchangepoint: one interface to fifty changepoint detectors, and measurements of how each behaves"
tags:
  - R
  - changepoint detection
  - structural breaks
  - time series
  - ggplot2
authors:
  - name: Youzhi Yu
    affiliation: 1
affiliations:
  - name: University of Chicago, United States
    index: 1
date: 26 September 2026
bibliography: paper.bib
---

# Summary

A changepoint is a time at which the process generating a series changes:
its level, its spread, its slope, its distribution, or the relationship
between a response and its covariates. R has more changepoint software
than any other language, spread over dozens of packages, and each takes
its own input, returns its own object, reports locations in its own
convention and draws its own plot. `ggchangepoint` puts fifty of those
detectors behind one call, `cpt_detect()`, returns every answer as the
same tidy result, and draws it with `ggplot2` [@wickham2016ggplot2]. It
then answers the questions that follow a detection in the same grammar:
how big the change was, whether anything changed at a date fixed in
advance, whether the result's assumptions hold for the series, and which
method suits the problem.

# Statement of need

Changepoint packages disagree on the most basic fact they report. On the
annual flow of the Nile, `changepoint` [@killick2014changepoint] and
`strucchange` [@zeileis2002strucchange] report the change as observation
28, the last year of the old regime; `ecp` [@james2014ecp] as 29, the
first year of the new one; `bcp` [@erdman2007bcp] as a posterior
probability for each year; and `segmented` [@muggeo2008segmented] as a
point on the covariate's continuous scale. An analyst comparing methods,
or switching from one to another, has to know all of this. `ggchangepoint`
translates every engine to one convention, verified by a test that runs
each of the fifty on an unmistakable step (the test found two wrappers
off by one), and records it on the result.

The larger need is for honesty about behaviour. A changepoint analysis
rarely fails loudly: the engine returns, the plot looks plausible, and the
answer is wrong. The commonest real error is autocorrelated noise read as
a sequence of changes; others are penalties on the wrong scale, Gaussian
costs on counts or binary data, and p-values computed at locations the
data chose. None of these raises an error in the engines themselves.

# Measured rather than asserted

`ggchangepoint` measures its engines and ships the measurements as data
sets: the runtime of every engine at up to a million observations, whether
each answer is invariant to rescaling, shifting and time reversal, false
positives and power under independent, heavy-tailed, autocorrelated and
heteroscedastic noise, behaviour on Bernoulli, Poisson and proportion
data, and false alarms on pure noise at scale. The registry's measured
columns and the method recommender, `cpt_recommend()`, are derived from
those tables rather than from hand-maintained lists, and the warnings the
package raises (a scale-sensitive engine handed wide noise, count or 0/1
data under a Gaussian cost, an implausible number of changepoints) exist
because a measurement showed the failure first. Under AR(1) noise with a
lag-one correlation of 0.7, for example, 21 of 35 engines at their
defaults reported more spurious changepoints than real ones, up to 36 per
series of 300 observations, while DeCAFS [@romano2022decafs], which models
the dependence, reported 0.3.

A calibration suite checks the package's own inferential promises in the
same way: the coverage of location intervals from `strucchange`, SMUCE
[@frick2014smuce] and the residual bootstrap, the size of the tests at a
date fixed in advance and of the tests for a change anywhere, and the
in-control run length of the sequential monitors, including the
e-detector [@shin2023edetectors]. One row is there to show what a
selection-adjusted p-value means: a test at a location the data chose
rejects about half the time at a nominal 5% on pure noise.

# Design

Every engine is a row in a declarative registry that records what it can
detect, what it returns (intervals, posteriors, a detector statistic, a
solution path), the distribution families it can fit, its
modelling-choice arguments and their legal values, and its measured
behaviour. `cpt_detect()` validates a request against the registry before
any engine sees it, so a misspelt argument or an unsupported combination
is refused by name with the nearest valid value, not swallowed by an
engine's `...`. Errors and warnings are classed conditions, results record
the engine versions that made them, and a result can be written to JSON
under a versioned schema and read back. A formula interface reaches the
regression-break engines, `strucchange` and `segmented`, whose CRAN
downloads exceed those of every other wired engine combined, and returns
per-segment coefficient tables. Detectors the
package does not wrap, including ones written in other languages, join the
same grammar through `cpt_register_method()`.

The Python package `ruptures` [@truong2020selective] plays a similar
unifying role for offline detection in Python; `ggchangepoint` differs in
wrapping existing, published implementations rather than reimplementing
them, and in measuring and reporting how each behaves. Its benchmarking
tools score methods against the Turing Change Point Dataset
[@vandenburg2020evaluation].

# References
