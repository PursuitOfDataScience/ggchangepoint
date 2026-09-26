# Calibration: What the Package Promises, and What It Delivers

Every interval in this package has a level, every test a size, and every
monitor a false-alarm rate. A number like that is a promise, and a
promise nobody measures can be wrong for years while every call
succeeds: before 0.5.0 the e-detector’s run-length bound was off by a
factor of two, and it was found only because someone measured it.

This page is the measurement. Each row is one guarantee the
documentation states, checked by simulation, with the realised value and
its Monte Carlo standard error. The table ships with the package as
`cpt_calibration` and is regenerated for each release by
`data-raw/calibration.R`.

## The table

``` r

show <- cal
rl <- show$kind == "run_length"
show$nominal <- ifelse(rl, as.character(round(show$nominal)),
                       pct(show$nominal))
show$realised <- ifelse(rl, as.character(round(show$realised)),
                        pct(show$realised))
show$mcse <- ifelse(rl, as.character(round(show$mcse)), pct(show$mcse))
show$kind <- sub("_", " ", show$kind)
knitr::kable(show[, c("call", "kind", "nominal", "realised", "mcse",
                      "reps")],
             col.names = c("Call", "Kind", "Promised", "Delivered",
                           "MC s.e.", "Replicates"))
```

| Call | Kind | Promised | Delivered | MC s.e. | Replicates |
|:---|:---|:---|:---|:---|---:|
| cpt_detect(x, method = “strucchange”) | coverage | 95.0% | 95.3% | 0.5% | 2000 |
| cpt_detect(x, method = “smuce”, alpha = 0.05) | coverage | 95.0% | 100.0% | 0.0% | 2000 |
| cpt_confint(cpt_detect(x), method = “bootstrap”) | coverage | 95.0% | 96.8% | 0.6% | 999 |
| cpt_test_at(x, when = 51) | size | 5.0% | 4.0% | 0.4% | 2000 |
| cpt_test_at(x, when = 51, family = “poisson”) | size | 5.0% | 3.8% | 0.4% | 2000 |
| cpt_test_at(x, when = 51, family = “binomial”) | size | 5.0% | 2.9% | 0.4% | 2000 |
| cpt_test_at(x, when = 51, family = “exponential”) | size | 5.0% | 5.9% | 0.5% | 2000 |
| cpt_test_at(x, when = 51, family = “l1”) | size | 5.0% | 4.9% | 0.5% | 2000 |
| cpt_test_at(x, when = 51, window = 5) | size | 5.0% | 4.8% | 0.5% | 2000 |
| cpt_test_null(x) | size | 5.0% | 4.9% | 0.5% | 2000 |
| cpt_test_null(x, method = “pettitt”) | size | 5.0% | 3.1% | 0.4% | 2000 |
| cpt_test_null(x, method = “supF”) | size | 5.0% | 5.0% | 0.5% | 2000 |
| cpt_test(cpt_detect(x, method = “amoc”, penalty = “None”)) | size | 5.0% | 54.6% | 1.1% | 1899 |
| cpt_monitor(“cpm”, arl0 = 500) | run length | 500 | 548 | 27 | 400 |
| cpt_monitor(“edetector”, baseline = b, alpha = 0.01) | run length | 100 | 186 | 11 | 400 |

## Reading it

**Location intervals.** Coverage is scored on one change of 1.5 noise
standard deviations after observation 100 of 200, conditional on a
changepoint landing within 20 positions of the truth. `strucchange`’s
interval covers 95.3% of the time and the residual bootstrap of
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
96.8%, both at a promised 95%. SMUCE’s jump intervals are simultaneous
over all changepoints at level `1 - alpha`, and conservative for a
single change: 100.0%.

**Tests of a date fixed in advance.** Every family of
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)
holds its size to within about two Monte Carlo standard errors. The
exact Poisson and Fisher tests are conservative, as tests of discrete
data are (3.8% and 2.9%), and the windowed search, which pays for
looking across eleven dates with a permutation distribution of the
maximum, delivers 4.8%.

**Tests for a change anywhere.** At `n = 200` the CUSUM test’s
Kolmogorov limit gives 4.9% and the sup-F test 5.0%; Pettitt’s
approximate p-value is conservative, 3.1%.

**A test at a location the data chose.** Forced to report one
changepoint on pure noise, the naive test at it rejects 54.6% of the
time at a nominal 5%. This is the row that explains
`selection_adjusted = FALSE`: the p-value is valid only for a location
chosen without the data, which is what
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)
is for.

**Monitors.** `cpm`’s in-control average run length is 548 against a
promised 500. The e-detector promises a lower bound, an average run
length of at least `1 / alpha`, and delivers 186 at `alpha = 0.01`.

## What is not here yet

[`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md)’s
choice of the number of changepoints,
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
against a reference implementation, and the intervals of the Bayesian
engines have no row, because they have not been measured. A guarantee
without a row is a claim, not a measurement.

## Regenerating it

``` r
# from the package source, with a pinned R
MEASURE_CORES=8 Rscript data-raw/calibration.R
Rscript data-raw/build_measured_data.R
```

Each cell draws from its own random stream, so the rows are independent
measurements.
