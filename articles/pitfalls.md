# Ten Ways to Get a Changepoint Wrong

A changepoint analysis rarely fails loudly. The engine returns, the plot
looks plausible, and the answer is wrong. Each section below is one way
that happens: a few lines that produce the wrong answer, why, and the
call that avoids it. Every number on this page is computed when the page
is built, so none of them is a claim you have to take on trust.

## 1. Autocorrelated noise read as a sequence of changes

The commonest real-world error. Noise that remembers its past wanders,
and a detector that assumes independent noise reads each excursion as a
new level.

``` r

set.seed(1)
ar <- as.numeric(arima.sim(list(ar = 0.8), 300))   # no change anywhere
fit <- quiet(cpt_detect(ar, method = "pelt"))
nrow(fit$changepoints)
#> [1] 7
cpt_assumptions(fit)
#> ggcpt_assumptions (method: pelt)
#>  ! residual_dependence       Ljung-Box at lag 10; lag-1 autocorrelation 0.54
#>       The residuals are autocorrelated, the commonest cause of spurious
#>       changepoints. cpt_detect(x, method = "kcp") (0.3 spurious, 1.9 of 2
#>       real changes found); cpt_detect(x, method = "bfast") (1.0 spurious,
#>       1.6 of 2 real changes found); cpt_detect(x, method = "nsp", variant =
#>       "selfnorm") (0.8 spurious, 1.2 of 2 real changes found)
#>    scale_sensitivity         noise sd 0.696; this engine's cost assumes unit noise
#>    expected_false_positives  measured on pure noise at n = 1,000 (50 replicates)
#>  ! count_plausibility        7 changepoints in 300 observations (2.33 per hundred)
#>       More than one changepoint per hundred observations is more often a
#>       penalty on the wrong scale or dependent noise than a finding; check
#>       the two rows above.
#>    data_type                 looks continuous; family gaussian
```

**Avoid it:** check before you believe the count.
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)
tests the residuals and names the measured alternatives; engines that
model the dependence answer differently:

``` r

nrow(quiet(cpt_detect(ar, method = "decafs"))$changepoints)
#> [1] 0
```

`cpt_robustness(fit)` re-runs the detector under its other noise models
and reports which changepoints survive, which is the check that touches
the cause: resampling
([`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md))
and other algorithms
([`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md))
reproduce an artifact of the noise path.

## 2. A penalty on the wrong scale

`pelt`, `binseg`, `segneigh` and `fpop` compare the penalty with a raw
segment cost that assumes the noise has standard deviation 1. Annual
Nile flows have a standard deviation near 170.

``` r

nrow(quiet(cpt_detect(Nile, method = "pelt"))$changepoints)
#> [1] 42
nrow(cpt_detect(as.numeric(scale(Nile)), method = "pelt")$changepoints)
#> [1] 1
```

**Avoid it:** the package warns when a scale-sensitive engine is handed
noise far from unit scale. Standardise the series, use
`change_in = "meanvar"`, or a method whose answer does not depend on the
units (`subset(cpt_methods(), scale_invariant)` lists the ones
measured).

## 3. Counts and 0/1 outcomes under a Gaussian cost

A binary series has two values, and a Gaussian cost that estimates the
noise from the data sees a perfect fit in every run of equal values.

``` r

set.seed(3)
b <- c(rbinom(150, 1, 0.2), rbinom(150, 1, 0.6))   # one change, at 150
nrow(quiet(cpt_detect(b, method = "smuce"))$changepoints)
#> [1] 103
cpt_detect(b, method = "smuce", family = "binomial")$changepoints
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   151        0      137      162
```

Measured across the engines, Gaussian costs raise about 247 times as
many false positives on Bernoulli data as on Gaussian data
(`cpt_data_types`).

**Avoid it:** say what the data are. `family = "poisson"`, `"binomial"`
or `"exponential"` fits a cost written for them
([`cpt_families()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_families.md)
lists which methods can), and the package warns when a series looks like
counts or 0/1 data and no family was chosen.

## 4. An abrupt detector on a gradual change

A detector of level shifts, given a trend, approximates it with a
staircase.

``` r

set.seed(4)
trend <- seq(0, 6, length.out = 300) + rnorm(300)   # a line, no change
nrow(cpt_detect(trend, method = "pelt")$changepoints)
#> [1] 3
```

**Avoid it:** ask for the change you mean. A change in slope is
`change_in = "slope"`:

``` r

nrow(quiet(cpt_detect(trend, method = "cpop", change_in = "slope"))$changepoints)
#> [1] 0
```

## 5. A value that is not a measurement, coerced into one

A factor’s level codes, a timestamp’s seconds since 1970 and a survival
object’s two columns all convert to numbers without complaint, and each
then has a “mean shift” that means nothing. The package refuses them:

``` r

stamps <- as.POSIXct("2026-01-01", tz = "UTC") + 3600 * (0:99)
tryCatch(cpt_detect(stamps), error = function(e) conditionMessage(e))
#> [1] "`x` holds dates or timestamps, which convert to an increasing count of days or seconds: every detector would report the calendar. If these are the times of events, count them per period and detect on the counts (with `family = \"poisson\"`); if they label observations, pass them as `index`."
```

**Avoid it:** pass the measurement as `x` and the time as `index`.

## 6. A p-value at a location the data chose

A test at a detected changepoint reuses the data that put the
changepoint there, so the difference it tests is the largest the noise
allowed. On pure noise, forced to report one changepoint:

``` r

set.seed(6)
p <- vapply(1:200, function(i) {
  f <- cpt_detect(rnorm(200), method = "amoc", penalty = "None")
  quiet(cpt_test(f))$p_value[1]
}, numeric(1))
mean(p < 0.05, na.rm = TRUE)   # a valid 5% test would give about 0.05
#> [1] 0.5233161
```

**Avoid it:**
[`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md)
marks such rows `selection_adjusted = FALSE`. When the date was fixed in
advance,
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)
is an exact test; to ask whether anything changed at all,
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)
tests the global null without locating anything first. The calibration
article has the measured size of each.

## 7. The method chosen after seeing the answer

Fifty methods behind one call make it easy to run ten and report the one
that found a change. A method chosen for its answer carries no valid
p-value, and agreement between methods is not a correction: they share
engines, costs and assumptions.

**Avoid it:** choose before you look.
[`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
answers from what you know about the problem (dimension, noise, data
type, expected number of changes), not from the series, and returns the
call to run:

``` r

head(cpt_recommend(noise = "autocorrelated")[, c("method", "call", "hits",
                                                 "false_positives")], 3)
#> # A tibble: 3 × 4
#>   method call                                  hits false_positives
#>   <chr>  <chr>                                <dbl>           <dbl>
#> 1 kcp    "cpt_detect(x, method = \"kcp\")"      1.9             0.3
#> 2 binseg "cpt_detect(x, method = \"binseg\")"   1.7             1.3
#> 3 bfast  "cpt_detect(x, method = \"bfast\")"    1.6             1
```

## 8. The effect size read off where the change was found

The same selection, applied to the size of a change: the detector put
the boundary where the two sides differ most.

``` r

set.seed(8)
sizes <- t(vapply(1:100, function(i) {
  x <- c(rnorm(150), rnorm(150, 0.5))                # the true shift is 0.5
  f <- cpt_detect(x, method = "pelt")
  if (!nrow(f$changepoints)) return(c(NA, NA))
  naive <- cpt_effect(f)
  split <- cpt_effect(f, method = "split", seed = i)
  c(naive$delta[which.min(abs(naive$cp - 150))],
    if (nrow(split)) split$delta[which.min(abs(split$cp - 150))] else NA)
}, numeric(2)))
colMeans(sizes, na.rm = TRUE)
#> [1] 0.6342823 0.5178504
```

**Avoid it:** `cpt_effect(fit, method = "split")` locates on half the
observations and measures on the other half; the naive estimate is
labelled `selection_adjusted = FALSE`.

## 9. “No changepoints” read as “no change”

An empty answer means the series is stable, or the change was too small
to see at this length and noise level. Those call for opposite
conclusions.

``` r

set.seed(9)
x <- c(rnorm(100), rnorm(100, 0.3))   # a real, small shift
fit <- cpt_detect(x, method = "pelt")
nrow(fit$changepoints)
#> [1] 0
cpt_null_power(fit, n_sim = 30, seed = 1)
#> ggcpt_null_power (method: pelt, n = 200)
#> At this length and noise level (sd 1.08), a single shift of about 1.06 (0.98 sd) is
#> detected with power 0.8. A smaller change could be present and missed.
```

**Avoid it:** report the smallest shift the analysis could have found
([`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md)),
and test for a change anywhere
([`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md))
rather than inferring stability from silence.

## 10. Outliers read as changepoints

A single wild observation is a segment of length one to a mean-change
detector, so it arrives as a pair of changepoints.

``` r

set.seed(10)
o <- rnorm(200)
o[140] <- -9
cpt_detect(o, method = "pelt")$changepoints$cp
#> [1] 139 141
```

**Avoid it:** a robust cost changes in the median, not the mean:

``` r

cpt_detect(o, method = "binsegrcpp", family = "l1")$changepoints
#> # A tibble: 0 × 2
#> # ℹ 2 variables: cp <int>, cp_value <dbl>
```

## And two that are about the data, not the model

**Missing values.** Some engines drop a missing observation without a
word and report positions in the shortened series.
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
refuses them by default; `na_action = "omit"` detects on what was
observed and reports every location in the original positions:

``` r

set.seed(11)
y <- c(rnorm(50), NA, rnorm(50, 3))
cpt_detect(y, method = "pelt", na_action = "omit")$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50    0.220
```

**A monitor trained on a baseline that already changed.** An online
detector learns “in control” from the baseline it is given; a change
inside it teaches the wrong normal.
[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)
checks the baseline, for dependence and (for `cpm`) for changes inside
it, and says so:

``` r

set.seed(12)
tryCatch(cpt_monitor("cpm", baseline = c(rnorm(100), rnorm(100, 2))),
         ggchangepoint_assumption = function(w) conditionMessage(w))
#> [1] "cpm detected 1 change(s) inside `baseline`, so the baseline is not in control. The monitor was restarted after each, and only the stretch after the last one informs it; check the baseline, or build the monitor without one."
```

## Where these came from

Several of these were found in this package before they were found in
anyone’s analysis: the 0.5.0 and 0.6.0 audits measured each engine’s
behaviour on autocorrelated, heavy-tailed, heteroscedastic, binary and
count data (`cpt_noise_benchmark`, `cpt_data_types`), on rescaled and
reversed series (`cpt_invariances`) and on pure noise at scale
(`cpt_null_sizes`), and every warning quoted above exists because a
measurement showed the failure first.
