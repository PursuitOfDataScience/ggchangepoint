# A stateful sequential changepoint monitor

Creates a detector that consumes observations as they arrive and raises
alarms, rather than segmenting a series that is already complete. Feed
it with
[`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md),
read its alarm log with
[`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md),
and score it with
[`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
— because for an online method "did you find the location?" is the wrong
question and "how long did you take, and how often do you false-alarm?"
is the right one.

## Usage

``` r
cpt_monitor(
  method = c("edetector", "cpm", "ocd"),
  baseline = NULL,
  alpha = 0.01,
  arl0 = 500,
  cpm_type = "Mann-Whitney",
  patience = 5000,
  deltas = c(0.5, 1, 2),
  reset = TRUE,
  relearn = 20,
  thresh = "MC",
  mc_reps = 100,
  ...
)

# S3 method for class 'ggcpt_monitor'
tidy(x, ...)

# S3 method for class 'ggcpt_monitor'
print(x, ...)

# S3 method for class 'ggcpt_monitor'
autoplot(object, plot_type = c("timeline", "statistic", "runlength"), ...)
```

## Arguments

- method:

  Which sequential detector:

  `"edetector"`

  :   (default) a mixture Shiryaev–Roberts e-detector; see the section
      below.

  `"cpm"`

  :   cpm's sequential change-point model, tuned by `ARL0`.

  `"ocd"`

  :   ocd's high-dimensional online detector. **Multivariate only** – it
      tracks a projection of the whole vector and needs at least two
      coordinates, so it refuses a single series rather than falling
      back to a univariate statistic.

- baseline:

  A numeric vector (or, for `"ocd"`, a matrix with rows as time points)
  of pre-change training data used to estimate the in-control mean and
  scale. Required for `"edetector"` and `"ocd"`; optional for `"cpm"`,
  which has its own start-up period.

- alpha:

  Target false-alarm probability for `"edetector"`: the average run
  length under the null is at least `1 / alpha`, by optional stopping on
  the martingale \\M_t - t\\ (see Details). Defaults to `0.01`.

- arl0:

  Target in-control average run length for `"cpm"`. Defaults to `500`.

- cpm_type:

  Statistic for `"cpm"`. Defaults to `"Mann-Whitney"`.

- patience:

  Target patience (average run length) for `"ocd"`. Defaults to `5000`.

- deltas:

  Shift sizes, in baseline standard deviations, mixed over by
  `"edetector"`. Defaults to `c(0.5, 1, 2)`, each taken in both
  directions. The mixture is a uniform average, so adding shifts costs
  power at the ones already there rather than inflating the false-alarm
  rate; mix over the range you think the change could fall in, not over
  everything.

- reset:

  After an alarm, restart the detector (`TRUE`, the default) or leave it
  running? Restarting is what makes a monitor report *repeated* changes
  rather than latching on the first one.

- relearn:

  How many observations after an alarm are used to re-learn the
  in-control baseline, during which no further alarm can fire. Defaults
  to `20`. This matters more than it looks: a real change is
  *persistent*, so a detector that restarts against the stale pre-change
  baseline alarms again on the very next observation and keeps alarming
  for the rest of the series — the monitor reports one change as
  hundreds, and
  [`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md)
  then counts them all as false alarms. Set `relearn = 0` to switch the
  behaviour off and see every threshold crossing.

- thresh:

  Threshold rule for `"ocd"`: `"MC"` (default) calibrates by Monte Carlo
  against `patience`, or supply a numeric vector of three thresholds.
  The Monte Carlo calibration is the expensive part of building an
  `"ocd"` monitor — a minute or more at the default `patience` — so pass
  thresholds directly when you already have them, or lower `mc_reps`
  while exploring.

- mc_reps:

  Monte Carlo repetitions for the `"ocd"` threshold.

- ...:

  Additional arguments passed to the engine's constructor.

- x:

  A `ggcpt_monitor` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- object:

  A `ggcpt_monitor` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- plot_type:

  `"timeline"` (the monitored series with the alarms marked),
  `"statistic"` (the running detection statistic against its threshold)
  or `"runlength"` (the gaps between alarms, which estimate the run
  length). For a multivariate monitor the timeline draws the first
  coordinate — the alarms are shared, so the rules are right whichever
  coordinate is shown, but the line is one of several.

## Value

A `ggcpt_monitor` object.

## The e-detector, and why it is implemented rather than wrapped

Shin, Ramdas and Rinaldo (2023) give a nonparametric sequential
framework with non-asymptotic control of the average run length, and it
has no R implementation. The construction used here is the mixture
Shiryaev–Roberts e-detector for a sub-Gaussian shift. For each candidate
shift \\\delta\\ the increment is the likelihood ratio \\e_t^{(\delta)}
= \exp(\delta (X_t - \mu_0)/\sigma^2 - \delta^2/(2\sigma^2))\\, which
has unit mean under the null; the running statistic is \\R_t^{(\delta)}
= (1 + R\_{t-1}^{(\delta)}) e_t^{(\delta)}\\; the shifts are combined by
*averaging*, \\M_t = K^{-1} \sum\_\delta R_t^{(\delta)}\\; and an alarm
is raised the first time \\M_t \ge 1/\alpha\\.

The averaging is not a detail. Under the null \\M_t - t\\ is a mean-zero
martingale, so optional stopping at the alarm time \\\tau\\ gives
\\E\_\infty\[\tau\] = E\_\infty\[M\_\tau\] \ge 1/\alpha\\: a
finite-sample lower bound on the in-control average run length, with no
asymptotics and no calibration run. A convex combination of e-detectors
is an e-detector; a *maximum* of them is not, and taking one silently
multiplies the false-alarm rate by roughly the number of shifts mixed
over. Every other detector in this package wraps published, separately
maintained code; this one does not, and is labelled as such wherever it
appears.

## References

Shin J, Ramdas A, Rinaldo A (2023). “E-detectors: a nonparametric
framework for sequential change detection.” *The New England Journal of
Statistics in Data Science*, **1**(2), 229–260.
[doi:10.51387/23-NEJSDS49](https://doi.org/10.51387/23-NEJSDS49) .

## See also

[`cpt_update()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_update.md),
[`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md),
[`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md),
[`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md).

## Examples

``` r
set.seed(2026)
mon <- cpt_monitor("edetector", baseline = rnorm(100))
mon <- cpt_update(mon, rnorm(50))          # still in control
mon <- cpt_update(mon, rnorm(50, 3))       # a change arrives
alarms(mon)
#> # A tibble: 1 × 3
#>    time statistic threshold
#>   <int>     <dbl>     <dbl>
#> 1    51      134.       100
```
