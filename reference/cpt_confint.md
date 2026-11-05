# Confidence intervals for changepoint locations

Answers "where could this changepoint be?" for any result, and says
which of five routes it used. `show_ci = TRUE` in
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
works only for the handful of engines that ship intervals of their own;
this generic covers the rest, and (because the four routes mean
genuinely different things) reports the provenance in a `source` column
rather than presenting them as interchangeable.

## Usage

``` r
cpt_confint(
  object,
  level = 0.95,
  method = c("auto", "native", "nsp", "bootstrap", "posterior", "conformal"),
  B = 200,
  seed = NULL,
  ...
)
```

## Arguments

- object:

  A `ggcpt` object.

- level:

  Confidence/credible level. Defaults to `0.95`. Honoured by the routes
  that compute an interval (`"bootstrap"`, `"posterior"` and `"nsp"`)
  and **ignored by** `"native"`, which reports the interval the engine
  already computed at whatever level it was asked for. Note that
  `method = "auto"` resolves to `"native"` whenever the engine supplied
  one, so an explicit `level` can go unused there too: it is reported in
  the `level` column either way, and supplying a level the answer does
  not carry now warns rather than passing silently. To choose the level
  yourself, name a computing route.

- method:

  Which route to use:

  `"auto"`

  :   (default) native if the engine supplied intervals, else posterior
      if it supplied one, else conformal where the change is one between
      exchangeable segments (a change in mean, variance, both, or
      distribution), else bootstrap.

  `"native"`

  :   the engine's own `ci_lower`/`ci_upper` (SMUCE/HSMUCE simultaneous
      confidence sets, strucchange break-date intervals, segmented
      breakpoint intervals, mcp posterior intervals, bfast break
      confidence intervals, and `taylor`'s bootstrap confidence limits).
      The engines that supply them are the ones
      [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
      marks in its `ci` column; `nsp` is marked there too but is
      reported under its own provenance below, because its regions are
      not intervals around an estimate.

  `"nsp"`

  :   Narrowest Significance Pursuit regions computed on the same series
      and matched to the changepoints. These are *not* intervals around
      an estimate (see
      [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)),
      but they are the strongest guarantee available, so the mapping is
      reported as `source = "nsp_region"` and a changepoint in no region
      gets `NA`.

  `"bootstrap"`

  :   within-segment residual resampling (the
      [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
      scheme), re-running the detector and taking quantiles of the
      re-detected location. Model-agnostic and available for every
      engine; it measures the sampling variability of the *procedure*,
      conditional on the fitted segmentation, and it is not exact.

  `"conformal"`

  :   a distribution-free confidence set (CONCH, Hore and Ramdas 2026).
      Each changepoint is examined in its window between the
      neighbouring changepoints; every candidate split of the window is
      tested by permuting the observations within each side of it, and
      the candidates that survive at `1 - level` form a set that
      contains the true location with probability at least `level`,
      whatever the distribution of the data, provided the observations
      within each segment are exchangeable (independent and identically
      distributed is the usual case) and the window holds one change. It
      needs no re-run of the detector, so it serves results built with
      [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
      and registered methods too. The set need not be contiguous:
      `ci_lower` and `ci_upper` are its hull and `set_size` counts its
      members, and the p-value of every candidate is attached as the
      `"profile"` attribute. The score matches what changed: a mean
      shift, a variance change, both, or a rank statistic for a change
      in distribution; summed over coordinates for a multivariate
      result. `B` sets the number of permutations per candidate.

  `"posterior"`

  :   a credible interval from the engine's posterior
      changepoint-probability profile (`bcp`, `beast`): the narrowest
      *contiguous* set of positions around the estimate holding `level`
      of the posterior changepoint mass in that changepoint's window,
      where the window is bounded by the neighbouring changepoints so
      two of them cannot claim the same mass twice.

      Expect these to be wide, and read the width as a statement about
      the profile rather than about the location. Both supplying engines
      put roughly two-thirds of the window's mass at the estimate itself
      and spread the remaining third as a thin floor across every other
      position, so reaching a high `level` means swallowing that floor.
      Measured on a 200-point series with one clean change:
      `level = 0.5` gives a width of 0 (the mode alone holds more than
      half), `0.8` gives 72 (bcp) and 91 (beast), and `0.95` gives 166
      and 187, which is 83% and 94% of the series. The requested level
      *is* delivered in each case; what a wide interval says is that the
      posterior did not localise the change, not that the location is
      uncertain by that much. `cpt_confint()` warns when an interval
      covers more than half its window, for exactly that reason.

- B:

  Bootstrap replicates for `method = "bootstrap"`, and permutations per
  candidate split for `method = "conformal"`. Defaults to `200`.

- seed:

  Optional seed (bootstrap and NSP are both random). The seed is scoped
  to this call: `.Random.seed` is saved and restored, so a seeded call
  inside a simulation loop does not pin the loop's own stream.

- ...:

  Passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  on the bootstrap replicates, or to
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md).

## Value

A tibble with one row per changepoint: `cp`, `ci_lower`, `ci_upper`,
`level`, `source`, plus `cp_index`/`ci_lower_index`/`ci_upper_index` on
the original scale when the result carries a time index, and
`n_replicates` for `method = "bootstrap"` (how many replicates actually
contributed a draw for that changepoint), and `set_size` for
`method = "conformal"` (how many locations the confidence set holds; an
empty set has `NA` bounds and warns, because no single change between
exchangeable segments fits that window). `method = "nsp"` returns `NA`
bounds for a changepoint that falls in no region, which is a finding
rather than a failure.

## How well these cover

Measured over 120 replicates on a 200-point series with one changepoint
at 100 and a jump of three standard deviations, at a nominal level of
0.95: `"bootstrap"` on `pelt` covered 0.992 of the time with a mean
width of 2.2; strucchange's native intervals covered 1.000 at width 4.4;
stepR's (`smuce`) covered 0.992 at width 4.6; and `"posterior"` on bcp
covered 1.000 at width 157. Every route is *conservative* (none
under-covers), and the width is what separates them. The two native
routes and the bootstrap are the ones to quote; see the note on
`"posterior"` above for why its interval is so much wider.

The conformal route was measured with
[`cpt_confint_coverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint_coverage.md)
on the same design (300 replicates per cell, `pelt`, level 0.95). For a
jump of three standard deviations it covered 0.960 at a mean width of
0.5 (the bootstrap: 0.973 at 2.0); for a jump of one standard deviation
0.970 at 18.6, where the bootstrap fell short of the level at 0.926
(width 25.8); under \\t_3\\ noise with a jump of two, 0.963 at 3.5
(bootstrap 0.943 at 4.4). Its guarantee assumes exchangeable
observations within each segment, and under AR(1) noise with \\\rho =
0.3\\ it covered 0.920 (bootstrap 0.887): dependent noise shortens every
interval here, and
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)
checks for it.

## References

Hore R, Ramdas A (2026). “Conformal changepoint localization.” *arXiv
preprint arXiv:2602.06267*.

## See also

[`cpt_confint_coverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint_coverage.md),
which measures how often each route covers;
[`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md),
[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(80), rnorm(80, 4))
fit <- cpt_detect(x, method = "pelt")
cpt_confint(fit, method = "conformal", B = 50, seed = 1)
#> # A tibble: 1 × 6
#>      cp ci_lower ci_upper level source    set_size
#>   <int>    <int>    <int> <dbl> <chr>        <int>
#> 1    80       80       80  0.95 conformal        1
cpt_confint(fit, method = "bootstrap", B = 25, seed = 1)
#> # A tibble: 1 × 6
#>      cp ci_lower ci_upper level source    n_replicates
#>   <int>    <int>    <int> <dbl> <chr>            <int>
#> 1    80       80       80  0.95 bootstrap           25
```
