# Confidence intervals for changepoint locations

Answers "where could this changepoint be?" for any result, and says
which of four routes it used. `show_ci = TRUE` in
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
works only for the handful of engines that ship intervals of their own;
this generic covers the rest, and — because the four routes mean
genuinely different things — reports the provenance in a `source` column
rather than presenting them as interchangeable.

## Usage

``` r
cpt_confint(
  object,
  level = 0.95,
  method = c("auto", "native", "nsp", "bootstrap", "posterior"),
  B = 200,
  seed = NULL,
  ...
)
```

## Arguments

- object:

  A `ggcpt` object.

- level:

  Confidence/credible level. Defaults to `0.95`. Ignored by
  `method = "native"`, which reports the interval the engine already
  computed at whatever level it was asked for.

- method:

  Which route to use:

  `"auto"`

  :   (default) native if the engine supplied intervals, else posterior
      if it supplied one, else bootstrap.

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
      an estimate — see
      [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
      — but they are the strongest guarantee available, so the mapping
      is reported as `source = "nsp_region"` and a changepoint in no
      region gets `NA`.

  `"bootstrap"`

  :   within-segment residual resampling (the
      [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
      scheme), re-running the detector and taking quantiles of the
      re-detected location. Model-agnostic and available for every
      engine; it measures the sampling variability of the *procedure*,
      conditional on the fitted segmentation, and it is not exact.

  `"posterior"`

  :   a credible interval from the engine's posterior
      changepoint-probability profile (`bcp`, `beast`).

- B:

  Bootstrap replicates for `method = "bootstrap"`. Defaults to `200`.

- seed:

  Optional seed (bootstrap and NSP are both random).

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
contributed a draw for that changepoint). `method = "nsp"` returns `NA`
bounds for a changepoint that falls in no region, which is a finding
rather than a failure.

## See also

[`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md),
[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(80), rnorm(80, 4))
fit <- cpt_detect(x, method = "pelt")
cpt_confint(fit, method = "bootstrap", B = 25, seed = 1)
#> # A tibble: 1 × 6
#>      cp ci_lower ci_upper level source    n_replicates
#>   <int>    <int>    <int> <dbl> <chr>            <int>
#> 1    80       80       80  0.95 bootstrap           25
```
