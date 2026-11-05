# How often do the confidence intervals cover? A Monte Carlo check

Simulates series with one changepoint of known location, detects with
`method`, computes
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
by each route in `routes`, and reports how often the interval contains
the true location and how wide it was. This is the measurement behind
the coverage statements in
[`?cpt_confint`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md),
and the way to check them for your own series length, jump size and
noise before relying on an interval.

## Usage

``` r
cpt_confint_coverage(
  method = "pelt",
  routes = c("conformal", "bootstrap"),
  n = 200,
  jump = 2,
  noise = c("gauss", "t", "ar1"),
  n_sim = 100,
  level = 0.95,
  B = 100,
  seed = NULL,
  ...
)

# S3 method for class 'ggcpt_coverage'
print(x, ...)

# S3 method for class 'ggcpt_coverage'
tidy(x, ...)

# S3 method for class 'ggcpt_coverage'
autoplot(object, ...)
```

## Arguments

- method:

  Detection method passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  Defaults to `"pelt"`.

- routes:

  Which
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  routes to measure: any of `"conformal"`, `"bootstrap"`, `"native"`,
  `"posterior"` and `"nsp"`. Defaults to `c("conformal", "bootstrap")`.

- n:

  Series length. Defaults to `200`.

- jump:

  Size of the mean shift, in noise standard deviations. Defaults to `2`.

- noise:

  Noise model, as in
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md):
  `"gauss"` (default), `"t"` or `"ar1"`.

- n_sim:

  Number of simulated series. Defaults to `100`.

- level:

  Nominal level. Defaults to `0.95`.

- B:

  Replicates (bootstrap) or permutations (conformal) per interval.
  Defaults to `100`.

- seed:

  Optional seed, scoped to this call.

- ...:

  Passed to
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  (`rho`, `df`, ...).

- x:

  A `ggcpt_coverage` object.

- object:

  A `ggcpt_coverage` object.

## Value

A `ggcpt_coverage` tibble with one row per route: `method`, `route`,
`n_sim`, `n_detected`, `n_failed`, `coverage`, `coverage_se` (its Monte
Carlo standard error), `mean_width`, `level`.
[`print()`](https://rdrr.io/r/base/print.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(coverage with a two-standard-error bar against the nominal level) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) work on it.

## Details

The changepoint sits at `floor(n / 2)`. Coverage is computed over the
replicates in which the detector reported at least one changepoint,
taking the one nearest the truth, so it is coverage *given a detection*;
`n_detected` says how many replicates that is. A route that fails on a
replicate (an engine that supplies no native interval, say) is counted
in `n_failed` rather than as a miss.

## See also

[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md),
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md).

Other inference:
[`cpt_abruptness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_abruptness.md),
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_k_weights()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_k_weights.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_screen()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_screen.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
cov <- cpt_confint_coverage(n_sim = 6, n = 80, B = 20, seed = 1)
cov
#> ggcpt_coverage (realised coverage of cpt_confint())
#>   Design:             n = 80, one change of 2 sd at n/2, gauss noise
#> 
#> # A tibble: 2 × 9
#>   method route   n_sim n_detected n_failed coverage coverage_se mean_width level
#>   <chr>  <chr>   <int>      <int>    <int>    <dbl>       <dbl>      <dbl> <dbl>
#> 1 pelt   confor…     6          6        0        1           0          3  0.95
#> 2 pelt   bootst…     6          6        0        1           0          5  0.95
ggplot2::autoplot(cov)
```
