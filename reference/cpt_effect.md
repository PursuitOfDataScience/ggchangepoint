# Effect size at each changepoint

The size of every change a fit reports, in the data's own units and
standardised: the level before and after, their difference with an
interval, the difference in noise standard deviations, the percentage
change, and for count and waiting-time data the rate or hazard ratio.

## Usage

``` r
cpt_effect(fit, level = 0.95, method = c("naive", "split"), seed = NULL, ...)

# S3 method for class 'ggcpt_effect'
print(x, ...)

# S3 method for class 'ggcpt_effect'
autoplot(object, ...)
```

## Arguments

- fit:

  A `ggcpt` object.

- level:

  Confidence level for the intervals. Defaults to `0.95`.

- method:

  `"naive"` (the default) measures each effect on the data that located
  the change. `"split"` locates the changepoints on the odd-numbered
  observations and measures their effects on the even-numbered ones,
  which removes the winner's curse at the price of locating on half the
  data (see below).

- seed:

  Optional seed for `method = "split"`, scoped to this call.

- ...:

  Further arguments for the re-run `method = "split"` makes (overriding
  what is recovered from the fit).

- x:

  A `ggcpt_effect` object.

- object:

  A `ggcpt_effect` object.

## Value

A `ggcpt_effect` tibble with one row per changepoint (per changepoint
and coordinate for a multivariate fit, per changepoint and term for a
regression fit): `cp` (and `cp_index`), `before`, `after`, `n_before`,
`n_after`, `delta`, `delta_lower`, `delta_upper` (a Welch interval),
`delta_std` (the change in noise standard deviations, the unit
`cpt_detect(min_effect = )` filters on), `pct_change`, `sd_before`,
`sd_after`, `sd_ratio`, the family-specific ratio where it applies
(`rate_ratio` for Poisson counts, `odds_ratio` for binary data,
`hazard_ratio` for exponential waiting times, each with an interval),
`selection_adjusted` and `method`. With
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## The winner's curse

A change measured at a location the same data chose is biased upward:
the detector put the boundary where the two sides differ most, so the
difference it reports is the largest the noise allowed. The bias is
worst for the marginal detections, which are exactly the ones whose size
matters. `method = "naive"` rows carry `selection_adjusted = FALSE` for
that reason. `method = "split"` is honest about the size (the measuring
half never influenced the locations) and less precise about the location
(the locating half has half the data), and it assumes the noise is
independent from one observation to the next, since the two halves are
interleaved.

## See also

[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)
for a change at a date fixed in advance,
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)'s
`min_effect`.

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(100, 10), rnorm(100, 12)), method = "pelt")
cpt_effect(fit)
#> ggcpt_effect (method: pelt, naive)
#> Measured where the same data located each change, so the sizes are biased upward
#> (selection_adjusted = FALSE). method = "split" measures on held-out observations. 
#> 
#> # A tibble: 1 × 15
#>      cp before after n_before n_after delta delta_lower delta_upper delta_std
#>   <int>  <dbl> <dbl>    <int>   <int> <dbl>       <dbl>       <dbl>     <dbl>
#> 1   100   10.1  12.0      100     100  1.85        1.59        2.11      1.92
#> # ℹ 6 more variables: pct_change <dbl>, sd_before <dbl>, sd_after <dbl>,
#> #   sd_ratio <dbl>, selection_adjusted <lgl>, method <chr>

counts <- c(rpois(100, 4), rpois(100, 8))
cpt_effect(cpt_detect(counts, method = "pelt", family = "poisson"))
#> ggcpt_effect (method: pelt, naive)
#> Measured where the same data located each change, so the sizes are biased upward
#> (selection_adjusted = FALSE). method = "split" measures on held-out observations. 
#> 
#> # A tibble: 1 × 18
#>      cp before after n_before n_after delta delta_lower delta_upper delta_std
#>   <int>  <dbl> <dbl>    <int>   <int> <dbl>       <dbl>       <dbl>     <dbl>
#> 1   100   4.06   8.3      100     100  4.24        3.55        4.93      2.02
#> # ℹ 9 more variables: pct_change <dbl>, sd_before <dbl>, sd_after <dbl>,
#> #   sd_ratio <dbl>, rate_ratio <dbl>, rate_ratio_lower <dbl>,
#> #   rate_ratio_upper <dbl>, selection_adjusted <lgl>, method <chr>
```
