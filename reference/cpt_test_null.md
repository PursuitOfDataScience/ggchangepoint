# Test for a change anywhere in a series

A test of the global null hypothesis "the series has no changepoint",
the question an empty detection raises. Nothing is located first, so
nothing is selected, and the p-value means what it says.

## Usage

``` r
cpt_test_null(x, method = c("cusum", "pettitt", "supF"), index = NULL)
```

## Arguments

- x:

  A numeric series, or a `ggcpt` fit (its series is used).

- method:

  Which test:

  `"cusum"`

  :   (the default) the CUSUM test for a change in mean: the largest
      standardised cumulative-sum deviation, referred to the Kolmogorov
      distribution of the supremum of a Brownian bridge. The noise scale
      is estimated from first differences, which a change barely
      affects.

  `"pettitt"`

  :   Pettitt's rank test, distribution-free, for a shift in location
      (approximate p-value).

  `"supF"`

  :   Andrews' sup-F structural change test for a change in mean, with
      15% trimming, through strucchange.

- index:

  Optional time index for a bare series.

## Value

A tibble with one row: `method`, `statistic`, `p_value`, `location`
(where the statistic peaks: the most likely changepoint, reported for
orientation only), `n` and `selection_adjusted` (`TRUE`).

## Assumptions

All three assume independent observations. Under positive
autocorrelation they reject too often, which is the same failure the
detectors share; see
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md).

## See also

[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md)
for what an empty answer could have detected,
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)
for a pre-specified date.

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md)

## Examples

``` r
set.seed(1)
cpt_test_null(rnorm(200))
#> # A tibble: 1 × 6
#>   method                   statistic p_value location     n selection_adjusted
#>   <chr>                        <dbl>   <dbl>    <int> <int> <lgl>             
#> 1 CUSUM (Kolmogorov limit)     0.812   0.525       96   200 TRUE              
shifted <- c(rnorm(100), rnorm(100, 0.6))
cpt_test_null(shifted, method = "pettitt")
#> # A tibble: 1 × 6
#>   method            statistic  p_value location     n selection_adjusted
#>   <chr>                 <dbl>    <dbl>    <int> <int> <lgl>             
#> 1 Pettitt rank test      3578 0.000142       94   200 TRUE              
cpt_test_null(shifted, method = "supF")
#> # A tibble: 1 × 6
#>   method                     statistic p_value location     n selection_adjusted
#>   <chr>                          <dbl>   <dbl>    <int> <int> <lgl>             
#> 1 sup-F (Andrews, 15% trimm…      20.6 1.68e-4       94   200 TRUE              
```
