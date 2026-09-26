# Goodness of fit, segment by segment

The checks behind a segmentation, as numbers: for every segment, its
length, level and spread, whether its residuals look independent
(Ljung-Box) and Gaussian (Shapiro-Wilk), and a flag where the segment is
too short for either test to mean anything.

## Usage

``` r
cpt_gof(fit, lag = NULL)

# S3 method for class 'ggcpt_gof'
print(x, ...)
```

## Arguments

- fit:

  A `ggcpt` object.

- lag:

  Ljung-Box lag. Defaults to `min(10, n / 5)` per segment.

- x:

  A `ggcpt_gof` object.

- ...:

  Ignored.

## Value

A `ggcpt_gof` tibble with one row per segment (per segment and
coordinate for a multivariate fit): `seg_id`, `start`, `end`, `n`,
`mean`, `sd`, `acf1` (lag-1 autocorrelation of the residuals),
`ljung_box_p`, `shapiro_p`, and `too_short` (fewer than 12 observations,
where the tests are left `NA`). The whole-series Ljung-Box result is
attached as the `"overall"` attribute, and
[`print()`](https://rdrr.io/r/base/print.html) summarises what the
columns say.

## See also

`autoplot(fit, type = "diagnostics")` draws the same checks;
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)
reports them with the alternatives.

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(100), rnorm(100, 3, 2)), method = "pelt")
cpt_gof(fit)
#> ggcpt_gof (method: pelt, 5 segment rows)
#> Whole series: Ljung-Box p = 0.00075 at lag 10, lag-1 autocorrelation -0.18
#> Note: 1 segment(s) with non-Gaussian residuals
#> Note: segment spreads differ by a factor of 2.2 
#> 
#> # A tibble: 5 × 10
#>   seg_id start   end     n  mean    sd     acf1 ljung_box_p shapiro_p too_short
#>    <int> <int> <int> <int> <dbl> <dbl>    <dbl>       <dbl>     <dbl> <lgl>    
#> 1      1     1   100   100 0.109 0.898 -0.00365      0.809    0.988   FALSE    
#> 2      2   101   133    33 3.13  1.44  -0.117        0.505    0.00375 FALSE    
#> 3      3   134   159    26 1.91  1.97  -0.254        0.0687   0.120   FALSE    
#> 4      4   160   180    21 4.09  1.99   0.0145       0.637    0.454   FALSE    
#> 5      5   181   200    20 2.67  1.81  -0.460        0.197    0.272   FALSE    
```
