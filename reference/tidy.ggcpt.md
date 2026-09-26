# Tidy a ggcpt object

Returns the changepoints tibble (one row per changepoint), or the
coefficients of every segment.

## Usage

``` r
# S3 method for class 'ggcpt'
tidy(x, what = c("changepoints", "coefficients"), conf_level = 0.95, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- what:

  `"changepoints"` (the default) or `"coefficients"`: one row per
  segment and model term, with `estimate`, `std_error` and a Wald
  interval. For a formula fit the terms are the regression's; for a
  series they are the segment level (and its slope in time, for a change
  in slope), so a mean-shift result reports each segment's mean with its
  standard error. Engines that estimate the coefficients themselves
  (segmented's slopes, fastcpd's per-segment parameters) report their
  own estimates.

- conf_level:

  Confidence level for the coefficient intervals.

- ...:

  Additional arguments (ignored).

## Value

A tibble. For `"changepoints"`: columns `cp`, `cp_value`, and any
method-specific columns. For `"coefficients"`: `segment`, `start`,
`end`, `term`, `estimate`, `std_error`, `conf_low`, `conf_high` (plus
`coordinate` for a multivariate result).

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(80), rnorm(80, 3)), method = "pelt")
tidy(fit)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    80   -0.590
tidy(fit, "coefficients")
#> # A tibble: 2 × 8
#>   segment start   end term        estimate std_error conf_low conf_high
#>     <int> <int> <int> <chr>          <dbl>     <dbl>    <dbl>     <dbl>
#> 1       1     1    80 (Intercept)    0.106     0.101  -0.0943     0.307
#> 2       2    81   160 (Intercept)    2.90      0.103   2.70       3.11 
```
