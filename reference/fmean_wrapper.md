# Functional mean changepoints

Wraps
[`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html)
for changes in the *mean function* of a functional time series: each
observation is a curve, and the question is when the average curve shape
changes. The binary-segmentation (`"segmentation"`) mode finds multiple
changes; the `"single"` mode runs the one-change test and reports its
p-value.

## Usage

``` r
fmean_wrapper(
  x,
  statistic = c("Tn", "Mn"),
  critical = c("simulation", "resample", "welch"),
  type = c("segmentation", "single"),
  alpha = 0.05,
  robust = FALSE,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per grid location (the curve's resolution).

- statistic:

  Test statistic: `"Tn"` (integrated, the default) or `"Mn"` (maximum).

- critical:

  How critical values are obtained: `"simulation"` (default),
  `"resample"` or `"welch"`.

- type:

  `"segmentation"` (default, multiple changes) or `"single"` (one
  change).

- alpha:

  Significance level. Defaults to `0.05`.

- robust:

  Use the robust (`"robustmean"`) statistic instead of the classical
  mean one? Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html).

## Value

A `ggcpt` object; the changepoints tibble carries the engine's `p_value`
for each location.

## References

Aue A, Rice G, Sönmez O (2018). “Detecting and dating structural breaks
in functional data without dimension reduction.” *Journal of the Royal
Statistical Society: Series B*, **80**(3), 509–529.
[doi:10.1111/rssb.12257](https://doi.org/10.1111/rssb.12257) .

## Examples

``` r
# \donttest{
set.seed(2026)
X <- matrix(rnorm(60 * 20), nrow = 60)
X[31:60, ] <- X[31:60, ] + 2
fmean_wrapper(X, M = 200)
#> ggcpt (changepoint detection result)
#>   Method:         fmean
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         alpha = 0.05 
#>   Series length:   60 
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value p_value
#>   <int>    <dbl>   <dbl>
#> 1    30    0.380       0
# }
```
