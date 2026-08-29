# Functional covariance changepoints

Wraps
[`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html)
for changes in the covariance operator, eigenstructure or trace of a
functional time series — the changes that leave the mean curve
untouched.

## Usage

``` r
fcov_wrapper(
  x,
  target = c("covariance", "trace", "eigenjoint", "eigensingle"),
  statistic = c("Tn", "Mn"),
  critical = c("simulation", "resample", "welch"),
  type = c("segmentation", "single"),
  alpha = 0.05,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per grid location (the curve's resolution).

- target:

  What to test: `"covariance"` (default), `"trace"`, `"eigenjoint"` or
  `"eigensingle"`.

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

- ...:

  Additional arguments passed to
  [`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html).

## Value

A `ggcpt` object with `change_in = "covariance"`.

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
X[31:60, ] <- X[31:60, ] * 3
fcov_wrapper(X, target = "trace", M = 200)
#> ggcpt (changepoint detection result)
#>   Method:         fcov
#>   Change in:       covariance 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         alpha = 0.05 
#>   Series length:   60 
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value p_value
#>   <int>    <dbl>   <dbl>
#> 1    30    0.380    0.05
# }
```
