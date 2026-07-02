# fastcpd wrapper — fast changepoint detection via sequential gradient descent

Wraps the fastcpd package (Li and Zhang, 2024), a modern PELT-family
engine that pairs pruning with sequential gradient descent so that exact
or near-exact segmentations of many model families run in near-linear
time. This wrapper exposes the time-series families most useful
alongside the other engines: mean, variance, mean-and-variance, and
AR/ARMA/GARCH model changepoints.

## Usage

``` r
fastcpd_wrapper(
  x,
  family = c("mean", "variance", "meanvariance", "ar", "arma", "garch"),
  order = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector, or (for `family` `"mean"`, `"variance"`,
  `"meanvariance"`) a matrix with one row per time point for
  multivariate detection.

- family:

  Model family: `"mean"`, `"variance"`, `"meanvariance"`, `"ar"`,
  `"arma"`, or `"garch"`. Defaults to `"mean"`.

- order:

  Model order for `"ar"` (a single integer), `"arma"` (length-2), or
  `"garch"` (length-2). Defaults to `1` for AR, `c(1, 1)` otherwise.

- ...:

  Additional arguments passed to the corresponding
  `fastcpd::fastcpd.*()` function (e.g. `beta`, `trim`).

## Value

A `ggcpt` object.

## References

Li X, Zhang X (2024). “fastcpd: Fast change point detection in R.”
*arXiv preprint arXiv:2404.05933*.

## Examples

``` r
set.seed(2026)
res <- fastcpd_wrapper(c(rnorm(100), rnorm(100, 4)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```
