# Self-normalisation wrapper (SNSeg)

Wraps
[`SNSeg::SNSeg_Uni()`](https://rdrr.io/pkg/SNSeg/man/SNSeg_Uni.html)
(Zhao, Jiang and Shao, 2022): self-normalised segmentation with nested
local windows. Self-normalisation avoids estimating the long-run
variance, is robust to temporal dependence, and detects changes in
general parameters — mean, variance, quantiles, autocorrelation, or
bivariate correlation — within one framework.

## Usage

``` r
sn_wrapper(
  x,
  parameter = c("mean", "variance", "acf", "bivcor"),
  confidence = 0.9,
  grid_size = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector (or a two-column matrix for `parameter = "bivcor"`).

- parameter:

  Which parameter to test for changes: `"mean"`, `"variance"`, `"acf"`,
  or `"bivcor"` (bivariate correlation). Defaults to `"mean"`.

- confidence:

  Confidence level of the self-normalised test, one of 0.9, 0.95, 0.99,
  0.995 or 0.999. Defaults to `0.9`.

- grid_size:

  Grid size controlling the local-window sweep; when `NULL` the engine's
  default is used.

- ...:

  Additional arguments passed to
  [`SNSeg::SNSeg_Uni()`](https://rdrr.io/pkg/SNSeg/man/SNSeg_Uni.html).

## Value

A `ggcpt` object.

## References

Zhao Z, Jiang F, Shao X (2022). “Segmenting time series via
self-normalisation.” *Journal of the Royal Statistical Society: Series
B*, **84**(5), 1699–1725.

## Examples

``` r
# \donttest{
set.seed(2026)
res <- sn_wrapper(c(rnorm(150), rnorm(150, 3)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   150    -1.56
# }
```
