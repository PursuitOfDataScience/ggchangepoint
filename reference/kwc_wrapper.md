# Robust depth-based changepoints for functional and multivariate data

Wraps KWCChangepoint (Ramsay and Chenouri): the functional
Kruskal–Wallis covariance test, which ranks observations by statistical
*depth* and segments on the ranks. Because it never touches the values
themselves it is insensitive to heavy tails and outlying curves, which
is exactly where the moment-based functional tests degrade.

## Usage

``` r
kwc_wrapper(
  x,
  algorithm = c("fkwc", "dwbs"),
  depth = NULL,
  change_in = c("covariance", "distribution"),
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per observation (time
  point) and one column per grid location or coordinate.

- algorithm:

  `"fkwc"` (default; pruned exact linear time over the depth ranks) or
  `"dwbs"` (depth-based wild binary segmentation).

- depth:

  Depth function. For `"fkwc"` one of `"RPD"` (default), `"FM"`,
  `"LTR"`, `"FMd"`, `"RPDd"`; for `"dwbs"` one of `"spat"`, `"hs"`,
  `"mahal"`, `"mahal75"`.

- change_in:

  Reported change type: `"covariance"` (default) or `"distribution"`.
  The test is sensitive to both; this only labels the result.

- seed:

  Optional seed — the random-projection depths and the wild binary
  segmentation both randomise.

- ...:

  Additional arguments passed to the engine.

## Value

A `ggcpt` object.

## References

Ramsay K, Chenouri S (2025). “Robust changepoint detection in the
variability of multivariate functional data.” *Journal of Nonparametric
Statistics*.
[doi:10.1080/10485252.2025.2503891](https://doi.org/10.1080/10485252.2025.2503891)
.

## Examples

``` r
set.seed(2026)
X <- matrix(rnorm(100 * 20), nrow = 100)
X[51:100, ] <- X[51:100, ] * 3
kwc_wrapper(X, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:         kwc
#>   Change in:       covariance 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         fkwc 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50   -0.240
```
