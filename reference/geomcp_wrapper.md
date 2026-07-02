# Geometrically-inspired multivariate changepoint wrapper (geomcp)

Wraps
[`changepoint.geo::geomcp()`](https://rdrr.io/pkg/changepoint.geo/man/geomcp.html)
(Grundy, Killick and Mihaylov, 2020): each multivariate observation is
mapped to its distance from, and angle to, a reference point, and
univariate PELT is run on the two mapped series. Distance changes
capture shifts in magnitude, angle changes capture shifts in
orientation/correlation structure.

## Usage

``` r
geomcp_wrapper(
  x,
  penalty = "MBIC",
  mapping = c("both", "distance", "angle"),
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point.

- penalty:

  Penalty for the univariate PELT runs (a changepoint-style character
  penalty). Defaults to `"MBIC"`.

- mapping:

  Which mapped series' changepoints to report: `"both"` (union,
  default), `"distance"`, or `"angle"`.

- ...:

  Additional arguments passed to
  [`changepoint.geo::geomcp()`](https://rdrr.io/pkg/changepoint.geo/man/geomcp.html).

## Value

A `ggcpt` object whose changepoints tibble carries a `mapping` column
(`"distance"` or `"angle"`; a location found in both is labelled
`"both"`).

## References

Grundy T, Killick R, Mihaylov G (2020). “High-dimensional changepoint
detection via a geometrically inspired mapping.” *Statistics and
Computing*, **30**, 1155–1166.

## Examples

``` r
set.seed(2026)
X <- rbind(matrix(rnorm(100 * 4), 100), matrix(rnorm(100 * 4, 2), 100))
res <- geomcp_wrapper(X)
res$changepoints
#> # A tibble: 2 × 3
#>      cp cp_value mapping 
#>   <int>    <dbl> <chr>   
#> 1    99   -1.08  angle   
#> 2   100    0.369 distance
```
