# inspect wrapper — high-dimensional changepoints via sparse projection

Wraps
[`InspectChangepoint::inspect()`](https://rdrr.io/pkg/InspectChangepoint/man/inspect.html)
(Wang and Samworth, 2018). For a \\p\\-variate series whose mean changes
in an unknown sparse subset of coordinates, the algorithm computes the
CUSUM transformation, finds the optimal sparse projection direction via
a convex relaxation, and locates changepoints on the projected
univariate series, recursing via wild binary segmentation.

## Usage

``` r
inspect_wrapper(x, lambda = NULL, threshold = NULL, ...)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per coordinate.

- lambda:

  Regularisation parameter of the sparse projection; when `NULL` the
  engine default \\\sqrt{\log(p \log n)/2}\\ is used.

- threshold:

  Detection threshold; when `NULL` it is computed by Monte Carlo (via
  the engine).

- ...:

  Additional arguments passed to
  [`InspectChangepoint::inspect()`](https://rdrr.io/pkg/InspectChangepoint/man/inspect.html).

## Value

A `ggcpt` object. The changepoints tibble carries a `strength` column
(the maximum projected CUSUM statistic). The first coordinate is used
for `cp_value` and the univariate plot line; the full matrix is kept for
the faceted multivariate
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## References

Wang T, Samworth RJ (2018). “High dimensional change point estimation
via sparse projection.” *Journal of the Royal Statistical Society:
Series B*, **80**(1), 57–83.

## Examples

``` r
set.seed(2026)
X <- cbind(c(rnorm(80), rnorm(80, 3)), c(rnorm(80), rnorm(80, -2)),
           rnorm(160))
res <- inspect_wrapper(X)
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value strength
#>   <int>    <dbl>    <dbl>
#> 1    80    0.785     21.9
```
