# ecp wrapper

The ecp package provides a non-parametric way to detect changepoints.
Unlike the changepoint package, it does not assume raw data to have any
formal distribution. This wrapper function wraps two functions from the
ecp package, i.e., `e.divisive()` and `e.agglo()`. Users can use either
function by switching the `algorithm` argument. Before using the wrapper
function, seed should be set for the sake of reproducibility.

## Usage

``` r
ecp_wrapper(data, algorithm = "divisive", min_size = 2, seed = NULL, ...)
```

## Arguments

- data:

  A numeric vector (for univariate) or matrix/data.frame (for
  multivariate).

- algorithm:

  Either `divisive` or `agglo`. `divisive` is the default.

- min_size:

  Minimum number of observations between change points. By default is 2.
  This argument is only applied when `algorithm = "divisive"`.

- seed:

  Optional. A seed for reproducibility of the stochastic permutation
  test.

- ...:

  Extra arguments to pass on either from `e.divisive()` or `e.agglo()`.

## Value

A tibble includes which point(s) is/are the changepoint along with raw
changepoint value corresponding to that changepoint. Changepoint
locations follow the `ecp` package convention: the first index of the
right segment. When no changepoint is found, an empty tibble is returned
(0 rows).

## References

James NA, Matteson DS (2013). “ecp: An R package for nonparametric
multiple change point analysis of multivariate data.” *arXiv preprint
arXiv:1309.3295*.

## Examples

``` r
set.seed(2022)
ecp_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <dbl>    <dbl>
#> 1   102    -12.2
ecp_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <dbl>    <dbl>
#> 1   101     9.07
```
