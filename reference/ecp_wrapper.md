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
(0 rows). The upstream fit is not retained — and `$fit` is `NULL` on a
`ggcpt` from `cpt_detect(method = "ecp")` — because
[`ecp::e.agglo()`](https://rdrr.io/pkg/ecp/man/e.agglo.html)'s
cluster-progression matrix is quadratic in the series length; call the
ecp functions directly if you need their full output.

## References

James NA, Matteson DS (2014). “ecp: An R package for nonparametric
multiple change point analysis of multivariate data.” *Journal of
Statistical Software*, **62**(7), 1–25.
[doi:10.18637/jss.v062.i07](https://doi.org/10.18637/jss.v062.i07) .

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
