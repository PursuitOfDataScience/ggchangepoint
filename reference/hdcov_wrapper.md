# High-dimensional covariance changepoints

Wraps
[`changepoints::BS.cov()`](https://rdrr.io/pkg/changepoints/man/BS.cov.html)
(Wang, Yu and Rinaldo): binary segmentation on the sample covariance
operator, which detects a change in the *dependence structure* of a
multivariate series even when every marginal mean and variance is
unchanged. No other engine in the package can see that.

## Usage

``` r
hdcov_wrapper(
  x,
  threshold = NULL,
  alpha = 0.05,
  n_perm = 20,
  delta = NULL,
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame, rows as time points.

- threshold:

  Detection threshold on the CUSUM statistic. When `NULL` (the default)
  it is calibrated by permutation: the time order is shuffled `n_perm`
  times, which destroys any changepoint while preserving the marginal
  distributions, and the threshold is the `1 - alpha` quantile of the
  largest statistic seen.

- alpha:

  Family-wise level for the permutation threshold. Defaults to `0.05`.

- n_perm:

  Permutations used to calibrate the threshold. Defaults to `20`; raise
  it for a sharper threshold at proportional cost.

- delta:

  Minimum spacing between changepoints. Defaults to
  `max(10, floor(n / 20))`.

- seed:

  Optional seed (the permutation calibration is random).

## Value

A `ggcpt` object with `change_in = "covariance"`; the changepoints
tibble carries the CUSUM statistic in `cusum`.

## References

Wang D, Yu Y, Rinaldo A (2021). “Optimal covariance change point
localization in high dimensions.” *Bernoulli*, **27**(1), 554–575.
[doi:10.3150/20-BEJ1249](https://doi.org/10.3150/20-BEJ1249) .

## Examples

``` r
set.seed(2026)
p <- 5
A <- matrix(rnorm(100 * p), ncol = p)
B <- matrix(rnorm(100 * p), ncol = p)
B[, 2] <- B[, 1] + 0.2 * B[, 2]        # correlation appears
hdcov_wrapper(rbind(A, B), n_perm = 20, alpha = 0.05, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:         hdcov
#>   Change in:       covariance 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 7.666 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value cusum
#>   <int>    <dbl> <dbl>
#> 1    93   -0.475  8.22
```
