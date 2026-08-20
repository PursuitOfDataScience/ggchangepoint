# Nonparametric MOSUM wrapper (NP-MOJO)

Wraps
[`CptNonPar::np.mojo()`](https://rdrr.io/pkg/CptNonPar/man/np.mojo.html)
(McGonigle and Cho, 2025): nonparametric moving-sum detection of changes
in the marginal or joint distribution of a (possibly multivariate) time
series, robust to serial dependence.

## Usage

``` r
npmojo_wrapper(x, G = NULL, lag = 0, ...)
```

## Arguments

- x:

  A numeric vector or matrix (rows are time points).

- G:

  Moving-window bandwidth. Defaults to `max(20, 0.1 * n)` observations,
  capped at `n / 2` — the largest bandwidth the engine accepts — so the
  default also works on series shorter than 40.

- lag:

  Time lag at which changes in the joint distribution are examined; `0`
  targets the marginal distribution. Defaults to `0`.

- ...:

  Additional arguments passed to
  [`CptNonPar::np.mojo()`](https://rdrr.io/pkg/CptNonPar/man/np.mojo.html).

## Value

A `ggcpt` object. Constant coordinates leave the kernel statistics
undefined, so they are dropped (with a warning) before detection and an
all-constant input returns an empty result. The engine calibrates its
detection threshold by bootstrap, so the value recorded in the penalty
descriptor varies between runs; call
[`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand, or pass
`threshold = "manual"` and `threshold.val` through `...`, for a
reproducible one.

## References

McGonigle ET, Cho H (2025). “Nonparametric data segmentation in
multivariate time series via joint characteristic functions.”
*Biometrika*, **112**(2), asaf024.

## Examples

``` r
res <- npmojo_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 2 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
#> 2   122    5.13 
```
