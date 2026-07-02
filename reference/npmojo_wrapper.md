# Nonparametric MOSUM wrapper (NP-MOJO)

Wraps
[`CptNonPar::np.mojo()`](https://rdrr.io/pkg/CptNonPar/man/np.mojo.html)
(McGonigle and Cho, 2023): nonparametric moving-sum detection of changes
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

  Moving-window bandwidth. Defaults to `max(20, 0.1 * n)` observations.

- lag:

  Time lag at which changes in the joint distribution are examined; `0`
  targets the marginal distribution. Defaults to `0`.

- ...:

  Additional arguments passed to
  [`CptNonPar::np.mojo()`](https://rdrr.io/pkg/CptNonPar/man/np.mojo.html).

## Value

A `ggcpt` object.

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
