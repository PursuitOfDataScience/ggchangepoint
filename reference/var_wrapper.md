# VAR(1) changepoints

Wraps
[`changepoints::CV.search.DP.VAR1()`](https://rdrr.io/pkg/changepoints/man/CV.search.DP.VAR1.html)
(Wang, Yu, Rinaldo and Willett): dynamic programming with an \\\ell_0\\
penalty for changes in the transition matrix of a vector autoregression,
with the two tuning parameters chosen by cross-validation. The change
here is in the *dynamics* — how the series predicts itself — not in the
level, so it is invisible to every mean-change engine in the package.

## Usage

``` r
var_wrapper(x, gamma_set = NULL, lambda_set = NULL, delta = NULL, ...)
```

## Arguments

- x:

  A numeric matrix or data frame, rows as time points.

- gamma_set:

  Candidate values of the \\\ell_0\\ tuning parameter. Defaults to a
  small grid scaled by the series length.

- lambda_set:

  Candidate lasso penalties. Defaults to `c(0.01, 0.1, 1)`.

- delta:

  Minimum spacing. Defaults to `max(5, floor(n / 20))`.

- ...:

  Additional arguments passed to the engine.

## Value

A `ggcpt` object with `change_in = "regression"`.

## References

Wang D, Yu Y, Rinaldo A, Willett R (2019). “Localizing changes in
high-dimensional vector autoregressive processes.” *arXiv preprint
arXiv:1909.06359*.
[doi:10.48550/arXiv.1909.06359](https://doi.org/10.48550/arXiv.1909.06359)
.

## Examples

``` r
# \donttest{
set.seed(2026)
p <- 3
step <- function(n, a) {
  Y <- matrix(0, n, p)
  for (i in 2:n) Y[i, ] <- a * Y[i - 1, ] + stats::rnorm(p)
  Y
}
var_wrapper(rbind(step(50, 0.1), step(50, 0.8)),
            gamma_set = c(1, 10), lambda_set = c(0.1, 1))
#> ggcpt (changepoint detection result)
#>   Method:         var
#>   Change in:       regression 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         l0 (CV) 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    56    0.423
# }
```
