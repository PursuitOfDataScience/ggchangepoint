# DeCAFS wrapper — changes amid drift and autocorrelated noise

Wraps [`DeCAFS::DeCAFS()`](https://rdrr.io/pkg/DeCAFS/man/DeCAFS.html)
(Romano, Rigaill, Runge and Fearnhead, 2022), which detects abrupt mean
changes when the underlying signal also drifts (random-walk
fluctuations) and the noise is AR(1)-autocorrelated — the two regimes in
which plain change-in-mean methods over-detect. Model parameters are
estimated automatically unless supplied.

## Usage

``` r
decafs_wrapper(x, penalty = NULL, model_param = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- penalty:

  Penalty \\\beta\\ for adding a changepoint. Defaults to
  `2 * log(length(x))`.
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  resolves its own `"MBIC"` default to a stronger numeric value — on a
  five-changepoint series that is 3 changepoints through the dispatcher
  against 5 here — so pass `penalty` explicitly when the two must agree.

- model_param:

  Optional list of model parameters (`sdEta`, `sdNu`, `phi`) as accepted
  by [`DeCAFS::DeCAFS()`](https://rdrr.io/pkg/DeCAFS/man/DeCAFS.html);
  when `NULL` they are estimated from the data.

- ...:

  Additional arguments passed to
  [`DeCAFS::DeCAFS()`](https://rdrr.io/pkg/DeCAFS/man/DeCAFS.html).

## Value

A `ggcpt` object. The `data` tibble carries the estimated signal in its
`fitted` column.

## References

Romano G, Rigaill G, Runge V, Fearnhead P (2022). “Detecting abrupt
changes in the presence of local fluctuations and autocorrelated noise.”
*Journal of the American Statistical Association*, **117**(540),
2147–2162.

## Examples

``` r
set.seed(2026)
res <- decafs_wrapper(c(rnorm(100), rnorm(100, 5)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```
