# WBS for nonstationary time series

Wraps [`wbsts::wbs.lsw()`](https://rdrr.io/pkg/wbsts/man/wbs.lsw.html)
(Korkas and Fryzlewicz): wild binary segmentation applied to the locally
stationary wavelet spectrum, so it detects changes in the *second-order*
structure — variance and autocovariance — of a nonstationary series.
Where `wbs` looks for jumps in the level, this looks for jumps in how
the series behaves.

## Usage

``` r
wbsts_wrapper(
  x,
  n_intervals = 0,
  cstar = 0.75,
  lambda = 0.75,
  scales = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector.

- n_intervals:

  Number of random intervals (`M`). Defaults to `0`, which is the
  engine's "all dyadic intervals" setting.

- cstar, lambda:

  Post-processing constants; the engine's defaults are `0.75` for both.

- scales:

  Wavelet scales to use. `NULL` lets the engine choose.

- seed:

  Optional seed.

- ...:

  Additional arguments passed to
  [`wbsts::wbs.lsw()`](https://rdrr.io/pkg/wbsts/man/wbs.lsw.html).

## Value

A `ggcpt` object with `change_in = "var"`.

## References

Korkas KK, Fryzlewicz P (2017). “Multiple change-point detection for
non-stationary time series using wild binary segmentation.” *Statistica
Sinica*, **27**(1), 287–311.
[doi:10.5705/ss.202015.0262](https://doi.org/10.5705/ss.202015.0262) .

## Examples

``` r
# \donttest{
set.seed(2026)
y <- c(as.numeric(stats::arima.sim(list(ar = 0.1), 250)),
       as.numeric(stats::arima.sim(list(ar = 0.9), 250)))
wbsts_wrapper(y)
#> ggcpt (changepoint detection result)
#>   Method:         wbsts
#>   Change in:       var 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold 
#>   Series length:   500 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   248    0.654
# }
```
