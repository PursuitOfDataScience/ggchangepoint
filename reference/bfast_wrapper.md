# BFAST wrapper — breaks for additive season and trend

Wraps the bfast family (Verbesselt et al.), the standard tool in remote
sensing and land-cover monitoring. BFAST decomposes a seasonal series
into trend and seasonal components and detects breaks in each
separately, which is the right question for satellite time series where
a shift in phenology and a shift in level mean different things.

## Usage

``` r
bfast_wrapper(
  x,
  frequency = 12,
  change_in = c("mean", "slope", "seasonality"),
  h = 0.15,
  season = c("harmonic", "dummy", "none"),
  max_iter = 5,
  ...
)
```

## Arguments

- x:

  A numeric vector, or a `ts` — a `ts` is strongly preferred, because
  BFAST needs the seasonal frequency and cannot guess it. A bare vector
  is turned into a `ts` with `frequency`.

- frequency:

  Observations per season, used when `x` carries none. Defaults to `12`.

- change_in:

  Which component's breaks to report: `"mean"` or `"slope"` give the
  trend breaks (the usual choice), `"seasonality"` gives the seasonal
  ones.

- h:

  Minimal segment size as a fraction of the series. Defaults to `0.15`.

- season:

  Seasonal model: `"harmonic"` (default), `"dummy"` or `"none"`.

- max_iter:

  Maximum iterations of the trend/season loop. Defaults to `5`.

- ...:

  Additional arguments passed to
  [`bfast::bfast()`](https://rdrr.io/pkg/bfast/man/bfast.html).

## Value

A `ggcpt` object whose `fitted` column holds the estimated trend
component (so `autoplot(show_fit = TRUE)` draws it) and whose
changepoints carry `ci_lower`/`ci_upper` from strucchange's break-date
intervals when available.

## References

Verbesselt J, Hyndman R, Newnham G, Culvenor D (2010). “Detecting trend
and seasonal changes in satellite image time series.” *Remote Sensing of
Environment*, **114**(1), 106–115.
[doi:10.1016/j.rse.2009.08.014](https://doi.org/10.1016/j.rse.2009.08.014)
.

## Examples

``` r
# \donttest{
set.seed(2026)
season <- rep(sin(seq(0, 2 * pi, length.out = 12)), 10)
y <- stats::ts(c(rnorm(60, 1), rnorm(60, 5)) + season,
               frequency = 12, start = c(2000, 1))
bfast_wrapper(y)
#> ggcpt (changepoint detection result)
#>   Method:         bfast
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         BIC 
#>   Series length:   120 
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1    60 0.000947       59       61
# }
```
