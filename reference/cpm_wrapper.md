# Sequential change point model wrapper (CPM)

Wraps
[`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html)
(Ross, 2015) for distribution-free sequential changepoint detection via
repeated two-sample tests (Mann-Whitney for location, Mood for scale,
Lepage, Kolmogorov-Smirnov and Cramer-von-Mises for general changes, and
parametric Student/Bartlett/GLR variants). Although the engine is
designed for streams, it is run here over the full series in one pass,
mimicking online monitoring with average run length `arl0`.

## Usage

``` r
cpm_wrapper(x, cpm_type = "Mann-Whitney", arl0 = 500, startup = 20, ...)
```

## Arguments

- x:

  A numeric vector.

- cpm_type:

  Test statistic, passed to
  [`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html)
  as `cpmType`. Distribution-free: `"Mann-Whitney"` (location, the
  default), `"Mood"` (scale), `"Lepage"`, `"Kolmogorov-Smirnov"`,
  `"Cramer-von-Mises"`. Parametric: `"Student"`, `"Bartlett"`, `"GLR"`
  (Gaussian), `"Exponential"` (positive data), `"FET"` (Fisher's exact
  test, for 0/1 Bernoulli data — this one also needs a `lambda` value
  passed through `...`, e.g. `lambda = 0.3`).

- arl0:

  Target in-control average run length (how many observations, on
  average, before a false alarm). Defaults to `500`. cpm ships
  thresholds only for 100, 200, 370, 400, 500, 600, 700, 1000, 2000,
  5000, 10000 and 20000; any other value is refused, because the engine
  answers it by printing an error and reporting no changepoints.

- startup:

  Number of observations after each restart before monitoring begins.
  Defaults to `20`.

- ...:

  Additional arguments passed to
  [`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html).

## Value

A `ggcpt` object. The `changepoints` tibble carries a `detection_time`
column: the index at which the sequential test flagged each change
(always later than the estimated location).

## References

Ross GJ (2015). “Parametric and nonparametric sequential change
detection in R: The cpm package.” *Journal of Statistical Software*,
**66**(3), 1–20.

## Examples

``` r
res <- cpm_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value detection_time
#>   <int>    <dbl>          <int>
#> 1   100    -1.02            104
```
