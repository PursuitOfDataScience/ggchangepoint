# ocd wrapper — online high-dimensional changepoint detection

Wraps the `ocd` package (Chen, Wang and Samworth, 2022): online
multiscale detection of a mean change in a high-dimensional stream, with
worst-case detection-delay guarantees and per-observation cost
independent of history. The detector assumes standardised data with
known pre-change mean; this wrapper estimates the baseline mean and
standard deviation from an initial training window, then monitors the
remainder of the series, resetting after each declaration so multiple
changes can be found.

## Usage

``` r
ocd_wrapper(
  x,
  train = NULL,
  thresh = "MC",
  patience = 5000,
  beta = 1,
  mc_reps = 100,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and at
  least two columns. The `ocd` detector is inherently high-dimensional
  and cannot be constructed for a single coordinate, so univariate input
  is rejected; use a univariate engine (see
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md))
  for one series.

- train:

  Number of initial observations used to estimate the baseline mean/sd
  (not monitored). Defaults to `max(20, floor(0.2 * n))`, capped at
  `n/2`.

- thresh:

  Threshold specification passed to
  [`ocd::ChangepointDetector()`](https://rdrr.io/pkg/ocd/man/ChangepointDetector.html);
  `"MC"` (default) calibrates by Monte Carlo, which is what makes this
  the slowest wrapper — see the timing note below. Supplying the three
  thresholds directly, as a named numeric vector
  `c(diag =, off_d =, off_s =)`, skips calibration altogether.

- patience:

  Target average run length to false alarm. Defaults to `5000`.

- beta:

  Assumed lower bound on the squared Euclidean norm of the mean change.
  Defaults to `1`.

- mc_reps:

  Monte Carlo repetitions for threshold calibration. Defaults to `100`.
  The cost is linear in this and grows with the number of coordinates;
  see the timing note below.

- ...:

  Additional arguments passed to
  [`ocd::ChangepointDetector()`](https://rdrr.io/pkg/ocd/man/ChangepointDetector.html).

## Value

A `ggcpt` object. Because the detector is online, reported locations are
*declaration times* (the changepoint plus the detection delay), stored
together with a `declared_at` column.

## How long this takes

Nearly all of the run time is `ocd`'s Monte Carlo threshold calibration,
which happens before a single observation is read. It is linear in
`mc_reps` and grows with the number of coordinates: measured at
`mc_reps = 5`, construction takes about 3 s at \\p = 3\\, 9 s at \\p =
10\\ and 55 s at \\p = 50\\, and four times as long at `mc_reps = 20`.
At the default `mc_reps = 100` that extrapolates to roughly a minute at
\\p = 3\\ and a quarter of an hour at \\p = 50\\. Monitoring the
observations afterwards is cheap by comparison — well under a second for
a thousand of them. Lower `mc_reps` while exploring, or pass `thresh`
directly to skip calibration entirely.

## References

Chen Y, Wang T, Samworth RJ (2022). “High-dimensional, multiscale online
changepoint detection.” *Journal of the Royal Statistical Society:
Series B*, **84**(1), 234–266.

## Examples

``` r
# \donttest{
set.seed(2026)
X <- rbind(matrix(rnorm(60 * 3), 60), matrix(rnorm(40 * 3, 3), 40))
res <- ocd_wrapper(X, mc_reps = 5)
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value declared_at
#>   <int>    <dbl>       <int>
#> 1    62     4.31          62
# }
```
