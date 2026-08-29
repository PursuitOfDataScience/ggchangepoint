# Replay a series through a sequential detector

Runs a whole series through a monitor in one call, so the alarm timeline
of an online method can be studied retrospectively without hand-rolling
the update loop. This is the honest version of what 0.4.0's `cpm`, `ocd`
and `bocpd` wrappers do implicitly: it reports *alarms*, which arrive
after the change, rather than pretending they are estimated changepoint
locations.

## Usage

``` r
cpt_replay(x, method = c("edetector", "cpm", "ocd"), baseline = NULL, ...)
```

## Arguments

- x:

  A numeric vector, or a matrix with rows as time points.

- method:

  Sequential detector; see
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md).

- baseline:

  Number of leading observations used as pre-change training data (an
  integer), or an explicit baseline vector. Defaults to
  `min(100, floor(n / 4))`.

- ...:

  Additional arguments passed to
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md).

## Value

A `ggcpt_monitor` that has already consumed the series.

## See also

[`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md),
[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md).

## Examples

``` r
set.seed(2026)
mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")
alarms(mon)
#> # A tibble: 5 × 3
#>    time statistic threshold
#>   <int>     <dbl>     <dbl>
#> 1    56      112.       100
#> 2    87      360.       100
#> 3   116      198.       100
#> 4   217      174.       100
#> 5   263      114.       100
```
