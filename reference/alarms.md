# The alarm log of a monitor

Every observation at which a sequential monitor crossed its threshold,
in the order they fired. A monitor that never fired returns a zero-row
tibble rather than `NULL`, so the result is always safe to
[`rbind()`](https://rdrr.io/r/base/cbind.html) or plot.

## Usage

``` r
alarms(x, ...)

# S3 method for class 'ggcpt_monitor'
alarms(x, ...)
```

## Arguments

- x:

  A `ggcpt_monitor` object.

- ...:

  Ignored.

## Value

A tibble with one row per alarm: `time` (the observation at which it
fired, counted from the first one fed to the monitor), `statistic` and
`threshold`.

## See also

[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
[`cpt_delay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_delay.md).

## Examples

``` r
set.seed(2026)
mon <- cpt_monitor("edetector", baseline = rnorm(100))
mon <- cpt_update(mon, c(rnorm(50), rnorm(50, 3)))
alarms(mon)
#> # A tibble: 1 × 3
#>    time statistic threshold
#>   <int>     <dbl>     <dbl>
#> 1    51      134.       100
```
