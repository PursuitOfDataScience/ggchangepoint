# Feed observations to a monitor

Pushes new data through a
[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
updating its internal state and appending to its alarm log. The monitor
is returned, so the idiom is `mon <- cpt_update(mon, new_obs)`.

## Usage

``` r
cpt_update(monitor, new_obs)
```

## Arguments

- monitor:

  A `ggcpt_monitor` object.

- new_obs:

  New observations: a numeric vector, or a matrix with rows as time
  points for a multivariate monitor.

## Value

The updated `ggcpt_monitor`: the same structure as the input (see
[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md)),
with `t` advanced by the number of new observations (rows, for a matrix)
and any new rows appended to `alarms`. Re-assign it: the monitor is a
value, not a mutable object, so `cpt_update(mon, y)` without assignment
discards the update.

## See also

[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
[`alarms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/alarms.md).

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
