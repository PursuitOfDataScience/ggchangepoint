# Detection delay and false-alarm rate

Scores an online result the way the sequential literature does: how long
after each true change did the first alarm arrive, and how many alarms
were raised with no change behind them.
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
is the wrong tool for an online detector — it asks whether the
*location* was recovered, which a sequential procedure never claims —
and warns if you point it at one.

## Usage

``` r
cpt_delay(object, truth, max_delay = Inf)

# S3 method for class 'ggcpt_delay'
tidy(x, ...)

# S3 method for class 'ggcpt_delay'
glance(x, ...)

# S3 method for class 'ggcpt_delay'
print(x, ...)

# S3 method for class 'ggcpt_delay'
autoplot(object, ...)
```

## Arguments

- object:

  A `ggcpt_delay` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- truth:

  Integer vector of true changepoint positions, on the same clock as the
  alarms. When `object` is a monitor built by
  [`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md)
  the baseline offset is applied automatically.

- max_delay:

  Alarms further than this after a change are treated as false alarms
  rather than late detections. Defaults to `Inf`.

- x:

  A `ggcpt_delay` object.

- ...:

  Ignored.

## Value

A `ggcpt_delay` object: a list with `per_change` (one row per true
change: `truth`, `alarm`, `delay`, `detected`), `false_alarms`, and the
summary statistics `mean_delay`, `median_delay`, `n_false_alarms` and
`arl` (mean observations per false alarm).

## See also

[`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
[`cpt_replay()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_replay.md).

## Examples

``` r
set.seed(2026)
mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")
cpt_delay(mon, truth = 200)
#> ggcpt_delay
#>   True changes:       1
#>   Detected:           1
#>   Mean delay:         16
#>   Median delay:       16
#>   False alarms:       4
#>   Average run length: 100
#> 
#> # A tibble: 1 × 4
#>   truth alarm delay detected
#>   <int> <int> <dbl> <lgl>   
#> 1   200   216    16 TRUE    
```
