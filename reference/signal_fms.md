# FMS (Four-Metric-Segments) test signal

A piecewise-constant test signal from the WBS/NOT literature.

## Usage

``` r
signal_fms(n = 2000, seed = NULL)
```

## Arguments

- n:

  Length of the signal. Defaults to 2000.

- seed:

  Optional seed. The seed is scoped to this call: `.Random.seed` is
  saved and restored, so a seeded call inside a simulation loop does not
  pin the loop's own stream.

## Value

A tibble with columns `index` and `value`.

## See also

Other test signals:
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
[`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md),
[`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md),
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)

## Examples

``` r
x <- signal_fms(seed = 2026)
cp <- cpt_detect(x$value, method = "pelt")$changepoints$cp
# the smallest jumps (0.5, against noise sd 0.5) are the ones missed
cpt_metrics(cp, attr(x, "true_changepoints"), n = nrow(x))$covering
#> [1] 0.8674641
```
