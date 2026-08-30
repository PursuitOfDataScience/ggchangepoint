# Mix test signal

A piecewise-constant/linear signal from the literature.

## Usage

``` r
signal_mix(n = 2000, seed = NULL)
```

## Arguments

- n:

  Length of the signal. Defaults to 2000.

- seed:

  Optional seed.

## Value

A tibble with columns `index` and `value`.

## See also

Other test signals:
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
[`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md),
[`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md),
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md),
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)

## Examples

``` r
x <- signal_mix(seed = 2026)
attr(x, "true_changepoints")
#> [1]  300  700  900 1300 1600
# the linear ramps are not level shifts, so a mean-change detector puts
# changepoints inside them rather than at the segment joins
cpt_detect(x$value, method = "pelt")$changepoints$cp
#> [1]  406  569 1012 1138 1257 1705 1893
```
