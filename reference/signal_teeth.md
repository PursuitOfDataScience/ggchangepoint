# Teeth test signal

A piecewise-constant signal with regularly spaced changepoints.

## Usage

``` r
signal_teeth(n = 2000, seed = NULL)
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
[`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md),
[`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md)

## Examples

``` r
x <- signal_teeth(n = 600, seed = 2026)
attr(x, "true_changepoints")   # a change every 100 observations
#> [1] 100 200 300 400 500
cpt_detect(x$value, method = "pelt")$changepoints$cp
#> [1] 100 200 300 400 500
```
