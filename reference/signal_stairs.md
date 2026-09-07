# Stairs test signal

A monotonically stepping signal (staircase).

## Usage

``` r
signal_stairs(n = 2000, seed = NULL)
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
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)

## Examples

``` r
x <- signal_stairs(n = 500, seed = 2026)
attr(x, "true_changepoints")   # ten steps, so nine changes
#> [1]  50 100 150 200 250 300 350 400 450
cpt_detect(x$value, method = "pelt")$changepoints$cp
#> [1]  50 100 150 200 250 300 350 400 450
```
