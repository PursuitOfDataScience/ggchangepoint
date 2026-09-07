# Blocks test signal

The classic Donoho-Johnstone blocks test signal with known changepoints.

## Usage

``` r
signal_blocks(n = 2048, seed = NULL)
```

## Arguments

- n:

  Length of the signal. Defaults to 2048.

- seed:

  Optional seed. The seed is scoped to this call: `.Random.seed` is
  saved and restored, so a seeded call inside a simulation loop does not
  pin the loop's own stream.

## Value

A tibble with columns `index` and `value`. The `true_changepoints`
attribute contains the known changepoint locations.

## References

Donoho, D. L. and Johnstone, I. M. (1994). Ideal spatial adaptation by
wavelet shrinkage. *Biometrika*, 81(3), 425-455.

## See also

Other test signals:
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md),
[`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md),
[`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md),
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)

## Examples

``` r
x <- signal_blocks(seed = 2026)
attr(x, "true_changepoints")
#>  [1]  205  266  307  471  512  819  901 1331 1556 1597 1659
# PELT recovers all eleven Donoho-Johnstone jumps
cpt_detect(x$value, method = "pelt")$changepoints$cp
#>  [1]  205  266  307  471  512  819  901 1331 1556 1596 1659
```
