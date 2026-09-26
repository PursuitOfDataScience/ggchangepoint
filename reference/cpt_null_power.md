# What an empty answer could have detected

When a detection comes back empty, the useful question is whether a
change of the size you care about would have been found. This runs
[`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md)
at the fit's own length, method and penalty, with the noise level
estimated from the series, and reports the smallest shift detected with
the requested power: changes smaller than that could well be there and
missed.

## Usage

``` r
cpt_null_power(fit, power = 0.8, n_sim = 50, seed = NULL, ...)

# S3 method for class 'ggcpt_null_power'
print(x, ...)
```

## Arguments

- fit:

  A `ggcpt` object (usually one with no changepoints; it works for any).

- power:

  Target power. Defaults to `0.8`.

- n_sim:

  Simulations per power evaluation. Defaults to `50`.

- seed:

  Optional seed, scoped to this call.

- ...:

  Further arguments for
  [`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md).

- x:

  A `ggcpt_null_power` object.

## Value

A `ggcpt_null_power` list with `n`, `sigma` (the noise level used),
`jump` (the smallest detectable shift, in the data's units), `jump_sd`
(the same in noise standard deviations), `power`, `method` and
`constant` (`TRUE` when the series has no variation, in which case
nothing is simulated). With a
[`print()`](https://rdrr.io/r/base/print.html) method.

## See also

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
# \donttest{
set.seed(1)
fit <- cpt_detect(rnorm(100), method = "pelt")
cpt_null_power(fit, n_sim = 20, seed = 1)
#> ggcpt_null_power (method: pelt, n = 100)
#> At this length and noise level (sd 0.961), a single shift of about 1.09 (1.13 sd) is
#> detected with power 0.8. A smaller change could be present and missed.
# }
```
