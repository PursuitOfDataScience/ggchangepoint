# Does a changepoint survive a change of noise model?

Re-runs a detector under each setting of its noise-model argument
(`family` for stepR's SMUCE, `variant` for NSP, the cost for the
changepoint engines, ...) and reports which changepoints appear under
every setting and which only under some. A changepoint that survives a
change of noise model is evidence; one that survives a bootstrap
([`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md))
or a change of algorithm
([`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md))
may only be a feature of the noise, which resampling and other
algorithms reproduce.

## Usage

``` r
cpt_robustness(
  x,
  method = "pelt",
  over = "noise_model",
  settings = NULL,
  margin = 5,
  seed = NULL,
  ...
)

# S3 method for class 'ggcpt_robustness'
print(x, ...)

# S3 method for class 'ggcpt_robustness'
autoplot(object, ...)
```

## Arguments

- x:

  A numeric series, or a `ggcpt` fit (its series, method and penalty are
  used).

- method:

  The detector, when `x` is a series.

- over:

  What to vary. Only `"noise_model"` for now.

- settings:

  Optional named list of settings, each a list of arguments for
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
  replacing the method's own sweep. The first is the reference.

- margin:

  Positions within which two changepoints count as the same. Defaults to
  `5`.

- seed:

  Optional seed, scoped to this call.

- ...:

  Further arguments for every
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  call.

- object:

  A `ggcpt_robustness` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_robustness` object: a list with `settings` (one row per
setting: `setting`, `n_cp`, `powered` and `error`), `changepoints` (one
row per changepoint of the reference setting: `cp`, `found_by` (how many
other powered settings find it within `margin`), `of` (how many powered
alternatives there are) and `survives`), `union` and `intersection`
(distinct locations found by any and by every powered setting) and
`fits`. With [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Powerless settings are reported, not counted

A setting that finds nothing where the reference finds several is not a
vote against them: measured, NSP's `variant = "ar"` removes the false
positives of autocorrelated noise by removing detection altogether (0 of
2 real changes found). Such a setting is marked `powered = FALSE` and
left out of `found_by`.

## See also

[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
for sweeping any argument.

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
x <- c(rnorm(100), rnorm(100, 2)) + as.numeric(arima.sim(list(ar = 0.6), 200))
cpt_robustness(x, method = "pelt")
#> ggcpt_robustness (method: pelt, over the noise model)
#> # A tibble: 2 × 3
#>   setting                    n_cp powered
#>   <chr>                     <int> <lgl>  
#> 1 "change_in = \"mean\""        1 TRUE   
#> 2 "change_in = \"meanvar\""     1 TRUE   
#> 
#> Reference changepoints (change_in = "mean"):
#> # A tibble: 1 × 4
#>      cp found_by    of survives
#>   <int>    <int> <int> <lgl>   
#> 1    84        1     1 TRUE    
#> 
#> Locations found by any powered setting: 1; by every one: 1.
```
