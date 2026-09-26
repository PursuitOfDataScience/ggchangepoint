# Changepoint stability diagnostics via bootstrap

Most engines report a point set of changepoints with no measure of how
fragile it is. `cpt_stability()` fits the detector once, then resamples
residuals *within* the fitted segments (so the estimated regime
structure is preserved), re-runs the detector on each replicate, and
reports how often each location is re-detected. The resulting
detection-frequency profile is a cheap, model-agnostic confidence signal
available for every wrapped engine, including the many that ship no
confidence intervals.

## Usage

``` r
cpt_stability(
  x,
  method = "pelt",
  B = 100,
  margin = 5,
  seed = NULL,
  bootstrap = c("iid", "block"),
  block_length = NULL,
  reversal = TRUE,
  ...
)

# S3 method for class 'ggcpt_stability'
print(x, ...)

# S3 method for class 'ggcpt_stability'
autoplot(object, ...)
```

## Arguments

- x:

  A numeric vector, or a `ggcpt` fit (its series, method, change type
  and penalty are used, and it is the original the replicates are
  compared with); for the [`print()`](https://rdrr.io/r/base/print.html)
  method, a `ggcpt_stability` object.

- method:

  Detection method, passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  Ignored when `x` is a fit.

- B:

  Number of bootstrap replicates. Defaults to `100`.

- margin:

  Tolerance (in indices) when counting a replicate detection as a
  re-detection of a location. Defaults to `5`.

- seed:

  Optional seed for reproducibility. The seed is scoped to this call:
  `.Random.seed` is saved and restored, so a seeded call inside a
  simulation loop does not pin the loop's own stream.

- bootstrap:

  `"iid"` (the default) resamples residuals one at a time within each
  segment; `"block"` resamples contiguous blocks of them, which keeps
  the residuals' short-range dependence, so an artifact of
  autocorrelated noise can move between replicates instead of being
  rebuilt in the same place every time.

- block_length:

  Block length for `bootstrap = "block"`. Defaults to \\n^{1/3}\\
  lengthened by the residuals' lag-1 autocorrelation \\r\\ by the factor
  \\(1 + \|r\|)/(1 - \|r\|)\\, and capped at a quarter of the series.

- reversal:

  Also run the detector once on the reversed series, and flag each
  changepoint that the reversed series does not find within `margin`?
  Defaults to `TRUE`. An offline detector looks at the whole series, so
  a changepoint that does not survive reversal is a borderline one:
  measured, it is the same instability the bootstrap finds, for one
  extra fit. Sequential (online) detectors genuinely depend on direction
  and get `NA`.

- ...:

  Additional arguments passed to every
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  call.

- object:

  A `ggcpt_stability` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_stability` object: a list with `frequency` (a tibble of `index`
and `freq`, the proportion of replicates detecting a changepoint within
`margin` of that index), `original` (the point-estimate `ggcpt`), `B`,
`n_failed` (replicates on which the detector failed, which are left out
of the proportion rather than counted as "found nothing"), `bootstrap`,
`block_length` and `reversal` (one row per original changepoint: `cp`
and `survives`). Methods: [`print()`](https://rdrr.io/r/base/print.html)
and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(frequency profile with the original detections marked).

## What a high score means

*Reproducible under resampling*, which is not the same as *real*, and
under dependent noise the two diverge: measured under AR(1) noise, a
spurious changepoint reached a stability of 1.00 and the score separated
real from spurious changepoints in only 4 of 10 replicates. The artifact
is a feature of the realised noise path, and an i.i.d. resample of the
residuals rebuilds it every time. `bootstrap = "block"` lets it vary;
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
which changes the noise model rather than the sample, is the check that
touches the cause; and
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)
tests the residuals directly.

## Examples

``` r
set.seed(2026)
x <- c(rnorm(60), rnorm(60, 4))
st <- cpt_stability(x, method = "pelt", B = 20)
st
#> ggcpt_stability (20 iid bootstrap replicates, method: pelt)
#> 
#> Original changepoints and their re-detection frequency:
#> # A tibble: 1 × 3
#>      cp stability survives_reversal
#>   <int>     <dbl> <lgl>            
#> 1    60         1 TRUE             
#> 
#> A high score means reproducible under resampling, not real; see ?cpt_stability.
ggplot2::autoplot(st)
```
