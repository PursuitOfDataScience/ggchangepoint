# Forecast from a segment of a segmentation

The point of detecting a regime change is usually that the earlier
regime should no longer inform what happens next.
[`predict()`](https://rdrr.io/r/stats/predict.html) on a `ggcpt`
forecasts from one segment only (by default the last), with the
segment's own model.

## Usage

``` r
# S3 method for class 'ggcpt'
predict(
  object,
  newdata = NULL,
  h = 1,
  segment = "last",
  models = NULL,
  level = 0.95,
  ...
)
```

## Arguments

- object:

  A `ggcpt` object.

- newdata:

  Optional data frame of covariates (for a formula fit, or a segment
  model with covariates), or of `time` positions.

- h:

  Forecast horizon, when `newdata` is not given: the positions `n + 1`
  to `n + h`. Defaults to `1`.

- segment:

  Which segment to forecast from: `"last"` (the default) or a segment
  number.

- models:

  Optional
  [`cpt_segment_models()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_segment_models.md)
  result; built with its defaults when omitted.

- level:

  Level of the prediction interval. Defaults to `0.95`.

- ...:

  Further arguments for the model's
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

## Value

A tibble with `time` (when forecasting by horizon), `.pred`, and
`.lower`/`.upper` when the model supplies a prediction interval.

## See also

Other segment models:
[`cpt_segment_models()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_segment_models.md)

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(100), rnorm(100, 3)), method = "pelt")
predict(fit, h = 3)
#> # A tibble: 3 × 4
#>    time .pred .lower .upper
#>   <int> <dbl>  <dbl>  <dbl>
#> 1   201  2.96   1.05   4.87
#> 2   202  2.96   1.05   4.87
#> 3   203  2.96   1.05   4.87
```
