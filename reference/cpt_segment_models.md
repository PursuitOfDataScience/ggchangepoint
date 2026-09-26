# Fit a model to every segment of a segmentation

Detection says where the regimes change; this fits *your* model to each
regime. Every segment of `fit` gets its own copy of `model`, fitted by
`engine` to that segment's rows, and the result is a table with one
fitted model per segment that
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html) read.

## Usage

``` r
cpt_segment_models(fit, model = NULL, data = NULL, engine = stats::lm, ...)

# S3 method for class 'ggcpt_segment_models'
tidy(x, conf_level = 0.95, ...)

# S3 method for class 'ggcpt_segment_models'
glance(x, ...)

# S3 method for class 'ggcpt_segment_models'
print(x, ...)
```

## Arguments

- fit:

  A `ggcpt` object.

- model:

  A formula. For a result from a formula fit it defaults to that
  formula; for a series it defaults to `value ~ 1` (the segment level),
  or `value ~ time` for a change in slope. The series is available as
  `value`, its position as `time`, and the time index (when the result
  carries one) as `index`.

- data:

  Optional data frame with one row per observation, supplying covariates
  the model uses. For a formula fit the covariates are taken from the
  result.

- engine:

  The fitting function, called as
  `engine(model, data = segment_rows, ...)`. Defaults to
  [`stats::lm`](https://rdrr.io/r/stats/lm.html);
  [`stats::glm`](https://rdrr.io/r/stats/glm.html) with a `family` works
  the same way.

- ...:

  Further arguments for `engine`.

- x:

  A `ggcpt_segment_models` object.

- conf_level:

  Confidence level for the coefficient intervals.

## Value

A `ggcpt_segment_models` tibble with one row per segment: `segment`,
`start`, `end`, `n` and `model` (a list-column of fitted models, `NULL`
where the fit failed).
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives the
coefficient table with a `segment` column;
[`glance()`](https://generics.r-lib.org/reference/glance.html) one row
per segment with `n`, `sigma`, `r_squared`, `AIC`, `BIC` and `logLik`.

## See also

[`predict.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/predict.ggcpt.md)
to forecast from a segment,
[`tidy.ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tidy.ggcpt.md)`(what = "coefficients")`
for the coefficients the detection itself implies.

Other segment models:
[`predict.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/predict.ggcpt.md)

## Examples

``` r
set.seed(1)
d <- data.frame(x = runif(200))
d$y <- ifelse(seq_len(200) <= 100, 1 + 2 * d$x, 3 - d$x) + rnorm(200, 0, 0.3)
fit <- cpt_detect(y ~ x, data = d, method = "strucchange")
sm <- cpt_segment_models(fit)
tidy(sm)
#> # A tibble: 4 × 8
#>   segment term        estimate std_error statistic  p_value conf_low conf_high
#>     <int> <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1       1 (Intercept)    0.999    0.0632     15.8  9.62e-29    0.874     1.12 
#> 2       1 x              1.98     0.108      18.2  2.75e-33    1.76      2.19 
#> 3       2 (Intercept)    3.00     0.0673     44.6  6.43e-67    2.87      3.14 
#> 4       2 x             -0.991    0.115      -8.59 1.36e-13   -1.22     -0.762
glance(sm)
#> # A tibble: 2 × 7
#>   segment     n sigma r_squared   AIC   BIC logLik
#>     <int> <int> <dbl>     <dbl> <dbl> <dbl>  <dbl>
#> 1       1   100 0.289     0.773  39.3  47.2  -16.7
#> 2       2   100 0.312     0.430  54.7  62.5  -24.4
```
