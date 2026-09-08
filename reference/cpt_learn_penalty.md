# Learn a penalty from labelled series

Fits the supervised penalty model of Hocking et al. (2013): each series
contributes a target *interval* of log-penalties (those achieving the
fewest label errors), a feature vector is computed from the series, and
a linear model is fitted by minimising the squared hinge loss on those
intervals — max-margin interval regression. The result has a
[`predict()`](https://rdrr.io/r/stats/predict.html) method, and
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
accepts it directly as `penalty`, so a learned penalty is used exactly
like a number.

## Usage

``` r
cpt_learn_penalty(
  series,
  labels,
  method = "pelt",
  penalties = NULL,
  engine = c("auto", "penaltyLearning", "native"),
  ...
)

# S3 method for class 'ggcpt_penalty_model'
print(x, ...)

# S3 method for class 'ggcpt_penalty_model'
coef(object, ...)

# S3 method for class 'ggcpt_penalty_model'
predict(object, newdata, ...)
```

## Arguments

- series:

  A named list of numeric vectors (or a matrix/data frame with one
  column per series).

- labels:

  Either a single `cpt_labels` tibble whose `series` column names the
  series, or a list of label tibbles parallel to `series`.

- method:

  Detection method used to build the label-error curves. Defaults to
  `"pelt"`.

- penalties:

  Penalty grid for the curves; passed to
  [`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md).

- engine:

  `"auto"` (default) uses penaltyLearning's `IntervalRegressionCV()`
  when the package is installed and there are enough series for its
  cross-validation, and the built-in squared-hinge fit otherwise;
  `"penaltyLearning"` and `"native"` force the choice.

- ...:

  Additional arguments passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  while building the curves.

- x:

  A `ggcpt_penalty_model` (for
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`coef()`](https://rdrr.io/r/stats/coef.html)).

- object:

  A `ggcpt_penalty_model`.

- newdata:

  A numeric vector (one series), or a list/matrix of series.

## Value

A `ggcpt_penalty_model` object with
[`print()`](https://rdrr.io/r/base/print.html),
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html) methods.

The two scales differ and it matters:
[`coef()`](https://rdrr.io/r/stats/coef.html) gives an intercept plus
one weight per feature **on the log-penalty scale**, which is where the
interval regression is fitted, while
[`predict()`](https://rdrr.io/r/stats/predict.html) exponentiates and
returns a penalty on the natural scale – the scale
[`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
and
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
consume. So a coefficient of \\-0.04\\ on `log_n` is a multiplicative
effect on the penalty, not an additive one.

## Reading the coefficients

**The signs are often not interpretable, and that is a property of the
labels rather than of the fit.** A target interval is open above
whenever the largest penalty on the grid still achieves the minimum
label error – which is the common case, because a large penalty usually
keeps the one changepoint the labels ask for. When every series'
interval is open above, any sufficiently large prediction is optimal,
the problem does not pin the slopes, and the \\L_2\\ term settles them
near zero with whatever sign the optimiser reached.

Two measurements on four series of very different length and noise, all
with one-change labels, differing only in the data drawn. In one, every
non-intercept coefficient came out slightly negative, so the predicted
penalty *decreased* with \\n\\ – the opposite of the \\\log n\\ growth a
reader would expect from BIC, and not evidence of anything. In the
other, every feature weight went to zero and the model became a
**constant**: [`predict()`](https://rdrr.io/r/stats/predict.html)
returned the same penalty for all four series. Both fits put every
prediction inside its target, which is the property the model is fitted
for, and neither outcome says anything about how a penalty should scale.

So do not read a `ggcpt_penalty_model` as having discovered a
relationship, and do not be surprised by a constant one.

If the coefficients need to mean something, the labels have to constrain
the penalty from both sides: widen `penalties` until the largest one
starts to over-segment, so the target intervals close above.
[`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md)
shows whether they do.

## References

Hocking TD, Rigaill G, Vert J, Bach F (2013). “Learning sparse penalties
for change-point detection using max margin interval regression.” In
*Proceedings of the 30th International Conference on Machine Learning*,
volume 28, 172–180.

## See also

[`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md),
[`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md).

## Examples

``` r
set.seed(2026)
series <- list(a = c(rnorm(60), rnorm(60, 4)),
               b = c(rnorm(80), rnorm(80, 2)))
labels <- list(a = as_cpt_labels(60, n = 120),
               b = as_cpt_labels(80, n = 160))
model <- cpt_learn_penalty(series, labels,
                           penalties = c(2, 8, 32, 128))
model
#> ggcpt_penalty_model (native interval regression)
#>   Trained on 2 series with method `pelt`
#>   Features: log_n, log_log_n, log_sd, log_mad, log_range, log_sd_diff, log_mad_diff, log_q90_abs_diff
#> 
#> Coefficients (predicting log penalty):
#>        intercept            log_n        log_log_n           log_sd 
#>           3.0794           0.0000           0.0000           0.0000 
#>          log_mad        log_range      log_sd_diff     log_mad_diff 
#>           0.0000           0.0000           0.0000           0.0000 
#> log_q90_abs_diff 
#>           0.0000 
#> 
#> Use it directly: cpt_detect(x, method = "pelt", penalty = model)
stats::predict(model, series$a)
#> [1] 21.74625
```
