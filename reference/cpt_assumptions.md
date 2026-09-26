# Check the assumptions behind a segmentation

A short report on whether the result's assumptions look violated for
this series, what that does to the answer, and what would model it. It
reports and does not act: every automatic correction measured trades
true changepoints for false ones, and which side of that trade matters
is the analyst's decision.

## Usage

``` r
cpt_assumptions(fit, lag = NULL)

# S3 method for class 'ggcpt_assumptions'
print(x, ...)
```

## Arguments

- fit:

  A `ggcpt` object.

- lag:

  Lag for the Ljung-Box test. Defaults to `10`, or `n / 5` for a short
  series.

- x:

  A `ggcpt_assumptions` object.

- ...:

  Ignored.

## Value

A `ggcpt_assumptions` tibble with one row per check: `component`,
`value`, `flag` (`TRUE` when the check raises a concern), `detail` and
`advice`. The components:

- `residual_dependence`:

  Ljung-Box p-value on the within-segment residuals (and their lag-1
  autocorrelation in `detail`). Flagged below 0.05. Autocorrelated noise
  is the commonest reason for spurious changepoints.

- `scale_sensitivity`:

  the noise standard deviation, flagged when the engine's answer depends
  on the data's units (measured) and the noise is far from unit scale.

- `expected_false_positives`:

  how many changepoints the engine is expected to report on this much
  pure noise: `n / arl0` for `cpm`, and the measured null-size table
  otherwise. Flagged above one.

- `count_plausibility`:

  reported changepoints per hundred observations, flagged above one:
  more than `n / 100` is more often misconfiguration than a finding.

- `data_type`:

  what the series looks like (continuous, counts, binary, proportions),
  flagged when a Gaussian cost is fitted to non-Gaussian data.

## See also

[`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md),
which prints this;
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md)
for the answer under other noise models.

Other inference:
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
ar <- as.numeric(arima.sim(list(ar = 0.7), 300))
cpt_assumptions(cpt_detect(ar, method = "pelt"))
#> ggcpt_assumptions (method: pelt)
#>  ! residual_dependence       Ljung-Box at lag 10; lag-1 autocorrelation 0.54
#>       The residuals are autocorrelated, the commonest cause of spurious
#>       changepoints. cpt_detect(x, method = "kcp") (0.3 spurious, 1.9 of 2
#>       real changes found); cpt_detect(x, method = "bfast") (1.0 spurious,
#>       1.6 of 2 real changes found); cpt_detect(x, method = "nsp", variant =
#>       "selfnorm") (0.8 spurious, 1.2 of 2 real changes found)
#>    scale_sensitivity         noise sd 0.717; this engine's cost assumes unit noise
#>    expected_false_positives  measured on pure noise at n = 1,000 (50 replicates)
#>  ! count_plausibility        4 changepoints in 300 observations (1.33 per hundred)
#>       More than one changepoint per hundred observations is more often a
#>       penalty on the wrong scale or dependent noise than a finding; check
#>       the two rows above.
#>    data_type                 looks continuous; family gaussian
```
