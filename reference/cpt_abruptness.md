# Was the change abrupt? A step against a ramp, at every changepoint

For each changepoint of a change in mean, fits the window around it
(between the neighbouring changepoints) twice: as a step, which is what
every detector assumes, and as a linear ramp from the old level to the
new one, with its centre and width estimated. When the ramp fits clearly
better, the change took time, and the reported location is the middle of
a transition rather than a moment.

## Usage

``` r
cpt_abruptness(fit, level = 0.95, max_width = NULL)

# S3 method for class 'ggcpt_abruptness'
print(x, ...)

# S3 method for class 'ggcpt_abruptness'
tidy(x, ...)

# S3 method for class 'ggcpt_abruptness'
autoplot(object, ...)
```

## Arguments

- fit:

  A `ggcpt` object with a change in mean (or mean and variance).

- level:

  Level of the interval for the transition width, and `1 - level` the
  threshold at which the step is rejected. Defaults to `0.95`.

- max_width:

  Widest ramp considered, in observations. Defaults to half the window.

- x:

  A `ggcpt_abruptness` object.

- ...:

  Ignored.

- object:

  A `ggcpt_abruptness` object.

## Value

A `ggcpt_abruptness` tibble with one row per changepoint: `cp`,
`window_start`, `window_end`, `width` (the best ramp width, in
observations), `width_lower` and `width_upper` (its interval), `centre`
(the ramp's centre), `statistic` (the likelihood-ratio statistic against
the step), `p_value` and `verdict` (`"abrupt"` when the step is not
rejected, `"gradual"` when it is).
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws each window with the step and the ramp;
[`print()`](https://rdrr.io/r/base/print.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) work as
usual.

## How it decides

For a ramp of width \\w\\ centred at \\c\\, the level is \\a\\ before
\\c - w/2\\, \\b\\ after \\c + w/2\\, and linear between, so for fixed
\\(c, w)\\ the fit is ordinary least squares in \\(a, b)\\. The residual
sum of squares is profiled over the centre for each width on a grid
(every width up to 8, then geometric), and the step is the width-0
member of the family. The statistic \\m \log(RSS\_{step} /
RSS\_{ramp})\\ over the window's \\m\\ observations is compared with a
chi-squared distribution on one degree of freedom. The step sits on the
boundary of the family, which makes that reference conservative, and the
search over widths and centres, which makes it liberal; measured on true
steps (jumps of 1 to 8 noise standard deviations, \\n\\ of 300 and
1,000, Gaussian and \\t_3\\ noise, 400 replicates each, detected with
`pelt`), the step was rejected in 1.0 to 5.2 per cent of fits at
`level = 0.95`. A ramp of 2 standard deviations over 30 observations was
called gradual in 53 per cent of fits at \\n = 300\\, over 60
observations in 70 per cent. The width interval collects every width
whose profile statistic is within the chi-squared quantile of the best.

A ramp also fits a step plus a few outliers near it, and a window that
holds a second, undetected change; read `"gradual"` as "a step is not an
adequate description", and look at
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## See also

[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
with `transition`, to measure how a detector places a gradual change;
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md).

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_confint_coverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint_coverage.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_k_weights()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_k_weights.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_screen()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_screen.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(2026)
ramp <- c(rep(0, 100), seq(0, 4, length.out = 30), rep(4, 100))
fit <- cpt_detect(ramp + rnorm(230, 0, 0.5), method = "pelt")
cpt_abruptness(fit)
#> ggcpt_abruptness (a step against a ramp at each changepoint)
#>   Changepoint 116:    gradual, transition width 24 (24 to 31)
#> 
#> # A tibble: 1 × 10
#>      cp window_start window_end width width_lower width_upper centre statistic
#>   <int>        <int>      <int> <dbl>       <dbl>       <dbl>  <dbl>     <dbl>
#> 1   116            1        230    24          24          31    115      82.2
#> # ℹ 2 more variables: p_value <dbl>, verdict <chr>
step <- cpt_detect(c(rnorm(100), rnorm(100, 4)), method = "pelt")
cpt_abruptness(step)
#> ggcpt_abruptness (a step against a ramp at each changepoint)
#>   Changepoint 100:    gradual, transition width 3 (1 to 16)
#> 
#> # A tibble: 1 × 10
#>      cp window_start window_end width width_lower width_upper centre statistic
#>   <int>        <int>      <int> <dbl>       <dbl>       <dbl>  <dbl>     <dbl>
#> 1   100            1        200     3           1          16    100      4.43
#> # ℹ 2 more variables: p_value <dbl>, verdict <chr>
```
