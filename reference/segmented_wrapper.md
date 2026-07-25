# Broken-line regression wrapper (segmented)

Wraps
[`segmented::segmented()`](https://rdrr.io/pkg/segmented/man/segmented.html)
(Muggeo, 2003, 2008): maximum likelihood estimation of *continuous*
piecewise-linear ("broken-line") regressions, with standard errors and
confidence intervals for the breakpoint locations. Where the step-change
engines model jumps in the level, `segmented` models kinks in the trend,
so `change_in` is `"slope"` and the fitted broken line is stored in the
`fitted` column for `autoplot(show_fit = TRUE)`.

## Usage

``` r
segmented_wrapper(x, npsi = 1, conf_level = 0.95, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector; a linear model of `x` on time `1:length(x)` is
  segmented.

- npsi:

  Number of breakpoints to estimate. Defaults to `1`.

- conf_level:

  Confidence level for breakpoint intervals. Defaults to `0.95`.

- seed:

  Optional seed (the estimator uses bootstrap restarting).

- ...:

  Additional arguments passed to
  [`segmented::segmented()`](https://rdrr.io/pkg/segmented/man/segmented.html).

## Value

A `ggcpt` object with `ci_lower`/`ci_upper` columns and the fitted
broken line in `$data$fitted`. Breakpoints are rounded to the nearest
index; for a continuous fit the reported location is the kink itself. A
constant series has no kink and returns an empty result, rather than the
arbitrary breakpoint a singular fit would give.

## References

Muggeo VM (2003). “Estimating regression models with unknown
break-points.” *Statistics in Medicine*, **22**(19), 3055–3071.

Muggeo VM (2008). “segmented: An R package to fit regression models with
broken-line relationships.” *R News*, **8**(1), 20–25.

## Examples

``` r
set.seed(2026)
y <- cumsum(c(rep(0.5, 100), rep(-0.3, 100))) + rnorm(200)
res <- segmented_wrapper(y, npsi = 1)
res$changepoints
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100     50.4       99      101
ggplot2::autoplot(res, show_fit = TRUE, show_ci = TRUE)
```
