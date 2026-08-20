# SMUCE / HSMUCE wrapper — multiscale changepoint inference

Wraps [`stepR::stepFit()`](https://rdrr.io/pkg/stepR/man/stepFit.html)
for the Simultaneous MUltiscale Changepoint Estimator (SMUCE) of Frick,
Munk and Sieling (2014) and its heterogeneous extension HSMUCE (Pein,
Sieling and Munk, 2017). SMUCE estimates a step function subject to a
simultaneous multiscale test at level `alpha`; the level bounds the
probability of over-estimating the number of changepoints, and the fit
delivers *confidence intervals for every changepoint location*, which
populate the `ci_lower`/`ci_upper` columns of the result and render via
`autoplot(show_ci = TRUE)` or
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md).

## Usage

``` r
smuce_wrapper(x, alpha = 0.5, family = c("gauss", "hsmuce"), ...)
```

## Arguments

- x:

  A numeric vector.

- alpha:

  Significance level of the multiscale test in \\(0, 1)\\; smaller
  values yield more conservative (fewer-changepoint) fits. Defaults to
  `0.5`, the upstream recommendation for estimation.

- family:

  Noise model: `"gauss"` (SMUCE, homogeneous Gaussian noise) or
  `"hsmuce"` (HSMUCE, segment-wise variance). Defaults to `"gauss"`. The
  remaining `stepR` families (`"jsmurf"`, `"mDependentPS"`, ...) all
  require a filter or covariance specification; call
  [`stepR::stepFit()`](https://rdrr.io/pkg/stepR/man/stepFit.html)
  directly for those. `"hsmuce"` additionally refuses a series whose
  point-to-point variation lies more than about seven orders of
  magnitude below its own scale — a globally flat series, or a step
  whose segments are numerically constant, as `cpt_simulate(sd = 0)`
  produces once any rounding is added. stepR's heterogeneous variance
  estimator aborts the R session on such input rather than raising an
  error, so it cannot be caught. `"gauss"` handles the whole range.

- ...:

  Additional arguments passed to
  [`stepR::stepFit()`](https://rdrr.io/pkg/stepR/man/stepFit.html).

## Value

A `ggcpt` object. The `changepoints` tibble carries
`ci_lower`/`ci_upper` (confidence interval for each changepoint
location) and the `data` tibble carries the SMUCE step fit in its
`fitted` column.

## References

Frick K, Munk A, Sieling H (2014). “Multiscale change point inference.”
*Journal of the Royal Statistical Society: Series B*, **76**(3),
495–580.

Pein F, Sieling H, Munk A (2017). “Heterogeneous change point
inference.” *Journal of the Royal Statistical Society: Series B*,
**79**(4), 1207–1227.

## Examples

``` r
# \donttest{
set.seed(2026)
x <- c(rnorm(100), rnorm(100, 3))
res <- smuce_wrapper(x)
res$changepoints
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100    0.369       99      101
ggplot2::autoplot(res, show_ci = TRUE)

# }
```
