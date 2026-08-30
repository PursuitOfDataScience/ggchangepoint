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

## See also

Other changepoint engines:
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
[`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md),
[`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md),
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
[`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md),
[`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md),
[`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md),
[`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md),
[`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md),
[`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md),
[`fabisearch_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fabisearch_wrapper.md),
[`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md),
[`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md),
[`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md),
[`fpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md),
[`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md),
[`hdcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdcov_wrapper.md),
[`hdreg_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdreg_wrapper.md),
[`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md),
[`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md),
[`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md),
[`kwc_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kwc_wrapper.md),
[`mcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mcp_wrapper.md),
[`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md),
[`network_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/network_wrapper.md),
[`not_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/not_wrapper.md),
[`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md),
[`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md),
[`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md),
[`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md),
[`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md),
[`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md),
[`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md),
[`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md),
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

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
