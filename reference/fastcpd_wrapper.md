# fastcpd wrapper: fast changepoint detection via sequential gradient descent

Wraps the fastcpd package (Li and Zhang, 2024), a modern PELT-family
engine that pairs pruning with sequential gradient descent so that exact
or near-exact segmentations of many model families run in near-linear
time. Every family the engine documents is reachable except its
user-supplied `custom` cost: Gaussian mean, variance and both; the count
and binary families (`"poisson"`, `"binomial"`) and waiting times
(`"exponential"`); linear and penalised regression (`"lm"`, `"lasso"`)
and generalised linear models (`"poisson"`, `"binomial"` with
`covariates`); and the time-series models `"ar"`, `"arma"`, `"arima"`,
`"garch"` and, for a multivariate series, `"var"`.

## Usage

``` r
fastcpd_wrapper(
  x,
  family = c("mean", "variance", "meanvariance", "ar", "arma", "arima", "garch", "var",
    "lm", "lasso", "poisson", "binomial", "exponential"),
  order = NULL,
  covariates = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector, or (for `family` `"mean"`, `"variance"`,
  `"meanvariance"` and `"var"`) a matrix with one row per time point for
  multivariate detection.

- family:

  Model family. `"mean"` (the default), `"variance"`, `"meanvariance"`;
  `"poisson"`, `"binomial"`, `"exponential"` (a change in the rate or
  probability of a count, binary or waiting-time series, or in a
  regression on `covariates`); `"lm"`, `"lasso"` (regression, which
  needs `covariates`); `"ar"`, `"arma"`, `"arima"`, `"garch"`, `"var"`.

- order:

  Model order for `"ar"` and `"var"` (a single integer), `"arma"` and
  `"garch"` (length 2) or `"arima"` (length 3). Defaults to `1` for AR
  and VAR, `c(1, 1)` for ARMA and GARCH and `c(1, 0, 0)` for ARIMA.

- covariates:

  Optional numeric matrix of regressors, one row per observation, for
  `"lm"`, `"lasso"`, `"poisson"` and `"binomial"`. Include a column of
  ones for an intercept. Without it the count and binary families fit an
  intercept only: a change in the rate or probability.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  builds it from a formula.

- ...:

  Additional arguments passed to the corresponding
  `fastcpd::fastcpd.*()` function (e.g. `beta`, `trim`). `beta` is
  fastcpd's penalty: a number, or one of its own names (`"MBIC"`, the
  default, `"BIC"`, `"MDL"`). It is recorded on the result, so
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) report
  the penalty the fit actually used.
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  forwards a numeric `penalty` here and translates the three names it
  shares with fastcpd; see the penalty-semantics section of
  [`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md).

## Value

A `ggcpt` object. The engine's per-segment parameter estimates
(`thetas`) are kept as `$coefficients`, one row per segment and
parameter, for the families where they are coefficients (the regressions
and the intercept-only count, binary and waiting-time models).

## References

Li X, Zhang X (2024). “fastcpd: Fast change point detection in R.”
*arXiv preprint arXiv:2404.05933*.

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
[`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md),
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
set.seed(2026)
res <- fastcpd_wrapper(c(rnorm(100), rnorm(100, 4)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369

# A change in a Poisson rate
counts <- fastcpd_wrapper(c(rpois(100, 3), rpois(100, 9)),
                          family = "poisson")
counts$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100        5
```
