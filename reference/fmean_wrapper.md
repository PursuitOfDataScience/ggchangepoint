# Functional mean changepoints

Wraps
[`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html)
for changes in the *mean function* of a functional time series: each
observation is a curve, and the question is when the average curve shape
changes. The binary-segmentation (`"segmentation"`) mode finds multiple
changes; the `"single"` mode runs the one-change test and reports its
p-value.

## Usage

``` r
fmean_wrapper(
  x,
  statistic = c("Tn", "Mn"),
  critical = c("simulation", "resample", "welch"),
  type = c("segmentation", "single"),
  alpha = 0.05,
  robust = FALSE,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per grid location (the curve's resolution).

- statistic:

  Test statistic: `"Tn"` (integrated, the default) or `"Mn"` (maximum).

- critical:

  How critical values are obtained: `"simulation"` (default),
  `"resample"` or `"welch"`.

- type:

  `"segmentation"` (default, multiple changes) or `"single"` (one
  change).

- alpha:

  Significance level. Defaults to `0.05`.

- robust:

  Use the robust (`"robustmean"`) statistic instead of the classical
  mean one? Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html).

## Value

A `ggcpt` object; the changepoints tibble carries the engine's `p_value`
for each location.

## References

Aue A, Rice G, Sönmez O (2018). “Detecting and dating structural breaks
in functional data without dimension reduction.” *Journal of the Royal
Statistical Society: Series B*, **80**(3), 509–529.
[doi:10.1111/rssb.12257](https://doi.org/10.1111/rssb.12257) .

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
# \donttest{
set.seed(2026)
X <- matrix(rnorm(60 * 20), nrow = 60)
X[31:60, ] <- X[31:60, ] + 2
fmean_wrapper(X, M = 200)
#> ggcpt (changepoint detection result)
#>   Method:         fmean
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         alpha = 0.05 
#>   Series length:   60 
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value p_value
#>   <int>    <dbl>   <dbl>
#> 1    30    0.380       0
# }
```
