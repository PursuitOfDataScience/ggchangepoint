# High-dimensional covariance changepoints

Wraps
[`changepoints::BS.cov()`](https://rdrr.io/pkg/changepoints/man/BS.cov.html)
(Wang, Yu and Rinaldo): binary segmentation on the sample covariance
operator, which detects a change in the *dependence structure* of a
multivariate series even when every marginal mean and variance is
unchanged. No other engine in the package can see that.

## Usage

``` r
hdcov_wrapper(
  x,
  threshold = NULL,
  alpha = 0.05,
  n_perm = 20,
  delta = NULL,
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame, rows as time points.

- threshold:

  Detection threshold on the CUSUM statistic. When `NULL` (the default)
  it is calibrated by permutation: the time order is shuffled `n_perm`
  times, which destroys any changepoint while preserving the marginal
  distributions, and the threshold is the `1 - alpha` quantile of the
  largest statistic seen.

- alpha:

  Family-wise level for the permutation threshold. Defaults to `0.05`.

- n_perm:

  Permutations used to calibrate the threshold. Defaults to `20`; raise
  it for a sharper threshold at proportional cost.

- delta:

  Minimum spacing between changepoints. Defaults to
  `max(10, floor(n / 20))`.

- seed:

  Optional seed (the permutation calibration is random).

## Value

A `ggcpt` object with `change_in = "covariance"`; the changepoints
tibble carries the CUSUM statistic in `cusum`.

## References

Wang D, Yu Y, Rinaldo A (2021). “Optimal covariance change point
localization in high dimensions.” *Bernoulli*, **27**(1), 554–575.
[doi:10.3150/20-BEJ1249](https://doi.org/10.3150/20-BEJ1249) .

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
p <- 5
A <- matrix(rnorm(100 * p), ncol = p)
B <- matrix(rnorm(100 * p), ncol = p)
B[, 2] <- B[, 1] + 0.2 * B[, 2]        # correlation appears
hdcov_wrapper(rbind(A, B), n_perm = 20, alpha = 0.05, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:         hdcov
#>   Change in:       covariance 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 7.666 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value cusum
#>   <int>    <dbl> <dbl>
#> 1    93   -0.475  8.22
```
