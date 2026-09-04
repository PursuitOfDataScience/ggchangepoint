# FPOP wrapper — Functional Pruning Optimal Partitioning

Wraps the `fpop` package for optimal changepoint detection via
functional pruning.

## Usage

``` r
fpop_wrapper(x, penalty = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- penalty:

  Penalty value. Defaults to `2 * log(length(x))` (BIC). This is an
  *absolute* penalty on the residual sum of squares, so it is only
  calibrated for noise of standard deviation 1: on wider data the
  default under-penalises badly and the segmentation shatters.
  Standardise the series, or scale the penalty by the noise variance
  (for example `2 * log(length(x)) * stats::var(diff(x)) / 2`). See the
  scale-sensitivity section of
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  This default differs from the one
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  applies, which resolves its `"MBIC"` default to a stronger numeric
  value, so the two entry points need not agree unless `penalty` is
  given.

- ...:

  Additional arguments passed to
  [`fpop::Fpop()`](https://rdrr.io/pkg/fpop/man/Fpop.html).

## Value

A `ggcpt` object.

## References

Maidstone R, Hocking T, Rigaill G, Fearnhead P (2017). “On Optimal
Multiple Changepoint Algorithms for Large Data.” *Statistics and
Computing*, **27**(2), 519–533.
[doi:10.1007/s11222-016-9636-3](https://doi.org/10.1007/s11222-016-9636-3)
.

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
x <- c(rnorm(60), rnorm(60, 4))
fpop_wrapper(x)$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60   -0.999
# the default penalty assumes sd 1; scale it for wider noise
y <- 5 * x
fpop_wrapper(y, penalty = log(length(y)) * var(diff(y)))$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60    -5.00
```
