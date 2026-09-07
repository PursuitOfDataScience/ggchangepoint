# Classical single-changepoint tests (Pettitt, Buishand, SNHT)

Wraps the three single-change tests that hydrology and climatology use
as their standard vocabulary, from the trend package. All three test
\\H_0\\: no change against a single change in the mean, differing in how
they measure it:

- `"pettitt"`:

  a rank-based (Mann–Whitney) statistic — distribution-free and robust
  to outliers.

- `"buishand"`:

  the Buishand range test, based on rescaled adjusted partial sums;
  assumes normality.

- `"snht"`:

  the standard normal homogeneity test of Alexandersson, the reference
  method for detecting inhomogeneities in climate records.

Each reports a location *and* a p-value, and — unlike most engines here
— that p-value is valid, because the location was not chosen from a
larger model search.

## Usage

``` r
trend_wrapper(x, test = c("pettitt", "buishand", "snht"), alpha = 0.05, ...)
```

## Arguments

- x:

  A numeric vector.

- test:

  Which test to run. Defaults to `"pettitt"`.

- alpha:

  Significance level below which the changepoint is reported. Defaults
  to `0.05`. A test that does not reject returns an empty result rather
  than an unsupported location.

- ...:

  Additional arguments passed to the trend function.

## Value

A `ggcpt` object with the test's `p_value` and `statistic` on the
changepoints tibble. The per-position test statistic is available
through
[`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md).

## References

Pettitt AN (1979). “A non-parametric approach to the change-point
problem.” *Journal of the Royal Statistical Society: Series C*,
**28**(2), 126–135.
[doi:10.2307/2346729](https://doi.org/10.2307/2346729) .

Buishand TA (1982). “Some methods for testing the homogeneity of
rainfall records.” *Journal of Hydrology*, **58**(1–2), 11–27.
[doi:10.1016/0022-1694(82)90066-X](https://doi.org/10.1016/0022-1694%2882%2990066-X)
.

Alexandersson H (1986). “A homogeneity test applied to precipitation
data.” *Journal of Climatology*, **6**(6), 661–675.
[doi:10.1002/joc.3370060607](https://doi.org/10.1002/joc.3370060607) .

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
[`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md),
[`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md),
[`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md),
[`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md),
[`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
set.seed(2026)
x <- c(rnorm(60), rnorm(60, 2))
trend_wrapper(x, test = "pettitt")
#> ggcpt (changepoint detection result)
#>   Method:             pettitt
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            alpha = 0.05
#>   Series length:      120
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value  p_value statistic
#>   <int>    <dbl>    <dbl>     <dbl>
#> 1    60   -0.999 1.54e-14      3072
trend_wrapper(x, test = "snht")
#> ggcpt (changepoint detection result)
#>   Method:             snht
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            alpha = 0.05
#>   Series length:      120
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value p_value statistic
#>   <int>    <dbl>   <dbl>     <dbl>
#> 1    60   -0.999       0      59.1
```
