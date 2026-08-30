# Changepoint wrapper

This function wraps a number of `cpt` functions from the changepoint
package and the `cpt.np()` function from the changepoint.np package. It
is handy that users can use this function to get the same changepoint
results as these functions output individually. Moreover, it returns a
tibble that inherits the tidyverse style. Functions from the changepoint
package do require data normality assumption by default, yet
changepoint.np is a non-parametric way to detect changepoints and let
data speak by itself. If user sets `change_in` as `np` (or `cpt_np`), a
seed should be set before using the function for the sake of
reproducibility. For more details on the changepoint and changepoint.np
packages, please refer to their documentation.

## Usage

``` r
cpt_wrapper(data, change_in = "mean_var", cp_method = "PELT", ...)
```

## Arguments

- data:

  A numeric vector.

- change_in:

  Choice of `mean_var`, `mean`, `var`, and `np` (or `cpt_np` for
  backward compatibility). Each choice corresponds to `cpt.meanvar()`,
  `cpt.mean()`, `cpt.var()` and `cpt.np()` respectively. The default is
  `mean_var`.

- cp_method:

  A wide range of choices (i.e., `AMOC`, `PELT`, `SegNeigh` or
  `BinSeg`). Please note when `change_in` is `np` or `cpt_np`, `PELT` is
  the only option.

- ...:

  Extra arguments for each `cpt` function mentioned in the `change_in`
  section.

## Value

A tibble including which point(s) is/are the changepoint along with raw
changepoint value corresponding to that changepoint. Changepoint
locations follow the convention of the `changepoint` package: the last
index of the left segment. The upstream `cpt` object is attached as the
`"ggcpt_fit"` attribute, which is what
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
stores in the result's `$fit`.

## Standardise the data for a change in mean

With `change_in = "mean"` the upstream Normal cost assumes a noise
standard deviation of 1 and the penalty is compared against the raw
residual sum of squares, so a series with wider noise is under-penalised
and over-segmented: 29 changepoints instead of 1 at \\\sigma = 3\\ in a
measured example. Standardise the series first, or use
`change_in = "mean_var"`, which estimates a variance per segment and is
unaffected. See the scale-sensitivity section of
[`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

## References

Killick R, Eckley I (2014). “changepoint: An R package for changepoint
analysis.” *Journal of statistical software*, **58**(3), 1–19.

## See also

Other changepoint engines:
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
[`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md),
[`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md),
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
[`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md),
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
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
set.seed(2022)
cpt_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
cpt_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.225
```
