# Sequential change point model wrapper (CPM)

Wraps
[`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html)
(Ross, 2015) for distribution-free sequential changepoint detection via
repeated two-sample tests (Mann-Whitney for location, Mood for scale,
Lepage, Kolmogorov-Smirnov and Cramer-von-Mises for general changes, and
parametric Student/Bartlett/GLR variants). Although the engine is
designed for streams, it is run here over the full series in one pass,
mimicking online monitoring with average run length `arl0`.

## Usage

``` r
cpm_wrapper(x, cpm_type = "Mann-Whitney", arl0 = 500, startup = 20, ...)
```

## Arguments

- x:

  A numeric vector.

- cpm_type:

  Test statistic, passed to
  [`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html)
  as `cpmType`. Distribution-free: `"Mann-Whitney"` (location, the
  default), `"Mood"` (scale), `"Lepage"`, `"Kolmogorov-Smirnov"`,
  `"Cramer-von-Mises"`. Parametric: `"Student"`, `"Bartlett"`, `"GLR"`
  (Gaussian), `"Exponential"` (positive data), `"FET"` (Fisher's exact
  test, for 0/1 Bernoulli data — this one also needs a `lambda` value
  passed through `...`, e.g. `lambda = 0.3`).

- arl0:

  Target in-control average run length (how many observations, on
  average, before a false alarm). Defaults to `500`. cpm ships
  thresholds only for 100, 200, 370, 400, 500, 600, 700, 1000, 2000,
  5000, 10000 and 20000; any other value is refused, because the engine
  answers it by printing an error and reporting no changepoints.

- startup:

  Number of observations after each restart before monitoring begins.
  Defaults to `20`.

- ...:

  Additional arguments passed to
  [`cpm::processStream()`](https://rdrr.io/pkg/cpm/man/processStream.html).

## Value

A `ggcpt` object. The `changepoints` tibble carries a `detection_time`
column: the index at which the sequential test flagged each change
(always later than the estimated location).

## References

Ross GJ (2015). “Parametric and nonparametric sequential change
detection in R: The cpm package.” *Journal of Statistical Software*,
**66**(3), 1–20.

## See also

Other changepoint engines:
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
[`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md),
[`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md),
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
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
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
res <- cpm_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value detection_time
#>   <int>    <dbl>          <int>
#> 1   100   -0.125            104
```
