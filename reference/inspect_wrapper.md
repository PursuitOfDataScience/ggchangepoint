# inspect wrapper — high-dimensional changepoints via sparse projection

Wraps
[`InspectChangepoint::inspect()`](https://rdrr.io/pkg/InspectChangepoint/man/inspect.html)
(Wang and Samworth, 2018). For a \\p\\-variate series whose mean changes
in an unknown sparse subset of coordinates, the algorithm computes the
CUSUM transformation, finds the optimal sparse projection direction via
a convex relaxation, and locates changepoints on the projected
univariate series, recursing via wild binary segmentation.

## Usage

``` r
inspect_wrapper(x, lambda = NULL, threshold = NULL, ...)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per coordinate.

- lambda:

  Regularisation parameter of the sparse projection; when `NULL` the
  engine default \\\sqrt{\log(p \log n)/2}\\ is used.

- threshold:

  Detection threshold; when `NULL` it is computed by Monte Carlo (via
  the engine).

- ...:

  Additional arguments passed to
  [`InspectChangepoint::inspect()`](https://rdrr.io/pkg/InspectChangepoint/man/inspect.html).

## Value

A `ggcpt` object. The changepoints tibble carries a `strength` column
(the maximum projected CUSUM statistic). The first coordinate is used
for `cp_value` and the univariate plot line; the full matrix is kept for
the faceted multivariate
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
Coordinates that are constant carry no changepoint information and would
make the engine's variance rescaling undefined, so they are dropped
(with a warning) before detection and an all-constant matrix returns an
empty result; the dropped coordinates are still kept for plotting, and
reported locations always refer to the original rows.

## References

Wang T, Samworth RJ (2018). “High dimensional change point estimation
via sparse projection.” *Journal of the Royal Statistical Society:
Series B*, **80**(1), 57–83.

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
X <- cbind(c(rnorm(80), rnorm(80, 3)), c(rnorm(80), rnorm(80, -2)),
           rnorm(160))
res <- inspect_wrapper(X)
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
#> Loading required namespace: RSpectra
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value strength
#>   <int>    <dbl>    <dbl>
#> 1    80    0.785     21.9
```
