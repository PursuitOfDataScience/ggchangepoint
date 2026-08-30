# EnvCpt wrapper — changepoints versus trends versus autocorrelation

Wraps [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html)
(Beaulieu and Killick, 2018), which fits up to twelve competing models —
constant mean or linear trend, each with or without changepoints, and
with white-noise, AR(1) or AR(2) errors — and lets an information
criterion decide whether the series really contains changepoints or
merely trend/autocorrelation ("memory"). The changepoints of the winning
model (if any) are returned, and the winning model's name is recorded,
guarding against the classic false positive of running a mean-shift
detector on autocorrelated data.

## Usage

``` r
envcpt_wrapper(
  x,
  models = c("mean", "meancpt", "meanar1", "meanar2", "meanar1cpt", "meanar2cpt",
    "trend", "trendcpt", "trendar1", "trendar2", "trendar1cpt", "trendar2cpt"),
  criterion = c("AIC", "BIC"),
  minseglen = 5,
  ...
)
```

## Arguments

- x:

  A numeric vector.

- models:

  Character vector of models to fit; see
  [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html).
  Defaults to all twelve.

- criterion:

  Model selection criterion: `"AIC"` (default) or `"BIC"`.

- minseglen:

  Minimum segment length. Defaults to `5`.

- ...:

  Additional arguments passed to
  [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html).

## Value

A `ggcpt` object. `$fit` holds the full `envcpt` output; the selected
model name is stored in the penalty descriptor and printed by
[`glance()`](https://generics.r-lib.org/reference/glance.html) via
`penalty_type`. Individual model fits that fail are expected — the
criterion ignores them — so the engine's own
[`try()`](https://rdrr.io/r/base/try.html) output is not passed on;
genuine warnings still are, and a series on which no model fits at all
raises an error.

## References

Beaulieu C, Killick R (2018). “Distinguishing trends and shifts from
memory in climate data.” *Journal of Climate*, **31**(23), 9519–9543.

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
set.seed(2026)
res <- envcpt_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    98   -0.490
```
