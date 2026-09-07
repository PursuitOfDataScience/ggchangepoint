# Taylor's change point analyzer

Wraps
[`ChangePointTaylor::change_point_analyzer()`](https://rdrr.io/pkg/ChangePointTaylor/man/change_point_analyzer.html):
the bootstrap-and-recursion procedure of Wayne Taylor that the
quality-control and Six Sigma community uses as its default. Each
candidate is scored by the bootstrap probability that a change occurred
there, which gives a confidence level per changepoint and a confidence
interval for its location — both carried onto the result.

## Usage

``` r
taylor_wrapper(
  x,
  n_bootstraps = 1000,
  min_candidate_conf = 0.5,
  min_conf = 0.9,
  conf_level = 0.95,
  seed = NULL
)
```

## Arguments

- x:

  A numeric vector.

- n_bootstraps:

  Bootstrap samples per candidate. Defaults to `1000`; the engine
  accepts 100 to 1,000,000.

- min_candidate_conf:

  Minimum confidence for a candidate to be considered. Defaults to
  `0.5`.

- min_conf:

  Minimum confidence for a changepoint to be reported. Defaults to
  `0.9`.

- conf_level:

  Confidence level of the reported location intervals. Defaults to
  `0.95`.

- seed:

  Optional seed (the procedure is bootstrap-based). The seed is scoped
  to this call: `.Random.seed` is saved and restored, so a seeded call
  inside a simulation loop does not pin the loop's own stream.

## Value

A `ggcpt` object with `ci_lower`/`ci_upper` (so
`autoplot(show_ci = TRUE)` works) and a `confidence` column.

## Series length, and why you cannot interrupt it

This engine is written for the series lengths quality control sees –
hundreds to low thousands – and it does not scale. At \\n = 10{,}000\\
with the default `n_bootstraps = 1000` it runs for **minutes**, and more
importantly it runs where R cannot look: a
[`setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html) of 45
seconds was still not honoured after 170, and one of 125 seconds after
200, so the call had to be killed from outside the session. R checks
elapsed-time limits and keyboard interrupts at the same points, which
means **Ctrl-C will not stop it either**.

So size the call before starting it rather than after. `n_bootstraps` is
the knob – the cost is roughly linear in it – and the “Benchmarks”
article lists the methods that do scale to long series. That page is
web-only, because the sweep behind it takes over twenty minutes: it is
published at
<https://pursuitofdatascience.github.io/ggchangepoint/articles/benchmarks.html>
rather than built into the package, so
[`vignette()`](https://rdrr.io/r/utils/vignette.html) will not find it.

## References

Taylor WA (2000). *Change-Point Analysis: A Powerful New Tool for
Detecting Changes*. Taylor Enterprises, Libertyville, Illinois.

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
[`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md),
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
set.seed(2026)
taylor_wrapper(c(rnorm(60), rnorm(60, 3)), n_bootstraps = 200, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:             taylor
#>   Change in:          mean
#>   Changepoints found: 2
#>   CP convention:      left
#>   Penalty:            confidence = 0.9
#>   Series length:      120
#> 
#> Changepoints:
#> # A tibble: 2 × 5
#>      cp cp_value ci_lower ci_upper confidence
#>   <int>    <dbl>    <int>    <int>      <dbl>
#> 1    15   -2.55         6       50          1
#> 2    60   -0.999       59       61          1
```
