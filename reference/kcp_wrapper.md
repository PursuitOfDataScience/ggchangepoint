# Kernel changepoint wrapper (KCP on running statistics)

Wraps [`kcpRS::kcpRS()`](https://rdrr.io/pkg/kcpRS/man/kcpRS.html)
(Cabrieto et al., 2018; the KCP framework of Arlot, Celisse and
Harchaoui, 2019). The data are mapped to a running statistic (mean,
variance, autocorrelation, or correlation) computed on a sliding window,
and a Gaussian-kernel change point analysis with a permutation
significance test is run on the statistic. Detecting changes in running
correlations or variances captures higher-order changes that mean-based
methods miss. Multivariate input (matrix or data frame) is supported.

## Usage

``` r
kcp_wrapper(
  x,
  running_stat = c("mean", "var", "autocorr", "corr"),
  wsize = 25,
  nperm = 1000,
  kmax = 10,
  alpha = 0.05,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame (columns are variables).

- running_stat:

  Which running statistic to monitor: `"mean"`, `"var"`, `"autocorr"`,
  or `"corr"` (correlation requires at least two columns). Defaults to
  `"mean"`.

- wsize:

  Sliding window size for the running statistic. Defaults to `25`.

- nperm:

  Number of permutations for the significance test, at least 2. Defaults
  to `1000`. Fewer than two leaves the engine with no permutation
  distribution: it reports no changepoints at all for 0, and fails
  inside its own code for 1.

- kmax:

  Maximum number of changepoints considered. Defaults to `10`.

- alpha:

  Significance level of the permutation test. Defaults to `0.05`.

- seed:

  Optional seed for reproducibility of the permutation test. The seed is
  scoped to this call: `.Random.seed` is saved and restored, so a seeded
  call inside a simulation loop does not pin the loop's own stream.

- ...:

  Additional arguments passed to
  [`kcpRS::kcpRS()`](https://rdrr.io/pkg/kcpRS/man/kcpRS.html).

## Value

A `ggcpt` object. Reported locations refer to the centre of the sliding
window in which the change occurs. The series must be at least `wsize`
long to form one window. Constant coordinates make every running
statistic `NA`, so they are dropped (with a warning) before detection
and an all-constant input returns an empty result.

## References

Arlot S, Celisse A, Harchaoui Z (2019). “A kernel multiple change-point
algorithm via model selection.” *Journal of Machine Learning Research*,
**20**(162), 1–56.

Cabrieto J, Adolf J, Tuerlinckx F, Kuppens P, Ceulemans E (2018).
“Detecting long-lived autodependency changes in a multivariate system
via change point detection and regime switching models.” *Scientific
Reports*, **8**, 15637.

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
res <- kcp_wrapper(c(rnorm(60), rnorm(60, 3)), nperm = 100, seed = 2026)
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    59    0.949
```
