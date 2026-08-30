# ESAC wrapper — sparsity-adaptive high-dimensional detection

Wraps [`HDCD::ESAC()`](https://rdrr.io/pkg/HDCD/man/ESAC.html) (Moen,
Glad and Tveten, 2023): Efficient Sparsity Adaptive Changepoint
estimation for a change in the mean vector of a high-dimensional series.
Where `inspect` projects onto a single estimated sparse direction, ESAC
adapts across the whole sparsity range at once, which is a different
regime rather than a refinement of the same one — it is competitive both
when a handful of coordinates change and when all of them do.

## Usage

``` r
esac_wrapper(
  x,
  threshold_d = 1.5,
  threshold_s = 1,
  empirical = FALSE,
  N = 1000,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with rows as time points and columns as
  coordinates.

- threshold_d, threshold_s:

  Leading constants of the dense and sparse thresholds. Defaults follow
  the engine (`1.5` and `1`).

- empirical:

  Calibrate the thresholds by Monte Carlo rather than using the
  theoretical values? Slower but sharper; defaults to `FALSE`.

- N:

  Monte Carlo samples when `empirical = TRUE`.

- seed:

  Optional seed (used by the empirical calibration).

- ...:

  Additional arguments passed to
  [`HDCD::ESAC()`](https://rdrr.io/pkg/HDCD/man/ESAC.html).

## Value

A `ggcpt` object. The changepoints tibble carries `cusum` (the ESAC
statistic at each detected location) and `depth` (its level in the
recursion).

## References

Moen PAJ, Glad IK, Tveten M (2024). “Efficient sparsity adaptive
changepoint estimation.” *Electronic Journal of Statistics*, **18**(2),
3975–4038. [doi:10.1214/24-EJS2294](https://doi.org/10.1214/24-EJS2294)
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
X <- matrix(rnorm(100 * 20), nrow = 100)
X[51:100, 1:5] <- X[51:100, 1:5] + 3
esac_wrapper(X)
#> ggcpt (changepoint detection result)
#>   Method:         esac
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 1.5 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value cusum depth
#>   <int>    <dbl> <dbl> <int>
#> 1    50    0.426  3.79     1
```
