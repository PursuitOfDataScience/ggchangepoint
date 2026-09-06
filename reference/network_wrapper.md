# Dynamic-network changepoints

Wraps
[`changepoints::WBS.network()`](https://rdrr.io/pkg/changepoints/man/WBS.network.html)
(Yu, Padilla, Wang and Rinaldo): wild binary segmentation on a sequence
of networks, detecting the times at which the edge-probability structure
changes. The input is one row per time point holding the *vectorised
adjacency matrix*, so a series of \\p \times p\\ networks over \\n\\
times is an \\n \times p^2\\ matrix.

## Usage

``` r
network_wrapper(
  x,
  copy2 = NULL,
  n_intervals = 100,
  threshold = NULL,
  alpha = 0.05,
  n_perm = 20,
  delta = NULL,
  seed = NULL
)
```

## Arguments

- x:

  The network sequence: an \\n \times p^2\\ matrix of vectorised
  adjacency matrices, or an \\n \times p \times p\\ array.

- copy2:

  An independent second observation of the same network sequence, in the
  same shape. The method's guarantees rest on sample splitting; see the
  section below for what happens when there is only one copy.

- n_intervals:

  Number of random intervals. Defaults to `100`.

- threshold:

  Detection threshold. When `NULL`, calibrated by permutation exactly as
  in
  [`hdcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdcov_wrapper.md).

- alpha, n_perm:

  Level and number of permutations for that calibration.

- delta:

  Minimum spacing. Defaults to `max(5, floor(n / 20))`.

- seed:

  Optional seed.

## Value

A `ggcpt` object with `change_in = "network"`. The series it carries –
and so the one
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws – is the **mean edge weight** at each time point,
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) of the vectorised
adjacency matrices. This is the one multivariate method with no
`data_wide` slot: a \\p \times p\\ network has \\p^2\\ entries per time
point, so a facet per coordinate would be unreadable. The changepoints
are estimated from the networks themselves, not from the summary.

## When you have only one copy of the network

`WBS.network()` takes two independent observations of the sequence,
which is how the theory controls the bias of the squared-Frobenius
statistic. Given a single sequence of *binary* networks this wrapper
constructs the second copy by splitting each edge indicator at random
(each present edge is assigned to one copy with probability one half),
which is the usual independent-thinning device and is reported in a
message. For weighted networks it splits the weight instead, which is
exact for Poisson weights and approximate otherwise. If you have a
genuine replicate, pass it as `copy2` and none of this applies.

## References

Yu Y, Padilla OHM, Wang D, Rinaldo A (2021). “Optimal network online
change point localisation.” *arXiv preprint arXiv:2101.05477*.
[doi:10.48550/arXiv.2101.05477](https://doi.org/10.48550/arXiv.2101.05477)
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
mk <- function(n, prob) {
  t(replicate(n, as.numeric(matrix(stats::rbinom(p * p, 1, prob), p))))
}
X <- rbind(mk(40, 0.2), mk(40, 0.6))
network_wrapper(X, n_intervals = 20, n_perm = 20, seed = 1)
#> No independent second observation supplied: splitting each edge at random to build one. See the "When you have only one copy" section of ?network_wrapper.
#> ggcpt (changepoint detection result)
#>   Method:         network
#>   Change in:       network 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 3.2294 
#>   Series length:   80 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    40     0.08
```
