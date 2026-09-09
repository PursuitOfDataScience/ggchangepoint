# Network-structure changepoints via non-negative matrix factorisation

Wraps
[`fabisearch::detect.cps()`](https://rdrr.io/pkg/fabisearch/man/detect.cps.html)
(Ondrus, Olds and Cribben, 2024): factorised binary search for changes
in the *network structure* of a high-dimensional series. Each candidate
split is scored by how much better a rank-\\r\\ non-negative matrix
factorisation fits the two halves separately than together, and
significance is assessed by permutation.

## Usage

``` r
fabisearch_wrapper(
  x,
  min_dist = 35,
  n_runs = 50,
  n_reps = 100,
  alpha = NULL,
  rank = NULL,
  n_core = 1,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A non-negative numeric matrix or data frame with rows as time points
  and columns as nodes. Non-negativity is a hard requirement of NMF, not
  a preference; see the section below.

- min_dist:

  Minimum distance between changepoints. Defaults to `35` (the engine's
  default), lowered automatically when the series is too short for it.

- n_runs:

  NMF runs per candidate split. Defaults to `50`.

- n_reps:

  Permutation replicates for the significance test. Defaults to `100`.

- alpha:

  Significance level applied to the permutation p-value each candidate
  split receives. Defaults to `0.05`. Note that a permutation p-value
  cannot fall below `1 / n_reps`, so `n_reps` must be at least
  `1 / alpha` for any split to be significant; the wrapper warns when it
  is not.

- rank:

  NMF rank. `NULL` estimates it with
  [`fabisearch::opt.rank()`](https://rdrr.io/pkg/fabisearch/man/opt.rank.html),
  which is expensive; supplying a rank is much faster.

- n_core:

  Cores for the permutation stage. Defaults to `1`.

- seed:

  Optional seed. The seed is scoped to this call: `.Random.seed` is
  saved and restored, so a seeded call inside a simulation loop does not
  pin the loop's own stream.

- ...:

  Additional arguments passed to
  [`fabisearch::detect.cps()`](https://rdrr.io/pkg/fabisearch/man/detect.cps.html).

## Value

A `ggcpt` object with `change_in = "network"`. Multivariate input is
reduced to **one series per observation by taking the cross-sectional
mean** of the columns, and that is the series stored on the result:
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws it, [`tidy()`](https://generics.r-lib.org/reference/tidy.html)'s
`cp_value` reads it, and `$segments$param_estimate` and
[`augment()`](https://generics.r-lib.org/reference/augment.html)'s
`.fitted`/`.resid` are computed from it. It is not any one column of the
input. The full input is kept in `$data_wide` for
`autoplot(type = "coordinates")`.

## Non-negativity, cost, and the attached namespace

Three practical notes. (1) NMF is undefined for negative entries, so
this wrapper refuses them rather than letting the engine fail deep
inside a factorisation; shift or rescale the series first if it has
negatives. (2) It is by far the most expensive engine here — `n_runs`
times `n_reps` factorisations — so the defaults are lowered in the
examples and a progress note is printed. (3) fabisearch calls NMF's
multi-run machinery, which resolves helpers through the search path and
fails with "none of the packages are loaded" when NMF is merely loaded;
this wrapper therefore attaches NMF for the duration of the call and
detaches it again afterwards.

## References

Ondrus M, Cribben I (2024). “fabisearch: a package for change point
detection in and visualization of the network structure of multivariate
high-dimensional time series in R.” *Neurocomputing*, **578**, 127321.
[doi:10.1016/j.neucom.2024.127321](https://doi.org/10.1016/j.neucom.2024.127321)
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
# \donttest{
# A change in *structure*, not in scale: two latent factors drive
# different halves of the node set before and after the change.
# Deliberately tiny -- this is by far the most expensive engine in the
# package (n_runs x n_reps factorisations per candidate split). Measured
# at 5-6 s across fresh sessions, against 27 s for the 2 x 25 /
# n_reps = 4
# version this replaced -- and this is the floor: `n_reps = 1` fails
# inside fabisearch with "not enough 'x' observations" (the permutation
# test needs two), and smaller matrices are not reliably cheaper because
# the search then evaluates more splits relative to `min_dist` (2 x 10 at
# min_dist = 8 measured 6.6 s). So this one example stays near CRAN's 5 s
# budget by necessity; `cran-comments.md` says so. Use the defaults on
# real data -- the settings here are for the budget, not for detection.
set.seed(2026)
block <- function(n, cols) {
  f <- abs(stats::rnorm(n)) + 0.5
  Y <- matrix(abs(stats::rnorm(n * 5)) * 0.2 + 0.1, n, 5)
  Y[, cols] <- Y[, cols] + f
  Y
}
Y <- rbind(block(12, 1:2), block(12, 3:5))
fabisearch_wrapper(Y, min_dist = 10, n_runs = 1, n_reps = 2,
                   alpha = 0.25, rank = 2)
#> Loading required package: foreach
#> Loading required package: rngtools
#> Warning: With `n_reps = 2` the smallest attainable permutation p-value is 0.5, which is above `alpha = 0.25`, so no split can be significant whatever the data. Raise `n_reps` to at least 4, or raise `alpha`.
#> ggcpt (changepoint detection result)
#>   Method:             fabisearch
#>   Change in:          network
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            engine alpha = 0.25
#>   Series length:      24
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    12    0.754
# }
```
