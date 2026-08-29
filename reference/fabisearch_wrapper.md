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

  Optional seed.

- ...:

  Additional arguments passed to
  [`fabisearch::detect.cps()`](https://rdrr.io/pkg/fabisearch/man/detect.cps.html).

## Value

A `ggcpt` object with `change_in = "network"`.

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

## Examples

``` r
# \donttest{
# A change in *structure*, not in scale: two latent factors drive
# different halves of the node set before and after the change.
# Deliberately tiny -- this is by far the most expensive engine in the
# package (n_runs x n_reps factorisations per candidate split), and the
# settings below are chosen to keep the example inside a check budget,
# not to detect anything. Use the defaults on real data.
set.seed(2026)
block <- function(n, cols) {
  f <- abs(stats::rnorm(n)) + 0.5
  Y <- matrix(abs(stats::rnorm(n * 5)) * 0.2 + 0.1, n, 5)
  Y[, cols] <- Y[, cols] + f
  Y
}
Y <- rbind(block(25, 1:2), block(25, 3:5))
fabisearch_wrapper(Y, min_dist = 10, n_runs = 1, n_reps = 4,
                   alpha = 0.25, rank = 2)
#> Loading required package: foreach
#> Loading required package: rngtools
#> ggcpt (changepoint detection result)
#>   Method:         fabisearch
#>   Change in:       network 
#>   Changepoints found: 0 
#>   CP convention:   left 
#>   Penalty:         engine alpha = 0.25 
#>   Series length:   50 
#> 
#> No changepoints detected.
# }
```
