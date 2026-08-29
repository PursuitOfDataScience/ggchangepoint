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

A `ggcpt` object with `change_in = "network"`.

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
