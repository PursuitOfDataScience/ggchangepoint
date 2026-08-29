# Fast binary segmentation across loss functions

Wraps
[`binsegRcpp::binseg()`](https://rdrr.io/pkg/binsegRcpp/man/binseg.html)
(Hocking): a C++ binary segmentation that runs in \\O(n \log n)\\ for
the best case and supports several loss functions, including ones no
other engine here offers (Poisson, \\\ell_1\\). It is the package's
*performance* path for binary segmentation on long series, and it
returns the whole nested family of segmentations, so it also feeds
[`cpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md).

## Usage

``` r
binsegrcpp_wrapper(
  x,
  change_in = c("mean", "meanvar"),
  distribution = NULL,
  max_segments = NULL,
  n_segments = NULL,
  min_segment_length = NULL
)
```

## Arguments

- x:

  A numeric vector.

- change_in:

  `"mean"` (Gaussian, the default) or `"meanvar"`, mapped to the
  engine's `mean_norm` and `meanvar_norm` distributions. binsegRcpp has
  no variance-only cost, so `"var"` is not offered here — use
  `"meanvar"`, or
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)`(method = "pelt", change_in = "var")`
  for a variance-only change.

- distribution:

  Loss function, overriding the mapping from `change_in`. Run
  [`binsegRcpp::get_distribution_info()`](https://rdrr.io/pkg/binsegRcpp/man/get_distribution_info.html)
  for the list (`"mean_norm"`, `"meanvar_norm"`, `"poisson"`, `"l1"`,
  ...).

- max_segments:

  Largest number of segments searched. Defaults to
  `min(20, floor(n / 5))`.

- n_segments:

  Number of segments to report. When `NULL` (the default) it is chosen
  by BIC over the nested family the engine returns.

- min_segment_length:

  Minimum segment length. Passed through when supplied.

## Value

A `ggcpt` object.

## References

Hocking TD (2024). “Finite sample complexity analysis of binary
segmentation.” *arXiv preprint arXiv:2410.08654*.
[doi:10.48550/arXiv.2410.08654](https://doi.org/10.48550/arXiv.2410.08654)
.

## Examples

``` r
set.seed(2026)
binsegrcpp_wrapper(c(rnorm(100), rnorm(100, 3)))
#> ggcpt (changepoint detection result)
#>   Method:         binsegrcpp
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         BIC over nested family = 2 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```
