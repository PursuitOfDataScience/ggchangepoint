# A grid of simulation scenarios

Builds the scenario grid a simulation study varies over, as data rather
than as nested loops, so it can be inspected, filtered, subsetted and
passed straight to
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).

## Usage

``` r
cpt_scenarios(
  n = 500,
  jump = c(0.5, 1, 2),
  location = 0.5,
  noise = "gauss",
  rho = 0.5,
  change_in = "mean",
  n_rep = 1,
  seed = 1,
  as_datasets = TRUE
)
```

## Arguments

- n:

  Series lengths.

- jump:

  Change sizes, in standard deviations.

- location:

  Change positions, as fractions of `n`.

- noise:

  Noise models; any value
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
  accepts.

- rho:

  AR(1) parameters (used by `noise = "ar1"`).

- change_in:

  Change types.

- n_rep:

  Replicates per scenario. Defaults to `1`.

- seed:

  Base seed; replicate `r` of scenario `i` uses
  `seed + (i - 1) * n_rep + r`, so the whole grid is reproducible and
  every cell is independent. The seed is scoped to this call:
  `.Random.seed` is saved and restored, so a seeded call inside a
  simulation loop does not pin the loop's own stream.

- as_datasets:

  Return simulated datasets in
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)'s
  shape (the default), or just the scenario table?

## Value

A named list of datasets, or a tibble of scenarios when
`as_datasets = FALSE`.

## See also

[`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md),
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).

## Examples

``` r
scen <- cpt_scenarios(n = 200, jump = c(1, 3), as_datasets = FALSE)
scen
#> # A tibble: 2 × 7
#>       n  jump location noise change_in   rho scenario          
#>   <int> <dbl>    <dbl> <chr> <chr>     <dbl> <chr>             
#> 1   200     1      0.5 gauss mean          0 mean_n200_j1_gauss
#> 2   200     3      0.5 gauss mean          0 mean_n200_j3_gauss
```
