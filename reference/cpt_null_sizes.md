# False alarms on pure noise at scale

Independent Gaussian noise with no changepoint anywhere, at 1,000 (50
replicates), 10,000 (10) and 100,000 (6) observations, for the engines
the runtime table shows finishing that length quickly. Every changepoint
reported is a false alarm.

## Usage

``` r
cpt_null_sizes
```

## Format

A tibble with one row per engine and length:

- method:

  the engine.

- n:

  the series length.

- reps:

  replicates that ran.

- size:

  share of replicates reporting at least one changepoint.

- mean_fp:

  mean number of changepoints reported.

## Source

`data-raw/measure_engines.R` (part `null`).

## See also

Other measurement tables:
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md),
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md),
[`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)

## Examples

``` r
subset(cpt_null_sizes, n == max(n))
#> # A tibble: 12 × 5
#>    method          n  reps  size mean_fp
#>    <chr>       <int> <int> <dbl>   <dbl>
#>  1 amoc       100000     6     0       0
#>  2 binseg     100000     6     0       0
#>  3 binsegrcpp 100000     6     0       0
#>  4 decafs     100000     6     0       0
#>  5 fastcpd    100000     6     0       0
#>  6 fpop       100000     6     0       0
#>  7 mosum      100000     6     0       0
#>  8 not        100000     6     0       0
#>  9 pelt       100000     6     0       0
#> 10 segmented  100000     6     1       1
#> 11 tguh       100000     6     0       0
#> 12 wbs        100000     6     0       0
```
