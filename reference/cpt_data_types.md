# False positives and power by data type

Ten replicates of a 400-point series with one change at 200, at
comparable effect sizes across four data types: Gaussian (a shift of 1.4
standard deviations), Bernoulli (probability 0.15 to 0.75), Poisson
(rate 2 to 10) and a proportion (binomial of 20, 0.25 to 0.6). Each
engine is at its default, as a user who does not set `family` gets it.

## Usage

``` r
cpt_data_types
```

## Format

A tibble with one row per engine and data type:

- method:

  the engine.

- data_type:

  `"gaussian"`, `"bernoulli"`, `"poisson"` or `"proportion"`.

- reps:

  replicates that ran.

- hit_rate:

  share of replicates finding the change within 20 positions.

- fp:

  mean number of other reported changepoints.

## Source

`data-raw/measure_engines.R` (part `datatype`).

## See also

Other measurement tables:
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md),
[`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md),
[`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_null_sizes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_sizes.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)

## Examples

``` r
b <- subset(cpt_data_types, data_type == "bernoulli")
b[order(-b$fp), ][1:5, ]
#> # A tibble: 5 × 5
#>   method  data_type  reps hit_rate    fp
#>   <chr>   <chr>     <int>    <dbl> <dbl>
#> 1 decafs  bernoulli    10        1 102. 
#> 2 nsp     bernoulli    10        1 102. 
#> 3 smuce   bernoulli    10        1 102. 
#> 4 wbs2    bernoulli    10        1 102. 
#> 5 idetect bernoulli    10        1  96.1
```
