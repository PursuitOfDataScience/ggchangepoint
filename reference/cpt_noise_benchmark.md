# False positives and power under four noise regimes

Ten replicates of a 300-point series with mean changes of two marginal
standard deviations at 100 and 200, under independent Gaussian noise,
heavy-tailed noise (\\t_3\\, scaled to unit variance), AR(1) noise with
\\\rho = 0.7\\ and heteroscedastic noise (standard deviations 0.5, 1 and
2.5 by segment), for every univariate engine at its default and at each
setting of its noise-model argument. A recommendation is a call, not a
name: `smuce` and `smuce` with `family = "hsmuce"` are different
detectors.

## Usage

``` r
cpt_noise_benchmark
```

## Format

A tibble with one row per engine, setting and regime:

- method, setting:

  the engine and the argument setting (`"default"` or the argument as
  written).

- call:

  the
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  call the row measures.

- regime:

  `"iid"`, `"heavy"`, `"ar1"` or `"hetero"`.

- reps:

  replicates that ran.

- hits:

  mean number of the two real changes found within ten positions.

- fp:

  mean number of reported changepoints more than ten positions from
  either.

## Source

`data-raw/measure_engines.R` (part `noise`).

## See also

Other measurement tables:
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md),
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md),
[`cpt_null_sizes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_sizes.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)

## Examples

``` r
ar <- subset(cpt_noise_benchmark, regime == "ar1")
ar[order(ar$fp), c("call", "hits", "fp")]
#> # A tibble: 42 × 3
#>    call                                                    hits    fp
#>    <chr>                                                  <dbl> <dbl>
#>  1 "cpt_detect(x, method = \"fastcpd\", family = \"ar\")"   0.2   0  
#>  2 "cpt_detect(x, method = \"wbsts\")"                      0     0  
#>  3 "cpt_detect(x, method = \"nsp\", variant = \"ar\")"      0     0.1
#>  4 "cpt_detect(x, method = \"pettitt\")"                    0.8   0.2
#>  5 "cpt_detect(x, method = \"buishand\")"                   0.7   0.3
#>  6 "cpt_detect(x, method = \"decafs\")"                     0.5   0.3
#>  7 "cpt_detect(x, method = \"kcp\")"                        1.9   0.3
#>  8 "cpt_detect(x, method = \"amoc\")"                       0.6   0.4
#>  9 "cpt_detect(x, method = \"envcpt\")"                     0.4   0.4
#> 10 "cpt_detect(x, method = \"snht\")"                       0.6   0.4
#> # ℹ 32 more rows
```
