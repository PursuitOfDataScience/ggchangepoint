# Engine runtimes by series length

One timed
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
call per engine and length, each in its own R process running an
installed (byte-compiled) build, after a warm-up call, so the time is
the fit and not loading or compiling anything, with a time cap of 120
seconds (300 at a million). A length is attempted only when the previous
one finished in under ten seconds. One run per cell: this answers "will
it finish?", and is not a basis for claiming one engine is faster than
another by a given factor.

## Usage

``` r
cpt_runtimes
```

## Format

A tibble with one row per engine and length attempted:

- method:

  the method, as
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  takes it.

- n:

  the series length: 1,000, 10,000, 100,000 or 1,000,000, with four mean
  changes of two noise standard deviations (three coordinates for the
  multivariate engines).

- seconds:

  elapsed time of the fit.

- k:

  changepoints reported (four are real; a single-change method reports
  one).

- status:

  `"ok"`, `"timeout"` or `"error"`.

## Source

`data-raw/measure_engines.R` (part `runtime`).

## See also

[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)'s
`cost` and `max_n` columns,
[`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md).

Other measurement tables:
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md),
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md),
[`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_null_sizes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_sizes.md)

## Examples

``` r
subset(cpt_runtimes, n == 1e5 & status == "ok")
#> # A tibble: 21 × 5
#>    method       n seconds     k status
#>    <chr>    <int>   <dbl> <int> <chr> 
#>  1 pelt    100000   0.071     4 ok    
#>  2 binseg  100000   0.068     4 ok    
#>  3 amoc    100000   0.042     1 ok    
#>  4 np      100000 106.        6 ok    
#>  5 fpop    100000   0.057     4 ok    
#>  6 wbs     100000   2.55      4 ok    
#>  7 not     100000   1.84      4 ok    
#>  8 mosum   100000   0.107     4 ok    
#>  9 idetect 100000   5.42      4 ok    
#> 10 tguh    100000   3.43      4 ok    
#> # ℹ 11 more rows
```
