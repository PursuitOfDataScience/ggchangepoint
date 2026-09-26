# Summary of a ggcpt object

Provides a human-readable digest of a changepoint detection result,
including the segment table with levels and lengths (the level is the
segment mean whatever \`change_in\` says; see
[`new_ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)),
total cost, penalty, and runtime.

## Usage

``` r
# S3 method for class 'ggcpt'
summary(object, ...)

# S3 method for class 'summary.ggcpt'
print(x, ...)
```

## Arguments

- object:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

- x:

  A `summary.ggcpt` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

## Value

A list with class `summary.ggcpt` containing the summary.

## Examples

``` r
set.seed(2026)
summary(cpt_detect(c(rnorm(60), rnorm(60, 3)), method = "pelt"))
#> ggcpt Summary
#>   Method:             pelt
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Series length:      120
#>   Penalty:            MBIC
#>   Runtime (seconds):  0.01
#> 
#> Segments:
#> # A tibble: 2 × 5
#>   seg_id start   end     n param_estimate
#>    <int> <int> <int> <int>          <dbl>
#> 1      1     1    60    60         -0.110
#> 2      2    61   120    60          2.90 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60   -0.999
```
