# Print a ggcpt object

A compact header – method, what changed, how many changepoints, the
convention their locations follow, the penalty and the series length –
followed by the first ten changepoints. For the segment table and the
fitted parameters use
[`summary()`](https://rdrr.io/r/base/summary.html); for the changepoints
as data use [`tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Usage

``` r
# S3 method for class 'ggcpt'
print(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly. Called for the side effect of printing.

## See also

[`summary.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/summary.ggcpt.md),
[`tidy.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tidy.ggcpt.md).

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)

## Examples

``` r
set.seed(2026)
print(cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt"))
#> ggcpt (changepoint detection result)
#>   Method:             pelt
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            MBIC
#>   Series length:      80
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    40   -0.825
```
