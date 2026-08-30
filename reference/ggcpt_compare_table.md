# Comparison table

Returns a tidy tibble combining the results of multiple detectors on the
same series.

## Usage

``` r
ggcpt_compare_table(
  x,
  methods = c("pelt", "binseg", "amoc"),
  change_in = "mean",
  ...
)
```

## Arguments

- x:

  A numeric vector (the data series). A one-column matrix or data frame
  is accepted; wider input is refused, because these detectors are
  univariate and flattening the columns would invent a changepoint at
  every seam. Use
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  for a panel of series.

- methods:

  Character vector of method names.

- change_in:

  What to detect change in.

- ...:

  Additional arguments passed to each detector.

## Value

A tibble with columns `method`, `cp`, `cp_value`.

## See also

Other plotting:
[`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md),
[`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md),
[`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md),
[`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)

## Examples

``` r
set.seed(2026)
x <- c(rnorm(100), rnorm(100, 5))
ggcpt_compare_table(x, methods = c("pelt", "binseg"))
#> # A tibble: 2 × 3
#>   method    cp cp_value
#>   <chr>  <int>    <dbl>
#> 1 pelt     100    0.369
#> 2 binseg   100    0.369
```
