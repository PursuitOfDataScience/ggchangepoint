# Test if an object is a ggcpt object

A class check, useful when a function accepts either a detection result
or the raw series. It tests the class only; a `ggcpt` subclass such as
`ggcpt_batch` is not one of these and returns `FALSE`.

## Usage

``` r
is_ggcpt(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` inherits from `ggcpt`.

## See also

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
is_ggcpt(fit)
#> [1] TRUE
is_ggcpt(fit$changepoints)
#> [1] FALSE
```
