# Test if an object is a ggcpt object

A class check, useful when a function accepts either a detection result
or the raw series. It tests for `ggcpt` in the class vector, so a
genuine `ggcpt` subclass — `ggcpt_consensus` is the one — returns
`TRUE`. The other `ggcpt_*` classes in the package (`ggcpt_batch`,
`ggcpt_benchmark`, `ggcpt_monitor`, `ggcpt_selection` and the rest) are
*not* subclasses of `ggcpt` — most are tibble subclasses — and return
`FALSE`.

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
