# Tidy the significance regions of a ggcpt object

The interval-valued methods (currently
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md))
return regions rather than points: each is guaranteed to contain at
least one changepoint at a prescribed *global* significance level. A
region is a different object from a confidence interval around an
estimate, so it lives in its own slot and has its own accessor rather
than being folded into
[`tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Usage

``` r
cpt_regions(x)
```

## Arguments

- x:

  A `ggcpt` object.

## Value

A tibble with columns `start`, `end` (positions), `length`, and — when
the result carries a time index — `start_index`/`end_index` on the
original scale. Any further columns the engine supplied are carried
through after those –
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
adds `value`, the region's statistic. A zero-row tibble when the result
carries no regions.

## See also

[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md).

## Examples

``` r
set.seed(2026)
fit <- as_ggcpt(50, c(rnorm(50), rnorm(50, 4)),
                regions = data.frame(start = 45, end = 56))
cpt_regions(fit)
#> # A tibble: 1 × 3
#>   start   end length
#>   <int> <int>  <int>
#> 1    45    56     12
```
