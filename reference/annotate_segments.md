# Annotate segments with alternating shading

Adds alternating shaded rectangles to highlight segments between
changepoints.

## Usage

``` r
annotate_segments(cp, n, fill = c("grey90", "white"), alpha = 0.5, ...)
```

## Arguments

- cp:

  Changepoint indices (including 0 and n).

- n:

  Length of the series.

- fill:

  Colors for alternating segments. Defaults to c("grey90", "white").

- alpha:

  Alpha for fill. Defaults to 0.5.

- ...:

  Additional arguments passed to `annotate`.

## Value

A list of ggplot annotations.

## See also

Other result class:
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 4)), method = "pelt")
ggplot(fit$data, aes(index, value)) +
  annotate_segments(fit$changepoints$cp, n = nrow(fit$data)) +
  geom_line()
```
