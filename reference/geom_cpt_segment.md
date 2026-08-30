# Changepoint segment level geom

Draws horizontal segments representing the estimated level of each
segment between changepoints. Typically used with data from
[`augment()`](https://generics.r-lib.org/reference/augment.html).

## Usage

``` r
geom_cpt_segment(
  mapping = NULL,
  data = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA
)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `x`, `xend`, `y`, `yend`.

- data:

  A data frame with segment information.

- ...:

  Other arguments passed to `geom_segment`.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.

## See also

Other ggplot2 layers:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 4)), method = "pelt")
ggplot(fit$data, aes(index, value)) + geom_line(colour = "grey70") +
  geom_cpt_segment(aes(x = start, xend = end, y = param_estimate,
                       yend = param_estimate),
                   data = fit$segments, colour = "blue", linewidth = 1)
```
