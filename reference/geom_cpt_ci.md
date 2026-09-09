# Changepoint confidence interval geom

Draws horizontal whiskers for changepoint-location confidence intervals
(e.g. from MOSUM, stepR, strucchange, segmented).

## Usage

``` r
geom_cpt_ci(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `y` (the height at which to draw the
  whisker) together with `xmin` and `xmax`. An `x` aesthetic is accepted
  but not needed: the layer is a horizontal error bar, so the interval
  is given by `xmin`/`xmax` and the changepoint itself is usually marked
  with a separate point layer, as `autoplot(show_ci = TRUE)` does.

- data:

  A data frame with CI information.

- ...:

  Other arguments passed to `geom_errorbar` (with `orientation = "y"`;
  [`geom_errorbarh()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  is deprecated).

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.

## See also

Other ggplot2 layers:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
ci <- data.frame(xmin = 45, xmax = 56, y = 0)
ggplot(d, aes(t, y)) + geom_line() +
  geom_cpt_ci(aes(xmin = xmin, xmax = xmax, y = y), data = ci,
              inherit.aes = FALSE, width = 0.4, colour = "blue")
```
