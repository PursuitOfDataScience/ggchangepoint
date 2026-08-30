# Significance region geom

Draws a changepoint that is an *interval*: a vertical band spanning
`xmin` to `xmax` and the full height of the panel. This is the display
Narrowest Significance Pursuit needs
([`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)):
NSP returns intervals each of which contains at least one changepoint at
a prescribed *global* significance level, which is neither a point
estimate nor a confidence interval around one.
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
adds this layer automatically for a result that carries regions.

## Usage

``` r
geom_cpt_region(
  mapping = NULL,
  data = NULL,
  ...,
  alpha = 0.2,
  fill = "steelblue",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `xmin` and `xmax`; `ymin`/`ymax` default
  to the panel extent, so they need not be supplied.

- data:

  A data frame of regions, e.g.
  [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md)
  output.

- ...:

  Other arguments passed to
  [`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

- alpha:

  Fill transparency. Defaults to `0.2` — light enough that the series
  stays readable through overlapping bands.

- fill:

  Band fill colour. Defaults to `"steelblue"`.

- na.rm:

  If `FALSE`, missing values are removed with a warning.

- show.legend:

  Whether to show a legend.

- inherit.aes:

  Whether to inherit the plot's aesthetics. Defaults to `FALSE`: a
  region frame has its own columns and nothing to do with the series'
  `x`/`y`.

## Value

A ggplot layer.

## See also

[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md),
[`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md).

Other ggplot2 layers:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
regions <- data.frame(xmin = 44, xmax = 57)
ggplot(d, aes(t, y)) +
  geom_cpt_region(aes(xmin = xmin, xmax = xmax), data = regions) +
  geom_line()
```
