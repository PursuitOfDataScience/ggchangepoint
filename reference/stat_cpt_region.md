# Significance regions computed in the layer

The region twin of
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md):
runs detection on the layer's own `x`/`y` and draws what the method
returns as an interval, so
`ggplot(d, aes(t, y)) + geom_line() + stat_cpt_region()` is the whole
plot. Narrowest Significance Pursuit (`method = "nsp"`, the default)
returns regions, each containing a changepoint at a global significance
level; a method with location intervals (`"smuce"`, `"strucchange"`,
`"segmented"`, ...) draws those instead.

## Usage

``` r
stat_cpt_region(
  mapping = NULL,
  data = NULL,
  geom = GeomCptRegion,
  position = "identity",
  ...,
  method = "nsp",
  na.rm = FALSE,
  show.legend = NA
)
```

## Arguments

- mapping:

  Aesthetic mappings.

- data:

  A data frame.

- geom:

  The geom to draw with. Defaults to
  [`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md)'s.

- position:

  Position adjustment.

- ...:

  Further arguments: those
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  takes (for example `alpha` for NSP's level) go to the detector, the
  rest to the geom.

- method:

  Detection method: `"nsp"` (the default) or any method whose result
  carries regions or location intervals (see `cpt_methods()$ci`).

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
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`ggchangepoint-ggproto`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggchangepoint-ggproto.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
d <- data.frame(t = 1:200, y = c(rnorm(100), rnorm(100, 2)))
ggplot(d, aes(t, y)) + stat_cpt_region(seed = 1) + geom_line()
```
