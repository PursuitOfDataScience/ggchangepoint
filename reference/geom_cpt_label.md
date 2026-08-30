# Changepoint label geom

Draws labelled regions behind a series — the central object of
supervised changepoint detection (Hocking et al.), where an expert marks
intervals as containing a change or not and the penalty is learned from
those labels. Fill defaults to the label's `change` status, so a plot of
labels reads as a picture of what the expert asserted; map `fill` to the
`status` column of
[`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
instead to get the correct / false-positive / false-negative display.

## Usage

``` r
geom_cpt_label(
  mapping = NULL,
  data = NULL,
  ...,
  alpha = 0.25,
  colour = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `xmin` and `xmax`; `ymin`/`ymax` default
  to the panel extent. `fill` is commonly mapped to `change` or
  `status`.

- data:

  A data frame of labels, e.g.
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md)
  output.

- ...:

  Other arguments passed to
  [`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

- alpha:

  Fill transparency. Defaults to `0.25`.

- colour:

  Border colour. Defaults to `NA` (no border).

- na.rm:

  If `FALSE`, missing values are removed with a warning.

- show.legend:

  Whether to show a legend.

- inherit.aes:

  Whether to inherit the plot's aesthetics. Defaults to `FALSE`.

## Value

A ggplot layer.

## See also

[`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md),
[`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md),
[`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md).

Other ggplot2 layers:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
labs <- cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
ggplot(d, aes(t, y)) +
  geom_cpt_label(aes(xmin = start, xmax = end, fill = change),
                 data = labs) +
  geom_line() +
  scale_fill_cpt_label()
```
