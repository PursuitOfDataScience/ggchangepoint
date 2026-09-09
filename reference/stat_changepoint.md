# Changepoint detection stat

Runs changepoint detection inside the ggplot pipeline. Useful for quick
exploration:
`ggplot(df, aes(t, y)) + geom_line() + stat_changepoint(method = "pelt")`.
Draws vertical lines at detected changepoint locations.

## Usage

``` r
stat_changepoint(
  mapping = NULL,
  data = NULL,
  geom = "vline",
  position = "identity",
  ...,
  method = "pelt",
  change_in = "mean",
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

  The geometric object to use (default: `"vline"`). The stat computes a
  single `xintercept` per changepoint and drops `x`/`y`, so `"vline"` is
  the geom that fits. A geom needing `x`/`y` — `"point"`, and `"rug"`,
  which consumes `x`/`y` rather than `xintercept` — errors for that
  reason. `inherit.aes` is fixed at `TRUE` here: the stat re-detects on
  the plot's own data, so the panel's `x`/`y` mapping is what it reads.

- position:

  Position adjustment.

- ...:

  Other arguments passed to the geom.

- method:

  Detection method (passed to `cpt_detect`).

- change_in:

  What to detect change in (passed to `cpt_detect`).

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
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md)

## Examples

``` r
library(ggplot2)
set.seed(2026)
d <- data.frame(t = 1:100, y = c(rnorm(50), rnorm(50, 4)))
ggplot(d, aes(t, y)) + geom_line() +
  stat_changepoint(method = "pelt", colour = "blue")
```
