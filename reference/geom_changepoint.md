# Changepoint vertical rules geom

Draws vertical lines at changepoint locations. Mimics `geom_vline` but
designed to work with the tidy changepoint data frames returned by the
package. Can be used as a standalone layer:
`geom_changepoint(data = cp_df, aes(xintercept = cp))`.

## Usage

``` r
geom_changepoint(
  mapping = NULL,
  data = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Requires `xintercept`.

- data:

  A data frame with changepoint information.

- ...:

  Other arguments passed to `geom_vline`.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.

## See also

Other ggplot2 layers:
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
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
cp <- data.frame(cp = cpt_detect(d$y, method = "pelt")$changepoints$cp)
ggplot(d, aes(t, y)) + geom_line() +
  geom_changepoint(aes(xintercept = cp), data = cp, colour = "blue")
```
