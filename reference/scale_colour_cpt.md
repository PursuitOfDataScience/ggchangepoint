# Colour-vision-safe scales for changepoint methods

The discrete palette used wherever this package colours by method,
series or class. It is the Okabe–Ito qualitative palette, which stays
distinguishable under deuteranopia, protanopia and tritanopia, extended
by recycling with a linetype change so that colour is never the only
channel carrying the distinction.

## Usage

``` r
scale_colour_cpt(..., na.value = "grey70")

scale_color_cpt(..., na.value = "grey70")

scale_fill_cpt(..., na.value = "grey70")

scale_linetype_cpt(...)
```

## Arguments

- ...:

  Passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- na.value:

  Colour for missing values.

## Value

A ggplot2 scale.

## See also

[`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md),
[`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md).

Other accessibility scales:
[`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md),
[`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md)

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() + scale_colour_cpt() + theme_ggcpt()
```
