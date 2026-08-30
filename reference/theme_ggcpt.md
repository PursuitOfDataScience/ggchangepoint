# ggchangepoint theme

A minimal, publication-ready ggplot2 theme for changepoint plots.

## Usage

``` r
theme_ggcpt(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size. Defaults to 11.

- base_family:

  Base font family. Defaults to "".

## Value

A ggplot2 theme object.

## See also

Other accessibility scales:
[`scale_colour_cpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_colour_cpt.md),
[`scale_fill_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/scale_fill_cpt_label.md)

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_ggcpt()
```
