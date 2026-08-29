# Interactive changepoint plot

Renders a `ggcpt` result (or any ggplot built from one) as an
interactive HTML widget, with values on hover. A thin convenience
wrapper: the static
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
path is untouched.

## Usage

``` r
ggcpt_interactive(
  x,
  engine = c("plotly", "ggiraph"),
  width_svg = 8,
  height_svg = 5,
  ...
)
```

## Arguments

- x:

  A `ggcpt` object or a ggplot object.

- engine:

  Which renderer: `"plotly"` (the default) rebuilds the plot in plotly's
  own model, which is richer but loses layers plotly does not know;
  `"ggiraph"` renders the ggplot itself to interactive SVG, so faceting
  and every layer survive and the result composes with other
  htmlwidgets. Neither is a dependency; whichever you ask for must be
  installed.

- width_svg, height_svg:

  Figure size in inches for `engine = "ggiraph"`.

- ...:

  Additional arguments passed to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  when `x` is a `ggcpt` object.

## Value

A plotly or ggiraph htmlwidget.

## Examples

``` r
if (FALSE) { # requireNamespace("plotly", quietly = TRUE) && interactive()
res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
ggcpt_interactive(res)
}
```
