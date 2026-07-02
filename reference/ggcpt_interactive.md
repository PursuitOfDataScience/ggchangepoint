# Interactive changepoint plot

Renders a `ggcpt` result (or any ggplot built from one) as an
interactive HTML widget via plotly, with values on hover. A thin
convenience wrapper: the static
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
path is untouched.

## Usage

``` r
ggcpt_interactive(x, ...)
```

## Arguments

- x:

  A `ggcpt` object or a ggplot object.

- ...:

  Additional arguments passed to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  when `x` is a `ggcpt` object.

## Value

A `plotly` htmlwidget.

## Examples

``` r
if (FALSE) { # requireNamespace("plotly", quietly = TRUE) && interactive()
res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
ggcpt_interactive(res)
}
```
