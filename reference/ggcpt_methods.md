# Coerce, format, and plot ggcpt objects

Convenience S3 methods for working with `ggcpt` objects: coerce the
changepoints to a tibble or data frame, render a one-line summary
string, or produce the default plot (a base-graphics fallback that
delegates to
[`autoplot.ggcpt`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md)).

## Usage

``` r
# S3 method for class 'ggcpt'
as_tibble(x, ..., .name_repair = NULL)

# S3 method for class 'ggcpt'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'ggcpt'
format(x, ...)

# S3 method for class 'ggcpt'
plot(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments passed to methods.

- .name_repair:

  Passed to
  [`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html).

- row.names, optional:

  Passed to
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

## Value

[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
and [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
return the changepoints table;
[`format()`](https://rdrr.io/r/base/format.html) returns a length-one
character string;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns a
`ggplot` object.

## Examples

``` r
set.seed(2022)
res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
as_tibble(res)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50    0.368
as.data.frame(res)
#>   cp  cp_value
#> 1 50 0.3681734
format(res)
#> [1] "ggcpt [pelt] 1 changepoint(s) on 100 observations"
```
