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
