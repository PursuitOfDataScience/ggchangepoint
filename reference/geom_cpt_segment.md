# Changepoint segment level geom

Draws horizontal segments representing the estimated level of each
segment between changepoints. Typically used with data from
[`augment()`](https://generics.r-lib.org/reference/augment.html).

## Usage

``` r
geom_cpt_segment(
  mapping = NULL,
  data = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA
)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `x`, `xend`, `y`, `yend`.

- data:

  A data frame with segment information.

- ...:

  Other arguments passed to `geom_segment`.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.
