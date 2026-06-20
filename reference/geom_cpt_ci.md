# Changepoint confidence interval geom

Draws horizontal whiskers for changepoint-location confidence intervals
(e.g. from MOSUM, stepR, strucchange, segmented).

## Usage

``` r
geom_cpt_ci(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `x`, `xmin`, `xmax`, and `y`.

- data:

  A data frame with CI information.

- ...:

  Other arguments passed to `geom_errorbarh`.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.
