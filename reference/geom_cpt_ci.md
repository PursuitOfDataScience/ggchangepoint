# Changepoint confidence interval geom

Draws horizontal whiskers for changepoint-location confidence intervals
(e.g. from MOSUM, stepR, strucchange, segmented).

## Usage

``` r
geom_cpt_ci(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA)
```

## Arguments

- mapping:

  Aesthetic mappings. Requires `y` (the height at which to draw the
  whisker) together with `xmin` and `xmax`. An `x` aesthetic is accepted
  but not needed: the layer is a horizontal error bar, so the interval
  is given by `xmin`/`xmax` and the changepoint itself is usually marked
  with a separate point layer, as `autoplot(show_ci = TRUE)` does.

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
