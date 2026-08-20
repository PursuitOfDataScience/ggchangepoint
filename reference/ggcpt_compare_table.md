# Comparison table

Returns a tidy tibble combining the results of multiple detectors on the
same series.

## Usage

``` r
ggcpt_compare_table(
  x,
  methods = c("pelt", "binseg", "amoc"),
  change_in = "mean",
  ...
)
```

## Arguments

- x:

  A numeric vector (the data series). A one-column matrix or data frame
  is accepted; wider input is refused, because these detectors are
  univariate and flattening the columns would invent a changepoint at
  every seam. Use
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  for a panel of series.

- methods:

  Character vector of method names.

- change_in:

  What to detect change in.

- ...:

  Additional arguments passed to each detector.

## Value

A tibble with columns `method`, `cp`, `cp_value`.
