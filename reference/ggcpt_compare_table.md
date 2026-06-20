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

  A numeric vector (the data series).

- methods:

  Character vector of method names.

- change_in:

  What to detect change in.

- ...:

  Additional arguments passed to each detector.

## Value

A tibble with columns `method`, `cp`, `cp_value`.
