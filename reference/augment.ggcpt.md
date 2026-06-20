# Augment a ggcpt object

Returns the original data with added columns: `seg_id`, `.fitted`,
`.resid`, and `is_changepoint`.

## Usage

``` r
# S3 method for class 'ggcpt'
augment(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A tibble with the original data plus augment columns.
