# Tidy a ggcpt object

Returns the changepoints tibble (one row per changepoint).

## Usage

``` r
# S3 method for class 'ggcpt'
tidy(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A tibble with columns `cp`, `cp_value`, and any method-specific columns.
