# Summary of a ggcpt object

Provides a human-readable digest of a changepoint detection result,
including the segment table with levels and lengths, total cost,
penalty, and runtime.

## Usage

``` r
# S3 method for class 'ggcpt'
summary(object, ...)

# S3 method for class 'summary.ggcpt'
print(x, ...)
```

## Arguments

- object:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

- x:

  A `summary.ggcpt` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

## Value

A list with class `summary.ggcpt` containing the summary.
