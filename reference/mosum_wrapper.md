# MOSUM wrapper — Moving Sum

Wraps the `mosum` package for moving-sum-based changepoint detection.

## Usage

``` r
mosum_wrapper(x, G = NULL, multiscale = FALSE, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- G:

  Bandwidth. If `NULL`, automatically selected.

- multiscale:

  Logical. Use multiscale MOSUM? Defaults to `FALSE`.

- seed:

  Optional seed for reproducibility.

- ...:

  Additional arguments passed to
  [`mosum::mosum()`](https://rdrr.io/pkg/mosum/man/mosum.html).

## Value

A `ggcpt` object.
