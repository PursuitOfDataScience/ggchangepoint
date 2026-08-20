# MOSUM wrapper — Moving Sum

Wraps the `mosum` package for moving-sum-based changepoint detection,
either at a single bandwidth or (with `multiscale = TRUE`) across a
bandwidth grid with localised pruning.

## Usage

``` r
mosum_wrapper(x, G = NULL, multiscale = FALSE, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- G:

  Bandwidth. If `NULL`, automatically selected (`min(n/10, 100)`, but
  never below 2, for the single-bandwidth procedure; the engine's
  default bandwidth grid for the multiscale procedure). A bandwidth of 1
  leaves the engine's local variance estimate undefined, so the
  automatic choice is floored at 2.

- multiscale:

  Logical. Use the multiscale MOSUM procedure
  ([`mosum::multiscale.localPrune()`](https://rdrr.io/pkg/mosum/man/multiscale.localPrune.html))
  instead of a single bandwidth? Defaults to `FALSE`.

- seed:

  Optional seed for reproducibility.

- ...:

  Additional arguments passed to
  [`mosum::mosum()`](https://rdrr.io/pkg/mosum/man/mosum.html) or
  [`mosum::multiscale.localPrune()`](https://rdrr.io/pkg/mosum/man/multiscale.localPrune.html).

## Value

A `ggcpt` object.
