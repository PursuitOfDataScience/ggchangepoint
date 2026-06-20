# FPOP wrapper — Functional Pruning Optimal Partitioning

Wraps the `fpop` package for optimal changepoint detection via
functional pruning.

## Usage

``` r
fpop_wrapper(x, penalty = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- penalty:

  Penalty value. Defaults to `2 * log(length(x))` (BIC).

- ...:

  Additional arguments passed to
  [`fpop::Fpop()`](https://rdrr.io/pkg/fpop/man/Fpop.html).

## Value

A `ggcpt` object.
