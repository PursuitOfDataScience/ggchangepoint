# Isolate-Detect wrapper

Wraps the `IDetect` package. Requires the `IDetect` package.

## Usage

``` r
idetect_wrapper(x, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- seed:

  Optional seed for reproducibility.

- ...:

  Additional arguments passed to
  [`IDetect::ID()`](https://rdrr.io/pkg/IDetect/man/ID.html).

## Value

A `ggcpt` object. When the engine finds no changepoints (including when
it signals "No change-points found"), an empty result is returned rather
than an error.
