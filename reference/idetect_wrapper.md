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
than an error. A constant series likewise returns the empty result; see
the note below.

## Constant input

[`IDetect::ID()`](https://rdrr.io/pkg/IDetect/man/ID.html) does not
treat a flat series consistently — its statistics become \\0/0\\, and
what comes back depends on the value and the length. `rep(3, 200)`
yields *126* changepoints, at 1, 3, 4, 6, 7, ...; `rep(0, 100)` raises
"No change-points found"; `rep(-2.5, 60)` returns the sentinel 0. A
constant series plainly has no changepoint, and every other search
wrapper here reports none, so this one short-circuits to the empty
result. Constancy is decided by exact equality, so a series with tiny
but genuine variation still reaches the engine.
