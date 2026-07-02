# NOT wrapper — Narrowest-Over-Threshold

Wraps the `not` package for changepoint detection via the
Narrowest-Over-Threshold method. The contrast determines what change is
detected: piecewise-constant mean (default), mean and variance, or
(continuous or discontinuous) piecewise-linear trend.

## Usage

``` r
not_wrapper(x, contrast = "pcwsConstMean", seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- contrast:

  Contrast type. One of `"pcwsConstMean"`, `"pcwsLinContMean"`,
  `"pcwsLinMean"`, `"pcwsConstMeanVar"`. Defaults to `"pcwsConstMean"`.

- seed:

  Optional seed for reproducibility.

- ...:

  Additional arguments passed to
  [`not::not()`](https://rdrr.io/pkg/not/man/not.html).

## Value

A `ggcpt` object whose `change_in` reflects the contrast: `"mean"`,
`"meanvar"`, or `"slope"`.
