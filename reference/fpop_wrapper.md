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

  Penalty value. Defaults to `2 * log(length(x))` (BIC). This is an
  *absolute* penalty on the residual sum of squares, so it is only
  calibrated for noise of standard deviation 1: on wider data the
  default under-penalises badly and the segmentation shatters.
  Standardise the series, or scale the penalty by the noise variance
  (for example `2 * log(length(x)) * stats::var(diff(x)) / 2`). See the
  scale-sensitivity section of
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  This default differs from the one
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  applies, which resolves its `"MBIC"` default to a stronger numeric
  value, so the two entry points need not agree unless `penalty` is
  given.

- ...:

  Additional arguments passed to
  [`fpop::Fpop()`](https://rdrr.io/pkg/fpop/man/Fpop.html).

## Value

A `ggcpt` object.
