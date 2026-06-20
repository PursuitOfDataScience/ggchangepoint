# WBS wrapper — Wild Binary Segmentation

Wraps the `wbs` package for randomised changepoint detection via Wild
Binary Segmentation.

## Usage

``` r
wbs_wrapper(x, n_intervals = 5000, threshold = NULL, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- n_intervals:

  Number of random intervals. Defaults to `5000`.

- threshold:

  Manual threshold for detection. If `NULL`, uses the strengthened
  Schwarz Information Criterion (sSIC).

- seed:

  Optional seed for reproducibility.

- ...:

  Additional arguments passed to
  [`wbs::wbs()`](https://rdrr.io/pkg/wbs/man/wbs.html).

## Value

A `ggcpt` object.
