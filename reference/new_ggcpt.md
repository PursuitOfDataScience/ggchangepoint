# Create a ggcpt object

Create a ggcpt object

## Usage

``` r
new_ggcpt(
  changepoints = tibble::tibble(cp = integer(), cp_value = numeric()),
  segments = tibble::tibble(seg_id = integer(), start = integer(), end = integer(), n =
    integer(), param_estimate = numeric()),
  data = tibble::tibble(index = integer(), value = numeric()),
  method = NA_character_,
  change_in = NA_character_,
  penalty = list(type = NA_character_, value = NA_real_),
  fit = NULL,
  call = NULL,
  cp_convention = "left",
  runtime = NA_real_
)
```

## Arguments

- changepoints:

  A tibble with columns `cp` and `cp_value`.

- segments:

  A tibble with segment information: `seg_id`, `start`, `end`, `n`,
  `param_estimate`.

- data:

  A tibble with `index` and `value`.

- method:

  Character. The detection method used. A length-one string; defaults to
  `NA_character_`. (A zero-length value would make
  [`glance()`](https://generics.r-lib.org/reference/glance.html) return
  zero rows instead of its documented single row, because every other
  column would be recycled against it.)

- change_in:

  Character. What was detected (e.g. "mean", "var", "meanvar"). A
  length-one string; defaults to `NA_character_`.

- penalty:

  A list with `type` and `value`.

- fit:

  The raw upstream object. Every wrapper stores one except `"ecp"`:
  [`ecp::e.agglo()`](https://rdrr.io/pkg/ecp/man/e.agglo.html) returns a
  cluster-progression matrix that is quadratic in the series length, so
  keeping it by default would make the result object explode on a long
  series. Call
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html) or
  [`ecp::e.agglo()`](https://rdrr.io/pkg/ecp/man/e.agglo.html) directly
  if you need it.

- call:

  The matched call.

- cp_convention:

  Character. The convention for reporting changepoint locations:
  `"left"` (last index of left segment, used by `changepoint`) or
  `"right"` (first index of right segment, used by `ecp`). Defaults to
  `"left"`.

- runtime:

  Numeric. Elapsed detection time in seconds, if measured. Defaults to
  `NA`.

## Value

An object of class `ggcpt`.
