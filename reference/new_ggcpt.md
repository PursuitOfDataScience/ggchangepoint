# Create a ggcpt object

Create a ggcpt object

## Usage

``` r
new_ggcpt(
  changepoints = tibble::tibble(cp = integer(), cp_value = numeric()),
  segments = tibble::tibble(seg_id = integer(), start = integer(), end = integer(), n =
    integer(), param_estimate = numeric()),
  data = tibble::tibble(index = integer(), value = numeric()),
  method = character(),
  change_in = character(),
  penalty = list(type = NA_character_, value = NA_real_),
  fit = NULL,
  call = NULL,
  cp_convention = "left"
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

  Character. The detection method used.

- change_in:

  Character. What was detected (e.g. "mean", "var", "meanvar").

- penalty:

  A list with `type` and `value`.

- fit:

  The raw upstream object.

- call:

  The matched call.

- cp_convention:

  Character. The convention for reporting changepoint locations:
  `"left"` (last index of left segment, used by `changepoint`) or
  `"right"` (first index of right segment, used by `ecp`). Defaults to
  `"left"`.

## Value

An object of class `ggcpt`.
