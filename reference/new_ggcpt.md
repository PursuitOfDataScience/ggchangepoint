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
  if you need it. A few of the engines that *are* kept are still large
  relative to the data — measured on a 2000-point series, `strucchange`
  costs about 135 MB (a triangular \\O(n^2)\\ RSS matrix), `bfast` about
  53 MB and `bocpd` about 31 MB, while every other engine stays under 4
  MB. That is the engine's own object, not overhead this package adds,
  and it matters mainly when many results are held at once:
  [`cpt_batch`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)`(keep_fit = FALSE)`
  drops them, or assign `res$fit <- NULL` yourself.

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

## Optional slots

Beyond the components in the signature, a `ggcpt` may carry any of
these, each present only when something supplied it and each safe to
test for with [`is.null()`](https://rdrr.io/r/base/NULL.html):

- `data_wide`:

  index plus one column per coordinate, for a multivariate result.

- `index`, `index_label`:

  a time index (one value per observation) and its axis label; see the
  `index` argument of
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- `regions`:

  a tibble of significance regions (`start`, `end`, ...) for the
  interval-valued methods — see
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
  and
  [`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md).

- `diagnostics`:

  a named list of engine internals rendered by
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  and
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md).

- `registered`:

  `TRUE` when the result came from a user-registered detector rather
  than a wired engine.

[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
is the validating way to build one of these from the outside; this
constructor does not check its arguments.

## See also

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)

## Examples

``` r
set.seed(2026)
new_ggcpt(
  changepoints = tibble::tibble(cp = 50L, cp_value = 0.1),
  data = tibble::tibble(index = 1:100,
                        value = c(rnorm(50), rnorm(50, 4))),
  method = "manual", change_in = "mean")
#> ggcpt (changepoint detection result)
#>   Method:         manual
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         NA 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50      0.1
```
