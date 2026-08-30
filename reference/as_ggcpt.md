# Turn external changepoints into a ggcpt result

Wraps a set of changepoint locations — from a detector this package does
not wrap, a Python tool called through reticulate, a neural detector, a
published paper's reported breaks, or an analyst's own annotations —
into a first-class `ggcpt` object. Everything built on the `ggcpt`
contract then applies:
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
the composable geoms,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`glance()`](https://generics.r-lib.org/reference/glance.html)/[`augment()`](https://generics.r-lib.org/reference/augment.html),
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).

## Usage

``` r
as_ggcpt(
  cp,
  x,
  fitted = NULL,
  method = "custom",
  change_in = "mean",
  ci = NULL,
  regions = NULL,
  penalty = NULL,
  cp_convention = c("left", "right"),
  index = NULL,
  fit = NULL,
  extra = NULL
)
```

## Arguments

- cp:

  Integer vector of changepoint locations. Out-of-range, duplicated and
  missing values are dropped, and the result is sorted — the same
  contract every built-in wrapper is held to.

- x:

  The series the changepoints refer to: a numeric vector, or a
  matrix/data frame (rows are time points) for a multivariate result.

- fitted:

  Optional length-`n` fitted signal, used by `autoplot(show_fit = TRUE)`
  and [`augment()`](https://generics.r-lib.org/reference/augment.html).

- method:

  Method label. Defaults to `"custom"`.

- change_in:

  What the changepoints are changes in. Defaults to `"mean"`.

- ci:

  Optional two-column matrix or data frame of location confidence
  intervals, one row per changepoint, giving lower and upper bounds as
  positions.

- regions:

  Optional two-column matrix or data frame of significance regions
  (`start`, `end`); see
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
  for the interval-valued case this exists to serve.

- penalty:

  Optional penalty descriptor: a number, a string, or a list with `type`
  and `value`.

- cp_convention:

  `"left"` (the changepoint is the last index of the left segment — this
  package's convention) or `"right"` (the first index of the right
  segment, which is converted on the way in).

- index:

  Optional time index, one value per observation.

- fit:

  Optional raw upstream object to carry along.

- extra:

  Optional named list of per-changepoint columns, each of the same
  length as `cp`, appended to the changepoints tibble.

## Value

A `ggcpt` object.

## See also

[`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
to make
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
dispatch to an external detector by name.

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)

## Examples

``` r
set.seed(2026)
x <- c(rnorm(60), rnorm(60, 4))
fit <- as_ggcpt(c(60), x, method = "my_detector")
fit
#> ggcpt (changepoint detection result)
#>   Method:         my_detector
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         user 
#>   Series length:   120 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60   -0.999
tidy(fit)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60   -0.999
```
