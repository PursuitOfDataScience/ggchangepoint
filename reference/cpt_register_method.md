# Register an external changepoint detector

Teaches
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
about a detector this package does not (and often cannot) depend on: an
engine that is not on CRAN, a Python detector reached through
reticulate, a deep-learning model, a proprietary in-house method, or a
hand-curated set of changepoints. The registered method then works with
everything built on the `ggcpt` contract —
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
the geoms,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`glance()`](https://generics.r-lib.org/reference/glance.html)/
[`augment()`](https://generics.r-lib.org/reference/augment.html),
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md),
[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
and
[`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).

## Usage

``` r
cpt_register_method(
  name,
  fn,
  change_in = "mean",
  engine = "user",
  citation = NULL,
  capabilities = list(),
  cp_convention = c("left", "right"),
  overwrite = FALSE
)

cpt_unregister_method(name)

cpt_registered_methods()
```

## Arguments

- name:

  Method name, as it will be passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  Must not clash with a built-in method.

- fn:

  A function called as `fn(x, ...)`. It may return either a `ggcpt`
  object (built with
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
  say) or a bare vector of changepoint indices, which is coerced with
  [`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md).

- change_in:

  Character vector of `change_in` values the detector supports. Defaults
  to `"mean"`.

- engine:

  Name of the package or system supplying the detector, for display in
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md).
  Defaults to `"user"`.

- citation:

  Optional citation string returned by
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).
  When `NULL`,
  [`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
  says plainly that the registration supplied none rather than inventing
  one.

- capabilities:

  Optional named list of capability flags overriding the defaults (all
  `FALSE`): `multivariate`, `online`, `ci`, `fitted`, `posterior`,
  `statistic`, `path`, `scale_space`.

- cp_convention:

  `"left"` (the changepoint is the last index of the left segment, this
  package's convention) or `"right"`. Used when `fn` returns bare
  indices.

- overwrite:

  Replace an existing registration of the same name? Defaults to
  `FALSE`.

## Value

Invisibly, the method name.

`cpt_registered_methods()` returns a tibble with one row per registered
method (columns `method`, `change_in`, `engine`, `has_citation`), or a
zero-row tibble when none are registered.

## Registered methods are labelled, not endorsed

A registered method is visibly user-supplied:
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
gives it `status = "registered"`,
[`print()`](https://rdrr.io/r/base/print.html) on its results marks it,
and
[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
reports the citation you supplied or states that none was given. The
package validates the *shape* of what your function returns (through
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
which runs the same contract checks as every built-in wrapper); it does
not and cannot validate the method.

## See also

[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md)
to turn changepoints into a result object without registering a method;
`cpt_unregister_method()`; `cpt_registered_methods()`.

## Examples

``` r
# A deliberately trivial detector: split at the largest jump.
cpt_register_method(
  "biggest_jump",
  fn = function(x, ...) which.max(abs(diff(x))),
  change_in = "mean",
  engine = "example",
  citation = "No citation supplied (illustration only)."
)
set.seed(1)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "biggest_jump")
fit
#> ggcpt (changepoint detection result)
#>   Method:             biggest_jump  [user-registered]
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            MBIC
#>   Series length:      100
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50    0.881
#> 
#> This result came from a user-registered detector; the package validated
#> its shape, not its statistics. See ?cpt_register_method.
cpt_unregister_method("biggest_jump")
```
