# Re-run a result against the engines installed now

A result records which version of its engine produced it (see
[`glance()`](https://generics.r-lib.org/reference/glance.html)'s
`engine_version`). `cpt_verify()` answers the question that record
exists for: *does the answer still hold?* It re-runs the same method on
the same series with the settings the result recorded, using whatever
engines are installed today, and reports whether the changepoints moved
and which versions changed in between.

## Usage

``` r
cpt_verify(object, ..., tolerance = 0)

# S3 method for class 'ggcpt_verification'
print(x, ...)
```

## Arguments

- object:

  A `ggcpt` object from
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- ...:

  Further arguments for the re-run, overriding what was recovered from
  the result (for instance an engine argument the recorded call held as
  a variable, which cannot be recovered from the object).

- tolerance:

  Positions a changepoint may move and still count as the same. Defaults
  to `0`: an exact match.

- x:

  A `ggcpt_verification` object.

## Value

A `ggcpt_verification` object: a list with `verified` (`TRUE` when every
changepoint reproduces within `tolerance`), `then` and `now` (the two
sets of locations), `added` and `removed` (locations in one set with no
partner in the other), `versions` (a tibble with `component`, `then`,
`now` and `changed`), `not_recovered` (call arguments that could not be
replayed) and `refit`, the new `ggcpt`. With a
[`print()`](https://rdrr.io/r/base/print.html) method.

## Details

Upstream engines change: a default is revised, a bug is fixed, a
tie-breaking rule moves. None of that raises an error, so a result
produced last year and re-run today can differ without anything saying
so. This is the check to run when re-opening an analysis, and before
quoting an old result as current.

The re-run uses the series stored on the result, its method, the change
type it can be asked for, its penalty, and every engine argument of the
original call that was written as a literal (`seed = 42`,
`n_intervals = 500`). An argument written as a variable cannot be
recovered from the object, and is listed in `not_recovered`; pass it
again through `...` if it mattered.

## See also

[`glance.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/glance.ggcpt.md)
for the recorded `engine_version`.

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`cpt_export()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md),
[`ggchangepoint-conditions`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggchangepoint-conditions.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(80), rnorm(80, 4)), method = "pelt")
cpt_verify(fit)
#> ggcpt_verification (method: pelt)
#>   Verdict:            reproduces with the engines installed now
#>   Changepoints then:  80
#>   Changepoints now:   80
#> 
#> Versions:
#> # A tibble: 3 × 4
#>   component            then  now   changed
#>   <chr>                <chr> <chr> <lgl>  
#> 1 changepoint (engine) 2.3   2.3   FALSE  
#> 2 ggchangepoint        0.6.0 0.6.0 FALSE  
#> 3 R                    4.6.1 4.6.1 FALSE  
```
