# Write a result to a file, and read it back

`cpt_export()` writes a result where software in any language can read
it; `cpt_import()` rebuilds a working `ggcpt` from the file. A saved
`.rds` already reads in any R session without this package (a `ggcpt` is
a plain list of plain data), so these are for leaving R, or for a format
a person can open.

## Usage

``` r
cpt_export(fit, file, format = NULL, ...)

cpt_import(file, format = NULL, method = "imported", change_in = "mean")
```

## Arguments

- fit:

  A `ggcpt` object.

- file:

  Path to write (`cpt_export()`) or read (`cpt_import()`).

- format:

  `"json"` (the full result, see
  [`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md))
  or `"csv"` (one row per observation: `index`, `value`, the time index
  as `index_value` when there is one, `seg_id`, `fitted` and
  `is_changepoint`, which is everything needed to rebuild the
  segmentation but not its metadata). Defaults to the file's extension.

- ...:

  For `cpt_export()`, further arguments for
  [`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md).

- method, change_in:

  For a CSV, which carries no metadata, the method and change type to
  record on the rebuilt result. Defaults to `"imported"` and `"mean"`.

## Value

`cpt_export()` returns `file`, invisibly. `cpt_import()` returns a
`ggcpt` object; the engine's own `$fit` is not stored in either format,
so it is `NULL`.

## See also

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`cpt_verify()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_verify.md),
[`ggchangepoint-conditions`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggchangepoint-conditions.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 3)), method = "pelt")
path <- tempfile(fileext = ".json")
cpt_export(fit, path)
back <- cpt_import(path)
back$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50    0.881
unlink(path)
```
