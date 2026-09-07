# Per-annotator ground truth for a benchmark dataset

Returns the annotation sets of a dataset one row per (annotator,
changepoint), so the disagreement between human annotators is visible
instead of averaged away.

## Usage

``` r
cpt_annotations(dataset)
```

## Arguments

- dataset:

  A dataset in the
  [`cpt_datasets()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_datasets.md)
  shape (a list with `annotations`), or a named list of them.

## Value

A tibble with `dataset`, `annotator` and `cp`.

## See also

[`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md).

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
cpt_annotations(cpt_datasets(n = 200, names = "step"))
#> # A tibble: 2 × 3
#>   dataset annotator    cp
#>   <chr>   <chr>     <int>
#> 1 step    1            66
#> 2 step    1           133
```
