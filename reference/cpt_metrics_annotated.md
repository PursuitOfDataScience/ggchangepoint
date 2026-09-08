# Multi-annotator evaluation

Computes averaged covering and F1 scores against multiple annotation
sets, as used in the Turing Change Point Dataset benchmark.

## Usage

``` r
cpt_metrics_annotated(pred, annotations, n, margin = 5)
```

## Arguments

- pred:

  Predicted changepoint indices.

- annotations:

  A list of ground-truth annotation vectors.

- n:

  Length of the series.

- margin:

  Tolerance margin (default 5).

## Value

A tibble with one row: `n`, `n_annotators`, `n_pred`, and the **four**
metrics this averages – `precision`, `recall`, `f1` and `covering`. Each
is a plain unweighted mean of the per-annotator value from
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
over all `n_annotators` of them.

**This is a narrower table than
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
returns**, so a call moved from one to the other loses columns:
`n_truth` (there is no single truth), and the location metrics
`hausdorff`, `mae_matched`, `rmse_matched`, `rand_index` and
`annotation_error`.

The three distance metrics are omitted for a reason worth stating,
because it is not obvious: they are `NA` whenever an annotator shares no
matched pair with the prediction, so averaging them would quietly divide
by fewer annotators than `n_annotators` reports. Measured on three
annotators against one prediction – `list(c(100, 200), integer(0), 150)`
at \\n = 300\\ – `mae_matched` was available for **one** of the three
and `hausdorff` for two, while `f1` and `covering` were finite for all
three. Score those per annotator with
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
and combine them yourself if you want them, so the divisor is yours to
choose.

`covering` and `f1` are the pair the Turing Change Point Dataset
benchmark reports, which is why they are the ones averaged here;
`precision` and `recall` come along as F1's parts.

## See also

[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
for the full single-truth table and what each column means.

## Examples

``` r
# two annotators who disagree slightly about where the change is
cpt_metrics_annotated(c(100, 200),
                      annotations = list(c(98, 200), c(100, 203)),
                      n = 300, margin = 5)
#> # A tibble: 1 × 7
#>       n n_annotators n_pred precision recall    f1 covering
#>   <dbl>        <int>  <int>     <dbl>  <dbl> <dbl>    <dbl>
#> 1   300            2      2         1      1     1    0.984
```
