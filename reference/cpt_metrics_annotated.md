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

A tibble with averaged metrics.

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
