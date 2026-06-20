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
