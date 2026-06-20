# Evaluation visualization

Overlays predictions and ground truth on the series with tolerance
windows, colouring true positives, false positives, and misses.

## Usage

``` r
ggcpt_eval(pred, truth, data_vec, margin = 5)
```

## Arguments

- pred:

  Predicted changepoint indices.

- truth:

  Ground truth changepoint indices.

- data_vec:

  The original data vector (for context).

- margin:

  Tolerance margin (default 5).

## Value

A ggplot object.
