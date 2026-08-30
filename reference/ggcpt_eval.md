# Evaluation visualization

Overlays predictions and ground truth on the series with tolerance
windows, colouring true positives, false positives, and misses. Uses the
same one-to-one matching as
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
so the plot and the metrics agree.

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

## See also

Other plotting:
[`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md),
[`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md),
[`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md),
[`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)

## Examples

``` r
set.seed(2026)
x <- c(rnorm(100), rnorm(100, 5))
fit <- cpt_detect(x, method = "pelt")
ggcpt_eval(fit$changepoints$cp, truth = 100, data_vec = x)
```
