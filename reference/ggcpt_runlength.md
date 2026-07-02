# Run-length posterior heatmap for Bayesian online results

Draws the signature BOCPD graphic: the posterior distribution of the run
length (time since the last changepoint) at every observation, as a
heatmap, with the series overlaid on top. Works with results from
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md).

## Usage

``` r
ggcpt_runlength(x, prob_floor = 0.001)
```

## Arguments

- x:

  A `ggcpt` object produced by
  [`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md).

- prob_floor:

  Posterior probabilities below this value are not drawn (keeps the
  heatmap legible). Defaults to `1e-3`.

## Value

A ggplot object.

## Examples

``` r
res <- bocpd_wrapper(c(rnorm(60), rnorm(60, 4)))
ggcpt_runlength(res)
```
