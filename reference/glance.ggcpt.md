# Glance at a ggcpt object

Returns a one-row summary of a changepoint detection result.

## Usage

``` r
# S3 method for class 'ggcpt'
glance(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A one-row tibble with columns: `n`, `n_changepoints`, `method`,
`change_in`, `penalty_type`, `penalty_value`, `cp_convention`,
`total_cost` (`NA` when the engine does not expose a cost), `runtime`
(elapsed seconds when measured by
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
otherwise `NA`).
