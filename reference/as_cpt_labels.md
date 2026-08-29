# Coerce annotations to changepoint labels

Turns a plain ground-truth changepoint set — the kind
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
takes — into labelled regions, so one annotation can drive both the
metric and the supervised machinery. Each true changepoint becomes a
`"one_change"` region of width `2 * margin + 1`, and the stretches
between them become `"no_change"` regions.

## Usage

``` r
as_cpt_labels(truth, n, margin = 5, negatives = TRUE, series = NA_character_)
```

## Arguments

- truth:

  Integer vector of true changepoint positions.

- n:

  Series length.

- margin:

  Half-width of the positive regions. Defaults to `5`, matching
  [`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)'s
  default tolerance.

- negatives:

  Add the `"no_change"` regions between the positives? Defaults to
  `TRUE`; without them a detector is never penalised for a false
  positive.

- series:

  Optional series identifier.

## Value

A `cpt_labels` tibble.

## Examples

``` r
as_cpt_labels(c(50, 120), n = 200)
#> # A tibble: 5 × 5
#>   label_id series start   end change    
#>      <int> <chr>  <int> <int> <chr>     
#> 1        3 NA         1    44 no_change 
#> 2        1 NA        45    55 one_change
#> 3        4 NA        56   114 no_change 
#> 4        2 NA       115   125 one_change
#> 5        5 NA       126   199 no_change 
```
