# Changepoint labels

Builds the tidy label set that the supervised functions consume: one row
per labelled region, each asserting how many changepoints that stretch
of the series contains.

## Usage

``` r
cpt_labels(start, end, change = "change", series = NA_character_)
```

## Arguments

- start, end:

  Integer vectors of region boundaries (positions, inclusive).

- change:

  What the region asserts, recycled to length:

  `"change"`

  :   at least one changepoint lies in the region.

  `"one_change"`

  :   exactly one does — a stricter label, and the one that makes false
      positives detectable inside a positive region.

  `"no_change"`

  :   none does.

- series:

  Optional series identifier, for label sets spanning a panel.

## Value

A `cpt_labels` tibble with columns `label_id`, `series`, `start`, `end`,
`change`.

## References

Hocking TD, Rigaill G, Vert J, Bach F (2013). “Learning sparse penalties
for change-point detection using max margin interval regression.” In
*Proceedings of the 30th International Conference on Machine Learning*,
volume 28, 172–180.

## See also

[`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md),
[`as_cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_labels.md)
to convert a plain changepoint set.

## Examples

``` r
cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
#> # A tibble: 2 × 5
#>   label_id series start   end change   
#>      <int> <chr>  <int> <int> <chr>    
#> 1        1 NA        40    60 change   
#> 2        2 NA        70    90 no_change
```
