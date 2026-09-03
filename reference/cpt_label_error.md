# Score a segmentation against labels

Counts label errors: a positive region with no changepoint is a false
negative, a negative region with one is a false positive, and a
`"one_change"` region with two or more is a false positive as well. This
is the accuracy measure supervised changepoint detection is built on,
and — unlike an information criterion — it is defined by what the expert
asserted rather than by a model assumption.

## Usage

``` r
cpt_label_error(object, labels)

# S3 method for class 'cpt_label_error'
tidy(x, ...)

# S3 method for class 'cpt_label_error'
print(x, ...)
```

## Arguments

- object:

  A `ggcpt` object, or an integer vector of changepoint positions.

- labels:

  A `cpt_labels` tibble (or anything with `start`/`end`/`change`
  columns).

- x:

  A `cpt_label_error` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- ...:

  Ignored.

## Value

A tibble with one row per label — `label_id`, `series` (the label set's
series identifier, `NA` for a single unnamed series), `start`, `end`,
`change`, `n_changes` (how many detections fell inside), `status`
(`"correct"`, `"false_positive"` or `"false_negative"`) — carrying the
totals in an `errors` attribute and printing them.

## See also

[`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md),
[`cpt_label_error_curve()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error_curve.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md).

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 4)), method = "pelt")
labs <- cpt_labels(c(40, 70), c(60, 95), c("one_change", "no_change"))
cpt_label_error(fit, labs)
#> cpt_label_error (2 label(s))
#>   correct: 2   false positives: 0   false negatives: 0
#>   total label errors: 0
#> 
#> # A tibble: 2 × 7
#>   label_id series start   end change     n_changes status 
#>      <int> <chr>  <int> <int> <chr>          <int> <chr>  
#> 1        1 NA        40    60 one_change         1 correct
#> 2        2 NA        70    95 no_change          0 correct
```
