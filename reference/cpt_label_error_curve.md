# Label error as a function of the penalty

Runs one detector across a penalty grid and counts label errors at each
setting — the curve penalty learning is fitted to, and the honest way to
see whether *any* penalty can satisfy the labels.

## Usage

``` r
cpt_label_error_curve(
  x,
  labels,
  method = "pelt",
  penalties = NULL,
  change_in = "mean",
  ...
)

# S3 method for class 'ggcpt_label_curve'
print(x, ...)

# S3 method for class 'ggcpt_label_curve'
autoplot(object, ...)
```

## Arguments

- x:

  A `ggcpt_label_curve` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- labels:

  A `cpt_labels` tibble.

- method:

  Detection method. Defaults to `"pelt"`.

- penalties:

  Numeric vector of penalties to try. When `NULL` (the default) the grid
  is chosen *adaptively*: it starts below `log(n)`, where the
  segmentation shatters, and the top end is found by doubling until the
  detector reports no changepoints at all. A fixed grid cannot do this —
  on a series with a large change, a grid that stops at a few hundred
  never produces a false negative, the error curve never turns back up,
  and the target interval comes out unbounded above, which is useless to
  [`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md).
  The probe costs at most a dozen extra detector fits; pass `penalties`
  explicitly for an expensive engine.

- change_in:

  Passed to the detector.

- ...:

  Additional arguments passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- object:

  A `ggcpt_label_curve` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_label_curve` object: a tibble with `penalty`, `n_cp`, `errors`,
`false_positive`, `false_negative`, plus
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
The `target` attribute holds the interval of `log(penalty)` achieving
the minimum error, which is what
[`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md)
regresses on.

## See also

[`cpt_learn_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_learn_penalty.md),
[`cpt_select()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_select.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(60), rnorm(60, 4))
labs <- as_cpt_labels(60, n = 120)
curve <- cpt_label_error_curve(x, labs, penalties = c(2, 8, 32, 128))
curve
#> ggcpt_label_curve (method: pelt, 4 penalties)
#>   Minimum label errors: 0
#>   Target log-penalty interval: (2.079, Inf)
#> 
#> # A tibble: 4 × 5
#>   penalty  n_cp errors false_positive false_negative
#>     <dbl> <int>  <int>          <int>          <int>
#> 1       2    24      3              3              0
#> 2       8     1      0              0              0
#> 3      32     1      0              0              0
#> 4     128     1      0              0              0
```
