# Consensus changepoints across several detectors

Runs a set of detectors on one series and reports the locations they
agree on. Two detections count as the same changepoint when they fall
within `tolerance` of each other — the same tolerance window
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
matches on (van den Burg and Williams, 2020), so the package has one
notion of "close enough" and not two. The *grouping* necessarily
differs:
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
matches two sets one-to-one, while consensus has to cluster \\K\\ of
them, so detections are swept in order and a new cluster opens as soon
as one lies more than `tolerance` from the cluster's first member. That
cap stops a chain of near-neighbours merging into one arbitrarily wide
cluster.

## Usage

``` r
cpt_consensus(
  x,
  methods = c("pelt", "binseg", "amoc"),
  tolerance = 5,
  min_votes = 2,
  change_in = "mean",
  index = NULL,
  seed = NULL,
  ...
)

# S3 method for class 'ggcpt_consensus'
autoplot(object, plot_type = c("series", "agreement"), ...)

# S3 method for class 'ggcpt_consensus'
print(x, ...)
```

## Arguments

- x:

  A `ggcpt_consensus` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- methods:

  Character vector of method names.

- tolerance:

  Matching window, in positions. Defaults to `5`.

- min_votes:

  Minimum number of methods that must find a location for it to enter
  the consensus. Defaults to `2`.

  A value **strictly between 0 and 1** is read as a proportion of the
  methods that ran; anything else is a count. The boundary is worth
  knowing, because it falls exactly where a reader thinking in
  proportions would write “unanimous”: with three methods,
  `min_votes = 0.99` needs all three, while `min_votes = 1` – and `1.0`,
  which is the same number – is a count of one and so the *least* strict
  setting there is. For unanimity, pass the number of methods, or a
  fraction just below 1.

  A count larger than the number of methods that ran cannot be reached,
  so the consensus would be empty by construction; that warns rather
  than returning a result indistinguishable from “the methods agreed on
  nothing”.

- change_in:

  Passed to each detector.

- index:

  Optional time index, carried onto the result.

- seed:

  Optional seed for reproducibility. The seed is scoped to this call:
  `.Random.seed` is saved and restored, so a seeded call inside a
  simulation loop does not pin the loop's own stream.

- ...:

  Passed on to
  [`autoplot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md)
  for `plot_type = "series"`.

- object:

  A `ggcpt_consensus` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- plot_type:

  `"series"` (the consensus segmentation, votes shown by line width) or
  `"agreement"` (a method-by-location dot matrix showing exactly who
  voted for what).

## Value

A `ggcpt` object (so it plots and tidies like any other result) whose
changepoints tibble carries `votes` and `methods` (a comma-separated
list of the methods that found each location), with the per-method
detections kept in a `consensus` attribute and printed by
`autoplot(type = "agreement")`.

## Consensus is not inference

Agreement among detectors is **not** a p-value, and a location found by
six of seven methods is not thereby significant at any level: the
methods are run on the same data and are strongly correlated, several of
them share an engine, and none of the votes is independent. Read the
vote count as a robustness display — "this feature does not depend on
which detector I picked" — and use
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
or
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
when you need a guarantee.

## References

van den Burg GJJ, Williams CKI (2020). “An evaluation of change point
detection algorithms.” *arXiv preprint arXiv:2003.06222*.
[doi:10.48550/arXiv.2003.06222](https://doi.org/10.48550/arXiv.2003.06222)
.

## See also

[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md),
[`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md),
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(80), rnorm(80, 4))
cons <- cpt_consensus(x, methods = c("pelt", "binseg", "amoc"))
tidy(cons)
#> # A tibble: 1 × 5
#>      cp cp_value votes methods            spread
#>   <int>    <dbl> <int> <chr>               <int>
#> 1    80    0.785     3 amoc, binseg, pelt      0
ggplot2::autoplot(cons)
```
