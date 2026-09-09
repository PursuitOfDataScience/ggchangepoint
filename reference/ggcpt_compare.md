# Compare multiple changepoint detection methods

Runs several detectors on the same data and returns a faceted or
overlaid ggplot comparison. Respects
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for parallel execution if the `future.apply` package is available.

## Usage

``` r
ggcpt_compare(
  x,
  methods = c("pelt", "binseg", "amoc"),
  layout = c("facet", "overlay"),
  change_in = "mean",
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector (the data series). A one-column matrix or data frame
  is accepted; wider input is refused, because these detectors are
  univariate and flattening the columns would invent a changepoint at
  every seam. Use
  [`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
  for a panel of series.

- methods:

  Character vector of method names (passed to `cpt_detect`).

- layout:

  Layout type. `"facet"` (default) shows one panel per method;
  `"overlay"` draws all changepoints in one panel, colour-coded. The
  overlay **dodges** the rules horizontally so that two methods agreeing
  on an index are both visible, which moves each rule by up to half an
  observation — so read positions off `"facet"`, or off
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), and treat
  the overlay as a picture of agreement rather than of location.

- change_in:

  What to detect change in. Passed to each detector.

- seed:

  Optional seed for reproducible parallelism. Passed to
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  as `future.seed`, and to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) when running
  sequentially. Left `NULL` under a parallel plan, `future.seed = TRUE`
  is used, so the workers get parallel-safe streams but the run is not
  reproducible. The seed is scoped to this call: `.Random.seed` is saved
  and restored, so a seeded call inside a simulation loop does not pin
  the loop's own stream.

- ...:

  Additional arguments passed to each detector.

## Value

A ggplot object.

## Positions, not a time index

Unlike
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
and
[`autoplot.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/autoplot.ggcpt.md),
these two take no `index`: the input is reduced to a bare numeric
vector, so a `ts`, `xts`, `zoo` or `tsibble` is plotted (and tabulated)
in observation positions with an "Index" axis. To compare detectors on
dated data, run `cpt_detect(x, method = m, index = dates)` per method
and read [`tidy()`](https://generics.r-lib.org/reference/tidy.html)'s
`cp_index`, or plot the results with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Examples

``` r
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
ggcpt_compare(x, methods = c("pelt", "binseg"))
```
