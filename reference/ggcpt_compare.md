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

  A numeric vector (the data series).

- methods:

  Character vector of method names (passed to `cpt_detect`).

- layout:

  Layout type. `"facet"` (default) shows one panel per method;
  `"overlay"` draws all changepoints in one panel, colour-coded.

- change_in:

  What to detect change in. Passed to each detector.

- seed:

  Optional seed for reproducible parallelism. Passed to
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  as `future.seed`.

- ...:

  Additional arguments passed to each detector.

## Value

A ggplot object.

## Examples

``` r
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
ggcpt_compare(x, methods = c("pelt", "binseg"))
```
