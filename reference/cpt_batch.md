# Batch changepoint detection over many series

Runs one detector over every series in a collection — the panel-data
loop that methodological and applied work both need constantly. Accepts
a matrix/data frame (one column per series) or a named list of numeric
vectors. Honours
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for parallel execution when the future.apply package is available, with
parallel-safe RNG.

## Usage

``` r
cpt_batch(
  x,
  method = "pelt",
  change_in = "mean",
  index = NULL,
  seed = NULL,
  keep_fit = TRUE,
  ...
)

# S3 method for class 'ggcpt_batch'
print(x, ...)

# S3 method for class 'ggcpt_batch'
tidy(x, ...)

# S3 method for class 'ggcpt_batch'
autoplot(object, ...)
```

## Arguments

- x:

  For `cpt_batch()`, a numeric matrix or data frame (columns are series)
  or a list of numeric vectors; for the
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods, a
  `ggcpt_batch` object.

- method:

  Detection method, passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- change_in:

  What to detect change in, passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- index:

  Optional time index shared by every series in the panel (a vector of
  dates, say), or a named list of one index per series. Carried onto
  each result and used by
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  so a faceted plot of fifty series shows dates rather than positions.

- seed:

  Optional seed for reproducible parallel execution (passed to
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  as `future.seed`; applied via
  [`set.seed()`](https://rdrr.io/r/base/Random.html) when running
  sequentially).

- keep_fit:

  Keep each engine's raw fit in `result[[i]]$fit`? Defaults to `TRUE`,
  which is what makes a batch result as inspectable as a single one. Set
  it to `FALSE` for a large panel: a few engines return fits far bigger
  than the data they were given — `strucchange` keeps a triangular
  \\O(n^2)\\ RSS matrix, so a single 2000-point series costs about 135
  MB, and `bfast` and `bocpd` are in the tens of MB — and a panel
  multiplies that by the number of series. Everything else on the
  object, including
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  is unaffected; only accessors that read `$fit`
  ([`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md),
  `cpt_confint(engine = \"native\")`) need it.

- ...:

  Additional arguments passed to every
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  call.

- object:

  A `ggcpt_batch` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_batch` object: a tibble with one row per series and columns
`series`, `n_changepoints`, `changepoints` (a list-column of tidy
tibbles), and `result` (a list-column of `ggcpt` objects). Methods:
[`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) (one row per
changepoint across all series), and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(faceted small-multiples with each series' changepoints).

## Examples

``` r
set.seed(2026)
X <- cbind(a = c(rnorm(60), rnorm(60, 4)), b = rnorm(120))
batch <- cpt_batch(X, method = "pelt")
batch
#> ggcpt_batch (2 series, method: pelt)
#> 
#> # A tibble: 2 × 2
#>   series n_changepoints
#>   <chr>           <int>
#> 1 a                   1
#> 2 b                   0
tidy(batch)
#> # A tibble: 1 × 3
#>   series    cp cp_value
#>   <chr>  <int>    <dbl>
#> 1 a         60   -0.999
ggplot2::autoplot(batch)
```
