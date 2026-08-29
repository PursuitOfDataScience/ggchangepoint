# Taylor's change point analyzer

Wraps
[`ChangePointTaylor::change_point_analyzer()`](https://rdrr.io/pkg/ChangePointTaylor/man/change_point_analyzer.html):
the bootstrap-and-recursion procedure of Wayne Taylor that the
quality-control and Six Sigma community uses as its default. Each
candidate is scored by the bootstrap probability that a change occurred
there, which gives a confidence level per changepoint and a confidence
interval for its location — both carried onto the result.

## Usage

``` r
taylor_wrapper(
  x,
  n_bootstraps = 1000,
  min_candidate_conf = 0.5,
  min_conf = 0.9,
  conf_level = 0.95,
  seed = NULL
)
```

## Arguments

- x:

  A numeric vector.

- n_bootstraps:

  Bootstrap samples per candidate. Defaults to `1000`; the engine
  accepts 100 to 1,000,000.

- min_candidate_conf:

  Minimum confidence for a candidate to be considered. Defaults to
  `0.5`.

- min_conf:

  Minimum confidence for a changepoint to be reported. Defaults to
  `0.9`.

- conf_level:

  Confidence level of the reported location intervals. Defaults to
  `0.95`.

- seed:

  Optional seed (the procedure is bootstrap-based).

## Value

A `ggcpt` object with `ci_lower`/`ci_upper` (so
`autoplot(show_ci = TRUE)` works) and a `confidence` column.

## References

Taylor WA (2000). *Change-Point Analysis: A Powerful New Tool for
Detecting Changes*. Taylor Enterprises, Libertyville, Illinois.

## Examples

``` r
set.seed(2026)
taylor_wrapper(c(rnorm(60), rnorm(60, 3)), n_bootstraps = 200, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:         taylor
#>   Change in:       mean 
#>   Changepoints found: 2 
#>   CP convention:   left 
#>   Penalty:         confidence = 0.9 
#>   Series length:   120 
#> 
#> Changepoints:
#> # A tibble: 2 × 5
#>      cp cp_value ci_lower ci_upper confidence
#>   <int>    <dbl>    <int>    <int>      <dbl>
#> 1    15   -2.55         6       50          1
#> 2    60   -0.999       59       61          1
```
