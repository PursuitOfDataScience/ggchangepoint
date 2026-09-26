# Test for a change at a date fixed in advance

The question behind most applied changepoint work: the policy took
effect on 1 March, the plant was retooled in week 14, the drug was
approved in Q2. *Did anything change then?* Because the date was chosen
before looking at the data, this is an ordinary two-sample comparison at
a fixed split and needs no adjustment for selection: the p-value means
what it says.

## Usage

``` r
cpt_test_at(
  x,
  when,
  window = 0,
  change_in = c("mean", "var", "meanvar", "distribution"),
  family = c("gaussian", "poisson", "binomial", "exponential", "l1"),
  span = NULL,
  index = NULL,
  data = NULL,
  level = 0.95,
  B = 999,
  seed = NULL
)

# S3 method for class 'ggcpt_test_at'
print(x, ...)
```

## Arguments

- x:

  A numeric series (a vector, `ts`, `zoo` and so on), a `ggcpt` fit (its
  series and index are used), or a formula with `data` for a break in a
  regression (a Chow test).

- when:

  When the change took effect: the **first observation of the new
  regime**, as a position or a value of the series' index (a date; for a
  numeric index such as a `ts`'s years, a number outside `1..n` is read
  as an index value). A detected changepoint `cp` (the last observation
  of the old regime) corresponds to `when = cp + 1`.

- window:

  Allow the change anywhere within `window` observations either side of
  `when`. The statistic is then the largest over the window and its
  p-value comes from a permutation distribution of that maximum, which
  pays for the search. Defaults to `0`: the date is exact.

- change_in:

  What to test: `"mean"` (the default), `"var"`, `"meanvar"` or
  `"distribution"`.

- family:

  For `change_in = "mean"`: `"gaussian"` (a Welch t-test, the default),
  `"poisson"` (an exact test of two rates), `"binomial"` (Fisher's exact
  test of two proportions), `"exponential"` (an exact F test of two
  rates) or `"l1"` (a Wilcoxon rank-sum test, robust to outliers).

- span:

  Optional number of observations each side of the split to compare.
  Defaults to all of them; a span keeps a change elsewhere in a long
  series out of the comparison.

- index:

  Optional time index when `x` is a bare series.

- data:

  A data frame, for formula input.

- level:

  Confidence level for the interval on the change.

- B:

  Permutations for `window > 0`. Defaults to `999`.

- seed:

  Optional seed for the permutations, scoped to this call.

- ...:

  Ignored.

## Value

A `ggcpt_test_at` tibble with one row: `when`, `cp` (the last
observation before it, in the package's convention) and `cp_index`,
`window`, `estimate` (the change: a difference in means, a ratio of
rates or variances), `conf_low`, `conf_high`, `statistic`, `p_value`,
`n_before`, `n_after`, `method` and `selection_adjusted` (`TRUE`: the
location was not chosen from the data, and a window's search is paid for
by the permutation).

## See also

[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md)
for the mirror question (was a detected change the event you know
about?),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md).

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_attribute_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute_event.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
dates <- as.Date("2026-01-01") + 0:119
x <- c(rnorm(60), rnorm(60, 0.8))
cpt_test_at(x, when = as.Date("2026-03-02"), index = dates)
#> ggcpt_test_at (change in mean; the location was fixed in advance, so no selection adjustment is needed)
#> 
#> # A tibble: 1 × 13
#>   when          cp cp_index   window estimate conf_low conf_high statistic
#>   <date>     <int> <date>      <int>    <dbl>    <dbl>     <dbl>     <dbl>
#> 1 2026-03-02    60 2026-03-01      0    0.804    0.484      1.12      4.98
#> # ℹ 5 more variables: p_value <dbl>, n_before <int>, n_after <int>,
#> #   method <chr>, selection_adjusted <lgl>
# the policy took effect sometime in that fortnight
cpt_test_at(x, when = 61, window = 7, seed = 1)
#> ggcpt_test_at (change in mean; the location was fixed in advance, so no selection adjustment is needed)
#> 
#> # A tibble: 1 × 12
#>    when    cp window estimate conf_low conf_high statistic p_value n_before
#>   <dbl> <int>  <int>    <dbl>    <dbl>     <dbl>     <dbl>   <dbl>    <int>
#> 1    61    60      7    0.804    0.484      1.12      4.98   0.001       60
#> # ℹ 3 more variables: n_after <int>, method <chr>, selection_adjusted <lgl>
```
