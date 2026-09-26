# Augment a ggcpt object

Returns the original data with added columns: `seg_id`, `.fitted`,
`.resid`, and `is_changepoint`.

## Usage

``` r
# S3 method for class 'ggcpt'
augment(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A tibble with one row per observation: the data as the result carries
it, plus four added columns. The data half depends on the result:
`index` and `value` for a univariate one, `index` plus **one column per
coordinate** (named as the input's columns were) for a multivariate one,
and an extra `fitted` column for the engines that supply their own
fitted signal. The added four are always the same:

- `seg_id`:

  which segment the observation falls in, counting from 1.

- `.fitted`:

  the segment's `param_estimate`: the segment **mean**, for every method
  in the package, or the engine's own fitted signal where there is one.
  See the details below for the multivariate case.

- `.resid`:

  `value - .fitted`, against the univariate series the result carries.

- `is_changepoint`:

  `TRUE` at each detected location, under the result's `cp_convention`.

Measured on a `pelt` fit the columns are `index`, `value`, `seg_id`,
`.fitted`, `.resid`, `is_changepoint`.

## Details

For a multivariate result every coordinate is returned, but the
changepoints are shared across them, so `seg_id` and `is_changepoint`
apply to the whole row. Each coordinate also gets its own
`.fitted_<name>` and `.resid_<name>` (its segment means and residuals),
while `.fitted` and `.resid` describe the **univariate series the result
carries**: `$data$value`, the same series `$segments$param_estimate`
summarises, so `.resid` is always `value - .fitted`. For most
multivariate engines that series is the first coordinate; `fmean`,
`fcov`, `kwc` and `fabisearch` store the cross-sectional mean
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) instead, and for
those `.fitted`/`.resid` describe that mean rather than any one column.
Either way the two columns agree with each other, which is what makes
`.resid` a residual. When an engine supplies its own fitted signal that
signal is used for `.fitted` in place of the segment means, and rides
along in a `fitted` column of its own, so for those engines the two
columns agree. The engines that do this are exactly the ones
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
marks in its `fitted` column: `smuce`, `hsmuce`, `cpop`, `bcp`, `bocpd`,
`beast`, `decafs`, `segmented`, `envcpt`, `mcp` and `bfast`; a formula
fit carries its piecewise regression's fitted values the same way.

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(60), rnorm(60, 3)), method = "pelt")
head(augment(fit))
#> # A tibble: 6 × 6
#>   index   value seg_id .fitted  .resid is_changepoint
#>   <int>   <dbl>  <int>   <dbl>   <dbl> <lgl>         
#> 1     1  0.521       1  -0.110  0.631  FALSE         
#> 2     2 -1.08        1  -0.110 -0.970  FALSE         
#> 3     3  0.139       1  -0.110  0.249  FALSE         
#> 4     4 -0.0847      1  -0.110  0.0254 FALSE         
#> 5     5 -0.667       1  -0.110 -0.556  FALSE         
#> 6     6 -2.52        1  -0.110 -2.41   FALSE         
```
