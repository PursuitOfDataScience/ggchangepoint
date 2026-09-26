# Glance at a ggcpt object

Returns a one-row summary of a changepoint detection result.

## Usage

``` r
# S3 method for class 'ggcpt'
glance(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A one-row tibble with columns: `n`, `n_changepoints`, `method`,
`change_in`, `penalty_type`, `penalty_value`, `cp_convention`,
`total_cost` (`NA` when the engine does not expose a cost), `runtime`
(elapsed seconds when measured by
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
otherwise `NA`) and `engine_version` (the version of the engine package
that produced the result, recorded when it was made, or `NA` for a
result with no engine package, such as one built by
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md);
see
[`cpt_verify()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_verify.md))
and `family` (the distribution family the fit was asked for with
`cpt_detect(family = )`, or `NA` for the method's own default).

## Details

`total_cost` is reported on whatever scale the engine itself uses, so it
is meaningful when comparing penalties within one method and not when
comparing one method against another. For the changepoint engines it is
the unpenalised \\-2\log L\\ of the chosen segmentation. Four cases
there are `NA` rather than filled with a number that would not mean the
same thing:

- `"binseg"` and `"segneigh"`, whose `cpt.range` fits report the raw
  within-segment cost instead: for one and the same segmentation that is
  219.7 where a PELT fit reports 659.9;

- `"np"`, because changepoint.np defines no `logLik` method;

- a change in *mean* under the default `"MBIC"` penalty. Loading
  changepoint.np (which this package imports, so it is always loaded)
  replaces changepoint's `logLik` method for `cpt` objects with one that
  errors on exactly that combination. Any other penalty (`"BIC"`,
  `"AIC"`, a numeric value) reports normally, as do `change_in = "var"`
  and `"meanvar"`.

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(60), rnorm(60, 3)), method = "pelt")
glance(fit)
#> # A tibble: 1 × 11
#>       n n_changepoints method change_in penalty_type penalty_value cp_convention
#>   <int>          <int> <chr>  <chr>     <chr>                <dbl> <chr>        
#> 1   120              1 pelt   mean      MBIC                    NA left         
#> # ℹ 4 more variables: total_cost <dbl>, runtime <dbl>, engine_version <chr>,
#> #   family <chr>
```
