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
otherwise `NA`).

## Details

`total_cost` is reported on whatever scale the engine itself uses, so it
is meaningful when comparing penalties within one method and not when
comparing one method against another. For the changepoint engines it is
the unpenalised \\-2\log L\\ of the chosen segmentation. Four cases
there are `NA` rather than filled with a number that would not mean the
same thing:

- `"binseg"` and `"segneigh"`, whose `cpt.range` fits report the raw
  within-segment cost instead — for one and the same segmentation that
  is 219.7 where a PELT fit reports 659.9;

- `"np"`, because changepoint.np defines no `logLik` method;

- a change in *mean* under the default `"MBIC"` penalty. Loading
  changepoint.np — which this package imports, so it is always loaded —
  replaces changepoint's `logLik` method for `cpt` objects with one that
  errors on exactly that combination. Any other penalty (`"BIC"`,
  `"AIC"`, a numeric value) reports normally, as do `change_in = "var"`
  and `"meanvar"`.
