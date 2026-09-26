# Convert a result to JSON

The whole result as JSON, under a documented and versioned schema, for
software that consumes changepoint results rather than a person reading
them.

## Usage

``` r
as_json(x, ...)

# S3 method for class 'ggcpt'
as_json(x, pretty = TRUE, data = TRUE, assumptions = TRUE, digits = NA, ...)

# Default S3 method
as_json(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Ignored.

- pretty:

  Indent the output? Defaults to `TRUE`.

- data:

  Include the series itself (`index`, `value` and, where the engine
  supplied one, `fitted`)? Defaults to `TRUE`; without it the output is
  the answer alone, a few hundred bytes.

- assumptions:

  Include
  [`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)?
  Defaults to `TRUE`.

- digits:

  Significant digits for numbers. Defaults to `NA`, the full precision
  needed for an exact round trip.

## Value

A single string of class `"json"`.

## Schema 1.0.0

One object with these fields. Every field is always present; one that
does not apply is `null`, so a reader never has to test for a key.

- `schema`, `schema_version`:

  `"ggchangepoint.ggcpt"` and the version, `"1.0.0"`.

- `method`, `engine`, `engine_version`, `ggchangepoint_version`,
  `r_version`, `created`:

  what produced the result, and when (ISO 8601).

- `change_in`, `family`:

  what was detected, and the distribution family if one was asked for.

- `penalty`:

  `{"type", "value"}`.

- `cp_convention`:

  `"left"`: a changepoint is the last observation of its segment.

- `n`:

  the series length.

- `index`:

  `{"class", "label"}` of the time index, or `null`. Dates are written
  as ISO 8601 strings and timestamps with their offset.

- `changepoints`:

  an array of objects, one per changepoint: `cp` (position) and
  `cp_value`, with `cp_index` when there is an index, and every
  engine-specific column (`ci_lower`, `ci_upper`, `posterior_prob`,
  ...).

- `segments`:

  `seg_id`, `start`, `end`, `n`, `param_estimate`.

- `regions`, `coefficients`, `constraints`:

  the optional slots, as arrays of objects (or an object), or `null`.

- `diagnostics`:

  `residual_dependence` (Ljung-Box `p_value`, `lag`, `acf1`),
  `na_omitted` and `expected_false_positives` where recorded.

- `assumptions`:

  the
  [`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md)
  rows, or `null`.

- `call`:

  the call that made the result, as text.

- `data`:

  `{"index", "value", "fitted"}` arrays, or `null`.

## See also

[`cpt_export()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md)
to write a file,
[`cpt_import()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md)
to read one back,
[`cpt_report`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md)`(format = "json")`.

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`cpt_export()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md),
[`cpt_verify()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_verify.md),
[`ggchangepoint-conditions`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggchangepoint-conditions.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
set.seed(1)
fit <- cpt_detect(c(rnorm(50), rnorm(50, 3)), method = "pelt")
cat(as_json(fit, data = FALSE))
#> {
#>   "schema": "ggchangepoint.ggcpt",
#>   "schema_version": "1.0.0",
#>   "method": "pelt",
#>   "engine": "changepoint",
#>   "engine_version": "2.3",
#>   "ggchangepoint_version": "0.6.0",
#>   "r_version": "4.6.1",
#>   "created": "2026-09-26T13:06:45+0000",
#>   "change_in": "mean",
#>   "family": null,
#>   "penalty": {
#>     "type": "MBIC",
#>     "value": null
#>   },
#>   "cp_convention": "left",
#>   "n": 100,
#>   "index": null,
#>   "changepoints": [
#>     {
#>       "cp": 50,
#>       "cp_value": 0.881107726454215
#>     }
#>   ],
#>   "segments": [
#>     {
#>       "seg_id": 1,
#>       "start": 1,
#>       "end": 50,
#>       "n": 50,
#>       "param_estimate": 0.100448279960256
#>     },
#>     {
#>       "seg_id": 2,
#>       "start": 51,
#>       "end": 100,
#>       "n": 50,
#>       "param_estimate": 3.11732645386905
#>     }
#>   ],
#>   "regions": null,
#>   "coefficients": null,
#>   "constraints": null,
#>   "diagnostics": {
#>     "residual_dependence": {
#>       "p_value": 0.80524816626966,
#>       "statistic": 6.1180662037446,
#>       "lag": 10,
#>       "acf1": -0.00382933266738007
#>     }
#>   },
#>   "assumptions": [
#>     {
#>       "component": "residual_dependence",
#>       "value": 0.80524816626966,
#>       "flag": false,
#>       "detail": "Ljung-Box at lag 10; lag-1 autocorrelation 0",
#>       "advice": null
#>     },
#>     {
#>       "component": "scale_sensitivity",
#>       "value": 0.973588001682784,
#>       "flag": false,
#>       "detail": "noise sd 0.974; this engine's cost assumes unit noise",
#>       "advice": null
#>     },
#>     {
#>       "component": "expected_false_positives",
#>       "value": 0,
#>       "flag": false,
#>       "detail": "measured on pure noise at n = 1,000 (50 replicates)",
#>       "advice": null
#>     },
#>     {
#>       "component": "count_plausibility",
#>       "value": 1,
#>       "flag": false,
#>       "detail": "1 changepoints in 100 observations (1 per hundred)",
#>       "advice": null
#>     },
#>     {
#>       "component": "data_type",
#>       "value": null,
#>       "flag": false,
#>       "detail": "looks continuous; family gaussian",
#>       "advice": null
#>     }
#>   ],
#>   "call": "cpt_detect(x = c(rnorm(50), rnorm(50, 3)), method = \"pelt\")",
#>   "data": null
#> }
```
