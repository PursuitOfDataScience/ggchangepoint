# Was a detected change the event you have in mind?

The mirror of
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md).
A detector found a change on 14 March and something is known to have
happened on 1 March: is that the same event? The detection's confidence
interval answers it: an event inside the interval is consistent with
being the change, one outside is not.

## Usage

``` r
cpt_attribute_event(fit, event, level = 0.95, ...)
```

## Arguments

- fit:

  A `ggcpt` object.

- event:

  One or more event locations: positions, values of the fit's index, or
  a
  [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  result.

- level:

  Confidence level of the interval. Defaults to `0.95`.

- ...:

  Further arguments for
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  (for example `B` or `seed` for a bootstrap interval).

## Value

A tibble with one row per event: `event`, `event_position`, the nearest
changepoint `cp` (and `cp_index`), its interval `ci_lower`/`ci_upper`,
`distance` (positions from the changepoint to the event), `inside` and a
plain-language `verdict`.

## See also

Other inference:
[`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md),
[`cpt_effect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_effect.md),
[`cpt_gof()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gof.md),
[`cpt_null_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_power.md),
[`cpt_robustness()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_robustness.md),
[`cpt_test_at()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_at.md),
[`cpt_test_null()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test_null.md)

## Examples

``` r
set.seed(1)
dates <- as.Date("2026-01-01") + 0:119
fit <- cpt_detect(c(rnorm(60), rnorm(60, 3)), method = "strucchange",
                  index = dates)
cpt_attribute_event(fit, as.Date(c("2026-02-28", "2026-04-10")))
#> # A tibble: 2 × 9
#>   event      event_position    cp cp_index   ci_lower ci_upper distance inside
#>   <chr>               <dbl> <int> <date>        <int>    <int>    <dbl> <lgl> 
#> 1 2026-02-28             59    60 2026-03-01       58       61       -1 TRUE  
#> 2 2026-04-10            100    60 2026-03-01       58       61       40 FALSE 
#> # ℹ 1 more variable: verdict <chr>
```
