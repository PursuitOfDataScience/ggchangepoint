# Conditions raised by ggchangepoint

Every error, warning and message this package raises carries a class, so
code that calls it can react to the *kind* of failure rather than
matching its wording. The text is written for a person and may be
improved between releases; the classes and the fields listed here are
the interface, and follow the package's deprecation policy.

## Errors

Every error inherits from `ggchangepoint_error` (and from `error` and
`condition`). The subclasses, most specific first:

- `ggchangepoint_input_error`:

  The call itself is not usable: a bad argument or data that violates a
  precondition. Its subclasses say which precondition:
  `ggchangepoint_short_series` (too few observations, for the package or
  for the engine that was asked; field `n`),
  `ggchangepoint_wrong_dimension` (a univariate method handed a matrix,
  or the reverse), `ggchangepoint_non_finite` (`NA`, `NaN` or `Inf`
  where the method needs finite data), `ggchangepoint_bad_type` (a
  factor, text, a survival object or timestamps where a numeric series
  was expected) and `ggchangepoint_bad_argument` (an argument value
  outside its documented range or vocabulary).

- `ggchangepoint_unsupported`:

  The request is well formed but not something this method offers: a
  `change_in`, `family` or capability the registry does not list for it.
  Fields `method`, `requested` and `supported` where they apply.
  Subclasses: `ggchangepoint_capability_absent` (the result carries no
  interval, statistic, path or posterior to read),
  `ggchangepoint_planned_method` (named in
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  but not wired) and `ggchangepoint_unknown_method`.

- `ggchangepoint_engine_missing`:

  A suggested engine package is not installed. Field `package`, and
  `install`, the call that installs it.

- `ggchangepoint_engine_error`:

  The upstream engine failed. Fields `engine`, `method` and, where it
  was caught, `parent`, the engine's own condition.

- `ggchangepoint_upstream_bug`:

  A known, version-guarded defect in an engine, refused rather than
  returned. Fields `package` and `version`.

- `ggchangepoint_internal_error`:

  A state the package should never reach: a bug here, worth reporting.

The functions that run many fits
([`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md))
record a failure that belongs to one method
(`ggchangepoint_engine_error`, `ggchangepoint_engine_missing`,
`ggchangepoint_unsupported`, `ggchangepoint_upstream_bug`, or a series
of the wrong shape or length for that method) and carry on; any other
error stops them, since every method would have hit it.

## Warnings

Every warning inherits from `ggchangepoint_warning`. The ones worth
handling by class: `ggchangepoint_assumption` and its subclasses
`ggchangepoint_scale_sensitive`, `ggchangepoint_data_type` and
`ggchangepoint_dependence` (the method's assumptions look violated for
this series); `ggchangepoint_degenerate_segmentation` (a changepoint
after every observation); `ggchangepoint_implausible_count` (more
changepoints than the series can plausibly hold);
`ggchangepoint_change_in_routed` (the request was answered with the
method's native change type); `ggchangepoint_argument_ignored`;
`ggchangepoint_level_not_applied`; `ggchangepoint_selection_unadjusted`
(p-values computed at data-chosen locations);
`ggchangepoint_collapsed_ladder`; `ggchangepoint_replicates_failed`;
`ggchangepoint_cp_dropped` (locations normalised away by
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md));
`ggchangepoint_irregular_index`; `ggchangepoint_na_omitted`;
`ggchangepoint_wide_interval`; `ggchangepoint_large_fit`;
`ggchangepoint_short_series_warning` (a series too short for the result
to be interpretable); `ggchangepoint_dropped_input` (constant
coordinates or out-of-range locations left out);
`ggchangepoint_engine_failed` (some methods failed and were left out);
`ggchangepoint_recycled_input`; `ggchangepoint_constraint` and
`ggchangepoint_deprecated`. Anything else is plain
`ggchangepoint_warning`.

## Messages

Informational messages inherit from `ggchangepoint_message`, so
`suppressMessages(classes = "ggchangepoint_message")` silences this
package without silencing the engines.

## See also

Other result class:
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md),
[`as_cpt_series()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_cpt_series.md),
[`as_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_ggcpt.md),
[`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md),
[`cpt_annotations()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotations.md),
[`cpt_export()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md),
[`cpt_verify()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_verify.md),
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md),
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md),
[`print.ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/print.ggcpt.md)

## Examples

``` r
res <- tryCatch(cpt_detect(c(1, 2), method = "pelt"),
                ggchangepoint_short_series = function(e) "too short")
res
#> [1] "too short"

# Branch on the kind of failure, not on its wording:
tryCatch(
  cpt_detect(rnorm(50), method = "pelt", change_in = "network"),
  ggchangepoint_unsupported = function(e) {
    paste("unsupported:", e$method, "offers", toString(e$supported))
  }
)
#> [1] "unsupported: pelt offers mean, var, meanvar"
```
