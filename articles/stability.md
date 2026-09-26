# Stability, Deprecation and the Contract

Code that calls this package, and results saved from it, should keep
working. This page says what that promise covers, how anything in it is
changed, and what happens when an engine the package wraps disappears.

## What is stable

| Part | The promise |
|:---|:---|
| Exported functions and their arguments | Not removed or renamed without the deprecation cycle below |
| The `ggcpt` result | Slots and columns are only ever added; `changepoints$cp` is always the last observation before the change (`cp_convention = "left"`) |
| Condition classes and their fields | Branch on `ggchangepoint_unsupported`, not on the wording. Messages may be reworded in any release; classes and the fields listed in `?ggchangepoint-conditions` may not |
| The JSON schema | [`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md) writes schema 1.x. Fields are only added within 1.x; removing or renaming one makes it 2.0, and [`cpt_import()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_export.md) refuses a major version it does not know |
| A saved result | A `ggcpt` is a list of plain data: [`readRDS()`](https://rdrr.io/r/base/readRDS.html) reads it in any R session, with no packages installed. Only `$fit`, the engine’s own object, needs the engine |

A caller that uses the classes, not the words:

``` r

res <- tryCatch(cpt_detect(rnorm(50), method = "wbs", family = "poisson"),
                ggchangepoint_unsupported = function(e) e)
class(res)[1:3]
#> [1] "ggchangepoint_unsupported" "ggchangepoint_error"      
#> [3] "error"
res$supported
#> [1] "gaussian"
```

## What is not

- **Printed output, plot styling and message wording.** They are for
  people and get better.
- **`$fit`.** Its structure belongs to the engine and changes when the
  engine does.
- **Numbers that depend on an engine’s version.** Every result records
  the versions that made it (`fit$versions`),
  [`print()`](https://rdrr.io/r/base/print.html) notes when the
  installed engine has moved on, and
  [`cpt_verify()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_verify.md)
  re-runs a saved result and reports what changed.
- **The measurement tables** (`cpt_runtimes`, `cpt_null_sizes`, …).
  Their columns are stable; their values are re-measured each release,
  which is the point of them.
- **Anything reached with `:::`.**

## How something changes

1.  **A default that changes results** is announced in NEWS under
    “Changes to results”, with the old behaviour one argument away.
    0.6.0 has three: a two-observation minimum segment for the
    `changepoint` engines, `cpm`’s false-alarm budget scaling with the
    series, and
    [`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md)
    requiring a majority by default.
2.  **A deprecated function or argument** warns once per session
    (through `lifecycle`) for at least two minor releases, then errors
    for one, then goes. NEWS lists it under “Deprecated” with its
    replacement.
3.  **A breaking change** is made only in a minor release before 1.0,
    and only in a major one after it, listed under “Breaking changes”
    with the reason.

## When an engine leaves CRAN

Thirty-odd engines are in `Suggests`, and CRAN archives packages as a
matter of routine. When one goes:

- the method stays in
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
  and its wrapper and tests stay in the package, skipped, so it returns
  the day the engine does;
- [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  says where the engine can still be installed from (`fpop`, archived in
  September 2026, installs from R-Forge) and names the installed methods
  that detect the same kind of change;
- a scheduled check (the all-engines workflow) compares every declared
  dependency with the live CRAN index each week, so an archival is found
  by the check, not by a user.

## Versioned results

``` r

fit <- cpt_detect(Nile, method = "strucchange")
str(fit$versions)
#> List of 5
#>  $ engine        : chr "strucchange"
#>  $ engine_version: chr "1.6-0"
#>  $ ggchangepoint : chr "0.6.0"
#>  $ r             : chr "4.6.1"
#>  $ created       : chr "2026-09-26T13:12:11+0000"
```

`glance(fit)` carries the engine version as a column, so a table of
results made across a year of package updates says which engine made
which row.
