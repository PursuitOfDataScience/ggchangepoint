# A publication-ready changepoint table

Renders the changepoints of a result as a gt table, with the time index,
location intervals and segment levels formatted for a paper. Falls back
to a plain tibble, with a note, when gt is not installed.

## Usage

``` r
cpt_gt(object, title = NULL, subtitle = NULL, digits = 3)
```

## Arguments

- object:

  A `ggcpt` object.

- title, subtitle:

  Table title and subtitle. Sensible defaults are derived from the
  result.

- digits:

  Digits for the numeric columns. Defaults to `3`.

## Value

A gt table, or a tibble when gt is unavailable.

## See also

[`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).

## Examples

``` r
set.seed(2026)
cpt_gt(cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt"))


  


Changepoints (pelt)
```
