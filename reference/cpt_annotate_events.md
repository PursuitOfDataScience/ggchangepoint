# Match detected changepoints to known events

Joins a table of real-world events to a detection result and reports all
three outcomes: changepoints an event explains, changepoints no event
explains, and events no changepoint found. Matching reuses the same
tolerance rule as
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
and
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
so the package has one notion of "close enough" throughout.

## Usage

``` r
cpt_annotate_events(
  object,
  events,
  location = NULL,
  label = NULL,
  tolerance = 5
)

# S3 method for class 'ggcpt_events'
print(x, ...)

# S3 method for class 'ggcpt_events'
tidy(x, ...)

# S3 method for class 'ggcpt_events'
autoplot(object, repel = NULL, ...)
```

## Arguments

- object:

  A `ggcpt_events` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- events:

  A data frame of events with a location column and a label column. The
  location may be on the position scale or — when the result carries a
  time index — on the index scale (dates, say); which one is detected
  automatically from the column's type and reported.

- location:

  Name of the location column. Defaults to the first column whose type
  matches the result's index (or the first numeric column).

- label:

  Name of the label column. Defaults to the first character or factor
  column.

- tolerance:

  Matching window in positions. Defaults to `5`.

- x:

  A `ggcpt_events` object.

- ...:

  Ignored.

- repel:

  Use ggrepel for the event labels? Defaults to `TRUE` when it is
  installed.

## Value

A `ggcpt_events` object: a list with

- `matched`:

  one row per matched pair: `cp`, `event`, `event_value` (the event's
  own location, on the index scale when it was given as one),
  `event_position` (that location as a position in the series) and
  `distance`.

- `unexplained`:

  detected changepoints with no event (`cp`).

- `undetected`:

  events with no changepoint (`event`, `event_value`, `event_position`).

`matched` and `unexplained` carry `cp_index`, the changepoint on the
original scale, when — and only when — the result carries a time index,
so `"cp_index" %in% names(x)` is the test for it.

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) flattens
those three slots into **one** table with a `status` column, and the row
count is therefore the number of changepoints *plus* the number of
events, not either one alone. The three values are `"matched"` (a pair:
both `cp` and `event` filled, with `distance`),
`"unexplained_changepoint"` (a changepoint with no event: `cp` filled,
`event` and `distance` `NA`) and `"undetected_event"` (an event with no
changepoint: `event` and `position` filled, `cp` and `distance` `NA`).

**Filter on `status`, not on `is.na(cp)`.** An
`"unexplained_changepoint"` row has a non-missing `cp`, so
`subset(tidy(x), !is.na(cp))` returns the matched pairs *and* the
unexplained changepoints — which is the natural mistake to make, and it
silently overstates how many changepoints an event explains. With
[`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## See also

[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`cpt_report()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_report.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(60), rnorm(60, 4))
dates <- as.Date("2020-01-01") + 0:119
fit <- cpt_detect(x, method = "pelt", index = dates)
events <- data.frame(when = as.Date(c("2020-03-01", "2020-04-15")),
                     what = c("policy change", "supply shock"))
cpt_annotate_events(fit, events)
#> ggcpt_events (tolerance 5 position(s))
#>   Changepoints explained by an event: 1
#>   Changepoints with no event:         0
#>   Events with no changepoint:         1
#> 
#> Matched:
#> # A tibble: 1 × 6
#>      cp cp_index   event         event_value event_position distance
#>   <int> <date>     <chr>         <date>               <int>    <int>
#> 1    60 2020-02-29 policy change 2020-03-01              61        1
#> 
#> Events the detector did not find:
#> # A tibble: 1 × 3
#>   event        event_value event_position
#>   <chr>        <date>               <int>
#> 1 supply shock 2020-04-15             106
```
