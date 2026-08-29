# Coerce a time series object to values plus a time index

Detection itself runs on positions — every wrapped engine assumes an
equally spaced sequence — but real series carry dates, and reporting a
changepoint as "index 147" when the data are monthly rainfall is an
unnecessary translation step for the user. `as_cpt_series()` is the one
place that separates the two: it pulls the numeric values out of a `ts`,
`xts`, `zoo`, `tsibble` or data frame and returns the time index
alongside them, so
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
can detect on positions and report on dates.

## Usage

``` r
as_cpt_series(x, index = NULL, check_regular = TRUE)
```

## Arguments

- x:

  A numeric vector or matrix, or a `ts`/`mts`, `xts`, `zoo` or `tsibble`
  object. The `xts`, `zoo` and `tsibble` paths need those packages
  installed (they are `Suggests`).

- index:

  Optional explicit index, one value per observation. Overrides any
  index carried by `x`, and is the way to attach dates to a plain
  numeric vector.

- check_regular:

  Warn when the index is not equally spaced? Defaults to `TRUE`. Every
  engine in the package assumes equal spacing, so an irregular index
  means the positions the engine sees are not the times the user means.

## Value

A list with components `values` (a numeric vector, or a matrix for
multivariate input), `index` (the time index, or `NULL` when there is
none) and `index_label` (a name for the x axis).

## Examples

``` r
as_cpt_series(1:10)$index
#> NULL
s <- as_cpt_series(1:10, index = as.Date("2020-01-01") + 0:9)
s$index
#>  [1] "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04" "2020-01-05"
#>  [6] "2020-01-06" "2020-01-07" "2020-01-08" "2020-01-09" "2020-01-10"
```
