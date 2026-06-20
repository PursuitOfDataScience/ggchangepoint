# Changepoint detection stat

Runs changepoint detection inside the ggplot pipeline. Useful for quick
exploration:
`ggplot(df, aes(t, y)) + geom_line() + stat_changepoint(method = "pelt")`.
Draws vertical lines at detected changepoint locations.

## Usage

``` r
stat_changepoint(
  mapping = NULL,
  data = NULL,
  geom = "vline",
  position = "identity",
  ...,
  method = "pelt",
  change_in = "mean",
  na.rm = FALSE,
  show.legend = NA
)
```

## Arguments

- mapping:

  Aesthetic mappings.

- data:

  A data frame.

- geom:

  The geometric object to use (default: "vline").

- position:

  Position adjustment.

- ...:

  Other arguments passed to the geom.

- method:

  Detection method (passed to `cpt_detect`).

- change_in:

  What to detect change in (passed to `cpt_detect`).

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Whether to show legend.

## Value

A ggplot layer.
