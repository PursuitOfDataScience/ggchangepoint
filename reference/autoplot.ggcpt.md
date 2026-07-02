# Autoplot a ggcpt object

Renders a changepoint detection result as a ggplot. The raw series is
drawn as a line (with optional points), changepoints are shown as
vertical lines, and (optionally) fitted segment levels, the engine's
fitted signal, and changepoint-location confidence intervals are
overlaid. Multivariate results (from `ecp`, `inspect`, `geomcp`, ...)
are drawn as faceted small-multiples with shared changepoint rules.

## Usage

``` r
# S3 method for class 'ggcpt'
autoplot(
  object,
  show_segments = FALSE,
  show_ci = FALSE,
  show_fit = FALSE,
  cptline_alpha = 1,
  cptline_color = "blue",
  cptline_type = "solid",
  cptline_linewidth = 0.5,
  show_points = NULL,
  show_line = TRUE,
  index = NULL,
  ...
)
```

## Arguments

- object:

  A `ggcpt` object.

- show_segments:

  Logical. Whether to draw the fitted segment means. Defaults to
  `FALSE`.

- show_ci:

  Logical. Whether to draw confidence intervals for changepoint
  locations, when the engine provides them (columns
  `ci_lower`/`ci_upper` on the changepoints tibble — SMUCE, strucchange,
  segmented). Drawn as horizontal whiskers near the bottom of the panel.
  Defaults to `FALSE`.

- show_fit:

  Logical. Whether to draw the engine's fitted signal (the `fitted`
  column of `$data`, provided by SMUCE, DeCAFS, cpop, segmented, bcp,
  beast). Defaults to `FALSE`.

- cptline_alpha:

  Alpha for changepoint lines. Defaults to `1`.

- cptline_color:

  Color for changepoint lines. Defaults to `"blue"`.

- cptline_type:

  Linetype for changepoint lines. Defaults to `"solid"`.

- cptline_linewidth:

  Linewidth for changepoint lines. Defaults to `0.5`.

- show_points:

  Logical. Whether to draw data points. Auto-off above 500 obs.

- show_line:

  Logical. Whether to draw the line. Defaults to `TRUE`.

- index:

  Optional vector of x-axis values (e.g. dates) of the same length as
  the series; defaults to the observation index.

- ...:

  Unknown arguments are ignored with a warning.

## Value

A ggplot object.
