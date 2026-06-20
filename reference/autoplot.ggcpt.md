# Autoplot a ggcpt object

Renders a changepoint detection result as a ggplot. The raw series is
drawn as a line (with optional points), changepoints are shown as
vertical lines, and (optionally) fitted segment levels are overlaid.

## Usage

``` r
# S3 method for class 'ggcpt'
autoplot(
  object,
  show_segments = FALSE,
  show_ci = FALSE,
  cptline_alpha = 1,
  cptline_color = "blue",
  cptline_type = "solid",
  cptline_linewidth = 0.5,
  show_points = NULL,
  show_line = TRUE,
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
  locations (if available). Defaults to `FALSE`.

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

- ...:

  Additional arguments passed to `ggcptplot_internal`.

## Value

A ggplot object.
