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
  show_regions = NULL,
  cptline_alpha = 1,
  cptline_color = "blue",
  cptline_type = "solid",
  cptline_linewidth = 0.5,
  show_points = NULL,
  show_line = TRUE,
  index = NULL,
  labels = NULL,
  type = c("series", "statistic", "path", "scale_space"),
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

- show_regions:

  Logical. Whether to shade the significance regions an interval-valued
  method returns (the `regions` slot — currently
  [`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)).
  Each band is an interval that contains at least one changepoint at the
  stated global level; it is not a confidence interval around a point
  estimate. Defaults to `TRUE` when the result carries regions, and is
  ignored otherwise.

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
  the series. Defaults to the time index carried by the result (see the
  `index` argument of
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)),
  and to the observation position when there is none.

- labels:

  Optional
  [`cpt_labels()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_labels.md)
  tibble. When supplied, the labelled regions are shaded behind the
  series and coloured by the outcome
  [`cpt_label_error()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_label_error.md)
  gives them — correct, false positive, false negative — so scoring a
  segmentation against expert labels becomes a picture rather than a
  table.

- type:

  Which view to draw. `"series"` (default) is the series with its
  changepoints; `"statistic"`, `"path"` and `"scale_space"` delegate to
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  and
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md),
  which error with the list of supporting engines when this one does not
  expose the internals.

- ...:

  Unknown arguments are ignored with a warning, except when `type` is
  not `"series"`, in which case they are passed to the delegate.

## Value

A ggplot object.
