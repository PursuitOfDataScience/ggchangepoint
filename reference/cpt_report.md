# A reproducible report of a changepoint analysis

Assembles everything a reader needs to judge a changepoint result into
one artifact: the method and its citation, the penalty, the number and
locations of the changepoints with intervals where available, the
segment table, stability and diagnostics if they were computed, and the
session information. Reproducibility and correct attribution in one
call.

## Usage

``` r
cpt_report(
  object,
  format = c("md", "text", "gt", "json"),
  file = NULL,
  stability = NULL,
  events = NULL,
  confint = TRUE,
  session = TRUE
)
```

## Arguments

- object:

  A `ggcpt` object.

- format:

  `"md"` (default, GitHub-flavoured markdown as a character vector),
  `"text"` (plain text), `"gt"` (a gt table of the changepoints, for a
  publication) or `"json"` (the whole result under
  [`as_json()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/as_json.md)'s
  versioned schema, for a pipeline rather than a reader).

- file:

  Optional path to write to. The report is returned invisibly when a
  file is written. Ignored for `format = "gt"`, which returns a table
  object rather than lines of text.

- stability:

  Optional
  [`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
  result to include.

- events:

  Optional
  [`cpt_annotate_events()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_annotate_events.md)
  result to include.

- confint:

  Include a
  [`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
  table? Defaults to `TRUE`, which uses the engine's own intervals when
  it has them and skips the section otherwise (rather than silently
  bootstrapping, which would be slow and unexpected inside a report).

- session:

  Include [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)?
  Defaults to `TRUE`.

## Value

A character vector of report lines (or a gt table when `format = "gt"`).

## See also

[`cpt_gt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_gt.md),
[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt")
cat(cpt_report(fit, session = FALSE), sep = "\n")
#> # Changepoint analysis report
#> 
#> - Method: `pelt`
#> - Change in: mean
#> - Penalty: MBIC
#> - Series length: 120
#> - Changepoints found: 1
#> - Detection runtime: 0.004 s
#> 
#> ## Changepoints
#> 
#> ```
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60   -0.999
#> ```
#> 
#> ## Segments
#> 
#> ```
#> # A tibble: 2 × 5
#>   seg_id start   end     n param_estimate
#>    <int> <int> <int> <int>          <dbl>
#> 1      1     1    60    60         -0.110
#> 2      2    61   120    60          3.90 
#> ```
#> 
#> ## Assumptions
#> 
#> ```
#> ggcpt_assumptions (method: pelt)
#>    residual_dependence       Ljung-Box at lag 10; lag-1 autocorrelation -0.13
#>    scale_sensitivity         noise sd 1.29; this engine's cost assumes unit noise
#>    expected_false_positives  measured on pure noise at n = 1,000 (50 replicates)
#>    count_plausibility        1 changepoints in 120 observations (0.83 per hundred)
#>    data_type                 looks continuous; family gaussian
#> 
#> No assumption check raised a concern.
#> ```
#> 
#> ## Citation
#> 
#> [pelt] Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.
#> 
#> 
#> ## Reproducibility
#> 
#> ```
#> Call:
#> cpt_detect(x = c(rnorm(60), rnorm(60, 4)), method = "pelt")
#> ```
#> 
#> The changepoints, the series and the metadata are plain data: a saved result reads back in any R session with no packages installed, and cpt_export() writes JSON or CSV for other languages. Only `$fit`, the engine's own object (0 MB), needs the engine; drop it before saving with `fit$fit <- NULL`, or refit with `keep_fit = FALSE`.
#> 
```
