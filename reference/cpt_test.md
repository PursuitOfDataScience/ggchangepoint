# Test detected changepoints

Attaches a test to each detected changepoint (`type = "jump"`) or to
each fitted segment (`type = "segment"`), using the engine's own test
where it has one and an explicitly *unadjusted* two-sample test where it
does not.

## Usage

``` r
cpt_test(object, type = c("jump", "segment"), correction = "none")
```

## Arguments

- object:

  A `ggcpt` object.

- type:

  `"jump"` (one row per changepoint: is the change at this location
  real?) or `"segment"` (one row per segment: does this segment differ
  from the one before it?).

- correction:

  Multiple-testing correction applied across the rows, one of the
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) methods (`"none"`,
  `"bonferroni"`, `"holm"`, `"BH"`, ...). Defaults to `"none"`; a
  `p_adjusted` column is added when it is not.

## Value

A tibble with columns `cp` (or `seg_id`), `estimate`, `statistic`,
`p_value`, `method` and `selection_adjusted`.

## Selection bias — read this before quoting a p-value

Testing a changepoint at a location that was *chosen because the data
looked like it changed there* is circular, and the resulting p-values
are anti-conservative, often severely. The `selection_adjusted` column
records, per row, whether the test accounts for that:

- `TRUE` for strucchange (the Chow/supF statistics the Bai–Perron
  framework supplies) and for segmented's Davies test, which is built
  for a nuisance parameter present only under the alternative;

- `FALSE` for the generic Welch two-sample fallback, which compares the
  segments either side of the changepoint as if the location had been
  fixed in advance. Useful as a descriptive effect size with a scale
  attached; not a valid significance test for the existence of the
  change.

For a guarantee that survives selection, use
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md)
(regions with exact global coverage) or
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md)
with `method = "nsp"`. The canonical post-detection tests of Jewell,
Fearnhead and Witten (2022) are implemented in `ChangepointInference`,
which is not on CRAN;
[`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
is the supported way to bring it in.

## See also

[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md).

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt")
cpt_test(fit)
#> Warning: `selection_adjusted` is FALSE for 1 of 1 row(s): the changepoint locations were chosen from these data, so those p-values are anti-conservative. See the selection-bias section of ?cpt_test.
#> # A tibble: 1 × 6
#>      cp estimate statistic  p_value method                    selection_adjusted
#>   <int>    <dbl>     <dbl>    <dbl> <chr>                     <lgl>             
#> 1    60     4.01      21.5 3.24e-42 Welch two-sample t (unad… FALSE             
```
