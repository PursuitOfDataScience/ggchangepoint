# What the package promises, against what it delivers

The calibration suite: every guarantee the documentation states that a
simulation can check, with the realised value and its Monte Carlo
standard error. Location intervals and sets are scored for coverage
(conditional on a changepoint landing within 20 positions of the truth),
tests for their size at 5% on data with no change, monitors for their
in-control run length, and the panel screen for its false discovery
rate. The row for
[`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md)
at a location the data chose is there to show what
`selection_adjusted = FALSE` means: its nominal 5% is not what it
delivers.

## Usage

``` r
cpt_calibration
```

## Format

A tibble with one row per guarantee and setting:

- guarantee:

  what is promised, and by which function.

- call:

  the call that was measured.

- setting:

  the data and the options it was measured on.

- kind:

  `"coverage"`, `"size"` (for
  [`cpt_attribute()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_attribute.md),
  the family-wise error over the coordinates that did not change),
  `"run_length"` or `"false_discovery_rate"`.

- nominal:

  the promised value: a confidence level, a test's level or an average
  run length (for the e-detector, a lower bound).

- realised:

  the measured value.

- mcse:

  its Monte Carlo standard error.

- reps:

  replicates that counted (for coverage, those that detected the
  change).

## Source

`data-raw/calibration.R`.

## See also

The calibration article on the package website,
<https://pursuitofdatascience.github.io/ggchangepoint/articles/calibration.html>.

Other measurement tables:
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md),
[`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_null_sizes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_sizes.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)

## Examples

``` r
cpt_calibration[, c("call", "nominal", "realised", "mcse")]
#> # A tibble: 21 × 4
#>    call                                                  nominal realised   mcse
#>    <chr>                                                   <dbl>    <dbl>  <dbl>
#>  1 "cpt_detect(x, method = \"strucchange\")"                0.95   0.953  0.0047
#>  2 "cpt_detect(x, method = \"smuce\", alpha = 0.05)"        0.95   1      0.0005
#>  3 "cpt_confint(cpt_detect(x), method = \"bootstrap\")"     0.95   0.968  0.0056
#>  4 "cpt_test_at(x, when = 51)"                              0.05   0.0395 0.0044
#>  5 "cpt_test_at(x, when = 51, family = \"poisson\")"        0.05   0.038  0.0043
#>  6 "cpt_test_at(x, when = 51, family = \"binomial\")"       0.05   0.0295 0.0038
#>  7 "cpt_test_at(x, when = 51, family = \"exponential\")"    0.05   0.059  0.0053
#>  8 "cpt_test_at(x, when = 51, family = \"l1\")"             0.05   0.0485 0.0048
#>  9 "cpt_test_at(x, when = 51, window = 5)"                  0.05   0.048  0.0048
#> 10 "cpt_test_null(x)"                                       0.05   0.049  0.0048
#> # ℹ 11 more rows
```
