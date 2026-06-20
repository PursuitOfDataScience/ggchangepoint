
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggchangepoint

<!-- badges: start -->

[![R-CMD-check](https://github.com/PursuitOfDataScience/ggchangepoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PursuitOfDataScience/ggchangepoint/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggchangepoint)](https://CRAN.R-project.org/package=ggchangepoint)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

ggchangepoint provides a unified tidy interface to changepoint analysis
in R. It wraps multiple detection engines (changepoint, changepoint.np,
ecp, wbs, breakfast, not, mosum, fpop, IDetect, and gfpop) behind a
consistent S3 result class (`ggcpt`) with `broom`-style methods
(`tidy()`, `glance()`, `augment()`), `ggplot2` integration via
`autoplot()` and custom geoms, and a full method-comparison, evaluation,
and simulation toolkit.

The engines beyond `changepoint`, `changepoint.np`, and `ecp` are
optional (`Suggests`); install the ones you need. The original 0.1.0
functions (`cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`,
`ggecpplot()`) continue to work unchanged.

## Installation

Install the released version from CRAN:

``` r
install.packages("ggchangepoint")
```

Or the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("PursuitOfDataScience/ggchangepoint")
```

## Quick start

``` r
library(ggchangepoint)
library(ggplot2)
```

Generate a series with a mean shift:

``` r
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
```

Detect changepoints with the unified `cpt_detect()`:

``` r
res <- cpt_detect(x, method = "pelt", change_in = "mean")
res
#> ggcpt (changepoint detection result)
#>   Method:          pelt 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         MBIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
```

The result is a `ggcpt` S3 object. Print it to see the changepoints, or
use `tidy()`, `glance()`, and `augment()`:

``` r
tidy(res)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
glance(res)
#> # A tibble: 1 × 9
#>       n n_changepoints method change_in penalty_type penalty_value cp_convention
#>   <int>          <int> <chr>  <chr>     <chr>                <dbl> <chr>        
#> 1   200              1 pelt   mean      MBIC                    NA left         
#> # ℹ 2 more variables: total_cost <dbl>, runtime <dbl>
```

Visualise with `autoplot()`:

``` r
autoplot(res)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="ggchangepoint plot of a time series with detected changepoints" width="100%" />

## Unified detection across engines

`cpt_detect()` dispatches to any supported method by name:

``` r
cpt_detect(x, method = "binseg", change_in = "mean")
#> ggcpt (changepoint detection result)
#>   Method:          binseg 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         MBIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <dbl>    <dbl>
#> 1   100    0.467
cpt_detect(x, method = "wbs", change_in = "mean")
#> ggcpt (changepoint detection result)
#>   Method:          wbs 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         sSIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
cpt_detect(x, method = "fpop", change_in = "mean")
#> ggcpt (changepoint detection result)
#>   Method:          fpop 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         Manual = 10.5966347330961 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
```

## Compare methods

``` r
ggcpt_compare(x, methods = c("pelt", "binseg", "fpop", "wbs"))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" alt="ggchangepoint plot of a time series with detected changepoints" width="100%" />

For a numeric summary, use `ggcpt_compare_table()`:

``` r
ggcpt_compare_table(x, methods = c("pelt", "binseg", "fpop", "wbs"))
#> # A tibble: 4 × 3
#>   method    cp cp_value
#>   <chr>  <dbl>    <dbl>
#> 1 pelt     100    0.467
#> 2 binseg   100    0.467
#> 3 fpop     100    0.467
#> 4 wbs      100    0.467
```

## Evaluation

When ground truth changepoints are known, compute accuracy metrics:

``` r
cpt_metrics(pred = c(100), truth = c(100), n = 200)
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   200      1       1         1      1     1        1         0          1
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
```

## Data simulation

``` r
dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
                    params = c(0, 10), sd = 1)
attributes(dat)$true_changepoints
#> [1] 100
```

Built-in test signals include `signal_blocks()`, `signal_fms()`,
`signal_mix()`, `signal_teeth()`, and `signal_stairs()`.

## Original wrappers (0.1.0 API)

The original `cpt_wrapper()`, `ecp_wrapper()`, `ggcptplot()`, and
`ggecpplot()` continue to work unchanged for backward compatibility.

``` r
cpt_wrapper(x)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
ggcptplot(x)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" alt="ggchangepoint plot of a time series with detected changepoints" width="100%" />

## Learn more

See the vignettes for a comprehensive walkthrough:

- `vignette("introduction", package = "ggchangepoint")`
- `vignette("comparison", package = "ggchangepoint")`
