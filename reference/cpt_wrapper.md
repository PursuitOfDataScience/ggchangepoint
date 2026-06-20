# Changepoint wrapper

This function wraps a number of `cpt` functions from the changepoint
package and the `cpt.np()` function from the changepoint.np package. It
is handy that users can use this function to get the same changepoint
results as these functions output individually. Moreover, it returns a
tibble that inherits the tidyverse style. Functions from the changepoint
package do require data normality assumption by default, yet
changepoint.np is a non-parametric way to detect changepoints and let
data speak by itself. If user sets `change_in` as `np` (or `cpt_np`), a
seed should be set before using the function for the sake of
reproducibility. For more details on the changepoint and changepoint.np
packages, please refer to their documentation.

## Usage

``` r
cpt_wrapper(data, change_in = "mean_var", cp_method = "PELT", ...)
```

## Arguments

- data:

  A numeric vector.

- change_in:

  Choice of `mean_var`, `mean`, `var`, and `np` (or `cpt_np` for
  backward compatibility). Each choice corresponds to `cpt.meanvar()`,
  `cpt.mean()`, `cpt.var()` and `cpt.np()` respectively. The default is
  `mean_var`.

- cp_method:

  A wide range of choices (i.e., `AMOC`, `PELT`, `SegNeigh` or
  `BinSeg`). Please note when `change_in` is `np` or `cpt_np`, `PELT` is
  the only option.

- ...:

  Extra arguments for each `cpt` function mentioned in the `change_in`
  section.

## Value

A tibble including which point(s) is/are the changepoint along with raw
changepoint value corresponding to that changepoint. Changepoint
locations follow the convention of the `changepoint` package: the last
index of the left segment.

## References

Killick R, Eckley I (2014). “changepoint: An R package for changepoint
analysis.” *Journal of statistical software*, **58**(3), 1–19.

## Examples

``` r
set.seed(2022)
cpt_wrapper(c(rnorm(100,0,1),rnorm(100,0,10)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
cpt_wrapper(c(rnorm(100,0,1),rnorm(100,10,1)))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.225
```
