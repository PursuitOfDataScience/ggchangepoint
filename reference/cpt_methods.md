# Introspect available changepoint detection methods

Returns a tibble describing every method the package knows about — those
that are wired and those that are planned — along with their
capabilities and installation status. Useful for discovering what can be
run and what needs to be installed.

## Usage

``` r
cpt_methods()
```

## Value

A tibble with columns:

- method:

  Method name as passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- change_in:

  What types of change the method can detect.

- engine:

  The upstream R package that implements the method.

- status:

  `"available"` (wired in this release) or `"planned"` (future).

- installed:

  `TRUE` if the engine package is installed, `FALSE` if it is a
  `Suggests` engine that is missing, `NA` for planned methods.

- target_release:

  The release that plans to wire this method, or `NA` for currently
  available methods.

## Examples

``` r
cpt_methods()
#> Warning: no DISPLAY variable so Tk is not available
#> # A tibble: 26 × 6
#>    method   change_in          engine         status    target_release installed
#>    <chr>    <chr>              <chr>          <chr>     <chr>          <lgl>    
#>  1 pelt     mean, var, meanvar changepoint    available NA             TRUE     
#>  2 binseg   mean, var, meanvar changepoint    available NA             TRUE     
#>  3 segneigh mean, var, meanvar changepoint    available NA             TRUE     
#>  4 amoc     mean, var, meanvar changepoint    available NA             TRUE     
#>  5 np       distribution       changepoint.np available NA             TRUE     
#>  6 ecp      distribution       ecp            available NA             TRUE     
#>  7 fpop     mean               fpop           available NA             TRUE     
#>  8 wbs      mean               wbs            available NA             TRUE     
#>  9 wbs2     mean               breakfast      available NA             TRUE     
#> 10 not      mean, var, slope   not            available NA             TRUE     
#> # ℹ 16 more rows
```
