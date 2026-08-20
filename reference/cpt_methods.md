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

  What a planned method is waiting on: a release, or `"when on CRAN"`
  when the engine package itself is not available from CRAN. `NA` for
  methods that are already wired. Asking
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  for a planned method reports this rather than claiming the name does
  not exist.

## Examples

``` r
cpt_methods()
#> # A tibble: 35 × 6
#>    method   change_in                   engine   status target_release installed
#>    <chr>    <chr>                       <chr>    <chr>  <chr>          <lgl>    
#>  1 pelt     mean, var, meanvar          changep… avail… NA             TRUE     
#>  2 binseg   mean, var, meanvar          changep… avail… NA             TRUE     
#>  3 segneigh mean, var, meanvar          changep… avail… NA             TRUE     
#>  4 amoc     mean, var, meanvar          changep… avail… NA             TRUE     
#>  5 np       distribution                changep… avail… NA             TRUE     
#>  6 ecp      distribution (multivariate) ecp      avail… NA             TRUE     
#>  7 fpop     mean                        fpop     avail… NA             TRUE     
#>  8 wbs      mean                        wbs      avail… NA             TRUE     
#>  9 wbs2     mean                        breakfa… avail… NA             TRUE     
#> 10 not      mean, var, slope            not      avail… NA             TRUE     
#> # ℹ 25 more rows
```
