# Introspect available changepoint detection methods

Returns a tibble describing every method the package knows about — those
that are wired, those a user has registered with
[`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md),
and those that are planned — along with their capabilities and
installation status. Useful for discovering what can be run, what needs
to be installed, and which methods expose the extras the diagnostics
need (confidence intervals, a fitted signal, a posterior, a detector
statistic, a solution path, a bandwidth to sweep).

## Usage

``` r
cpt_methods(capabilities = TRUE)
```

## Arguments

- capabilities:

  Include the capability flag columns? Defaults to `TRUE`. Set `FALSE`
  for the compact 0.4.0-shaped table.

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

  `"available"` (wired in this release), `"registered"` (supplied by the
  user this session — see
  [`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)),
  or `"planned"` (future).

- installed:

  `TRUE` if the engine package is installed, `FALSE` if it is a
  `Suggests` engine that is missing, `NA` for planned and registered
  methods.

- target_release:

  What a planned method is waiting on: a release, or `"when on CRAN"`
  when the engine package itself is not available from CRAN. `NA` for
  methods that are already wired. Asking
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  for a planned method reports this rather than claiming the name does
  not exist.

- multivariate, univariate, online, ci, fitted, posterior, statistic,
  path, scale_space:

  Capability flags (omitted when `capabilities = FALSE`). `ci` means the
  engine supplies changepoint-location confidence intervals; `fitted` a
  length-\\n\\ fitted signal; `posterior` a per-location posterior
  probability; `statistic`, `path` and `scale_space` the internals
  rendered by
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md)
  and
  [`ggcpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md).

## See also

[`cpt_install_engines()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_install_engines.md)
to install a whole family of the engines this table reports on;
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
to run one;
[`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md)
to add your own.

## Examples

``` r
cpt_methods()
#> # A tibble: 55 × 15
#>    method   change_in        engine status installed target_release multivariate
#>    <chr>    <chr>            <chr>  <chr>  <lgl>     <chr>          <lgl>       
#>  1 pelt     mean, var, mean… chang… avail… TRUE      NA             FALSE       
#>  2 binseg   mean, var, mean… chang… avail… TRUE      NA             FALSE       
#>  3 segneigh mean, var, mean… chang… avail… TRUE      NA             FALSE       
#>  4 amoc     mean, var, mean… chang… avail… TRUE      NA             FALSE       
#>  5 np       distribution     chang… avail… TRUE      NA             FALSE       
#>  6 ecp      distribution (m… ecp    avail… TRUE      NA             TRUE        
#>  7 fpop     mean             fpop   avail… TRUE      NA             FALSE       
#>  8 wbs      mean             wbs    avail… TRUE      NA             FALSE       
#>  9 wbs2     mean             break… avail… TRUE      NA             FALSE       
#> 10 not      mean, var, slope not    avail… TRUE      NA             FALSE       
#> # ℹ 45 more rows
#> # ℹ 8 more variables: univariate <lgl>, online <lgl>, ci <lgl>, fitted <lgl>,
#> #   posterior <lgl>, statistic <lgl>, path <lgl>, scale_space <lgl>
# which methods can draw a confidence interval?
subset(cpt_methods(), ci)$method
#> [1] "smuce"       "hsmuce"      "strucchange" "segmented"   "nsp"        
#> [6] "mcp"         "bfast"       "taylor"     
```
