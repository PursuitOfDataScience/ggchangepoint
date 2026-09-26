# Introspect available changepoint detection methods

Returns a tibble describing every method the package knows about (those
that are wired, those a user has registered with
[`cpt_register_method()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_register_method.md),
and those that are planned), along with their capabilities and
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
  user this session; see
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
  probability; `statistic` and `path` the internals rendered by
  [`ggcpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md)
  and
  [`ggcpt_solution_path()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_solution_path.md),
  which error with the list of supporting engines when a result does not
  carry them.

  `scale_space` is not one of those, despite sitting beside them.
  Nothing stores a scale space on a result:
  [`cpt_scale_space()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scale_space.md)
  computes one on demand by sweeping a multiscale detector's bandwidth
  over the series, so it works on *any* series and any result, a `pelt`
  fit included. What this column marks is the two engines that sweep can
  be run *with*, i.e. the domain of that function's own `method`
  argument: `subset(cpt_methods(), scale_space)$method`.

  `online` means the *algorithm* is sequential (it consumes observations
  one at a time), and this table reports it because it governs how the
  method behaves in batch: an online detector's threshold is a rate per
  observation, so run over a whole series through
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  it reports roughly \\n / \mathrm{arl0}\\ changepoints by construction.
  It does **not** mean the method can be passed to
  [`cpt_monitor()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_monitor.md),
  which takes its own three: `"edetector"`, `"cpm"` and `"ocd"`. The two
  sets overlap without coinciding: `bocpd` is an online algorithm this
  table marks but the monitor does not offer, and `edetector` is native
  to this package rather than a wrapped engine, so it has no row here at
  all.

- families, choices, formula, min_segment, na_handling,
  cp_convention_upstream:

  The modelling vocabulary (omitted when `capabilities = FALSE`): the
  distribution families `cpt_detect(family = )` accepts (see
  [`cpt_families()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_families.md);
  `NA` for a distribution-free method); the engine's modelling-choice
  arguments and their legal values, which
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  validates; whether it takes a formula with covariates; the engine
  argument `min_segment` is translated into; what the engine does with a
  missing value, *measured* rather than read from its documentation
  (`"native"`, `"compacts"`, `"silent_loss"` or `"reject"`; see
  `cpt_detect(na_action = )`); and the engine's own changepoint
  convention, which the wrapper translates to `"left"` (`"right"`: it
  reports the first observation after the change; `"continuous"`: a
  location it estimates on a continuous scale; `"design"`: rows of a
  lagged design).

- scale_invariant, sequential, max_cp, tier, noise_model_arg, rate_arg,
  cost, max_n:

  Measured and recorded properties (omitted when
  `capabilities = FALSE`): whether the answer is the same at `x`,
  `10 * x` and `0.1 * x`; whether it depends on the direction of time;
  how many changepoints the method can return where that is fixed;
  `"general"` for the general-purpose workhorses; the argument that
  selects the noise model and the one that sets the false-alarm rate;
  the runtime class at \\n = 10{,}000\\ (`"fast"` under a second,
  `"moderate"` under ten, `"slow"`); and the longest series it finished
  within the measurement's time cap (`0` when it did not finish 1,000
  observations). The measurements behind them are the
  [`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)
  and
  [`cpt_invariances`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_invariances.md)
  data sets.

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
#> # A tibble: 55 × 29
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
#> 10 not      mean, var, mean… not    avail… TRUE      NA             FALSE       
#> # ℹ 45 more rows
#> # ℹ 22 more variables: univariate <lgl>, online <lgl>, ci <lgl>, fitted <lgl>,
#> #   posterior <lgl>, statistic <lgl>, path <lgl>, scale_space <lgl>,
#> #   families <chr>, choices <chr>, formula <lgl>, min_segment <chr>,
#> #   na_handling <chr>, cp_convention_upstream <chr>, scale_invariant <lgl>,
#> #   sequential <lgl>, max_cp <int>, tier <chr>, noise_model_arg <chr>,
#> #   rate_arg <chr>, cost <chr>, max_n <dbl>
# which methods can draw a confidence interval?
subset(cpt_methods(), ci)$method
#> [1] "smuce"       "hsmuce"      "strucchange" "segmented"   "nsp"        
#> [6] "mcp"         "bfast"       "taylor"     
```
