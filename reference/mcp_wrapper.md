# Bayesian formula-based changepoint regression (mcp)

Wraps [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html)
(Lindeløv): a Bayesian multiple-changepoint regression specified as a
*list of formulas*, one per segment. This is the most expressive
detector in the package — each segment can have its own intercept,
slope, variance and autocorrelation, and the changepoints themselves get
full posterior distributions rather than point estimates, which
[`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
already knows how to draw.

## Usage

``` r
mcp_wrapper(
  x,
  change_in = c("mean", "slope", "var"),
  n_changepoints = 1,
  model = NULL,
  prior = list(),
  iter = 3000,
  adapt = 1000,
  chains = 3,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector.

- change_in:

  Shorthand for the segment model when `model` is not given: `"mean"`
  fits a change in intercept (`list(y ~ 1, ~ 1)`), `"slope"` a change in
  slope (`list(y ~ 1 + t, ~ 0 + t)`), `"var"` a change in residual
  standard deviation (`list(y ~ 1, ~ 0 + sigma(1))`).

- n_changepoints:

  Number of changepoints in the shorthand model. Defaults to `1`.

- model:

  An explicit mcp model: a list of formulas. Overrides `change_in` and
  `n_changepoints`, and is the reason to reach for this engine at all.

- prior:

  Optional named list of priors, passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

- iter, adapt, chains:

  Sampler settings, passed through.

- seed:

  Optional seed.

- ...:

  Additional arguments passed to
  [`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html).

## Value

A `ggcpt` object. The changepoints tibble carries the posterior mean
location together with `ci_lower`/`ci_upper` from the posterior
quantiles, and `$data$fitted` holds the posterior predictive mean, so
`autoplot(show_ci = TRUE, show_fit = TRUE)` shows both.

## JAGS is a system dependency

mcp samples through JAGS, which is a separate program that has to be
installed outside R; mcp imports rjags, which is built against it, so if
JAGS is missing mcp will not install at all and this wrapper reports
that rather than failing obscurely. Everything else in the package works
without it.

## References

Lindeløv JK (2020). “mcp: An R package for regression with multiple
change points.” *OSF Preprints*.
[doi:10.31219/osf.io/fzqxv](https://doi.org/10.31219/osf.io/fzqxv) .

## Examples

``` r
# \donttest{
set.seed(2026)
fit <- mcp_wrapper(c(rnorm(60), rnorm(60, 4)), iter = 500, adapt = 200)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 120
#>    Unobserved stochastic nodes: 4
#>    Total graph size: 1460
#> 
#> Initializing model
#> 
#> Finished sampling in 0.4 seconds
#> Family: gaussian(link = 'identity')
#> Iterations: 1500 from 3 chains.
#> Segments:
#>   1: ~, y, 1
#>   2: ~, y ~ 1, 1
#> 
#> Population-level parameters:
#>     name mean lower upper Rhat n.eff
#>     cp_1 60.5 60.04 60.99    1  1125
#>    int_1 -0.1 -0.33  0.14    1  1203
#>    int_2  3.9  3.66  4.18    1  1118
#>  sigma_1  1.0  0.90  1.17    1  1031
fit
#> ggcpt (changepoint detection result)
#>   Method:         mcp
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         posterior 
#>   Series length:   120 
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1    61     3.86       60       61
# }
```
