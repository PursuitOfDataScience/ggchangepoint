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
#> Error in get_segment_table(model, data, family, par_x): This is a plateau-only model so no x-axis variable could be derived from the segment formulas. Use argument 'par_x' to set it explicitly
fit
#> function (object, ...) 
#> {
#>     UseMethod("fit")
#> }
#> <bytecode: 0x55a52bf3b760>
#> <environment: namespace:generics>
# }
```
