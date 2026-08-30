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

mcp samples through JAGS, a separate program installed outside R. Having
the *package* is not the same as being able to *run* it: mcp imports
rjags, and on some platforms rjags installs happily and only fails when
it looks for the JAGS library at run time — in which case
[`mcp::mcp()`](https://lindeloev.github.io/mcp/reference/mcp.html)
returns a fit carrying no posterior samples, with a warning rather than
an error. This wrapper checks for that and says so plainly instead of
failing several frames later inside
[`summary()`](https://rdrr.io/r/base/summary.html). Everything else in
the package works without JAGS.

## References

Lindeløv JK (2020). “mcp: An R package for regression with multiple
change points.” *OSF Preprints*.
[doi:10.31219/osf.io/fzqxv](https://doi.org/10.31219/osf.io/fzqxv) .

## See also

Other changepoint engines:
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
[`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md),
[`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md),
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
[`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md),
[`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md),
[`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md),
[`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md),
[`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md),
[`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md),
[`fabisearch_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fabisearch_wrapper.md),
[`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md),
[`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md),
[`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md),
[`fpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md),
[`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md),
[`hdcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdcov_wrapper.md),
[`hdreg_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdreg_wrapper.md),
[`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md),
[`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md),
[`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md),
[`kwc_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kwc_wrapper.md),
[`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md),
[`network_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/network_wrapper.md),
[`not_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/not_wrapper.md),
[`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md),
[`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md),
[`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md),
[`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md),
[`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md),
[`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md),
[`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md),
[`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md),
[`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md),
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
# Not run by R CMD check: whether this works depends on a *system*
# library, and no test of installed R packages predicts that reliably --
# `rjags` can be present and still fail to find JAGS at run time.
if (FALSE) { # \dontrun{
set.seed(2026)
fit <- mcp_wrapper(c(rnorm(60), rnorm(60, 4)), iter = 500, adapt = 200)
fit
} # }
```
