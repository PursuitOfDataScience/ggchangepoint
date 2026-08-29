# BEAST wrapper — Bayesian estimation of abrupt change, seasonality, and trend

Wraps [`Rbeast::beast()`](https://rdrr.io/pkg/Rbeast/man/beast.html)
(Zhao et al., 2019), a Bayesian model-averaging ensemble that estimates
the number and location of trend changepoints together with their
posterior occurrence probabilities. Locations whose posterior
probability reaches `prob_threshold` are reported; the probability
profile renders via
[`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md).

## Usage

``` r
beast_wrapper(x, prob_threshold = 0.5, seed = NULL, ...)
```

## Arguments

- x:

  A numeric vector (treated as a non-seasonal series).

- prob_threshold:

  Posterior probability cutoff in \\(0, 1)\\ above which a candidate
  trend changepoint is reported. Defaults to `0.5`.

- seed:

  Optional seed for the engine's MCMC sampler (passed to
  [`Rbeast::beast()`](https://rdrr.io/pkg/Rbeast/man/beast.html) as
  `mcmc.seed`).

- ...:

  Additional arguments passed to
  [`Rbeast::beast()`](https://rdrr.io/pkg/Rbeast/man/beast.html).

## Value

A `ggcpt` object. The `changepoints` tibble carries `posterior_prob`,
and the `data` tibble carries the posterior mean trend in its `fitted`
column.

## References

Zhao K, Wulder MA, Hu T, Bright R, Wu Q, Qin H, Li Y, Toman E, Mallick
B, Zhang X, Brown M (2019). “Detecting change-point, trend, and
seasonality in satellite time series data to track abrupt changes and
nonlinear dynamics: A Bayesian ensemble algorithm.” *Remote Sensing of
Environment*, **232**, 111181.

## Examples

``` r
res <- beast_wrapper(c(rnorm(60), rnorm(60, 4)), seed = 2026)
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value posterior_prob
#>   <int>    <dbl>          <dbl>
#> 1    60    -1.79              1
```
