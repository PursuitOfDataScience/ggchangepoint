# CPOP wrapper — optimal change-in-slope detection

Wraps [`cpop::cpop()`](https://rdrr.io/pkg/cpop/man/cpop.html)
(Fearnhead, Maidstone and Letchford, 2019; Fearnhead and Grose, 2024):
exact penalised estimation of a *continuous* piecewise-linear mean via
dynamic programming with functional pruning. This is the engine behind
`cpt_detect(change_in = "slope")`. The fitted broken line is stored in
the `fitted` column and renders via `autoplot(show_fit = TRUE)`.

## Usage

``` r
cpop_wrapper(x, penalty = NULL, sd = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- penalty:

  Penalty for adding a changepoint. Defaults to `2 * log(length(x))`.
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  resolves its own `"MBIC"` default to a stronger numeric value, so the
  two entry points need not agree unless `penalty` is given.

- sd:

  Noise standard deviation; when `NULL` it is estimated from the data by
  the engine's default difference-based estimator.

- ...:

  Additional arguments passed to
  [`cpop::cpop()`](https://rdrr.io/pkg/cpop/man/cpop.html).

## Value

A `ggcpt` object with `change_in = "slope"`. For a continuous fit the
reported location is the kink point itself.

## References

Fearnhead P, Maidstone R, Letchford A (2019). “Detecting changes in
slope with an L0 penalty.” *Journal of Computational and Graphical
Statistics*, **28**(2), 265–275.

Fearnhead P, Grose D (2024). “cpop: Detecting changes in
piecewise-linear signals.” *Journal of Statistical Software*,
**109**(7), 1–30.

## Examples

``` r
set.seed(2026)
y <- cumsum(c(rep(0.3, 100), rep(-0.4, 100))) + rnorm(200)
res <- cpop_wrapper(y)
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100     30.4
ggplot2::autoplot(res, show_fit = TRUE)
```
