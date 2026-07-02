# EnvCpt wrapper — changepoints versus trends versus autocorrelation

Wraps [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html)
(Beaulieu and Killick, 2018), which fits up to twelve competing models —
constant mean or linear trend, each with or without changepoints, and
with white-noise, AR(1) or AR(2) errors — and lets an information
criterion decide whether the series really contains changepoints or
merely trend/autocorrelation ("memory"). The changepoints of the winning
model (if any) are returned, and the winning model's name is recorded,
guarding against the classic false positive of running a mean-shift
detector on autocorrelated data.

## Usage

``` r
envcpt_wrapper(
  x,
  models = c("mean", "meancpt", "meanar1", "meanar2", "meanar1cpt", "meanar2cpt",
    "trend", "trendcpt", "trendar1", "trendar2", "trendar1cpt", "trendar2cpt"),
  criterion = c("AIC", "BIC"),
  minseglen = 5,
  ...
)
```

## Arguments

- x:

  A numeric vector.

- models:

  Character vector of models to fit; see
  [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html).
  Defaults to all twelve.

- criterion:

  Model selection criterion: `"AIC"` (default) or `"BIC"`.

- minseglen:

  Minimum segment length. Defaults to `5`.

- ...:

  Additional arguments passed to
  [`EnvCpt::envcpt()`](https://rdrr.io/pkg/EnvCpt/man/envcpt.html).

## Value

A `ggcpt` object. `$fit` holds the full `envcpt` output; the selected
model name is stored in the penalty descriptor and printed by
[`glance()`](https://generics.r-lib.org/reference/glance.html) via
`penalty_type`.

## References

Beaulieu C, Killick R (2018). “Distinguishing trends and shifts from
memory in climate data.” *Journal of Climate*, **31**(23), 9519–9543.

## Examples

``` r
set.seed(2026)
res <- envcpt_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    98   -0.490
```
