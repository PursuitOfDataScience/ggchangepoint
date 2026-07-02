# Bai-Perron structural break wrapper (strucchange)

Wraps
[`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html)
(Zeileis et al., 2002), the dynamic-programming implementation of the
Bai and Perron (1998, 2003) multiple structural break estimator. Called
with a bare numeric vector it dates mean shifts (`y ~ 1`); called with a
formula and data it dates breaks in arbitrary regression coefficients.
Break-date confidence intervals from
[`confint()`](https://rdrr.io/r/stats/confint.html) populate
`ci_lower`/`ci_upper` and render via `autoplot(show_ci = TRUE)`.

## Usage

``` r
strucchange_wrapper(
  x,
  data = NULL,
  breaks = NULL,
  h = 0.15,
  conf_level = 0.95,
  ...
)
```

## Arguments

- x:

  A numeric vector (mean-shift mode), or a model formula (regression
  mode; supply `data` too).

- data:

  Optional data frame for formula input.

- breaks:

  Maximum number of breaks; when `NULL` the number is chosen by BIC.

- h:

  Minimal segment size, as a fraction of the sample size (or an integer
  count). Defaults to `0.15`.

- conf_level:

  Confidence level for the break-date intervals. Defaults to `0.95`.

- ...:

  Additional arguments passed to
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html).

## Value

A `ggcpt` object with `ci_lower`/`ci_upper` columns on the changepoints
tibble.

## References

Bai J, Perron P (2003). “Computation and analysis of multiple structural
change models.” *Journal of Applied Econometrics*, **18**(1), 1–22.

Zeileis A, Leisch F, Hornik K, Kleiber C (2002). “strucchange: An R
package for testing for structural change in linear regression models.”
*Journal of Statistical Software*, **7**(2), 1–38.

## Examples

``` r
set.seed(2026)
res <- strucchange_wrapper(c(rnorm(100), rnorm(100, 3)))
res$changepoints
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100    0.369       99      102
```
