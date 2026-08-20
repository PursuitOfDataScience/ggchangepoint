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

## Result size

`$fit` is the `breakpoints` object itself, and that object is quadratic
in the series length: it keeps `RSS.triang`, the triangular table of
segment residual sums of squares, which is what lets `strucchange`
return the optimal segmentation for *any* number of breaks without
refitting. Measured here, the whole result is about 1.7 MB at `n = 200`,
5.9 MB at `n = 400` and 22.6 MB at `n = 800` — roughly four times larger
each time the series doubles — and that one table outweighs everything
else in the fit put together, by a margin that widens as the series
grows. A single fit is not a problem; a few hundred of them are, so when
running this engine over a panel with
[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
keep what you need (`res$changepoints`) rather than the whole list of
results. No other engine here behaves this way: the median result across
the other thirty is under ten times the size of the series it was given.

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
