# NSP wrapper — Narrowest Significance Pursuit

Wraps the nsp package (Fryzlewicz 2024). NSP inverts the usual framing
of post-selection inference: rather than estimating changepoint
locations and then asking whether they are real, it returns a set of
*intervals*, each of which contains at least one changepoint, with the
guarantee holding *globally* across all intervals simultaneously at
level `alpha`. The guarantee is exact and finite-sample, and the
self-normalised and autoregressive variants keep it under heavy tails,
heteroscedasticity and serial dependence.

## Usage

``` r
nsp_wrapper(
  x,
  alpha = 0.1,
  variant = c("poly", "selfnorm", "ar", "tvreg"),
  change_in = c("mean", "slope"),
  deg = NULL,
  M = 1000,
  covariates = NULL,
  ord = 1,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector.

- alpha:

  Global significance level: with probability at least \\1 - \alpha\\,
  *every* returned interval contains a changepoint. Defaults to `0.1`.

- variant:

  Which NSP procedure to run:

  `"poly"`

  :   (default)
      [`nsp::nsp_poly()`](https://rdrr.io/pkg/nsp/man/nsp_poly.html) —
      piecewise polynomial signal, Gaussian noise of constant variance.

  `"selfnorm"`

  :   [`nsp::nsp_poly_selfnorm()`](https://rdrr.io/pkg/nsp/man/nsp_poly_selfnorm.html)
      — self-normalised, for heavy tails and heteroscedasticity. Slower.

  `"ar"`

  :   [`nsp::nsp_poly_ar()`](https://rdrr.io/pkg/nsp/man/nsp_poly_ar.html)
      — autoregressive noise of order `ord`.

  `"tvreg"`

  :   [`nsp::nsp_tvreg()`](https://rdrr.io/pkg/nsp/man/nsp_tvreg.html) —
      a general linear model whose coefficients change; requires
      `covariates`.

- change_in:

  `"mean"` (a piecewise-constant signal, `deg = 0`) or `"slope"`
  (piecewise linear, `deg = 1`). Ignored when `deg` is given explicitly,
  and when `variant = "tvreg"` (which takes its model from
  `covariates`).

- deg:

  Degree of the piecewise polynomial. Derived from `change_in` when
  `NULL`.

- M:

  Number of intervals drawn. Defaults to `1000`; the engine's own
  default.

- covariates:

  A design matrix for `variant = "tvreg"`, with one row per observation.

- ord:

  AR order for `variant = "ar"`. Defaults to `1`.

- seed:

  Optional seed. NSP draws random intervals, so a run is reproducible
  only with one.

- ...:

  Additional arguments passed to the underlying nsp function.

## Value

A `ggcpt` object with a populated `regions` slot.

## What `cp` means here, and what it does not

NSP produces no point estimates. This wrapper still fills the `cp`
column — with the *midpoint* of each interval — because every downstream
consumer in the package
([`augment()`](https://generics.r-lib.org/reference/augment.html),
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html))
is built on that column, and a result with an empty `cp` would silently
score as "found nothing". The midpoint is **not** an estimate of the
changepoint location and must not be reported as one: the interval is
the inferential object. The result therefore

- carries the intervals in a `regions` slot, read with
  [`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md);

- marks itself, so [`print()`](https://rdrr.io/r/base/print.html) says
  the `cp` column is a midpoint and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  shades the bands by default;

- adds a `cp_source` column reading `"region_midpoint"` to the
  changepoints tibble.

## References

Fryzlewicz P (2024). “Narrowest significance pursuit: inference for
multiple change-points in linear models.” *Journal of the American
Statistical Association*, **119**(546), 1633–1646.
[doi:10.1080/01621459.2023.2211733](https://doi.org/10.1080/01621459.2023.2211733)
.

## See also

[`cpt_regions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_regions.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`cpt_confint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_confint.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(100), rnorm(100, 4))
fit <- nsp_wrapper(x, M = 100, seed = 1)
cpt_regions(fit)
#> # A tibble: 1 × 4
#>   start   end length value
#>   <int> <int>  <int> <dbl>
#> 1    96   105     10  4.54
ggplot2::autoplot(fit)
```
