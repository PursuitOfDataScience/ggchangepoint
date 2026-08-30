# Rank observations by influence

Orders the observations of a
[`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
result by how much perturbing them disturbs the segmentation, most
influential first. The composite `leverage` score is the sum of three
standardised components — the change in the number of changepoints, the
largest movement of a changepoint, and the largest change in a segment
parameter — so an observation that shifts a location without changing
the count is still ranked.

## Usage

``` r
cpt_leverage(object, ...)
```

## Arguments

- object:

  A `ggcpt_influence` object, or a `ggcpt` object (in which case
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  is run first).

- ...:

  Passed to
  [`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md)
  when `object` is a `ggcpt`.

## Value

A tibble ordered by `leverage`, with columns `index`, `delta_n_cp`,
`max_shift`, `param_shift` and `leverage`.

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
head(cpt_leverage(fit), 3)
#> # A tibble: 3 × 5
#>   index delta_n_cp max_shift param_shift leverage
#>   <int>      <int>     <dbl>       <dbl>    <dbl>
#> 1    40          0         1      0.0201     8.90
#> 2    15          0         0      0.0642     2.93
#> 3     6          0         0      0.0635     2.88
```
