# Cite the method behind a result

Returns the bibliographic reference(s) for the method behind a `ggcpt`
result (or a method name), so an analysis can cite the right
methodological paper without leaving R.

## Usage

``` r
cpt_cite(x)
```

## Arguments

- x:

  A `ggcpt` object, a method name (e.g. `"pelt"`), or missing — in which
  case references for every known method are returned.

## Value

A tibble with columns `method` and `reference`, invisibly; the
references are also printed.

## Examples

``` r
cpt_cite("pelt")
#> [pelt] Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.
#> 
res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
cpt_cite(res)
#> [pelt] Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.
#> 
```
