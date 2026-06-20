# Construct changepoint penalties

Helper to construct standard penalty values for use with changepoint
detection methods. Returns a numeric penalty value.

## Usage

``` r
cpt_penalty(type, n = NULL, k = 1, value = NULL)
```

## Arguments

- type:

  Penalty type: `"None"`, `"BIC"` (or `"SIC"`), `"MBIC"`, `"AIC"`,
  `"Hannan-Quinn"`, `"sSIC"`, or `"Manual"`.

- n:

  Series length. Required for BIC, MBIC, AIC, Hannan-Quinn, sSIC.

- k:

  Number of parameters per changepoint (typically 2 for mean+variance, 1
  for mean-only). Defaults to 1.

- value:

  Numeric value for `Manual` type.

## Value

A numeric penalty value.

## Examples

``` r
cpt_penalty("BIC", n = 100)
#> [1] 4.60517
cpt_penalty("AIC", n = 100)
#> [1] 2
cpt_penalty("Manual", value = 5)
#> [1] 5
```
