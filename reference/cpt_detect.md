# Unified changepoint detection dispatcher

Runs a changepoint detection method on a sequence and returns a tidy
`ggcpt` result object. This is the recommended entry point for most
users. See
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
for the full method table with engines and capabilities.

## Usage

``` r
cpt_detect(x, method = "pelt", change_in = "mean", penalty = "MBIC", ...)
```

## Arguments

- x:

  A numeric vector for univariate methods, or a numeric matrix/data
  frame (rows are time points) for the multivariate methods (`"ecp"`,
  `"inspect"`, `"geomcp"`, `"ocd"`, `"npmojo"`, `"kcp"`, `"fastcpd"`).

- method:

  Detection method. One of `"pelt"`, `"binseg"`, `"segneigh"`, `"amoc"`,
  `"np"`, `"ecp"`, `"fpop"`, `"wbs"`, `"wbs2"`, `"not"`, `"mosum"`,
  `"idetect"`, `"tguh"`, `"smuce"`, `"hsmuce"`, `"cpop"`, `"bcp"`,
  `"bocpd"`, `"beast"`, `"cpm"`, `"kcp"`, `"npmojo"`, `"decafs"`,
  `"sn"`, `"inspect"`, `"ocd"`, `"geomcp"`, `"strucchange"`,
  `"segmented"`, `"envcpt"`, or `"fastcpd"`. Methods whose engines live
  in `Suggests` prompt for installation when missing.

- change_in:

  What to detect change in. One of `"mean"`, `"var"`, `"meanvar"`,
  `"slope"`, or `"distribution"`. Defaults to `"mean"`. The requested
  value is validated against the method's capabilities (see
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md));
  incompatible combinations error rather than silently running something
  else.

- penalty:

  Penalty type or value. Either a character string (`"MBIC"`, `"BIC"`,
  `"SIC"`, `"AIC"`, `"Hannan-Quinn"`, `"None"`) or a numeric penalty
  value. Defaults to `"MBIC"`. See the penalty-semantics section of
  [`cpt_penalty`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
  for how each engine interprets it; methods that use thresholds,
  significance levels, or posteriors instead of penalties ignore this
  argument.

- ...:

  Additional arguments passed to the specific wrapper (see the wrapper's
  help page for engine-specific options). Where an argument is also
  derived from `change_in` (`not`'s `contrast`, `cpm`'s `cpm_type`,
  `kcp`'s `running_stat`, `sn`'s `parameter`, `fastcpd`'s `family`), a
  value supplied here takes precedence.

## Value

A `ggcpt` object.

## Examples

``` r
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
result <- cpt_detect(x, method = "pelt", change_in = "mean")
result
#> ggcpt (changepoint detection result)
#>   Method:          pelt 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         MBIC 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.467
ggplot2::autoplot(result)
```
