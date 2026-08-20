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
  argument, and `"segneigh"` falls back to `"SIC"` because changepoint
  does not implement MBIC for Segment Neighbourhood. Note also that the
  default `"MBIC"` is resolved to a *numeric* value for the
  numeric-penalty engines (`"fpop"`, `"cpop"`, `"decafs"`), and that
  value is stronger than those wrappers' own `2 * log(n)` default — 19.9
  against 11.8 at \\n = 360\\ — so `cpt_detect(x, method = "decafs")`
  can report fewer changepoints than `decafs_wrapper(x)` on the same
  series. Pass `penalty` explicitly to make the two entry points agree.

- ...:

  Additional arguments passed to the specific wrapper (see the wrapper's
  help page for engine-specific options). Where an argument is also
  derived from `change_in` (`not`'s `contrast`, `cpm`'s `cpm_type`,
  `kcp`'s `running_stat`, `sn`'s `parameter`, `fastcpd`'s `family`), a
  value supplied here takes precedence. Check the spelling against the
  wrapper's help page: several engines end their own signature in `...`
  (wbs, not, Rbeast, strucchange, segmented, fastcpd), so for those a
  misspelt argument name is silently discarded upstream and the engine
  quietly uses its default rather than reporting the typo.

## Value

A `ggcpt` object.

## Scale sensitivity of the penalised change-in-mean engines

`"pelt"`, `"binseg"`, `"segneigh"` and `"fpop"` compare a penalty
against a *raw* segment cost when `change_in = "mean"`: changepoint's
Normal cost assumes a noise standard deviation of 1, and fpop's `lambda`
is an absolute penalty on the residual sum of squares. Neither rescales
the data, so on a series whose noise is much wider than 1 the penalty is
effectively negligible and the segmentation shatters. On one true
changepoint with a jump of five standard deviations, `"pelt"` returns 1
changepoint at \\\sigma = 1\\, 29 at \\\sigma = 3\\ and 138 at \\\sigma
= 10\\. Three ways to avoid it, in order of convenience:

- standardise the series first
  (`cpt_detect(scale(x)[, 1], method = "pelt")`);

- pass a penalty on the data's own scale, for example
  `penalty = 2 * log(length(x)) * stats::var(diff(x)) / 2`;

- use `change_in = "meanvar"`, which estimates a variance per segment
  and is unaffected.

The other engines are unaffected: SMUCE, WBS, WBS2, NOT, MOSUM,
Isolate-Detect, TGUH, CPOP, DeCAFS and the Bayesian, nonparametric and
multivariate methods all estimate or cancel the noise scale internally,
and return the same segmentation whatever the units.

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
