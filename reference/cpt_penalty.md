# Construct changepoint penalties

Helper to construct standard penalty values for use with changepoint
detection methods. Returns a numeric penalty value.

## Usage

``` r
cpt_penalty(type, n = NULL, k = 1, value = NULL, alpha = 1.01)
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

- alpha:

  Exponent of the strengthened SIC (`"sSIC"`) penalty \\k (\log
  n)^\alpha\\; must exceed 1. Defaults to `1.01` (Fryzlewicz, 2014).

## Value

A numeric penalty value.

## Penalty semantics across engines

The same penalty name may be interpreted differently by different
engines:

- **changepoint-based methods** (PELT, BinSeg, SegNeigh, AMOC): accept
  character penalties (`"MBIC"`, `"BIC"`, `"AIC"`, `"Hannan-Quinn"`,
  `"None"`) and pass them to the upstream changepoint package. These
  methods do *not* accept raw numeric penalty values.

- **Functional-pruning methods** (`fpop`, `cpop`, `decafs`): accept
  numeric penalties only. When a character penalty is supplied via
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
  it is resolved to a numeric value using `cpt_penalty()` before
  dispatch.

- **Search-based methods** (WBS, WBS2, NOT, MOSUM, IDetect, TGUH): use
  internal model-selection criteria (e.g., sSIC, threshold) and
  generally *ignore* the `penalty` argument. Specify thresholds via the
  wrapper's own arguments.

- **Inference/Bayesian methods** (`smuce`, `bcp`, `bocpd`, `beast`,
  `cpm`, `sn`): are tuned by a significance level, posterior-probability
  threshold, hazard, or average run length rather than a penalty; see
  each wrapper.

- **`MBIC`** in `cpt_penalty()` uses the Zhang-Siegmund (2007) formula
  \\0.5(k+1)\log n + \log{n \choose k}\\, which differs from the
  changepoint package's MBIC. Use the character `"MBIC"` with
  changepoint-based methods to get the engine's native MBIC.

## Examples

``` r
cpt_penalty("BIC", n = 100)
#> [1] 4.60517
cpt_penalty("AIC", n = 100)
#> [1] 2
cpt_penalty("Manual", value = 5)
#> [1] 5
```
