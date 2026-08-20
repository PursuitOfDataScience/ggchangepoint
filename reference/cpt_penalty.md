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

  Series length (at least 3 for the \\\log n\\-based penalties).
  Required for BIC, MBIC, AIC, Hannan-Quinn, sSIC.

- k:

  Number of parameters per changepoint (typically 2 for mean+variance, 1
  for mean-only). Defaults to 1. The `"MBIC"` penalty additionally reads
  `k` as the number of changepoints being placed, in its \\\log{n
  \choose k}\\ term.

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
  `"None"`) and pass them to the upstream changepoint package. A numeric
  penalty is translated to that package's `penalty = "Manual"` plus
  `pen.value`. The one exception is Segment Neighbourhood, for which
  changepoint does not implement MBIC: `cpt_detect(method = "segneigh")`
  and `cpt_wrapper(cp_method = "SegNeigh")` therefore fall back to
  `"SIC"` when the default penalty is left in place, so a segneigh
  result is not directly penalty-comparable with a PELT one. Pass
  `penalty` explicitly to pin it. For a change in *mean* these engines
  also read the penalty on the data's own scale rather than a
  standardised one; see the scale-sensitivity section of
  [`cpt_detect`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

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

- **`MBIC`** in `cpt_penalty()` is a BIC-type penalty that adds a
  combinatorial term for the number of ways `k` changepoints can be
  placed in `n` observations, \\0.5(k+1)\log n + \log{n \choose k}\\. It
  is deliberately stronger than `"BIC"`. It is *not* the modified BIC of
  Zhang and Siegmund (2007), whose penalty \\1.5 k \log n + 0.5 \sum_i
  \log(l_i / n)\\ depends on the segment lengths \\l_i\\ and so cannot
  be expressed by a function of `n` and `k` alone. Use the character
  `"MBIC"` with changepoint-based methods to get the engine's native
  MBIC.

## Examples

``` r
cpt_penalty("BIC", n = 100)
#> [1] 4.60517
cpt_penalty("AIC", n = 100)
#> [1] 2
cpt_penalty("Manual", value = 5)
#> [1] 5
```
