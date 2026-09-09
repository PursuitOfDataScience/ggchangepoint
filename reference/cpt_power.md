# Detection power for a changepoint scenario

Simulates replicate series under a scenario and reports how often the
detector finds the change, how accurately it locates it, and how often
it reports changes that are not there. This is the calculation that
should precede an analysis, and the one applied papers are increasingly
asked for.

## Usage

``` r
cpt_power(
  n,
  jump,
  sigma = 1,
  method = "pelt",
  location = 0.5,
  n_sim = 200,
  tolerance = 5,
  change_in = "mean",
  noise = "gauss",
  rho = 0,
  df = 3,
  seed = NULL,
  parallel = TRUE,
  ...
)

# S3 method for class 'ggcpt_power'
tidy(x, ...)

# S3 method for class 'ggcpt_power'
print(x, ...)

# S3 method for class 'ggcpt_power'
autoplot(object, ...)
```

## Arguments

- n:

  Series length.

- jump:

  Size of the change, in units of `sigma`. A vector runs one scenario
  per value.

- sigma:

  Noise standard deviation. Defaults to `1`.

- method:

  Detection method. Defaults to `"pelt"`.

- location:

  Changepoint position, as a fraction of `n` in \\(0, 1)\\ or an integer
  position. Defaults to `0.5`. A vector runs one scenario per value.

- n_sim:

  Replicates per scenario. Defaults to `200`.

- tolerance:

  A detection counts as finding the change when it falls within this
  many positions of it. Defaults to `5`.

- change_in:

  What changes. Defaults to `"mean"`.

- noise:

  Noise model, passed to
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md).

- rho:

  AR(1) parameter when `noise = "ar1"`.

- df:

  Degrees of freedom when `noise = "t"`.

- seed:

  Optional seed. The seed is scoped to this call: `.Random.seed` is
  saved and restored, so a seeded call inside a simulation loop does not
  pin the loop's own stream.

- parallel:

  Use
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  when future.apply is available? Defaults to `TRUE`. It has no effect
  unless a non-sequential plan is set, but when one is it changes where
  the replicates' random numbers come from – see the section below,
  which matters if the power figure is going into a paper.

- ...:

  Additional arguments passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).

- x:

  A `ggcpt_power` object.

- object:

  A `ggcpt_power` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_power` object: a tibble with one row per scenario — `n`,
`jump`, `sigma`, `location`, `power` (proportion of replicates detecting
the change within `tolerance`), `mc_se` (the Monte Carlo standard error
of that proportion), `mean_abs_error` (location error among detections),
`mean_abs_error` is `NaN` when no replicate detected a changepoint
within `tolerance` of the true one — there is no distance to average —
and `power` reads `0` in the same row. `false_positives` (mean number of
*extra* changepoints per replicate) and `n_sim` — with
[`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Reproducibility under a parallel plan

A seeded call is reproducible **for a given**
[`future::plan()`](https://future.futureverse.org/reference/plan.html),
and not across plans. Under a parallel plan the replicates' random
numbers come from future.apply's parallel-safe L'Ecuyer streams, derived
from `seed`; run sequentially they come from the calling stream that
`seed` set. Both are deterministic, and they are not the same numbers.
Measured on
`cpt_power(n = c(100, 200), jump = 0.5, n_sim = 8, seed = 11)`:
`power = 0.25, 0.125` sequentially and `0, 0.375` on two workers. The
scenario is named because the numbers depend on it and on the worker
count — what does not depend on either is that the two disagree.

So the guarantee is: same seed and same plan, same answer – every time,
whichever plan it is. If a power figure needs to be reproducible by
someone else, pin the execution as well as the seed: pass
`parallel = FALSE`, or state the plan alongside the seed. Raising
`n_sim` narrows the gap, because it is Monte Carlo error rather than
disagreement – both estimates are of the same quantity, and `mc_se` says
how precisely.

This is specific to `cpt_power()`, which is the one function here whose
parallel tasks consume random numbers. The other six that dispatch on
[`future::plan()`](https://future.futureverse.org/reference/plan.html) –
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md),
[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`cpt_influence()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_influence.md),
[`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md)
and
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
– farm out work that is deterministic given its input, and were measured
to return identical results under a sequential and a two-worker plan,
stochastic engines included.

## See also

[`cpt_min_detectable()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_min_detectable.md),
[`cpt_scenarios()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_scenarios.md),
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md).

## Examples

``` r
# \donttest{
pw <- cpt_power(n = 200, jump = c(0.5, 1, 2), n_sim = 30, seed = 1)
pw
#> ggcpt_power (method: pelt, change in mean, gauss noise, tolerance 5)
#>   3 scenario(s), 30 replicates each
#> 
#> # A tibble: 3 × 9
#>       n  jump sigma location power  mc_se mean_abs_error false_positives n_sim
#>   <int> <dbl> <dbl>    <int> <dbl>  <dbl>          <dbl>           <dbl> <int>
#> 1   200   0.5     1      100 0.1   0.0548          2.67            0.1      30
#> 2   200   1       1      100 0.833 0.0680          1.32            0.167    30
#> 3   200   2       1      100 1     0               0.467           0        30
#> 
#> Monte Carlo standard errors are in `mc_se`; a power of 0.80 from 30
#> replicates is only known to about +/- 0.14.
ggplot2::autoplot(pw)

# }
```
