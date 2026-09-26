# Invariances of every engine's answer

Five replicates of a 200-point series with changes at 70 and 140 (a
3-coordinate version for the multivariate engines), detected as given
and transformed. A correct mean-change detector returns the same
changepoints on `10 * x`, `0.1 * x` and `x + 100`, and `n - cp` on the
reversed series.

## Usage

``` r
cpt_invariances
```

## Format

A tibble with one row per engine:

- method:

  the method.

- reps:

  replicates that ran.

- k:

  median changepoints on the series as given.

- scale_invariant:

  the same answer at `x`, `10 * x` and `0.1 * x` in at least four of
  five replicates. `FALSE` for the Gaussian-cost engines that assume
  unit noise, which is why
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  warns when one is handed noise far from unit scale.

- shift_invariant:

  the same answer on `x + 100`.

- reversal_rate:

  share of replicates whose reversed series gives the mirrored answer.

- sequential:

  the reversal test fails in most replicates: the answer depends on the
  direction of time.

- concat_k:

  median changepoints on `c(x, x)` (four expected, one for a
  single-change design).

- dup_k:

  median changepoints when every observation is repeated twice, which
  makes the noise strongly autocorrelated: a measure of how badly the
  engine fails when its independence assumption does.

## Source

`data-raw/measure_engines.R` (part `invariance`).

## See also

Other measurement tables:
[`cpt_calibration`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_calibration.md),
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_null_sizes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_null_sizes.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)

## Examples

``` r
subset(cpt_invariances, !scale_invariant)$method
#> [1] "amoc"     "binseg"   "envcpt"   "fpop"     "geomcp"   "pelt"     "segneigh"
#> [8] "var"     
```
