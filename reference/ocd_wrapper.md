# ocd wrapper — online high-dimensional changepoint detection

Wraps the `ocd` package (Chen, Wang and Samworth, 2022): online
multiscale detection of a mean change in a high-dimensional stream, with
worst-case detection-delay guarantees and per-observation cost
independent of history. The detector assumes standardised data with
known pre-change mean; this wrapper estimates the baseline mean and
standard deviation from an initial training window, then monitors the
remainder of the series, resetting after each declaration so multiple
changes can be found.

## Usage

``` r
ocd_wrapper(
  x,
  train = NULL,
  thresh = "MC",
  patience = 5000,
  beta = 1,
  mc_reps = 100,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and at
  least two columns. The `ocd` detector is inherently high-dimensional
  and cannot be constructed for a single coordinate, so univariate input
  is rejected; use a univariate engine (see
  [`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md))
  for one series.

- train:

  Number of initial observations used to estimate the baseline mean/sd
  (not monitored). Defaults to `max(20, floor(0.2 * n))`, capped at
  `n/2`.

- thresh:

  Threshold specification passed to
  [`ocd::ChangepointDetector()`](https://rdrr.io/pkg/ocd/man/ChangepointDetector.html);
  `"MC"` (default) calibrates by Monte Carlo, which is what makes this
  the slowest wrapper — see the timing note below. Supplying the three
  thresholds directly, as a named numeric vector
  `c(diag =, off_d =, off_s =)`, skips calibration altogether.

- patience:

  Target average run length to false alarm. Defaults to `5000`.

- beta:

  Assumed lower bound on the squared Euclidean norm of the mean change.
  Defaults to `1`.

- mc_reps:

  Monte Carlo repetitions for threshold calibration. Defaults to `100`.
  The cost is linear in this and grows with the number of coordinates;
  see the timing note below.

- ...:

  Additional arguments passed to
  [`ocd::ChangepointDetector()`](https://rdrr.io/pkg/ocd/man/ChangepointDetector.html).

## Value

A `ggcpt` object. Because the detector is online, reported locations are
*declaration times* (the changepoint plus the detection delay). The
`declared_at` column holds the same values as `cp`, and deliberately:
ocd declares a change without also estimating where it began, so there
is no separate location for the second column to carry. Compare
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
whose engine supplies both, and whose `cp` is an estimated location with
`detection_time` strictly later.

## How long this takes

Nearly all of the run time is `ocd`'s Monte Carlo threshold calibration,
which happens before a single observation is read. It is linear in
`mc_reps` and grows with the number of coordinates. Timed on one Linux
x86-64 machine at `mc_reps = 5`, construction took about 10 s at \\p =
3\\, 22 s at \\p = 10\\ and 113 s at \\p = 50\\; raising `mc_reps`
scales it linearly, so at \\p = 3\\ it was 38 s at `mc_reps = 20` and
189 s at the default `mc_reps = 100`. The practical reading is that the
default costs *minutes* rather than seconds even for a handful of
coordinates, and better than half an hour at \\p = 50\\. Another machine
will give different absolute numbers; the linearity in `mc_reps` is the
part to plan around. Monitoring the observations afterwards is cheap by
comparison — 0.37 s for a thousand of them at \\p = 3\\. Lower `mc_reps`
while exploring, or pass `thresh` directly to skip calibration entirely,
which brings the same fit down to a tenth of a second.

## References

Chen Y, Wang T, Samworth RJ (2022). “High-dimensional, multiscale online
changepoint detection.” *Journal of the Royal Statistical Society:
Series B*, **84**(1), 234–266.

## See also

Other changepoint engines:
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md),
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md),
[`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md),
[`binsegrcpp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/binsegrcpp_wrapper.md),
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md),
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md),
[`cpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpop_wrapper.md),
[`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md),
[`decafs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/decafs_wrapper.md),
[`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md),
[`envcpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/envcpt_wrapper.md),
[`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md),
[`fabisearch_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fabisearch_wrapper.md),
[`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md),
[`fcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fcov_wrapper.md),
[`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md),
[`fpop_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fpop_wrapper.md),
[`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md),
[`hdcov_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdcov_wrapper.md),
[`hdreg_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/hdreg_wrapper.md),
[`idetect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/idetect_wrapper.md),
[`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md),
[`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md),
[`kwc_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kwc_wrapper.md),
[`mcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mcp_wrapper.md),
[`mosum_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/mosum_wrapper.md),
[`network_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/network_wrapper.md),
[`not_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/not_wrapper.md),
[`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md),
[`nsp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/nsp_wrapper.md),
[`pilliat_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/pilliat_wrapper.md),
[`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md),
[`smuce_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/smuce_wrapper.md),
[`sn_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/sn_wrapper.md),
[`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md),
[`taylor_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/taylor_wrapper.md),
[`tguh_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/tguh_wrapper.md),
[`trend_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/trend_wrapper.md),
[`var_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/var_wrapper.md),
[`wbs2_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs2_wrapper.md),
[`wbs_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbs_wrapper.md),
[`wbsts_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/wbsts_wrapper.md)

## Examples

``` r
# \donttest{
set.seed(2026)
X <- rbind(matrix(rnorm(60 * 3), 60), matrix(rnorm(40 * 3, 3), 40))
res <- ocd_wrapper(X, mc_reps = 5)
res$changepoints
#> # A tibble: 1 × 3
#>      cp cp_value declared_at
#>   <int>    <dbl>       <int>
#> 1    62     4.31          62
# }
```
