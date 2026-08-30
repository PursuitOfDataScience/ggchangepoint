# Generate simulated changepoint data

Creates a synthetic time series with known changepoints for testing and
benchmarking.

## Usage

``` r
cpt_simulate(
  n,
  changepoints = integer(),
  change_in = c("mean", "var", "meanvar", "slope"),
  params = NULL,
  noise = c("gauss", "t", "ar1", "rw"),
  sd = 1,
  df = 3,
  rho = 0,
  seasonality = NULL,
  sd_trend = NULL,
  seed = NULL
)

rcpt(...)
```

## Arguments

- n:

  Length of the series.

- changepoints:

  Integer vector of changepoint locations (last index of each segment
  before the change).

- change_in:

  What changes: `"mean"`, `"var"`, `"meanvar"`, or `"slope"`.

- params:

  A list of parameters per segment. For `mean` changes, a vector of
  segment means. For `var` changes, a vector of segment sds. For
  `meanvar`, a list of lists with `mean` and `sd` per segment. For
  `slope`, a list with `intercept` and `slope` per segment. When `NULL`,
  every segment gets the same neutral parameters, so the series has no
  actual change. Supplying fewer entries than there are segments
  recycles the last one and warns, because the trailing `changepoints`
  would then be recorded as ground truth without a change behind them.

- noise:

  Noise type: `"gauss"` (Gaussian), `"t"` (Student-t), `"ar1"` (AR(1)),
  or `"rw"` (random walk).

- sd:

  Noise standard deviation, non-negative (for Gaussian and t; t-noise is
  rescaled so its standard deviation is exactly `sd`). Defaults to 1.

- df:

  Degrees of freedom for t-noise; must exceed 2 so the variance exists.
  Defaults to 3.

- rho:

  AR(1) autocorrelation parameter, strictly between -1 and 1 for
  stationarity. Defaults to 0. Used only when `noise = "ar1"`.

- seasonality:

  Optional seasonal component added to the signal, as a list with
  `period` and `amplitude` (and optionally `phase`, in radians, and
  `shape`, either `"sine"` — the default — or `"sawtooth"`). A seasonal
  series is where the difference between a real level shift and a phase
  artefact starts to matter, and it is what
  [`bfast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bfast_wrapper.md)
  is built for; a detector that has never been shown one is untested
  against the case its users have.

- sd_trend:

  Optional smoothly varying noise scale: a length-2 numeric giving the
  multiplier on `sd` at the first and last observation, interpolated
  log-linearly in between. Distinct from `change_in = "var"`, which is
  piecewise constant — this is the *gradual* heteroscedasticity that
  makes constant-variance detectors shatter, and the condition HSMUCE,
  NSP-self-normalised and fastcpd's variance families exist to handle.

- seed:

  Optional seed for reproducibility.

- ...:

  Passed to `cpt_simulate`.

## Value

A tibble with columns `index`, `value`, and `seg_id`. The true
changepoints are stored in the `true_changepoints` attribute.

## See also

Other test signals:
[`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md),
[`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md),
[`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md),
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md)

## Examples

``` r
dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
                    params = c(0, 10), seed = 2022)
attr(dat, "true_changepoints")
#> [1] 100

# a seasonal series with a level shift, and one with drifting noise
seasonal <- cpt_simulate(240, changepoints = 120, params = c(0, 3),
                         seasonality = list(period = 12, amplitude = 2),
                         seed = 1)
drifting <- cpt_simulate(240, changepoints = 120, params = c(0, 3),
                         sd_trend = c(0.5, 3), seed = 1)
```
