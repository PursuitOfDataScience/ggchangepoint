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
  `slope`, a list with `intercept` and `slope` per segment.

- noise:

  Noise type: `"gauss"` (Gaussian), `"t"` (Student-t), `"ar1"` (AR(1)),
  or `"rw"` (random walk).

- sd:

  Noise standard deviation (for Gaussian and t). Defaults to 1.

- df:

  Degrees of freedom for t-noise. Defaults to 3.

- rho:

  AR(1) autocorrelation parameter. Defaults to 0.

- seed:

  Optional seed for reproducibility.

- ...:

  Passed to `cpt_simulate`.

## Value

A tibble with columns `index`, `value`, and `seg_id`. The true
changepoints are stored in the `true_changepoints` attribute.

## Examples

``` r
dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
                    params = c(0, 10), seed = 2022)
attr(dat, "true_changepoints")
#> [1] 100
```
