# Kernel changepoint wrapper (KCP on running statistics)

Wraps [`kcpRS::kcpRS()`](https://rdrr.io/pkg/kcpRS/man/kcpRS.html)
(Cabrieto et al., 2018; the KCP framework of Arlot, Celisse and
Harchaoui, 2019). The data are mapped to a running statistic (mean,
variance, autocorrelation, or correlation) computed on a sliding window,
and a Gaussian-kernel change point analysis with a permutation
significance test is run on the statistic. Detecting changes in running
correlations or variances captures higher-order changes that mean-based
methods miss. Multivariate input (matrix or data frame) is supported.

## Usage

``` r
kcp_wrapper(
  x,
  running_stat = c("mean", "var", "autocorr", "corr"),
  wsize = 25,
  nperm = 1000,
  kmax = 10,
  alpha = 0.05,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame (columns are variables).

- running_stat:

  Which running statistic to monitor: `"mean"`, `"var"`, `"autocorr"`,
  or `"corr"` (correlation requires at least two columns). Defaults to
  `"mean"`.

- wsize:

  Sliding window size for the running statistic. Defaults to `25`.

- nperm:

  Number of permutations for the significance test, at least 2. Defaults
  to `1000`. Fewer than two leaves the engine with no permutation
  distribution: it reports no changepoints at all for 0, and fails
  inside its own code for 1.

- kmax:

  Maximum number of changepoints considered. Defaults to `10`.

- alpha:

  Significance level of the permutation test. Defaults to `0.05`.

- seed:

  Optional seed for reproducibility of the permutation test.

- ...:

  Additional arguments passed to
  [`kcpRS::kcpRS()`](https://rdrr.io/pkg/kcpRS/man/kcpRS.html).

## Value

A `ggcpt` object. Reported locations refer to the centre of the sliding
window in which the change occurs. The series must be at least `wsize`
long to form one window. Constant coordinates make every running
statistic `NA`, so they are dropped (with a warning) before detection
and an all-constant input returns an empty result.

## References

Arlot S, Celisse A, Harchaoui Z (2019). “A kernel multiple change-point
algorithm via model selection.” *Journal of Machine Learning Research*,
**20**(162), 1–56.

Cabrieto J, Adolf J, Tuerlinckx F, Kuppens P, Ceulemans E (2018).
“Detecting long-lived autodependency changes in a multivariate system
via change point detection and regime switching models.” *Scientific
Reports*, **8**, 15637.

## Examples

``` r
res <- kcp_wrapper(c(rnorm(100), rnorm(100, 3)), nperm = 200, seed = 2026)
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100   -0.405
```
