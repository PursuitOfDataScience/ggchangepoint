# The smallest detectable change

Inverts
[`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md):
searches for the change size at which the detector reaches a target
power. The study-design counterpart of a power curve, and the number
that belongs in a pre-registration.

## Usage

``` r
cpt_min_detectable(
  n,
  sigma = 1,
  method = "pelt",
  power = 0.8,
  range = c(0.1, 5),
  n_sim = 100,
  tolerance = 5,
  change_in = "mean",
  location = 0.5,
  noise = "gauss",
  rho = 0,
  df = 3,
  tol = 0.05,
  max_iter = 12,
  seed = NULL,
  ...
)

# S3 method for class 'ggcpt_min_detectable'
print(x, ...)
```

## Arguments

- n:

  Series length.

- sigma:

  Noise standard deviation. Defaults to `1`.

- method:

  Detection method. Defaults to `"pelt"`.

- power:

  Target detection probability. Defaults to `0.8`.

- range:

  Search range for the change size, in standard deviations. Defaults to
  `c(0.1, 5)`.

- n_sim:

  Replicates per evaluation. Defaults to `100`; the answer is only as
  precise as this makes it, and the returned object records the Monte
  Carlo interval at the solution.

- tolerance:

  A detection counts as finding the change when it falls within this
  many positions of it. Defaults to `5`.

- change_in:

  What changes. Defaults to `"mean"`.

- location:

  Changepoint position, as a fraction of `n` in \\(0, 1)\\ or an integer
  position. Defaults to `0.5`. A vector runs one scenario per value.

- noise:

  Noise model, passed to
  [`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md).

- rho:

  AR(1) parameter when `noise = "ar1"`.

- df:

  Degrees of freedom when `noise = "t"`.

- tol:

  Bisection tolerance on the change size. Defaults to `0.05`.

- max_iter:

  Maximum bisection steps. Defaults to `12`.

- seed:

  Optional seed. The seed is scoped to this call: `.Random.seed` is
  saved and restored, so a seeded call inside a simulation loop does not
  pin the loop's own stream.

- ...:

  Ignored.

- x:

  A `ggcpt_min_detectable` object.

## Value

A list with `jump` (the smallest change reaching `power`),
`achieved_power`, `mc_se`, and the `trace` of evaluations, with a
[`print()`](https://rdrr.io/r/base/print.html) method.

## See also

[`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md).

## Examples

``` r
# \donttest{
# n_sim = 10, max_iter = 3: the search is n_sim detector fits per
# iteration, and n_sim = 30 with max_iter = 4 measured 5.4 s against
# CRAN's 5 s example budget. Raise both for a usable answer.
cpt_min_detectable(n = 200, n_sim = 10, max_iter = 3, seed = 1)
#> Smallest detectable change
#>   Target power:      0.8
#>   Change size:       1.32 standard deviations
#>   Achieved power:    1 (Monte Carlo SE 0)
#> 
#> # A tibble: 5 × 3
#>    jump power mc_se
#>   <dbl> <dbl> <dbl>
#> 1 0.1     0   0    
#> 2 0.712   0.4 0.155
#> 3 1.32    1   0    
#> 4 2.55    1   0    
#> 5 5       1   0    
# }
```
