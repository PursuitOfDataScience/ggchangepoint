# Recommend a detection method

Turns the capability matrix and the package's measurements into an
answer. Given what the analyst knows about their problem (how many
dimensions, what kind of change, what the noise and the data look like,
how long the series is, how many changes they expect, whether they need
uncertainty or an online alarm), this returns the methods that fit, as
calls, with the power and false-alarm rate measured for each in that
situation, a reason and the caveats. It is a decision table, not a
model: everything it knows is in
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
and the measurement tables
([`cpt_noise_benchmark`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_noise_benchmark.md),
[`cpt_data_types`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_data_types.md),
[`cpt_runtimes`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_runtimes.md)),
and making that explicit and printable is the point.

## Usage

``` r
cpt_recommend(
  dimension = c("univariate", "multivariate"),
  change_in = "mean",
  noise = c("iid", "heavy", "autocorrelated", "heteroscedastic"),
  n = NULL,
  need_uncertainty = FALSE,
  online = FALSE,
  installed_only = FALSE,
  n_expected = NULL,
  data_type = c("continuous", "counts", "binary", "proportion"),
  family = NULL,
  fit = NULL,
  jump = NULL
)

# S3 method for class 'ggcpt_recommendation'
tidy(x, ...)

# S3 method for class 'ggcpt_recommendation'
print(x, top = 5, ...)

# S3 method for class 'ggcpt_recommendation'
autoplot(object, top = 15, ...)
```

## Arguments

- dimension:

  `"univariate"` (default) or `"multivariate"`.

- change_in:

  What kind of change is expected: any value accepted by
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md).
  Defaults to `"mean"`.

- noise:

  Noise structure: `"iid"` (default), `"heavy"` (heavy-tailed),
  `"autocorrelated"`, or `"heteroscedastic"`.

- n:

  Series length. Methods the runtime table saw fail to finish at that
  length are left out (and listed), and the slow ones carry a caveat.
  Optional.

- need_uncertainty:

  Does the answer have to come with a confidence interval or
  significance region? Defaults to `FALSE`.

- online:

  Is detection sequential (alarms as data arrive) rather than
  retrospective? Defaults to `FALSE`.

- installed_only:

  Restrict to engines that are installed. Defaults to `FALSE`, so the
  recommendation names the right method even when it needs an install.

- n_expected:

  How many changes you expect, if you know. The single-change designs
  (`amoc`, `pettitt`, `buishand`, `snht`) cannot find a second one, so
  they are ranked down unless this is `1`.

- data_type:

  What the values are: `"continuous"` (default), `"counts"`, `"binary"`
  or `"proportion"`. Measured, a Gaussian cost raises about 165 times
  the false positives on binary data that it raises on Gaussian data, so
  engines are scored on their measured false positives for this data
  type, and those that can fit a cost for it (`family = "poisson"` or
  `"binomial"`) are preferred and recommended with that argument.

- family:

  Optional distribution family the method must fit (see
  [`cpt_families()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_families.md)).

- fit:

  Optional `ggcpt` fit already made. Its dimension, length and change
  type fill in the arguments not given; its values set the data type;
  and when its residuals are autocorrelated (see
  [`cpt_assumptions()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_assumptions.md))
  the noise is taken as `"autocorrelated"` unless `noise` was given.

- jump:

  Optional expected change size, in noise standard deviations. With `n`,
  the top three candidates' power to find it is simulated
  ([`cpt_power()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_power.md)
  with 50 replicates), and the printout says so when no method is likely
  to.

- x:

  A `ggcpt_recommendation` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- ...:

  Ignored.

- top:

  How many candidates to print. Defaults to `5`.

- object:

  A `ggcpt_recommendation` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Value

A `ggcpt_recommendation` tibble ordered by suitability, with columns
`method`, `engine`, `installed`, `score`, `tie` (candidates with the
same score share a number: the recommender cannot tell them apart on
what was supplied), `call` (the
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
call to run, including the noise-model or family argument the
recommendation depends on), `hits` and `false_positives` (measured in
the stated regime: mean real changes found out of two, and mean spurious
ones), `power` (when `jump` is given), `why` and `caveat`.
[`print()`](https://rdrr.io/r/base/print.html) reads as advice and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
shows the scores and the ties.

## How the score is built

Where the method was measured in the stated noise regime, the score
starts from \\3 - E\\, where \\E\\ is the expected number of errors per
series in that benchmark (real changes missed plus false alarms), taking
the method's best setting of its noise-model argument (which is then the
one in `call`). A method not measured there starts at 1. Then: minus 3
for a single-change design unless `n_expected = 1`; minus the log of one
plus its measured false positives on the stated data type, and plus 1.5
if it fits that data type's family; plus 1 for a location interval and
0.5 for a posterior when uncertainty is needed; at \\n \ge 5000\\, plus
0.5 for a fast engine and minus 1 for a slow one; and small tie-breakers
for the general-purpose methods (0.25) and for an installed engine
(0.1). Sort by `false_positives` instead if a false alarm costs you more
than a miss.

## See also

[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).

## Examples

``` r
cpt_recommend(noise = "autocorrelated")
#> Recommended methods for: univariate series, change in mean, autocorrelated noise
#> 
#> 1. cpt_detect(x, method = "kcp")
#>    measured: 1.9 of 2 changes found, 0.3 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise
#> 2. cpt_detect(x, method = "binseg")
#>    measured: 1.7 of 2 changes found, 1.3 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise
#>    caveat: 1.3 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 3. cpt_detect(x, method = "bfast")
#>    measured: 1.6 of 2 changes found, 1 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise
#>    caveat: 1 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 4. cpt_detect(x, method = "fpop")
#>    measured: 1.7 of 2 changes found, 1.4 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise
#>    caveat: 1.4 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 5. cpt_detect(x, method = "pelt")
#>    measured: 1.7 of 2 changes found, 1.5 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise
#>    caveat: 1.5 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 
#> (31 further candidate(s); the full table is the return value.)
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_robustness() and cpt_assumptions().
cpt_recommend(data_type = "counts", n_expected = 2)
#> Recommended methods for: univariate series, change in mean, iid noise, counts data, 2 change(s) expected
#> 
#> 1. cpt_detect(x, method = "pelt", family = "poisson")
#>    measured: 2 of 2 changes found, 0 spurious per series
#>    why: handles change_in = "mean"; fits a poisson cost
#> 2. cpt_detect(x, method = "binseg", family = "poisson")
#>    measured: 2 of 2 changes found, 0.1 spurious per series
#>    why: handles change_in = "mean"; fits a poisson cost
#> 3. cpt_detect(x, method = "bocpd", family = "poisson")
#>    measured: 2 of 2 changes found, 0 spurious per series
#>    why: handles change_in = "mean"; fits a poisson cost
#> 4. cpt_detect(x, method = "fastcpd", family = "poisson")
#>    measured: 2 of 2 changes found, 0 spurious per series
#>    why: handles change_in = "mean"; fits a poisson cost
#> 5. cpt_detect(x, method = "smuce", family = "hsmuce", family = "poisson")
#>    measured: 2 of 2 changes found, 0 spurious per series
#>    why: handles change_in = "mean"; its best setting under iid noise is family = "hsmuce"; fits a poisson cost
#> 
#> (31 further candidate(s); the full table is the return value.)
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_robustness() and cpt_assumptions().
cpt_recommend(dimension = "multivariate", change_in = "covariance")
#> Recommended methods for: multivariate series, change in covariance, iid noise
#> 
#> 1. cpt_detect(x, method = "fcov")
#>    why: handles change_in = "covariance"
#> 2. cpt_detect(x, method = "hdcov")
#>    why: handles change_in = "covariance"
#> 3. cpt_detect(x, method = "kwc")
#>    why: handles change_in = "covariance"
#> 
#> 3 candidates tie for first on the information supplied: fcov, hdcov, kwc. What would separate them: `n_expected`, `n`, `data_type`, `need_uncertainty`, or a `fit` to read the residuals of.
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_robustness() and cpt_assumptions().
```
