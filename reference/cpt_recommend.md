# Recommend a detection method

Turns the capability matrix into an answer. Given what the analyst knows
about their problem — how many dimensions, what kind of change, what the
noise looks like, how long the series is, whether they need uncertainty
or an online alarm — this returns the shortlist of methods that actually
fit, with a reason for each and the reference to cite. It is a decision
table, not a model: everything it knows is in
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
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
  installed_only = FALSE
)

# S3 method for class 'ggcpt_recommendation'
tidy(x, ...)

# S3 method for class 'ggcpt_recommendation'
print(x, top = 5, ...)
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

  Series length, used to flag methods that are impractical at that size.
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

- x:

  A `ggcpt_recommendation` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- ...:

  Ignored.

- top:

  How many candidates to print. Defaults to `5`.

## Value

A tibble of candidate methods ordered by suitability, with columns
`method`, `engine`, `installed`, `score`, `why` and `caveat`, and a
[`print()`](https://rdrr.io/r/base/print.html) that reads as advice.

## See also

[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md),
[`cpt_consensus()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_consensus.md),
[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).

## Examples

``` r
cpt_recommend(noise = "autocorrelated")
#> Recommended methods for: univariate series, change in mean, autocorrelated noise
#> 
#> 1. decafs (DeCAFS)
#>    why: handles change_in = "mean"; built for autocorrelated noise
#> 2. envcpt (EnvCpt)
#>    why: handles change_in = "mean"; built for autocorrelated noise
#> 3. mcp (mcp)
#>    why: handles change_in = "mean"; built for autocorrelated noise
#> 4. nsp (nsp)
#>    why: handles change_in = "mean"; built for autocorrelated noise
#> 5. sn (SNSeg)
#>    why: handles change_in = "mean"; built for autocorrelated noise
#> 
#> (31 further candidate(s); the full table is the return value.)
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_consensus() and cpt_sensitivity().
cpt_recommend(dimension = "multivariate", change_in = "covariance")
#> Recommended methods for: multivariate series, change in covariance, iid noise
#> 
#> 1. fcov (fChange)
#>    why: handles change_in = "covariance"
#> 2. hdcov (changepoints)
#>    why: handles change_in = "covariance"
#> 3. kwc (KWCChangepoint)
#>    why: handles change_in = "covariance"
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_consensus() and cpt_sensitivity().
```
