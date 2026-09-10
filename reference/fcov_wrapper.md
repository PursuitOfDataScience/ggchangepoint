# Functional covariance changepoints

Wraps
[`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html)
for changes in the covariance operator, eigenstructure or trace of a
functional time series — the changes that leave the mean curve
untouched.

## Usage

``` r
fcov_wrapper(
  x,
  target = c("covariance", "trace", "eigenjoint", "eigensingle"),
  statistic = c("Tn", "Mn"),
  critical = c("simulation", "resample", "welch"),
  type = c("segmentation", "single"),
  alpha = 0.05,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with one row per time point and one
  column per grid location (the curve's resolution).

- target:

  What to test: `"covariance"` (default), `"trace"`, `"eigenjoint"` or
  `"eigensingle"`. This is also by far the biggest lever on run time —
  see the timing section below, and note that the four answer different
  questions, so a cheaper one is a different test rather than a faster
  route to the same answer.

- statistic:

  Test statistic: `"Tn"` (integrated, the default) or `"Mn"` (maximum).

- critical:

  How critical values are obtained: `"simulation"` (default),
  `"resample"` or `"welch"`.

- type:

  `"segmentation"` (default, multiple changes) or `"single"` (one
  change).

- alpha:

  Significance level. Defaults to `0.05`.

- ...:

  Additional arguments passed to
  [`fChange::fchange()`](https://jrvanderdoes.github.io/fChange/reference/fchange.html).

## Value

A `ggcpt` object with `change_in = "covariance"`. Multivariate input is
reduced to **one series per observation by taking the cross-sectional
mean** of the columns, and that is the series stored on the result:
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws it, [`tidy()`](https://generics.r-lib.org/reference/tidy.html)'s
`cp_value` reads it, and `$segments$param_estimate` and
[`augment()`](https://generics.r-lib.org/reference/augment.html)'s
`.fitted`/`.resid` are computed from it. It is not any one column of the
input. The full input is kept in `$data_wide` for
`autoplot(type = "coordinates")`.

For a covariance change this matters when reading the plot: a change in
the covariance structure need not move the cross-sectional mean at all,
so [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
can legitimately show changepoint rules on a series with no visible
change in it. That is the detector working, not misfiring — use
`autoplot(type = "coordinates")` to see the columns the change is in.

## How long this takes

**Minutes, not seconds, on a series of a hundred points** – by a wide
margin the most expensive engine in the package, and slow enough that a
first call looks like a hung session. Timed on one Linux x86-64 machine,
against
[`fmean_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fmean_wrapper.md)
on the identical input so the comparison is the same package and the
same data:

|                        |           |           |
|------------------------|-----------|-----------|
| **input**              | **fmean** | **fcov**  |
| \\n = 60\\, \\p = 5\\  | 4.5 s     | **316 s** |
| \\n = 120\\, \\p = 5\\ | 2.9 s     | **598 s** |

The cost is roughly linear in the number of time points and it is in the
engine's own estimation rather than in this wrapper. It is, however,
dominated by `target`, which the rest of this section used to deny —
measured at \\n = 60\\, \\p = 6\\, `M = 50` on one Linux x86-64 machine:

|                          |           |                        |
|--------------------------|-----------|------------------------|
| **target**               | **time**  | **changepoints found** |
| `"covariance"` (default) | **477 s** | none                   |
| `"eigenjoint"`           | 21.7 s    | none                   |
| `"eigensingle"`          | 21.7 s    | none                   |
| `"trace"`                | **2.1 s** | 16, 30, 38             |

So the default is some two hundred times the cost of `"trace"`, and the
example below uses `"trace"` for that reason. Read that as a choice of
test and not as a free speedup: the trace is a scalar summary of the
covariance operator, so it is a weaker instrument that happens to be
cheap, and the row above is one series rather than a comparison of
power. If a covariance change matters and the full operator test is the
one you want, budget for it.

Two practical consequences either way: size the call before starting it,
and do not put the default in a loop – a twelve-replicate study at \\n =
120\\ is two hours. Another machine will give different absolute
numbers; the ratios are the part to plan around. The “Benchmarks”
article compares the engines that do scale.

## References

Aue A, Rice G, Sönmez O (2020). “Structural break analysis for spectrum
and trace of covariance operators.” *Environmetrics*, **31**(1), e2617.

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
[`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md),
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
# See the note in ?fmean_wrapper on why this is 10 curves and M = 50.
X <- matrix(rnorm(60 * 10), nrow = 60)
X[31:60, ] <- X[31:60, ] * 3
fcov_wrapper(X, target = "trace", M = 50)
#> ggcpt (changepoint detection result)
#>   Method:             fcov
#>   Change in:          covariance
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            alpha = 0.05
#>   Series length:      60
#> 
#> Changepoints:
#> # A tibble: 1 × 3
#>      cp cp_value p_value
#>   <int>    <dbl>   <dbl>
#> 1    30    0.439       0
# }
```
