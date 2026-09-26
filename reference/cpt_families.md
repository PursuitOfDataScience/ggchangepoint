# Distribution families, and what each request runs

The `family` argument of
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
names the distribution a segment is modelled with, so count data,
waiting times and binary series can be segmented with a cost written for
them rather than a Gaussian one. `cpt_families()` is the table behind
it: every legal combination of `method`, `change_in` and `family`, and
the engine call each becomes.

## Usage

``` r
cpt_families(method = NULL)
```

## Arguments

- method:

  Optional method name(s) to restrict the table to.

## Value

A tibble with one row per legal combination: `method`, `change_in`,
`family` (`NA` for a distribution-free method, which takes no family),
`engine_call` (what is run) and `data` (what the family requires of the
series: `"counts"`, `"binary"`, `"positive"` or `NA`).

## The families

- `"gaussian"`:

  The default of every parametric method. On `"cpm"` it also switches
  from the nonparametric statistics to the Gaussian ones (Student,
  Bartlett).

- `"poisson"`:

  Counts: `change_in = "mean"` is a change in the rate. Needs
  non-negative whole numbers.

- `"binomial"`:

  Binary outcomes: a change in the success probability. Needs 0/1 data.

- `"exponential"`:

  Waiting times: a change in the rate (the hazard), so
  `change_in = "mean"`. Needs positive data.

- `"gamma"`:

  Positive data with a known shape (pass `shape`, default 1): a change
  in the scale.

- `"laplace"`:

  Heavy tails: a change in the median and the scale
  (`change_in = "meanvar"`).

- `"l1"`:

  The absolute-loss cost: a change in the median, robust to outliers
  (`change_in = "mean"`).

A single-parameter family has one thing to change, so Poisson, binomial,
exponential and gamma requests use `change_in = "mean"`.

## Why an argument and not a model grammar

A grammar that marks the changing parameter (`poisson(rate = NA)`,
`normal(mean = NA, sd = 1)`) would unify `change_in`, `family` and the
formula interface in one expression, and measured against the fifty
methods it fits 42 of them cleanly. It is deferred to 1.0, where
`change_in` can be replaced rather than joined by a second idiom, and
this table is the first half of it:
`change_in = "mean", family = "poisson"` already means
`poisson(rate = NA)` internally.

## See also

[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md),
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
(its `families` column),
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
to generate data from each family.

## Examples

``` r
cpt_families("pelt")
#> # A tibble: 6 × 5
#>   method change_in family      engine_call                                 data 
#>   <chr>  <chr>     <chr>       <chr>                                       <chr>
#> 1 pelt   mean      gaussian    "changepoint::cpt.mean(method = \"PELT\")"  NA   
#> 2 pelt   mean      poisson     "changepoint::cpt.meanvar(test.stat = \"Po… coun…
#> 3 pelt   mean      exponential "changepoint::cpt.meanvar(test.stat = \"Ex… posi…
#> 4 pelt   mean      gamma       "changepoint::cpt.meanvar(test.stat = \"Ga… posi…
#> 5 pelt   meanvar   gaussian    "changepoint::cpt.meanvar(method = \"PELT\… NA   
#> 6 pelt   var       gaussian    "changepoint::cpt.var(method = \"PELT\")"   NA   
# every method that can fit a Poisson rate
unique(subset(cpt_families(), family == "poisson")$method)
#> [1] "pelt"       "binseg"     "segneigh"   "amoc"       "smuce"     
#> [6] "bocpd"      "segmented"  "fastcpd"    "binsegrcpp"

set.seed(1)
counts <- c(rpois(100, 3), rpois(100, 9))
cpt_detect(counts, method = "pelt", family = "poisson")
#> ggcpt (changepoint detection result)
#>   Method:             pelt
#>   Change in:          mean
#>   Changepoints found: 1
#>   CP convention:      left
#>   Penalty:            MBIC
#>   Series length:      200
#>   Family:             poisson
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100        3
```
