# Choosing a Method: a Decision Tree and a Cheatsheet

“Which of these fifty do I want?” is the first question every new user
has. This page answers it three ways: a tree to walk, a cheatsheet to
scan, and
[`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md),
which answers from the same measurements in code. Every list below is
computed from
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
when the page is built, so it cannot drift from the registry.

## Decide before you look

Pick the method from what you know about the problem, before you see
what each method says about your series. A method chosen because it
found a change carries no valid p-value, and that selection is the
easiest mistake to make with fifty methods behind one call.

## The tree

    What arrives?
    |
    +-- data still arriving, and you want an alarm ---> cpt_monitor()
    |                                                   (edetector, cpm, ocd)
    +-- a formula: did a regression's relationship change?
    |       ---> cpt_detect(y ~ x, data, method = "strucchange" | "segmented" | "fastcpd")
    |
    +-- several series that should change together ---> the multivariate methods
    |
    +-- one finished series. What changes?
        |
        +-- its level (the mean)
        |     is the noise independent?   yes --> see "A change in mean" below
        |                                 no  --> cpt_assumptions() says so; see
        |                                         "Autocorrelated noise"
        +-- its spread, or level and spread together --> change_in = "var" / "meanvar"
        +-- its slope, the level staying continuous   --> change_in = "slope"
        +-- its whole distribution                    --> change_in = "distribution"
        +-- counts, 0/1 outcomes, waiting times       --> family = "poisson" |
                                                          "binomial" | "exponential"

## A change in mean

37 methods detect a change in mean. Which of them depends on three
things.

**How long is the series?** The univariate ones measured to fit a
1,000,000-point series within five minutes (`cpt_runtimes`):

``` r

long <- subset(wired, max_n >= 1e6 & univariate &
                 method %in% supports("mean"))
long[order(long$method), c("method", "engine", "cost", "scale_invariant")]
#> # A tibble: 11 × 4
#>    method     engine      cost  scale_invariant
#>    <chr>      <chr>       <chr> <lgl>          
#>  1 amoc       changepoint fast  FALSE          
#>  2 binsegrcpp binsegRcpp  fast  TRUE           
#>  3 decafs     DeCAFS      fast  TRUE           
#>  4 fastcpd    fastcpd     fast  TRUE           
#>  5 fpop       fpop        fast  FALSE          
#>  6 idetect    IDetect     fast  TRUE           
#>  7 mosum      mosum       fast  TRUE           
#>  8 not        not         fast  TRUE           
#>  9 pelt       changepoint fast  FALSE          
#> 10 tguh       breakfast   fast  TRUE           
#> 11 wbs        wbs         fast  TRUE
```

**Do you need to say where it could be?** Methods returning a location
interval or a significance region: `smuce`, `hsmuce`, `strucchange`,
`segmented`, `nsp`, `mcp`, `bfast`, `taylor`. Methods returning a
posterior over locations: `bcp`, `bocpd`, `beast`, `mcp`.

**Is the noise on unit scale?** These were measured to give a different
answer on the same series in different units, so standardise first or
choose another: `pelt`, `binseg`, `segneigh`, `amoc`, `fpop`, `geomcp`,
`envcpt`, `var`.

## Autocorrelated noise

Measured on AR(1) noise with a lag-one correlation of 0.7, two real
changes and 300 observations, the calls with the fewest false positives
that still find the changes (`cpt_noise_benchmark`):

``` r

ar <- subset(cpt_noise_benchmark, regime == "ar1" & fp <= 1)
head(ar[order(-ar$hits, ar$fp), c("call", "hits", "fp")], 6)
#> # A tibble: 6 × 3
#>   call                                                       hits    fp
#>   <chr>                                                     <dbl> <dbl>
#> 1 "cpt_detect(x, method = \"kcp\")"                           1.9   0.3
#> 2 "cpt_detect(x, method = \"bfast\")"                         1.6   1  
#> 3 "cpt_detect(x, method = \"nsp\", variant = \"selfnorm\")"   1.2   0.8
#> 4 "cpt_detect(x, method = \"pettitt\")"                       0.8   0.2
#> 5 "cpt_detect(x, method = \"buishand\")"                      0.7   0.3
#> 6 "cpt_detect(x, method = \"amoc\")"                          0.6   0.4
```

A general-purpose detector on the same data, for comparison:

``` r

subset(cpt_noise_benchmark, regime == "ar1" & method == "pelt" &
         setting == "default")[, c("call", "hits", "fp")]
#> # A tibble: 1 × 3
#>   call                                hits    fp
#>   <chr>                              <dbl> <dbl>
#> 1 "cpt_detect(x, method = \"pelt\")"   1.7   1.5
```

## Counts, binary outcomes and waiting times

Methods that fit a Poisson cost: `pelt`, `binseg`, `segneigh`, `amoc`,
`smuce`, `bocpd`, `segmented`, `fastcpd`, `binsegrcpp`. A binomial cost:
`smuce`, `cpm`, `segmented`, `fastcpd`. An exponential cost: `pelt`,
`binseg`, `segneigh`, `amoc`, `cpm`, `fastcpd`.
[`cpt_families()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_families.md)
has the engine call each combination becomes.

## One change, or several?

Some designs return at most one changepoint, which is right for a
before-and-after question and wrong for everything else: `amoc`,
`segmented`, `mcp`, `pettitt`, `buishand`, `snht`. Two of them
(`segmented` and `mcp`) return exactly as many as they are asked for,
one by default, whether or not the data change: test the answer with
[`cpt_test()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_test.md)
before reporting it.

## The cheatsheet

| I need | Start with |
|:---|:---|
| A default for one series, one kind of change | `cpt_detect(x)` (`pelt`, on standardised data) |
| Intervals for the locations | `cpt_detect(x, method = "smuce")`, `"strucchange"`, `"nsp"` |
| A posterior probability of change at each time | `method = "bcp"`, `"beast"`, `"bocpd"` |
| Robustness to outliers | `cpt_detect(x, method = "binsegrcpp", family = "l1")` |
| A change in slope | `cpt_detect(x, method = "cpop", change_in = "slope")` |
| Counts | `cpt_detect(x, method = "pelt", family = "poisson")` |
| 0/1 outcomes | `cpt_detect(x, method = "smuce", family = "binomial")` |
| A regression break | `cpt_detect(y ~ x, data = d, method = "strucchange")` |
| Several series at once | `cpt_detect(X, method = "inspect")`, `"geomcp"`, `"ecp"` |
| A long series (10^6) | `method = "pelt"`, `"fpop"`, `"binsegrcpp"`, `"mosum"` |
| An alarm as data arrive | `cpt_monitor("edetector", baseline = b)` |
| A test at a date fixed in advance | `cpt_test_at(x, when = date)` |
| To know whether to believe the answer | `cpt_assumptions(fit)`, `cpt_robustness(fit)` |

## The same answer in code

[`cpt_recommend()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_recommend.md)
reads the same tables and scores every method on what you tell it; it
returns the call to run, the measured numbers behind the score, and a
reason:

``` r

cpt_recommend(noise = "autocorrelated", n = 5000, n_expected = 2)
#> Recommended methods for: univariate series, change in mean, autocorrelated noise, n = 5000, 2 change(s) expected
#> 
#> 1. cpt_detect(x, method = "binseg")
#>    measured: 1.7 of 2 changes found, 1.3 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise; fast at this length
#>    caveat: 1.3 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 2. cpt_detect(x, method = "fpop")
#>    measured: 1.7 of 2 changes found, 1.4 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise; fast at this length
#>    caveat: 1.4 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 3. cpt_detect(x, method = "pelt")
#>    measured: 1.7 of 2 changes found, 1.5 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise; fast at this length
#>    caveat: 1.5 spurious changepoints per series measured under autocorrelated noise; check the fit with cpt_assumptions()
#> 4. cpt_detect(x, method = "decafs")
#>    measured: 0.5 of 2 changes found, 0.3 spurious per series
#>    why: handles change_in = "mean"; measured under autocorrelated noise; fast at this length
#> 5. cpt_detect(x, method = "fastcpd", family = "ar")
#>    measured: 0.2 of 2 changes found, 0 spurious per series
#>    why: handles change_in = "mean"; its best setting under autocorrelated noise is family = "ar"; measured under autocorrelated noise; fast at this length
#> 
#> (31 further candidate(s); the full table is the return value.)
#> 
#> Cite the method you use with cpt_cite(). Cross-check the choice with
#> cpt_robustness() and cpt_assumptions().
```

Given a fit you already made, it fills in what the fit knows (length,
dimension, data type, and whether the residuals are autocorrelated):

``` r

set.seed(1)
fit <- suppressWarnings(cpt_detect(as.numeric(arima.sim(list(ar = 0.7), 400)),
                                   method = "pelt"))
head(cpt_recommend(fit = fit)[, c("method", "call", "why")], 3)
#> # A tibble: 3 × 3
#>   method call                                 why                               
#>   <chr>  <chr>                                <chr>                             
#> 1 kcp    "cpt_detect(x, method = \"kcp\")"    "handles change_in = \"mean\"; me…
#> 2 binseg "cpt_detect(x, method = \"binseg\")" "handles change_in = \"mean\"; me…
#> 3 bfast  "cpt_detect(x, method = \"bfast\")"  "handles change_in = \"mean\"; me…
```
