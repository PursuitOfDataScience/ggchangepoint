# The ggchangepoint Feature Tour: A Complete Map of the Package Surface

## Abstract

**ggchangepoint** provides a unified, tidy, `ggplot2`-native interface
to changepoint detection in R: one dispatcher
([`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md))
covering 31 methods across five methodological families, one result
class (`ggcpt`) with a stable tidy contract, and one visualization entry
point
([`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html))
that can draw everything a method reports — including confidence
intervals and posterior probabilities (Wickham 2016; Robinson 2017).
This vignette is the *feature tour*: it visits **every exported
function** in the package exactly where it belongs in the workflow, so
that a reader can map the full surface in one sitting. The companion
vignettes develop the methodology in depth
([`vignette("introduction")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/introduction.md))
and treat method comparison and evaluation
([`vignette("comparison")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/comparison.md)).

``` r

set.seed(2026)
x <- c(rnorm(100, 0, 1), rnorm(100, 6, 1))    # one mean shift at t = 100
x3 <- c(rnorm(100), rnorm(100, 4), rnorm(100, 1))  # shifts at 100, 200
```

## The result object and its methods

Every detector returns a `ggcpt` object: a list carrying the tidy
`changepoints` tibble (`cp` = last index of the left segment, `cp_value`
= series value there, plus any method-specific columns), a `segments`
table, the `data`, and metadata (method, `change_in`, penalty,
convention, runtime).
[`new_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/new_ggcpt.md)
is the low-level constructor and
[`is_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/is_ggcpt.md)
the class test; most users never call either directly.

``` r

res <- cpt_detect(x, method = "pelt", change_in = "mean")
is_ggcpt(res)
#> [1] TRUE
print(res)
#> ggcpt (changepoint detection result)
#>   Method:          pelt 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         MBIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

manual <- new_ggcpt(
  changepoints = tibble::tibble(cp = 100L, cp_value = x[100]),
  data = tibble::tibble(index = seq_along(x), value = x),
  method = "manual"
)
is_ggcpt(manual)
#> [1] TRUE
```

The `broom` verbs give one row per changepoint
([`tidy()`](https://generics.r-lib.org/reference/tidy.html)), a one-row
model summary
([`glance()`](https://generics.r-lib.org/reference/glance.html)), and
the data augmented with segment ids, fitted levels, and residuals
([`augment()`](https://generics.r-lib.org/reference/augment.html)):

``` r

tidy(res)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
glance(res)
#> # A tibble: 1 × 9
#>       n n_changepoints method change_in penalty_type penalty_value cp_convention
#>   <int>          <int> <chr>  <chr>     <chr>                <dbl> <chr>        
#> 1   200              1 pelt   mean      MBIC                    NA left         
#> # ℹ 2 more variables: total_cost <dbl>, runtime <dbl>
head(augment(res))
#> # A tibble: 6 × 6
#>   index   value seg_id .fitted  .resid is_changepoint
#>   <int>   <dbl>  <int>   <dbl>   <dbl> <lgl>         
#> 1     1  0.521       1 -0.0980  0.619  FALSE         
#> 2     2 -1.08        1 -0.0980 -0.982  FALSE         
#> 3     3  0.139       1 -0.0980  0.237  FALSE         
#> 4     4 -0.0847      1 -0.0980  0.0133 FALSE         
#> 5     5 -0.667       1 -0.0980 -0.569  FALSE         
#> 6     6 -2.52        1 -0.0980 -2.42   FALSE
```

The remaining S3 surface: a human-readable digest, tibble/data-frame
coercion, a one-line format, and a
base-[`plot()`](https://rdrr.io/r/graphics/plot.default.html) fallback
that delegates to
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

``` r

summary(res)
#> ggcpt Summary
#>   Method:                   pelt 
#>   Change in:                mean 
#>   Changepoints found:       1 
#>   CP convention:            left 
#>   Series length:            200 
#>   Penalty:                  MBIC = NA 
#>   Runtime (seconds):        0.015 
#> 
#> Segments:
#> # A tibble: 2 × 5
#>   seg_id start   end     n param_estimate
#>    <int> <dbl> <int> <dbl>          <dbl>
#> 1      1     1   100   100        -0.0980
#> 2      2   101   200   100         6.12  
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
as_tibble(res)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
head(as.data.frame(res))
#>    cp  cp_value
#> 1 100 0.3694259
format(res)
#> [1] "ggcpt [pelt] 1 changepoint(s) on 200 observations"
```

``` r

plot(res)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/plot-fallback-1.png)

## Unified detection

[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
is the recommended entry point: pick a `method`, say what the change is
in (`change_in` = `"mean"`, `"var"`, `"meanvar"`, `"slope"`, or
`"distribution"`), and optionally set a `penalty`. Incompatible
`method`/`change_in` combinations error — they are never silently
substituted.

``` r

cpt_detect(x, method = "binseg", change_in = "mean")
#> ggcpt (changepoint detection result)
#>   Method:          binseg 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         MBIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
is the live capability table: every method the package knows, its engine
package, what it can detect, and whether the engine is installed.

``` r

cpt_methods()
#> # A tibble: 35 × 6
#>    method   change_in                   engine   status target_release installed
#>    <chr>    <chr>                       <chr>    <chr>  <chr>          <lgl>    
#>  1 pelt     mean, var, meanvar          changep… avail… NA             TRUE     
#>  2 binseg   mean, var, meanvar          changep… avail… NA             TRUE     
#>  3 segneigh mean, var, meanvar          changep… avail… NA             TRUE     
#>  4 amoc     mean, var, meanvar          changep… avail… NA             TRUE     
#>  5 np       distribution                changep… avail… NA             TRUE     
#>  6 ecp      distribution (multivariate) ecp      avail… NA             TRUE     
#>  7 fpop     mean                        fpop     avail… NA             TRUE     
#>  8 wbs      mean                        wbs      avail… NA             TRUE     
#>  9 wbs2     mean                        breakfa… avail… NA             TRUE     
#> 10 not      mean, var, slope            not      avail… NA             TRUE     
#> # ℹ 25 more rows
```

[`cpt_penalty()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_penalty.md)
constructs standard penalty values for the engines that take numeric
penalties; see its help for the per-engine penalty semantics.

``` r

cpt_penalty("BIC", n = 200)
#> [1] 5.298317
cpt_penalty("MBIC", n = 200)
#> [1] 10.59663
cpt_penalty("Manual", value = 10)
#> [1] 10
```

## Engine wrappers

Each engine also has a direct wrapper exposing its native arguments. All
return the same `ggcpt` object.

### The classical wave

[`cpt_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_wrapper.md)
and
[`ecp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ecp_wrapper.md)
are the original 0.1.0 interface to the *changepoint*/*changepoint.np*
(Killick et al. 2012; Killick and Eckley 2014; Haynes et al. 2017) and
*ecp* (Matteson and James 2014; James and Matteson 2014) engines; they
return bare tibbles for backward compatibility.

``` r

cpt_wrapper(x, change_in = "mean", cp_method = "PELT")
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
ecp_wrapper(x, algorithm = "divisive", seed = 1)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <dbl>    <dbl>
#> 1   101     7.22
```

The search and pruning engines added in 0.2.0 (Fryzlewicz 2014, 2022;
Baranowski et al. 2019; Eichinger and Kirch 2018; Anastasiou and
Fryzlewicz 2022; Maidstone et al. 2017), one call each:

``` r

fpop_wrapper(x, penalty = 2 * log(length(x)))
#> ggcpt (changepoint detection result)
#>   Method:          fpop 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         Manual = 10.5966347330961 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

wbs_wrapper(x, n_intervals = 2000, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:          wbs 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         sSIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

wbs2_wrapper(x)
#> ggcpt (changepoint detection result)
#>   Method:          wbs2 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         SDLL = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

not_wrapper(x, contrast = "pcwsConstMean", seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:          not 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         sSIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

mosum_wrapper(x)
#> ggcpt (changepoint detection result)
#>   Method:          mosum 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 3.63416800924526 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
mosum_wrapper(x3, multiscale = TRUE)
#> ggcpt (changepoint detection result)
#>   Method:          mosum 
#>   Change in:       mean 
#>   Changepoints found: 2 
#>   CP convention:   left 
#>   Penalty:         threshold = NA 
#>   Series length:   300 
#> 
#> Changepoints:
#> # A tibble: 2 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100   -0.100
#> 2   200    2.90
```

``` r

idetect_wrapper(x, seed = 1)
#> ggcpt (changepoint detection result)
#>   Method:          idetect 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

tguh_wrapper(x)
#> ggcpt (changepoint detection result)
#>   Method:          tguh 
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         sSIC = NA 
#>   Series length:   200 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

### The 0.4.0 wave

**Multiscale inference with confidence.** SMUCE (Frick et al. 2014) and
its heteroskedastic extension HSMUCE (Pein et al. 2017) control the
probability of over-estimating the number of changes and return a
confidence interval for every changepoint location
(`ci_lower`/`ci_upper`):

``` r

res_smuce <- smuce_wrapper(x, alpha = 0.5)
tidy(res_smuce)
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100    0.369      100      100
```

**Change in slope.** CPOP performs exact penalised estimation of a
continuous piecewise-linear mean (Fearnhead et al. 2019; Fearnhead and
Grose 2024):

``` r

y_slope <- cumsum(c(rep(0.4, 100), rep(-0.3, 100))) + rnorm(200)
res_cpop <- cpop_wrapper(y_slope)
tidy(res_cpop)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100     39.5
```

**Bayesian detection.**
[`bcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bcp_wrapper.md)
implements the Barry–Hartigan product-partition model (Barry and
Hartigan 1993; Erdman and Emerson 2007) and reports a posterior
probability per location;
[`bocpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/bocpd_wrapper.md)
runs Bayesian online changepoint detection over the run-length posterior
(Adams and MacKay 2007);
[`beast_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/beast_wrapper.md)
wraps the BEAST Bayesian model-averaging ensemble (Zhao et al. 2019):

``` r

res_bcp <- bcp_wrapper(x, seed = 1)
tidy(res_bcp)
#> # A tibble: 1 × 3
#>      cp cp_value posterior_prob
#>   <int>    <dbl>          <dbl>
#> 1   100    0.369              1
```

``` r

res_bocpd <- bocpd_wrapper(x)
tidy(res_bocpd)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

res_beast <- beast_wrapper(x, seed = 1)
tidy(res_beast)
#> # A tibble: 1 × 3
#>      cp cp_value posterior_prob
#>   <int>    <dbl>          <dbl>
#> 1   100    0.369              1
```

**Sequential and kernel nonparametrics.**
[`cpm_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpm_wrapper.md)
runs distribution-free sequential tests and reports when each change
would have been *detected* in a stream (Ross 2015);
[`kcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/kcp_wrapper.md)
applies kernel change point analysis to running statistics (mean,
variance, autocorrelation, correlation) (Arlot et al. 2019; Cabrieto et
al. 2018);
[`npmojo_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/npmojo_wrapper.md)
detects distributional changes under serial dependence (McGonigle and
Cho 2025):

``` r

tidy(cpm_wrapper(x, cpm_type = "Mann-Whitney"))
#> # A tibble: 3 × 3
#>      cp cp_value detection_time
#>   <int>    <dbl>          <int>
#> 1    23   -1.39              30
#> 2    50    0.426             67
#> 3   100    0.369            104
```

``` r

tidy(kcp_wrapper(x, running_stat = "mean", nperm = 100, seed = 1))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   101     7.22
```

``` r

tidy(npmojo_wrapper(x))
#> # A tibble: 2 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
#> 2   122    8.13
```

**Robustness to drift and dependence.** DeCAFS detects abrupt changes
when the signal also drifts and the noise is autocorrelated (Romano et
al. 2022); self-normalised segmentation avoids long-run variance
estimation entirely (Zhao et al. 2022); EnvCpt only reports changepoints
when a changepoint model beats trend and autoregressive alternatives
(Beaulieu and Killick 2018):

``` r

tidy(decafs_wrapper(x))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

``` r

tidy(sn_wrapper(x3, parameter = "mean"))
#> # A tibble: 2 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100   -0.100
#> 2   199    5.53
```

``` r

res_env <- envcpt_wrapper(x, models = c("mean", "meancpt", "trendcpt"))
tidy(res_env)
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
res_env$penalty$type   # which model won
#> [1] "AIC: meancpt"
```

**High-dimensional and multivariate.**
[`inspect_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/inspect_wrapper.md)
finds sparse mean changes by projection (Wang and Samworth 2018);
[`ocd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ocd_wrapper.md)
monitors a high-dimensional stream online (Chen et al. 2022);
[`geomcp_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geomcp_wrapper.md)
maps each observation to a distance and an angle and segments both
(Grundy et al. 2020); multivariate `ecp` input flows through
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
unchanged.

``` r

set.seed(1)
X <- cbind(a = c(rnorm(80), rnorm(80, 4)),
           b = c(rnorm(80), rnorm(80, -3)),
           c = rnorm(160))
```

``` r

res_hd <- inspect_wrapper(X)
tidy(res_hd)
#> # A tibble: 1 × 3
#>      cp cp_value strength
#>   <int>    <dbl>    <dbl>
#> 1    80   -0.590     30.8
```

``` r

# Online high-dimensional detection; Monte Carlo threshold calibration makes
# this the slowest wrapper, so it is shown but not run here.
res_ocd <- ocd_wrapper(X, mc_reps = 100)
tidy(res_ocd)
```

``` r

tidy(geomcp_wrapper(X))
#> # A tibble: 0 × 2
#> # ℹ 2 variables: cp <int>, cp_value <dbl>
```

**Regression structure.**
[`strucchange_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/strucchange_wrapper.md)
dates Bai–Perron breaks with confidence intervals (Bai and Perron 1998,
2003; Zeileis et al. 2002) and accepts either a bare series or a
formula;
[`segmented_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/segmented_wrapper.md)
fits continuous broken-line regressions with breakpoint standard errors
(Muggeo 2003, 2008):

``` r

tidy(strucchange_wrapper(x))
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100    0.369       99      101
```

``` r

tidy(segmented_wrapper(y_slope, npsi = 1, seed = 1))
#> # A tibble: 1 × 4
#>      cp cp_value ci_lower ci_upper
#>   <int>    <dbl>    <int>    <int>
#> 1   100     39.5       98      101
```

**The modern PELT family.**
[`fastcpd_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/fastcpd_wrapper.md)
exposes the *fastcpd* engine — mean, variance, mean-and-variance, and
AR/ARMA/GARCH model changepoints under one interface (Li and Zhang
2024):

``` r

tidy(fastcpd_wrapper(x, family = "mean"))
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1   100    0.369
```

## The penalty path

Rather than guessing one penalty,
[`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
computes *every* optimal segmentation over a penalty interval (the CROPS
algorithm, via *changepoint* (Killick and Eckley 2014)) and returns a
`ggcpt_path` with its own
[`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), and three
plots:

``` r

path <- cpt_crops(x3)
path
#> ggcpt_path (CROPS penalty path)
#>   Change in:       mean 
#>   Penalty range:  [5.704, 57.04]
#>   Series length:   300 
#>   Distinct segmentations: 5 
#> 
#> # A tibble: 5 × 3
#>   penalty n_cpts  cost
#>     <dbl>  <int> <dbl>
#> 1    6.69      2  327.
#> 2    6.27      5  307.
#> 3    6.19      8  288.
#> 4    5.76      9  282.
#> 5    5.70     10  276.
tidy(path)
#> # A tibble: 5 × 4
#>   penalty n_cpts  cost cpts      
#>     <dbl>  <int> <dbl> <list>    
#> 1    6.69      2  327. <int [2]> 
#> 2    6.27      5  307. <int [5]> 
#> 3    6.19      8  288. <int [8]> 
#> 4    5.76      9  282. <int [9]> 
#> 5    5.70     10  276. <int [10]>
```

``` r

autoplot(path)                          # the cost elbow
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/crops-elbow-1.png)

``` r

autoplot(path, type = "path")           # changepoints vs penalty
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/crops-path-1.png)

``` r

autoplot(path, type = "segmentations")  # the candidate models themselves
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/crops-seg-1.png)

## The visualization layer

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
renders any `ggcpt`. Options: `show_segments` (fitted segment means),
`show_fit` (the engine’s own fitted signal, where provided — SMUCE,
DeCAFS, CPOP, segmented, bcp, BEAST), `show_ci` (changepoint-location
confidence intervals, where provided — SMUCE/HSMUCE, strucchange,
segmented), `show_points`/`show_line`, and the `cptline_*` styling
arguments (`cptline_color`, `cptline_alpha`, `cptline_type`,
`cptline_linewidth`).

``` r

autoplot(res, show_segments = TRUE, cptline_color = "firebrick",
         cptline_type = "dashed", cptline_linewidth = 0.8)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/autoplot-1.png)

``` r

autoplot(res_smuce, show_ci = TRUE, show_fit = TRUE)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/autoplot-ci-1.png)

Multivariate results facet automatically:

``` r

autoplot(res_hd)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/autoplot-mv-1.png)

The composable layers work inside any ggplot pipeline:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md)
draws vertical rules,
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md)
draws segment levels,
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md)
draws horizontal interval whiskers, and
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)
runs detection *inside* the plot.
[`theme_ggcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/theme_ggcpt.md)
is a publication theme and
[`annotate_segments()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/annotate_segments.md)
shades alternating segments.

``` r

cp_tbl <- tidy(res)
df <- data.frame(index = seq_along(x), value = x)

ggplot(df, aes(index, value)) +
  annotate_segments(cp = cp_tbl$cp, n = length(x)) +
  geom_line() +
  geom_changepoint(data = cp_tbl, aes(xintercept = cp), color = "red") +
  geom_cpt_segment(
    data = res$segments,
    aes(x = start, xend = end, y = param_estimate, yend = param_estimate),
    inherit.aes = FALSE, color = "darkred", linewidth = 1
  ) +
  theme_ggcpt()
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/geoms-1.png)

``` r

ggplot(df, aes(index, value)) +
  geom_line() +
  stat_changepoint(method = "pelt", color = "blue")
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/stat-1.png)

``` r

ci_tbl <- tidy(res_smuce)
ci_tbl$y_pos <- min(x) - 1
ggplot(df, aes(index, value)) +
  geom_line(color = "grey60") +
  geom_cpt_ci(
    data = ci_tbl,
    aes(y = y_pos, xmin = ci_lower, xmax = ci_upper),
    width = 0.6, color = "blue", inherit.aes = FALSE
  ) +
  geom_changepoint(data = ci_tbl, aes(xintercept = cp), color = "blue")
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/geom-ci-1.png)

[`ggcptplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcptplot.md)
and
[`ggecpplot()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggecpplot.md)
are the original one-call plots for the two classical engines:

``` r

ggcptplot(x, change_in = "mean", cp_method = "PELT")
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/original-plots-1.png)

``` r

ggecpplot(x, algorithm = "divisive", seed = 1)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/ggecpplot-1.png)

The Bayesian engines get the field’s signature displays:
[`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md)
shows the posterior mean over the series and the per-location
changepoint probability;
[`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
shows the BOCPD run-length posterior as a heatmap.

``` r

ggcpt_posterior(res_bcp)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/posterior-1.png)

``` r

ggcpt_runlength(res_bocpd)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/runlength-1.png)

Finally,
[`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md)
turns any result (or ggplot built from one) into a `plotly` widget with
values on hover:

``` r

ggcpt_interactive(res)   # requires plotly; opens an htmlwidget
```

## Comparing methods

[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
runs several detectors on the same series and renders them faceted
(default) or overlaid;
[`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
returns the tidy union. Both honour
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for parallel execution.

``` r

ggcpt_compare(x, methods = c("pelt", "binseg", "amoc"))
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/compare-1.png)

``` r

ggcpt_compare(x, methods = c("pelt", "binseg"), layout = "overlay")
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/compare-overlay-1.png)

``` r

ggcpt_compare_table(x, methods = c("pelt", "binseg", "amoc"))
#> # A tibble: 3 × 3
#>   method    cp cp_value
#>   <chr>  <int>    <dbl>
#> 1 pelt     100    0.369
#> 2 binseg   100    0.369
#> 3 amoc     100    0.369
```

## Batch detection and stability

[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md)
runs one detector over many series (matrix, data frame, or list of
vectors) and returns a tidy `ggcpt_batch` tibble with list-columns:

``` r

XB <- cbind(shifted = x, pure_noise = rnorm(200))
batch <- cpt_batch(XB, method = "pelt")
batch
#> ggcpt_batch (2 series, method: pelt)
#> 
#> # A tibble: 2 × 2
#>   series     n_changepoints
#>   <chr>               <int>
#> 1 shifted                 1
#> 2 pure_noise              0
tidy(batch)
#> # A tibble: 1 × 3
#>   series     cp cp_value
#>   <chr>   <int>    <dbl>
#> 1 shifted   100    0.369
autoplot(batch)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/batch-1.png)

[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md)
quantifies how fragile a segmentation is: it resamples residuals within
the fitted segments, re-runs the detector, and reports the re-detection
frequency at every location — a model-agnostic confidence signal for
engines with no native intervals:

``` r

st <- cpt_stability(x, method = "pelt", B = 30, seed = 1)
st
#> ggcpt_stability (30 bootstrap replicates, method: pelt)
#> 
#> Original changepoints and their re-detection frequency:
#> # A tibble: 1 × 2
#>      cp stability
#>   <int>     <dbl>
#> 1   100         1
autoplot(st)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/stability-1.png)

## Evaluation against ground truth

[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
scores predictions against a known truth: precision, recall and F1 under
one-to-one matching, the covering metric and adjusted Rand index in the
conventions of van den Burg and Williams (2020), Hausdorff distance,
annotation error, and matched MAE/RMSE.
[`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
averages over multiple annotators, and
[`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
draws the agreement (true positives, false positives, and misses):

``` r

truth <- c(100)
pred <- tidy(res)$cp
cpt_metrics(pred, truth, n = length(x), margin = 5)
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   200      1       1         1      1     1        1         0          1
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
cpt_metrics_annotated(pred, list(c(100), c(101), c(99)), n = length(x))
#> # A tibble: 1 × 7
#>       n n_annotators n_pred precision recall    f1 covering
#>   <int>        <int>  <int>     <dbl>  <dbl> <dbl>    <dbl>
#> 1   200            3      1         1      1     1    0.993
```

``` r

ggcpt_eval(pred, truth, x, margin = 5)
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/eval-plot-1.png)

## Simulation and canonical signals

[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
(alias
[`rcpt()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md))
generates series with known changepoints in mean, variance, both, or
slope, under Gaussian, Student-t, AR(1), or random-walk noise; the truth
is carried in attributes. Five canonical test signals from the
literature ship ready-made:
[`signal_blocks()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_blocks.md)
(the Donoho–Johnstone blocks signal (Donoho and Johnstone 1994)),
[`signal_fms()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_fms.md),
[`signal_mix()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_mix.md),
[`signal_teeth()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_teeth.md),
and
[`signal_stairs()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/signal_stairs.md).

``` r

sim <- cpt_simulate(300, changepoints = c(100, 200), change_in = "mean",
                    params = c(0, 5, 1), seed = 1)
attr(sim, "true_changepoints")
#> [1] 100 200
sim2 <- rcpt(300, changepoints = 150, params = c(0, 3), seed = 2)

blocks <- signal_blocks(1024, seed = 1)
fms    <- signal_fms(500, seed = 1)
mix    <- signal_mix(500, seed = 1)
teeth  <- signal_teeth(400, seed = 1)
stairs <- signal_stairs(500, seed = 1)
```

``` r

ggplot(blocks, aes(index, value)) +
  geom_line(color = "grey50") +
  geom_vline(xintercept = attr(blocks, "true_changepoints"),
             color = "blue", linewidth = 0.3) +
  labs(title = "The blocks test signal with its true changepoints")
```

![ggchangepoint feature tour
plot](ggchangepoint_files/figure-html/blocks-plot-1.png)

## Citing the methodology

[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md)
returns the verified reference(s) behind a result or a method name, so a
write-up can cite the right paper without leaving R:

``` r

cpt_cite("pelt")
#> [pelt] Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.
cpt_cite(res)
#> [pelt] Killick, R., Fearnhead, P. and Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590-1598.
```

## Closing note

This tour visited every exported function once. For the framework’s
design, the mathematics of the wrapped methods, and worked analyses, see
[`vignette("introduction", package = "ggchangepoint")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/introduction.md);
for method comparison and accuracy evaluation in depth, see
[`vignette("comparison", package = "ggchangepoint")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/comparison.md).

## References

Adams, Ryan Prescott, and David JC MacKay. 2007. “Bayesian Online
Changepoint Detection.” *arXiv Preprint arXiv:0710.3742*.

Anastasiou, Andreas, and Piotr Fryzlewicz. 2022. “Detecting Multiple
Generalized Change-Points by Isolating Single Ones.” *Metrika* 85:
141–74. <https://doi.org/10.1007/s00184-021-00821-6>.

Arlot, Sylvain, Alain Celisse, and Zaid Harchaoui. 2019. “A Kernel
Multiple Change-Point Algorithm via Model Selection.” *Journal of
Machine Learning Research* 20 (162): 1–56.

Bai, Jushan, and Pierre Perron. 1998. “Estimating and Testing Linear
Models with Multiple Structural Changes.” *Econometrica* 66 (1): 47–78.

Bai, Jushan, and Pierre Perron. 2003. “Computation and Analysis of
Multiple Structural Change Models.” *Journal of Applied Econometrics* 18
(1): 1–22.

Baranowski, Rafał, Yining Chen, and Piotr Fryzlewicz. 2019.
“Narrowest-over-Threshold Detection of Multiple Change Points and
Change-Point-Like Features.” *Journal of the Royal Statistical Society
Series B* 81 (3): 649–72. <https://doi.org/10.1111/rssb.12322>.

Barry, Daniel, and John A Hartigan. 1993. “A Bayesian Analysis for
Change Point Problems.” *Journal of the American Statistical
Association* 88 (421): 309–19.

Beaulieu, Claudie, and Rebecca Killick. 2018. “Distinguishing Trends and
Shifts from Memory in Climate Data.” *Journal of Climate* 31 (23):
9519–43.

Cabrieto, Jedelyn, Janne Adolf, Francis Tuerlinckx, Peter Kuppens, and
Eva Ceulemans. 2018. “Detecting Long-Lived Autodependency Changes in a
Multivariate System via Change Point Detection and Regime Switching
Models.” *Scientific Reports* 8: 15637.

Chen, Yudong, Tengyao Wang, and Richard J Samworth. 2022.
“High-Dimensional, Multiscale Online Changepoint Detection.” *Journal of
the Royal Statistical Society: Series B* 84 (1): 234–66.

Donoho, David L, and John M Johnstone. 1994. “Ideal Spatial Adaptation
by Wavelet Shrinkage.” *Biometrika* 81 (3): 425–55.
<https://doi.org/10.1093/biomet/81.3.425>.

Eichinger, Birte, and Claudia Kirch. 2018. “A MOSUM Procedure for the
Estimation of Multiple Random Change Points.” *Bernoulli* 24 (1):
526–64. <https://doi.org/10.3150/16-BEJ887>.

Erdman, Chandra, and John W Emerson. 2007. “Bcp: An r Package for
Performing a Bayesian Analysis of Change Point Problems.” *Journal of
Statistical Software* 23 (3): 1–13.

Fearnhead, Paul, and Daniel Grose. 2024. “Cpop: Detecting Changes in
Piecewise-Linear Signals.” *Journal of Statistical Software* 109 (7):
1–30.

Fearnhead, Paul, Robert Maidstone, and Adam Letchford. 2019. “Detecting
Changes in Slope with an L0 Penalty.” *Journal of Computational and
Graphical Statistics* 28 (2): 265–75.

Frick, Klaus, Axel Munk, and Hannes Sieling. 2014. “Multiscale Change
Point Inference.” *Journal of the Royal Statistical Society: Series B*
76 (3): 495–580.

Fryzlewicz, Piotr. 2014. “Wild Binary Segmentation for Multiple
Change-Point Detection.” *The Annals of Statistics* 42 (6): 2243–81.
<https://doi.org/10.1214/14-AOS1245>.

Fryzlewicz, Piotr. 2022. “Tail-Greedy Bottom-up Data Decompositions and
Fast Multiple Change-Point Detection.” *The Annals of Statistics* 50
(5): 2721–61.

Grundy, Thomas, Rebecca Killick, and Gueorgui Mihaylov. 2020.
“High-Dimensional Changepoint Detection via a Geometrically Inspired
Mapping.” *Statistics and Computing* 30: 1155–66.

Haynes, Kaylea, Paul Fearnhead, and Idris A Eckley. 2017. “A
Computationally Efficient Nonparametric Approach for Changepoint
Detection.” *Statistics and Computing* 27 (5): 1313–29.
<https://doi.org/10.1007/s11222-016-9687-5>.

James, Nicholas A, and David S Matteson. 2014. “Ecp: An R Package for
Nonparametric Multiple Change Point Analysis of Multivariate Data.”
*Journal of Statistical Software* 62 (7): 1–25.
<https://doi.org/10.18637/jss.v062.i07>.

Killick, Rebecca, and Idris Eckley. 2014. “Changepoint: An R Package for
Changepoint Analysis.” *Journal of Statistical Software* 58 (3): 1–19.
<https://doi.org/10.18637/jss.v058.i03>.

Killick, Rebecca, Paul Fearnhead, and Idris A Eckley. 2012. “Optimal
Detection of Changepoints with a Linear Computational Cost.” *Journal of
the American Statistical Association* 107 (500): 1590–98.
<https://doi.org/10.1080/01621459.2012.737745>.

Li, Xingchi, and Xianyang Zhang. 2024. “Fastcpd: Fast Change Point
Detection in r.” *arXiv Preprint arXiv:2404.05933*.

Maidstone, Robert, Toby Hocking, Guillem Rigaill, and Paul Fearnhead.
2017. “On Optimal Multiple Changepoint Algorithms for Large Data.”
*Statistics and Computing* 27 (2): 519–33.
<https://doi.org/10.1007/s11222-016-9636-3>.

Matteson, David S, and Nicholas A James. 2014. “A Nonparametric Approach
for Multiple Change Point Analysis of Multivariate Data.” *Journal of
the American Statistical Association* 109 (505): 334–45.

McGonigle, Euan T, and Haeran Cho. 2025. “Nonparametric Data
Segmentation in Multivariate Time Series via Joint Characteristic
Functions.” *Biometrika* 112 (2): asaf024.

Muggeo, Vito MR. 2003. “Estimating Regression Models with Unknown
Break-Points.” *Statistics in Medicine* 22 (19): 3055–71.

Muggeo, Vito MR. 2008. “Segmented: An r Package to Fit Regression Models
with Broken-Line Relationships.” *R News* 8 (1): 20–25.

Pein, Florian, Hannes Sieling, and Axel Munk. 2017. “Heterogeneous
Change Point Inference.” *Journal of the Royal Statistical Society:
Series B* 79 (4): 1207–27.

Robinson, David. 2017. “Broom: An R Package for Converting Statistical
Analysis Objects into Tidy Data Frames.” *Journal of Open Source
Software* 2 (19): 401. <https://doi.org/10.21105/joss.00401>.

Romano, Gaetano, Guillem Rigaill, Vincent Runge, and Paul Fearnhead.
2022. “Detecting Abrupt Changes in the Presence of Local Fluctuations
and Autocorrelated Noise.” *Journal of the American Statistical
Association* 117 (540): 2147–62.

Ross, Gordon J. 2015. “Parametric and Nonparametric Sequential Change
Detection in r: The Cpm Package.” *Journal of Statistical Software* 66
(3): 1–20.

van den Burg, Gerrit J J, and Christopher K I Williams. 2020. “An
Evaluation of Change Point Detection Algorithms.” *arXiv Preprint
arXiv:2003.06222*, ahead of print.
<https://doi.org/10.48550/arXiv.2003.06222>.

Wang, Tengyao, and Richard J Samworth. 2018. “High Dimensional Change
Point Estimation via Sparse Projection.” *Journal of the Royal
Statistical Society: Series B* 80 (1): 57–83.

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer. <https://doi.org/10.1007/978-3-319-24277-4>.

Zeileis, Achim, Friedrich Leisch, Kurt Hornik, and Christian Kleiber.
2002. “Strucchange: An r Package for Testing for Structural Change in
Linear Regression Models.” *Journal of Statistical Software* 7 (2):
1–38.

Zhao, Kaiguang, Michael A Wulder, Tongxi Hu, et al. 2019. “Detecting
Change-Point, Trend, and Seasonality in Satellite Time Series Data to
Track Abrupt Changes and Nonlinear Dynamics: A Bayesian Ensemble
Algorithm.” *Remote Sensing of Environment* 232: 111181.

Zhao, Zifeng, Feiyu Jiang, and Xiaofeng Shao. 2022. “Segmenting Time
Series via Self-Normalisation.” *Journal of the Royal Statistical
Society: Series B* 84 (5): 1699–725.
