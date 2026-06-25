# Comparing changepoint methods with ggchangepoint

## Introduction

The companion vignette
[`vignette("introduction", package = "ggchangepoint")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/introduction.md)
provides a comprehensive overview of ggchangepoint’s design, API, and
supported methods. This vignette focuses on **method comparison**—the
problem of running multiple detectors on the same data, comparing their
outputs, and measuring their accuracy when ground truth is known.

## Problem setup

Let $`y_{1:n}`$ be an ordered sequence. A segmentation with $`m`$
changepoints is an ordered set $`\tau_{0:m+1}`$ with
$`0=\tau_0<\tau_1<\dots<\tau_m<\tau_{m+1}=n`$, partitioning the data
into $`m+1`$ contiguous segments. Within each segment the data are
governed by a parameter $`\theta_j`$ (e.g. mean, variance, or
distribution) that changes at each $`\tau_j`$(Truong et al. 2020).

Most offline methods minimise a penalised cost

``` math
 \sum_{j=1}^{m+1} \mathcal{C}(y_{(\tau_{j-1}+1):\tau_j}) + \beta f(m), 
```

where $`\mathcal{C}(\cdot)`$ is a segment cost (typically $`-2 \times`$
maximised log-likelihood) and $`\beta f(m)`$ guards against
over-segmentation (Yao 1988). The choice of penalty $`\beta`$ is the
central challenge of the field.

## Taxonomy of methods

The methods available through
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
in this release fall into three broad families:

| Family | Methods | Approach |
|----|----|----|
| Exact / pruned optimal partitioning | PELT, BinSeg, SegNeigh, AMOC, FPOP | Dynamic programming with pruning (Killick et al. 2012; Maidstone et al. 2017) |
| Randomised and multiscale search | WBS, WBS2, NOT, MOSUM, IDetect, TGUH | Random intervals or moving windows (Fryzlewicz 2014, 2020, 2022; Baranowski et al. 2019; Eichinger and Kirch 2018; Anastasiou and Fryzlewicz 2022) |
| Nonparametric / distribution-free | NP (changepoint.np), ECP (ecp) | Empirical distribution or energy distance (Haynes et al. 2017; James and Matteson 2014) |

The
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
function runs multiple methods on the same series and arranges the
results. It respects the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for optional parallel execution when many methods are compared.

## Comparing methods on a simulated series

Generate a three-regime series with changes in mean and variance:

``` r

set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1), rnorm(100, 5, 2))
```

The faceted layout shows each method in its own panel, with x-axes
aligned:

``` r

ggcpt_compare(x, methods = cmp_methods, layout = "facet")
```

![ggchangepoint plot comparing changepoint detection methods on a time
series](comparison_files/figure-html/fig-1-1.png)

The overlay layout draws all changepoints on a single panel,
colour-coded:

``` r

ggcpt_compare(x, methods = cmp_methods, layout = "overlay")
```

![ggchangepoint plot comparing changepoint detection methods on a time
series](comparison_files/figure-html/fig-2-1.png)

For a numeric summary,
[`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md)
returns a tidy tibble:

``` r

ggcpt_compare_table(x, methods = cmp_methods)
#> # A tibble: 10 × 3
#>    method    cp cp_value
#>    <chr>  <dbl>    <dbl>
#>  1 pelt     100    0.467
#>  2 pelt     200   10.3  
#>  3 binseg   100    0.467
#>  4 binseg   200   10.3  
#>  5 fpop     100    0.467
#>  6 fpop     200   10.3  
#>  7 fpop     247    3.83 
#>  8 fpop     251    8.22 
#>  9 fpop     284    2.81 
#> 10 fpop     287    8.76
```

## Accuracy metrics

When true changepoint locations are known,
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
provides a standard suite of accuracy measures (Truong et al. 2020; van
den Burg and Williams 2020):

``` r

truth <- c(100, 200)

# PELT
res_pelt <- cpt_detect(x, method = "pelt", change_in = "meanvar")
generics::tidy(res_pelt)$cp
#> [1] 100 200

# Metrics for BinSeg
res_binseg <- cpt_detect(x, method = "binseg", change_in = "meanvar")
cpt_metrics(generics::tidy(res_binseg)$cp, truth, n = length(x))
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   300      2       2         1      1     1        1         0          1
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
```

### Visual evaluation

[`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md)
overlays predictions and ground truth, colouring true positives, false
positives, and misses within a tolerance margin:

``` r

ggcpt_eval(pred = c(100), truth = c(100, 200), data_vec = x)
```

![ggchangepoint plot comparing changepoint detection methods on a time
series](comparison_files/figure-html/fig-3-1.png)

## Evaluation study on canonical signals

We now conduct a systematic comparison across the five built-in test
signals (Donoho and Johnstone 1994), using a tolerance margin of 5
observations.

``` r

set.seed(42)
signals <- list(
  blocks = signal_blocks(512),
  fms    = signal_fms(512),
  mix    = signal_mix(512),
  teeth  = signal_teeth(512),
  stairs = signal_stairs(512)
)
methods <- c("pelt", "binseg", "fpop", "wbs", "not")
margin <- 5

results <- do.call(rbind, lapply(names(signals), function(nm) {
  sig <- signals[[nm]]
  truth <- attr(sig, "true_changepoints")
  do.call(rbind, lapply(methods, function(m) {
    res <- tryCatch(
      cpt_detect(sig$value, method = m, change_in = "mean"),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    pred <- generics::tidy(res)$cp
    met <- cpt_metrics(pred, truth, n = length(sig$value), margin = margin)
    data.frame(signal = nm, method = m, met)
  }))
}))
#> Warning in BINSEG(sumstat, pen = pen.value, cost_func = costfunc, minseglen =
#> minseglen, : The number of changepoints identified is Q, it is advised to
#> increase Q to make sure changepoints have not been missed.
#> Warning in BINSEG(sumstat, pen = pen.value, cost_func = costfunc, minseglen =
#> minseglen, : The number of changepoints identified is Q, it is advised to
#> increase Q to make sure changepoints have not been missed.
results[, c("signal", "method", "precision", "recall", "f1", "covering",
            "hausdorff")]
#>    signal method precision    recall        f1  covering hausdorff
#> 1  blocks   pelt 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 2  blocks binseg 1.0000000 0.4545455 0.6250000 0.6310029        82
#> 3  blocks   fpop 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 4  blocks    wbs 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 5  blocks    not 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 6     fms   pelt 1.0000000 0.2857143 0.4444444 0.3773701       179
#> 7     fms binseg 0.7500000 0.4285714 0.5454545 0.6386754       172
#> 8     fms   fpop 1.0000000 0.8571429 0.9230769 0.8626271        51
#> 9     fms    wbs 1.0000000 1.0000000 1.0000000 0.9519373         5
#> 10    fms    not 1.0000000 1.0000000 1.0000000 0.9632035         4
#> 11    mix   pelt 0.0000000 0.0000000 0.0000000 0.4667797        55
#> 12    mix binseg 0.0000000 0.0000000 0.0000000 0.3929160        59
#> 13    mix   fpop 0.0000000 0.0000000 0.0000000 0.4667797        55
#> 14    mix    wbs 0.1111111 0.2000000 0.1428571 0.5560015        77
#> 15    mix    not 0.1000000 0.2000000 0.1333333 0.5355308        77
#> 16  teeth   pelt 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 17  teeth binseg 1.0000000 1.0000000 1.0000000 0.9961131         1
#> 18  teeth   fpop 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 19  teeth    wbs 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 20  teeth    not 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 21 stairs   pelt 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 22 stairs binseg 1.0000000 0.5555556 0.7142857 0.5991635        51
#> 23 stairs   fpop 1.0000000 1.0000000 1.0000000 1.0000000         0
#> 24 stairs    wbs 1.0000000 1.0000000 1.0000000 0.9961313         1
#> 25 stairs    not 1.0000000 1.0000000 1.0000000 1.0000000         0
```

The table shows how different methods perform across signal types. PELT
and FPOP, as exact methods, generally have strong performance, while WBS
and NOT can detect small or short features that the exact methods miss
at the default penalty setting—but may also over-segment.

### Interpreting the metrics

- **Precision / Recall / F1**: standard classification measures with a
  margin of tolerance.
- **Covering**: the average Jaccard overlap between true and predicted
  segments (van den Burg and Williams 2020).
- **Hausdorff distance**: worst-case localisation error between
  predicted and true changepoint sets.

## Full workflow: simulate, detect, evaluate

A complete reproducible workflow combining the simulator, detection, and
evaluation:

``` r

set.seed(1)
dat <- cpt_simulate(200, changepoints = c(60, 120),
                    change_in = "mean", params = c(0, 8, 3), sd = 1)
truth <- attr(dat, "true_changepoints")

res <- cpt_detect(dat$value, method = "pelt", change_in = "mean")
pred <- generics::tidy(res)$cp

cpt_metrics(pred, truth, n = length(dat$value))
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   200      2       2         1      1     1        1         0          1
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
```

## Multi-annotator evaluation

When multiple ground-truth annotations are available—as in the Turing
Change Point Dataset (van den Burg and Williams 2020)—use
[`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
to average metrics across annotators:

``` r

cpt_metrics_annotated(pred = c(100, 200),
                      annotations = list(c(100, 200), c(105, 198)),
                      n = 300)
#> # A tibble: 1 × 7
#>       n n_annotators n_pred precision recall    f1 covering
#>   <dbl>        <int>  <int>     <dbl>  <dbl> <dbl>    <dbl>
#> 1   300            2      2         1      1     1    0.977
```

## Parallel comparison (optional)

If the `future` and `future.apply` packages are installed,
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
and the evaluation loop can run methods in parallel by setting a
[`future::plan()`](https://future.futureverse.org/reference/plan.html):

``` r

future::plan(future::multisession, workers = 2)
ggcpt_compare(x, methods = c("pelt", "binseg", "wbs", "fpop", "not"),
              seed = 1)
```

Parallel execution is reproducible: when a `seed` is supplied, the
detectors are fanned out over parallel-safe L’Ecuyer-CMRG random
streams, so the result is identical regardless of worker count or
backend.

## See also

- [`vignette("introduction", package = "ggchangepoint")`](https://pursuitofdatascience.github.io/ggchangepoint/articles/introduction.md)
  for a comprehensive walkthrough of the package API and supported
  methods.
- The R package references for the individual detection engines:
  **changepoint** (Killick and Eckley 2014), **wbs** (Fryzlewicz 2014),
  **not** (Baranowski et al. 2019), **mosum** (Eichinger and Kirch
  2018), **fpop** (Maidstone et al. 2017), **IDetect** (Anastasiou and
  Fryzlewicz 2022), **breakfast** (Fryzlewicz 2020, 2022),
  **changepoint.np** (Haynes et al. 2017), and **ecp** (James and
  Matteson 2014).

## References

Anastasiou, Andreas, and Piotr Fryzlewicz. 2022. “Detecting Multiple
Generalized Change-Points by Isolating Single Ones.” *Metrika* 85:
141–74. <https://doi.org/10.1007/s00184-021-00821-6>.

Baranowski, Rafał, Yining Chen, and Piotr Fryzlewicz. 2019.
“Narrowest-over-Threshold Detection of Multiple Change Points and
Change-Point-Like Features.” *Journal of the Royal Statistical Society
Series B* 81 (3): 649–72. <https://doi.org/10.1111/rssb.12322>.

Donoho, David L, and John M Johnstone. 1994. “Ideal Spatial Adaptation
by Wavelet Shrinkage.” *Biometrika* 81 (3): 425–55.
<https://doi.org/10.1093/biomet/81.3.425>.

Eichinger, Birte, and Claudia Kirch. 2018. “A MOSUM Procedure for the
Estimation of Multiple Random Change Points.” *Bernoulli* 24 (1):
526–64. <https://doi.org/10.3150/16-BEJ887>.

Fryzlewicz, Piotr. 2014. “Wild Binary Segmentation for Multiple
Change-Point Detection.” *The Annals of Statistics* 42 (6): 2243–81.
<https://doi.org/10.1214/14-AOS1245>.

Fryzlewicz, Piotr. 2020. “Detecting Multiple Change-Point Features via
Narrowest-over-Threshold.” *Journal of the Royal Statistical Society
Series B* 82 (5): 1377–418.

Fryzlewicz, Piotr. 2022. “Tail-Greedy Bottom-up Data Decompositions and
Fast Multiple Change-Point Detection.” *The Annals of Statistics* 50
(5): 2721–61.

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

Maidstone, Robert, Toby Hocking, Guillem Rigaill, and Paul Fearnhead.
2017. “On Optimal Multiple Changepoint Algorithms for Large Data.”
*Statistics and Computing* 27 (2): 519–33.
<https://doi.org/10.1007/s11222-016-9636-3>.

Truong, Charles, Laurent Oudre, and Nicolas Vayatis. 2020. “Selective
Review of Offline Change Point Detection Methods.” *Signal Processing*
167: 107299. <https://doi.org/10.1016/j.sigpro.2019.107299>.

van den Burg, Gerrit J J, and Christopher K I Williams. 2020. “An
Evaluation of Change Point Detection Algorithms.” *arXiv Preprint
arXiv:2003.06222*, ahead of print.
<https://doi.org/10.48550/arXiv.2003.06222>.

Yao, Yi-Ching. 1988. “Estimating the Number of Change-Points via
Schwarz’ Criterion.” *Statistics & Probability Letters* 6 (3): 181–89.
<https://doi.org/10.1016/0167-7152(88)90118-6>.
