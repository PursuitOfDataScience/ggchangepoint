# `ggchangepoint` package

Unified tidy changepoint detection with `ggplot2` visualisation.

## Details

`ggchangepoint` provides a consistent S3 result class (`ggcpt`) for
changepoint detection results, `broom`-style methods
([`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`augment()`](https://generics.r-lib.org/reference/augment.html)),
`ggplot2` integration via
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and composable geoms
([`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md)),
and a unified dispatcher
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
that supports over thirty methods.

\*\*Detection engines.\*\*
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
currently dispatches to 31 methods across six families (run
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
for the live table with installation status):

- **Penalised/optimal:** PELT, BinSeg, SegNeigh, AMOC (changepoint);
  FPOP (fpop); the CROPS penalty path
  ([`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md));
  fastcpd (fastcpd, incl. AR/ARMA/GARCH); change-in-slope via CPOP
  (cpop).

- **Multiscale/search:** WBS (wbs), WBS2 and TGUH (breakfast), NOT
  (not), MOSUM incl. multiscale (mosum), Isolate-Detect (IDetect),
  SMUCE/HSMUCE with confidence intervals (stepR).

- **Nonparametric/kernel:** NP (changepoint.np), E-Divisive/E-Agglo
  (ecp), kernel running statistics (kcpRS), NP-MOJO (CptNonPar),
  sequential CPM (cpm), self-normalisation (SNSeg).

- **Bayesian:** Barry-Hartigan posterior (bcp), online BOCPD (ocp),
  BEAST model averaging (Rbeast).

- **Multivariate/high-dimensional:** sparse projection
  (InspectChangepoint), online ocd (ocd), geometric mapping
  (changepoint.geo).

- **Regression breaks and robust detection:** Bai-Perron breaks with CIs
  (strucchange), broken-line regression (segmented),
  changepoints-vs-autocorrelation model selection (EnvCpt), drift+AR
  robust detection (DeCAFS).

\*\*Key features.\*\* Every detector returns a `ggcpt` object with a
stable `tibble(cp, cp_value)` contract (plus engine extras such as
`ci_lower`/`ci_upper` and `posterior_prob`). Visualise any result
directly with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(confidence intervals, fitted signals, multivariate facets), the
Bayesian displays
([`ggcpt_posterior()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_posterior.md),
[`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)),
or interactively via
[`ggcpt_interactive()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_interactive.md).
Compare methods with
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md);
run panels of series with
[`cpt_batch()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_batch.md);
quantify uncertainty with
[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md);
sweep penalties with
[`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md).
Evaluate accuracy with
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
and
[`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md);
simulate ground-truth data with
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
and the canonical test signals; and cite the methodology behind any
result with
[`cpt_cite()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_cite.md).

## See also

Useful links:

- <https://pursuitofdatascience.github.io/ggchangepoint/>

- Report bugs at
  <https://github.com/PursuitOfDataScience/ggchangepoint/issues>

## Author

**Maintainer**: Youzhi Yu <yuyouzhi666@icloud.com>
