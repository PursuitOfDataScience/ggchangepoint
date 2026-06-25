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
that supports multiple engines.

\*\*Detection engines.\*\* The package currently wraps 13 methods behind
the unified
[`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
front-end:

- **changepoint:** PELT, BINSEG, SEGNEIGH, AMOC (mean, var, meanvar)

- **changepoint.np:** NP (non-parametric, distribution-free)

- **ecp:** E-Divisive, E-Agglo (multivariate, non-parametric)

- **fpop:** FPOP (functional pruning optimal partitioning)

- **wbs:** Wild Binary Segmentation

- **breakfast:** WBS2, TGUH

- **not:** Narrowest-Over-Threshold (mean, var, slope contrasts)

- **mosum:** Moving Sum

- **IDetect:** Isolate-Detect

Additional engines are planned (see
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
for the full table).

\*\*Key features.\*\* Every detector returns a `ggcpt` object with a
stable `tibble(cp, cp_value)` contract. Visualise any result directly
with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
Compare methods with
[`ggcpt_compare()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare.md)
and
[`ggcpt_compare_table()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_compare_table.md).
Evaluate accuracy with
[`cpt_metrics()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics.md)
and
[`ggcpt_eval()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_eval.md).
Generate synthetic data with known ground truth via
[`cpt_simulate()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_simulate.md)
and the built-in canonical test signals.

## See also

Useful links:

- <https://pursuitofdatascience.github.io/ggchangepoint/>

- Report bugs at
  <https://github.com/PursuitOfDataScience/ggchangepoint/issues>

## Author

**Maintainer**: Youzhi Yu <yuyouzhi666@icloud.com>
