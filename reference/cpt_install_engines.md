# Install the engines behind a family of methods

The package wraps a lot of upstream engines, all of them in `Suggests`
so that installing ggchangepoint does not drag in dozens of packages
nobody asked for. The cost of that choice is a discovery problem:
`cpt_detect(x, method = "smuce")` tells you to install stepR, but only
one package at a time. This installs a whole family in one call.

## Usage

``` r
cpt_install_engines(bundle = "core", dry_run = FALSE, ...)
```

## Arguments

- bundle:

  Which family to install:

  `"core"`

  :   the engines the common methods need: fpop, wbs, breakfast, not,
      mosum, IDetect, stepR.

  `"bayesian"`

  :   bcp, ocp, Rbeast.

  `"nonparametric"`

  :   cpm, kcpRS, CptNonPar, SNSeg.

  `"highdim"`

  :   InspectChangepoint, ocd, changepoint.geo, HDCD, changepoints.

  `"functional"`

  :   fChange, KWCChangepoint, fabisearch.

  `"regression"`

  :   strucchange, segmented, EnvCpt, DeCAFS, cpop, fastcpd.

  `"inference"`

  :   nsp, crossvalidationCP, changepoint.influence, penaltyLearning.

  `"applied"`

  :   trend, ChangePointTaylor, bfast, wbsts, binsegRcpp.

  `"time"`

  :   index and coercion support: zoo, xts, tsibble.

  `"reporting"`

  :   gt, ggrepel, plotly, ggiraph, progressr, jsonlite.

  `"all"`

  :   every engine and extra the package knows about.

  Several bundles may be given at once.

- dry_run:

  Report what would be installed without installing anything. Defaults
  to `FALSE`.

- ...:

  Passed to
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

Invisibly, a tibble with one row per package (`package`, `bundle`,
`installed_before`, `installed_after`).

## See also

[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
for the per-method installation status.

## Examples

``` r
cpt_install_engines("bayesian", dry_run = TRUE)
#> Every package in bayesian is already installed.
```
