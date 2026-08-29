# Influence diagnostics for a changepoint segmentation

Perturbs one observation at a time — deleting it, or replacing it with
an outlier — re-runs the detector, and reports what changed: the number
of changepoints, where they moved to, and how the segment parameters
responded. This is the diagnostic family of Wilms, Killick and Matteson
(2022), rendered in ggplot2 so it composes with the rest of the package.

## Usage

``` r
cpt_influence(
  object,
  type = c("delete", "outlier"),
  engine = c("auto", "changepoint.influence", "recompute"),
  subset = NULL,
  outlier_sd = 5,
  seed = NULL,
  ...
)

# S3 method for class 'ggcpt_influence'
print(x, ...)

# S3 method for class 'ggcpt_influence'
autoplot(
  object,
  plot_type = c("overview", "location", "parameter", "map"),
  ...
)

# S3 method for class 'ggcpt_influence'
tidy(x, ...)
```

## Arguments

- object:

  A `ggcpt_influence` object (for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

- type:

  `"delete"` (drop the observation) or `"outlier"` (replace it with a
  large value). Defaults to `"delete"`.

- engine:

  Which implementation to use: `"auto"` (default) uses
  changepoint.influence when the result came from a changepoint engine
  and that package is installed, and the generic recomputation
  otherwise; `"changepoint.influence"` insists on the former;
  `"recompute"` insists on the latter, which works for every wired and
  registered method.

- subset:

  Optional integer vector of observation positions to perturb. Influence
  by deletion costs one detector fit per observation, so on a long
  series or an expensive engine this is the argument that makes the
  diagnostic affordable. Defaults to every observation.

- outlier_sd:

  For `type = "outlier"`, how many residual standard deviations the
  substituted value sits above the fitted level. Defaults to `5`.

- seed:

  Optional seed, for detectors that randomise.

- ...:

  Additional arguments passed to
  [`cpt_detect()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_detect.md)
  on each perturbed series.

- x:

  A `ggcpt_influence` object (for
  [`print()`](https://rdrr.io/r/base/print.html)).

- plot_type:

  Which diagnostic to draw: `"overview"` (the series with the
  influential observations highlighted), `"location"` (perturbed
  changepoint locations against the perturbed observation),
  `"parameter"` (segment-parameter shift per perturbation) or `"map"`
  (the full influence map: perturbed observation on x, position on y,
  parameter shift as fill).

## Value

A `ggcpt_influence` object: a list with

- `influence`:

  a tibble with one row per perturbed observation — `index`, `n_cp`,
  `delta_n_cp` (against the unperturbed fit), `max_shift` (largest
  movement of a surviving changepoint, in positions), `param_shift`
  (largest absolute change in a segment parameter) and `cpts` (a
  list-column of the perturbed changepoint sets);

- `param`:

  an \\n \times n\\ matrix of per-observation segment parameters, one
  row per perturbation — the input to the influence map;

- `original`, `type`, `engine`, `method`:

with [`print()`](https://rdrr.io/r/base/print.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods.

## References

Wilms I, Killick R, Matteson DS (2022). “Graphical influence diagnostics
for changepoint models.” *Journal of Computational and Graphical
Statistics*, **31**(3), 753–765.
[doi:10.1080/10618600.2021.2000873](https://doi.org/10.1080/10618600.2021.2000873)
.

## See also

[`cpt_leverage()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_leverage.md),
[`cpt_sensitivity()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_sensitivity.md),
[`cpt_stability()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_stability.md).

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
inf <- cpt_influence(fit)
inf
#> ggcpt_influence (delete perturbation, method: pelt, engine: changepoint.influence)
#>   Observations perturbed: 80
#>   Unperturbed changepoints: 1
#>   Perturbations changing the number of changepoints: 0 (0%)
#> 
#> Most influential observations:
#> # A tibble: 5 × 5
#>   index delta_n_cp max_shift param_shift leverage
#>   <int>      <int>     <dbl>       <dbl>    <dbl>
#> 1    40          0         1      0.0201     8.90
#> 2    15          0         0      0.0642     2.93
#> 3     6          0         0      0.0635     2.88
#> 4    77          0         0      0.0578     2.50
#> 5    26          0         0      0.0500     1.97
head(cpt_leverage(inf))
#> # A tibble: 6 × 5
#>   index delta_n_cp max_shift param_shift leverage
#>   <int>      <int>     <dbl>       <dbl>    <dbl>
#> 1    40          0         1      0.0201     8.90
#> 2    15          0         0      0.0642     2.93
#> 3     6          0         0      0.0635     2.88
#> 4    77          0         0      0.0578     2.50
#> 5    26          0         0      0.0500     1.97
#> 6    27          0         0      0.0454     1.66
```
