# The solution path of a search-based detector

The order in which candidate changepoints entered the model, with the
contrast (or split criterion) at each step. Binary segmentation splits
recursively, WBS/WBS2/NOT/TGUH rank random intervals — in every case the
final answer is a prefix of a path, and seeing the path shows how
decisively each changepoint beat the next.

## Usage

``` r
cpt_solution_path(object)

ggcpt_solution_path(object, max_steps = 40)
```

## Arguments

- object:

  A `ggcpt` object from an engine with a solution path
  (`subset(cpt_methods(), path)$method`).

- max_steps:

  Longest prefix of the path drawn. Defaults to `40` — a randomised
  search proposes hundreds of candidates and only the head of the
  ranking is readable.

## Value

`cpt_solution_path()` returns a tibble with `step`, `cp`, `contrast` and
— for interval-based searches — `start`/`end` of the interval that
proposed it, plus a `selected` flag marking the changepoints in the
final model. `ggcpt_solution_path()` draws it.

`contrast` is the engine's own ordering criterion, and it is a
*different quantity* per engine: a penalty value for `binseg`/`segneigh`
(from
[`changepoint::pen.value.full()`](https://rdrr.io/pkg/changepoint/man/pen.value.full.html)),
\\\|CUSUM\|\\ for `wbs`, \\\|\\`max.contrast`\\\|\\ for `not`, and
breakfast's candidate criterion for `wbs2`/`tguh`. The values order the
candidates within one result; they are not comparable across engines,
and the plot legend names the quantity rather than calling all of them
"Contrast".

For `wbs2` and `tguh` the path is **recomputed** with breakfast, because
their fit objects do not keep the candidate list. `wbs2`'s search is
randomised, so its path is a second search of the same series rather
than a record of the first — it can differ between calls, and `selected`
can be `FALSE` throughout if the recomputed candidates miss the fit's
own changepoints. Every other engine's path is read off the fit.

## See also

[`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md),
[`cpt_crops()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_crops.md)
for the penalty path of an optimal-partitioning method.

## Examples

``` r
set.seed(2026)
fit <- cpt_detect(c(rnorm(150), rnorm(150, 3)), method = "binseg")
cpt_solution_path(fit)
#> # A tibble: 5 × 6
#>    step    cp contrast start   end selected
#>   <int> <int>    <dbl> <int> <int> <lgl>   
#> 1     1   150   730.      NA    NA TRUE    
#> 2     2    15     4.21    NA    NA FALSE   
#> 3     3    14     3.87    NA    NA FALSE   
#> 4     4   149     2.61    NA    NA FALSE   
#> 5     5   117     1.78    NA    NA FALSE   
```
