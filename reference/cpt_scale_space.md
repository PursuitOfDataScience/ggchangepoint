# Scale space: the statistic across bandwidths

Sweeps a multiscale detector's bandwidth and returns the statistic at
every (location, bandwidth) pair. The resulting heatmap answers the
question a single-bandwidth fit cannot: *at which resolutions does this
feature exist?* A change that is significant only at a wide bandwidth is
a slow shift; one that appears only at a narrow bandwidth is a spike.

## Usage

``` r
cpt_scale_space(x, bandwidths = NULL, method = c("mosum", "npmojo"), ...)

ggcpt_scale_space(x, bandwidths = NULL, method = c("mosum", "npmojo"), ...)
```

## Arguments

- x:

  A numeric vector (or, for `method = "npmojo"`, a matrix with rows as
  time points), or a `ggcpt` object produced by a multiscale engine
  (`subset(cpt_methods(), scale_space)$method`).

- bandwidths:

  Integer vector of bandwidths to sweep. Defaults to a geometric grid
  between `max(5, n/50)` and `n/4`.

- method:

  Which engine to sweep: `"mosum"` (default) or `"npmojo"`. Taken from
  `x` when it is a `ggcpt` produced by one of them.

- ...:

  Additional arguments passed to the engine at each bandwidth.

## Value

`cpt_scale_space()` returns a tibble with `index`, `bandwidth`,
`statistic`, `threshold`, `significant` (the statistic at that location
and bandwidth exceeds the threshold) and `detected` (the engine reported
a changepoint there at that bandwidth). The two differ: a location can
clear the threshold without surviving the engine's own pruning.
`ggcpt_scale_space()` draws the heatmap with the accepted changepoints
overlaid.

## See also

[`cpt_statistic()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_statistic.md).

## Examples

``` r
set.seed(2026)
x <- c(rnorm(300), rnorm(300, 2))
ss <- cpt_scale_space(x, bandwidths = c(20, 40, 80))
head(ss)
#> # A tibble: 6 × 6
#>   index bandwidth statistic threshold significant detected
#>   <int>     <int>     <dbl>     <dbl> <lgl>       <lgl>   
#> 1     1        20     0.618      3.91 FALSE       FALSE   
#> 2     2        20     0.375      3.91 FALSE       FALSE   
#> 3     3        20     0.193      3.91 FALSE       FALSE   
#> 4     4        20     0.194      3.91 FALSE       FALSE   
#> 5     5        20     0.500      3.91 FALSE       FALSE   
#> 6     6        20     1.65       3.91 FALSE       FALSE   
```
