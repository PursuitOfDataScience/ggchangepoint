# A catalogue of benchmark datasets

Builds a ready-made collection of labelled series for
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).
By default these are the canonical simulated signals this package
already ships — which means the benchmark runs offline,
deterministically, and inside `R CMD check`. Pass `source = "tcpd"` for
the Turing Change Point Dataset instead, which is downloaded and cached.

## Usage

``` r
cpt_datasets(
  source = c("simulated", "tcpd"),
  n = 500,
  seed = 1,
  names = NULL,
  ...
)
```

## Arguments

- source:

  `"simulated"` (default) or `"tcpd"`.

- n:

  Length of each simulated series. Defaults to `500`.

- seed:

  Seed for the simulated signals. Defaults to `1`.

- names:

  Optional subset of dataset names.

- ...:

  Passed to
  [`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md)
  for `source = "tcpd"`.

## Value

A named list of `list(series, annotations)` datasets, ready for
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).

## See also

[`cpt_load_tcpd()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_load_tcpd.md),
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md).

## Examples

``` r
names(cpt_datasets(n = 200))
#> [1] "blocks"   "fms"      "mix"      "teeth"    "stairs"   "step"     "ar1"     
#> [8] "heavy"    "varshift"
```
