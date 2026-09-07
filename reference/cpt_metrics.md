# Changepoint accuracy metrics

Computes standard accuracy metrics comparing predicted changepoints to
ground truth, including precision/recall/F1 with margin, covering
metric, Hausdorff distance, adjusted Rand index, annotation error, and
MAE/RMSE of matched locations.

## Usage

``` r
cpt_metrics(pred, truth, n, margin = 5)
```

## Arguments

- pred:

  Predicted changepoint indices (integer vector).

- truth:

  Ground truth changepoint indices (integer vector).

- n:

  Length of the series.

- margin:

  Tolerance margin for matching (default 5).

## Value

A tibble with one row and the columns below. “Higher” and “lower” mark
which direction is better — the same directions
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
ranks by.

- `n`, `n_pred`, `n_truth`:

  the series length and the two changepoint counts, after out-of-range
  indices are dropped.

- `precision`, `recall`, `f1`:

  higher is better. The fraction of predictions that matched a truth,
  the fraction of truths that were matched, and their harmonic mean —
  all under the one-to-one matching within `margin`, so they score
  *whether* a change was found, not how precisely it was located.

- `covering`:

  higher is better, in \\\[0, 1\]\\. The segmentation covering metric:
  each true segment's best Jaccard overlap with a predicted segment,
  averaged weighted by segment length. Unlike F1 it needs no margin and
  degrades smoothly with location error.

- `hausdorff`:

  lower is better, in observations. The largest distance from any
  changepoint on either side to the nearest one on the other — a
  worst-case location error, so one badly placed changepoint dominates
  it.

- `rand_index`:

  higher is better. The *adjusted* Rand index between the two segment
  labellings: 1 for identical partitions, 0 for chance agreement, and
  negative for worse than chance.

- `annotation_error`:

  lower is better. **A count difference only**: `abs(n_pred - n_truth)`.
  It says nothing about location, so a segmentation with the right
  *number* of changepoints in entirely the wrong *places* scores a
  perfect 0. Read it beside `covering` or `hausdorff`, never alone.

- `mae_matched`, `rmse_matched`:

  lower is better, in observations. Mean absolute and root-mean-square
  location error over the matched pairs only, so they describe how well
  the changepoints that were found are placed and ignore the ones that
  were missed. `NA` when nothing matched.

## Details

Precision/recall use a one-to-one matching: each truth may be claimed by
at most one prediction (predictions are scanned in order and take the
earliest unmatched truth within `margin`, which yields a maximum
matching for interval-structured problems). The covering metric follows
van den Burg and Williams (2020): the prediction-side partition is
always well defined, so an empty `pred` scores the covering of the
trivial single-segment partition rather than 0.

## Degenerate cases, and which way each metric resolves them

Three of the twelve columns are ratios with a zero denominator when one
side is empty, and the row does not resolve them all the same way, so
the conventions are worth stating rather than inferring from a benchmark
table:

- **Both empty.** The segmentation is exactly right: `precision`,
  `recall`, `f1`, `covering` and `rand_index` are all 1 and
  `annotation_error` is 0. (A metric that scored this 0 was a 0.4.0
  bug.)

- **One side empty.** `precision` and `recall` are *0* rather than `NA`
  — finding nothing where there was a change, and finding a change where
  there was none, both score badly — and so is `f1`. `rand_index` is 0
  for the same reason.

- **Nothing matched.** `hausdorff`, `mae_matched` and `rmse_matched` are
  `NA`, because they are distances and there is no pair to measure. They
  are also `NA` whenever either side is empty. This is the one place the
  row mixes conventions: an all-wrong answer gives `f1 = 0` and
  `mae_matched = NA` in the same row.
  [`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
  treats an `NA` as the worst rank rather than dropping it.

An index outside `1..(n - 1)` is dropped with a warning: locations
follow the `"left"` convention, and an out-of-range one would corrupt
the partition metrics rather than merely miss.

## See also

[`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
for scoring against multiple annotators, and
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
to run a method-by-dataset grid on these metrics.

## Examples

``` r
cpt_metrics(c(100, 200), c(100, 200), n = 300)
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   300      2       2         1      1     1        1         0          1
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
cpt_metrics(c(101, 205), c(100, 200), n = 300, margin = 5)
#> # A tibble: 1 × 12
#>       n n_pred n_truth precision recall    f1 covering hausdorff rand_index
#>   <int>  <int>   <int>     <dbl>  <dbl> <dbl>    <dbl>     <dbl>      <dbl>
#> 1   300      2       2         1      1     1    0.961         5      0.941
#> # ℹ 3 more variables: annotation_error <int>, mae_matched <dbl>,
#> #   rmse_matched <dbl>
```
