# Changepoint accuracy metrics

Computes standard accuracy metrics comparing predicted changepoints to
ground truth, including precision/recall/F1 with margin, covering
metric, Hausdorff distance, (adjusted) Rand index, annotation error, and
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

A tibble with columns: `n`, `n_pred`, `n_truth`, `precision`, `recall`,
`f1`, `covering`, `hausdorff`, `rand_index`, `annotation_error`,
`mae_matched`, `rmse_matched`.

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
