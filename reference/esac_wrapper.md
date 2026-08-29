# ESAC wrapper — sparsity-adaptive high-dimensional detection

Wraps [`HDCD::ESAC()`](https://rdrr.io/pkg/HDCD/man/ESAC.html) (Moen,
Glad and Tveten, 2023): Efficient Sparsity Adaptive Changepoint
estimation for a change in the mean vector of a high-dimensional series.
Where `inspect` projects onto a single estimated sparse direction, ESAC
adapts across the whole sparsity range at once, which is a different
regime rather than a refinement of the same one — it is competitive both
when a handful of coordinates change and when all of them do.

## Usage

``` r
esac_wrapper(
  x,
  threshold_d = 1.5,
  threshold_s = 1,
  empirical = FALSE,
  N = 1000,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with rows as time points and columns as
  coordinates.

- threshold_d, threshold_s:

  Leading constants of the dense and sparse thresholds. Defaults follow
  the engine (`1.5` and `1`).

- empirical:

  Calibrate the thresholds by Monte Carlo rather than using the
  theoretical values? Slower but sharper; defaults to `FALSE`.

- N:

  Monte Carlo samples when `empirical = TRUE`.

- seed:

  Optional seed (used by the empirical calibration).

- ...:

  Additional arguments passed to
  [`HDCD::ESAC()`](https://rdrr.io/pkg/HDCD/man/ESAC.html).

## Value

A `ggcpt` object. The changepoints tibble carries `cusum` (the ESAC
statistic at each detected location) and `depth` (its level in the
recursion).

## References

Moen PAJ, Glad IK, Tveten M (2024). “Efficient sparsity adaptive
changepoint estimation.” *Electronic Journal of Statistics*, **18**(2),
3975–4038. [doi:10.1214/24-EJS2294](https://doi.org/10.1214/24-EJS2294)
.

## Examples

``` r
set.seed(2026)
X <- matrix(rnorm(100 * 20), nrow = 100)
X[51:100, 1:5] <- X[51:100, 1:5] + 3
esac_wrapper(X)
#> ggcpt (changepoint detection result)
#>   Method:         esac
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 1.5 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 4
#>      cp cp_value cusum depth
#>   <int>    <dbl> <dbl> <int>
#> 1    50    0.426  3.79     1
```
