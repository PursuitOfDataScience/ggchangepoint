# Pilliat wrapper — high-dimensional detection by three complementary tests

Wraps [`HDCD::Pilliat()`](https://rdrr.io/pkg/HDCD/man/Pilliat.html)
(Pilliat, Carpentier and Verzelen, 2023): a high-dimensional mean-change
procedure combining a dense test, a Berk–Jones test and a partial-sum
test, so it is powerful across sparsity regimes without estimating the
sparsity level. A useful cross-check on
[`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md)
— the two adapt differently and disagreeing answers are informative.

## Usage

``` r
pilliat_wrapper(
  x,
  threshold_d_const = 4,
  threshold_bj_const = 6,
  threshold_partial_const = 4,
  empirical = FALSE,
  N = 100,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame with rows as time points and columns as
  coordinates.

- threshold_d_const, threshold_bj_const, threshold_partial_const:

  Leading constants of the dense, Berk–Jones and partial-sum thresholds.

- empirical:

  Calibrate the thresholds by Monte Carlo rather than using the
  theoretical values? Slower but sharper; defaults to `FALSE`.

- N:

  Monte Carlo samples when `empirical = TRUE`.

- seed:

  Optional seed (used by the empirical calibration).

- ...:

  Additional arguments passed to
  [`HDCD::Pilliat()`](https://rdrr.io/pkg/HDCD/man/Pilliat.html).

## Value

A `ggcpt` object.

## Dimension precondition

`HDCD` 1.1's `Pilliat()` builds one fewer partial-sum threshold than it
uses whenever the number of coordinates is an exact power of two, so the
C routine reads past the end of that vector and the engine reports a
changepoint at *every* observation — on pure noise as readily as on a
real change. This wrapper refuses those dimensions rather than returning
the result, because it is wrong in a way that looks like a finding.
[`esac_wrapper()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/esac_wrapper.md)
is unaffected at every dimension. Note that constant coordinates are
dropped before the count, so 9 coordinates one of which is constant is 8
for this purpose.

## References

Pilliat E, Carpentier A, Verzelen N (2023). “Optimal multiple
change-point detection for high-dimensional data.” *Electronic Journal
of Statistics*, **17**(1), 1240–1315.
[doi:10.1214/23-EJS2126](https://doi.org/10.1214/23-EJS2126) .

## Examples

``` r
set.seed(2026)
X <- matrix(rnorm(100 * 20), nrow = 100)
X[51:100, 1:5] <- X[51:100, 1:5] + 3
pilliat_wrapper(X)
#> ggcpt (changepoint detection result)
#>   Method:         pilliat
#>   Change in:       mean 
#>   Changepoints found: 1 
#>   CP convention:   left 
#>   Penalty:         threshold = 4 
#>   Series length:   100 
#> 
#> Changepoints:
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    50    0.426
```
