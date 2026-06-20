# Blocks test signal

The classic Donoho-Johnstone blocks test signal with known changepoints.

## Usage

``` r
signal_blocks(n = 2048, seed = NULL)
```

## Arguments

- n:

  Length of the signal. Defaults to 2048.

- seed:

  Optional seed.

## Value

A tibble with columns `index` and `value`. The `true_changepoints`
attribute contains the known changepoint locations.

## References

Donoho, D. L. and Johnstone, I. M. (1994). Ideal spatial adaptation by
wavelet shrinkage. *Biometrika*, 81(3), 425-455.
