# Bayesian online changepoint detection wrapper (BOCPD)

Wraps [`ocp::onlineCPD()`](https://rdrr.io/pkg/ocp/man/onlineCPD.html),
an implementation of Bayesian Online Changepoint Detection (Adams and
MacKay, 2007). BOCPD recursively updates a posterior over the current
*run length* (time since the last change); the maximum a posteriori set
of changepoints is reported, and the full run-length posterior is kept
so that
[`ggcpt_runlength()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/ggcpt_runlength.md)
can draw the signature run-length heatmap.

## Usage

``` r
bocpd_wrapper(x, hazard = 100, ...)
```

## Arguments

- x:

  A numeric vector.

- hazard:

  Constant hazard rate \\1/\lambda\\ of the change process; larger
  `hazard` values mean changes are expected less often. Defaults to
  `100` (the upstream default).

- ...:

  Additional arguments passed to
  [`ocp::onlineCPD()`](https://rdrr.io/pkg/ocp/man/onlineCPD.html).

## Value

A `ggcpt` object with the MAP changepoint set. The full `ocp` fit
(including the run-length posterior) is kept in `$fit`.

## References

Adams RP, MacKay DJ (2007). “Bayesian online changepoint detection.”
*arXiv preprint arXiv:0710.3742*.

## Examples

``` r
res <- bocpd_wrapper(c(rnorm(60), rnorm(60, 4)))
res$changepoints
#> # A tibble: 1 × 2
#>      cp cp_value
#>   <int>    <dbl>
#> 1    60     1.10
ggcpt_runlength(res)
```
