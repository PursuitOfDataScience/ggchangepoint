# Augment a ggcpt object

Returns the original data with added columns: `seg_id`, `.fitted`,
`.resid`, and `is_changepoint`.

## Usage

``` r
# S3 method for class 'ggcpt'
augment(x, ...)
```

## Arguments

- x:

  A `ggcpt` object.

- ...:

  Additional arguments (ignored).

## Value

A tibble with the original data plus augment columns.

## Details

For a multivariate result every coordinate is returned, but the
changepoints are shared across them, so `seg_id` and `is_changepoint`
apply to the whole row while `.fitted` and `.resid` describe the *first*
coordinate only — the same coordinate `$segments$param_estimate`
summarises. When an engine supplies its own fitted signal (SMUCE,
DeCAFS, cpop, segmented, bcp, beast) that signal is used for `.fitted`
in place of the segment means.
