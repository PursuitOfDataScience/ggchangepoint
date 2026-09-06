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
summarises. When an engine supplies its own fitted signal that signal is
used for `.fitted` in place of the segment means, and rides along in a
`fitted` column of its own – so for those engines the two columns agree.
The engines that do this are exactly the ones
[`cpt_methods()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_methods.md)
marks in its `fitted` column: `smuce`, `hsmuce`, `cpop`, `bcp`, `beast`,
`decafs`, `segmented`, `mcp` and `bfast`.
