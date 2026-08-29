# Colour scales for changepoint labels and label errors

A colour-vision-deficiency-safe fill scale covering both vocabularies
the supervised-detection displays use: the label's assertion (`change` /
`no_change` / `one_change`) and the outcome of scoring a segmentation
against it (`correct` / `false_positive` / `false_negative`). Unknown
values fall back to grey rather than erroring, so a partially-scored
frame still plots.

## Usage

``` r
scale_fill_cpt_label(..., na.value = "grey70")

scale_colour_cpt_label(..., na.value = "grey70")
```

## Arguments

- ...:

  Passed to
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  /
  [`ggplot2::scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

- na.value:

  Fill for values outside the vocabulary.

## Value

A ggplot2 scale.
