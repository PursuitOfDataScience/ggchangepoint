# Annotate segments with alternating shading

Adds alternating shaded rectangles to highlight segments between
changepoints.

## Usage

``` r
annotate_segments(cp, n, fill = c("grey90", "white"), alpha = 0.5, ...)
```

## Arguments

- cp:

  Changepoint indices (including 0 and n).

- n:

  Length of the series.

- fill:

  Colors for alternating segments. Defaults to c("grey90", "white").

- alpha:

  Alpha for fill. Defaults to 0.5.

- ...:

  Additional arguments passed to `annotate`.

## Value

A list of ggplot annotations.
