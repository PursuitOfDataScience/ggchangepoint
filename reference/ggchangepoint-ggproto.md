# ggproto objects for the changepoint layers

The layers are real ggplot2 extensions: each `geom_*()` is a layer built
on one of these, with its own default aesthetics, required aesthetics
and legend glyph, so they take part in scales, guides and the position
system like any geom, and can be extended with
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html).
Until 0.6.0 every layer was a thin wrapper around a stock geom and
borrowed its glyph.

- `GeomChangepoint`:

  a vertical rule at each `xintercept`
  ([`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md));
  key: a vertical rule.

- `GeomCptSegment`:

  a segment level
  ([`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md)).

- `GeomCptCi`:

  a horizontal interval with caps
  ([`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md));
  key: the same interval.

- `GeomCptRegion`:

  a band from `xmin` to `xmax`, full height unless `ymin`/`ymax` are
  mapped
  ([`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md));
  key: a shaded band.

- `GeomCptLabel`:

  a labelled interval behind the series
  ([`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md)).

- `GeomCptEvent`:

  a dotted rule with its label
  ([`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md));
  key: a dotted rule with a flag.

- `StatChangepoint`, `StatCptRegion`:

  detection inside the layer
  ([`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md),
  [`stat_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_cpt_region.md)).

## Format

ggproto objects.

## See also

Other ggplot2 layers:
[`geom_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_changepoint.md),
[`geom_cpt_ci()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_ci.md),
[`geom_cpt_event()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_event.md),
[`geom_cpt_label()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_label.md),
[`geom_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_region.md),
[`geom_cpt_segment()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/geom_cpt_segment.md),
[`stat_changepoint()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_changepoint.md),
[`stat_cpt_region()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/stat_cpt_region.md)

## Examples

``` r
library(ggplot2)
class(GeomChangepoint)
#> [1] "GeomChangepoint" "GeomVline"       "Geom"            "ggproto"        
#> [5] "gg"             
GeomCptRegion$default_aes
#> Aesthetic mapping: 
#> * `colour`    -> NA
#> * `fill`      -> "steelblue"
#> * `linewidth` -> 0.5
#> * `linetype`  -> 1
#> * `alpha`     -> 0.2
```
