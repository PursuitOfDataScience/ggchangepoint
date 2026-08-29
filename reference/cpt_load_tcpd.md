# Download and cache the Turing Change Point Dataset

Fetches the benchmark of van den Burg and Williams (2020) — real series
from many domains, each annotated independently by several human
annotators — and caches it under
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). The
multi-annotator structure is the point: scoring against a single "true"
set silently discards the disagreement, and
[`cpt_metrics_annotated()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_metrics_annotated.md)
is built to keep it.

## Usage

``` r
cpt_load_tcpd(name = NULL, cache_dir = NULL, refresh = FALSE, quiet = FALSE)
```

## Arguments

- name:

  Dataset name, a character vector of names, or `NULL` (the default) for
  the whole catalogue.

- cache_dir:

  Where to cache. Defaults to
  `tools::R_user_dir("ggchangepoint", "cache")`.

- refresh:

  Re-download even when a cached copy exists? Defaults to `FALSE`.

- quiet:

  Suppress progress messages. Defaults to `FALSE`.

## Value

With `name = NULL`, a tibble catalogue (`name`, `n_obs`, `n_dim`,
`n_annotators`, `cached`). Otherwise a named list of
`list(series, annotations, index, longname)` datasets, in the shape
[`cpt_benchmark()`](https://pursuitofdatascience.github.io/ggchangepoint/reference/cpt_benchmark.md)
takes.

## Network access and licences

This function downloads from GitHub, so it needs a working connection
and is skipped in every example and test here. The data belong to their
original owners under their own licences; this package neither bundles
nor redistributes them. A few TCPD series are not in the repository at
all (their sources do not permit redistribution) and are reported as
unavailable rather than silently omitted.

## References

van den Burg GJJ, Williams CKI (2020). “An evaluation of change point
detection algorithms.” *arXiv preprint arXiv:2003.06222*.
[doi:10.48550/arXiv.2003.06222](https://doi.org/10.48550/arXiv.2003.06222)
.

## Examples

``` r
if (FALSE) { # \dontrun{
cpt_load_tcpd()                       # the catalogue
d <- cpt_load_tcpd("nile")            # one dataset
cpt_benchmark(d, methods = c("pelt", "amoc"))
} # }
```
