## Submission

This is a minor-version update (0.1.0 -> 0.2.0) of an existing CRAN package.

It adds a unified `ggcpt` S3 result class with `broom`-style
`tidy()`/`glance()`/`augment()` and `ggplot2::autoplot()` methods, a
`cpt_detect()` dispatcher, new wrappers for several optional engines, a method
comparison module, accuracy metrics, and a data simulator. The four functions
from 0.1.0 keep their signatures and behaviour. A confirmed edge-case bug in
`ecp_wrapper()` (spurious boundary changepoints on a no-change series) and the
`ggplot2` `size` -> `linewidth` deprecation are fixed.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 0 notes

(Locally, the only items reported are environment-specific: `qpdf`, a LaTeX
installation, and HTML `tidy` are not available on the test machine. These are
not package issues.)

## Reverse dependencies

There are no reverse dependencies.
