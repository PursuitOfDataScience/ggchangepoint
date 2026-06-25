## Submission

This is a minor-version update (0.2.0 -> 0.3.0) of an existing CRAN package.

It closes the documentation-coverage gap from the previous cycle (every export is
now introduced in the README and a feature-tour vignette, and the package-level
help describes the current engine list), makes the `cpt_detect()` dispatcher
honest (it advertises only wired methods and adds a `cpt_methods()` introspection
helper), completes the `ggcpt` S3 surface (`summary()`, `as_tibble()`,
`as.data.frame()`, `format()`, `plot()`), and fixes a set of correctness issues
found by testing (a corrupted `signal_blocks()` test signal, `cpt_metrics()`
recall/F1 exceeding 1, `ecp_wrapper()` returning a wrong value on multivariate
input, and `stat_changepoint()` ignoring the x aesthetic). The 0.1.0/0.2.0
function signatures and behaviour are unchanged.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 0 notes

(Locally, the only items reported are environment-specific: `qpdf`, a LaTeX
installation, and HTML `tidy` are not available on the test machine. These are
not package issues.)

## Reverse dependencies

There are no reverse dependencies.
