## Submission

This is a minor-version update (0.5.0 -> 0.7.0) of an existing CRAN package.
0.6.0 was tagged on GitHub and never submitted, so its changes arrive with
this release; NEWS.md keeps the two sections apart.

0.7.0 is an inference release. Around a detected changepoint it adds the
questions applied users ask next: which coordinates of a multivariate
series changed (`cpt_attribute()`, with family-wise error or false
discovery rate control), a distribution-free confidence set for each
location (`cpt_confint(method = "conformal")`), what the series would have
done without the change (`cpt_counterfactual()`), whether the change was
abrupt (`cpt_abruptness()`), which of many series changed (`cpt_screen()`),
and weights over the number of changes (`cpt_k_weights()`). It adds CUSUM
and EWMA control charts to `cpt_monitor()`, implemented here because they
are short and textbook, with limits calibrated by the standard Markov-chain
approximation, and one engine, 'anomaly' (CAPA), for changes that return
to a baseline. Every function that makes a statistical promise ships with a
measurement of it in the `cpt_calibration` data set.

**Changes to results** are listed first in NEWS.md: `cpt_confint(method =
"auto")` now prefers the conformal set to the bootstrap where it applies.
Every argument of 0.5.0 and 0.6.0 keeps its name and position, which a test
checks against fixtures of both.

One engine route uses a function its package documents as deprecated:
'stepR' fits Poisson and binomial SMUCE only through `smuceR()`, which
'stepR' describes as deprecated but working. The wrapper checks for the
function and, if it is ever removed, refuses with a classed error that
names the other methods fitting the same family.

Two suggested engines are no longer on CRAN, and both are declared in
`Additional_repositories`: 'fpop' (archived 2026-09-14 at its maintainer's
request, and still built on R-Forge, where it is developed) and 'ocp'
(archived 2026-10-12). The second repository listed there is a CRAN-like
repository on the package website that serves the last CRAN release of
each, so both stay installable. Every use of either is conditional on
`requireNamespace()`, and the tests and examples that need them skip
without them.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), `R CMD check --as-cran`, with
  every suggested engine installed except 'mcp' and 'rjags' (JAGS is not
  available there)
* local: R 4.6.0 on Rocky/RHEL 8 (x86_64), `R CMD check --as-cran`, against
  a library holding the Imports and none of the Suggests
* GitHub Actions: Linux (R release and devel), macOS and Windows (R
  release), with the suggested engines

## R CMD check results

* R 4.6.0, no Suggests: Status OK (0 errors, 0 warnings, 0 notes).
* R 4.4.1: 0 errors, 1 warning, 2 notes, all three about the local
  machine or already familiar:
  * WARNING: `qpdf` is not installed on this machine, so the PDF
    size-reduction check cannot run.
  * NOTE: 'mcp' and 'rjags' are suggested but not available here, because
    both need JAGS. Both are on CRAN; every use is guarded with
    `requireNamespace()`, and the local run uses
    `_R_CHECK_FORCE_SUGGESTS_=false`.
  * NOTE: installed size 5.6 MB, of which `doc` is 4.0 MB: the seven
    vignettes, whose figures are rendered at `dpi = 72`. Most of the new
    material for this release is on the package website only, as pkgdown
    articles excluded from the build.

The incoming-feasibility check may add a "days since last update" note:
0.5.0 was published in September 2026.

The test suite runs in 44 seconds under CRAN conditions (26 on R 4.6.0);
tests that take longer than a fraction of a second, and every test that
runs 'stepR', skip on CRAN and run on every CI push. The whole suite with
every engine installed and `NOT_CRAN=true` reports 8,834 passing
expectations and no failures.

## Suggested methods and their references

The detection methods themselves are not implemented here, apart from the
monitors in `cpt_monitor()` that the package implements and labels as its
own (the e-detector, the CUSUM and the EWMA), and the two inference
procedures added in this release, ARM attribution (Peng et al. 2026,
arXiv:2608.01691) and conformal changepoint localisation (Hore and Ramdas
2026, arXiv:2602.06267), each a permutation procedure computed in base R.
Every other method comes from the package named beside it, and this
package supplies the interface. Every reference is on the help page of the
function that uses it, via Rdpack and inst/REFERENCES.bib, and `cpt_cite()`
returns the citation for any result. A test asserts that every method
`cpt_methods()` reports as available has a `cpt_cite()` entry, and another
asserts that every `\insertRef` key in R/ resolves in inst/REFERENCES.bib
and agrees with the vignette bibliography.

## Reverse dependencies

There are no reverse dependencies.
