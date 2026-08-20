## Submission

This is a minor-version update (0.3.0 -> 0.4.0) of an existing CRAN package.

The release wires eighteen new detection engines behind the unified
`cpt_detect()` dispatcher (SMUCE/HSMUCE via 'stepR', CPOP via 'cpop',
Bayesian detection via 'bcp'/'ocp'/'Rbeast', sequential and kernel methods
via 'cpm'/'kcpRS'/'CptNonPar', robust detection via 'DeCAFS'/'SNSeg',
high-dimensional and multivariate detection via
'InspectChangepoint'/'ocd'/'changepoint.geo', regression breaks via
'strucchange'/'segmented'/'EnvCpt', and 'fastcpd'). Twenty-eight of the
thirty-one engines live in Suggests: every one is guarded with
requireNamespace(), its examples use @examplesIf, its tests use
skip_if_not_installed(), and the vignettes gate the chunks that need it, so
the package checks cleanly with none of them installed (see below). New
tooling includes the CROPS penalty path (`cpt_crops()`), batch detection
(`cpt_batch()`), bootstrap stability diagnostics (`cpt_stability()`),
Bayesian posterior and run-length displays, and per-method citations
(`cpt_cite()`).

Three successive audits preceded this submission and NEWS.md itemises
every fix; each one has a regression test. The most substantive from the
final pass: `cpt_stability()` reported an inflated re-detection frequency
(it counted changepoints rather than replicates, then clipped the
overflow, so an index only half the replicates covered was shown as 1.00);
`ggcpt_interactive()` failed for every multivariate result because the
faceted plot used a column name 'plotly' also generates; `glance()`
returned zero rows instead of one for a hand-built result; `$fit` was NULL
for the five engines backed by 'changepoint' and 'changepoint.np',
although it is documented to hold the upstream object; and the covering
metric was quadratic in the number of changepoints. A redundant
`.onLoad()` that re-registered S3 methods into the method tables of 'base'
and 'generics' was removed, after verifying that the NAMESPACE
declarations alone suffice.

The documentation gained one note worth singling out: `?cpt_detect` now
records that 'changepoint's Normal cost and 'fpop's lambda are compared
against a raw segment cost for a change in mean, so those four engines
over-segment badly on data whose noise is much wider than 1 unless the
series is standardised. Behaviour is unchanged; the trap was simply
undocumented.

The 0.1.0-0.3.0 function signatures keep working unchanged.

The methods themselves are not implemented here: each is provided by the
package named alongside it above, and this package supplies the interface.
The reference for every method is therefore attached to the wrapper that
calls it, in the \references section of its help page (via Rdpack and
inst/REFERENCES.bib), rather than collected in the Description field, and
`cpt_cite()` returns the citation for any result. Please let me know if you
would prefer a selection of them in DESCRIPTION as well.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), R CMD check --as-cran
* local: R 4.6.0 on Rocky/RHEL 8 (x86_64), against a library holding the
  Imports and none of the Suggests
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macos-latest (release), windows-latest (release)

The declared 'ggplot2 (>= 3.4.0)' floor was exercised at both ends: the test
suite passes unchanged against 'ggplot2' 4.0.3 and against 3.5.2, the last of
the 3.x series. The suite also passes under LC_ALL=C.

## R CMD check results

0 errors | 0 warnings | 0 notes

The no-Suggests run is a real one: the R 4.6.0 library above contains the
eleven Imports and not one of the twenty-six suggested engines. It is also
0 errors | 0 warnings | 0 notes, with the test suite reporting 0 failures
and skipping what it cannot run.

(Under R 4.4.1 the only item reported is environment-specific: `qpdf` is not
available on that machine, so `R CMD check --as-cran` cannot run its PDF
size-reduction check. That is not a package issue.)

## Reverse dependencies

There are no reverse dependencies.
