## Submission

This is a minor-version update (0.3.0 -> 0.4.0) of an existing CRAN package.

The release wires eighteen new detection engines behind the unified
`cpt_detect()` dispatcher (SMUCE/HSMUCE via 'stepR', CPOP via 'cpop',
Bayesian detection via 'bcp'/'ocp'/'Rbeast', sequential and kernel methods
via 'cpm'/'kcpRS'/'CptNonPar', robust detection via 'DeCAFS'/'SNSeg',
high-dimensional and multivariate detection via
'InspectChangepoint'/'ocd'/'changepoint.geo', regression breaks via
'strucchange'/'segmented'/'EnvCpt', and 'fastcpd'). All new engines are in
Suggests, guarded with requireNamespace(), and their examples use
@examplesIf so the package checks cleanly without them. New tooling includes
the CROPS penalty path (`cpt_crops()`), batch detection (`cpt_batch()`),
bootstrap stability diagnostics (`cpt_stability()`), Bayesian posterior and
run-length displays, and per-method citations (`cpt_cite()`). Twenty bugs
found by a systematic audit of 0.3.0 were fixed, and a further pre-release
audit fixed eight more (silent column-major flattening of multi-column input
in the ten univariate wrappers added this release; SegNeigh's `Q` clamp on
short series; a multivariate coordinate named `index`; `NA` changepoint
indices; a broken `...` passthrough in `cpt_detect()`; and three enumerated
engine options that could never succeed). Every fix has a regression test.
The 0.1.0-0.3.0 function signatures keep working unchanged.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), R CMD check --as-cran
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macos-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

(Locally, the only items reported are environment-specific: `qpdf`, a LaTeX
installation, and HTML `tidy` are not available on the test machine. These
are not package issues.)

## Reverse dependencies

There are no reverse dependencies.
