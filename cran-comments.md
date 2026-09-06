## Submission

This is a minor-version update (0.4.0 -> 0.5.0) of an existing CRAN package.

Where 0.4.0 was an engine wave, 0.5.0 is mostly a surface release: nineteen
further detection engines are wired behind `cpt_detect()` (taking it from 31
to 50 methods), but the larger part of the work is the machinery around
them.

**New capabilities.** Narrowest Significance Pursuit ('nsp') returns
significance *regions* rather than point estimates, which needed a new
optional `regions` slot on the result class, a new `geom_cpt_region()`
layer, and a `cpt_confint()` generic that unifies four provenances (the
engine's own interval, a posterior credible interval, a within-segment
bootstrap, and NSP regions) behind one contract with a `source` column.
`cpt_test()` reports a `selection_adjusted` flag so an unadjusted
two-sample p-value can never be mistaken for a selection-adjusted one.
`cpt_select()` chooses the number of changepoints by any of six criteria,
including the Zhang-Siegmund segment-length mBIC and order-preserved
cross-validation. `cpt_influence()`, `cpt_sensitivity()`, `cpt_statistic()`,
`cpt_solution_path()` and `cpt_scale_space()` expose what a detector
computed rather than only its argmax. Supervised detection arrives with
`cpt_labels()`, `cpt_label_error()` and `cpt_learn_penalty()`. Series may
now carry a time index ('ts'/'xts'/'zoo'/'tsibble' input and a data-frame
interface), and there are new facilities for consensus, method
recommendation, event annotation, reproducible reports, benchmarking,
sequential monitoring with detection-delay accounting, and power analysis.

**Extension mechanism.** `as_ggcpt()` and `cpt_register_method()` let a
detector this package does not wrap join the same tidy, plottable grammar.
Registered methods are visibly user-supplied: `cpt_methods()` marks them,
`print()` says so on every result, and `cpt_cite()` returns the citation the
registration supplied or states plainly that none was given.

**Dependencies.** Fifty-six packages are suggested; thirty-five of them are
detection engines and the rest are optional extras (time-index coercion,
tables, interactivity, progress bars, the test toolchain). One of them, 'mcp', needs JAGS -- a system library -- so a machine
without JAGS must check with `_R_CHECK_FORCE_SUGGESTS_=false`. 'mcp' is on
CRAN and checks there; `mcp_wrapper()` names JAGS in its error when 'mcp' is
absent, and its example is wrapped in `\dontrun{}` rather than gated with
`@examplesIf`, deliberately: whether the engine works depends on a *system*
library, and `requireNamespace("mcp")` does not predict that -- 'rjags' can
be installed and still fail to find JAGS at run time. The example therefore
never runs anywhere, which is the only guard that holds. Every one is guarded with `requireNamespace()`, its examples use
`@examplesIf`, its tests use `skip_if_not_installed()`, and the vignettes
gate the chunks that need it, so the package checks cleanly with none of
them installed (see below). Only 'changepoint', 'changepoint.np' and 'ecp'
are required. `cpt_install_engines()` installs a family at a time for users
who want more.

**One original implementation, labelled as such.** Every detector in this
package wraps a separately maintained one, with a single deliberate
exception: `cpt_monitor(method = "edetector")` implements the mixture
Shiryaev-Roberts e-detector of Shin, Ramdas and Rinaldo (2023), which has no
R implementation. It is a dozen lines, optional stopping on the martingale
`M_t - t` gives it a finite-sample lower bound of `1 / alpha` on the
in-control average run length, a test measures the realised in-control alarm
rate against that bound, and it is identified as native in `print()`, in
`?cpt_monitor`, in `cpt_cite("edetector")` and in NEWS.md.

**Network access.** `cpt_load_tcpd()` downloads the Turing Change Point
Dataset and caches it under `tools::R_user_dir()`. Nothing is bundled or
redistributed, the function is not called by any example, test or vignette
(its examples are `\dontrun{}`), and every benchmark example runs offline
against `cpt_datasets()`, which is built from the package's own simulated
signals.

Forty-one defects found while building and auditing this release were fixed in
the same cycle; NEWS.md itemises them and each has a regression test. The
most instructive: a `tibble::tribble()` list-column silently deparsed into a
string, which made every multi-capability method lose its extra `change_in`
values; `$` on a ggplot2 mapping partially matching `xintercept` when asked
for `x`, which left one new layer with no x aesthetic; an upstream column
that is a p-value or a logical depending on an argument, which made one
wrapper return every candidate split it had considered; and -- the one
worth singling out -- the native e-detector combined its per-shift
statistics with a maximum rather than an average, which broke the very
average-run-length bound the method is chosen for. The test for it now
measures the realised in-control alarm rate.

**One engine is refused at some dimensions, deliberately.** `HDCD` 1.1's
`Pilliat()` builds one fewer partial-sum threshold than it indexes when the
number of coordinates is an exact power of two, and reports a changepoint at
every observation as a result -- on pure noise as readily as on a real
change, at p = 2, 4, 8, 16, 32, 64 and 128. `pilliat_wrapper()` therefore
stops with an explanation at those dimensions instead of returning the
engine's answer, points at `esac` (the other `HDCD` method, unaffected at
every dimension), and lifts the restriction automatically for any `HDCD`
newer than 1.1. This will be reported upstream.

The 0.1.0-0.4.0 function signatures keep working unchanged.

## Test environments

* local: R 4.4.1 on Rocky/RHEL 8 (x86_64), R CMD check --as-cran
* local: R 4.6.0 on Rocky/RHEL 8 (x86_64), against a library holding the
  Imports and none of the Suggests
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macos-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes on ubuntu-latest (devel, release and
oldrel-1) and on windows-latest; macos-latest reports one note, which is
the runner's and not the package's (see below).

**The installed-size note that 0.4.0 carried is much reduced, but the
package still sits on the threshold.** The seven package vignettes now
render their figures at `dpi = 72` rather than rmarkdown's default 96, which
takes the source tarball from 5.2 MB to 4.2 MB and cuts the installed `doc`
directory by about a quarter. `html_vignette` displays the figures at their
natural size, so this removes pixels rather than shrinking the pictures.

The installed size is nevertheless within a rounding error of the 5.0 MB
that triggers the note, and we would rather say so than have it come as a
surprise: repeated `--as-cran` runs on the same unchanged sources report
`checking installed package size ... OK` and `... NOTE   installed size is
5.1Mb` in roughly equal measure. When it does fire, the breakdown is `doc`
3.8 MB and `help` 2.0 MB. The `doc` half is 65 vignette figures already at
`dpi = 72`, the largest 96 KB, with no outlier and nothing unreferenced; the
`help` half is 0.5 MB of Rd database and 1.1 MB of `man/figures`, all of it
reachable from `README.md`. We can find nothing further to remove that is
not documentation the package deliberately ships. The `vdiffr` snapshots are test
fixtures, excluded from the build, and were never part of the installed
package.

The one macOS note is `checking dependencies in R code`, and its entire
body is a failure to `dlopen` 'rgl':

```
unable to load shared object '.../rgl/libs/rgl.so':
  Library not loaded: /opt/X11/lib/libGLU.1.dylib
```

'rgl' is not a dependency of this package. It arrives three levels down from
the suggested engine 'fabisearch', which imports 'NMF' and 'plot3D';
'plot3D' reaches 'misc3d', and 'misc3d' imports 'rgl' for interactive 3-D
rendering that neither of the other two needs. The macOS runner has no
XQuartz, so `libGLU` is absent and the namespace cannot load when the check
step tries to. Every other macOS check line, including the examples,
`--run-donttest`, the tests and the vignette rebuild, is OK. Nothing in this
package loads 'rgl': `cpt_methods()` answers "is this engine installed?"
with `find.package()` rather than `requireNamespace()` precisely so that no
engine namespace is loaded to build a table, and `need_pkg()` -- the single
point at which a wrapper does load its engine -- suppresses load-time
warnings about the machine so a headless user is not told about their
display.

Depending on when this is submitted, the incoming-feasibility check may add
a "days since last update" note; 0.4.0 was published on 2026-08-24.

Two further items appear on the local machines and will not appear on yours,
noted here so the difference is not a surprise:

* `qpdf` is not installed on the R 4.4.1 machine, so `R CMD check --as-cran`
  reports that it cannot run its PDF size-reduction check. That is the
  machine, not the package.
* the same machine reports 'mcp' and 'rjags' as suggested-but-not-available,
  because both need JAGS -- a system library -- and it is not installed
  there. Both are on CRAN and check there; locally the run uses
  `_R_CHECK_FORCE_SUGGESTS_=false`. 'rjags' is suggested because
  `mcp_wrapper()` tests whether it can load: having 'mcp' installed does not
  imply JAGS can be reached, and the wrapper reports that rather than
  failing inside the engine.

The no-Suggests run is a real one: the R 4.6.0 library above holds the
Imports and not one of the suggested engines. It reports 0 errors,
0 warnings and only the incoming-feasibility note, with the test suite
reporting 0 failures and skipping what it cannot run.

## Suggested methods and their references

The methods themselves are not implemented here (apart from the one
exception noted above); each comes from the package named beside it, and
this package supplies the interface. Every reference is on the help page of
the wrapper that calls it, via Rdpack and inst/REFERENCES.bib, and
`cpt_cite()` returns the citation for any result. A test asserts that every
method `cpt_methods()` reports as available has a `cpt_cite()` entry, and
another asserts that every `\insertRef` key in R/ resolves in
inst/REFERENCES.bib and agrees with the vignette bibliography.

## Reverse dependencies

There are no reverse dependencies.
