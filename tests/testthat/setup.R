# Test-suite setup.
#
# Which tests CRAN runs. CRAN allows a package's whole check about ten
# minutes, and this suite alone took 587s of the 17 minutes win-builder spent
# on the first 0.5.0 submission. So every test that took 0.2s or more in a
# timed run under CRAN conditions starts with skip_on_cran(): 201 tests, most
# of them engine sweeps and the audit regressions, leaving about 250 fast
# ones for CRAN. They still run wherever NOT_CRAN is "true", which is every
# CI runner (r-lib/actions sets it) and devtools::test() or devtools::check().
# A plain `R CMD check` skips them, so export NOT_CRAN=true for a full local
# run. A new test that is slow should start with skip_on_cran() too.
#
# "CRAN conditions" has to include a cold stepR cache, or the timing lies.
# stepR keeps the Monte Carlo critical values behind smuce and hsmuce in
# R.cache, which R.cache moves to a temporary directory whenever it detects
# R CMD check, so every check simulates them afresh. Once tcltk is loaded
# (mosum pulls it in through plot3D and misc3d) a fresh simulation is also
# about six times slower on Linux: 32s instead of 5s for one n = 200 series.
# A warm cache hid all of that, so every test that runs either engine is
# skipped on CRAN whatever its measured time. Time new tests with
# R_CMD_CHECK=true set, which is how R.cache recognises a check.
#
# fabisearch imports rgl, which prints two warnings ("unable to open X11
# display", "'rgl.init' failed") the moment its namespace loads on a headless
# machine -- which every check machine is. The option is rgl's own documented
# way to say "no window needed"; setting it here keeps the suite's output
# about the package rather than about the display.
old_rgl <- getOption("rgl.useNULL")
options(rgl.useNULL = TRUE)
withr::defer(options(rgl.useNULL = old_rgl), teardown_env())

# plot() methods draw as a side effect, so a test that calls one opens a
# device. Without a null device R starts the default pdf() and leaves an
# Rplots.pdf behind in the test directory; this keeps the suite from
# littering the check output.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), teardown_env())

# skip_if_not_installed() loads a package to answer the question, so the
# first mosum test pulls tcltk (through plot3D and misc3d) into the session
# and tcltk's load hook warns about the missing DISPLAY before any package
# code runs. Getting that load out of the way here keeps a warning about the
# machine out of the suite's report; need_pkg() does the same for users.
if (length(find.package("mosum", quiet = TRUE)) > 0) {
  suppressWarnings(requireNamespace("mosum", quietly = TRUE))
}

# `engine_installed()` asks whether a package is on disk, with
# find.package(). That is the right question for the package's own code,
# which uses it to tell a user what to install -- but it is the wrong
# question for a test that is about to *run* an engine, because a package
# can be installed and still not load.
#
# The macOS CI runner is exactly that case: {mosum} is installed, and
# loading it fails because rgl cannot find libGLU.1.dylib. Nine tests
# already skipped there via skip_if_not_installed(), which tries the load;
# the change_in contract sweep guarded with engine_installed() instead, so
# it went ahead and asserted that mosum honours change_in = "mean" on a
# machine where mosum cannot be loaded at all. One red job, no defect in
# the package.
#
# So the suite gets a predicate for the question it actually asks. This is
# requireNamespace() used deliberately for what it does answer -- "can this
# be loaded here" -- rather than as a stand-in for "is it installed".
engine_usable <- function(pkg) {
  if (!ggchangepoint:::engine_installed(pkg)) {
    return(FALSE)
  }
  isTRUE(suppressWarnings(suppressMessages(
    requireNamespace(pkg, quietly = TRUE)
  )))
}

# The package sources are present when the suite runs from a checkout and
# ABSENT under `R CMD check`, which unpacks the tarball into
# `<pkg>.Rcheck/tests/` and leaves `R/` behind. Every test that reads a
# source file therefore has to say so -- three did not, and failed on all
# five CI runners with base R's `cannot open the connection`, which names
# neither the file nor the reason. `NAMESPACE` and `DESCRIPTION` are the
# exception: they ship, so read those through system.file() and they work
# in both places.
pkg_source_root <- function() {
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(root, "R"))) root else NA_character_
}

skip_if_no_sources <- function() {
  if (is.na(pkg_source_root())) {
    skip("package sources are not available (R CMD check runs from the tarball)")
  }
}

pkg_source_lines <- function(...) {
  root <- pkg_source_root()
  if (is.na(root)) skip("package sources are not available")
  readLines(file.path(root, ...), warn = FALSE)
}
