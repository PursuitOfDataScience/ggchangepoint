# Test-suite setup.
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
