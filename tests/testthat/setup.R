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
