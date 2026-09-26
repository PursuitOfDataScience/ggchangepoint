# 0.6.0: the changepoint convention, verified rather than asserted.
#
# `ggcpt_build()` records every result as `cp_convention = "left"`: a
# changepoint is the last observation of the segment before the change.
# Fifty engines feed it, each with its own upstream convention, and an
# off-by-one in any wrapper is the ideal silent bug: it never errors, the
# plot looks right, the segments still tile, and the location is wrong by
# one, which matters exactly when the index is a date and the change is
# being attributed to an event. This found one: EnvCpt's autoregressive
# changepoint models report rows of a lagged design, one or two positions
# early.

test_that("every engine reports an unmistakable change at the same index", {
  skip_on_cran()
  set.seed(82)
  n <- 120L
  k <- 60L
  jump <- function(sign = 1) {
    c(rep(0, k), rep(10 * sign, n - k)) + stats::rnorm(n, 0, 0.1)
  }
  step <- jump()
  X <- cbind(a = jump(), b = jump(-1), c = jump())
  reg <- ggchangepoint:::builtin_registry()
  # Engines that need data of another kind (graphs, a regression response,
  # a VAR process) or cannot run here, and the two slope engines, which
  # model a step as a pair of kinks and are checked within one position
  # below.
  special <- c("network", "fabisearch", "hdreg", "var", "mcp")
  slope <- c("cpop", "segmented")
  offenders <- character(0)
  assessed <- character(0)
  for (i in seq_len(nrow(reg))) {
    m <- reg$method[i]
    if (m %in% special || !engine_usable(reg$engine[i])) next
    x <- if (reg$univariate[i]) step else X
    args <- list(x, method = m)
    wrapper <- get(reg$wrapper[i], asNamespace("ggchangepoint"))
    if ("seed" %in% names(formals(wrapper))) args$seed <- 1
    res <- tryCatch(suppressWarnings(suppressMessages(
      utils::capture.output(fit <- do.call(cpt_detect, args)))),
      error = function(e) NULL)
    if (is.null(res)) next
    cps <- fit$changepoints$cp
    near <- cps[abs(cps - k) <= 3]
    if (!length(near)) next
    assessed <- c(assessed, m)
    tol <- if (m %in% slope) 1L else 0L
    if (!any(abs(near - k) <= tol)) {
      offenders <- c(offenders, paste0(m, " reported ",
                                       paste(near, collapse = "/"),
                                       " for a change after ", k))
    }
  }
  # The sweep must have assessed a real share of the engines, or it passes
  # for the wrong reason.
  expect_gt(length(assessed), 10L)
  expect_equal(offenders, character(0))
})

test_that("the upstream conventions are recorded for every engine", {
  reg <- ggchangepoint:::builtin_registry()
  expect_true(all(reg$cp_convention_upstream %in%
                    c("left", "right", "continuous", "design")))
  tab <- cpt_methods()
  expect_true("cp_convention_upstream" %in% names(tab))
})
