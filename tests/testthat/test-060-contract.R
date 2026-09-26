# 0.6.0: the contract, checked mechanically before it is frozen.

test_that("every export is called somewhere in the suite", {
  skip_on_cran()
  skip_if_no_sources()
  # §141 measured three exported layers that no test had ever called: the
  # suite reached them only through autoplot(). "Documented" and "executed"
  # are different properties; this checks the second, statically: every
  # exported function must appear as a call in some test file, or be on
  # the short list of those that cannot run here, with the reason.
  ns <- readLines(system.file("NAMESPACE", package = "ggchangepoint"),
                  warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns,
                                                  value = TRUE))
  exports <- gsub('^"|"$', "", exports)
  cannot_run <- c(
    cpt_load_tcpd = "needs the network",
    mcp_wrapper = "needs JAGS, which is not installable everywhere"
  )
  generics <- c("tidy", "glance", "augment", "autoplot", "as_tibble")
  root <- pkg_source_root()
  tests <- list.files(file.path(root, "tests", "testthat"), "\\.R$",
                      full.names = TRUE)
  txt <- paste(vapply(tests, function(f) {
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  fns <- exports[vapply(exports, function(e) {
    is.function(get0(e, envir = asNamespace("ggchangepoint")))
  }, logical(1))]
  uncalled <- fns[!vapply(fns, function(e) {
    grepl(paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", e, fixed = TRUE),
                 "\\("), txt)
  }, logical(1))]
  uncalled <- setdiff(uncalled, c(names(cannot_run), generics))
  expect_equal(uncalled, character(0))
})

test_that("the measured invariances still hold for the fast engines", {
  skip_on_cran()
  # §184.5: three genuine invariances, each with exemptions that are
  # registry facts rather than escape hatches. The exemptions here come
  # from the shipped measurement (cpt_invariances), so this also checks the
  # table against the engines' current behaviour.
  inv <- cpt_invariances
  engines <- c("pelt", "binseg", "wbs", "not", "mosum", "fpop", "idetect",
               "tguh", "binsegrcpp", "fastcpd", "amoc")
  set.seed(184)
  x <- c(stats::rnorm(70), stats::rnorm(70, 3), stats::rnorm(60))
  n <- length(x)
  reg <- ggchangepoint:::builtin_registry()
  for (m in engines) {
    if (!engine_usable(reg$engine[reg$method == m])) next
    run <- function(v) {
      args <- list(v, method = m)
      w <- get(reg$wrapper[reg$method == m], asNamespace("ggchangepoint"))
      if ("seed" %in% names(formals(w))) args$seed <- 1
      suppressWarnings(do.call(cpt_detect, args))$changepoints$cp
    }
    base <- run(x)
    row <- inv[inv$method == m, , drop = FALSE]
    if (!nrow(row)) next
    if (isTRUE(row$shift_invariant)) {
      expect_equal(run(x + 100), base, info = paste(m, "shift"))
    }
    if (isTRUE(row$scale_invariant)) {
      expect_equal(run(10 * x), base, info = paste(m, "scale"))
    }
    if (isFALSE(row$sequential) && length(base)) {
      rev_cp <- sort(n - run(rev(x)))
      expect_true(all(vapply(base, function(cp) any(abs(rev_cp - cp) <= 2),
                             logical(1))), info = paste(m, "reversal"))
    }
  }
})

test_that("no engine returns a changepoint per observation without saying so", {
  skip_on_cran()
  # §172.5 item 6: the boundary-input matrix that would have caught the
  # three-point series with two changepoints. Every engine either refuses
  # the input with a classed error, or returns a result that is not
  # degenerate, or says it is.
  inputs <- list(
    n3 = c(1, 5, 9), n4 = c(0, 1, 5, 6), n5 = c(1, 2, 10, 11, 12),
    n8 = c(rep(0, 4), rep(3, 4)), n12 = c(rep(0, 6), rep(3, 6)),
    constant = rep(2, 40), near_constant = c(rep(2, 39), 2 + 1e-9),
    spike = replace(rep(0, 40), 20, 50), zeros = rep(0, 40),
    monotone = seq(0, 10, length.out = 40)
  )
  reg <- ggchangepoint:::builtin_registry()
  uni <- reg$method[reg$univariate & !reg$method %in%
                      c("mcp", "bocpd", "beast", "taylor", "cpop", "ecp",
                        "kcp", "npmojo", "ocd", "fabisearch", "smuce",
                        "hsmuce", "bfast", "envcpt", "sn", "nsp")]
  offenders <- character(0)
  for (m in uni) {
    if (!engine_usable(reg$engine[reg$method == m])) next
    for (nm in names(inputs)) {
      degenerate_warned <- FALSE
      res <- withCallingHandlers(
        tryCatch(cpt_detect(inputs[[nm]], method = m),
                 ggchangepoint_error = function(e) NULL,
                 error = function(e) e),
        warning = function(w) {
          if (inherits(w, "ggchangepoint_degenerate_segmentation")) {
            degenerate_warned <<- TRUE
          }
          invokeRestart("muffleWarning")
        })
      if (inherits(res, "error")) {
        offenders <- c(offenders, paste0(m, "/", nm, ": unclassed error"))
        next
      }
      if (is.null(res)) next
      n <- length(inputs[[nm]])
      if (nrow(res$changepoints) >= n - 1L && !degenerate_warned) {
        offenders <- c(offenders, paste0(m, "/", nm, ": a changepoint per ",
                                         "observation, unannounced"))
      }
    }
  }
  expect_equal(offenders, character(0))
})

test_that("0.5.0's arguments keep their names and positions", {
  # The first release under the stability policy: an argument may be added
  # after the ones a caller could have passed by position, never before
  # them, and none may disappear. 0.6.0's first draft broke three (the
  # `seed` of cpt_simulate() and of cpt_stability(), and the segmented
  # wrapper's `npsi`, all moved), which is the mistake this catches. The
  # fixture is the formals of every 0.5.0 export.
  old <- readRDS(test_path("fixtures", "formals-0.5.0.rds"))
  broken <- character(0)
  for (fn in names(old)) {
    # Re-exports (tidy(), autoplot(), ...) live in the imports, so look up
    # the export rather than the namespace's own bindings.
    obj <- tryCatch(getExportedValue("ggchangepoint", fn),
                    error = function(e) NULL)
    if (!is.function(obj)) {
      broken <- c(broken, paste0(fn, ": removed"))
      next
    }
    new <- names(formals(obj))
    o <- old[[fn]]
    positional <- o[seq_len(match("...", o, nomatch = length(o) + 1L) - 1L)]
    if (!identical(new[seq_along(positional)], positional)) {
      broken <- c(broken, paste0(fn, ": positions moved"))
    }
    gone <- setdiff(o, new)
    if (length(gone)) {
      broken <- c(broken, paste0(fn, ": dropped ", paste(gone, collapse = ", ")))
    }
  }
  expect_equal(broken, character(0))
})

test_that("the result contract holds on generated series", {
  skip_on_cran()
  # §72 item 1: the ggcpt contract is a list of invariants, so check them
  # as properties over generated input rather than on hand-picked
  # examples: segments tile the series, changepoints are sorted, unique,
  # whole and inside [1, n), glance() is one row, augment() is n rows, and
  # an index comes back with every changepoint.
  engines <- c("pelt", "binseg", "amoc", "wbs", "not", "mosum", "fpop",
               "binsegrcpp", "idetect", "decafs")
  reg <- ggchangepoint:::builtin_registry()
  engines <- engines[vapply(engines, function(m) {
    engine_usable(reg$engine[reg$method == m])
  }, logical(1))]
  set.seed(72)
  failures <- character(0)
  for (i in seq_len(15)) {
    n <- sample(30:300, 1)
    k <- sample(0:3, 1)
    cps <- sort(sample(seq(10, n - 10), k))
    mu <- rep(stats::rnorm(k + 1, 0, 3), diff(c(0, cps, n)))
    x <- mu + stats::rnorm(n, 0, stats::runif(1, 0.5, 2))
    idx <- as.Date("2026-01-01") + seq_len(n) - 1
    for (m in engines) {
      fit <- tryCatch(suppressWarnings(cpt_detect(x, method = m, index = idx)),
                      ggchangepoint_error = function(e) NULL)
      if (is.null(fit)) next
      cp <- fit$changepoints$cp
      ok <- c(
        tiles = sum(fit$segments$end - fit$segments$start + 1L) == n &&
          fit$segments$start[1] == 1L && utils::tail(fit$segments$end, 1) == n,
        sorted = !is.unsorted(cp, strictly = TRUE),
        range = all(cp >= 1L & cp < n),
        whole = is.integer(cp),
        glance = nrow(glance(fit)) == 1L,
        augment = nrow(augment(fit)) == n,
        index = identical(fit$changepoints$cp_index, idx[cp])
      )
      if (!all(ok)) {
        failures <- c(failures, paste0(m, " (n = ", n, "): ",
                                       paste(names(ok)[!ok], collapse = ", ")))
      }
    }
  }
  expect_equal(failures, character(0))
})
