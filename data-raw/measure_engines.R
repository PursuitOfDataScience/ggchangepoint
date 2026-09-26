# Measure the engines: the numbers behind the registry's measured columns
# and the package's data tables. Every column here replaced a hand-written
# list that turned out to be wrong somewhere (roadmap §176, §184, §187,
# §205), so none of them is written by hand any more.
#
# Run from the package root with a pinned R, one part at a time:
#   module load R/4.4.1
#   Rscript data-raw/measure_engines.R invariance   # §179, §184
#   Rscript data-raw/measure_engines.R noise        # §187, §189
#   Rscript data-raw/measure_engines.R datatype     # §246
#   Rscript data-raw/measure_engines.R null         # §211
#   Rscript data-raw/measure_engines.R runtime      # §208 (serial, see below)
# then `Rscript data-raw/build_measured_data.R` to turn the CSVs into the
# package's data sets. Parts write to data-raw/measurements/<part>.csv.
#
# Workers: MEASURE_CORES (default 1). Every engine call runs inside a
# callr subprocess with a timeout, so a hang or a C-level stall costs one
# cell, not the run. The runtime part is always serial: contention can only
# inflate a timing (§208.1), so it is measured with nothing else running.
suppressMessages({
  library(parallel)
})
args <- commandArgs(trailingOnly = TRUE)
part <- if (length(args)) args[1] else "invariance"
cores <- as.integer(Sys.getenv("MEASURE_CORES", "1"))
root <- normalizePath(".")
out_dir <- file.path(root, "data-raw", "measurements")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
options(rgl.useNULL = TRUE)

suppressMessages(pkgload::load_all(root, quiet = TRUE))
`%||%` <- function(a, b) if (is.null(a)) b else a
reg <- ggchangepoint:::builtin_registry_core()
skip <- c("fabisearch", "mcp")
installed <- vapply(reg$engine, ggchangepoint:::engine_installed, logical(1))
reg <- reg[installed & !reg$method %in% skip, , drop = FALSE]
only <- Sys.getenv("MEASURE_METHODS")
if (nzchar(only)) reg <- reg[reg$method %in% strsplit(only, ",")[[1]], ]
has_seed <- function(m) {
  w <- get(reg$wrapper[reg$method == m], asNamespace("ggchangepoint"))
  "seed" %in% names(formals(w))
}

# One engine's cells, in a subprocess with a timeout. `body` is a function
# of the method name returning a data frame; it runs with the package
# loaded and warnings muffled.
run_isolated <- function(m, body, timeout) {
  helpers <- list(detect_cps = detect_cps, noise_settings = noise_settings,
                  `%||%` = `%||%`)
  res <- tryCatch(
    callr::r(function(root, m, body, helpers) {
      options(rgl.useNULL = TRUE, warn = -1)
      suppressMessages(pkgload::load_all(root, quiet = TRUE))
      for (h in names(helpers)) assign(h, helpers[[h]], envir = globalenv())
      environment(body) <- globalenv()
      body(m)
    }, args = list(root = root, m = m, body = body, helpers = helpers),
    timeout = timeout),
    error = function(e) {
      data.frame(method = m, error = substr(conditionMessage(e), 1, 120))
    })
  res
}

# A detection that never errors: the changepoints, or NA on failure.
detect_cps <- function(x, m, ...) {
  a <- list(x, method = m, ...)
  w <- get(ggchangepoint:::builtin_registry_core()$wrapper[
    ggchangepoint:::builtin_registry_core()$method == m],
    asNamespace("ggchangepoint"))
  if ("seed" %in% names(formals(w)) && is.null(a$seed)) a$seed <- 1
  r <- tryCatch(suppressWarnings(suppressMessages(
    do.call(ggchangepoint::cpt_detect, a))), error = function(e) NULL)
  if (is.null(r)) NA_integer_ else r$changepoints$cp
}

# ---- invariances (§179, §184) ------------------------------------------------
invariance_body <- function(m) {
  reg <- ggchangepoint:::builtin_registry_core()
  uni <- reg$univariate[reg$method == m]
  detect_cps <- get("detect_cps", envir = globalenv())
  same <- function(a, b, tol = 1) {
    if (anyNA(a) || anyNA(b)) return(NA)
    length(a) == length(b) && (length(a) == 0 ||
      all(abs(sort(a) - sort(b)) <= tol))
  }
  rows <- lapply(1:5, function(r) {
    set.seed(1790 + r)
    base <- c(stats::rnorm(70), stats::rnorm(70, 3), stats::rnorm(60))
    x <- if (uni) base else cbind(base, c(stats::rnorm(70),
                                          stats::rnorm(70, 3),
                                          stats::rnorm(60)),
                                  stats::rnorm(200))
    n <- 200
    rev_x <- if (uni) rev(x) else x[nrow(x):1, , drop = FALSE]
    cat_x <- if (uni) c(x, x) else rbind(x, x)
    dup_x <- if (uni) x[rep(seq_len(n), each = 2)] else
      x[rep(seq_len(n), each = 2), , drop = FALSE]
    c0 <- detect_cps(x, m)
    c10 <- detect_cps(x * 10, m)
    c01 <- detect_cps(x * 0.1, m)
    csh <- detect_cps(x + 100, m)
    crv <- detect_cps(rev_x, m)
    ccat <- detect_cps(cat_x, m)
    cdup <- detect_cps(dup_x, m)
    data.frame(
      method = m, rep = r,
      k = if (anyNA(c0)) NA else length(c0),
      scale = same(c0, c10) && same(c0, c01),
      shift = same(c0, csh),
      reversal = if (anyNA(crv)) NA else same(c0, n - crv, tol = 2),
      concat_k = if (anyNA(ccat)) NA else length(ccat),
      dup_k = if (anyNA(cdup)) NA else length(cdup))
  })
  do.call(rbind, rows)
}

# ---- noise regimes (§187, §189) -------------------------------------------
# Each engine at its default and at every setting of its noise-model
# argument, so a recommendation can be a call rather than a name.
noise_settings <- list(
  smuce = list(default = list(), `family = "hsmuce"` = list(family = "hsmuce")),
  nsp = list(default = list(), `variant = "selfnorm"` = list(variant = "selfnorm"),
             `variant = "ar"` = list(variant = "ar")),
  fastcpd = list(default = list(), `family = "ar"` = list(family = "ar",
                                                          order = 1)),
  cpm = list(default = list(), `cpm_type = "Student"` =
               list(cpm_type = "Student")),
  pelt = list(default = list(), `change_in = "meanvar"` =
                list(change_in = "meanvar")),
  binseg = list(default = list(), `change_in = "meanvar"` =
                  list(change_in = "meanvar"))
)
noise_body <- function(m) {
  settings <- get("noise_settings", envir = globalenv())[[m]] %||%
    list(default = list())
  detect_cps <- get("detect_cps", envir = globalenv())
  regimes <- c("iid", "heavy", "ar1", "hetero")
  out <- list()
  for (st in names(settings)) for (rg in regimes) for (r in 1:10) {
    set.seed(1870 + r)
    n <- 300
    mu <- rep(c(0, 2, 0), each = 100)
    e <- switch(rg,
      iid = stats::rnorm(n),
      heavy = stats::rt(n, df = 3) / sqrt(3),
      ar1 = as.numeric(stats::arima.sim(list(ar = 0.7), n)) * sqrt(1 - 0.49),
      hetero = stats::rnorm(n) * rep(c(0.5, 1, 2.5), each = 100))
    cps <- do.call(detect_cps, c(list(mu + e, m), settings[[st]]))
    ok <- !anyNA(cps)
    hits <- if (ok) sum(vapply(c(100, 200), function(t) any(abs(cps - t) <= 10),
                               logical(1))) else NA
    fp <- if (ok) sum(vapply(cps, function(c) all(abs(c - c(100, 200)) > 10),
                             logical(1))) else NA
    out[[length(out) + 1]] <- data.frame(method = m, setting = st,
                                         regime = rg, rep = r, hits = hits,
                                         fp = fp)
  }
  do.call(rbind, out)
}

# ---- data types (§246) -------------------------------------------------------
datatype_body <- function(m) {
  detect_cps <- get("detect_cps", envir = globalenv())
  out <- list()
  for (tp in c("gaussian", "bernoulli", "poisson", "proportion")) {
    for (r in 1:10) {
      set.seed(2460 + r)
      n <- 400
      x <- switch(tp,
        gaussian = c(stats::rnorm(200), stats::rnorm(200, 1.4)),
        bernoulli = c(stats::rbinom(200, 1, 0.15), stats::rbinom(200, 1, 0.75)),
        poisson = c(stats::rpois(200, 2), stats::rpois(200, 10)),
        proportion = c(stats::rbinom(200, 20, 0.25),
                       stats::rbinom(200, 20, 0.6)) / 20)
      cps <- detect_cps(x, m)
      ok <- !anyNA(cps)
      out[[length(out) + 1]] <- data.frame(
        method = m, data_type = tp, rep = r,
        hit = if (ok) any(abs(cps - 200) <= 20) else NA,
        fp = if (ok) sum(abs(cps - 200) > 20) else NA)
    }
  }
  do.call(rbind, out)
}

# ---- null size at scale (§211) -------------------------------------------------
null_body_for <- function(n, reps) {
  # The length and replicate count are written into the body: the function
  # runs in a subprocess with its environment reset, so a closure over them
  # would find neither.
  f <- function(m) {
    detect_cps <- get("detect_cps", envir = globalenv())
    out <- lapply(seq_len(REPS), function(r) {
      set.seed(2110 + r)
      cps <- detect_cps(stats::rnorm(N), m)
      data.frame(method = m, n = N, rep = r,
                 fp = if (anyNA(cps)) NA else length(cps))
    })
    do.call(rbind, out)
  }
  body(f) <- do.call(substitute, list(body(f), list(N = n, REPS = reps)))
  f
}

# ---- runtime (§208) --------------------------------------------------------------
# Timed against an INSTALLED build, never load_all(): under pkgload nothing
# is byte-compiled, and the JIT compiling each function on its first calls
# added about half a second to every cell (the first draft of this table
# had pelt at 0.6 s for 1,000 points, which an installed build fits in 8
# ms). MEASURE_LIB names a library holding the build to time; without it
# the package is installed into a temporary one first.
runtime_lib <- function() {
  lib <- Sys.getenv("MEASURE_LIB")
  if (nzchar(lib)) return(normalizePath(lib))
  lib <- file.path(tempdir(), "measure-lib")
  dir.create(lib, showWarnings = FALSE)
  utils::install.packages(root, repos = NULL, type = "source", lib = lib,
                          quiet = TRUE)
  lib
}

runtime_cell <- function(m, n, lib, timeout = 120) {
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(
    callr::r(function(m, n) {
      options(rgl.useNULL = TRUE, warn = -1)
      suppressMessages(library(ggchangepoint))
      set.seed(2080)
      mu <- rep(c(0, 2, 0, 2, 0), each = ceiling(n / 5))[seq_len(n)]
      x <- mu + stats::rnorm(n)
      reg <- ggchangepoint:::builtin_registry_core()
      if (!reg$univariate[reg$method == m]) {
        x <- cbind(x, stats::rnorm(n), stats::rnorm(n))
      }
      a <- list(x, method = m, keep_fit = FALSE)
      w <- get(reg$wrapper[reg$method == m], asNamespace("ggchangepoint"))
      if ("seed" %in% names(formals(w))) a$seed <- 1
      # A warm-up call on a short series first, so the timing is the fit
      # and not the one-off cost of loading the engine's namespace.
      warm <- a
      warm[[1]] <- if (is.matrix(x)) x[seq_len(min(nrow(x), 200)), ,
                                        drop = FALSE] else
        x[seq_len(min(length(x), 200))]
      try(suppressWarnings(suppressMessages(do.call(cpt_detect, warm))),
          silent = TRUE)
      t1 <- proc.time()[["elapsed"]]
      r <- suppressWarnings(suppressMessages(do.call(cpt_detect, a)))
      list(seconds = proc.time()[["elapsed"]] - t1,
           k = nrow(r$changepoints))
    }, args = list(m = m, n = n), libpath = c(lib, .libPaths()),
    timeout = timeout),
    error = function(e) {
      if (inherits(e, "callr_timeout_error") ||
          grepl("timed out|timeout", conditionMessage(e), ignore.case = TRUE)) {
        list(seconds = NA_real_, k = NA_integer_, status = "timeout")
      } else {
        list(seconds = NA_real_, k = NA_integer_, status = "error",
             msg = substr(conditionMessage(e), 1, 100))
      }
    })
  secs <- res$seconds %||% NA_real_
  status <- res$status %||% if (is.finite(secs)) "ok" else "error"
  data.frame(method = m, n = n, seconds = secs,
             k = res$k %||% NA_integer_, status = status,
             message = res$msg %||% "")
}

methods <- reg$method
t_start <- Sys.time()
result <- switch(part,
  invariance = mclapply(methods, run_isolated, body = invariance_body,
                        timeout = 1200, mc.cores = cores),
  noise = mclapply(reg$method[reg$univariate], run_isolated,
                   body = noise_body, timeout = 3600, mc.cores = cores),
  datatype = mclapply(reg$method[reg$univariate], run_isolated,
                      body = datatype_body, timeout = 2400, mc.cores = cores),
  null = {
    # A length is attempted only for engines the runtime table shows
    # finishing it quickly: the O(n^2) engines (segneigh, strucchange,
    # bocpd) would need tens of gigabytes at n = 100,000 before any
    # timeout fired.
    uni <- reg$method[reg$univariate]
    rt_path <- file.path(out_dir, "runtime.csv")
    fast_at <- function(n, limit) {
      if (!file.exists(rt_path)) return(if (n <= 1000) uni else character(0))
      rt <- utils::read.csv(rt_path)
      ok <- rt$method[rt$n == n & rt$status == "ok" & rt$seconds < limit]
      intersect(uni, ok)
    }
    small <- mclapply(fast_at(1000, 60), run_isolated,
                      body = null_body_for(1000, 50),
                      timeout = 3600, mc.cores = cores)
    mid <- mclapply(fast_at(10000, 20), run_isolated,
                    body = null_body_for(10000, 10),
                    timeout = 3600, mc.cores = cores)
    big <- mclapply(fast_at(100000, 10), run_isolated,
                    body = null_body_for(100000, 6),
                    timeout = 3600, mc.cores = cores)
    c(small, mid, big)
  },
  runtime = {
    # A length is attempted only when the previous one finished in under
    # ten seconds: at least linear scaling puts the next one past the cap,
    # and for the O(n^2) engines past the machine's memory first.
    cells <- list()
    lib <- runtime_lib()
    for (m in methods) {
      for (n in c(1000, 10000, 100000, 1e6)) {
        cell <- runtime_cell(m, n, lib, timeout = if (n >= 1e6) 300 else 120)
        cells[[length(cells) + 1]] <- cell
        message(sprintf("%-12s n=%-7d %s %s", m, n, cell$status,
                        format(cell$seconds, digits = 3)))
        if (cell$status != "ok" || !is.finite(cell$seconds) ||
            cell$seconds >= 10) break
      }
    }
    cells
  },
  stop("unknown part: ", part)
)
# bind_rows(), not rbind(): an engine whose subprocess failed returns a
# two-column error row, and rbind() refused to stack it with the rest.
tab <- dplyr::bind_rows(lapply(result, function(r) {
  if (is.data.frame(r)) r else NULL
}))
out_name <- paste0(part, if (nzchar(only)) "_subset" else "", ".csv")
utils::write.csv(tab, file.path(out_dir, out_name), row.names = FALSE)
message("wrote ", nrow(tab), " rows for ", part, " in ",
        format(difftime(Sys.time(), t_start, units = "mins"), digits = 3))
