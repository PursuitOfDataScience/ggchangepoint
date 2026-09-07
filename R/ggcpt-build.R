# Internal: assemble a standard ggcpt object from detected changepoint indices.
#
# Shared by the 0.4.0 wrapper generation. `cp_indices` must already follow the
# package convention ("left": last index of the left segment). `extra_cp_cols`
# is an optional named list of per-changepoint columns (e.g. ci_lower/ci_upper,
# posterior_prob) appended to the changepoints tibble. `fitted` is an optional
# length-n engine-fitted signal stored as a `fitted` column on `$data` and used
# by `autoplot(show_fit = TRUE)` and `augment()`. `data_wide` is an optional
# tibble (index + one column per coordinate) for multivariate input.
#
# 0.5.0 adds two more optional slots, both defaulted off and both following
# the `data_wide` precedent (present only when an engine supplies them, and
# tested for before use):
#   `regions`     a tibble of significance regions (start, end, ...) for the
#                 interval-valued methods -- NSP returns intervals that must
#                 each contain a change, which is not a point estimate and
#                 must not be squeezed into one.
#   `diagnostics` a named list of engine internals (the detector statistic,
#                 the solution path, a scale-space grid) that
#                 ggcpt_statistic() / ggcpt_solution_path() /
#                 ggcpt_scale_space() render.
#' @noRd
ggcpt_build <- function(data_vec, cp_indices, method, change_in, penalty,
                        fit = NULL, call = NULL, extra_cp_cols = NULL,
                        fitted = NULL, data_wide = NULL, regions = NULL,
                        diagnostics = NULL) {
  n <- length(data_vec)
  data_vec <- as.numeric(data_vec)

  # Build the changepoints tibble before filtering, so the optional extra
  # columns stay row-aligned with their indices through dedup/range checks.
  cp_indices <- as.integer(cp_indices)
  changepoints <- tibble::tibble(
    cp = cp_indices,
    cp_value = data_vec[pmax(pmin(cp_indices, n), 1L)]
  )
  if (!is.null(extra_cp_cols)) {
    for (nm in names(extra_cp_cols)) {
      changepoints[[nm]] <- extra_cp_cols[[nm]]
    }
  }
  keep <- !is.na(changepoints$cp) & changepoints$cp >= 1 & changepoints$cp < n
  changepoints <- changepoints[keep, , drop = FALSE]
  changepoints <- changepoints[!duplicated(changepoints$cp), , drop = FALSE]
  changepoints <- changepoints[order(changepoints$cp), , drop = FALSE]

  data_tbl <- tibble::tibble(index = seq_len(n), value = data_vec)
  if (!is.null(fitted) && length(fitted) == n) {
    data_tbl$fitted <- as.numeric(fitted)
  }

  if (nrow(changepoints) == 0) {
    res <- ggcpt_empty(data_vec, method)
    res$change_in <- change_in
    res$penalty <- penalty
    res$fit <- fit
    res$call <- call
    res$data <- data_tbl
    res$data_wide <- data_wide
    res$regions <- normalise_regions(regions, n)
    res$diagnostics <- diagnostics
    return(res)
  }

  res <- new_ggcpt(
    changepoints = changepoints,
    segments = build_segments(data_vec, changepoints$cp),
    data = data_tbl,
    method = method,
    change_in = change_in,
    penalty = penalty,
    fit = fit,
    call = call,
    cp_convention = "left"
  )
  res$data_wide <- data_wide
  res$regions <- normalise_regions(regions, n)
  res$diagnostics <- diagnostics
  warn_if_degenerate(res, method)
  res
}

# Internal: several engines refuse a short series from deep inside
# themselves, in their own vocabulary -- two of them with their own typos.
# Measured on a plain vector, the shortest length each accepts, and what it
# says below that:
#
#   wbs          4   "sample size is too small"
#   not          4   "max.length must satisfy 3 < max.lenght <= n"     [sic]
#   wbsts        4   "subscript out of bounds"                   (base R's)
#   taylor       5   "Invalid x argument. 'x' must be a numeric vector"
#   envcpt      12   "Minimum segment legnth is too large to include a
#                     change"                                          [sic]
#   strucchange 15   "minimum segment size must be greater than the number
#                     of regressors"
#   bfast       25   "series is not periodic or has less than two periods"
#
# Not one names the method the caller asked for or the length they gave it,
# and "subscript out of bounds" does not even say the series is the problem.
# Three of the thresholds move with an argument -- `minseglen`, `h`,
# `frequency` -- so a constant guard per wrapper would go stale against its
# own engine. Translating keeps the engine's diagnosis, which is the
# informative half, and adds the two things it never carried.
SHORT_SERIES_PATTERNS <- paste(
  "too small", "max\\.leng", "segment size", "segment legnth",
  "segment length", "not periodic", "two periods",
  "subscript out of bounds", "Invalid x argument",
  sep = "|")

#' @noRd
rethrow_short_series <- function(e, method, n, hint = NULL) {
  msg <- gsub("\\s+", " ", conditionMessage(e))
  if (!grepl(SHORT_SERIES_PATTERNS, msg)) stop(e)
  # Engine messages do not end in punctuation, so quote them and close the
  # sentence -- otherwise the hint runs straight on from the engine's last
  # word ("... less than two periods `bfast` needs at least ...").
  msg <- sub("[.;:, ]+$", "", msg)
  stop("Method `", method, "` could not segment a series of ", n,
       " observation(s). The engine reported: \"", msg, "\".",
       if (!is.null(hint)) paste0(" ", hint) else "", call. = FALSE)
}

#' @noRd
engine_short_series <- function(expr, method, n, hint = NULL) {
  tryCatch(expr, error = function(e) rethrow_short_series(e, method, n, hint))
}

# Internal: a segmentation in which every observation is its own segment is
# not a segmentation, it is a failure to segment -- and several engines
# produce exactly that on a series that is too short for them rather than
# refusing it. Measured at n = 3: pelt, fpop, wbs2, tguh, smuce, decafs and
# nsp all report a changepoint after every observation; at n = 5, wbs2,
# decafs and nsp still do. The threshold is engine-specific, so a blanket
# minimum in validate_data() would refuse calls that work; saying what
# happened is the honest alternative to returning the number silently.
#' @noRd
warn_if_degenerate <- function(res, method) {
  n <- nrow(res$data)
  k <- nrow(res$changepoints)
  if (n <= 2L || k != n - 1L) return(invisible(res))

  # Two different situations produce it, and blaming the wrong one is worse
  # than saying nothing: a zero penalty makes one segment per observation
  # the *correct* unpenalised optimum (`penalty = "None"` on an fpop fit
  # resolves to 0 and returns n - 1 changepoints on any length of series),
  # whereas a positive penalty reaching the same place means the series is
  # too short for the engine.
  pen <- res$penalty$value
  zero_penalty <- is.numeric(pen) && length(pen) == 1L &&
    is.finite(pen) && pen == 0
  warning("`", method, "` put a changepoint after every observation: ", k,
          " changepoint(s) on ", n, " observation(s), so every segment is ",
          "one point long. ",
          if (zero_penalty) {
            paste0("With a penalty of 0 that is the unpenalised optimum, ",
                   "not a segmentation -- give `penalty` a positive value.")
          } else {
            paste0("That is a failure to segment rather than a ",
                   "segmentation -- the series is too short for this engine.")
          }, call. = FALSE)
  invisible(res)
}

# Internal: normalise the optional `regions` slot to a tibble with integer
# `start`/`end` clipped to the series, dropping anything unusable. Returns
# NULL when there is nothing to store, so `is.null(res$regions)` stays the
# test for "this engine does not do regions".
#' @noRd
normalise_regions <- function(regions, n) {
  if (is.null(regions)) return(NULL)
  if (is.matrix(regions)) {
    if (ncol(regions) < 2L) {
      stop("`regions` must have at least two columns (start, end).",
           call. = FALSE)
    }
    cn <- colnames(regions)
    regions <- tibble::as_tibble(as.data.frame(regions),
                                 .name_repair = "minimal")
    if (is.null(cn)) names(regions)[1:2] <- c("start", "end")
  }
  regions <- tibble::as_tibble(regions)
  if (!all(c("start", "end") %in% names(regions))) {
    if (ncol(regions) < 2L) {
      stop("`regions` must have `start` and `end` columns.", call. = FALSE)
    }
    names(regions)[1:2] <- c("start", "end")
  }
  if (nrow(regions) == 0) {
    regions$start <- integer(0)
    regions$end <- integer(0)
    return(regions)
  }
  # as.integer() again after the clip: pmin() against a double `n` would
  # silently widen the columns back to double, and `cpt_regions()` derives
  # `length` from them.
  regions$start <- as.integer(pmax(1L, pmin(as.integer(round(regions$start)),
                                            n)))
  regions$end <- as.integer(pmax(1L, pmin(as.integer(round(regions$end)), n)))
  # An interval given the other way round is a data-entry slip, not a
  # different meaning; ordering it is what every plotting call assumes.
  flip <- regions$start > regions$end
  if (any(flip)) {
    tmp <- regions$start[flip]
    regions$start[flip] <- regions$end[flip]
    regions$end[flip] <- tmp
  }
  keep <- !is.na(regions$start) & !is.na(regions$end)
  regions <- regions[keep, , drop = FALSE]
  regions[order(regions$start, regions$end), , drop = FALSE]
}

# Internal: leave the caller's search path as we found it. Two engines
# mutate it. `fabisearch` needs NMF *attached* rather than loaded, and
# attaching NMF brings its own Depends (Biobase, BiocGenerics) and its
# foreach/doParallel/doRNG stack with it -- eight packages measured, where
# the wrapper only ever detached NMF itself. `bcp::bcp()` calls
# `require(bcp)` in its own body, so every call attaches `package:bcp` and
# `package:grid` (bcp's Depends). Neither is something a detection call
# should do to a user's session.
#
# Only what *this* call added is detached, so a package the user had already
# attached is untouched, and `search()` lists the most recently attached
# first, which is the order they have to go in.
#
# The attach also *speaks*: `require()` announces itself with
# `packageStartupMessage()`, so a plain `bcp_wrapper()` call printed
# "Loading required package: bcp" and "Loading required package: grid" on
# stderr -- restoring the search path silently was not enough, because the
# noise had already been emitted. Suppressing only package startup messages
# leaves the engine's own `message()` and `warning()` output intact.
#' @noRd
with_search_path_restored <- function(expr) {
  before <- search()
  on.exit({
    for (p in setdiff(search(), before)) {
      try(detach(p, character.only = TRUE, unload = FALSE), silent = TRUE)
    }
  }, add = TRUE)
  suppressPackageStartupMessages(force(expr))
}

# Internal: check that an optional engine package is installed.
#' @noRd
need_pkg <- function(pkg) {
  # Loading an engine's namespace can warn about the machine rather than the
  # data: `mosum` reaches tcltk through plot3D and misc3d, so on any headless
  # box -- a server, a container, a CI runner, a cluster node -- the first
  # `cpt_scale_space()` call warns "no DISPLAY variable so Tk is not
  # available". That is never actionable here, and a load that warns still
  # succeeds; a load that fails returns FALSE and is reported below.
  if (!suppressWarnings(requireNamespace(pkg, quietly = TRUE))) {
    stop("Package '", pkg, "' is required. ",
         "Install it with install.packages('", pkg, "').",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: coerce input for a univariate wrapper. Accepts vectors and
# single-column matrices/data frames; errors on wider input instead of
# silently flattening it column-major.
#' @noRd
as_uni_vector <- function(x, method) {
  if (is.matrix(x) || is.data.frame(x)) {
    X <- as.matrix(x)
    if (ncol(X) > 1) {
      stop("Method `", method, "` is univariate, but `x` has ", ncol(X),
           " columns. See cpt_methods() for multivariate methods.",
           call. = FALSE)
    }
    # A zero-column frame reaches `X[, 1]` and stops with base R's
    # "subscript out of bounds", which names neither the argument nor this
    # package.
    if (ncol(X) == 0L) {
      stop("`x` is empty: it has no columns to detect on.", call. = FALSE)
    }
    return(as.numeric(X[, 1]))
  }
  coerce_series_values(x)
}

# Internal: TRUE when a series carries no variation at all. Exact equality
# (rather than a tolerance) is deliberate: only a genuinely flat series is
# degenerate, and a series with tiny-but-real fluctuation should still be
# handed to the engine.
#' @noRd
is_constant <- function(v) {
  v <- v[is.finite(v)]
  length(v) == 0L || max(v) == min(v)
}

# Internal: which coordinates of a matrix are flat. Engines that standardise
# each coordinate (inspect, NP-MOJO, kcpRS) divide by an estimated standard
# deviation, so a flat coordinate turns their statistics into NaN and they
# fail with an opaque error -- even when the other coordinates carry a real
# change. A flat coordinate also carries no changepoint information, so
# dropping it loses nothing.
#' @noRd
constant_cols <- function(X) {
  vapply(seq_len(ncol(X)), function(j) is_constant(X[, j]), logical(1))
}

# Internal: drop flat coordinates before handing a matrix to such an engine,
# telling the user which went. Returns NULL when nothing is left to detect on.
#' @noRd
drop_constant_cols <- function(X, method) {
  flat <- constant_cols(X)
  if (!any(flat)) return(X)
  if (all(flat)) return(NULL)
  warning("Dropping constant coordinate(s) ",
          paste(colnames(X)[flat], collapse = ", "),
          " before running `", method,
          "`: a flat coordinate carries no changepoint information and ",
          "makes the engine's standardised statistics undefined.",
          call. = FALSE)
  X[, !flat, drop = FALSE]
}

# Internal: normalise multivariate input to a numeric matrix and build the
# wide data tibble stored on the ggcpt object.
#' @noRd
as_mv_matrix <- function(x, arg = "x") {
  # as.matrix(NULL) stops with "'data' must be of a vector type, was 'NULL'",
  # which says nothing about this package or which argument was empty. `arg`
  # exists for the same reason: the caller's argument is often `baseline`,
  # `new_obs` or `series` rather than `x`, and naming `x` sends the reader
  # looking for an argument the function they called does not have.
  if (is.null(x) || length(x) == 0L) {
    stop("`", arg, "` is empty: a multivariate series needs at least one ",
         "column with at least 3 observations.", call. = FALSE)
  }
  X <- as.matrix(x)
  if (!is.numeric(X)) {
    stop("`", arg, "` must be numeric.", nonnumeric_columns_note(x),
         call. = FALSE)
  }
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("V", seq_len(ncol(X)))
  }
  X
}

#' @noRd
mv_data_wide <- function(X) {
  # The coordinates become columns alongside the position column added below,
  # so their names must be unique both from "index" and from each other --
  # a matrix may legally carry duplicate colnames, and add_column() rejects
  # the frame if any survive. Deduplicating against a leading "index" handles
  # both collisions in one pass.
  cn <- colnames(X)
  if (is.null(cn)) cn <- paste0("V", seq_len(ncol(X)))
  colnames(X) <- make.unique(c("index", cn))[-1L]
  out <- tibble::as_tibble(as.data.frame(X, check.names = FALSE),
                           .name_repair = "minimal")
  tibble::add_column(out, index = seq_len(nrow(X)), .before = 1)
}
