# Internal: assemble a standard ggcpt object from detected changepoint indices.
#
# Shared by the 0.4.0 wrapper generation. `cp_indices` must already follow the
# package convention ("left": last index of the left segment). `extra_cp_cols`
# is an optional named list of per-changepoint columns (e.g. ci_lower/ci_upper,
# posterior_prob) appended to the changepoints tibble. `fitted` is an optional
# length-n engine-fitted signal stored as a `fitted` column on `$data` and used
# by `autoplot(show_fit = TRUE)` and `augment()`. `data_wide` is an optional
# tibble (index + one column per coordinate) for multivariate input.
#' @noRd
ggcpt_build <- function(data_vec, cp_indices, method, change_in, penalty,
                        fit = NULL, call = NULL, extra_cp_cols = NULL,
                        fitted = NULL, data_wide = NULL) {
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
  res
}

# Internal: check that an optional engine package is installed.
#' @noRd
need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
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
    return(as.numeric(X[, 1]))
  }
  as.numeric(x)
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
as_mv_matrix <- function(x) {
  X <- as.matrix(x)
  if (!is.numeric(X)) {
    stop("`x` must be numeric.", call. = FALSE)
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
