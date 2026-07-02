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
  changepoints <- changepoints[changepoints$cp >= 1 & changepoints$cp < n, , drop = FALSE]
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
  out <- tibble::as_tibble(as.data.frame(X))
  tibble::add_column(out, index = seq_len(nrow(X)), .before = 1)
}
