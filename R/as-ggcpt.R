#' Turn external changepoints into a ggcpt result
#'
#' Wraps a set of changepoint locations — from a detector this package does
#' not wrap, a Python tool called through \pkg{reticulate}, a neural
#' detector, a published paper's reported breaks, or an analyst's own
#' annotations — into a first-class \code{ggcpt} object. Everything built on
#' the \code{ggcpt} contract then applies: \code{autoplot()}, the composable
#' geoms, \code{tidy()}/\code{glance()}/\code{augment()},
#' \code{\link{cpt_metrics}()}, \code{\link{cpt_consensus}()},
#' \code{\link{cpt_report}()}.
#'
#' @param cp Integer vector of changepoint locations. Out-of-range,
#'   duplicated and missing values are dropped, and the result is sorted —
#'   the same contract every built-in wrapper is held to.
#' @param x The series the changepoints refer to: a numeric vector, or a
#'   matrix/data frame (rows are time points) for a multivariate result.
#' @param fitted Optional length-\code{n} fitted signal, used by
#'   \code{autoplot(show_fit = TRUE)} and \code{augment()}.
#' @param method Method label. Defaults to \code{"custom"}.
#' @param change_in What the changepoints are changes in. Defaults to
#'   \code{"mean"}.
#' @param ci Optional two-column matrix or data frame of location confidence
#'   intervals, one row per changepoint, giving lower and upper bounds as
#'   positions.
#' @param regions Optional two-column matrix or data frame of significance
#'   regions (\code{start}, \code{end}); see \code{\link{nsp_wrapper}()} for
#'   the interval-valued case this exists to serve.
#' @param penalty Optional penalty descriptor: a number, a string, or a list
#'   with \code{type} and \code{value}.
#' @param cp_convention \code{"left"} (the changepoint is the last index of
#'   the left segment — this package's convention) or \code{"right"} (the
#'   first index of the right segment, which is converted on the way in).
#' @param index Optional time index, one value per observation.
#' @param fit Optional raw upstream object to carry along.
#' @param extra Optional named list of per-changepoint columns, each of the
#'   same length as \code{cp}, appended to the changepoints tibble.
#'
#' @return A \code{ggcpt} object.
#' @seealso \code{\link{cpt_register_method}()} to make
#'   \code{\link{cpt_detect}()} dispatch to an external detector by name.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 4))
#' fit <- as_ggcpt(c(60), x, method = "my_detector")
#' fit
#' tidy(fit)
as_ggcpt <- function(cp, x, fitted = NULL, method = "custom",
                     change_in = "mean", ci = NULL, regions = NULL,
                     penalty = NULL, cp_convention = c("left", "right"),
                     index = NULL, fit = NULL, extra = NULL) {
  cp_convention <- match.arg(cp_convention)
  if (!is.character(method) || length(method) != 1L || !nzchar(method)) {
    stop("`method` must be a single non-empty string.", call. = FALSE)
  }
  change_in <- match.arg(change_in, cpt_change_in_levels())

  series <- as_cpt_series(x, index = index)
  values <- series$values
  is_mv <- (is.matrix(values) || is.data.frame(values)) &&
    ncol(as.matrix(values)) > 1
  if (is_mv) {
    X <- as_mv_matrix(values)
    data_vec <- as.numeric(X[, 1])
  } else {
    X <- NULL
    data_vec <- as.numeric(values)
  }
  validate_data(if (is_mv) X else data_vec)
  n <- length(data_vec)

  cp <- suppressWarnings(as.integer(cp))
  if (cp_convention == "right") cp <- cp - 1L

  extra_cols <- list()
  if (!is.null(ci)) {
    ci_m <- as.matrix(ci)
    if (ncol(ci_m) != 2L) {
      stop("`ci` must have two columns (lower, upper).", call. = FALSE)
    }
    if (nrow(ci_m) != length(cp)) {
      stop("`ci` must have one row per changepoint: ", length(cp),
           " changepoint(s) but ", nrow(ci_m), " row(s).", call. = FALSE)
    }
    extra_cols$ci_lower <- as.integer(ci_m[, 1])
    extra_cols$ci_upper <- as.integer(ci_m[, 2])
  }
  if (!is.null(extra)) {
    if (!is.list(extra) || is.null(names(extra)) || any(!nzchar(names(extra)))) {
      stop("`extra` must be a named list.", call. = FALSE)
    }
    bad <- names(extra)[lengths(extra) != length(cp)]
    if (length(bad) > 0) {
      stop("Every element of `extra` must have one value per changepoint (",
           length(cp), "); wrong length: ", paste(bad, collapse = ", "), ".",
           call. = FALSE)
    }
    extra_cols <- c(extra_cols, extra)
  }

  res <- ggcpt_build(
    data_vec, cp,
    method = method,
    change_in = change_in,
    penalty = as_penalty_descriptor(penalty),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (length(extra_cols)) extra_cols else NULL,
    fitted = fitted,
    data_wide = if (is_mv) mv_data_wide(X) else NULL,
    regions = regions
  )
  res$cp_convention <- "left"
  attach_index(res, series$index, series$index_label)
}

# Internal: accept a penalty as a number, a string, or an already-built
# list(type, value), and normalise it to the list form the object stores.
#' @noRd
as_penalty_descriptor <- function(penalty) {
  if (is.null(penalty)) return(list(type = "user", value = NA_real_))
  if (is.list(penalty)) {
    return(list(type = scalar_chr(penalty$type %||% "user"),
                value = suppressWarnings(
                  as.numeric(penalty$value %||% NA_real_))[1]))
  }
  penalty_descriptor(penalty)
}
