#' Coerce a time series object to values plus a time index
#'
#' Detection itself runs on positions — every wrapped engine assumes an
#' equally spaced sequence — but real series carry dates, and reporting a
#' changepoint as "index 147" when the data are monthly rainfall is an
#' unnecessary translation step for the user. \code{as_cpt_series()} is the
#' one place that separates the two: it pulls the numeric values out of a
#' \code{ts}, \code{xts}, \code{zoo}, \code{tsibble} or data frame and
#' returns the time index alongside them, so \code{\link{cpt_detect}()} can
#' detect on positions and report on dates.
#'
#' @param x A numeric vector or matrix, or a \code{ts}/\code{mts},
#'   \code{xts}, \code{zoo} or \code{tsibble} object. The \code{xts},
#'   \code{zoo} and \code{tsibble} paths need those packages installed
#'   (they are \code{Suggests}).
#' @param index Optional explicit index, one value per observation. Overrides
#'   any index carried by \code{x}, and is the way to attach dates to a plain
#'   numeric vector.
#' @param check_regular Warn when the index is not equally spaced? Defaults
#'   to \code{TRUE}. Every engine in the package assumes equal spacing, so an
#'   irregular index means the positions the engine sees are not the times
#'   the user means.
#'
#' @return A list with components \code{values} (a numeric vector, or a
#'   matrix for multivariate input), \code{index} (the time index, or
#'   \code{NULL} when there is none) and \code{index_label} (a name for the
#'   x axis).
#' @export
#' @examples
#' as_cpt_series(1:10)$index
#' s <- as_cpt_series(1:10, index = as.Date("2020-01-01") + 0:9)
#' s$index
as_cpt_series <- function(x, index = NULL, check_regular = TRUE) {
  validate_flag(check_regular, "check_regular")
  label <- "Index"

  if (inherits(x, "tbl_ts")) {
    parts <- tsibble_parts(x)
    values <- parts$values
    carried <- parts$index
    label <- parts$label
  } else if (inherits(x, c("xts", "zoo"))) {
    if (!requireNamespace("zoo", quietly = TRUE)) {
      stop("Package 'zoo' is required to read a ", class(x)[1],
           " object. Install it with install.packages('zoo').", call. = FALSE)
    }
    values <- zoo::coredata(x)
    carried <- zoo::index(x)
    label <- "Time"
  } else if (stats::is.ts(x)) {
    values <- if (is.matrix(x)) unclass(x)[, , drop = FALSE] else as.numeric(x)
    carried <- as.numeric(stats::time(x))
    label <- "Time"
  } else {
    values <- x
    carried <- NULL
  }

  if (is.matrix(values) && ncol(values) == 1L) {
    nm <- colnames(values)
    values <- as.numeric(values[, 1])
    if (!is.null(nm)) label <- label
  } else if (!is.matrix(values) && !is.data.frame(values)) {
    values <- as.numeric(values)
  }

  idx <- index %||% carried
  n <- if (is.matrix(values) || is.data.frame(values)) {
    nrow(as.matrix(values))
  } else {
    length(values)
  }
  if (!is.null(idx)) {
    validate_index(idx, n)
    check_index_usable(idx, check_regular = check_regular)
    if (!is.null(index)) label <- "Index"
  }

  list(values = values, index = idx, index_label = label)
}

# Internal: pull values + index out of a tsibble without importing it.
#' @noRd
tsibble_parts <- function(x) {
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    stop("Package 'tsibble' is required to read a tsibble. ",
         "Install it with install.packages('tsibble').", call. = FALSE)
  }
  keys <- tsibble::key_vars(x)
  if (length(keys) > 0) {
    stop("`x` is a keyed tsibble (key: ", paste(keys, collapse = ", "),
         "), which holds several series. Detection runs on one series at a ",
         "time: split it first, or use cpt_batch() for the whole panel.",
         call. = FALSE)
  }
  idx_var <- tsibble::index_var(x)
  measures <- setdiff(names(x), idx_var)
  num <- measures[vapply(measures, function(v) is.numeric(x[[v]]),
                         logical(1))]
  if (length(num) == 0) {
    stop("The tsibble has no numeric measurement column to detect on.",
         call. = FALSE)
  }
  values <- if (length(num) == 1L) {
    as.numeric(x[[num]])
  } else {
    as.matrix(as.data.frame(x)[, num, drop = FALSE])
  }
  list(values = values, index = x[[idx_var]], label = idx_var)
}

# Internal: an index labels the x axis and is used to translate changepoint
# positions back to the user's own scale, so it has to be usable for both.
# NAs and out-of-order values are refused; unequal spacing is only warned
# about, because a user may legitimately want dates on the axis of a series
# they know is (near enough) regular.
#' @noRd
check_index_usable <- function(idx, check_regular = TRUE) {
  if (anyNA(idx)) {
    stop("`index` must not contain NA.", call. = FALSE)
  }
  num <- suppressWarnings(as.numeric(idx))
  if (anyNA(num)) {
    # A non-numeric, non-time index (e.g. character labels) cannot be checked
    # for ordering or spacing, but it can still label an axis.
    return(invisible(TRUE))
  }
  if (is.unsorted(num, strictly = FALSE)) {
    stop("`index` must be non-decreasing: the series is a sequence in time, ",
         "and detection runs on its order.", call. = FALSE)
  }
  if (isTRUE(check_regular) && length(num) > 2) {
    gaps <- diff(num)
    span <- max(num) - min(num)
    if (span > 0 && stats::sd(gaps) / mean(gaps) > 1e-6) {
      warning("`index` is not equally spaced. Every engine in this package ",
              "assumes equal spacing, so detection runs on observation ",
              "positions and the index only labels them. Pass ",
              "`check_regular = FALSE` (or use a regular index) to silence ",
              "this.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Internal: attach a time index to a finished ggcpt object. Positions stay
# canonical -- `$data$index` remains 1..n and `$changepoints$cp` remains a
# position -- and the original scale is added alongside, so nothing that
# reads the old contract changes behaviour.
#' @noRd
attach_index <- function(res, index, label = "Index") {
  if (is.null(index)) return(res)
  n <- nrow(res$data)
  if (length(index) != n) return(res)
  res$index <- index
  res$index_label <- label
  res$data$index_value <- index
  if (nrow(res$changepoints) > 0) {
    cp <- res$changepoints$cp
    res$changepoints$cp_index <- index[pmax(pmin(cp, n), 1L)]
    # Keep cp_index next to cp rather than after the engine's extra columns.
    nm <- names(res$changepoints)
    ord <- c("cp", "cp_index", setdiff(nm, c("cp", "cp_index")))
    res$changepoints <- res$changepoints[, ord, drop = FALSE]
  } else {
    res$changepoints$cp_index <- index[0]
  }
  if (!is.null(res$data_wide)) {
    res$data_wide$index_value <- index
  }
  if (!is.null(res$regions) && nrow(res$regions) > 0) {
    res$regions$start_index <- index[pmax(pmin(res$regions$start, n), 1L)]
    res$regions$end_index <- index[pmax(pmin(res$regions$end, n), 1L)]
  }
  res
}

# Internal: the x-axis values a plot should use, in priority order --
# an explicit argument, then the index carried on the object, then position.
#' @noRd
plot_index <- function(object, index = NULL) {
  index %||% object$index %||% object$data$index
}

# Internal: the x-axis label matching plot_index().
#' @noRd
plot_index_label <- function(object, index = NULL) {
  if (!is.null(index)) return("Index")
  if (!is.null(object$index)) return(object$index_label %||% "Time")
  "Index"
}

# Internal: resolve a column selector (bare symbol, string, or position)
# against a data frame. Used by cpt_detect()'s data-frame interface, which
# takes `y` and `index` as column selections rather than vectors.
#' @noRd
df_column <- function(df, expr, arg_name, env) {
  if (is.null(expr)) return(NULL)
  # A bare symbol names a column; anything else is evaluated in the caller's
  # frame first (so a variable holding a column name, or a literal string or
  # position, all work).
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% names(df)) return(df[[nm]])
    val <- tryCatch(eval(expr, env), error = function(e) NULL)
    if (is.null(val)) {
      stop("`", arg_name, " = ", nm, "`: no column called \"", nm,
           "\" in the data frame (columns: ",
           paste(names(df), collapse = ", "), ").", call. = FALSE)
    }
  } else {
    val <- eval(expr, df, env)
  }
  if (length(val) == 1L && (is.character(val) || is.numeric(val)) &&
      nrow(df) != 1L) {
    # A name or position selecting a column, rather than a length-1 series.
    # The `nrow(df) != 1` guard matters: in a one-row frame a legitimate
    # value could also be a valid column position, and reading it as one
    # would silently return the wrong column. A one-row frame is not a
    # series anyway -- validate_data() rejects it with a clear message.
    if (is.character(val) && val %in% names(df)) return(df[[val]])
    if (is.numeric(val) && val >= 1 && val <= ncol(df) && val %% 1 == 0) {
      return(df[[as.integer(val)]])
    }
  }
  if (length(val) != nrow(df)) {
    stop("`", arg_name, "` must select a column of the data frame, or be a ",
         "vector with one value per row (", nrow(df), "); got length ",
         length(val), ".", call. = FALSE)
  }
  val
}
