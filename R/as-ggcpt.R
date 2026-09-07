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
#'   duplicated and missing values are dropped and the result is sorted —
#'   the same contract every built-in wrapper is held to — but unlike a
#'   wrapper, which is normalising an engine's output, this function is
#'   given yours, so \strong{anything it drops it warns about}, with the
#'   values and the range they had to fall in. A fractional index is
#'   included in that: it is truncated rather than rounded, which makes
#'   \code{50.5} into a changepoint at 50.
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
#' @family result class
as_ggcpt <- function(cp, x, fitted = NULL, method = "custom",
                     change_in = "mean", ci = NULL, regions = NULL,
                     penalty = NULL, cp_convention = c("left", "right"),
                     index = NULL, fit = NULL, extra = NULL) {
  cp_convention <- match.arg(cp_convention)
  # Kept because the checks below need the value as the caller wrote it:
  # as_cp_locations() truncates a fractional index through as.integer(),
  # which is exactly one of the things worth refusing.
  cp_input <- cp
  if (!is.character(method) || length(method) != 1L || !nzchar(method)) {
    stop("`method` must be a single non-empty string.", call. = FALSE)
  }
  change_in <- match.arg(change_in, cpt_change_in_levels())

  series <- as_cpt_series(x, index = index)
  values <- series$values
  is_mv <- (is.matrix(values) || is.data.frame(values)) &&
    ncol(as.matrix(values)) > 1
  if (is_mv) {
    X <- as_mv_matrix(values, arg = "x")
    data_vec <- as.numeric(X[, 1])
  } else {
    X <- NULL
    data_vec <- as.numeric(values)
  }
  validate_data(if (is_mv) X else data_vec)
  n <- length(data_vec)

  cp <- as_cp_locations(cp, "cp")
  # `ggcpt_build()` below drops a `cp` that is NA or outside 1..(n-1), and
  # `@param cp` documents that as the contract -- "the same contract every
  # built-in wrapper is held to". For a wrapper that is right: the indices
  # come from an engine, some of which legitimately emit a boundary value
  # (`ecp` reports n), and normalising a machine's output is the wrapper's
  # job.
  #
  # Here they come from a person. The documented use cases are a published
  # paper's reported breaks, an analyst's annotations, another package's
  # output -- and for those, one mistyped index vanishing without a word
  # leaves a result that looks complete and is short a changepoint.
  # Measured on a 200-point series:
  #
  #   as_ggcpt(c(50, 500), x)  -> 1 changepoint at 50
  #   as_ggcpt(c(0, 50), x)    -> 1 changepoint at 50
  #   as_ggcpt(c(50, NA), x)   -> 1 changepoint at 50
  #   as_ggcpt(50.5, x)        -> 1 changepoint at 50   (truncated)
  #   as_ggcpt(c(50, 50), x)   -> 1 changepoint at 50   (deduplicated)
  #
  # The dropping is kept, because it is the documented behaviour and
  # refusing would break code that works today. What is not kept is the
  # silence: each of the five now says what it discarded and why. Sorting
  # stays silent, because reordering loses nothing.
  if (length(cp) > 0) {
    lo <- if (cp_convention == "right") 2L else 1L
    hi <- if (cp_convention == "right") n else n - 1L
    dropped <- character(0)
    if (anyNA(cp)) {
      dropped <- c(dropped, paste0(sum(is.na(cp)), " missing"))
    }
    # The fractional case has to be read off the input rather than `cp`,
    # because as_cp_locations() has already truncated it through
    # as.integer() -- and truncating 50.5 to 50 is a different changepoint,
    # not a rounding.
    cp_raw <- suppressWarnings(as.numeric(cp_input))
    frac <- is.finite(cp_raw) & cp_raw != trunc(cp_raw)
    if (any(frac)) {
      dropped <- c(dropped, paste0(
        sum(frac), " truncated to whole numbers (",
        paste(format(utils::head(cp_raw[frac], 3)), collapse = ", "), ")"))
    }
    oob <- !is.na(cp) & (cp < lo | cp > hi)
    if (any(oob)) {
      dropped <- c(dropped, paste0(
        sum(oob), " outside ", lo, "..", hi, " (",
        paste(utils::head(cp[oob], 3), collapse = ", "),
        if (sum(oob) > 3) ", ..." else "", ")"))
    }
    dup <- !is.na(cp) & duplicated(cp)
    if (any(dup)) {
      dropped <- c(dropped, paste0(sum(dup), " duplicated (",
                                   paste(unique(cp[dup]), collapse = ", "), ")"))
    }
    if (length(dropped) > 0) {
      # Classed, so the one caller for which the silence *was* right can
      # muffle it precisely. `cpt_detect()` routes a registered method's
      # bare-vector return through here, and that is the wrapper case: the
      # indices come from a detector, not from a person transcribing them,
      # and "check the values against the series" is advice for the wrong
      # audience. See run_registered_method().
      msg <- paste0(
        "`cp`: ", paste(dropped, collapse = ", "),
        ". ", length(cp), " location(s) supplied. A \"", cp_convention,
        "\" changepoint is ",
        if (cp_convention == "right") {
          paste0("the first index of the segment after it, so 1 would ",
                 "leave no segment before it")
        } else {
          paste0("the last index of the segment before it, so ", n,
                 " would leave no segment after it")
        },
        ". Check the values against the series rather than relying on ",
        "this normalisation.")
      warning(structure(
        class = c("ggchangepoint_cp_dropped", "warning", "condition"),
        list(message = msg, call = NULL)))
    }
  }
  if (cp_convention == "right") cp <- cp - 1L

  # Every other optional slot below reports a length mismatch; `fitted` was
  # the one that did not, and ggcpt_build() drops a wrong-length signal
  # silently -- after which autoplot(show_fit = TRUE) tells the user the
  # result "carries no fitted signal", about a signal they supplied.
  if (!is.null(fitted) && length(fitted) != n) {
    stop("`fitted` must have one value per observation: the series has ", n,
         " observation(s) but `fitted` has ", length(fitted), ".",
         call. = FALSE)
  }

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
