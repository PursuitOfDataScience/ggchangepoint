#' Create a ggcpt object
#'
#' @param changepoints A tibble with columns \code{cp} and \code{cp_value}.
#' @param segments A tibble with segment information: \code{seg_id}, \code{start},
#'   \code{end}, \code{n}, \code{param_estimate}.
#' @param data A tibble with \code{index} and \code{value}.
#' @param method Character. The detection method used. A length-one string;
#'   defaults to \code{NA_character_}. (A zero-length value would make
#'   \code{glance()} return zero rows instead of its documented single row,
#'   because every other column would be recycled against it.)
#' @param change_in Character. What was detected (e.g. "mean", "var",
#'   "meanvar"). A length-one string; defaults to \code{NA_character_}.
#' @param penalty A list with \code{type} and \code{value}.
#' @param fit The raw upstream object. Every wrapper stores one except
#'   \code{"ecp"}: \code{ecp::e.agglo()} returns a cluster-progression
#'   matrix that is quadratic in the series length, so keeping it by
#'   default would make the result object explode on a long series. Call
#'   \code{ecp::e.divisive()} or \code{ecp::e.agglo()} directly if you
#'   need it.
#' @param call The matched call.
#' @param cp_convention Character. The convention for reporting changepoint
#'   locations: \code{"left"} (last index of left segment, used by
#'   \code{changepoint}) or \code{"right"} (first index of right segment, used
#'   by \code{ecp}). Defaults to \code{"left"}.
#' @param runtime Numeric. Elapsed detection time in seconds, if measured.
#'   Defaults to \code{NA}.
#'
#' @return An object of class \code{ggcpt}.
#' @export
new_ggcpt <- function(changepoints = tibble::tibble(cp = integer(), cp_value = numeric()),
                       segments = tibble::tibble(seg_id = integer(),
                                                  start = integer(),
                                                  end = integer(),
                                                  n = integer(),
                                                  param_estimate = numeric()),
                       data = tibble::tibble(index = integer(), value = numeric()),
                       method = NA_character_,
                       change_in = NA_character_,
                       penalty = list(type = NA_character_, value = NA_real_),
                       fit = NULL,
                       call = NULL,
                       cp_convention = "left",
                       runtime = NA_real_) {
  structure(
    list(
      changepoints = changepoints,
      segments = segments,
      data = data,
      method = method,
      change_in = change_in,
      penalty = penalty,
      fit = fit,
      call = call,
      cp_convention = cp_convention,
      runtime = runtime
    ),
    class = "ggcpt"
  )
}

# Internal: render a penalty descriptor for printing. Numeric values are
# rounded to something readable rather than shown at full double precision,
# and a penalty that has no numeric value (a name, threshold type, or model
# selector) prints as just its type instead of trailing " = NA".
#' @noRd
format_penalty <- function(penalty) {
  if (!is.list(penalty)) return(as.character(penalty))
  val <- penalty$value
  if (length(val) != 1 || !is.numeric(val) || !is.finite(val)) {
    return(as.character(penalty$type))
  }
  paste0(penalty$type, " = ", format(val, digits = 5))
}

#' Test if an object is a ggcpt object
#'
#' @param x An object to test.
#' @return \code{TRUE} if \code{x} inherits from \code{ggcpt}.
#' @export
is_ggcpt <- function(x) {
  inherits(x, "ggcpt")
}

#' Print a ggcpt object
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @export
print.ggcpt <- function(x, ...) {
  cat("ggcpt (changepoint detection result)\n")
  cat("  Method:         ", x$method, "\n")
  cat("  Change in:      ", x$change_in, "\n")
  cat("  Changepoints found:", nrow(x$changepoints), "\n")
  cat("  CP convention:  ", x$cp_convention, "\n")
  cat("  Penalty:        ", format_penalty(x$penalty), "\n")
  cat("  Series length:  ", nrow(x$data), "\n")
  if (nrow(x$changepoints) > 0) {
    cat("\nChangepoints:\n")
    print(x$changepoints, n = 10)
  } else {
    cat("\nNo changepoints detected.\n")
  }
  invisible(x)
}
