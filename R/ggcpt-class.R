#' Create a ggcpt object
#'
#' @param changepoints A tibble with columns \code{cp} and \code{cp_value}.
#' @param segments A tibble with segment information: \code{seg_id}, \code{start},
#'   \code{end}, \code{n}, \code{param_estimate}.
#' @param data A tibble with \code{index} and \code{value}.
#' @param method Character. The detection method used.
#' @param change_in Character. What was detected (e.g. "mean", "var", "meanvar").
#' @param penalty A list with \code{type} and \code{value}.
#' @param fit The raw upstream object.
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
                       method = character(),
                       change_in = character(),
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
  penalty_str <- if (is.list(x$penalty)) {
    paste0(x$penalty$type, " = ", x$penalty$value)
  } else {
    as.character(x$penalty)
  }
  cat("  Penalty:        ", penalty_str, "\n")
  cat("  Series length:  ", nrow(x$data), "\n")
  if (nrow(x$changepoints) > 0) {
    cat("\nChangepoints:\n")
    print(x$changepoints, n = min(nrow(x$changepoints), 10))
    if (nrow(x$changepoints) > 10) {
      cat("  ... ", nrow(x$changepoints) - 10, " more changepoint(s)\n")
    }
  } else {
    cat("\nNo changepoints detected.\n")
  }
  invisible(x)
}
