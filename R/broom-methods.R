#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' Tidy a ggcpt object
#'
#' Returns the changepoints tibble (one row per changepoint).
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @return A tibble with columns \code{cp}, \code{cp_value}, and any
#'   method-specific columns.
#' @export
tidy.ggcpt <- function(x, ...) {
  x$changepoints
}

#' Glance at a ggcpt object
#'
#' Returns a one-row summary of a changepoint detection result.
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @return A one-row tibble with columns: \code{n}, \code{n_changepoints},
#'   \code{method}, \code{change_in}, \code{penalty_type}, \code{penalty_value},
#'   \code{cp_convention}, \code{total_cost} (if available), \code{runtime} (NA).
#' @export
glance.ggcpt <- function(x, ...) {
  total_cost <- NA_real_
  if (!is.null(x$fit)) {
    if (inherits(x$fit, "cpt")) {
      total_cost <- tryCatch(-as.numeric(logLik(x$fit)), error = function(e) NA_real_)
    } else if (inherits(x$fit, "cptrange")) {
      total_cost <- tryCatch(x$fit$cost, error = function(e) NA_real_)
    } else if (!is.null(x$fit$cost) || !is.null(x$fit$loss)) {
      total_cost <- x$fit$cost %||% x$fit$loss %||% NA_real_
    } else if (!is.null(x$fit$value)) {
      total_cost <- x$fit$value
    }
  }

  runtime <- x$runtime %||% NA_real_

  tibble::tibble(
    n = nrow(x$data),
    n_changepoints = nrow(x$changepoints),
    method = x$method,
    change_in = x$change_in,
    penalty_type = if (is.list(x$penalty)) x$penalty$type else NA_character_,
    penalty_value = if (is.list(x$penalty)) x$penalty$value else NA_real_,
    cp_convention = x$cp_convention %||% "left",
    total_cost = total_cost,
    runtime = runtime
  )
}

#' Augment a ggcpt object
#'
#' Returns the original data with added columns: \code{seg_id}, \code{.fitted},
#' \code{.resid}, and \code{is_changepoint}.
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @return A tibble with the original data plus augment columns.
#' @export
augment.ggcpt <- function(x, ...) {
  data <- x$data
  # Rename columns by position in a way that works with >2 columns
  nms <- names(data)
  if (length(nms) >= 2) {
    names(data)[1:2] <- c("index", "value")
  }

  data$seg_id <- NA_integer_
  data$.fitted <- NA_real_
  data$.resid <- NA_real_
  data$is_changepoint <- FALSE

  if (nrow(x$segments) > 0) {
    for (i in seq_len(nrow(x$segments))) {
      s <- x$segments[i, ]
      idx <- seq(s$start, s$end)
      data$seg_id[idx] <- s$seg_id
      data$.fitted[idx] <- s$param_estimate
    }
    data$.resid <- data$value - data$.fitted
  }

  if (nrow(x$changepoints) > 0) {
    data$is_changepoint[data$index %in% x$changepoints$cp] <- TRUE
  }

  tibble::as_tibble(data)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Summary of a ggcpt object
#'
#' Provides a human-readable digest of a changepoint detection result,
#' including the segment table with levels and lengths, total cost,
#' penalty, and runtime.
#'
#' @param object A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @return A list with class \code{summary.ggcpt} containing the summary.
#' @exportS3Method base::summary
summary.ggcpt <- function(object, ...) {
  structure(
    list(
      method = object$method,
      change_in = object$change_in,
      n_changepoints = nrow(object$changepoints),
      cp_convention = object$cp_convention %||% "left",
      n_obs = nrow(object$data),
      penalty = object$penalty,
      segments = object$segments,
      changepoints = object$changepoints,
      runtime = object$runtime %||% NA_real_
    ),
    class = "summary.ggcpt"
  )
}

#' @export
print.summary.ggcpt <- function(x, ...) {
  cat("ggcpt Summary\n")
  cat("  Method:                  ", x$method, "\n")
  cat("  Change in:               ", x$change_in, "\n")
  cat("  Changepoints found:      ", x$n_changepoints, "\n")
  cat("  CP convention:           ", x$cp_convention, "\n")
  cat("  Series length:           ", x$n_obs, "\n")
  pen <- x$penalty
  if (is.list(pen)) {
    cat("  Penalty:                 ", paste0(pen$type, " = ", pen$value), "\n")
  }
  cat("  Runtime (seconds):       ", format(x$runtime, digits = 4), "\n")
  if (nrow(x$segments) > 0) {
    cat("\nSegments:\n")
    print(x$segments, n = min(nrow(x$segments), 10))
  }
  if (nrow(x$changepoints) > 0) {
    cat("\nChangepoints:\n")
    print(x$changepoints, n = min(nrow(x$changepoints), 10))
  }
  invisible(x)
}

#' @export
as_tibble.ggcpt <- function(x, ..., .name_repair = NULL) {
  x$changepoints
}

#' @export
as.data.frame.ggcpt <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$changepoints, row.names = row.names, optional = optional, ...)
}

#' @export
format.ggcpt <- function(x, ...) {
  paste0(
    "ggcpt [", x$method, "] ",
    nrow(x$changepoints), " changepoint(s) on ",
    nrow(x$data), " observations"
  )
}

#' @exportS3Method base::plot
plot.ggcpt <- function(x, ...) {
  autoplot.ggcpt(x, ...)
}
