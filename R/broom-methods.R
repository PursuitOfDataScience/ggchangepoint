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

  tibble::tibble(
    n = nrow(x$data),
    n_changepoints = nrow(x$changepoints),
    method = x$method,
    change_in = x$change_in,
    penalty_type = if (is.list(x$penalty)) x$penalty$type else NA_character_,
    penalty_value = if (is.list(x$penalty)) x$penalty$value else NA_real_,
    cp_convention = x$cp_convention %||% "left",
    total_cost = total_cost,
    runtime = NA_real_
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
  colnames(data) <- c("index", "value")

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
