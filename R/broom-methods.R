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
#'   \code{cp_convention}, \code{total_cost} (\code{NA} when the engine does
#'   not expose a cost), \code{runtime} (elapsed seconds when measured by
#'   \code{cpt_detect()}, otherwise \code{NA}).
#' @export
glance.ggcpt <- function(x, ...) {
  total_cost <- NA_real_
  if (!is.null(x$fit)) {
    if (inherits(x$fit, "cpt")) {
      total_cost <- tryCatch(-as.numeric(logLik(x$fit)), error = function(e) NA_real_)
    } else if (inherits(x$fit, "cptrange")) {
      total_cost <- tryCatch(x$fit$cost, error = function(e) NA_real_)
    } else if (is.list(x$fit)) {
      # Exact [[ ]] subsetting: $ would partial-match unrelated elements
      # (e.g. DeCAFS's costFunction).
      cand <- x$fit[["cost"]] %||% x$fit[["loss"]] %||% x$fit[["value"]]
      if (!is.null(cand) && is.numeric(cand)) total_cost <- cand
    }
    # Some engines (e.g. fpop) expose a per-position cost vector; glance is
    # one row per model, so keep the terminal (total) cost only.
    total_cost <- suppressWarnings(as.numeric(total_cost))
    if (length(total_cost) == 0) {
      total_cost <- NA_real_
    } else if (length(total_cost) > 1) {
      total_cost <- total_cost[length(total_cost)]
    }
  }

  penalty_value <- if (is.list(x$penalty)) x$penalty$value else NA_real_
  penalty_value <- suppressWarnings(as.numeric(penalty_value %||% NA_real_))
  if (length(penalty_value) != 1) penalty_value <- NA_real_

  runtime <- x$runtime %||% NA_real_

  tibble::tibble(
    n = nrow(x$data),
    n_changepoints = nrow(x$changepoints),
    method = x$method,
    change_in = x$change_in,
    penalty_type = if (is.list(x$penalty)) x$penalty$type else NA_character_,
    penalty_value = penalty_value,
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
  # For a multivariate result use the wide frame (index + one column per
  # coordinate) so all coordinates are kept; otherwise use the univariate
  # single-column frame.
  use_wide <- !is.null(x$data_wide)
  data <- if (use_wide) tibble::as_tibble(x$data_wide) else x$data

  if (!use_wide) {
    # Rename columns by position in a way that works with >2 columns
    nms <- names(data)
    if (length(nms) >= 2) {
      names(data)[1:2] <- c("index", "value")
    }
  }

  # The index used to flag changepoints, and the value vector used for .resid
  # (the first coordinate for the wide multivariate frame).
  index_col <- data[["index"]]
  value_vec <- if (use_wide) {
    coord_cols <- setdiff(names(data), "index")
    as.numeric(data[[coord_cols[1]]])
  } else {
    data$value
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
    data$.resid <- value_vec - data$.fitted
  }

  # Prefer the engine's fitted signal when the object carries one.
  engine_fitted <- x$data[["fitted"]]
  if (!is.null(engine_fitted) && length(engine_fitted) == nrow(data)) {
    data$.fitted <- as.numeric(engine_fitted)
    data$.resid <- value_vec - data$.fitted
  }

  if (nrow(x$changepoints) > 0) {
    data$is_changepoint[index_col %in% x$changepoints$cp] <- TRUE
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

#' @rdname summary.ggcpt
#' @param x A \code{summary.ggcpt} object (for \code{print()}).
#' @export
print.summary.ggcpt <- function(x, ...) {
  cat("ggcpt Summary\n")
  cat("  Method:                  ", x$method, "\n")
  cat("  Change in:               ", x$change_in, "\n")
  cat("  Changepoints found:      ", x$n_changepoints, "\n")
  cat("  CP convention:           ", x$cp_convention, "\n")
  cat("  Series length:           ", x$n_obs, "\n")
  cat("  Penalty:                 ", format_penalty(x$penalty), "\n")
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

#' Coerce, format, and plot ggcpt objects
#'
#' Convenience S3 methods for working with \code{ggcpt} objects: coerce the
#' changepoints to a tibble or data frame, render a one-line summary string, or
#' produce the default plot (a base-graphics fallback that delegates to
#' \code{\link{autoplot.ggcpt}}).
#'
#' @param x A \code{ggcpt} object.
#' @param row.names,optional Passed to \code{\link[base]{as.data.frame}}.
#' @param .name_repair Ignored (the changepoints tibble already has valid,
#'   unique names); present for signature compatibility with the generic.
#' @param ... Additional arguments passed to methods.
#' @return \code{as_tibble()} and \code{as.data.frame()} return the changepoints
#'   table; \code{format()} returns a length-one character string; \code{plot()}
#'   returns a \code{ggplot} object.
#' @name ggcpt_methods
#' @examples
#' set.seed(2022)
#' res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
#' as_tibble(res)
#' as.data.frame(res)
#' format(res)
#' @export
as_tibble.ggcpt <- function(x, ..., .name_repair = NULL) {
  x$changepoints
}

#' @rdname ggcpt_methods
#' @export
as.data.frame.ggcpt <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$changepoints, row.names = row.names, optional = optional, ...)
}

#' @rdname ggcpt_methods
#' @export
format.ggcpt <- function(x, ...) {
  paste0(
    "ggcpt [", x$method, "] ",
    nrow(x$changepoints), " changepoint(s) on ",
    nrow(x$data), " observations"
  )
}

#' @rdname ggcpt_methods
#' @exportS3Method base::plot
plot.ggcpt <- function(x, ...) {
  autoplot.ggcpt(x, ...)
}
