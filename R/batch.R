#' Batch changepoint detection over many series
#'
#' Runs one detector over every series in a collection — the panel-data loop
#' that methodological and applied work both need constantly. Accepts a
#' matrix/data frame (one column per series) or a named list of numeric
#' vectors. Honours \code{future::plan()} for parallel execution when the
#' \pkg{future.apply} package is available, with parallel-safe RNG.
#'
#' @param x For \code{cpt_batch()}, a numeric matrix or data frame (columns
#'   are series) or a list of numeric vectors; for the \code{print()} and
#'   \code{tidy()} methods, a \code{ggcpt_batch} object.
#' @param method Detection method, passed to \code{\link{cpt_detect}()}.
#' @param change_in What to detect change in, passed to
#'   \code{\link{cpt_detect}()}.
#' @param seed Optional seed for reproducible parallel execution (passed to
#'   \code{future.apply::future_lapply()} as \code{future.seed}; applied via
#'   \code{set.seed()} when running sequentially).
#' @param ... Additional arguments passed to every \code{cpt_detect()} call.
#' @return A \code{ggcpt_batch} object: a tibble with one row per series and
#'   columns \code{series}, \code{n_changepoints}, \code{changepoints} (a
#'   list-column of tidy tibbles), and \code{result} (a list-column of
#'   \code{ggcpt} objects). Methods: \code{print()}, \code{tidy()} (one row
#'   per changepoint across all series), and \code{autoplot()} (faceted
#'   small-multiples with each series' changepoints).
#' @export
#' @examples
#' set.seed(2026)
#' X <- cbind(a = c(rnorm(60), rnorm(60, 4)), b = rnorm(120))
#' batch <- cpt_batch(X, method = "pelt")
#' batch
#' tidy(batch)
#' ggplot2::autoplot(batch)
cpt_batch <- function(x, method = "pelt", change_in = "mean", seed = NULL,
                      ...) {
  series_list <- if (is.list(x) && !is.data.frame(x)) {
    lapply(x, as.numeric)
  } else {
    X <- as_mv_matrix(x)
    stats::setNames(lapply(seq_len(ncol(X)), function(j) X[, j]),
                    colnames(X))
  }
  # Fill in only the missing names, keeping any the user supplied.
  nms <- names(series_list) %||% rep("", length(series_list))
  nms[is.na(nms)] <- ""
  missing_nm <- !nzchar(nms)
  nms[missing_nm] <- paste0("series_", which(missing_nm))
  # De-duplicate so downstream factor levels (in autoplot()/tidy()) are
  # unique; duplicate column names otherwise crash factor construction.
  names(series_list) <- make.unique(as.character(nms))

  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  # Name the offending series when one of them fails. A panel can hold
  # hundreds; "`x` must have at least 3 observations" on its own leaves the
  # user to bisect the list to find which.
  run_one <- function(i) {
    tryCatch(
      cpt_detect(series_list[[i]], method = method, change_in = change_in,
                 ...),
      error = function(e) {
        stop("Series `", names(series_list)[i], "` (", i, " of ",
             length(series_list), "): ", conditionMessage(e), call. = FALSE)
      }
    )
  }

  results <- if (has_future) {
    future.apply::future_lapply(seq_along(series_list), run_one,
                                future.seed = seed %||% TRUE)
  } else {
    if (!is.null(seed)) set.seed(seed)
    lapply(seq_along(series_list), run_one)
  }
  names(results) <- names(series_list)

  out <- tibble::tibble(
    series = names(series_list),
    n_changepoints = vapply(results, function(r) nrow(r$changepoints),
                            integer(1)),
    changepoints = lapply(results, function(r) r$changepoints),
    result = results
  )
  class(out) <- c("ggcpt_batch", class(out))
  attr(out, "method") <- method
  attr(out, "change_in") <- change_in
  out
}

#' @rdname cpt_batch
#' @param object A \code{ggcpt_batch} object (for \code{autoplot()}).
#' @export
print.ggcpt_batch <- function(x, ...) {
  cat("ggcpt_batch (", nrow(x), " series, method: ",
      attr(x, "method") %||% "?", ")\n\n", sep = "")
  print(tibble::as_tibble(x[, c("series", "n_changepoints")]), n = 20)
  invisible(x)
}

#' @rdname cpt_batch
#' @export
tidy.ggcpt_batch <- function(x, ...) {
  out <- do.call(rbind, lapply(seq_len(nrow(x)), function(i) {
    cp <- x$changepoints[[i]]
    if (nrow(cp) == 0) return(NULL)
    tibble::add_column(cp[, c("cp", "cp_value")], series = x$series[i],
                       .before = 1)
  }))
  out %||% tibble::tibble(series = character(), cp = integer(),
                          cp_value = numeric())
}

#' @rdname cpt_batch
#' @export
autoplot.ggcpt_batch <- function(object, ...) {
  panel_data <- do.call(rbind, lapply(seq_len(nrow(object)), function(i) {
    d <- object$result[[i]]$data[, c("index", "value")]
    d$series <- object$series[i]
    d
  }))
  series_levels <- make.unique(as.character(object$series))
  panel_data$series <- factor(panel_data$series, levels = series_levels)

  cp_data <- tidy.ggcpt_batch(object)

  p <- ggplot2::ggplot(panel_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::facet_wrap(~series, scales = "free_y") +
    ggplot2::labs(x = "Index", y = "Value",
                  title = paste0("Batch changepoint detection (",
                                 attr(object, "method") %||% "?", ")"))

  if (nrow(cp_data) > 0) {
    cp_data$series <- factor(cp_data$series, levels = series_levels)
    p <- p + ggplot2::geom_vline(
      data = cp_data,
      ggplot2::aes(xintercept = cp),
      color = "blue", linewidth = 0.4
    )
  }
  p
}
