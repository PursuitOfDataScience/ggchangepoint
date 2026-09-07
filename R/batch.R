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
#' @param index Optional time index shared by every series in the panel (a
#'   vector of dates, say), or a named list of one index per series. Carried
#'   onto each result and used by \code{tidy()} and \code{autoplot()}, so a
#'   faceted plot of fifty series shows dates rather than positions.
#' @param seed Optional seed for reproducible parallel execution (passed to
#'   \code{future.apply::future_lapply()} as \code{future.seed}; applied via
#'   \code{set.seed()} when running sequentially). The seed is scoped to
#'   this call: \code{.Random.seed} is saved and restored, so a seeded call
#'   inside a simulation loop does not pin the loop's own stream.
#' @param keep_fit Keep each engine's raw fit in \code{result[[i]]$fit}?
#'   Defaults to \code{TRUE}, which is what makes a batch result as
#'   inspectable as a single one. Set it to \code{FALSE} for a large panel:
#'   a few engines return fits far bigger than the data they were given —
#'   \code{strucchange} keeps a triangular \eqn{O(n^2)} RSS matrix, so a
#'   single 2000-point series costs about 135 MB, and \code{bfast} and
#'   \code{bocpd} are in the tens of MB — and a panel multiplies that by the
#'   number of series. Everything else on the object, including
#'   \code{tidy()} and \code{autoplot()}, is unaffected; only accessors
#'   that read \code{$fit} (\code{cpt_statistic()},
#'   \code{ggcpt_posterior()}, \code{cpt_confint(engine = \"native\")})
#'   need it.
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
cpt_batch <- function(x, method = "pelt", change_in = "mean", index = NULL,
                      seed = NULL, keep_fit = TRUE, ...) {
  validate_flag(keep_fit, "keep_fit")
  series_list <- if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0L) {
      stop("`x` is empty: `cpt_batch()` needs at least one series.",
           call. = FALSE)
    }
    # coerce_series_values(), not a bare as.numeric(): a factor coerces to
    # its LEVEL CODES -- an alphabetical ordering of the labels rather than
    # the data -- and a character vector to NAs, both silently. cpt_detect()
    # has refused those since 0.4.0; cpt_batch() reached the engine through
    # `as.numeric()` and so accepted a whole panel of them, reporting
    # changepoints in the label ordering. The series name goes into the
    # message because a panel is exactly where "which one?" is the question.
    stats::setNames(lapply(seq_along(x), function(i) {
      nm <- names(x)[i] %||% ""
      label <- paste0("Series ", if (nzchar(nm)) paste0("`", nm, "` ") else "",
                      "(", i, " of ", length(x), ")")
      xi <- x[[i]]
      # A panel is documented as "a list of numeric vectors", and that is
      # load-bearing: as.numeric() on a matrix unrolls it column after
      # column, so an 80x2 member became a 160-point series and reported a
      # changepoint at index 80 -- the seam where the second column was
      # appended, which the data does not contain. One column is exempt
      # because unrolling it changes nothing.
      if (is.matrix(xi) || is.data.frame(xi)) {
        withCallingHandlers(
          reject_multicolumn(xi, "x",
                             paste("Pass a multivariate series to",
                                   "cpt_detect() directly, or split the",
                                   "columns into separate panel members.")),
          error = function(e) {
            stop(label, ": ", conditionMessage(e), call. = FALSE)
          })
        return(withCallingHandlers(
          as_mv_matrix(xi, arg = "x")[, 1],
          error = function(e) {
            stop(label, ": ", conditionMessage(e), call. = FALSE)
          }))
      }
      withCallingHandlers(
        coerce_series_values(xi),
        error = function(e) {
          stop(label, ": ", conditionMessage(e), call. = FALSE)
        })
    }), names(x))
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
  # One index for the whole panel, or one per series. A named list is
  # matched by name so the caller does not have to keep the order straight.
  index_for <- function(i) {
    if (is.null(index)) return(NULL)
    if (is.list(index)) {
      nm <- names(series_list)[i]
      if (!is.null(names(index)) && nm %in% names(index)) return(index[[nm]])
      if (length(index) >= i) return(index[[i]])
      return(NULL)
    }
    index
  }

  run_one <- function(i) {
    tryCatch(
      cpt_detect(series_list[[i]], method = method, change_in = change_in,
                 index = index_for(i), ...),
      error = function(e) {
        stop("Series `", names(series_list)[i], "` (", i, " of ",
             length(series_list), "): ", conditionMessage(e), call. = FALSE)
      }
    )
  }

  results <- if (has_future) {
    future.apply::future_lapply(seq_along(series_list),
                                with_session_registry(run_one),
                                future.seed = seed %||% TRUE)
  } else {
    local_seed(seed)
    lapply(seq_along(series_list), run_one)
  }
  names(results) <- names(series_list)
  if (!keep_fit) {
    results <- lapply(results, function(r) { r$fit <- NULL; r })
  }

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
  has_index <- any(vapply(x$result, function(r) !is.null(r$index),
                          logical(1)))
  keep <- if (has_index) c("cp", "cp_index", "cp_value") else {
    c("cp", "cp_value")
  }
  out <- do.call(rbind, lapply(seq_len(nrow(x)), function(i) {
    cp <- x$changepoints[[i]]
    if (nrow(cp) == 0) return(NULL)
    cols <- intersect(keep, names(cp))
    tibble::add_column(cp[, cols, drop = FALSE], series = x$series[i],
                       .before = 1)
  }))
  out %||% tibble::tibble(series = character(), cp = integer(),
                          cp_value = numeric())
}

#' @rdname cpt_batch
#' @export
autoplot.ggcpt_batch <- function(object, ...) {
  panel_data <- do.call(rbind, lapply(seq_len(nrow(object)), function(i) {
    res <- object$result[[i]]
    d <- tibble::tibble(index = plot_index(res), value = res$data$value)
    d$series <- object$series[i]
    d
  }))
  series_levels <- make.unique(as.character(object$series))
  panel_data$series <- factor(panel_data$series, levels = series_levels)
  x_lab <- plot_index_label(object$result[[1]])

  cp_data <- tidy.ggcpt_batch(object)

  p <- ggplot2::ggplot(panel_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::facet_wrap(~series, scales = "free_y") +
    ggplot2::labs(x = x_lab, y = "Value",
                  title = paste0("Batch changepoint detection (",
                                 attr(object, "method") %||% "?", ")"))

  if (nrow(cp_data) > 0) {
    cp_data$series <- factor(cp_data$series, levels = series_levels)
    # The rule has to be on the same scale as the panel, which is the time
    # index when the results carry one.
    cp_data$.at <- if ("cp_index" %in% names(cp_data)) {
      cp_data$cp_index
    } else {
      cp_data$cp
    }
    p <- p + ggplot2::geom_vline(
      data = cp_data,
      ggplot2::aes(xintercept = .at),
      color = "blue", linewidth = 0.4
    )
  }
  p
}
