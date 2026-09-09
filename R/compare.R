#' Compare multiple changepoint detection methods
#'
#' Runs several detectors on the same data and returns a faceted or overlaid
#' ggplot comparison. Respects \code{future::plan()} for parallel execution
#' if the \code{future.apply} package is available.
#'
#' @param x A numeric vector (the data series). A one-column matrix or data
#'   frame is accepted; wider input is refused, because these detectors are
#'   univariate and flattening the columns would invent a changepoint at
#'   every seam. Use \code{\link{cpt_batch}()} for a panel of series.
#' @param methods Character vector of method names (passed to \code{cpt_detect}).
#' @param layout Layout type. \code{"facet"} (default) shows one panel per method;
#'   \code{"overlay"} draws all changepoints in one panel, colour-coded.
#'   The overlay \strong{dodges} the rules horizontally so that two methods
#'   agreeing on an index are both visible, which moves each rule by up to
#'   half an observation --- so read positions off \code{"facet"}, or off
#'   \code{tidy()}, and treat the overlay as a picture of agreement rather
#'   than of location.
#' @param change_in What to detect change in. Passed to each detector.
#' @param seed Optional seed for reproducible parallelism. Passed to
#'   \code{future.apply::future_lapply()} as \code{future.seed}, and to
#'   \code{set.seed()} when running sequentially. Left \code{NULL} under a
#'   parallel plan, \code{future.seed = TRUE} is used, so the workers get
#'   parallel-safe streams but the run is not reproducible. The seed is
#'   scoped to this call: \code{.Random.seed} is saved and restored, so a
#'   seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Additional arguments passed to each detector.
#'
#' @section Positions, not a time index:
#' Unlike \code{\link{cpt_detect}()}, \code{\link{cpt_batch}()} and
#' \code{\link{autoplot.ggcpt}()}, these two take no \code{index}: the
#' input is reduced to a bare numeric vector, so a \code{ts}, \code{xts},
#' \code{zoo} or \code{tsibble} is plotted (and tabulated) in observation
#' positions with an "Index" axis. To compare detectors on dated data, run
#' \code{cpt_detect(x, method = m, index = dates)} per method and read
#' \code{tidy()}'s \code{cp_index}, or plot the results with
#' \code{autoplot()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' set.seed(2022)
#' x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
#' ggcpt_compare(x, methods = c("pelt", "binseg"))
ggcpt_compare <- function(x,
                          methods = c("pelt", "binseg", "amoc"),
                          layout = c("facet", "overlay"),
                          change_in = "mean",
                          seed = NULL,
                          ...) {

  layout <- match.arg(layout)
  # De-duplicate methods so repeated names do not crash factor construction
  # (duplicated factor levels) downstream.
  methods <- unique(methods)
  data_vec <- compare_input(x, "ggcpt_compare")

  # Attempt parallel execution if future is set up
  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  # Above the branch: future.apply forwards the caller's RNG state one step
  # for every `future.seed` value except FALSE/NULL, so the parallel path
  # perturbs `.Random.seed` as well and needs the same restore. See the note
  # in cpt_batch().
  local_seed(seed)
  if (has_future) {
    # `future.seed` takes a logical, an integer, or a list of seeds -- NULL
    # is not one of its documented values, and `seed = NULL` is the default
    # here, so the parallel branch passed one every time it ran without an
    # explicit seed. `cpt_batch()` already sent TRUE in that case, which is
    # what asks future.apply for parallel-safe L'Ecuyer streams.
    run_one <- function(m) {
      cpt_detect(data_vec, method = m, change_in = change_in, ...)
    }
    results <- future.apply::future_lapply(methods,
                                           with_session_registry(run_one),
                                           future.seed = seed %||% TRUE)
  } else {
    results <- lapply(methods, function(m) {
      cpt_detect(data_vec, method = m, change_in = change_in, ...)
    })
  }

  names(results) <- methods

  if (layout == "facet") {
    ggcpt_compare_facet(data_vec, results, methods)
  } else {
    ggcpt_compare_overlay(data_vec, results, methods)
  }
}

# Internal: vertical extent used to pad changepoint rules. A flat series has
# zero range, which would collapse the rules to invisible zero-height
# segments; fall back to 1 as ggcptplot_internal() does.
#' @noRd
pad_range <- function(v) {
  r <- diff(range(v, na.rm = TRUE))
  if (!is.finite(r) || r == 0) 1 else r
}

ggcpt_compare_facet <- function(data_vec, results, methods) {
  # One panel per method, whether or not it found changepoints — a method
  # that ran and found nothing is a result, not a missing panel.
  plot_data <- do.call(rbind, lapply(methods, function(m) {
    tibble::tibble(
      index = seq_along(data_vec),
      value = data_vec,
      method = m
    )
  }))
  plot_data$method <- factor(plot_data$method, levels = methods)

  cp_data <- do.call(rbind, lapply(methods, function(m) {
    cp <- results[[m]]$changepoints
    if (nrow(cp) == 0) return(NULL)
    tibble::tibble(
      index = cp$cp,
      value = data_vec[cp$cp],
      method = m
    )
  }))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey50") +
    # `free_y` is a no-op today -- every panel plots the SAME series, so
    # every panel's free range is identical -- and it is deliberately left
    # in: the changepoint linerange below uses one global ymin/ymax, which
    # lines up only because the ranges coincide, so the day the panels are
    # allowed to differ this argument is what keeps the rules inside them.
    ggplot2::facet_wrap(~method, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "Changepoint Detection Comparison")

  if (!is.null(cp_data)) {
    pad <- 0.05 * pad_range(data_vec)
    ymin <- min(data_vec) - pad
    ymax <- max(data_vec) + pad
    cp_data <- dplyr::mutate(cp_data, .ymin = ymin, .ymax = ymax)
    cp_data$method <- factor(cp_data$method, levels = methods)
    p <- p + ggplot2::geom_linerange(
      data = cp_data,
      ggplot2::aes(x = index, ymin = .ymin, ymax = .ymax),
      inherit.aes = FALSE, color = "blue", linewidth = 0.5
    )
  }

  p
}

ggcpt_compare_overlay <- function(data_vec, results, methods) {
  plot_data <- tibble::tibble(
    index = seq_along(data_vec),
    value = data_vec
  )

  pad <- 0.05 * pad_range(data_vec)
  ymin <- min(data_vec) - pad
  ymax <- max(data_vec) + pad

  cp_data <- do.call(rbind, lapply(methods, function(m) {
    cp <- results[[m]]$changepoints
    if (nrow(cp) == 0) return(NULL)
    tibble::tibble(
      index = cp$cp,
      method = m
    )
  }))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(index, value)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "Changepoint Detection Comparison",
                  color = "Method")

  if (!is.null(cp_data)) {
    cp_data <- dplyr::mutate(cp_data, .ymin = ymin, .ymax = ymax)
    p <- p + ggplot2::geom_linerange(
      data = cp_data,
      ggplot2::aes(x = index, ymin = .ymin, ymax = .ymax, color = method,
                   linetype = method),
      inherit.aes = FALSE, linewidth = 0.5,
      # Deliberate, and documented in @param layout: two methods that agree
    # on an index would otherwise draw one rule on top of the other. The
    # cost is that a dodged rule is up to half an observation off its
    # reported position, which is why the facet layout is the default.
    position = ggplot2::position_dodge(width = 1)
    ) +
      # Colour-vision-safe hues, with linetype carrying the same information
      # so the panel is readable in greyscale and to a colour-blind reader.
      scale_colour_cpt(name = "Method") +
      scale_linetype_cpt(name = "Method")
  }

  p
}

# Internal: both comparison entry points run *univariate* detectors, but each
# took `as.numeric(x)` on trust. A two-column matrix was unrolled column after
# column and the join between the columns then read as a level shift: a 160x2
# input reported changepoints at 80 and 160, and 160 is the seam, not a
# feature of either series. Every wrapper already refuses wide input through
# as_uni_vector(); these two did not, and non-numeric input reached
# as.numeric() to fail with "cannot coerce type 'object'".
#' @noRd
compare_input <- function(x, fn) {
  validate_data(x)
  if (is.matrix(x) || is.data.frame(x)) {
    nc <- ncol(as.matrix(x))
    if (nc > 1) {
      stop("`", fn, "()` compares univariate detectors, but `x` has ", nc,
           " columns. Pass a single series, or use `cpt_batch()` to run a ",
           "detector over every column.", call. = FALSE)
    }
  }
  as_uni_vector(x, fn)
}

#' Comparison table
#'
#' Returns a tidy tibble combining the results of multiple detectors on
#' the same series.
#'
#' @param x A numeric vector (the data series). A one-column matrix or data
#'   frame is accepted; wider input is refused, because these detectors are
#'   univariate and flattening the columns would invent a changepoint at
#'   every seam. Use \code{\link{cpt_batch}()} for a panel of series.
#' @param methods Character vector of method names.
#' @param change_in What to detect change in.
#' @param ... Additional arguments passed to each detector.
#'
#' @return A tibble with columns \code{method}, \code{cp}, \code{cp_value}.
#' @export
#' @family plotting
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(100), rnorm(100, 5))
#' ggcpt_compare_table(x, methods = c("pelt", "binseg"))
ggcpt_compare_table <- function(x,
                                methods = c("pelt", "binseg", "amoc"),
                                change_in = "mean",
                                ...) {
  data_vec <- compare_input(x, "ggcpt_compare_table")

  do.call(rbind, lapply(methods, function(m) {
    res <- cpt_detect(data_vec, method = m, change_in = change_in, ...)
    tbl_cp <- res$changepoints
    if (nrow(tbl_cp) == 0) {
      return(tibble::tibble(method = m, cp = NA_integer_, cp_value = NA_real_))
    }
    tibble::tibble(method = m, cp = tbl_cp$cp, cp_value = tbl_cp$cp_value)
  }))
}
