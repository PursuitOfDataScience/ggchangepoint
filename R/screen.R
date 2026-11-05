# ---------------------------------------------------------------------------
# Theme AK (0.7.0): screening many series. "I have ten thousand series and
# I want the ones that changed" is the panel question cpt_batch() answers
# without a correction: a test at 0.05 on ten thousand stable series flags
# five hundred of them. cpt_screen() computes one p-value per series for
# "did anything change?" (cpt_test_null()'s tests), adjusts across the
# panel, and orders the answer by evidence.
# ---------------------------------------------------------------------------

#' Which of many series changed? Screening with error control across the panel
#'
#' Tests every series in a panel for a change anywhere, with one of
#' \code{\link{cpt_test_null}()}'s tests, adjusts the p-values across the
#' panel, and returns the series ordered by evidence with the ones that
#' survive flagged. Optionally runs a detector on the flagged series, so
#' the segmentation is spent only where there is something to segment.
#'
#' @param x A list of numeric vectors, or a matrix or data frame with one
#'   column per series, as for \code{\link{cpt_batch}()}. For
#'   \code{print()}, \code{tidy()} and \code{glance()}, a
#'   \code{ggcpt_screen} object.
#' @param test Which test gives each series its p-value:
#'   \code{"pettitt"} (default), \code{"cusum"} or \code{"supF"}; see
#'   \code{\link{cpt_test_null}()}. Pettitt's rank test is the default
#'   because a panel adjustment works in the far tail of each test's null
#'   distribution, and there its p-value errs on the safe side while the
#'   CUSUM's Kolmogorov limit does not at moderate lengths: measured on
#'   panels of 100 series of 120 observations, ten of them with a shift of
#'   one standard deviation, the false discovery rate at \code{alpha =
#'   0.05} was 0.030 with Pettitt and 0.065 with the CUSUM, for the same
#'   power (0.97 and 0.98; 300 panels). At 500 observations both held (0.039
#'   and 0.052).
#' @param alpha Error level across the panel. Defaults to \code{0.05}.
#' @param control \code{"fdr"} (default): Benjamini-Hochberg control of the
#'   false discovery rate, valid for independent series (and positively
#'   dependent ones); \code{"fdr_dependent"}: Benjamini and Yekutieli's
#'   version, valid under any dependence between series; \code{"fwer"}:
#'   Holm control of the family-wise error rate; \code{"none"}: no
#'   adjustment, which is what running each test on its own amounts to.
#' @param method Optional detection method. When given, every flagged
#'   series is segmented with \code{\link{cpt_detect}()} and the result kept
#'   in a \code{result} column. Defaults to \code{NULL} (screen only).
#' @param ... Passed to \code{\link{cpt_detect}()} when \code{method} is
#'   given.
#'
#' @section Why the panel needs its own correction:
#' Each series' p-value is valid for that series. Across a panel of
#' \eqn{m} stable series, testing each at \code{alpha} flags about
#' \eqn{m \alpha} of them, and nothing in a per-series answer says so. The
#' adjustment is over the series, so its guarantee is about the panel:
#' under \code{"fdr"}, of the series flagged, the expected share that did
#' not change is at most \code{alpha}. The tests assume independent
#' observations within a series, and autocorrelation makes every one of
#' them reject too often; \code{\link{cpt_assumptions}()} checks a single
#' fit for it.
#'
#' @return A \code{ggcpt_screen} tibble with one row per series, ordered by
#'   p-value: \code{series}, \code{rank}, \code{n}, \code{statistic},
#'   \code{p_value}, \code{p_adjusted}, \code{flagged} and \code{location}
#'   (where the test statistic peaks), plus \code{n_changepoints} and
#'   \code{result} (a list-column of \code{ggcpt} fits, \code{NULL} for
#'   series not flagged) when \code{method} is given. \code{print()},
#'   \code{autoplot()} (the sorted p-values against the rejection line),
#'   \code{tidy()} and \code{glance()} work on it.
#' @seealso \code{\link{cpt_batch}()}, which segments every series and
#'   adjusts nothing; \code{\link{cpt_test_null}()}.
#' @family inference
#' @export
#' @examples
#' set.seed(2026)
#' panel <- c(lapply(1:15, function(i) rnorm(120)),
#'            lapply(1:5, function(i) c(rnorm(60), rnorm(60, 1.5))))
#' names(panel) <- paste0("store", 1:20)
#' sc <- cpt_screen(panel)
#' sc
#' ggplot2::autoplot(sc)
cpt_screen <- function(x, test = c("pettitt", "cusum", "supF"),
                       alpha = 0.05,
                       control = c("fdr", "fdr_dependent", "fwer", "none"),
                       method = NULL, ...) {
  test <- cpt_match_arg(test)
  control <- cpt_match_arg(control)
  validate_scalar(alpha, "alpha", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  if (!is.null(method) && (!is.character(method) || length(method) != 1L ||
                           is.na(method))) {
    cpt_abort("`method` must be one method name, or NULL to screen only.",
              class = "bad_argument")
  }
  series <- batch_series_list(x, caller = "cpt_screen()")
  rows <- lapply(seq_along(series), function(i) {
    v <- series[[i]]
    res <- withCallingHandlers(
      cpt_test_null(v, method = test),
      error = function(e) {
        cpt_rethrow(e, "Series `", names(series)[i], "`: ")
      })
    tibble::tibble(series = names(series)[i], n = length(v),
                   statistic = res$statistic, p_value = res$p_value,
                   location = res$location)
  })
  out <- do.call(rbind, rows)
  out$p_adjusted <- switch(control,
    fdr = stats::p.adjust(out$p_value, method = "BH"),
    fdr_dependent = stats::p.adjust(out$p_value, method = "BY"),
    fwer = stats::p.adjust(out$p_value, method = "holm"),
    none = out$p_value
  )
  out$flagged <- !is.na(out$p_adjusted) & out$p_adjusted <= alpha
  ord <- order(out$p_value, out$series)
  out <- out[ord, , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  out <- out[, c("series", "rank", "n", "statistic", "p_value", "p_adjusted",
                 "flagged", "location")]
  if (!is.null(method)) {
    fits <- lapply(seq_len(nrow(out)), function(i) {
      if (!out$flagged[i]) return(NULL)
      withCallingHandlers(
        cpt_detect(series[[out$series[i]]], method = method, ...),
        error = function(e) {
          cpt_rethrow(e, "Series `", out$series[i], "`: ")
        })
    })
    out$n_changepoints <- vapply(fits, function(f) {
      if (is.null(f)) NA_integer_ else nrow(f$changepoints)
    }, integer(1))
    out$result <- fits
  }
  structure(out, class = c("ggcpt_screen", class(tibble::tibble())),
            test = test, alpha = alpha, control = control,
            method = method)
}

#' @rdname cpt_screen
#' @param object A \code{ggcpt_screen} object, for \code{autoplot()}.
#' @export
print.ggcpt_screen <- function(x, ...) {
  ctl <- switch(attr(x, "control") %||% "fdr",
    fdr = "false discovery rate (Benjamini-Hochberg)",
    fdr_dependent = "false discovery rate (Benjamini-Yekutieli)",
    fwer = "family-wise error (Holm)",
    none = "nothing (each series at its own level)")
  cat("ggcpt_screen (", attr(x, "test"), " test on each series)\n", sep = "")
  cat_field("Series", nrow(x))
  cat_field("Controls", paste0(ctl, " at ", format(attr(x, "alpha"))))
  cat_field("Flagged", sum(x$flagged))
  cat("\n")
  print(tibble::as_tibble(x[, setdiff(names(x), "result")]), n = 10)
  invisible(x)
}

#' @rdname cpt_screen
#' @export
tidy.ggcpt_screen <- function(x, ...) {
  tibble::as_tibble(x[, setdiff(names(x), "result")])
}

#' @rdname cpt_screen
#' @export
glance.ggcpt_screen <- function(x, ...) {
  tibble::tibble(n_series = nrow(x), n_flagged = sum(x$flagged),
                 alpha = attr(x, "alpha") %||% NA_real_,
                 control = attr(x, "control") %||% NA_character_,
                 test = attr(x, "test") %||% NA_character_)
}

#' @rdname cpt_screen
#' @export
autoplot.ggcpt_screen <- function(object, ...) {
  d <- tibble::as_tibble(object[, setdiff(names(object), "result")])
  m <- nrow(d)
  a <- attr(object, "alpha")
  line <- switch(attr(object, "control") %||% "fdr",
    fdr = a * seq_len(m) / m,
    fdr_dependent = a * seq_len(m) / (m * sum(1 / seq_len(m))),
    fwer = a / (m - seq_len(m) + 1),
    none = rep(a, m))
  d$line <- line
  d$status <- ifelse(d$flagged, "flagged", "not flagged")
  ggplot2::ggplot(d, ggplot2::aes(x = rank, y = p_value)) +
    ggplot2::geom_line(ggplot2::aes(y = line), colour = "#D55E00",
                       linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(colour = status), size = 2) +
    ggplot2::scale_colour_manual(values = c(flagged = "#5e3c99",
                                            `not flagged` = "grey60"),
                                 name = NULL) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Series, by evidence", y = "p-value (log scale)",
                  title = "Screening the panel",
                  subtitle = paste0("Dashed: the rejection line; ", sum(d$flagged),
                                    " of ", m, " series flagged")) +
    theme_ggcpt()
}
