# ---------------------------------------------------------------------------
# Theme L: communication -- the last mile.
#
# "Changepoint at index 147" is not a finding. "Changepoint at 2020-03-11,
# matching the WHO pandemic declaration" is. And the changepoints that match
# nothing are where the interesting analysis starts, so the three-way output
# (matched / unexplained / undetected) is the point of the function, not a
# by-product of it.
# ---------------------------------------------------------------------------

#' Match detected changepoints to known events
#'
#' Joins a table of real-world events to a detection result and reports all
#' three outcomes: changepoints an event explains, changepoints no event
#' explains, and events no changepoint found. Matching reuses the same
#' tolerance rule as \code{\link{cpt_metrics}()} and
#' \code{\link{cpt_consensus}()}, so the package has one notion of "close
#' enough" throughout.
#'
#' @param object A \code{ggcpt} object.
#' @param events A data frame of events with a location column and a label
#'   column. The location may be on the position scale or — when the result
#'   carries a time index — on the index scale (dates, say); which one is
#'   detected automatically from the column's type and reported.
#' @param location Name of the location column. Defaults to the first column
#'   whose type matches the result's index (or the first numeric column).
#' @param label Name of the label column. Defaults to the first character or
#'   factor column.
#' @param tolerance Matching window in positions. Defaults to \code{5}.
#'
#' @return A \code{ggcpt_events} object: a list with
#'   \describe{
#'     \item{\code{matched}}{one row per matched pair: \code{cp},
#'       \code{event}, \code{event_position}, \code{distance}.}
#'     \item{\code{unexplained}}{detected changepoints with no event.}
#'     \item{\code{undetected}}{events with no changepoint.}
#'   }
#'   with \code{print()}, \code{tidy()} and \code{autoplot()}.
#' @seealso \code{\link{geom_cpt_event}()}, \code{\link{cpt_report}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 4))
#' dates <- as.Date("2020-01-01") + 0:119
#' fit <- cpt_detect(x, method = "pelt", index = dates)
#' events <- data.frame(when = as.Date(c("2020-03-01", "2020-04-15")),
#'                      what = c("policy change", "supply shock"))
#' cpt_annotate_events(fit, events)
cpt_annotate_events <- function(object, events, location = NULL,
                                label = NULL, tolerance = 5) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  events <- as.data.frame(events)
  if (nrow(events) == 0) {
    stop("`events` has no rows.", call. = FALSE)
  }
  validate_scalar(tolerance, "tolerance", min = 0)

  idx <- object$index
  n <- nrow(object$data)

  if (is.null(location)) {
    location <- guess_event_column(events, idx)
  }
  if (!location %in% names(events)) {
    stop("`location = \"", location, "\"` is not a column of `events` ",
         "(columns: ", paste(names(events), collapse = ", "), ").",
         call. = FALSE)
  }
  if (is.null(label)) {
    cand <- names(events)[vapply(events, function(v) {
      is.character(v) || is.factor(v)
    }, logical(1))]
    if (length(cand) == 0) {
      events$.label <- paste0("event ", seq_len(nrow(events)))
      label <- ".label"
    } else {
      label <- cand[1]
    }
  }

  raw <- events[[location]]
  # Two scales are possible: positions, or the result's own index. Deciding
  # by class rather than by magnitude means a Date column is never read as a
  # position, and a position column is never re-looked-up in a Date index.
  on_index <- !is.null(idx) && identical(class(raw)[1], class(idx)[1]) &&
    !is.numeric(idx)
  if (!on_index && !is.null(idx) && is.numeric(idx) && is.numeric(raw) &&
      (min(raw, na.rm = TRUE) < 1 || max(raw, na.rm = TRUE) > n)) {
    # A numeric index whose events fall outside 1..n can only be on the
    # index scale.
    on_index <- TRUE
  }
  event_pos <- if (on_index) {
    vapply(raw, function(v) {
      d <- abs(as.numeric(idx) - as.numeric(v))
      if (all(is.na(d))) NA_integer_ else as.integer(which.min(d))
    }, integer(1))
  } else {
    as.integer(round(as.numeric(raw)))
  }

  ev <- tibble::tibble(
    event = as.character(events[[label]]),
    event_value = raw,
    event_position = event_pos
  )
  ev <- ev[!is.na(ev$event_position), , drop = FALSE]

  cp <- object$changepoints$cp
  m <- match_changepoints(cp, ev$event_position, tolerance)

  matched <- if (nrow(m) > 0) {
    rows <- lapply(seq_len(nrow(m)), function(i) {
      j <- which(ev$event_position == m$truth[i])[1]
      tibble::tibble(cp = as.integer(m$pred[i]),
                     cp_index = if (is.null(idx)) NA else idx[m$pred[i]],
                     event = ev$event[j],
                     event_value = ev$event_value[j],
                     event_position = as.integer(m$truth[i]),
                     distance = as.integer(abs(m$pred[i] - m$truth[i])))
    })
    do.call(rbind, rows)
  } else {
    tibble::tibble(cp = integer(), cp_index = if (is.null(idx)) logical() else idx[0],
                   event = character(), event_value = ev$event_value[0],
                   event_position = integer(), distance = integer())
  }

  unexplained_cp <- setdiff(cp, matched$cp)
  unexplained <- tibble::tibble(
    cp = as.integer(unexplained_cp),
    cp_index = if (is.null(idx)) NA else idx[unexplained_cp]
  )
  undetected <- ev[!ev$event_position %in% matched$event_position, ,
                   drop = FALSE]

  structure(
    list(matched = matched, unexplained = unexplained,
         undetected = undetected, object = object, tolerance = tolerance,
         on_index = on_index),
    class = "ggcpt_events"
  )
}

# Internal: pick the events column holding the location. A column of the
# same class as the result's index wins; otherwise the first numeric one.
#' @noRd
guess_event_column <- function(events, idx) {
  if (!is.null(idx)) {
    same <- names(events)[vapply(events, function(v) {
      identical(class(v)[1], class(idx)[1])
    }, logical(1))]
    if (length(same) > 0) return(same[1])
  }
  num <- names(events)[vapply(events, is.numeric, logical(1))]
  if (length(num) == 0) {
    stop("Could not find a location column in `events`: none is numeric or ",
         "matches the result's index type. Name it with `location =`.",
         call. = FALSE)
  }
  num[1]
}

#' @rdname cpt_annotate_events
#' @param x A \code{ggcpt_events} object.
#' @param ... Ignored.
#' @export
print.ggcpt_events <- function(x, ...) {
  cat("ggcpt_events (tolerance ", x$tolerance, " position(s))\n", sep = "")
  cat("  Changepoints explained by an event: ", nrow(x$matched), "\n",
      sep = "")
  cat("  Changepoints with no event:         ", nrow(x$unexplained), "\n",
      sep = "")
  cat("  Events with no changepoint:         ", nrow(x$undetected), "\n",
      sep = "")
  if (nrow(x$matched) > 0) {
    cat("\nMatched:\n")
    print(x$matched, n = 10)
  }
  if (nrow(x$unexplained) > 0) {
    cat("\nUnexplained changepoints (where the interesting analysis ",
        "starts):\n", sep = "")
    print(x$unexplained, n = 10)
  }
  if (nrow(x$undetected) > 0) {
    cat("\nEvents the detector did not find:\n")
    print(x$undetected[, c("event", "event_value", "event_position")], n = 10)
  }
  invisible(x)
}

#' @rdname cpt_annotate_events
#' @export
tidy.ggcpt_events <- function(x, ...) {
  rows <- list()
  if (nrow(x$matched) > 0) {
    rows[[1]] <- tibble::tibble(status = "matched", cp = x$matched$cp,
                                event = x$matched$event,
                                position = x$matched$event_position,
                                distance = x$matched$distance)
  }
  if (nrow(x$unexplained) > 0) {
    rows[[2]] <- tibble::tibble(status = "unexplained_changepoint",
                                cp = x$unexplained$cp,
                                event = NA_character_,
                                position = x$unexplained$cp,
                                distance = NA_integer_)
  }
  if (nrow(x$undetected) > 0) {
    rows[[3]] <- tibble::tibble(status = "undetected_event",
                                cp = NA_integer_,
                                event = x$undetected$event,
                                position = x$undetected$event_position,
                                distance = NA_integer_)
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(tibble::tibble(status = character(), cp = integer(),
                          event = character(), position = integer(),
                          distance = integer()))
  }
  do.call(rbind, rows)
}

#' @rdname cpt_annotate_events
#' @param object A \code{ggcpt_events} object (for \code{autoplot()}).
#' @param repel Use \pkg{ggrepel} for the event labels? Defaults to
#'   \code{TRUE} when it is installed.
#' @export
autoplot.ggcpt_events <- function(object, repel = NULL, ...) {
  fit <- object$object
  idx_vals <- plot_index(fit)
  p <- autoplot.ggcpt(fit, ...)

  ev <- rbind(
    if (nrow(object$matched) > 0) {
      tibble::tibble(pos = object$matched$event_position,
                     label = object$matched$event, status = "matched")
    },
    if (nrow(object$undetected) > 0) {
      tibble::tibble(pos = object$undetected$event_position,
                     label = object$undetected$event,
                     status = "undetected")
    }
  )
  if (is.null(ev) || nrow(ev) == 0) return(p)
  ev$x <- idx_vals[ev$pos]
  ev$y <- min(fit$data$value, na.rm = TRUE)

  has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
  if (is.null(repel)) repel <- has_ggrepel

  p +
    ggplot2::geom_vline(data = ev,
                        ggplot2::aes(xintercept = x, linetype = status),
                        colour = "grey30") +
    # inherit.aes = FALSE: the event frame has its own columns, and the
    # series plot maps `raw_value`, which is not one of them.
    (if (isTRUE(repel) && has_ggrepel) {
      ggrepel::geom_text_repel(data = ev,
                               ggplot2::aes(x = x, y = y, label = label),
                               inherit.aes = FALSE, size = 3,
                               colour = "grey20", direction = "y",
                               na.rm = TRUE)
    } else {
      ggplot2::geom_text(data = ev,
                         ggplot2::aes(x = x, y = y, label = label),
                         inherit.aes = FALSE, size = 3, colour = "grey20",
                         angle = 90, hjust = 0, vjust = -0.4, na.rm = TRUE)
    }) +
    ggplot2::scale_linetype_manual(values = c(matched = "dotted",
                                              undetected = "longdash"),
                                   name = "Event") +
    ggplot2::labs(subtitle = paste0(nrow(object$unexplained),
                                    " changepoint(s) unexplained, ",
                                    nrow(object$undetected),
                                    " event(s) undetected"))
}

#' A reproducible report of a changepoint analysis
#'
#' Assembles everything a reader needs to judge a changepoint result into one
#' artifact: the method and its citation, the penalty, the number and
#' locations of the changepoints with intervals where available, the segment
#' table, stability and diagnostics if they were computed, and the session
#' information. Reproducibility and correct attribution in one call.
#'
#' @param object A \code{ggcpt} object.
#' @param format \code{"md"} (default, GitHub-flavoured markdown as a
#'   character vector), \code{"text"} (plain text) or \code{"gt"} (a
#'   \pkg{gt} table of the changepoints, for a publication).
#' @param file Optional path to write to. The report is returned invisibly
#'   when a file is written. Ignored for \code{format = "gt"}, which returns
#'   a table object rather than lines of text.
#' @param stability Optional \code{\link{cpt_stability}()} result to include.
#' @param events Optional \code{\link{cpt_annotate_events}()} result to
#'   include.
#' @param confint Include a \code{\link{cpt_confint}()} table? Defaults to
#'   \code{TRUE}, which uses the engine's own intervals when it has them and
#'   skips the section otherwise (rather than silently bootstrapping, which
#'   would be slow and unexpected inside a report).
#' @param session Include \code{sessionInfo()}? Defaults to \code{TRUE}.
#'
#' @return A character vector of report lines (or a \pkg{gt} table when
#'   \code{format = "gt"}).
#' @seealso \code{\link{cpt_gt}()}, \code{\link{cpt_cite}()}.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt")
#' cat(cpt_report(fit, session = FALSE), sep = "\n")
cpt_report <- function(object, format = c("md", "text", "gt"), file = NULL,
                       stability = NULL, events = NULL, confint = TRUE,
                       session = TRUE) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  format <- match.arg(format)
  validate_flag(confint, "confint")
  validate_flag(session, "session")
  if (format == "gt") return(cpt_gt(object))

  h <- function(txt, lvl = 2) {
    if (format == "md") paste0(strrep("#", lvl), " ", txt) else toupper(txt)
  }
  bullet <- function(txt) if (format == "md") paste0("- ", txt) else paste0("  ", txt)
  fenced <- function(lines) {
    if (format == "md") c("```", lines, "```") else lines
  }

  g <- glance.ggcpt(object)
  out <- c(
    h("Changepoint analysis report", 1),
    "",
    bullet(paste0("Method: `", object$method, "`",
                  if (isTRUE(object$registered)) " (user-registered)" else "")),
    bullet(paste0("Change in: ", object$change_in)),
    bullet(paste0("Penalty: ", format_penalty(object$penalty))),
    bullet(paste0("Series length: ", g$n)),
    bullet(paste0("Changepoints found: ", g$n_changepoints)),
    if (!is.null(object$index)) {
      bullet(paste0("Index: ", format_index_range(object$index)))
    },
    if (is.finite(g$runtime)) {
      bullet(paste0("Detection runtime: ", format(g$runtime, digits = 3),
                    " s"))
    },
    ""
  )

  out <- c(out, h("Changepoints"), "",
           fenced(utils::capture.output(print(object$changepoints,
                                              n = 50))), "")

  if (!is.null(object$regions)) {
    out <- c(out, h("Significance regions"), "",
             paste0("Each region contains at least one changepoint",
                    if (!is.null(object$region_level)) {
                      paste0(" at global level ",
                             format(object$region_level))
                    } else "", "."),
             "",
             fenced(utils::capture.output(print(cpt_regions(object),
                                                n = 50))), "")
  }

  if (isTRUE(confint) &&
      all(c("ci_lower", "ci_upper") %in% names(object$changepoints))) {
    ci <- cpt_confint(object, method = "native")
    out <- c(out, h("Location intervals"), "",
             fenced(utils::capture.output(print(ci, n = 50))), "")
  }

  out <- c(out, h("Segments"), "",
           fenced(utils::capture.output(print(object$segments, n = 50))), "")

  if (!is.null(stability)) {
    if (!inherits(stability, "ggcpt_stability")) {
      stop("`stability` must be a cpt_stability() result.", call. = FALSE)
    }
    out <- c(out, h("Stability"), "",
             fenced(utils::capture.output(print(stability))), "")
  }
  if (!is.null(events)) {
    if (!inherits(events, "ggcpt_events")) {
      stop("`events` must be a cpt_annotate_events() result.", call. = FALSE)
    }
    out <- c(out, h("Events"), "",
             fenced(utils::capture.output(print(events))), "")
  }

  ref <- tryCatch(utils::capture.output(cpt_cite(object)),
                  error = function(e) NULL)
  if (!is.null(ref)) {
    out <- c(out, h("Citation"), "", ref, "")
  }
  out <- c(out, h("Reproducibility"), "",
           fenced(c("Call:", utils::capture.output(print(object$call)))), "")
  if (isTRUE(session)) {
    out <- c(out,
             fenced(utils::capture.output(print(utils::sessionInfo()))), "")
  }

  # `c()` has already dropped the NULLs the conditional lines produce, so
  # this only flattens.
  out <- as.character(unlist(out))
  if (!is.null(file)) {
    writeLines(out, file)
    return(invisible(out))
  }
  out
}

#' A publication-ready changepoint table
#'
#' Renders the changepoints of a result as a \pkg{gt} table, with the time
#' index, location intervals and segment levels formatted for a paper. Falls
#' back to a plain tibble, with a note, when \pkg{gt} is not installed.
#'
#' @param object A \code{ggcpt} object.
#' @param title,subtitle Table title and subtitle. Sensible defaults are
#'   derived from the result.
#' @param digits Digits for the numeric columns. Defaults to \code{3}.
#' @return A \pkg{gt} table, or a tibble when \pkg{gt} is unavailable.
#' @seealso \code{\link{cpt_report}()}.
#' @export
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' set.seed(2026)
#' cpt_gt(cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt"))
cpt_gt <- function(object, title = NULL, subtitle = NULL, digits = 3) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  validate_scalar(digits, "digits", min = 0)
  cp <- object$changepoints
  seg <- object$segments

  tbl <- tibble::tibble(
    `#` = seq_len(nrow(cp)),
    Location = cp$cp
  )
  if ("cp_index" %in% names(cp)) tbl[["Time"]] <- cp$cp_index
  if (all(c("ci_lower", "ci_upper") %in% names(cp))) {
    tbl[["Interval"]] <- paste0("[", cp$ci_lower, ", ", cp$ci_upper, "]")
  }
  tbl[["Value at change"]] <- round(cp$cp_value, digits)
  if (nrow(seg) == nrow(cp) + 1L) {
    tbl[["Level before"]] <- round(seg$param_estimate[-nrow(seg)], digits)
    tbl[["Level after"]] <- round(seg$param_estimate[-1], digits)
    tbl[["Change"]] <- round(diff(seg$param_estimate), digits)
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    message("Package 'gt' is not installed, so the plain tibble is returned. ",
            "Install it with install.packages('gt') for a formatted table.")
    return(tbl)
  }
  gt::tab_header(
    gt::gt(tbl),
    title = title %||% paste0("Changepoints (", object$method, ")"),
    subtitle = subtitle %||% paste0(
      nrow(cp), " changepoint(s) in ", object$change_in, " over ",
      nrow(object$data), " observations; penalty ",
      format_penalty(object$penalty)
    )
  )
}
