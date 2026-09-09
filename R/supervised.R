# ---------------------------------------------------------------------------
# Theme D: supervised changepoint detection.
#
# Hocking's paradigm: an expert marks regions of the series as containing a
# change or not, accuracy is measured in *label errors* rather than by an
# information criterion, and the penalty is LEARNED by max-margin interval
# regression instead of assumed. The central object is a rectangle drawn over
# a time series, which is why it belongs in a ggplot2-native package.
#
# The label representation here is deliberately the same shape as the
# annotations cpt_metrics_annotated() already consumes, so the package has
# one notion of "ground truth" (open question 5 of the roadmap).
# ---------------------------------------------------------------------------

#' Changepoint labels
#'
#' Builds the tidy label set that the supervised functions consume: one row
#' per labelled region, each asserting how many changepoints that stretch of
#' the series contains.
#'
#' @param start,end Integer vectors of region boundaries (positions,
#'   inclusive).
#' @param change What the region asserts, recycled to length:
#'   \describe{
#'     \item{\code{"change"}}{at least one changepoint lies in the region.}
#'     \item{\code{"one_change"}}{exactly one does — a stricter label, and
#'       the one that makes false positives detectable inside a positive
#'       region.}
#'     \item{\code{"no_change"}}{none does.}
#'   }
#' @param series Optional series identifier, for label sets spanning a panel.
#'
#' @return A \code{cpt_labels} tibble with columns \code{label_id},
#'   \code{series}, \code{start}, \code{end}, \code{change}.
#' @seealso \code{\link{cpt_label_error}()}, \code{\link{geom_cpt_label}()},
#'   \code{\link{cpt_learn_penalty}()},
#'   \code{\link{as_cpt_labels}()} to convert a plain changepoint set.
#' @references
#' \insertRef{hocking2013penalties}{ggchangepoint}
#' @export
#' @examples
#' cpt_labels(c(40, 70), c(60, 90), c("change", "no_change"))
cpt_labels <- function(start, end, change = "change", series = NA_character_) {
  start <- as_cp_locations(start, "start")
  end <- as_cp_locations(end, "end")
  if (length(start) != length(end)) {
    stop("`start` and `end` must be the same length (", length(start),
         " vs ", length(end), ").", call. = FALSE)
  }
  if (length(start) == 0) {
    return(new_cpt_labels(tibble::tibble(
      label_id = integer(), series = character(), start = integer(),
      end = integer(), change = character()
    )))
  }
  change <- rep(as.character(change), length.out = length(start))
  bad <- setdiff(unique(change), c("change", "one_change", "no_change"))
  if (length(bad) > 0) {
    stop("Unknown label(s): ", paste(bad, collapse = ", "),
         ". Use \"change\", \"one_change\" or \"no_change\".", call. = FALSE)
  }
  if (any(end < start)) {
    stop("Every label must have `end >= start`; ", sum(end < start),
         " do not.", call. = FALSE)
  }
  out <- tibble::tibble(
    label_id = seq_along(start),
    series = rep(as.character(series), length.out = length(start)),
    start = start, end = end, change = change
  )
  new_cpt_labels(out[order(out$series, out$start), , drop = FALSE])
}

#' @noRd
#' @rdname cpt_labels
#' @export
tidy.cpt_labels <- function(x, ...) {
  tibble::as_tibble(unclass_keep_tbl(x))
}

#' @noRd
new_cpt_labels <- function(x) {
  class(x) <- unique(c("cpt_labels", class(x)))
  x
}

#' Coerce annotations to changepoint labels
#'
#' Turns a plain ground-truth changepoint set — the kind
#' \code{\link{cpt_metrics}()} takes — into labelled regions, so one
#' annotation can drive both the metric and the supervised machinery. Each
#' true changepoint becomes a \code{"one_change"} region of width
#' \code{2 * margin + 1}, and the stretches between them become
#' \code{"no_change"} regions.
#'
#' @param truth Integer vector of true changepoint positions.
#' @param n Series length.
#' @param margin Half-width of the positive regions. Defaults to \code{5},
#'   matching \code{cpt_metrics()}'s default tolerance.
#' @param negatives Add the \code{"no_change"} regions between the positives?
#'   Defaults to \code{TRUE}; without them a detector is never penalised for
#'   a false positive.
#' @param series Optional series identifier.
#' @return A \code{cpt_labels} tibble.
#' @export
#' @examples
#' as_cpt_labels(c(50, 120), n = 200)
as_cpt_labels <- function(truth, n, margin = 5, negatives = TRUE,
                          series = NA_character_) {
  validate_scalar(n, "n", min = 2)
  validate_scalar(margin, "margin", min = 0)
  validate_flag(negatives, "negatives")
  truth <- as_cp_locations(truth, "truth", sort = TRUE)
  truth <- truth[truth >= 1 & truth < n]
  n <- as.integer(n)
  margin <- as.integer(margin)

  pos_start <- pmax(1L, truth - margin)
  pos_end <- pmin(n - 1L, truth + margin)
  starts <- pos_start
  ends <- pos_end
  changes <- rep("one_change", length(truth))

  if (negatives) {
    edges <- sort(c(0L, as.vector(rbind(pos_start - 1L, pos_end + 1L)), n))
    # The gaps between consecutive positive regions carry no changepoint.
    gap_start <- c(1L, pos_end + 1L)
    gap_end <- c(pos_start - 1L, n - 1L)
    keep <- gap_end >= gap_start & gap_end - gap_start >= 1L
    starts <- c(starts, gap_start[keep])
    ends <- c(ends, gap_end[keep])
    changes <- c(changes, rep("no_change", sum(keep)))
  }
  cpt_labels(starts, ends, changes, series = series)
}

#' Score a segmentation against labels
#'
#' Counts label errors: a positive region with no changepoint is a false
#' negative, a negative region with one is a false positive, and a
#' \code{"one_change"} region with two or more is a false positive as well.
#' This is the accuracy measure supervised changepoint detection is built on,
#' and — unlike an information criterion — it is defined by what the expert
#' asserted rather than by a model assumption.
#'
#' @param object A \code{ggcpt} object, or an integer vector of changepoint
#'   positions.
#' @param labels A \code{cpt_labels} tibble (or anything with
#'   \code{start}/\code{end}/\code{change} columns).
#'
#' @return A tibble with one row per label — \code{label_id},
#'   \code{series} (the label set's series identifier, \code{NA} for a
#'   single unnamed series), \code{start}, \code{end}, \code{change},
#'   \code{n_changes} (how many detections fell inside), \code{status} (\code{"correct"}, \code{"false_positive"} or
#'   \code{"false_negative"}) — carrying the totals in an \code{errors}
#'   attribute and printing them.
#' @seealso \code{\link{cpt_labels}()},
#'   \code{\link{cpt_label_error_curve}()}, \code{\link{geom_cpt_label}()}.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(50), rnorm(50, 4)), method = "pelt")
#' labs <- cpt_labels(c(40, 70), c(60, 95), c("one_change", "no_change"))
#' cpt_label_error(fit, labs)
cpt_label_error <- function(object, labels) {
  cp <- if (is_ggcpt(object)) {
    object$changepoints$cp
  } else {
    as_cp_locations(object, "object")
  }
  labels <- check_labels(labels)
  # The `series` column is carried into the output and described in @return,
  # and nothing filtered on it -- so a multi-series label set, which is
  # exactly what cpt_learn_penalty() takes, had every series' labels scored
  # against this one fit. as_label_list() does the per-series split for the
  # learning path; this door has no equivalent, so say so rather than
  # returning a number that mixes them.
  ser <- unique(labels$series[!is.na(labels$series)])
  if (length(ser) > 1L) {
    warning("`labels` names ", length(ser), " series (",
            paste(utils::head(ser, 3), collapse = ", "),
            if (length(ser) > 3) ", ..." else "",
            ") and all of them are scored against this one fit. Subset ",
            "first, e.g. `labels[labels$series == \"", ser[1],
            "\", ]`.", call. = FALSE)
  }
  if (nrow(labels) == 0) {
    return(new_label_error(tibble::tibble(
      label_id = integer(), start = integer(), end = integer(),
      change = character(), n_changes = integer(), status = character()
    )))
  }
  n_changes <- vapply(seq_len(nrow(labels)), function(i) {
    sum(cp >= labels$start[i] & cp <= labels$end[i])
  }, integer(1))

  status <- character(nrow(labels))
  for (i in seq_len(nrow(labels))) {
    k <- n_changes[i]
    status[i] <- switch(labels$change[i],
      no_change = if (k == 0L) "correct" else "false_positive",
      change = if (k >= 1L) "correct" else "false_negative",
      one_change = if (k == 1L) {
        "correct"
      } else if (k == 0L) {
        "false_negative"
      } else {
        "false_positive"
      }
    )
  }
  out <- tibble::tibble(
    label_id = labels$label_id, series = labels$series,
    start = labels$start, end = labels$end, change = labels$change,
    n_changes = n_changes, status = status
  )
  new_label_error(out)
}

#' @noRd
new_label_error <- function(x) {
  attr(x, "errors") <- c(
    correct = sum(x$status == "correct"),
    false_positive = sum(x$status == "false_positive"),
    false_negative = sum(x$status == "false_negative"),
    total_errors = sum(x$status != "correct")
  )
  class(x) <- unique(c("cpt_label_error", class(x)))
  x
}

#' @rdname cpt_label_error
#' @export
tidy.cpt_label_error <- function(x, ...) {
  tibble::as_tibble(unclass_keep_tbl(x))
}

#' @rdname cpt_label_error
#' @param x A \code{cpt_label_error} object (for \code{print()}).
#' @param ... Ignored.
#' @export
print.cpt_label_error <- function(x, ...) {
  # `%||%` because tibble's `[` need not carry a custom attribute through a
  # row subset, and reclass_subset() keeps the subclass whenever the
  # required COLUMNS survive -- so `err[1, ]` can be a cpt_label_error with
  # no `errors` attribute, and `NULL[["correct"]]` is an error rather than
  # a degraded print. print.ggcpt_batch() already guards this way.
  e <- attr(x, "errors") %||% list()
  fld <- function(nm) {
    v <- e[[nm]]
    if (is.null(v) || length(v) != 1L) "?" else v
  }
  cat("cpt_label_error (", nrow(x), " label(s))\n", sep = "")
  cat("  correct: ", fld("correct"),
      "   false positives: ", fld("false_positive"),
      "   false negatives: ", fld("false_negative"), "\n", sep = "")
  cat("  total label errors: ", fld("total_errors"), "\n\n", sep = "")
  print(tibble::as_tibble(x), n = 12)
  invisible(x)
}

#' @noRd
check_labels <- function(labels) {
  if (is.null(labels)) {
    stop("`labels` is required. Build one with cpt_labels().", call. = FALSE)
  }
  labels <- tibble::as_tibble(labels)
  need <- c("start", "end", "change")
  miss <- setdiff(need, names(labels))
  if (length(miss) > 0) {
    stop("`labels` needs column(s): ", paste(miss, collapse = ", "),
         ". Build one with cpt_labels().", call. = FALSE)
  }
  # cpt_labels() validates the vocabulary; this door only checked that the
  # columns exist -- and `@param labels` deliberately widens the contract to
  # "or anything with start/end/change columns". So a documented input with
  # `change = "maybe"` reached a switch() with no default, which returns
  # NULL, and `status[i] <- NULL` on a character vector is base R's
  # "replacement has length zero": a message naming neither the column nor
  # the legal values.
  bad <- setdiff(unique(as.character(labels$change)),
                 c("change", "one_change", "no_change"))
  if (length(bad) > 0) {
    stop("Unknown `change` label(s): ", paste(bad, collapse = ", "),
         ". Use \"change\", \"one_change\" or \"no_change\"; ",
         "cpt_labels() builds a table with the right vocabulary.",
         call. = FALSE)
  }
  if (!"label_id" %in% names(labels)) labels$label_id <- seq_len(nrow(labels))
  if (!"series" %in% names(labels)) labels$series <- NA_character_
  labels
}

#' Label error as a function of the penalty
#'
#' Runs one detector across a penalty grid and counts label errors at each
#' setting — the curve penalty learning is fitted to, and the honest way to
#' see whether \emph{any} penalty can satisfy the labels.
#'
#' @param x A numeric vector (the series).
#' @param labels A \code{cpt_labels} tibble.
#' @param method Detection method. Defaults to \code{"pelt"}.
#' @param penalties Numeric vector of penalties to try. When \code{NULL}
#'   (the default) the grid is chosen \emph{adaptively}: it starts below
#'   \code{log(n)}, where the segmentation shatters, and the top end is
#'   found by doubling until the detector reports no changepoints at all.
#'   A fixed grid cannot do this — on a series with a large change, a grid
#'   that stops at a few hundred never produces a false negative, the error
#'   curve never turns back up, and the target interval comes out unbounded
#'   above, which is useless to \code{\link{cpt_learn_penalty}()}. The
#'   probe costs at most a dozen extra detector fits; pass \code{penalties}
#'   explicitly for an expensive engine.
#' @param change_in Passed to the detector.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()}.
#'
#' @return A \code{ggcpt_label_curve} object: a tibble with \code{penalty},
#'   \code{n_cp}, \code{errors}, \code{false_positive},
#'   \code{false_negative}, plus \code{print()} and \code{autoplot()}.
#'   The \code{target} attribute holds the interval of \code{log(penalty)}
#'   achieving the minimum error, which is what
#'   \code{\link{cpt_learn_penalty}()} regresses on.
#' @seealso \code{\link{cpt_learn_penalty}()}, \code{\link{cpt_select}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 4))
#' labs <- as_cpt_labels(60, n = 120)
#' curve <- cpt_label_error_curve(x, labs, penalties = c(2, 8, 32, 128))
#' curve
cpt_label_error_curve <- function(x, labels, method = "pelt",
                                  penalties = NULL, change_in = "mean", ...) {
  validate_data(x)
  series <- as_uni_vector(x, method)
  labels <- check_labels(labels)
  n <- length(series)
  if (is.null(penalties)) {
    penalties <- default_penalty_grid(series, method, change_in, ...)
  }
  penalties <- sort(unique(as.numeric(penalties)))
  if (any(penalties <= 0)) {
    stop("`penalties` must be positive (the curve is fitted on the log ",
         "scale).", call. = FALSE)
  }

  rows <- lapply(penalties, function(p) {
    cp <- tryCatch(
      cpt_detect(series, method = method, change_in = change_in,
                 penalty = p, ...)$changepoints$cp,
      error = function(e) NULL
    )
    if (is.null(cp)) {
      return(tibble::tibble(penalty = p, n_cp = NA_integer_,
                            errors = NA_integer_,
                            false_positive = NA_integer_,
                            false_negative = NA_integer_))
    }
    le <- cpt_label_error(cp, labels)
    e <- attr(le, "errors")
    tibble::tibble(penalty = p, n_cp = length(cp),
                   errors = e[["total_errors"]],
                   false_positive = e[["false_positive"]],
                   false_negative = e[["false_negative"]])
  })
  out <- do.call(rbind, rows)

  # The target interval: the widest contiguous run of log-penalties over
  # which the label error is minimal. Penalty learning predicts a point
  # inside this interval, so a wider interval is an easier series.
  target <- target_interval(out)
  attr(out, "target") <- target
  attr(out, "method") <- method
  attr(out, "n") <- n
  class(out) <- c("ggcpt_label_curve", class(out))
  out
}

# Internal: a penalty grid whose two ends bracket the useful range -- one
# where the detector over-segments, one where it finds nothing. Both ends
# have to be reached or the label-error curve is one-sided: with no false
# negatives at the top the minimum runs to the edge of the grid, the target
# interval is unbounded above, and interval regression has no margin to fit
# against. Found by doubling rather than guessed, because how large a
# penalty it takes to silence a detector depends on the size of the change
# and the scale of the data, not on n alone.
#' @noRd
default_penalty_grid <- function(series, method, change_in, n_grid = 30,
                                 max_doublings = 12, ...) {
  base <- log(length(series))
  n_cp_at <- function(p) {
    v <- tryCatch(
      cpt_detect(series, method = method, change_in = change_in,
                 penalty = p, ...)$changepoints$cp,
      error = function(e) NULL
    )
    if (is.null(v)) NA_integer_ else length(v)
  }
  hi <- base * 40
  for (i in seq_len(max_doublings)) {
    k <- n_cp_at(hi)
    if (!is.na(k) && k == 0L) break
    hi <- hi * 2
  }
  exp(seq(log(base / 20), log(hi), length.out = as.integer(n_grid)))
}

# Internal: the widest contiguous run of penalties achieving the minimum
# label error, returned as an interval of log(penalty). Infinite endpoints
# mean the minimum extends past the grid, which is the standard convention
# in the penalty-learning target-interval literature.
#' @noRd
target_interval <- function(curve) {
  ok <- !is.na(curve$errors)
  if (!any(ok)) return(c(-Inf, Inf))
  err <- curve$errors[ok]
  pen <- log(curve$penalty[ok])
  best <- min(err)
  is_best <- err == best
  # Longest run of TRUEs.
  r <- rle(is_best)
  if (!any(r$values)) return(c(-Inf, Inf))
  widths <- ifelse(r$values, r$lengths, 0L)
  j <- which.max(widths)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  lo_i <- starts[j]
  hi_i <- ends[j]
  lo <- if (lo_i == 1L) -Inf else pen[lo_i]
  hi <- if (hi_i == length(pen)) Inf else pen[hi_i]
  c(lo, hi)
}

#' @rdname cpt_label_error_curve
#' @param x A \code{ggcpt_label_curve} object (for \code{print()}).
#' @export
print.ggcpt_label_curve <- function(x, ...) {
  tg <- attr(x, "target")
  cat("ggcpt_label_curve (method: ", attr(x, "method"), ", ", nrow(x),
      " penalties)\n", sep = "")
  cat("  Minimum label errors: ", min(x$errors, na.rm = TRUE), "\n", sep = "")
  cat("  Target log-penalty interval: (", format(tg[1], digits = 4), ", ",
      format(tg[2], digits = 4), ")\n\n", sep = "")
  print(tibble::as_tibble(x), n = 10)
  invisible(x)
}

#' @rdname cpt_label_error_curve
#' @param object A \code{ggcpt_label_curve} object (for \code{autoplot()}).
#' @export
autoplot.ggcpt_label_curve <- function(object, ...) {
  d <- tibble::as_tibble(object)
  long <- rbind(
    tibble::tibble(penalty = d$penalty, count = d$false_positive,
                   kind = "false positive"),
    tibble::tibble(penalty = d$penalty, count = d$false_negative,
                   kind = "false negative"),
    tibble::tibble(penalty = d$penalty, count = d$errors, kind = "total")
  )
  tg <- attr(object, "target")
  p <- ggplot2::ggplot(long, ggplot2::aes(penalty, count, colour = kind,
                                          linetype = kind)) +
    ggplot2::geom_step(na.rm = TRUE, linewidth = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(
      values = c(`false positive` = "#E69F00",
                 `false negative` = "#D55E00", total = "#0072B2"),
      name = NULL) +
    ggplot2::scale_linetype_manual(
      values = c(`false positive` = "dashed",
                 `false negative` = "dotted", total = "solid"),
      name = NULL) +
    ggplot2::labs(x = "Penalty (log scale)", y = "Label errors",
                  title = paste0("Label error curve (",
                                 attr(object, "method"), ")"))
  # Two bugs in one guard. `all(logical(0))` is TRUE, so a MISSING `target`
  # attribute passed it and `annotate()` got zero-length xmin/xmax from
  # `exp(NULL[1])`. And an infinite endpoint is returned deliberately --
  # target_interval() uses it for "the minimum extends past the grid", the
  # standard convention in the penalty-learning literature -- which is
  # exactly the diagnosis the reader needs (the grid was too narrow), and
  # the finiteness test hid the band in precisely that case. -Inf/Inf is how
  # the package draws open-ended bands elsewhere, so draw it.
  if (length(tg) == 2L && !anyNA(tg) && any(is.finite(tg))) {
    p <- p + ggplot2::annotate("rect",
                               xmin = if (is.finite(tg[1])) exp(tg[1]) else -Inf,
                               xmax = if (is.finite(tg[2])) exp(tg[2]) else Inf,
                               ymin = -Inf, ymax = Inf,
                               fill = "#009E73", alpha = 0.15)
  }
  p
}

#' Learn a penalty from labelled series
#'
#' Fits the supervised penalty model of Hocking et al. (2013): each series
#' contributes a target \emph{interval} of log-penalties (those achieving the
#' fewest label errors), a feature vector is computed from the series, and a
#' linear model is fitted by minimising the squared hinge loss on those
#' intervals — max-margin interval regression. The result has a
#' \code{predict()} method, and \code{\link{cpt_detect}()} accepts it
#' directly as \code{penalty}, so a learned penalty is used exactly like a
#' number.
#'
#' @param series A named list of numeric vectors (or a matrix/data frame with
#'   one column per series).
#' @param labels Either a single \code{cpt_labels} tibble whose \code{series}
#'   column names the series, or a list of label tibbles parallel to
#'   \code{series}.
#' @param method Detection method used to build the label-error curves.
#'   Defaults to \code{"pelt"}.
#' @param penalties Penalty grid for the curves; passed to
#'   \code{\link{cpt_label_error_curve}()}.
#' @param engine \code{"auto"} (default) uses
#'   \pkg{penaltyLearning}'s \code{IntervalRegressionCV()} when the package is
#'   installed and there are enough series for its cross-validation, and the
#'   built-in squared-hinge fit otherwise; \code{"penaltyLearning"} and
#'   \code{"native"} force the choice.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()} while
#'   building the curves.
#'
#' @return A \code{ggcpt_penalty_model} object with \code{print()},
#'   \code{coef()} and \code{predict()} methods.
#'
#'   The two scales differ and it matters: \code{coef()} gives an
#'   intercept plus one weight per feature \strong{on the log-penalty
#'   scale}, which is where the interval regression is fitted, while
#'   \code{predict()} exponentiates and returns a penalty on the natural
#'   scale -- the scale \code{\link{cpt_penalty}()} and
#'   \code{\link{cpt_detect}()} consume. So a coefficient of
#'   \eqn{-0.04} on \code{log_n} is a multiplicative effect on the
#'   penalty, not an additive one.
#' @section Reading the coefficients:
#' \strong{The signs are often not interpretable, and that is a property of
#'   the labels rather than of the fit.} A target interval is open above
#'   whenever the largest penalty on the grid still achieves the minimum
#'   label error -- which is the common case, because a large penalty
#'   usually keeps the one changepoint the labels ask for. When every
#'   series' interval is open above, any sufficiently large prediction is
#'   optimal, the problem does not pin the slopes, and the \eqn{L_2} term
#'   settles them near zero with whatever sign the optimiser reached.
#'
#'   Two measurements on four series of very different length and noise,
#'   all with one-change labels, differing only in the data drawn. In one,
#'   every non-intercept coefficient came out slightly negative, so the
#'   predicted penalty \emph{decreased} with \eqn{n} -- the opposite of the
#'   \eqn{\log n} growth a reader would expect from BIC, and not evidence
#'   of anything. In the other, every feature weight went to zero and the
#'   model became a \strong{constant}: \code{predict()} returned the same
#'   penalty for all four series. Both fits put every prediction inside its
#'   target, which is the property the model is fitted for, and neither
#'   outcome says anything about how a penalty should scale.
#'
#'   So do not read a \code{ggcpt_penalty_model} as having discovered a
#'   relationship, and do not be surprised by a constant one.
#'
#'   If the coefficients need to mean something, the labels have to
#'   constrain the penalty from both sides: widen \code{penalties} until
#'   the largest one starts to over-segment, so the target intervals close
#'   above. \code{\link{cpt_label_error_curve}()} shows whether they do.
#' @references
#' \insertRef{hocking2013penalties}{ggchangepoint}
#' @seealso \code{\link{cpt_label_error_curve}()},
#'   \code{\link{cpt_penalty}()}.
#' @export
#' @examples
#' set.seed(2026)
#' series <- list(a = c(rnorm(60), rnorm(60, 4)),
#'                b = c(rnorm(80), rnorm(80, 2)))
#' labels <- list(a = as_cpt_labels(60, n = 120),
#'                b = as_cpt_labels(80, n = 160))
#' model <- cpt_learn_penalty(series, labels,
#'                            penalties = c(2, 8, 32, 128))
#' model
#' stats::predict(model, series$a)
cpt_learn_penalty <- function(series, labels, method = "pelt",
                              penalties = NULL,
                              engine = c("auto", "penaltyLearning",
                                         "native"),
                              ...) {
  engine <- match.arg(engine)
  series_list <- as_series_list(series)
  label_list <- as_label_list(labels, names(series_list))

  feats <- do.call(rbind, lapply(series_list, cpt_features))
  rownames(feats) <- names(series_list)

  targets <- do.call(rbind, lapply(seq_along(series_list), function(i) {
    curve <- cpt_label_error_curve(series_list[[i]], label_list[[i]],
                                   method = method, penalties = penalties,
                                   ...)
    tg <- attr(curve, "target")
    c(lower = tg[1], upper = tg[2])
  }))
  rownames(targets) <- names(series_list)

  usable <- is.finite(targets[, 1]) | is.finite(targets[, 2])
  if (!any(usable)) {
    stop("Every series has an unbounded target interval, so there is nothing ",
         "to learn: the labels are satisfied at every penalty in the grid. ",
         "Add `no_change` labels, or widen `penalties`.", call. = FALSE)
  }
  # `usable` gated the error above and nothing else: the unusable rows still
  # reached the fit. In the native path a (-Inf, Inf) row contributes zero
  # loss, which is harmless but dilutes nothing; in the penaltyLearning path
  # IntervalRegressionCV() rejects it, the tryCatch below catches the error,
  # and the whole fit silently downgrades to the fallback with a warning
  # naming the wrong cause. Drop them here, and say how many.
  if (!all(usable)) {
    dropped <- rownames(targets)[!usable] %||%
      as.character(which(!usable))
    warning(length(dropped), " of ", nrow(targets), " series have an ",
            "unbounded target interval (",
            paste(utils::head(dropped, 3), collapse = ", "),
            if (length(dropped) > 3) ", ..." else "",
            ") and are dropped from the fit: their labels are satisfied at ",
            "every penalty in the grid, so they say nothing about how the ",
            "penalty should scale. Widen `penalties` to close them.",
            call. = FALSE)
    feats <- feats[usable, , drop = FALSE]
    targets <- targets[usable, , drop = FALSE]
  }

  # A constant series has no scale to learn from -- sd, mad and range are
  # all zero, so cpt_features() floors their logs -- and one of them in the
  # training set pulls the fit toward that floor.
  flat <- vapply(series_list, function(v) {
    sd_v <- stats::sd(as.numeric(v))
    !is.finite(sd_v) || sd_v == 0
  }, logical(1))
  if (any(flat)) {
    warning(sum(flat), " training series ",
            if (sum(flat) > 1) "are" else "is", " constant (",
            paste(utils::head(names(series_list)[flat], 3),
                  collapse = ", "),
            "), so the scale features are at their floor rather than at a ",
            "measured value and the fit is pulled toward it. Drop the flat ",
            "series, or check the input.", call. = FALSE)
  }

  use_pl <- (engine == "penaltyLearning") ||
    (engine == "auto" && requireNamespace("penaltyLearning", quietly = TRUE) &&
       nrow(feats) >= 10)
  if (engine == "penaltyLearning" &&
      !requireNamespace("penaltyLearning", quietly = TRUE)) {
    stop("Package 'penaltyLearning' is required for ",
         "`engine = \"penaltyLearning\"`. Install it with ",
         "install.packages('penaltyLearning').", call. = FALSE)
  }

  fit <- if (use_pl) {
    # IntervalRegressionCV() draws internally with a deprecated ggplot2
    # aesthetic, so it emits a lifecycle warning the caller did not ask for
    # and cannot act on. Muffle that one; anything else still gets through.
    pl <- tryCatch(
      withCallingHandlers(
        penaltyLearning::IntervalRegressionCV(feats, targets, verbose = 0),
        warning = function(w) {
          if (inherits(w, "lifecycle_warning_deprecated") ||
              grepl("aesthetic was deprecated|`size` aesthetic",
                    conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) NULL
    )
    if (is.null(pl)) {
      warning("penaltyLearning::IntervalRegressionCV() failed on this ",
              "training set; falling back to the built-in squared-hinge ",
              "fit.", call. = FALSE)
      interval_regression(feats, targets)
    } else {
      list(engine = "penaltyLearning", model = pl,
           features = colnames(feats))
    }
  } else {
    interval_regression(feats, targets)
  }

  structure(
    list(fit = fit, features = colnames(feats), targets = targets,
         feature_matrix = feats, method = method,
         n_series = length(series_list)),
    class = "ggcpt_penalty_model"
  )
}

# Internal: max-margin interval regression, fitted by minimising the squared
# hinge loss sum_i [ (l_i - w'x_i + 1)_+^2 + (w'x_i - u_i + 1)_+^2 ] with a
# small ridge term. This is the Hocking et al. objective; the ridge keeps the
# fit defined when there are fewer series than features, which is the common
# case for a handful of training series.
#' @noRd
interval_regression <- function(feats, targets, lambda = 1e-3, margin = 1) {
  X <- cbind(intercept = 1, feats)
  lo <- targets[, 1]
  hi <- targets[, 2]
  obj <- function(w) {
    pred <- as.numeric(X %*% w)
    left <- pmax(0, lo + margin - pred)
    right <- pmax(0, pred - hi + margin)
    left[!is.finite(lo)] <- 0
    right[!is.finite(hi)] <- 0
    sum(left^2 + right^2) / nrow(X) + lambda * sum(w[-1]^2)
  }
  # Start at the midpoint of the finite targets, which is already a sensible
  # constant prediction.
  mids <- ifelse(is.finite(lo) & is.finite(hi), (lo + hi) / 2,
                 ifelse(is.finite(lo), lo + margin, hi - margin))
  w0 <- c(mean(mids[is.finite(mids)]), rep(0, ncol(X) - 1L))
  opt <- stats::optim(w0, obj, method = "BFGS",
                      control = list(maxit = 500))
  w <- stats::setNames(opt$par, colnames(X))
  list(engine = "native", coefficients = w, loss = opt$value,
       features = colnames(feats))
}

# Internal: the per-series feature vector. Scale-free and length-aware
# summaries, all on the log scale so the linear model works in the same
# units as the log-penalty target.
#' @noRd
cpt_features <- function(y) {
  y <- as.numeric(y)
  n <- length(y)
  d <- diff(y)
  # The floor is .Machine$double.eps, so a CONSTANT series gives
  # log_sd = log_mad = log_range = log(2.2e-16) ~ -36 against typical
  # values near 0. interval_regression() ridges only the slopes and starts
  # from a constant, so one flat series in the training set dominates the
  # fit -- and validate_data() accepts a flat series. Floor at something
  # that is small on the log scale rather than astronomically small, and
  # say so where the features are documented.
  lg <- function(v) log(max(v, 1e-8))
  c(
    log_n = log(n),
    log_log_n = log(log(max(n, 3))),
    log_sd = lg(stats::sd(y)),
    log_mad = lg(stats::mad(y)),
    log_range = lg(diff(range(y))),
    log_sd_diff = lg(stats::sd(d)),
    log_mad_diff = lg(stats::mad(d)),
    log_q90_abs_diff = lg(stats::quantile(abs(d), 0.9, names = FALSE))
  )
}

#' @noRd
as_series_list <- function(series) {
  out <- if (is.list(series) && !is.data.frame(series)) {
    lapply(series, as.numeric)
  } else {
    X <- as_mv_matrix(series, arg = "series")
    stats::setNames(lapply(seq_len(ncol(X)), function(j) X[, j]), colnames(X))
  }
  nms <- names(out) %||% rep("", length(out))
  nms[!nzchar(nms) | is.na(nms)] <- paste0("series_",
                                           which(!nzchar(nms) | is.na(nms)))
  names(out) <- make.unique(nms)
  out
}

#' @noRd
as_label_list <- function(labels, series_names) {
  if (inherits(labels, "cpt_labels") || is.data.frame(labels)) {
    labels <- check_labels(labels)
    if (all(is.na(labels$series))) {
      stop("A single label tibble must name its series in the `series` ",
           "column so each label can be matched to a series; or pass a list ",
           "of label tibbles parallel to `series`.", call. = FALSE)
    }
    out <- lapply(series_names, function(nm) {
      labels[labels$series == nm, , drop = FALSE]
    })
    names(out) <- series_names
  } else {
    out <- lapply(labels, check_labels)
    if (is.null(names(out))) names(out) <- series_names
    out <- out[series_names]
  }
  missing_lab <- vapply(out, function(l) is.null(l) || nrow(l) == 0,
                        logical(1))
  if (any(missing_lab)) {
    stop("No labels for series: ",
         paste(series_names[missing_lab], collapse = ", "), ".",
         call. = FALSE)
  }
  out
}

#' @rdname cpt_learn_penalty
#' @param x A \code{ggcpt_penalty_model} (for \code{print()} and
#'   \code{coef()}).
#' @export
print.ggcpt_penalty_model <- function(x, ...) {
  cat("ggcpt_penalty_model (", x$fit$engine, " interval regression)\n",
      sep = "")
  cat("  Trained on ", x$n_series, " series with method `", x$method,
      "`\n", sep = "")
  cat("  Features: ", paste(x$features, collapse = ", "), "\n", sep = "")
  cf <- stats::coef(x)
  if (!is.null(cf)) {
    cat("\nCoefficients (predicting log penalty):\n")
    print(round(cf, 4))
  }
  cat("\nUse it directly: cpt_detect(x, method = \"", x$method,
      "\", penalty = model)\n", sep = "")
  invisible(x)
}

#' @rdname cpt_learn_penalty
#' @param object A \code{ggcpt_penalty_model}.
#' @exportS3Method stats::coef
coef.ggcpt_penalty_model <- function(object, ...) {
  if (identical(object$fit$engine, "native")) return(object$fit$coefficients)
  tryCatch(stats::coef(object$fit$model), error = function(e) NULL)
}

#' @rdname cpt_learn_penalty
#' @param newdata A numeric vector (one series), or a list/matrix of series.
#' @export
predict.ggcpt_penalty_model <- function(object, newdata, ...) {
  series_list <- if (is.numeric(newdata) && is.null(dim(newdata))) {
    list(newdata)
  } else {
    as_series_list(newdata)
  }
  feats <- do.call(rbind, lapply(series_list, cpt_features))
  log_pen <- if (identical(object$fit$engine, "native")) {
    as.numeric(cbind(1, feats) %*% object$fit$coefficients)
  } else {
    as.numeric(stats::predict(object$fit$model, feats))
  }
  out <- exp(log_pen)
  names(out) <- names(series_list)
  out
}
