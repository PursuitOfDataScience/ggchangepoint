# ---------------------------------------------------------------------------
# Theme E: seeing the statistic.
#
# Every detector computes something -- a CUSUM path, a MOSUM statistic at
# each bandwidth, a set of random intervals with contrast values -- and 0.4.0
# kept only the argmax. These three accessors (plus their plots) give the
# discarded object back, for the engines that expose it, and say plainly
# which engines do when one does not.
# ---------------------------------------------------------------------------

#' The detector's statistic as a function of location
#'
#' Returns the criterion the engine evaluated at each position, which is what
#' explains \emph{why} a changepoint landed where it did. Available for the
#' engines that expose it —
#' \code{subset(cpt_methods(), statistic)$method} lists them — and an error
#' naming those engines for the ones that do not.
#'
#' @param object A \code{ggcpt} object.
#' @return \code{cpt_statistic()} returns a tibble with \code{index},
#'   \code{statistic}, \code{threshold} (the engine's rejection threshold, or
#'   \code{NA}) and \code{label} (what the statistic is).
#'   \code{ggcpt_statistic()} returns a two-panel ggplot: the series above,
#'   the statistic below, sharing the x axis.
#' @seealso \code{\link{cpt_solution_path}()},
#'   \code{\link{cpt_scale_space}()}.
#' @export
#' @examplesIf requireNamespace("mosum", quietly = TRUE)
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(200), rnorm(200, 3)), method = "mosum")
#' head(cpt_statistic(fit))
#' ggcpt_statistic(fit)
cpt_statistic <- function(object) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  out <- extract_statistic(object)
  if (is.null(out)) {
    stop("Engine `", object$method, "` does not expose a per-location ",
         "statistic. These do: ",
         paste(subset(cpt_methods(), statistic %in% TRUE)$method,
               collapse = ", "), ".", call. = FALSE)
  }
  out
}

# Internal: per-engine statistic extraction. Returns NULL when the engine
# keeps nothing usable, so the caller can produce the "these engines do"
# message in one place.
#' @noRd
extract_statistic <- function(object) {
  n <- nrow(object$data)
  fit <- object$fit
  method <- object$method

  mk <- function(stat, label, threshold = NA_real_) {
    stat <- as.numeric(stat)
    if (length(stat) != n) {
      # Some engines report a statistic on a shorter grid (boundary
      # trimming); pad rather than silently mis-aligning it.
      full <- rep(NA_real_, n)
      full[seq_len(min(n, length(stat)))] <- stat[seq_len(min(n, length(stat)))]
      stat <- full
    }
    tibble::tibble(index = seq_len(n), statistic = stat,
                   threshold = as.numeric(threshold)[1], label = label)
  }

  # Sparse statistics (one value per candidate location) come back as a
  # length-n vector of zeros with the candidate values filled in, so the
  # panel is comparable with the dense ones.
  sparse <- function(loc, val, label, threshold = NA_real_) {
    stat <- rep(0, n)
    keep <- loc >= 1 & loc <= n & is.finite(val)
    if (any(keep)) {
      # A location can be the argmax of several intervals; keep the largest.
      ag <- stats::aggregate(list(v = abs(val[keep])),
                             by = list(i = loc[keep]), FUN = max)
      stat[ag$i] <- ag$v
    }
    mk(stat, label, threshold)
  }

  # `[[` with exact = TRUE throughout: `$` on a list partially matches, so
  # a renamed upstream field could silently resolve to a longer sibling
  # (`$stat` -> `statistics`) and be plotted as if it were the right one.
  fld <- function(nm) if (is.list(fit)) fit[[nm, exact = TRUE]] else NULL
  if (method == "mosum" && !is.null(fld("stat"))) {
    return(mk(fld("stat"), "MOSUM statistic",
              fld("threshold.value") %||% NA_real_))
  }
  if (method == "wbs" && !is.null(fld("res"))) {
    res <- fld("res")
    return(sparse(res[, "cpt"], res[, "CUSUM"],
                  "WBS CUSUM (max over intervals)"))
  }
  if (method == "not" && !is.null(fld("contrasts"))) {
    ct <- fld("contrasts")
    return(sparse(ct[, "arg.max"], ct[, "max.contrast"],
                  "NOT contrast (max over intervals)"))
  }
  if (method %in% c("pettitt", "buishand", "snht") &&
      inherits(fit, "htest") && !is.null(fld("data"))) {
    return(mk(fld("data"), paste0(toupper(substr(method, 1, 1)),
                               substring(method, 2), " statistic")))
  }
  if (method == "amoc") {
    return(mk(amoc_lr_profile(object$data$value),
              "AMOC log-likelihood-ratio profile"))
  }
  if (method == "nsp" && !is.null(object$regions)) {
    stat <- rep(0, n)
    reg <- object$regions
    if (nrow(reg) > 0 && !is.null(reg$value)) {
      for (i in seq_len(nrow(reg))) {
        stat[reg$start[i]:reg$end[i]] <- pmax(
          stat[reg$start[i]:reg$end[i]], abs(reg$value[i])
        )
      }
    }
    return(mk(stat, "NSP interval deviation",
              object$threshold_used %||% NA_real_))
  }
  prob <- posterior_prob_profile(object)
  if (!is.null(prob)) {
    return(mk(prob, "Posterior changepoint probability"))
  }
  diag_stat <- object$diagnostics[["statistic", exact = TRUE]]
  if (!is.null(diag_stat)) {
    return(mk(diag_stat[["statistic", exact = TRUE]] %||% diag_stat,
              diag_stat[["label", exact = TRUE]] %||% "Detector statistic",
              diag_stat[["threshold", exact = TRUE]] %||% NA_real_))
  }
  NULL
}

# Internal: the AMOC single-changepoint log-likelihood-ratio profile for a
# change in mean with common variance. AMOC *is* this statistic maximised,
# so drawing it is drawing the method rather than inventing a diagnostic.
#' @noRd
amoc_lr_profile <- function(y) {
  n <- length(y)
  cs <- cumsum(y)
  tot <- cs[n]
  k <- seq_len(n - 1L)
  # Standardised CUSUM: |sqrt(n/(k(n-k))) * (S_k - k/n * S_n)|
  stat <- abs(sqrt(n / (k * (n - k))) * (cs[k] - k * tot / n))
  s <- stats::sd(y)
  if (is.finite(s) && s > 0) stat <- stat / s
  c(stat, NA_real_)
}

#' @rdname cpt_statistic
#' @export
ggcpt_statistic <- function(object) {
  st <- cpt_statistic(object)
  idx_vals <- plot_index(object)
  x_lab <- plot_index_label(object)
  lab <- st$label[1]

  top <- tibble::tibble(index = idx_vals, y = object$data$value,
                        panel = "Series")
  bottom <- tibble::tibble(index = idx_vals, y = st$statistic,
                           panel = lab)
  both <- rbind(top, bottom)
  both$panel <- factor(both$panel, levels = c("Series", lab))

  p <- ggplot2::ggplot(both, ggplot2::aes(index, y)) +
    ggplot2::geom_line(data = both[both$panel == "Series", ],
                       colour = "grey40") +
    ggplot2::geom_line(data = both[both$panel == lab, ],
                       colour = "#0072B2", na.rm = TRUE) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = x_lab, y = NULL,
                  title = paste0("Detector statistic (", object$method, ")"))

  if (nrow(object$changepoints) > 0) {
    p <- p + ggplot2::geom_vline(
      xintercept = idx_vals[object$changepoints$cp],
      colour = "blue", linetype = "dashed", linewidth = 0.4
    )
  }
  thr <- st$threshold[1]
  if (is.finite(thr)) {
    p <- p + ggplot2::geom_hline(
      data = tibble::tibble(yint = thr, panel = factor(lab,
                                                       levels = levels(both$panel))),
      ggplot2::aes(yintercept = yint),
      linetype = "dotted", colour = "grey30"
    )
  }
  p
}

#' The solution path of a search-based detector
#'
#' The order in which candidate changepoints entered the model, with the
#' contrast (or split criterion) at each step. Binary segmentation splits
#' recursively, WBS/WBS2/NOT/TGUH rank random intervals — in every case the
#' final answer is a prefix of a path, and seeing the path shows how
#' decisively each changepoint beat the next.
#'
#' @param object A \code{ggcpt} object from an engine with a solution path
#'   (\code{subset(cpt_methods(), path)$method}).
#' @return \code{cpt_solution_path()} returns a tibble with \code{step},
#'   \code{cp}, \code{contrast} and — for interval-based searches —
#'   \code{start}/\code{end} of the interval that proposed it, plus a
#'   \code{selected} flag marking the changepoints in the final model.
#'   \code{ggcpt_solution_path()} draws it.
#' @seealso \code{\link{cpt_statistic}()}, \code{\link{cpt_crops}()} for the
#'   penalty path of an optimal-partitioning method.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(150), rnorm(150, 3)), method = "binseg")
#' cpt_solution_path(fit)
cpt_solution_path <- function(object) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  out <- extract_solution_path(object)
  if (is.null(out)) {
    stop("Engine `", object$method, "` does not expose a solution path. ",
         "These do: ",
         paste(subset(cpt_methods(), path %in% TRUE)$method,
               collapse = ", "),
         ". For the penalty path of an optimal-partitioning method see ",
         "cpt_crops().", call. = FALSE)
  }
  out
}

#' @noRd
extract_solution_path <- function(object) {
  fit <- object$fit
  method <- object$method
  n <- nrow(object$data)
  selected <- object$changepoints$cp
  # Exact extraction, for the same reason as in extract_statistic().
  pfld <- function(nm) if (is.list(fit)) fit[[nm, exact = TRUE]] else NULL

  finish <- function(cp, contrast, start = NA_integer_, end = NA_integer_) {
    keep <- !is.na(cp) & cp >= 1 & cp < n
    cp <- cp[keep]
    contrast <- contrast[keep]
    if (length(start) > 1) start <- start[keep]
    if (length(end) > 1) end <- end[keep]
    if (length(cp) == 0) return(NULL)
    tibble::tibble(step = seq_along(cp), cp = as.integer(cp),
                   contrast = as.numeric(contrast),
                   start = as.integer(start), end = as.integer(end),
                   selected = cp %in% selected)
  }

  if (method %in% c("binseg", "segneigh") && inherits(fit, "cpt.range")) {
    full <- changepoint::cpts.full(fit)
    if (is.null(dim(full))) full <- matrix(full, nrow = 1)
    pens <- as.numeric(changepoint::pen.value.full(fit))
    # Row i of cpts.full holds the segmentation with i changepoints, and the
    # rows are nested, so the new entry in each row is the split that step
    # added.
    order_cp <- integer(0)
    contrast <- numeric(0)
    prev <- integer(0)
    for (i in seq_len(nrow(full))) {
      row <- as.integer(full[i, ][!is.na(full[i, ])])
      new <- setdiff(row, prev)
      if (length(new) > 0) {
        order_cp <- c(order_cp, new[1])
        contrast <- c(contrast, if (i <= length(pens)) pens[i] else NA_real_)
      }
      prev <- row
    }
    return(finish(order_cp, contrast))
  }
  if (method == "wbs" && !is.null(pfld("res"))) {
    res <- pfld("res")
    ord <- order(-abs(res[, "CUSUM"]))
    res <- res[ord, , drop = FALSE]
    # One row per distinct candidate location, strongest first.
    dup <- duplicated(res[, "cpt"])
    res <- res[!dup, , drop = FALSE]
    return(finish(res[, "cpt"], abs(res[, "CUSUM"]),
                  res[, "s"], res[, "e"]))
  }
  if (method == "not" && !is.null(pfld("contrasts"))) {
    ct <- pfld("contrasts")
    ord <- order(-abs(ct[, "max.contrast"]))
    ct <- ct[ord, , drop = FALSE]
    dup <- duplicated(ct[, "arg.max"])
    ct <- ct[!dup, , drop = FALSE]
    return(finish(ct[, "arg.max"], abs(ct[, "max.contrast"]),
                  ct[, "start"], ct[, "end"]))
  }
  if (method %in% c("wbs2", "tguh") &&
      requireNamespace("breakfast", quietly = TRUE)) {
    sol <- tryCatch(
      if (method == "wbs2") {
        breakfast::sol.wbs2(object$data$value)
      } else {
        breakfast::sol.tguh(object$data$value)
      },
      error = function(e) NULL
    )
    if (!is.null(sol) && !is.null(sol$cands)) {
      cands <- sol$cands
      return(finish(cands[, 3], cands[, 4], cands[, 1], cands[, 2]))
    }
  }
  diag_path <- object$diagnostics[["solution_path", exact = TRUE]]
  if (!is.null(diag_path)) {
    return(tibble::as_tibble(diag_path))
  }
  NULL
}

#' @rdname cpt_solution_path
#' @param max_steps Longest prefix of the path drawn. Defaults to \code{40} —
#'   a randomised search proposes hundreds of candidates and only the head of
#'   the ranking is readable.
#' @export
ggcpt_solution_path <- function(object, max_steps = 40) {
  path <- cpt_solution_path(object)
  validate_scalar(max_steps, "max_steps", min = 1)
  path <- utils::head(path, as.integer(max_steps))
  idx_vals <- plot_index(object)
  x_lab <- plot_index_label(object)
  path$cp_x <- idx_vals[path$cp]
  has_interval <- !all(is.na(path$start))
  if (has_interval) {
    path$start_x <- idx_vals[pmax(1L, pmin(path$start, length(idx_vals)))]
    path$end_x <- idx_vals[pmax(1L, pmin(path$end, length(idx_vals)))]
  }

  p <- ggplot2::ggplot(path, ggplot2::aes(cp_x, step))
  if (has_interval) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(x = start_x, xend = end_x, y = step, yend = step,
                   colour = selected),
      linewidth = 0.5, alpha = 0.7
    )
  }
  p +
    ggplot2::geom_point(ggplot2::aes(colour = selected, size = contrast)) +
    ggplot2::scale_y_reverse(breaks = scales_int_breaks(path$step)) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "grey60",
                                            `TRUE` = "#D55E00"),
                                 name = "In final model") +
    ggplot2::scale_size_continuous(range = c(0.6, 3), name = "Contrast") +
    ggplot2::labs(x = x_lab, y = "Step in the solution path",
                  title = paste0("Solution path (", object$method, ")"),
                  subtitle = if (has_interval) {
                    "Bars show the interval that proposed each candidate"
                  } else {
                    NULL
                  })
}

# Internal: integer axis breaks without pulling in the scales package.
#' @noRd
scales_int_breaks <- function(v) {
  r <- range(v)
  by <- max(1L, ceiling(diff(r) / 10))
  seq(r[1], r[2], by = by)
}

#' Scale space: the statistic across bandwidths
#'
#' Sweeps a multiscale detector's bandwidth and returns the statistic at
#' every (location, bandwidth) pair. The resulting heatmap answers the
#' question a single-bandwidth fit cannot: \emph{at which resolutions does
#' this feature exist?} A change that is significant only at a wide bandwidth
#' is a slow shift; one that appears only at a narrow bandwidth is a spike.
#'
#' @param x A numeric vector (or, for \code{method = "npmojo"}, a matrix
#'   with rows as time points), or a \code{ggcpt} object produced by a
#'   multiscale engine (\code{subset(cpt_methods(), scale_space)$method}).
#' @param bandwidths Integer vector of bandwidths to sweep. Defaults to a
#'   geometric grid between \code{max(5, n/50)} and \code{n/4}.
#' @param method Which engine to sweep: \code{"mosum"} (default) or
#'   \code{"npmojo"}. Taken from \code{x} when it is a \code{ggcpt} produced
#'   by one of them.
#' @param ... Additional arguments passed to the engine at each bandwidth.
#'
#' @return \code{cpt_scale_space()} returns a tibble with \code{index},
#'   \code{bandwidth}, \code{statistic}, \code{threshold} and
#'   \code{significant}. \code{ggcpt_scale_space()} draws the heatmap with
#'   the accepted changepoints overlaid.
#' @seealso \code{\link{cpt_statistic}()}.
#' @export
#' @examplesIf requireNamespace("mosum", quietly = TRUE)
#' set.seed(2026)
#' x <- c(rnorm(300), rnorm(300, 2))
#' ss <- cpt_scale_space(x, bandwidths = c(20, 40, 80))
#' head(ss)
cpt_scale_space <- function(x, bandwidths = NULL,
                            method = c("mosum", "npmojo"), ...) {
  if (is_ggcpt(x)) {
    if (missing(method) && x$method %in% c("mosum", "npmojo")) {
      method <- x$method
    }
    # A multivariate result keeps its coordinates: npmojo sweeps them all,
    # and flattening to the first one would sweep a different series from
    # the one that was detected on.
    series <- if (!is.null(x$data_wide) && n_coordinates(x) > 1) {
      as.matrix(x$data_wide[, setdiff(names(x$data_wide),
                                      c("index", "index_value")),
                            drop = FALSE])
    } else {
      x$data$value
    }
  } else {
    validate_data(x)
    series <- x
  }
  method <- match.arg(method)
  need_pkg(if (method == "mosum") "mosum" else "CptNonPar")

  # mosum is univariate; npmojo is not, and refusing its matrix here would
  # make the scale-space view unavailable for the one multiscale engine that
  # needs it most.
  if (method == "mosum") {
    series <- as_uni_vector(series, "scale_space")
  } else if (is.data.frame(series)) {
    series <- as_mv_matrix(series)
  }
  n <- NROW(series)

  if (is.null(bandwidths)) {
    lo <- max(5, floor(n / 50))
    hi <- max(lo + 1, floor(n / 4))
    bandwidths <- unique(as.integer(round(
      exp(seq(log(lo), log(hi), length.out = 8))
    )))
  }
  bandwidths <- sort(unique(as.integer(bandwidths)))
  bandwidths <- bandwidths[bandwidths >= 2 & 2 * bandwidths < n]
  if (length(bandwidths) == 0) {
    stop("No usable bandwidth: a moving window needs `2 * G < n`, and the ",
         "series has ", n, " observations.", call. = FALSE)
  }

  rows <- lapply(bandwidths, function(G) {
    if (method == "mosum") {
      fit <- tryCatch(mosum::mosum(series, G = G, ...),
                      error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      thr <- as.numeric(fit$threshold.value %||% NA_real_)
      stat <- as.numeric(fit$stat)
      cp <- as.integer(fit$cpts)
    } else {
      fit <- tryCatch(CptNonPar::np.mojo(series, G = G, ...),
                      error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      # np.mojo names its per-location statistic `test.stat`, and
      # `threshold` is the *rule* ("bootstrap"), not the number -- that is
      # `threshold.val`. Reading `$threshold` here would put a character
      # string where a cutoff belongs and make every cell insignificant.
      fl <- function(nm) if (is.list(fit)) fit[[nm, exact = TRUE]] else NULL
      stat <- as.numeric(fl("test.stat") %||% rep(NA_real_, n))
      thr <- suppressWarnings(as.numeric(fl("threshold.val") %||% NA_real_))[1]
      cp <- as.integer(fl("cpts") %||% integer(0))
    }
    if (length(stat) != n) {
      full <- rep(NA_real_, n)
      full[seq_len(min(n, length(stat)))] <- stat[seq_len(min(n, length(stat)))]
      stat <- full
    }
    tibble::tibble(index = seq_len(n), bandwidth = G, statistic = stat,
                   threshold = thr,
                   significant = is.finite(stat) & is.finite(thr) &
                     stat > thr,
                   detected = seq_len(n) %in% cp)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    stop("No bandwidth produced a usable fit for method `", method, "`.",
         call. = FALSE)
  }
  out <- do.call(rbind, rows)
  attr(out, "method") <- method
  out
}

#' @rdname cpt_scale_space
#' @export
ggcpt_scale_space <- function(x, bandwidths = NULL,
                              method = c("mosum", "npmojo"), ...) {
  # Passing `method` straight through would make `missing(method)` FALSE
  # inside cpt_scale_space(), so a result from npmojo would be swept with
  # mosum. Resolve the default here instead.
  if (missing(method) && is_ggcpt(x) && x$method %in% c("mosum", "npmojo")) {
    method <- x$method
  }
  ss <- cpt_scale_space(x, bandwidths = bandwidths, method = method, ...)
  eng <- attr(ss, "method")
  idx_vals <- if (is_ggcpt(x)) plot_index(x) else seq_len(max(ss$index))
  x_lab <- if (is_ggcpt(x)) plot_index_label(x) else "Index"
  ss$index_x <- idx_vals[ss$index]

  det <- ss[ss$detected, , drop = FALSE]

  p <- ggplot2::ggplot(ss, ggplot2::aes(index_x, factor(bandwidth),
                                        fill = statistic)) +
    ggplot2::geom_raster(na.rm = TRUE) +
    ggplot2::scale_fill_viridis_c(option = "D", na.value = "grey95",
                                  name = "Statistic") +
    ggplot2::labs(x = x_lab, y = "Bandwidth (G)",
                  title = paste0("Scale space (", eng, ")"),
                  subtitle = "Points mark the changepoints accepted at each bandwidth")
  if (nrow(det) > 0) {
    p <- p + ggplot2::geom_point(data = det, colour = "white", size = 1.6,
                                 shape = 21, fill = "#D55E00")
  }
  p
}
