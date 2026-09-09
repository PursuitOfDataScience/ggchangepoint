# ---------------------------------------------------------------------------
# Theme K: benchmarking and evaluation.
#
# cpt_metrics() implements the right metrics; until now nothing supplied the
# right data or ran the grid. cpt_benchmark() is mostly composition -- a grid
# of (dataset x method) run through cpt_detect() and cpt_metrics() -- and the
# work is in the result class, in not letting one failing engine kill the
# run, and in respecting multi-annotator ground truth rather than collapsing
# it to a single "true" set.
#
# TCPD is downloaded and cached, never bundled: it is other people's data
# under other people's licences. Every example and test here runs offline
# against cpt_simulate() signals.
# ---------------------------------------------------------------------------

#' Benchmark detectors across datasets
#'
#' Runs a grid of methods over a collection of labelled series and scores
#' every cell with \code{\link{cpt_metrics}()} (or
#' \code{\link{cpt_metrics_annotated}()} when a dataset carries several
#' annotators). An engine that errors on one dataset records the message and
#' the grid continues.
#'
#' @param datasets A named list of datasets. Each element is either a plain
#'   numeric vector (no ground truth — only descriptive columns are filled)
#'   or a list with \code{series} and one of \code{truth},
#'   \code{changepoints} (an integer vector) or \code{annotations} (a list
#'   of integer vectors, one per annotator). A list carrying none of those
#'   is scored \code{NA} and warns.
#'   \code{\link{cpt_datasets}()} builds a ready-made collection.
#' @param methods Character vector of method names.
#' @param metrics Which metrics to keep. Defaults to
#'   \code{c("covering", "f1")}, the pair van den Burg and Williams (2020)
#'   established as the benchmark standard.
#' @param tolerance Matching window passed to the metrics. Defaults to
#'   \code{5}.
#' @param change_in Passed to every detector.
#' @param parallel Use \code{future::plan()} when \pkg{future.apply} is
#'   available? Defaults to \code{TRUE}.
#' @param progress Show a \pkg{progressr} progress bar when that package is
#'   installed and a handler is enabled? Defaults to \code{TRUE}.
#' @param seed Optional seed. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments passed to every \code{cpt_detect()} call.
#'
#' @return A \code{ggcpt_benchmark} object: a tibble with one row per
#'   (dataset, method) — \code{dataset}, \code{method}, \code{n},
#'   \code{n_annotators} (how many ground-truth sets the dataset supplied),
#'   \code{n_cp}, the requested metrics, \code{runtime}, \code{error} — with
#'   \code{print()}, \code{tidy()} and \code{autoplot()}
#'   (\code{"heatmap"}, \code{"ranks"}, \code{"critical_difference"}).
#' @references
#' \insertRef{vandenburg2020evaluation}{ggchangepoint}
#' @seealso \code{\link{cpt_datasets}()}, \code{\link{cpt_load_tcpd}()},
#'   \code{\link{cpt_metrics}()}.
#' @export
#' @examples
#' \donttest{
#' bm <- cpt_benchmark(cpt_datasets(n = 200, seed = 1),
#'                     methods = c("pelt", "binseg", "amoc"),
#'                     progress = FALSE)
#' bm
#' ggplot2::autoplot(bm)
#' }
cpt_benchmark <- function(datasets, methods = c("pelt", "binseg", "wbs"),
                          metrics = c("covering", "f1"), tolerance = 5,
                          change_in = "mean", parallel = TRUE,
                          progress = TRUE, seed = NULL, ...) {
  if (!is.list(datasets) || length(datasets) == 0) {
    stop("`datasets` must be a non-empty named list; see cpt_datasets().",
         call. = FALSE)
  }
  methods <- unique(as.character(methods))
  # An empty method set builds a zero-row grid, and the attribute the result
  # carries is then set on NULL -- base R answers "attempt to set an
  # attribute on NULL", which says nothing about the argument. `datasets`
  # above is checked the same way, for the same reason.
  if (length(methods) == 0L) {
    stop("`methods` is empty: `cpt_benchmark()` needs at least one method ",
         "to score. See cpt_methods() for what is available.", call. = FALSE)
  }
  metric_choices <- c("covering", "f1", "precision", "recall", "hausdorff",
                      "rand_index", "annotation_error", "mae_matched",
                      "rmse_matched")
  metrics <- match.arg(metrics, metric_choices, several.ok = TRUE)
  validate_scalar(tolerance, "tolerance", min = 0)
  validate_flag(parallel, "parallel")
  validate_flag(progress, "progress")

  nms <- names(datasets) %||% rep("", length(datasets))
  nms[!nzchar(nms) | is.na(nms)] <- paste0("dataset_",
                                           which(!nzchar(nms) | is.na(nms)))
  names(datasets) <- make.unique(nms)
  datasets <- stats::setNames(
    lapply(names(datasets), function(nm) normalise_dataset(datasets[[nm]], nm)),
    names(datasets))

  grid <- expand.grid(dataset = names(datasets), method = methods,
                      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  local_seed(seed)

  use_progress <- isTRUE(progress) &&
    requireNamespace("progressr", quietly = TRUE)
  p <- if (use_progress) progressr::progressor(steps = nrow(grid)) else NULL

  run_cell <- function(i) {
    if (!is.null(p)) p()
    d <- datasets[[grid$dataset[i]]]
    t0 <- proc.time()[["elapsed"]]
    fit <- tryCatch(
      cpt_detect(d$series, method = grid$method[i], change_in = change_in,
                 ...),
      error = function(e) structure(conditionMessage(e), class = "cpt_failed")
    )
    rt <- proc.time()[["elapsed"]] - t0
    if (inherits(fit, "cpt_failed")) {
      return(list(cp = integer(0), runtime = rt,
                  error = as.character(fit)))
    }
    list(cp = fit$changepoints$cp, runtime = rt, error = NA_character_)
  }

  has_future <- isTRUE(parallel) &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")
  cells <- if (has_future) {
    future.apply::future_lapply(seq_len(nrow(grid)),
                                with_session_registry(run_cell),
                                future.seed = seed %||% TRUE)
  } else {
    lapply(seq_len(nrow(grid)), run_cell)
  }

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    d <- datasets[[grid$dataset[i]]]
    cell <- cells[[i]]
    n <- length(d$series)
    base <- tibble::tibble(dataset = grid$dataset[i],
                           method = grid$method[i],
                           n = n,
                           n_annotators = length(d$annotations),
                           n_cp = length(cell$cp),
                           runtime = cell$runtime,
                           error = cell$error)
    if (length(d$annotations) == 0) {
      for (m in metrics) base[[m]] <- NA_real_
      return(base)
    }
    scores <- if (length(d$annotations) == 1) {
      cpt_metrics(cell$cp, d$annotations[[1]], n = n, margin = tolerance)
    } else {
      cpt_metrics_annotated(cell$cp, d$annotations, n = n,
                            margin = tolerance)
    }
    for (m in metrics) {
      base[[m]] <- if (m %in% names(scores)) as.numeric(scores[[m]][1]) else NA_real_
    }
    base
  })
  out <- do.call(rbind, rows)
  # Metrics first, bookkeeping after: this is the table people read.
  front <- c("dataset", "method", metrics)
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]

  attr(out, "metrics") <- metrics
  attr(out, "tolerance") <- tolerance
  class(out) <- c("ggcpt_benchmark", class(out))
  out
}

# Internal: accept a bare vector, a list(series, truth), or a
# list(series, annotations) and return the canonical form.  Extraction is
# exact: `$` partial-matches, so a list carrying `annotations_raw` would
# otherwise be read as ground truth.
#' @noRd
normalise_dataset <- function(d, name = NULL) {
  if (is.numeric(d) && is.null(dim(d))) {
    return(list(series = as.numeric(d), annotations = list()))
  }
  if (inherits(d, "tbl_df") || is.data.frame(d)) {
    truth <- attr(d, "true_changepoints")
    series <- if ("value" %in% names(d)) as.numeric(d$value) else {
      as.numeric(d[[1]])
    }
    return(list(series = series,
                annotations = if (is.null(truth)) list() else list(truth)))
  }
  el <- function(nm) if (is.list(d)) d[[nm, exact = TRUE]] else NULL
  if (!is.list(d) || is.null(el("series"))) {
    stop("Each dataset must be a numeric vector, a cpt_simulate() tibble, or ",
         "a list with a `series` element.", call. = FALSE)
  }
  # `changepoints` is accepted alongside `truth` because that is what
  # cpt_simulate() calls the same thing; without it a hand-built dataset
  # scored silently as NA, with n_annotators = 0 and no hint why.
  raw <- el("annotations") %||% el("truth") %||% el("changepoints")
  ann <- if (is.null(raw)) {
    list()
  } else if (is.list(raw)) {
    lapply(raw, function(v) as_cp_locations(unlist(v), "annotations"))
  } else {
    list(as_cp_locations(raw, "annotations"))
  }
  if (length(ann) == 0 && length(setdiff(names(d), "series")) > 0) {
    warning("Dataset ", if (is.null(name)) "" else paste0("`", name, "` "),
            "has no ground truth: none of `annotations`, `truth` or ",
            "`changepoints` is present, so its metrics will be NA. Found: ",
            paste(setdiff(names(d), "series"), collapse = ", "), ".",
            call. = FALSE)
  }
  list(series = as.numeric(el("series")), annotations = ann)
}

#' @rdname cpt_benchmark
#' @param x A \code{ggcpt_benchmark} object.
#' @export
print.ggcpt_benchmark <- function(x, ...) {
  # `%||%` on both attributes: reclass_subset() keeps the subclass whenever
  # the required COLUMNS survive a subset, and tibble's `[` need not carry
  # a custom attribute through a row subset -- so `bm[1, ]` can arrive here
  # with neither. print.ggcpt_batch() guards the same way.
  metrics <- attr(x, "metrics") %||% intersect(names(x), cpt_metric_names())
  cat("ggcpt_benchmark (", length(unique(x$dataset)), " dataset(s) x ",
      length(unique(x$method)), " method(s), tolerance ",
      attr(x, "tolerance") %||% "?", ")\n", sep = "")
  n_err <- sum(!is.na(x[["error", exact = TRUE]]))
  if (n_err > 0) {
    cat("  ", n_err, " cell(s) errored; see the `error` column.\n", sep = "")
  }
  rk <- benchmark_ranks(x)
  if (!is.null(rk)) {
    cat("\nMean rank across datasets (1 = best):\n")
    print(rk, n = 20)
  }
  cat("\n")
  print(tibble::as_tibble(x)[, c("dataset", "method", metrics)], n = 12)
  invisible(x)
}

#' @rdname cpt_benchmark
#' @export
tidy.ggcpt_benchmark <- function(x, ...) {
  # Same fallback as print(): a row subset can arrive without the attribute.
  metrics <- attr(x, "metrics") %||% intersect(names(x), cpt_metric_names())
  rows <- lapply(metrics, function(m) {
    tibble::tibble(dataset = x$dataset, method = x$method,
                   metric = m, value = x[[m]])
  })
  do.call(rbind, rows)
}

# Internal: mean rank per method over the datasets, on the first metric
# (all the supported metrics are "higher is better" except the distance
# ones, which are inverted here so a rank of 1 always means best).
#' @noRd
benchmark_ranks <- function(x, metric = NULL) {
  metrics <- attr(x, "metrics") %||% intersect(names(x), cpt_metric_names())
  metric <- metric %||% metrics[1]
  if (length(metric) != 1L || is.na(metric)) return(NULL)
  if (!metric %in% names(x) || all(is.na(x[[metric]]))) return(NULL)
  lower_better <- metric %in% c("hausdorff", "annotation_error",
                                "mae_matched", "rmse_matched")
  per <- split(seq_len(nrow(x)), x$dataset)
  rk <- lapply(per, function(ii) {
    v <- x[[metric]][ii]
    v[is.na(v)] <- if (lower_better) Inf else -Inf
    tibble::tibble(method = x$method[ii],
                   rank = rank(if (lower_better) v else -v,
                               ties.method = "average"))
  })
  rk <- do.call(rbind, rk)
  agg <- stats::aggregate(rank ~ method, data = rk, FUN = mean)
  # A dataset on which every method scored NA ties them all at the same
  # worst rank -- no information about which is better -- but it still
  # incremented N, and the Nemenyi critical distance
  # CD = q sqrt(k(k+1)/(6N)) SHRINKS with N. So a benchmark where 3 of 8
  # datasets failed drew a narrower critical distance than the 5
  # informative ones support and called more methods distinguishable than
  # they are: anti-conservative, which is the dangerous direction. Count
  # the datasets that carry at least one score.
  informative <- vapply(per, function(ii) any(!is.na(x[[metric]][ii])),
                        logical(1))
  out <- tibble::tibble(method = agg$method, mean_rank = agg$rank,
                        n_datasets = sum(informative),
                        n_datasets_total = length(per))
  out[order(out$mean_rank), , drop = FALSE]
}

#' @rdname cpt_benchmark
#' @param object A \code{ggcpt_benchmark} object (for \code{autoplot()}).
#' @param plot_type \code{"heatmap"} (method by dataset, coloured by the
#'   metric), \code{"ranks"} (mean rank per method) or
#'   \code{"critical_difference"} (the Demšar diagram: mean ranks with the
#'   Nemenyi critical distance, the standard way this literature says
#'   "method A beats method B").
#' @param metric Which metric to plot. Defaults to the first one scored.
#' @param alpha Level for the critical distance, in \code{(0, 1)}.
#'   Defaults to \code{0.05}. Only used by
#'   \code{plot_type = "critical_difference"}, which also needs at least two
#'   methods to compare.
#' @section Reading the critical-difference diagram:
#' Rank 1 is best. Each method's mean rank is taken over the datasets, in
#' the direction the metric calls for -- higher is better for
#' \code{covering}, \code{f1}, \code{precision}, \code{recall} and
#' \code{rand_index}; lower for \code{hausdorff},
#' \code{annotation_error}, \code{mae_matched} and \code{rmse_matched}.
#' A metric that came back \code{NA} takes the \emph{worst} rank on that
#' dataset rather than being dropped, so a method that failed there is
#' penalised for it instead of quietly scoring on a smaller sample; ties
#' share the average rank.
#'
#' The bar is the Nemenyi critical distance
#' \deqn{CD = q_\alpha \sqrt{k(k + 1) / (6N)}}
#' for \eqn{k} methods over \eqn{N} datasets, where \eqn{q_\alpha} is
#' the Studentised range statistic at \eqn{\alpha} divided by
#' \eqn{\sqrt 2} (Demsar, 2006 -- the constants agree with that paper's
#' Table 5 to three decimals for \eqn{k = 2, \ldots, 10}). Two methods
#' whose mean ranks differ by less than \eqn{CD} are not distinguished at
#' that level.
#'
#' \strong{One caveat the diagram cannot show.} Nemenyi is a
#' \emph{post-hoc} procedure, and the convention is to run it only after a
#' Friedman test has rejected the null that all methods rank equally.
#' \code{autoplot()} does not run that omnibus test -- it draws the
#' diagram it is asked for -- so a gap wider than \eqn{CD} on a grid where
#' Friedman would not have rejected is not the significant difference it
#' looks like. With the handful of datasets \code{\link{cpt_datasets}()}
#' supplies, \eqn{N} is small and \eqn{CD} correspondingly wide; read the
#' diagram as a descriptive summary unless \eqn{N} is large enough to
#' support the test.
#' @references
#' \insertRef{demsar2006statistical}{ggchangepoint}
#' @export
autoplot.ggcpt_benchmark <- function(object,
                                     plot_type = c("heatmap", "ranks",
                                                   "critical_difference"),
                                     metric = NULL, alpha = 0.05, ...) {
  plot_type <- match.arg(plot_type)
  metrics <- attr(object, "metrics") %||%
    intersect(names(object), cpt_metric_names())
  metric <- metric %||% metrics[1]
  if (!metric %in% names(object)) {
    stop("`metric = \"", metric, "\"` was not scored. Available: ",
         paste(metrics, collapse = ", "), ".", call. = FALSE)
  }

  if (plot_type == "heatmap") {
    d <- tibble::as_tibble(object)
    d$value <- d[[metric]]
    return(
      ggplot2::ggplot(d, ggplot2::aes(method, dataset, fill = value)) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
        # "\u2014" rather than the literal em dash: R code must be ASCII to
        # be portable, and only comments are exempt.
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(is.na(value), "\u2014",
                                      format(round(value, 2)))),
          size = 3, colour = "grey15") +
        ggplot2::scale_fill_viridis_c(option = "D", na.value = "grey90",
                                      name = metric) +
        ggplot2::labs(x = NULL, y = NULL,
                      title = paste0("Benchmark: ", metric),
                      subtitle = paste0("Tolerance ",
                                        attr(object, "tolerance"),
                                        "; blank cells are engine failures"))
    )
  }

  rk <- benchmark_ranks(object, metric)
  if (is.null(rk)) {
    # benchmark_ranks() returns NULL on two different conditions and this
    # reported both as the first one. An all-NA metric column is also what
    # you get when every detector ERRORED on every dataset, and sending that
    # user to fix `annotations` points away from the `error` column that
    # holds the diagnosis.
    if (!metric %in% names(object)) {
      stop("No dataset carries ground truth, so there is nothing to rank. ",
           "Supply `truth` or `annotations` with each dataset.",
           call. = FALSE)
    }
    failed <- if ("error" %in% names(object)) sum(!is.na(object$error)) else 0L
    if (failed == nrow(object)) {
      stop("Every method failed on every dataset, so there is nothing to ",
           "rank. The `error` column of the benchmark table holds the ",
           "message from each run: `subset(bm, !is.na(error))$error`.",
           call. = FALSE)
    }
    stop("`", metric, "` is NA for every row, so there is nothing to rank. ",
         if (failed > 0) paste0(failed, " of ", nrow(object),
                                " runs failed (see the `error` column); ")
         else "",
         "a metric is NA when the dataset carries no ground truth or the ",
         "metric is undefined for the result -- `cpt_metrics()` on one row ",
         "shows which.", call. = FALSE)
  }
  rk$method <- factor(rk$method, levels = rev(rk$method))

  if (plot_type == "ranks") {
    return(
      ggplot2::ggplot(rk, ggplot2::aes(mean_rank, method)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = mean_rank,
                                           y = method, yend = method),
                              colour = "grey70") +
        ggplot2::geom_point(size = 3, colour = "#0072B2") +
        ggplot2::labs(x = paste0("Mean rank over ", rk$n_datasets[1],
                                 " dataset(s) (1 = best)"), y = NULL,
                      title = paste0("Method ranking by ", metric))
    )
  }

  # critical_difference
  k <- nrow(rk)
  N <- rk$n_datasets[1]
  cd <- nemenyi_cd(k, N, alpha)
  best <- min(rk$mean_rank)
  rk$within_cd <- rk$mean_rank <= best + cd
  ggplot2::ggplot(rk, ggplot2::aes(mean_rank, method)) +
    ggplot2::annotate("rect", xmin = best, xmax = best + cd,
                      ymin = -Inf, ymax = Inf, fill = "#009E73",
                      alpha = 0.12) +
    ggplot2::geom_point(ggplot2::aes(shape = within_cd), size = 3,
                        colour = "#0072B2") +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                                name = "Within CD of best") +
    ggplot2::labs(x = "Mean rank (1 = best)", y = NULL,
                  title = paste0("Critical-difference diagram (", metric,
                                 ")"),
                  subtitle = paste0("Nemenyi CD = ", format(cd, digits = 3),
                                    " at alpha = ", alpha, ", k = ", k,
                                    " methods, N = ", N, " datasets"))
}

# Internal: the metric columns cpt_benchmark() can score, used as the
# fallback when a subset has dropped the `metrics` attribute.
#' @noRd
cpt_metric_names <- function() {
  c("f1", "precision", "recall", "covering", "hausdorff", "mae_matched",
    "rmse_matched", "annotation_error", "n_pred", "n_true")
}

# Internal: the Nemenyi critical distance,
#   CD = q_alpha * sqrt(k (k + 1) / (6 N)),
# where q_alpha is the studentised range statistic divided by sqrt(2)
# (Demsar 2006, Table 5). Tabulated because the exact quantile needs the
# studentised range distribution with infinite degrees of freedom, which
# base R's ptukey supplies -- so it is computed, with the table kept only as
# a cross-check for the common cases.
#' @noRd
nemenyi_cd <- function(k, N, alpha = 0.05) {
  # qtukey() is undefined below nmeans = 2 and for a probability outside
  # (0, 1), and it answers both with NaN rather than an error -- which then
  # propagated to `annotate("rect", xmax = NaN)`, an all-NA `within_cd`
  # column and an NA level in the shape scale, i.e. a diagram that drew
  # without saying anything was wrong. The test itself also needs more than
  # one method to compare.
  validate_scalar(alpha, "alpha", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 2) {
    stop("A critical-difference diagram compares methods to each other, so ",
         "it needs at least two; got ", if (is.numeric(k)) k else "none",
         ". Use `plot_type = \"heatmap\"` or `\"ranks\"` for a single ",
         "method.", call. = FALSE)
  }
  if (!is.numeric(N) || length(N) != 1L || is.na(N) || N < 1) {
    stop("The Nemenyi critical distance is computed over datasets, so it ",
         "needs at least one; got ", if (is.numeric(N)) N else "none", ".",
         call. = FALSE)
  }
  q <- stats::qtukey(1 - alpha, nmeans = k, df = Inf) / sqrt(2)
  q * sqrt(k * (k + 1) / (6 * N))
}

#' A catalogue of benchmark datasets
#'
#' Builds a ready-made collection of labelled series for
#' \code{\link{cpt_benchmark}()}. By default these are the canonical
#' simulated signals this package already ships — which means the benchmark
#' runs offline, deterministically, and inside \code{R CMD check}. Pass
#' \code{source = "tcpd"} for the Turing Change Point Dataset instead, which
#' is downloaded and cached.
#'
#' @param source \code{"simulated"} (default) or \code{"tcpd"}.
#' @param n Length of each simulated series. Defaults to \code{500}.
#' @param seed Seed for the simulated signals. Defaults to \code{1}. The
#'   seed is scoped to this call: \code{.Random.seed} is saved and restored,
#'   so a seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param names Optional subset of dataset names.
#' @param ... Passed to \code{\link{cpt_load_tcpd}()} for
#'   \code{source = "tcpd"}.
#' @return A named list of \code{list(series, annotations)} datasets, ready
#'   for \code{\link{cpt_benchmark}()}.
#' @seealso \code{\link{cpt_load_tcpd}()}, \code{\link{cpt_benchmark}()}.
#' @export
#' @examples
#' names(cpt_datasets(n = 200))
cpt_datasets <- function(source = c("simulated", "tcpd"), n = 500, seed = 1,
                         names = NULL, ...) {
  source <- match.arg(source)
  if (source == "tcpd") {
    return(cpt_load_tcpd(name = names, ...))
  }
  validate_scalar(n, "n", min = 100)
  builders <- list(
    blocks = function() signal_blocks(max(n, 100), seed = seed),
    fms = function() signal_fms(max(n, 40), seed = seed + 1),
    mix = function() signal_mix(max(n, 40), seed = seed + 2),
    teeth = function() signal_teeth(max(n, 200), seed = seed + 3),
    stairs = function() signal_stairs(max(n, 10), seed = seed + 4),
    step = function() {
      cpt_simulate(n, changepoints = c(floor(n / 3), floor(2 * n / 3)),
                   change_in = "mean", params = c(0, 3, 1), seed = seed + 5)
    },
    ar1 = function() {
      cpt_simulate(n, changepoints = floor(n / 2), change_in = "mean",
                   params = c(0, 2), noise = "ar1", rho = 0.5,
                   seed = seed + 6)
    },
    heavy = function() {
      cpt_simulate(n, changepoints = floor(n / 2), change_in = "mean",
                   params = c(0, 3), noise = "t", df = 3, seed = seed + 7)
    },
    varshift = function() {
      cpt_simulate(n, changepoints = floor(n / 2), change_in = "var",
                   params = c(1, 3), seed = seed + 8)
    }
  )
  # `names` is this function's own argument, which is why every call to the
  # base function inside it (and inside its siblings below) has to be
  # written `base::names()`. Keep it that way: a plain `names(x)` here
  # silently resolves to the argument.
  if (!is.null(names)) {
    unknown <- setdiff(names, base::names(builders))
    if (length(unknown) > 0) {
      stop("Unknown dataset(s): ", paste(unknown, collapse = ", "),
           ". Available: ", paste(base::names(builders), collapse = ", "),
           ".", call. = FALSE)
    }
    builders <- builders[names]
  }
  lapply(builders, function(f) {
    d <- f()
    list(series = as.numeric(d$value),
         annotations = list(as.integer(attr(d, "true_changepoints"))))
  })
}

#' Download and cache the Turing Change Point Dataset
#'
#' Fetches the benchmark of van den Burg and Williams (2020) — real series
#' from many domains, each annotated independently by several human
#' annotators — and caches it under \code{tools::R_user_dir()}. The
#' multi-annotator structure is the point: scoring against a single "true"
#' set silently discards the disagreement, and
#' \code{\link{cpt_metrics_annotated}()} is built to keep it.
#'
#' @param name Dataset name, a character vector of names, or \code{NULL}
#'   (the default) for the whole catalogue.
#' @param cache_dir Where to cache. Defaults to
#'   \code{tools::R_user_dir("ggchangepoint", "cache")}.
#' @param refresh Re-download even when a cached copy exists? Defaults to
#'   \code{FALSE}.
#' @param quiet Suppress progress messages. Defaults to \code{FALSE}.
#' @return With \code{name = NULL}, a tibble catalogue (\code{name},
#'   \code{n_obs}, \code{n_dim}, \code{n_annotators}, \code{cached}).
#'   Otherwise a named list of \code{list(series, annotations, index,
#'   longname)} datasets, in the shape \code{\link{cpt_benchmark}()} takes.
#' @section Network access and licences:
#' This function downloads from GitHub, so it needs a working connection and
#' is skipped in every example and test here. The data belong to their
#' original owners under their own licences; this package neither bundles nor
#' redistributes them. A few TCPD series are not in the repository at all
#' (their sources do not permit redistribution) and are reported as
#' unavailable rather than silently omitted.
#' @references
#' \insertRef{vandenburg2020evaluation}{ggchangepoint}
#' @export
#' @examples
#' # Not run by R CMD check: every call downloads from the Turing Change
#' # Point Dataset's GitHub repository, and an example must not require
#' # network access.
#' \dontrun{
#' cpt_load_tcpd()                       # the catalogue
#' d <- cpt_load_tcpd("nile")            # one dataset
#' cpt_benchmark(d, methods = c("pelt", "amoc"))
#' }
cpt_load_tcpd <- function(name = NULL, cache_dir = NULL, refresh = FALSE,
                          quiet = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to read the Turing Change Point ",
         "Dataset. Install it with install.packages('jsonlite').",
         call. = FALSE)
  }
  validate_flag(refresh, "refresh")
  validate_flag(quiet, "quiet")
  cache_dir <- cache_dir %||% tools::R_user_dir("ggchangepoint", "cache")
  tcpd_dir <- file.path(cache_dir, "tcpd")
  dir.create(tcpd_dir, showWarnings = FALSE, recursive = TRUE)

  base_url <- paste0("https://raw.githubusercontent.com/",
                     "alan-turing-institute/TCPD/master")
  ann_file <- file.path(tcpd_dir, "annotations.json")
  if (isTRUE(refresh) || !file.exists(ann_file)) {
    if (!quiet) message("Downloading TCPD annotations ...")
    ok <- tcpd_download(paste0(base_url, "/annotations.json"), ann_file)
    if (!ok) {
      stop("Could not download the TCPD annotations. Check the network ",
           "connection, or point `cache_dir` at a copy you already have.",
           call. = FALSE)
    }
  }
  annotations <- jsonlite::fromJSON(ann_file, simplifyVector = FALSE)

  if (is.null(name)) {
    nms <- base::names(annotations)
    return(tibble::tibble(
      name = nms,
      n_annotators = vapply(annotations, length, integer(1)),
      cached = file.exists(file.path(tcpd_dir, paste0(nms, ".json")))
    ))
  }

  name <- as.character(name)
  unknown <- setdiff(name, base::names(annotations))
  if (length(unknown) > 0) {
    stop("Unknown TCPD dataset(s): ", paste(unknown, collapse = ", "),
         ". Call cpt_load_tcpd() with no arguments for the catalogue.",
         call. = FALSE)
  }

  out <- list()
  for (nm in name) {
    dst <- file.path(tcpd_dir, paste0(nm, ".json"))
    if (isTRUE(refresh) || !file.exists(dst)) {
      if (!quiet) message("Downloading TCPD dataset '", nm, "' ...")
      ok <- tcpd_download(paste0(base_url, "/datasets/", nm, "/", nm,
                                 ".json"), dst)
      if (!ok) {
        warning("TCPD dataset '", nm, "' could not be downloaded from ",
                base_url, ": either it is not in the repository (several ",
                "series' sources do not permit redistribution) or the ",
                "network call failed. Skipping it.", call. = FALSE)
        next
      }
    }
    j <- jsonlite::fromJSON(dst, simplifyVector = FALSE)
    raws <- lapply(j$series, function(s) {
      # NA-substitute BEFORE unlist(), not after. `unlist()` *drops* NULLs
      # rather than preserving a slot for them -- `unlist(list(1, 2, NULL,
      # 4))` has length 3 -- so running the substitution afterwards found
      # nothing left to find, and a JSON `null` did not become `NA`: it
      # vanished, shifting every later observation down by one and
      # invalidating the human annotations cpt_benchmark() scores against.
      # TCPD ships series with missing values, so this was reachable with
      # the real data the function exists to load. If coordinates carried
      # different numbers of nulls, the do.call(cbind, .) below then
      # recycled them against each other.
      v <- s$raw
      v[vapply(v, is.null, logical(1))] <- NA
      as.numeric(unlist(v))
    })
    series <- if (length(raws) == 1) raws[[1]] else do.call(cbind, raws)
    ann <- lapply(annotations[[nm]], function(v) as.integer(unlist(v)))
    out[[nm]] <- list(
      series = series,
      annotations = ann,
      index = unlist(j$time$raw) %||% NULL,
      longname = j$longname %||% nm
    )
  }
  out
}

# Internal: one download, returning TRUE on success. Kept separate so the
# 404 that means "this series is not redistributable" is handled as data
# rather than as an error.
#' @noRd
tcpd_download <- function(url, dst) {
  # A warning is not a failure. download.file() warns about things that do
  # not stop it -- a redirect, a missing content length, a non-fatal libcurl
  # note -- and treating any warning as failure threw away a successful but
  # noisy download and then reported the specific, wrong diagnosis "not in
  # the repository (its source does not permit redistribution)". Judge the
  # file, not the noise: only an ERROR is fatal on its own.
  ok <- tryCatch({
    withCallingHandlers(
      utils::download.file(url, dst, quiet = TRUE, mode = "wb"),
      warning = function(w) invokeRestart("muffleWarning")
    )
    TRUE
  }, error = function(e) FALSE)
  got <- file.exists(dst) && file.size(dst) > 0
  if (!got && file.exists(dst)) unlink(dst)
  ok && got
}

#' Per-annotator ground truth for a benchmark dataset
#'
#' Returns the annotation sets of a dataset one row per (annotator,
#' changepoint), so the disagreement between human annotators is visible
#' instead of averaged away.
#'
#' @param dataset A dataset in the \code{\link{cpt_datasets}()} shape (a list
#'   with \code{annotations}), or a named list of them.
#' @return A tibble with \code{dataset}, \code{annotator} and \code{cp}.
#' @seealso \code{\link{cpt_metrics_annotated}()}.
#' @export
#' @examples
#' cpt_annotations(cpt_datasets(n = 200, names = "step"))
#' @family result class
cpt_annotations <- function(dataset) {
  if (!is.null(dataset$series)) dataset <- list(dataset = dataset)
  rows <- lapply(base::names(dataset), function(nm) {
    d <- normalise_dataset(dataset[[nm]])
    if (length(d$annotations) == 0) return(NULL)
    ann_names <- base::names(d$annotations) %||%
      as.character(seq_along(d$annotations))
    do.call(rbind, lapply(seq_along(d$annotations), function(i) {
      v <- d$annotations[[i]]
      if (length(v) == 0) {
        return(tibble::tibble(dataset = nm, annotator = ann_names[i],
                              cp = NA_integer_))
      }
      tibble::tibble(dataset = nm, annotator = ann_names[i],
                     cp = as.integer(v))
    }))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(tibble::tibble(dataset = character(), annotator = character(),
                          cp = integer()))
  }
  do.call(rbind, rows)
}
