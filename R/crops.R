#' CROPS — the full penalty path of a penalised changepoint method
#'
#' Runs PELT once per \emph{distinct} optimal segmentation as the penalty
#' ranges over \code{[pen_min, pen_max]}, using the CROPS algorithm of
#' Haynes, Eckley and Fearnhead (2017) as implemented by the
#' \pkg{changepoint} package. Instead of committing to one penalty, the
#' analyst sees every segmentation the data admits along the path, together
#' with its cost, and picks the elbow.
#'
#' @param x For \code{cpt_crops()}, a numeric vector; for the \code{print()}
#'   and \code{tidy()} methods, a \code{ggcpt_path} object.
#' @param change_in What to detect change in: \code{"mean"}, \code{"var"},
#'   or \code{"meanvar"}. Defaults to \code{"mean"}.
#' @param pen_min,pen_max The penalty interval to sweep. Default to
#'   \code{log(n)} and \code{10 * log(n)}.
#' @param ... Additional arguments passed to the underlying
#'   \code{changepoint::cpt.mean()}, \code{cpt.var()}, or
#'   \code{cpt.meanvar()} call.
#' @return A \code{ggcpt_path} object: a list with a \code{solutions} tibble
#'   (one row per distinct segmentation: \code{penalty}, \code{n_cpts},
#'   \code{cost}, and a \code{cpts} list-column), the \code{data}, and
#'   metadata. Methods: \code{print()}, \code{tidy()}, and
#'   \code{autoplot()} (elbow plot by default;
#'   \code{type = "path"} for penalty vs. number of changepoints;
#'   \code{type = "segmentations"} for the faceted segmentations).
#' @references
#' \insertRef{haynes2017crops}{ggchangepoint}
#'
#' \insertRef{killick2014changepoint}{ggchangepoint}
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(100), rnorm(100, 3), rnorm(100, -1))
#' path <- cpt_crops(x)
#' path
#' ggplot2::autoplot(path)
#' ggplot2::autoplot(path, type = "segmentations")
cpt_crops <- function(x, change_in = c("mean", "var", "meanvar"),
                      pen_min = NULL, pen_max = NULL, ...) {
  change_in <- match.arg(change_in)

  validate_data(x)
  data_vec <- as_uni_vector(x, "crops")
  n <- length(data_vec)

  if (is.null(pen_min)) pen_min <- log(n)
  if (is.null(pen_max)) pen_max <- 10 * log(n)
  validate_scalar(pen_min, "pen_min", min = 0, min_open = TRUE)
  validate_scalar(pen_max, "pen_max", min = 0, min_open = TRUE)
  if (pen_min >= pen_max) {
    stop("`pen_min` must be strictly smaller than `pen_max`.", call. = FALSE)
  }

  cpt_fun <- switch(change_in,
    mean = changepoint::cpt.mean,
    var = changepoint::cpt.var,
    meanvar = changepoint::cpt.meanvar
  )

  # The CROPS run prints progress; keep the console clean.
  utils::capture.output(
    fit <- cpt_fun(data_vec, method = "PELT", penalty = "CROPS",
                   pen.value = c(pen_min, pen_max), ...)
  )

  pens <- as.numeric(changepoint::pen.value.full(fit))
  cpts_mat <- changepoint::cpts.full(fit)
  if (is.null(dim(cpts_mat))) {
    cpts_mat <- matrix(cpts_mat, nrow = 1)
  }

  cpts_list <- lapply(seq_len(nrow(cpts_mat)), function(i) {
    v <- cpts_mat[i, ]
    v <- as.integer(v[!is.na(v)])
    v[v >= 1 & v < n]
  })
  # One penalty value per solution; the changepoint package returns the
  # penalty at which each segmentation first becomes optimal.
  if (length(pens) > length(cpts_list)) {
    pens <- pens[seq_along(cpts_list)]
  } else if (length(pens) < length(cpts_list)) {
    pens <- c(pens, rep(NA_real_, length(cpts_list) - length(pens)))
  }

  # Match the changepoint package's cost conventions so the elbow is the
  # same objective CROPS optimised: change-in-var fixes the mean at the
  # global estimate (as cpt.var does); meanvar re-estimates both.
  global_mean <- mean(data_vec)
  seg_cost <- function(cps) {
    breaks <- c(0L, cps, n)
    cost <- 0
    for (i in seq_len(length(breaks) - 1)) {
      seg <- data_vec[(breaks[i] + 1):breaks[i + 1]]
      m <- length(seg)
      if (change_in == "mean") {
        cost <- cost + sum((seg - mean(seg))^2)
      } else {
        mu <- if (change_in == "var") global_mean else mean(seg)
        v <- sum((seg - mu)^2) / m
        if (!is.finite(v) || v <= 0) v <- .Machine$double.eps
        cost <- cost + m * log(v)
      }
    }
    cost
  }
  costs <- vapply(cpts_list, seg_cost, numeric(1))

  solutions <- tibble::tibble(
    penalty = pens,
    n_cpts = vapply(cpts_list, length, integer(1)),
    cost = costs,
    cpts = cpts_list
  )
  solutions <- solutions[order(solutions$n_cpts), , drop = FALSE]

  structure(
    list(
      solutions = solutions,
      data = tibble::tibble(index = seq_len(n), value = data_vec),
      change_in = change_in,
      pen_range = c(pen_min, pen_max),
      fit = fit,
      call = match.call()
    ),
    class = "ggcpt_path"
  )
}

#' @rdname cpt_crops
#' @param object A \code{ggcpt_path} object (for \code{autoplot()}).
#' @param type Plot type for \code{autoplot()}: \code{"elbow"} (cost against
#'   number of changepoints, the classic CROPS diagnostic), \code{"path"}
#'   (number of changepoints against penalty), or \code{"segmentations"}
#'   (the data faceted by solution, with that solution's changepoints
#'   drawn).
#' @param max_facets Maximum number of solutions shown by
#'   \code{type = "segmentations"}. Defaults to \code{12}.
#' @export
autoplot.ggcpt_path <- function(object,
                                type = c("elbow", "path", "segmentations"),
                                max_facets = 12, ...) {
  type <- match.arg(type)
  sol <- object$solutions

  if (type == "elbow") {
    return(
      ggplot2::ggplot(sol, ggplot2::aes(n_cpts, cost)) +
        ggplot2::geom_line(color = "grey50") +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(x = "Number of changepoints",
                      y = "Segmentation cost",
                      title = "CROPS penalty path: cost elbow")
    )
  }

  if (type == "path") {
    sol_p <- sol[is.finite(sol$penalty), , drop = FALSE]
    return(
      ggplot2::ggplot(sol_p, ggplot2::aes(penalty, n_cpts)) +
        ggplot2::geom_step(direction = "vh") +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(x = "Penalty", y = "Number of changepoints",
                      title = "CROPS penalty path")
    )
  }

  # type == "segmentations"
  sol_f <- utils::head(sol, max_facets)
  panels <- do.call(rbind, lapply(seq_len(nrow(sol_f)), function(i) {
    d <- object$data
    d$panel <- paste0(sol_f$n_cpts[i], " changepoint(s)")
    d$.order <- i
    d
  }))
  cp_panels <- do.call(rbind, lapply(seq_len(nrow(sol_f)), function(i) {
    cps <- sol_f$cpts[[i]]
    if (length(cps) == 0) return(NULL)
    data.frame(panel = paste0(sol_f$n_cpts[i], " changepoint(s)"),
               cp = cps, .order = i)
  }))
  panels$panel <- stats::reorder(panels$panel, panels$.order)

  p <- ggplot2::ggplot(panels, ggplot2::aes(index, value)) +
    ggplot2::geom_line(color = "grey50") +
    ggplot2::facet_wrap(~panel) +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "CROPS penalty path: candidate segmentations")

  if (!is.null(cp_panels)) {
    cp_panels$panel <- factor(cp_panels$panel, levels = levels(panels$panel))
    p <- p + ggplot2::geom_vline(
      data = cp_panels,
      ggplot2::aes(xintercept = cp),
      color = "blue", linewidth = 0.4
    )
  }
  p
}

#' @rdname cpt_crops
#' @export
print.ggcpt_path <- function(x, ...) {
  cat("ggcpt_path (CROPS penalty path)\n")
  cat("  Change in:      ", x$change_in, "\n")
  cat("  Penalty range:  [", format(x$pen_range[1], digits = 4), ", ",
      format(x$pen_range[2], digits = 4), "]\n", sep = "")
  cat("  Series length:  ", nrow(x$data), "\n")
  cat("  Distinct segmentations:", nrow(x$solutions), "\n\n")
  print(x$solutions[, c("penalty", "n_cpts", "cost")],
        n = min(nrow(x$solutions), 10))
  invisible(x)
}

#' @rdname cpt_crops
#' @export
tidy.ggcpt_path <- function(x, ...) {
  x$solutions
}
