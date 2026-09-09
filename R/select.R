# ---------------------------------------------------------------------------
# Theme B: choosing the number of changepoints.
#
# cpt_penalty() builds a penalty and cpt_crops() draws the path; neither
# *selects*. cpt_select() does, over one candidate ladder shared by every
# criterion, so "BIC says 3, cross-validation says 5" is a comparison of
# criteria and not of two different searches.
# ---------------------------------------------------------------------------

#' Choose the number of changepoints
#'
#' Builds a ladder of candidate segmentations with \eqn{K = 0, 1, \ldots}
#' changepoints and scores each one, returning the chosen \eqn{K}, the
#' criterion curve behind the choice, and the fitted result at that \eqn{K}.
#' Five criteria are available, including the consistent sample-splitting
#' cross-validation of Zou, Wang and Li (2020) and the segment-length mBIC of
#' Zhang and Siegmund (2007).
#'
#' @param x A numeric vector, or a \code{ggcpt} object (its series and method
#'   are used).
#' @param method Detection method used to build the candidate ladder.
#'   Defaults to \code{"pelt"}. Taken from \code{x} when it is a
#'   \code{ggcpt}. The penalised methods (\code{"pelt"}, \code{"binseg"},
#'   \code{"segneigh"}, \code{"amoc"}, \code{"fpop"}) give a full nested
#'   ladder; the search-based methods tune themselves by an internal
#'   criterion and largely ignore \code{penalty}, so their ladder collapses
#'   to one or two rungs and the function warns.
#' @param criterion Which criterion selects \eqn{K}:
#'   \describe{
#'     \item{\code{"bic"}}{Gaussian BIC over the ladder,
#'       \eqn{n\log(\mathrm{RSS}/n) + (2K + 1)\log n}. The first term is
#'       the Gaussian profile cost reported in the \code{cost} column
#'       (\eqn{-2\log L} up to an additive constant, for a common
#'       variance); the parameter count is \eqn{K} locations plus
#'       \eqn{K + 1} segment means. Stated because the \code{value}
#'       column is otherwise not reproducible: both the cost convention and
#'       the parameter count vary between authors.}
#'     \item{\code{"mbic"}}{the modified BIC of Zhang and Siegmund (2007),
#'       \eqn{3K\log n + \sum_i \log(l_i/n)} on the \strong{deviance}
#'       (\eqn{-2\log L}) scale, which is the scale the \code{cost} column
#'       is on --- the same criterion is \eqn{1.5K\log n + 0.5\sum_i
#'       \log(l_i/n)} on the log-likelihood scale, which is how
#'       \code{\link{cpt_penalty}()} states it. It depends on the segment
#'       lengths \eqn{l_i} and so cannot be expressed by
#'       \code{cpt_penalty()}'s function of \eqn{n} and \eqn{k} alone.
#'       This is the one place in the package where the real
#'       Zhang–Siegmund penalty is computed.}
#'     \item{\code{"aic"}}{Gaussian AIC over the ladder,
#'       \eqn{n\log(\mathrm{RSS}/n) + 2(2K + 1)} — the same cost and the
#'       same parameter count as \code{"bic"}, with \eqn{2} in place of
#'       \eqn{\log n}. That penalty does not grow with \eqn{n}, so it
#'       over-selects changepoints, often taking every rung offered: on a
#'       300-point series with changes at 100 and 200, \code{"bic"} and
#'       \code{"mbic"} both choose \eqn{K = 2} and \code{"aic"} chooses
#'       the largest \eqn{K} available. Included because people ask for it
#'       and because seeing the curve is instructive; \code{"bic"} or
#'       \code{"mbic"} is the better default.}
#'     \item{\code{"crops_elbow"}}{the knee of the CROPS cost-against-\eqn{K}
#'       curve, made an explicit rule (maximum distance from the chord
#'       joining the endpoints — the standard Kneedle construction) rather
#'       than something eyeballed off a plot.}
#'     \item{\code{"cv"}}{order-preserved sample-splitting cross-validation
#'       (COPPS) via \pkg{crossvalidationCP}. This is the criterion with a
#'       consistency guarantee. Note that \code{cpss}, the authors' own
#'       package, was removed from CRAN; \pkg{crossvalidationCP} is the
#'       supportable route.}
#'     \item{\code{"stability"}}{the \eqn{K} whose changepoints are
#'       re-detected most often under within-segment bootstrap resampling.
#'       A robustness criterion, not a model-selection one; use it to
#'       cross-check the others.}
#'   }
#' @param k_max Largest number of changepoints considered. Defaults to
#'   \code{20}, capped at \code{floor(n / 4)}.
#' @param folds Folds for \code{criterion = "cv"}. Defaults to \code{5};
#'   \code{2} gives the original COPPS split.
#' @param B Bootstrap replicates for \code{criterion = "stability"}.
#'   Defaults to \code{100}.
#' @param change_in Passed to the detector, and inherited from \code{x}
#'   when \code{x} is a \code{ggcpt} (an explicit value still wins).
#'   Defaults to \code{"mean"}.
#'
#'   \strong{The three closed-form criteria assume a change in the mean
#'   whatever this is set to.} \code{"bic"}, \code{"aic"} and
#'   \code{"mbic"} all score the ladder with the Gaussian profile cost
#'   \eqn{n\log(\mathrm{RSS}/n)} -- the deviance for a change in mean with
#'   a common variance -- so on a \code{change_in = "var"} ladder the
#'   \emph{candidates} come from the variance detector while the
#'   \emph{score} does not, and splitting a segment whose mean did not move
#'   barely reduces \eqn{\mathrm{RSS}}: the criterion will tend to choose
#'   \eqn{K = 0} on a real variance change. Use \code{"cv"} or
#'   \code{"stability"} there, both of which score by re-detection with the
#'   same \code{change_in} and so carry no such assumption.
#' @param index Optional time index (a vector of dates, or a \code{ts},
#'   \code{xts}, \code{zoo} or \code{tsibble} passed as \code{x}), carried
#'   onto the chosen fit so \code{tidy()} and \code{autoplot()} report the
#'   changepoint on your scale rather than as a position. Inherited from
#'   \code{x} when \code{x} is an indexed \code{ggcpt}.
#' @param seed Optional seed. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()} when
#'   the ladder is built by repeated detection.
#'
#' @return A \code{ggcpt_selection} object: a list with
#'   \code{criterion_table} (one row per candidate \eqn{K}: \code{k},
#'   \code{value}, \code{cost}, \code{chosen}, and a \code{cpts}
#'   list-column), \code{k} (the chosen number), \code{fit} (the
#'   \code{ggcpt} at that \eqn{K}), \code{criterion} and \code{data}.
#'   Methods: \code{print()}, \code{tidy()} and \code{autoplot()} with
#'   \code{plot_type = "criterion"}, \code{"segmentation"} or
#'   \code{"ladder"}.
#' @references
#' \insertRef{zou2020copps}{ggchangepoint}
#'
#' \insertRef{zhang2007mbic}{ggchangepoint}
#'
#' \insertRef{haynes2017crops}{ggchangepoint}
#' @seealso \code{\link{cpt_crops}()}, \code{\link{cpt_penalty}()},
#'   \code{\link{cpt_sensitivity}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(80), rnorm(80, 4), rnorm(80, 1))
#' sel <- cpt_select(x, criterion = "bic", k_max = 6)
#' sel
#' ggplot2::autoplot(sel)
#' ggplot2::autoplot(sel, plot_type = "ladder")
cpt_select <- function(x, method = "pelt",
                       criterion = c("bic", "mbic", "aic", "crops_elbow",
                                     "cv", "stability"),
                       k_max = 20, folds = 5, B = 100, change_in = "mean",
                       index = NULL, seed = NULL, ...) {
  criterion <- match.arg(criterion)
  if (is_ggcpt(x)) {
    method <- x$method
    # ...and the change type, which was left at this function's own default
    # of "mean" however the fit was made. `@param x` says "its series and
    # method are used" -- honest about the method and silent about the
    # change type -- so a meanvar fit was re-segmented as a change in the
    # mean at every rung of the ladder, and scored with gaussian_cost(),
    # which is a Gaussian-mean cost. `missing()` is what keeps an explicit
    # argument winning over the object.
    if (missing(change_in)) change_in <- x$change_in %||% change_in
    series <- x$data$value
    # A selection made from an indexed fit has to stay on the user's scale:
    # dropping the index here would hand back a result plotted in positions
    # when the input was plotted in dates.
    if (is.null(index)) index <- x$index
    index_label <- x$index_label %||% "Index"
  } else {
    validate_data(x)
    parts <- as_cpt_series(x, index = index)
    series <- as_uni_vector(parts$values, method)
    index <- parts$index
    index_label <- parts$index_label %||% "Index"
  }
  if (!is.null(index) && length(index) != length(series)) {
    stop("`index` must have one entry per observation: ", length(index),
         " supplied for a series of ", length(series), ".", call. = FALSE)
  }
  n <- length(series)
  validate_scalar(k_max, "k_max", min = 0)
  validate_scalar(folds, "folds", min = 2)
  validate_scalar(B, "B", min = 1)
  k_max <- min(as.integer(k_max), max(1L, floor(n / 4)))
  local_seed(seed)

  ladder <- candidate_ladder(series, method, change_in, k_max, ...)
  costs <- vapply(ladder$cpts, function(cp) gaussian_cost(series, cp),
                  numeric(1))
  ks <- ladder$k
  # A method that ignores `penalty` -- most of the search-based ones do,
  # tuning themselves by an internal criterion instead -- returns the same
  # segmentation at every rung, so there is no ladder to score. Say so
  # rather than reporting a "chosen" K off a one- or two-point curve.
  if (length(ks) < 3) {
    warning("`", method, "` produced only ", length(ks),
            " distinct segmentation(s) over the candidate range, so there ",
            "is little for `criterion = \"", criterion,
            "\"` to choose between. Methods with an explicit penalty ",
            "(pelt, binseg, segneigh, amoc, fpop) give a full ladder; the ",
            "search-based methods tune themselves and largely ignore ",
            "`penalty`.", call. = FALSE)
  }

  value <- switch(criterion,
    bic = costs + (2 * ks + 1) * log(n),
    aic = costs + 2 * (2 * ks + 1),
    mbic = costs + vapply(seq_along(ks), function(i) {
      zhang_siegmund_penalty(ladder$cpts[[i]], n)
    }, numeric(1)),
    crops_elbow = costs,
    cv = rep(NA_real_, length(ks)),
    stability = rep(NA_real_, length(ks))
  )

  chosen_k <- switch(criterion,
    bic = ks[which.min(value)],
    aic = ks[which.min(value)],
    mbic = ks[which.min(value)],
    crops_elbow = ks[knee_point(ks, costs)],
    cv = select_by_cv(series, k_max, folds),
    stability = {
      value <- stability_curve(series, ladder, method, B, ...)
      ks[which.max(value)]
    }
  )
  # `cv` and `stability` are the two criteria whose answer comes from a
  # separate search rather than from `value`, and each can come back with
  # nothing usable: stability_curve() is NA at every rung with no
  # changepoints, so an all-NA curve makes which.max() return integer(0),
  # and `if (integer(0) %in% ks)` then failed with base R's "argument is of
  # length zero"; VfoldCV() returning nothing usable gives NA, which sent
  # `0:NA` into candidate_ladder(). Neither message names the criterion or
  # the series.
  if (length(chosen_k) != 1L || is.na(chosen_k)) {
    stop("`criterion = \"", criterion, "\"` could not score the candidate ",
         "segmentations for `", method, "`: it scores by re-detection, and ",
         "every rung on the ladder (K = ", paste(ks, collapse = ", "),
         ") came back undefined. Use `criterion = \"bic\"`, `\"aic\"` or ",
         "`\"mbic\"`, which are closed-form, or a method with an explicit ",
         "penalty (pelt, binseg, segneigh, amoc, fpop).", call. = FALSE)
  }
  if (!chosen_k %in% ks) {
    # Cross-validation searches its own ladder, so its answer can exceed the
    # candidates we scored; extend rather than silently snapping to k_max.
    extra <- candidate_ladder(series, method, change_in, chosen_k, ...)
    keep <- extra$k == chosen_k
    ladder <- list(k = c(ks, chosen_k),
                   cpts = c(ladder$cpts, extra$cpts[keep]))
    ks <- ladder$k
    costs <- c(costs, gaussian_cost(series, ladder$cpts[[length(ks)]]))
    value <- c(value, NA_real_)
  }

  chosen_i <- match(chosen_k, ks)
  tab <- tibble::tibble(
    k = ks,
    value = value,
    cost = costs,
    chosen = seq_along(ks) == chosen_i,
    cpts = ladder$cpts
  )

  fit <- as_ggcpt(ladder$cpts[[chosen_i]], series, method = method,
                  change_in = change_in,
                  penalty = list(type = paste0("selected by ", criterion),
                                 value = NA_real_))
  fit <- attach_index(fit, index, index_label)

  structure(
    list(criterion_table = tab, k = chosen_k, fit = fit,
         criterion = criterion, method = method, data = series,
         index = index, index_label = index_label),
    class = "ggcpt_selection"
  )
}

# Internal: candidate segmentations with 0, 1, ..., k_max changepoints.
#
# Binary segmentation gives an exactly-K segmentation for every K in one
# nested family, which is what a ladder needs; the penalised searches give
# whatever K their penalty implies. `changepoint::cpt.mean(method = "BinSeg",
# Q = k)` is used where it applies, and repeated detection under a
# decreasing penalty everywhere else.
#' @noRd
candidate_ladder <- function(series, method, change_in, k_max, ...) {
  n <- length(series)
  if (method %in% c("pelt", "binseg", "segneigh", "amoc", "fpop") &&
      change_in %in% c("mean", "var", "meanvar")) {
    cpts <- lapply(0:k_max, function(k) {
      if (k == 0) return(integer(0))
      fun <- switch(change_in, mean = changepoint::cpt.mean,
                    var = changepoint::cpt.var,
                    meanvar = changepoint::cpt.meanvar)
      fit <- tryCatch(
        fun(series, method = "BinSeg", Q = k, penalty = "None"),
        error = function(e) NULL
      )
      if (is.null(fit)) return(NA_integer_)
      cp <- as.integer(changepoint::cpts(fit))
      cp[cp >= 1 & cp < n]
    })
  } else {
    # A generic ladder: sweep the penalty and keep the first segmentation
    # seen at each K. Coarse, but it works for every method the dispatcher
    # knows, including registered ones.
    pens <- exp(seq(log(0.05 * log(n)), log(60 * log(n)), length.out = 40))
    found <- vector("list", k_max + 1L)
    found[[1]] <- integer(0)
    for (p in pens) {
      cp <- tryCatch(
        cpt_detect(series, method = method, change_in = change_in,
                   penalty = p, ...)$changepoints$cp,
        error = function(e) NULL
      )
      if (is.null(cp)) next
      k <- length(cp)
      if (k >= 1 && k <= k_max && is.null(found[[k + 1L]])) {
        found[[k + 1L]] <- cp
      }
    }
    cpts <- lapply(seq_along(found), function(i) {
      if (is.null(found[[i]])) NA_integer_ else found[[i]]
    })
  }
  keep <- !vapply(cpts, function(v) length(v) == 1L && is.na(v[1]),
                  logical(1))
  list(k = (0:k_max)[keep], cpts = cpts[keep])
}

# Internal: -2 log-likelihood of a Gaussian change-in-mean model with a
# common unknown variance, up to a constant. This is the cost every
# information criterion here is built on, so they are all on one scale.
#' @noRd
gaussian_cost <- function(y, cp) {
  n <- length(y)
  cp <- sort(unique(as.integer(cp)))
  cp <- cp[cp >= 1 & cp < n]
  bounds <- c(0L, cp, n)
  rss <- 0
  for (i in seq_len(length(bounds) - 1)) {
    seg <- y[seq.int(bounds[i] + 1L, bounds[i + 1L])]
    rss <- rss + sum((seg - mean(seg))^2)
  }
  if (rss <= 0) rss <- .Machine$double.eps
  n * log(rss / n)
}

# Internal: Zhang & Siegmund's (2007) modified BIC penalty, on the DEVIANCE
# (-2 log L) scale to match gaussian_cost(). ?cpt_penalty states the same
# criterion on the log-likelihood scale, i.e. half of this -- both pages now
# name their scale, because a reader comparing them without that would
# conclude one of the two was a factor-of-two bug. Unlike the `"MBIC"` of
# cpt_penalty(), this one reads the segment lengths, which is exactly what
# makes it unavailable as a function of n and k.
#' @noRd
zhang_siegmund_penalty <- function(cp, n) {
  cp <- sort(unique(as.integer(cp)))
  cp <- cp[cp >= 1 & cp < n]
  k <- length(cp)
  if (k == 0) return(0)
  lens <- diff(c(0L, cp, n))
  3 * k * log(n) + sum(log(lens / n))
}

# Internal: the knee of a decreasing cost curve -- the point furthest from
# the chord joining the two endpoints (Satopaa et al.'s Kneedle rule). Both
# axes are scaled to [0, 1] first, so the answer does not depend on the units
# of the cost.
#' @noRd
knee_point <- function(k, cost) {
  if (length(k) < 3) return(length(k))
  kx <- (k - min(k)) / max(diff(range(k)), 1)
  cy <- (cost - min(cost)) / max(diff(range(cost)), .Machine$double.eps)
  x1 <- kx[1]; y1 <- cy[1]
  x2 <- kx[length(kx)]; y2 <- cy[length(cy)]
  denom <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
  if (denom == 0) return(1L)
  d <- abs((y2 - y1) * kx - (x2 - x1) * cy + x2 * y1 - y2 * x1) / denom
  which.max(d)
}

# Internal: COPPS / V-fold cross-validation for the number of changepoints.
#' @noRd
select_by_cv <- function(series, k_max, folds) {
  need_pkg("crossvalidationCP")
  out <- tryCatch(
    crossvalidationCP::VfoldCV(series, V = as.integer(folds),
                               Kmax = as.integer(max(1L, k_max)),
                               output = "param"),
    error = function(e) NULL
  )
  if (is.null(out)) {
    stop("crossvalidationCP could not select a number of changepoints for ",
         "this series. Try a different `criterion`, or a larger `k_max`.",
         call. = FALSE)
  }
  as.integer(out)[1]
}

# Internal: mean re-detection frequency of each candidate segmentation under
# within-segment residual resampling.
#' @noRd
stability_curve <- function(series, ladder, method, B, ...) {
  n <- length(series)
  vapply(seq_along(ladder$k), function(i) {
    cp <- ladder$cpts[[i]]
    if (length(cp) == 0) return(NA_real_)
    fitted_step <- rep_segment_means(series, cp)
    resid <- series - fitted_step
    seg_id <- rep(seq_len(length(cp) + 1L), diff(c(0L, cp, n)))
    hits <- numeric(length(cp))
    for (b in seq_len(B)) {
      resampled <- resid
      for (s in unique(seg_id)) {
        idx <- which(seg_id == s)
        # `sample.int()` on the index, not `sample()` on the values: R's
      # classic pitfall is that `sample(x, n)` means `sample.int(x, n)` when
      # `x` is a single number >= 1, so a one-observation segment resamples
      # `1:round(resid)` instead of the residual itself. It is currently
      # safe only by accident -- a length-1 segment's residual against its
      # own mean is exactly 0, and `0 >= 1` is FALSE -- which couples this
      # bootstrap to `param_estimate` staying the exact segment mean.
      resampled[idx] <- resid[idx][sample.int(length(idx), length(idx),
                                              replace = TRUE)]
      }
      rep_cp <- tryCatch(
        cpt_detect(fitted_step + resampled, method = method,
                   ...)$changepoints$cp,
        error = function(e) integer(0)
      )
      if (length(rep_cp) == 0) next
      hits <- hits + vapply(cp, function(k) {
        as.numeric(min(abs(rep_cp - k)) <= 5)
      }, numeric(1))
    }
    mean(hits / B)
  }, numeric(1))
}

#' @noRd
rep_segment_means <- function(y, cp) {
  n <- length(y)
  bounds <- c(0L, sort(cp), n)
  out <- numeric(n)
  for (i in seq_len(length(bounds) - 1)) {
    idx <- seq.int(bounds[i] + 1L, bounds[i + 1L])
    out[idx] <- mean(y[idx])
  }
  out
}

#' @rdname cpt_select
#' @param object A \code{ggcpt_selection} object (for \code{autoplot()}).
#' @export
print.ggcpt_selection <- function(x, ...) {
  cat("ggcpt_selection (criterion: ", x$criterion, ", method: ", x$method,
      ")\n", sep = "")
  cat("  Candidates scored: K = ", min(x$criterion_table$k), " to ",
      max(x$criterion_table$k), "\n", sep = "")
  cat("  Chosen K:          ", x$k, "\n", sep = "")
  cat("  Locations:         ",
      if (x$k == 0) "none" else paste(x$fit$changepoints$cp, collapse = ", "),
      "\n", sep = "")
  if (x$criterion == "crops_elbow") {
    cat("\nNote: the elbow is the point of maximum distance from the chord\n",
        "      joining the endpoints of the cost curve. It is a heuristic,\n",
        "      not a consistent selector; `criterion = \"cv\"` has a proof.\n",
        sep = "")
  }
  cat("\n")
  print(x$criterion_table[, c("k", "value", "cost", "chosen")], n = 12)
  invisible(x)
}

#' @rdname cpt_select
#' @export
tidy.ggcpt_selection <- function(x, ...) {
  x$criterion_table[, c("k", "value", "cost", "chosen")]
}

#' @rdname cpt_select
#' @param plot_type \code{"criterion"} (the criterion against \eqn{K}, with
#'   the choice marked), \code{"segmentation"} (the series with the chosen
#'   segmentation) or \code{"ladder"} (small multiples showing how the
#'   segmentation coarsens as \eqn{K} falls — the display that makes the
#'   choice inspectable rather than asserted).
#' @param max_facets Maximum number of rungs drawn by
#'   \code{plot_type = "ladder"}. Defaults to \code{12}.
#' @export
autoplot.ggcpt_selection <- function(object,
                                     plot_type = c("criterion",
                                                   "segmentation", "ladder"),
                                     max_facets = 12, ...) {
  plot_type <- match.arg(plot_type)
  tab <- object$criterion_table

  if (plot_type == "criterion") {
    d <- tab[is.finite(tab$value), , drop = FALSE]
    if (nrow(d) == 0) {
      d <- tab
      d$value <- d$cost
    }
    return(
      ggplot2::ggplot(d, ggplot2::aes(k, value)) +
        ggplot2::geom_line(colour = "grey55") +
        ggplot2::geom_point(ggplot2::aes(colour = chosen, size = chosen)) +
        ggplot2::scale_colour_manual(values = c(`FALSE` = "grey30",
                                                `TRUE` = "#D55E00"),
                                     guide = "none") +
        ggplot2::scale_size_manual(values = c(`FALSE` = 1.6, `TRUE` = 3.4),
                                   guide = "none") +
        ggplot2::labs(x = "Number of changepoints (K)",
                      y = paste0(object$criterion, " criterion"),
                      title = paste0("Selecting K by ", object$criterion),
                      subtitle = paste0("Chosen K = ", object$k))
    )
  }

  if (plot_type == "segmentation") {
    return(autoplot.ggcpt(object$fit, show_segments = TRUE) +
             ggplot2::labs(title = paste0("Chosen segmentation (K = ",
                                          object$k, ", ", object$criterion,
                                          ")")))
  }

  # plot_type == "ladder"
  rungs <- utils::head(tab[order(-tab$k), , drop = FALSE], max_facets)
  rungs <- rungs[order(rungs$k), , drop = FALSE]
  series <- tibble::tibble(index = seq_along(object$data),
                           value = object$data)
  panels <- do.call(rbind, lapply(seq_len(nrow(rungs)), function(i) {
    d <- series
    d$panel <- paste0("K = ", rungs$k[i], if (rungs$chosen[i]) "  *" else "")
    d$.order <- rungs$k[i]
    d
  }))
  panels$panel <- stats::reorder(panels$panel, panels$.order)
  cp_panels <- do.call(rbind, lapply(seq_len(nrow(rungs)), function(i) {
    cp <- rungs$cpts[[i]]
    if (length(cp) == 0) return(NULL)
    data.frame(panel = paste0("K = ", rungs$k[i],
                              if (rungs$chosen[i]) "  *" else ""),
               cp = cp)
  }))

  p <- ggplot2::ggplot(panels, ggplot2::aes(index, value)) +
    ggplot2::geom_line(colour = "grey55") +
    ggplot2::facet_wrap(~panel, ncol = 1, strip.position = "right") +
    ggplot2::labs(x = "Index", y = "Value",
                  title = "Segmentation ladder",
                  subtitle = "* marks the chosen K")
  if (!is.null(cp_panels)) {
    cp_panels$panel <- factor(cp_panels$panel, levels = levels(panels$panel))
    p <- p + ggplot2::geom_vline(data = cp_panels,
                                 ggplot2::aes(xintercept = cp),
                                 colour = "#0072B2", linewidth = 0.4)
  }
  p
}
