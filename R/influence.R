# ---------------------------------------------------------------------------
# Theme C: influence, leverage and sensitivity.
#
# cpt_stability() answers "would I find this again?" by resampling.
# cpt_influence() answers the sharper question -- "which observation is
# driving this?" -- by perturbing one observation at a time and re-fitting,
# following Wilms, Killick & Matteson (JCGS 2022). cpt_sensitivity() is the
# tuning-parameter analogue: not which point, but which penalty.
# ---------------------------------------------------------------------------

#' Influence diagnostics for a changepoint segmentation
#'
#' Perturbs one observation at a time — deleting it, or replacing it with an
#' outlier — re-runs the detector, and reports what changed: the number of
#' changepoints, where they moved to, and how the segment parameters
#' responded. This is the diagnostic family of Wilms, Killick and Matteson
#' (2022), rendered in \pkg{ggplot2} so it composes with the rest of the
#' package.
#'
#' @param object A \code{ggcpt} object.
#' @param type \code{"delete"} (drop the observation) or \code{"outlier"}
#'   (replace it with a large value). Defaults to \code{"delete"}.
#' @param engine Which implementation to use:
#'   \code{"auto"} (default) uses \pkg{changepoint.influence} when the result
#'   came from a \pkg{changepoint} engine and that package is installed, and
#'   the generic recomputation otherwise; \code{"changepoint.influence"}
#'   insists on the former; \code{"recompute"} insists on the latter, which
#'   works for every wired and registered method.
#' @param subset Optional integer vector of observation positions to perturb.
#'   Influence by deletion costs one detector fit per observation, so on a
#'   long series or an expensive engine this is the argument that makes the
#'   diagnostic affordable. Defaults to every observation.
#' @param outlier_sd For \code{type = "outlier"}, how many residual standard
#'   deviations the substituted value sits above the fitted level. Defaults
#'   to \code{5}.
#' @param seed Optional seed, for detectors that randomise. The seed is
#'   scoped to this call: \code{.Random.seed} is saved and restored, so a
#'   seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()} on
#'   each perturbed series.
#'
#' @return A \code{ggcpt_influence} object: a list with
#'   \describe{
#'     \item{\code{influence}}{a tibble with one row per perturbed
#'       observation — \code{index}, \code{n_cp}, \code{delta_n_cp} (against
#'       the unperturbed fit), \code{max_shift} (largest movement of a
#'       surviving changepoint, in positions), \code{param_shift} (largest
#'       absolute change in a segment parameter) and \code{cpts} (a
#'       list-column of the perturbed changepoint sets);}
#'     \item{\code{param}}{an \eqn{n \times n} matrix of per-observation
#'       segment parameters, one row per perturbation — the input to the
#'       influence map;}
#'     \item{\code{original}, \code{type}, \code{engine}, \code{method}}{}
#'   }
#'   with \code{print()} and \code{autoplot()} methods.
#' @references
#' \insertRef{wilms2022influence}{ggchangepoint}
#' @seealso \code{\link{cpt_leverage}()}, \code{\link{cpt_sensitivity}()},
#'   \code{\link{cpt_stability}()}.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
#' inf <- cpt_influence(fit)
#' inf
#' head(cpt_leverage(inf))
cpt_influence <- function(object, type = c("delete", "outlier"),
                          engine = c("auto", "changepoint.influence",
                                     "recompute"),
                          subset = NULL, outlier_sd = 5, seed = NULL, ...) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  type <- match.arg(type)
  engine <- match.arg(engine)
  validate_scalar(outlier_sd, "outlier_sd", min = 0)

  y <- object$data$value
  n <- length(y)
  if (n < 4) {
    stop("Influence diagnostics need at least 4 observations; the series ",
         "has ", n, ".", call. = FALSE)
  }
  if (is.null(subset)) {
    subset <- seq_len(n)
  } else {
    subset <- sort(unique(as.integer(subset)))
    if (any(subset < 1 | subset > n)) {
      stop("`subset` must index observations of the series (1..", n, ").",
           call. = FALSE)
    }
  }

  can_native <- inherits(object$fit, "cpt") &&
    requireNamespace("changepoint.influence", quietly = TRUE)
  if (engine == "changepoint.influence" && !can_native) {
    stop("`engine = \"changepoint.influence\"` needs a result from a ",
         "changepoint-package engine (pelt, binseg, segneigh, amoc) and the ",
         "changepoint.influence package installed.", call. = FALSE)
  }
  use_native <- (engine == "auto" && can_native && identical(subset, seq_len(n))) ||
    engine == "changepoint.influence"

  if (!use_native && length(subset) > 500) {
    warning("Recomputing influence perturbs one observation at a time, so ",
            "this is ", length(subset), " detector fits. Pass `subset` to ",
            "sample positions, or set a parallel `future::plan()`.",
            call. = FALSE)
  }

  local_seed(seed)

  res <- if (use_native) {
    influence_native(object, type)
  } else {
    influence_recompute(object, type, subset, outlier_sd, ...)
  }

  structure(
    list(
      influence = res$influence,
      param = res$param,
      original = object,
      type = type,
      engine = if (use_native) "changepoint.influence" else "recompute",
      method = object$method
    ),
    class = "ggcpt_influence"
  )
}

# Internal: the changepoint.influence route. That package computes the whole
# n x n perturbation grid in one pass, which is far cheaper than n calls to
# cpt_detect(), so it is preferred whenever it applies.
#' @noRd
influence_native <- function(object, type) {
  inf <- changepoint.influence::influence(object$fit)
  part <- if (type == "delete") inf$delete else inf$outlier
  class_mat <- if (type == "delete") part$class.del else part$class.out
  param_mat <- if (type == "delete") part$param.del else part$param.out

  n <- nrow(class_mat)
  orig_cp <- object$changepoints$cp
  orig_param <- rep(object$segments$param_estimate, times = object$segments$n)

  cpts <- lapply(seq_len(n), function(i) {
    lab <- class_mat[i, ]
    keep <- which(!is.na(lab))
    v <- lab[keep]
    if (length(v) < 2) return(integer(0))
    as.integer(keep[which(diff(v) != 0)])
  })
  summarise_influence(cpts, param_mat, orig_cp, orig_param, seq_len(n))
}

# Internal: the generic route -- re-run whatever detector produced the
# result, once per perturbed observation. Works for every method, including
# registered ones.
#' @noRd
influence_recompute <- function(object, type, subset, outlier_sd, ...) {
  y <- object$data$value
  n <- length(y)
  method <- object$method
  # Inherit the change type from the result, not just the method.
  #
  # Every entry point that takes raw data forwards `change_in` to
  # cpt_detect() explicitly; every one that takes a finished `ggcpt` read
  # `object$method` and stopped there, leaving `change_in` at its own
  # default of "mean" -- so a meanvar or var fit was silently re-detected as
  # a change in the MEAN. `object$change_in` was on the object the whole
  # time and never read.
  #
  # The symptom pointed away from the cause. On a pure variance change the
  # mean detector finds nothing, every replicate is discarded, and the
  # caller gets a zero-width interval plus a warning blaming the detector
  # for finding no changepoints.
  #
  # `...` still wins, so an explicit `change_in` overrides the object --
  # same precedence cpt_detect() gives `dots` over `derived_args_for()`.
  dots <- list(...)
  if (is.null(dots$change_in)) dots$change_in <- object$change_in %||% "mean"
  orig_cp <- object$changepoints$cp
  fitted_step <- rep(object$segments$param_estimate, times = object$segments$n)
  orig_param <- fitted_step
  resid_sd <- stats::sd(y - fitted_step)
  if (!is.finite(resid_sd) || resid_sd == 0) resid_sd <- stats::sd(y)
  if (!is.finite(resid_sd) || resid_sd == 0) resid_sd <- 1

  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  run_one <- function(i) {
    if (type == "delete") {
      pert <- y[-i]
    } else {
      pert <- y
      pert[i] <- fitted_step[i] + outlier_sd * resid_sd
    }
    fit <- tryCatch(do.call(cpt_detect,
                            c(list(pert, method = method), dots)),
                    error = function(e) NULL)
    if (is.null(fit)) return(list(cp = NA_integer_, param = rep(NA_real_, n)))
    cp <- fit$changepoints$cp
    par_i <- rep(fit$segments$param_estimate, times = fit$segments$n)
    if (type == "delete") {
      # Re-insert the deleted position so every row is length n and the
      # columns keep meaning "position in the original series". Positions
      # after the deletion shift by one, which is also why the changepoint
      # locations are shifted back below.
      # Not ifelse(): on an empty `cp` -- which is what a perturbed fit that
      # found nothing returns -- `ifelse(logical(0), ...)` gives
      # logical(0), so the list of segmentations mixed integer and logical
      # vectors. unlist() absorbed it downstream, but it is a type-stability
      # hole in the hot loop of a diagnostic.
      cp[cp >= i] <- cp[cp >= i] + 1L
      par_i <- append(par_i, NA_real_, after = i - 1L)[seq_len(n)]
    }
    list(cp = cp, param = par_i)
  }

  outs <- if (has_future) {
    future.apply::future_lapply(subset, with_session_registry(run_one),
                                # `seed %||% TRUE`, as the other three
                                # parallel call sites do. local_seed(seed)
                                # above means future.apply derives its
                                # streams from the seeded state, so results
                                # are reproducible either way today -- but
                                # a hard-coded TRUE makes that a property
                                # of two lines being in the right order,
                                # with no test that would notice if either
                                # moved.
                                future.seed = seed %||% TRUE)
  } else {
    lapply(subset, run_one)
  }

  cpts <- lapply(outs, function(o) o$cp)
  param_mat <- do.call(rbind, lapply(outs, function(o) o$param))
  summarise_influence(cpts, param_mat, orig_cp, orig_param, subset)
}

# Internal: turn per-perturbation changepoint sets and parameter rows into
# the one-row-per-observation summary both the print method and the plots
# read.
#' @noRd
summarise_influence <- function(cpts, param_mat, orig_cp, orig_param, index) {
  n_cp <- vapply(cpts, function(v) sum(!is.na(v)), integer(1))
  max_shift <- vapply(cpts, function(v) {
    v <- v[!is.na(v)]
    if (length(orig_cp) == 0 || length(v) == 0) return(NA_real_)
    # How far each original changepoint had to move to find a match.
    max(vapply(orig_cp, function(k) min(abs(v - k)), numeric(1)))
  }, numeric(1))
  param_shift <- vapply(seq_len(nrow(param_mat)), function(i) {
    d <- abs(param_mat[i, ] - orig_param)
    if (all(is.na(d))) return(NA_real_)
    max(d, na.rm = TRUE)
  }, numeric(1))

  influence <- tibble::tibble(
    index = as.integer(index),
    n_cp = n_cp,
    delta_n_cp = n_cp - length(orig_cp),
    max_shift = max_shift,
    param_shift = param_shift,
    cpts = cpts
  )
  list(influence = influence, param = param_mat)
}

#' @rdname cpt_influence
#' @param x A \code{ggcpt_influence} object (for \code{print()}).
#' @export
print.ggcpt_influence <- function(x, ...) {
  inf <- x$influence
  cat("ggcpt_influence (", x$type, " perturbation, method: ", x$method,
      ", engine: ", x$engine, ")\n", sep = "")
  cat("  Observations perturbed: ", nrow(inf), "\n", sep = "")
  cat("  Unperturbed changepoints: ",
      nrow(x$original$changepoints), "\n", sep = "")
  changed <- sum(inf$delta_n_cp != 0, na.rm = TRUE)
  cat("  Perturbations changing the number of changepoints: ", changed,
      " (", format(100 * changed / nrow(inf), digits = 3), "%)\n", sep = "")
  top <- utils::head(cpt_leverage(x), 5)
  if (nrow(top) > 0) {
    cat("\nMost influential observations:\n")
    print(top)
  }
  invisible(x)
}

#' Rank observations by influence
#'
#' Orders the observations of a \code{\link{cpt_influence}()} result by how
#' much perturbing them disturbs the segmentation, most influential first.
#' The composite \code{leverage} score is the sum of three standardised
#' components — the change in the number of changepoints, the largest
#' movement of a changepoint, and the largest change in a segment parameter
#' — so an observation that shifts a location without changing the count is
#' still ranked.
#'
#' @param object A \code{ggcpt_influence} object, or a \code{ggcpt} object
#'   (in which case \code{\link{cpt_influence}()} is run first).
#' @param ... Passed to \code{\link{cpt_influence}()} when \code{object} is a
#'   \code{ggcpt}.
#' @return A tibble ordered most influential first, with columns
#'   \code{index}, \code{delta_n_cp}, \code{max_shift},
#'   \code{param_shift} and \code{leverage}.
#'
#'   \strong{Rows with \code{leverage = NA} come first, and they are the
#'   most influential of all.} \code{max_shift} and \code{param_shift}
#'   are undefined for a perturbation that left the engine with no
#'   changepoints at all -- there is nothing to match against and no
#'   parameters to compare -- so the composite score cannot be formed for
#'   an observation whose removal destroys the segmentation entirely. The
#'   \code{NA} is kept rather than filled in with a fabricated number;
#'   read the \code{delta_n_cp} column on those rows, which says how many
#'   changepoints were lost.
#'
#'   An \code{NA} here is always that case. If the \emph{original} fit
#'   found no changepoints then \code{max_shift} is missing for every
#'   observation, the standardisation returns zeros rather than
#'   \code{NA}s, and every \code{leverage} is finite.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
#' head(cpt_leverage(fit), 3)
cpt_leverage <- function(object, ...) {
  if (is_ggcpt(object)) object <- cpt_influence(object, ...)
  if (!inherits(object, "ggcpt_influence")) {
    stop("`object` must be a ggcpt_influence or ggcpt object.", call. = FALSE)
  }
  inf <- object$influence
  z <- function(v) {
    v <- abs(as.numeric(v))
    s <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(0, length(v)))
    (v - mean(v, na.rm = TRUE)) / s
  }
  lev <- z(inf$delta_n_cp) + z(inf$max_shift) + z(inf$param_shift)
  out <- tibble::tibble(
    index = inf$index,
    delta_n_cp = inf$delta_n_cp,
    max_shift = inf$max_shift,
    param_shift = inf$param_shift,
    leverage = lev
  )
  # A row with no `leverage` is the most influential observation there is,
  # not the least.
  #
  # `max_shift` is the distance each original changepoint had to move to
  # find a match, and `param_shift` the largest change in a segment
  # parameter. Both are NA for a perturbation that left the engine with
  # *no* changepoints at all -- there is nothing to match against and no
  # parameters to compare -- so `NA + z + z` is NA, and `order(-leverage)`
  # sent that row to the bottom of a table whose entire purpose is "which
  # observations matter most". Measured: deleting one observation destroyed
  # a two-changepoint segmentation and the row ranked 3 of 3.
  #
  # The NA is honest and is kept -- the two components genuinely are
  # undefined, and inventing a number for them would be worse. What was
  # wrong is where it sorted. The visible `delta_n_cp` on such a row says
  # what happened.
  #
  # This is per-row. When the *original* fit found no changepoints,
  # `max_shift` is NA for every row, `z()`'s zero-variance guard returns
  # zeros, and no leverage is NA -- so an NA here always means this
  # particular perturbation collapsed the fit.
  collapsed <- is.na(out$leverage)
  out[order(!collapsed, -out$leverage, out$index), , drop = FALSE]
}

#' @rdname cpt_influence
#' @param object A \code{ggcpt_influence} object (for \code{autoplot()}).
#' @param plot_type Which diagnostic to draw:
#'   \code{"overview"} (the series with the influential observations
#'   highlighted), \code{"location"} (perturbed changepoint locations against
#'   the perturbed observation), \code{"parameter"} (segment-parameter shift
#'   per perturbation) or \code{"map"} (the full influence map: perturbed
#'   observation on x, position on y, parameter shift as fill).
#' @export
autoplot.ggcpt_influence <- function(object,
                                     plot_type = c("overview", "location",
                                                   "parameter", "map"),
                                     ...) {
  plot_type <- match.arg(plot_type)
  inf <- object$influence
  orig <- object$original
  orig_cp <- orig$changepoints$cp
  idx_vals <- plot_index(orig)
  x_lab <- plot_index_label(orig)

  if (plot_type == "overview") {
    lev <- cpt_leverage(object)
    d <- tibble::tibble(
      x = idx_vals[inf$index],
      value = orig$data$value[inf$index],
      leverage = lev$leverage[match(inf$index, lev$index)]
    )
    p <- ggplot2::ggplot(d, ggplot2::aes(x, value)) +
      ggplot2::geom_line(colour = "grey60") +
      ggplot2::geom_point(ggplot2::aes(colour = leverage, size = leverage)) +
      ggplot2::scale_colour_viridis_c(option = "C", direction = -1) +
      ggplot2::scale_size_continuous(range = c(0.4, 2.6), guide = "none") +
      ggplot2::labs(x = x_lab, y = "Value", colour = "Leverage",
                    title = paste0("Influence overview (", object$type,
                                   " perturbation)"),
                    subtitle = "Point size and colour show how much
                                perturbing that observation disturbs the
                                segmentation")
    if (length(orig_cp) > 0) {
      p <- p + ggplot2::geom_vline(xintercept = idx_vals[orig_cp],
                                   colour = "blue", linetype = "dashed",
                                   linewidth = 0.4)
    }
    return(p)
  }

  if (plot_type == "location") {
    rows <- do.call(rbind, lapply(seq_len(nrow(inf)), function(i) {
      v <- inf$cpts[[i]]
      v <- v[!is.na(v)]
      if (length(v) == 0) return(NULL)
      tibble::tibble(perturbed = inf$index[i], cp = v)
    }))
    if (is.null(rows)) {
      stop("No perturbation produced a changepoint, so there is nothing to ",
           "draw. Try plot_type = \"parameter\".", call. = FALSE)
    }
    rows$perturbed_x <- idx_vals[rows$perturbed]
    rows$cp_x <- idx_vals[rows$cp]
    p <- ggplot2::ggplot(rows, ggplot2::aes(perturbed_x, cp_x)) +
      ggplot2::geom_point(size = 0.7, alpha = 0.6, colour = "#0072B2") +
      ggplot2::labs(x = paste0("Perturbed observation (", x_lab, ")"),
                    y = paste0("Detected changepoint (", x_lab, ")"),
                    title = "Location stability")
    if (length(orig_cp) > 0) {
      p <- p + ggplot2::geom_hline(yintercept = idx_vals[orig_cp],
                                   colour = "grey40", linetype = "dashed")
    }
    return(p)
  }

  if (plot_type == "parameter") {
    d <- tibble::tibble(x = idx_vals[inf$index],
                        param_shift = inf$param_shift)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, param_shift)) +
      ggplot2::geom_col(fill = "#0072B2", width = 1) +
      ggplot2::labs(x = paste0("Perturbed observation (", x_lab, ")"),
                    y = "Largest segment-parameter shift",
                    title = "Parameter stability")
    if (length(orig_cp) > 0) {
      p <- p + ggplot2::geom_vline(xintercept = idx_vals[orig_cp],
                                   colour = "red", linetype = "dashed",
                                   linewidth = 0.4)
    }
    return(p)
  }

  # plot_type == "map"
  pm <- object$param
  orig_param <- rep(orig$segments$param_estimate, times = orig$segments$n)
  diff_mat <- sweep(pm, 2, orig_param, "-")
  d <- expand.grid(row = seq_len(nrow(diff_mat)),
                   col = seq_len(ncol(diff_mat)))
  d$perturbed <- idx_vals[inf$index[d$row]]
  d$position <- idx_vals[d$col]
  d$shift <- as.numeric(diff_mat[cbind(d$row, d$col)])
  ggplot2::ggplot(d, ggplot2::aes(perturbed, position, fill = shift)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(low = "#0C4479", mid = "white",
                                  high = "#AB9783", midpoint = 0,
                                  na.value = "grey90") +
    ggplot2::labs(x = paste0("Perturbed observation (", x_lab, ")"),
                  y = paste0("Position (", x_lab, ")"),
                  fill = "Parameter shift",
                  title = paste0("Influence map (", object$type,
                                 " perturbation)"))
}

#' Sensitivity of a segmentation to its tuning parameters
#'
#' The parameter analogue of \code{\link{cpt_influence}()}: instead of asking
#' which observation drives the answer, it asks which \emph{setting} does.
#' Runs the detector over a grid of tuning values and reports the detected
#' locations for each, which is the direct answer to the commonest reviewer
#' question about a changepoint analysis — "is this robust to the penalty?".
#'
#' @param x A numeric vector (the series), or a \code{ggcpt} object, in which
#'   case its series and method are used.
#' @param method Detection method. Taken from \code{x} when it is a
#'   \code{ggcpt}.
#' @param over A named list of parameter vectors to sweep. Every combination
#'   is run, so keep the grid small:
#'   \code{list(penalty = c(5, 10, 20), minseglen = c(2, 10))}.
#' @param seed Optional seed. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments held fixed across the grid and passed to
#'   \code{\link{cpt_detect}()}.
#'
#' @return A \code{ggcpt_sensitivity} object: a list with a \code{grid}
#'   tibble (one row per setting: the parameter columns, \code{n_cp}, and a
#'   \code{cpts} list-column), the \code{data}, and the swept parameter
#'   names. Methods: \code{print()}, \code{tidy()} (one row per detected
#'   changepoint) and \code{autoplot()} (a location heatmap over the grid).
#' @seealso \code{\link{cpt_influence}()}, \code{\link{cpt_select}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(60), rnorm(60, 3))
#' s <- cpt_sensitivity(x, method = "pelt",
#'                      over = list(penalty = c(2, 10, 40)))
#' s
#' ggplot2::autoplot(s)
cpt_sensitivity <- function(x, method = "pelt", over = list(), seed = NULL,
                            ...) {
  # Same omission as cpt_select() and the two recompute paths: the method
  # was inherited from the fit and the change type was not, so a var or
  # meanvar fit had its penalty sensitivity measured on a change-in-mean
  # re-detection. `...` still wins.
  dots_ci <- list(...)
  if (is_ggcpt(x)) {
    method <- x$method
    if (is.null(dots_ci$change_in)) {
      dots_ci$change_in <- x$change_in %||% "mean"
    }
    series <- x$data$value
  } else {
    validate_data(x)
    series <- as_uni_vector(x, method)
  }
  if (!is.list(over) || length(over) == 0 || is.null(names(over)) ||
      any(!nzchar(names(over)))) {
    stop("`over` must be a non-empty named list of parameter vectors, e.g. ",
         "list(penalty = c(5, 10, 20)).", call. = FALSE)
  }
  grid <- expand.grid(over, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  local_seed(seed)

  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  run_one <- function(i) {
    args <- c(list(x = series, method = method),
              as.list(grid[i, , drop = FALSE]), dots_ci)
    fit <- tryCatch(do.call(cpt_detect, args),
                    error = function(e) structure(conditionMessage(e),
                                                  class = "cpt_error"))
    if (inherits(fit, "cpt_error")) {
      return(list(cpts = integer(0), error = as.character(fit)))
    }
    list(cpts = fit$changepoints$cp, error = NA_character_)
  }

  outs <- if (has_future) {
    future.apply::future_lapply(seq_len(nrow(grid)),
                                with_session_registry(run_one),
                                # `seed %||% TRUE`, as the other three
                                # parallel call sites do. local_seed(seed)
                                # above means future.apply derives its
                                # streams from the seeded state, so results
                                # are reproducible either way today -- but
                                # a hard-coded TRUE makes that a property
                                # of two lines being in the right order,
                                # with no test that would notice if either
                                # moved.
                                future.seed = seed %||% TRUE)
  } else {
    lapply(seq_len(nrow(grid)), run_one)
  }

  grid <- tibble::as_tibble(grid)
  grid$n_cp <- vapply(outs, function(o) length(o$cpts), integer(1))
  grid$cpts <- lapply(outs, function(o) o$cpts)
  grid$error <- vapply(outs, function(o) o$error, character(1))
  if (any(!is.na(grid$error))) {
    warning(sum(!is.na(grid$error)), " of ", nrow(grid),
            " settings errored; their rows report 0 changepoints and carry ",
            "the message in the `error` column.", call. = FALSE)
  }

  structure(
    list(grid = grid, data = series, method = method,
         params = names(over)),
    class = "ggcpt_sensitivity"
  )
}

#' @rdname cpt_sensitivity
#' @export
print.ggcpt_sensitivity <- function(x, ...) {
  cat("ggcpt_sensitivity (method: ", x$method, ", ", nrow(x$grid),
      " settings)\n", sep = "")
  cat("  Swept: ", paste(x$params, collapse = ", "), "\n", sep = "")
  cat("  Changepoints found: ", min(x$grid$n_cp), " to ", max(x$grid$n_cp),
      "\n\n", sep = "")
  print(x$grid[, c(x$params, "n_cp")], n = 10)
  invisible(x)
}

#' @rdname cpt_influence
#' @export
tidy.ggcpt_influence <- function(x, ...) {
  x$influence
}

#' @rdname cpt_sensitivity
#' @export
tidy.ggcpt_sensitivity <- function(x, ...) {
  rows <- lapply(seq_len(nrow(x$grid)), function(i) {
    cp <- x$grid$cpts[[i]]
    if (length(cp) == 0) return(NULL)
    out <- x$grid[rep(i, length(cp)), x$params, drop = FALSE]
    out$cp <- cp
    out
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    out <- x$grid[0, x$params, drop = FALSE]
    out$cp <- integer(0)
    return(out)
  }
  do.call(rbind, rows)
}

#' @rdname cpt_sensitivity
#' @param object A \code{ggcpt_sensitivity} object (for \code{autoplot()}).
#' @export
autoplot.ggcpt_sensitivity <- function(object, ...) {
  cps <- tidy.ggcpt_sensitivity(object)
  first <- object$params[1]
  d <- object$grid
  d$.setting <- factor(seq_len(nrow(d)),
                       labels = apply(d[, object$params, drop = FALSE], 1,
                                      function(r) {
                                        paste(object$params, format(r),
                                              sep = " = ", collapse = ", ")
                                      }))
  series <- tibble::tibble(index = seq_along(object$data),
                           value = object$data)

  p <- ggplot2::ggplot(series, ggplot2::aes(index, value)) +
    ggplot2::geom_line(colour = "grey55") +
    ggplot2::labs(x = "Index", y = "Value",
                  title = paste0("Sensitivity of `", object$method,
                                 "` to ", paste(object$params,
                                                collapse = ", ")))
  if (nrow(cps) == 0) {
    return(p + ggplot2::labs(subtitle = "No setting detected a changepoint"))
  }
  cps$.setting <- d$.setting[match(
    do.call(paste, c(cps[, object$params, drop = FALSE], sep = "\r")),
    do.call(paste, c(d[, object$params, drop = FALSE], sep = "\r"))
  )]
  p +
    ggplot2::geom_vline(data = cps, ggplot2::aes(xintercept = cp),
                        colour = "#0072B2", linewidth = 0.4) +
    ggplot2::facet_wrap(~.setting, ncol = 1, strip.position = "right")
}
