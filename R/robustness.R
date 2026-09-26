# ---------------------------------------------------------------------------
# Robustness to the noise model.
#
# The package had two tools for "should I trust this changepoint?", and
# both perturb something that is not the cause of the commonest error:
# cpt_consensus() varies the algorithm and cpt_stability() the sample, and a
# false positive from autocorrelated noise is a feature of the realised
# noise path, which every algorithm and every resample reproduce. Varying
# the noise model is the axis that touches the cause: measured on pure
# AR(1) noise, the alternative setting removed 80% of `smuce`'s artifacts
# (family = "hsmuce") and 95% of `nsp`'s (variant = "selfnorm"). It is a
# graded signal, not a verdict on any one changepoint, so the output is the
# set difference, not a score.
# ---------------------------------------------------------------------------

# Internal: each method's noise-model settings to sweep, default first. The
# argument names are noise_model_args()'s; a test keeps them in step with
# the wrappers' formals.
#' @noRd
noise_model_settings <- function() {
  cp <- list(`change_in = "mean"` = list(change_in = "mean"),
             `change_in = "meanvar"` = list(change_in = "meanvar"))
  list(
    pelt = cp, binseg = cp, segneigh = cp, amoc = cp,
    smuce = list(`family = "gauss"` = list(family = "gauss"),
                 `family = "hsmuce"` = list(family = "hsmuce")),
    hsmuce = list(`family = "hsmuce"` = list(family = "hsmuce"),
                  `family = "gauss"` = list(family = "gauss")),
    nsp = list(`variant = "poly"` = list(variant = "poly"),
               `variant = "selfnorm"` = list(variant = "selfnorm"),
               `variant = "ar"` = list(variant = "ar")),
    fastcpd = list(`family = "mean"` = list(family = "mean"),
                   `family = "meanvariance"` =
                     list(family = "meanvariance")),
    fmean = list(`robust = FALSE` = list(robust = FALSE),
                 `robust = TRUE` = list(robust = TRUE)),
    cpm = list(`cpm_type = "Mann-Whitney"` = list(cpm_type = "Mann-Whitney"),
               `cpm_type = "Student"` = list(cpm_type = "Student"),
               `cpm_type = "Lepage"` = list(cpm_type = "Lepage"))
  )
}

#' Does a changepoint survive a change of noise model?
#'
#' Re-runs a detector under each setting of its noise-model argument
#' (\code{family} for \pkg{stepR}'s SMUCE, \code{variant} for NSP, the cost
#' for the \pkg{changepoint} engines, ...) and reports which changepoints
#' appear under every setting and which only under some. A changepoint that
#' survives a change of noise model is evidence; one that survives a
#' bootstrap (\code{\link{cpt_stability}()}) or a change of algorithm
#' (\code{\link{cpt_consensus}()}) may only be a feature of the noise, which
#' resampling and other algorithms reproduce.
#'
#' @param x A numeric series, or a \code{ggcpt} fit (its series, method and
#'   penalty are used).
#' @param method The detector, when \code{x} is a series.
#' @param over What to vary. Only \code{"noise_model"} for now.
#' @param settings Optional named list of settings, each a list of
#'   arguments for \code{\link{cpt_detect}()}, replacing the method's own
#'   sweep. The first is the reference.
#' @param margin Positions within which two changepoints count as the same.
#'   Defaults to \code{5}.
#' @param seed Optional seed, scoped to this call.
#' @param ... Further arguments for every \code{cpt_detect()} call.
#' @return A \code{ggcpt_robustness} object: a list with \code{settings}
#'   (one row per setting: \code{setting}, \code{n_cp}, \code{powered} and
#'   \code{error}), \code{changepoints} (one row per changepoint of the
#'   reference setting: \code{cp}, \code{found_by} (how many other powered
#'   settings find it within \code{margin}), \code{of} (how many powered
#'   alternatives there are) and \code{survives}), \code{union} and
#'   \code{intersection} (distinct locations found by any and by every
#'   powered setting) and \code{fits}. With \code{print()} and
#'   \code{autoplot()}.
#'
#' @section Powerless settings are reported, not counted:
#' A setting that finds nothing where the reference finds several is not a
#' vote against them: measured, NSP's \code{variant = "ar"} removes the
#' false positives of autocorrelated noise by removing detection altogether
#' (0 of 2 real changes found). Such a setting is marked
#' \code{powered = FALSE} and left out of \code{found_by}.
#' @seealso \code{\link{cpt_assumptions}()}, \code{\link{cpt_sensitivity}()}
#'   for sweeping any argument.
#' @export
#' @family inference
#' @examples
#' set.seed(1)
#' x <- c(rnorm(100), rnorm(100, 2)) + as.numeric(arima.sim(list(ar = 0.6), 200))
#' cpt_robustness(x, method = "pelt")
cpt_robustness <- function(x, method = "pelt", over = "noise_model",
                           settings = NULL, margin = 5, seed = NULL, ...) {
  over <- cpt_match_arg(over, "noise_model", name = "over")
  validate_scalar(margin, "margin", min = 0)
  dots <- list(...)
  if (is_ggcpt(x)) {
    if (!bootstrap_possible(x) || !rerun_matches_result(x)) {
      cpt_abort("`cpt_robustness()` re-runs the detector, and this result ",
                "cannot be re-run from the object.",
                class = "capability_absent")
    }
    method <- scalar_chr(x$method)
    if (is.null(dots$penalty)) dots$penalty <- rerun_penalty(x)
    series <- if (n_coordinates(x) > 1L) {
      w <- x$data_wide
      as.matrix(w[, setdiff(names(w), c("index", "index_value")),
                  drop = FALSE])
    } else {
      x$data$value
    }
  } else {
    series <- x
  }
  settings <- settings %||% noise_model_settings()[[method]]
  if (is.null(settings)) {
    have <- names(noise_model_settings())
    cpt_abort("`", method, "` has no noise-model argument to vary. Methods ",
              "that do: ", paste(have, collapse = ", "), ". To compare with ",
              "an engine that models autocorrelated noise, see ",
              "ggcpt_compare(x, methods = c(\"", method, "\", \"decafs\")).",
              class = "unsupported",
              data = list(method = method, requested = "noise_model",
                          supported = have))
  }
  if (!is.list(settings) || is.null(names(settings)) ||
      length(settings) < 2L) {
    cpt_abort("`settings` must be a named list of at least two settings, ",
              "each a list of cpt_detect() arguments.",
              class = "bad_argument")
  }
  local_seed(seed)
  fits <- lapply(settings, function(st) {
    args <- utils::modifyList(dots, st)
    tryCatch(suppressWarnings(do.call(cpt_detect,
                                      c(list(series, method = method), args))),
             error = function(e) e)
  })
  failed <- vapply(fits, inherits, logical(1), "error")
  if (failed[1]) {
    cpt_abort("The reference setting (", names(settings)[1], ") failed: ",
              conditionMessage(fits[[1]]), class = "engine_error",
              parent = fits[[1]])
  }
  cps <- lapply(fits, function(f) if (inherits(f, "error")) integer(0) else
    f$changepoints$cp)
  n_cp <- lengths(cps)
  powered <- !failed & !(n_cp == 0L & n_cp[1] > 0L)
  powered[1] <- TRUE
  ref <- cps[[1]]
  alt <- which(powered)[-1]
  found_by <- vapply(ref, function(cp) {
    sum(vapply(alt, function(j) any(abs(cps[[j]] - cp) <= margin),
               logical(1)))
  }, numeric(1))
  all_cp <- sort(unique(unlist(cps[powered])))
  clusters <- if (length(all_cp)) {
    cluster_changepoints(stats::setNames(cps[powered],
                                         names(settings)[powered]), margin)
  } else {
    tibble::tibble(cp = integer(), votes = integer())
  }
  out <- list(
    method = method,
    settings = tibble::tibble(
      setting = names(settings), n_cp = unname(n_cp),
      powered = unname(powered),
      error = vapply(fits, function(f) if (inherits(f, "error"))
        conditionMessage(f) else NA_character_, character(1))),
    changepoints = tibble::tibble(cp = ref, found_by = as.integer(found_by),
                                  of = length(alt),
                                  survives = found_by == length(alt) &
                                    length(alt) > 0),
    union = nrow(clusters),
    intersection = sum(clusters$votes == sum(powered)),
    margin = margin,
    fits = fits[!failed]
  )
  structure(out, class = "ggcpt_robustness")
}

#' @rdname cpt_robustness
#' @param object A \code{ggcpt_robustness} object (for \code{autoplot()}).
#' @export
print.ggcpt_robustness <- function(x, ...) {
  cat("ggcpt_robustness (method: ", x$method, ", over the noise model)\n",
      sep = "")
  print(x$settings[, c("setting", "n_cp", "powered")])
  if (any(!x$settings$powered)) {
    cat("Settings finding nothing where the reference finds changepoints ",
        "are left out of the count:\nthey have no power here, and are not ",
        "votes against.\n", sep = "")
  }
  cat("\nReference changepoints (", x$settings$setting[1], "):\n", sep = "")
  print(x$changepoints)
  cat("\nLocations found by any powered setting: ", x$union,
      "; by every one: ", x$intersection, ".\n", sep = "")
  gone <- x$changepoints$cp[!x$changepoints$survives]
  if (length(gone)) {
    cat("Not found under every noise model: ", paste(gone, collapse = ", "),
        ". Treat these as the ones that depend on the noise assumption.\n",
        sep = "")
  }
  invisible(x)
}

#' @rdname cpt_robustness
#' @export
autoplot.ggcpt_robustness <- function(object, ...) {
  rows <- lapply(seq_along(object$fits), function(i) {
    f <- object$fits[[i]]
    tibble::tibble(setting = names(object$fits)[i], cp = f$changepoints$cp)
  })
  d <- do.call(rbind, rows)
  d$setting <- factor(d$setting, levels = rev(object$settings$setting))
  ggplot2::ggplot(d, ggplot2::aes(x = cp, y = setting)) +
    ggplot2::geom_vline(xintercept = object$changepoints$cp,
                        colour = "grey80") +
    ggplot2::geom_point(size = 2.5, colour = "#0072B2") +
    ggplot2::labs(x = "Changepoint", y = NULL,
                  title = paste0("Changepoints under each noise model (",
                                 object$method, ")"),
                  subtitle = "Grey rules: the reference setting's changepoints") +
    theme_ggcpt()
}
