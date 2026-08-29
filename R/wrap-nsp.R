#' NSP wrapper — Narrowest Significance Pursuit
#'
#' Wraps the \pkg{nsp} package (Fryzlewicz 2024). NSP inverts the usual
#' framing of post-selection inference: rather than estimating changepoint
#' locations and then asking whether they are real, it returns a set of
#' \emph{intervals}, each of which contains at least one changepoint, with
#' the guarantee holding \emph{globally} across all intervals simultaneously
#' at level \code{alpha}. The guarantee is exact and finite-sample, and the
#' self-normalised and autoregressive variants keep it under heavy tails,
#' heteroscedasticity and serial dependence.
#'
#' @param x A numeric vector.
#' @param alpha Global significance level: with probability at least
#'   \eqn{1 - \alpha}, \emph{every} returned interval contains a changepoint.
#'   Defaults to \code{0.1}.
#' @param variant Which NSP procedure to run:
#'   \describe{
#'     \item{\code{"poly"}}{(default) \code{nsp::nsp_poly()} — piecewise
#'       polynomial signal, Gaussian noise of constant variance.}
#'     \item{\code{"selfnorm"}}{\code{nsp::nsp_poly_selfnorm()} —
#'       self-normalised, for heavy tails and heteroscedasticity. Slower.}
#'     \item{\code{"ar"}}{\code{nsp::nsp_poly_ar()} — autoregressive noise of
#'       order \code{ord}.}
#'     \item{\code{"tvreg"}}{\code{nsp::nsp_tvreg()} — a general linear model
#'       whose coefficients change; requires \code{covariates}.}
#'   }
#' @param change_in \code{"mean"} (a piecewise-constant signal,
#'   \code{deg = 0}) or \code{"slope"} (piecewise linear, \code{deg = 1}).
#'   Ignored when \code{deg} is given explicitly, and when
#'   \code{variant = "tvreg"} (which takes its model from
#'   \code{covariates}).
#' @param deg Degree of the piecewise polynomial. Derived from
#'   \code{change_in} when \code{NULL}.
#' @param M Number of intervals drawn. Defaults to \code{1000}; the engine's
#'   own default.
#' @param covariates A design matrix for \code{variant = "tvreg"}, with one
#'   row per observation.
#' @param ord AR order for \code{variant = "ar"}. Defaults to \code{1}.
#' @param seed Optional seed. NSP draws random intervals, so a run is
#'   reproducible only with one.
#' @param ... Additional arguments passed to the underlying \pkg{nsp}
#'   function.
#'
#' @section What \code{cp} means here, and what it does not:
#' NSP produces no point estimates. This wrapper still fills the \code{cp}
#' column — with the \emph{midpoint} of each interval — because every
#' downstream consumer in the package (\code{augment()},
#' \code{\link{cpt_metrics}()}, \code{\link{cpt_consensus}()},
#' \code{autoplot()}) is built on that column, and a result with an empty
#' \code{cp} would silently score as "found nothing". The midpoint is
#' \strong{not} an estimate of the changepoint location and must not be
#' reported as one: the interval is the inferential object. The result
#' therefore
#' \itemize{
#'   \item carries the intervals in a \code{regions} slot, read with
#'     \code{\link{cpt_regions}()};
#'   \item marks itself, so \code{print()} says the \code{cp} column is a
#'     midpoint and \code{autoplot()} shades the bands by default;
#'   \item adds a \code{cp_source} column reading
#'     \code{"region_midpoint"} to the changepoints tibble.
#' }
#'
#' @return A \code{ggcpt} object with a populated \code{regions} slot.
#' @references
#' \insertRef{fryzlewicz2024nsp}{ggchangepoint}
#' @seealso \code{\link{cpt_regions}()}, \code{\link{geom_cpt_region}()},
#'   \code{\link{cpt_confint}()}.
#' @export
#' @examplesIf requireNamespace("nsp", quietly = TRUE)
#' set.seed(2026)
#' x <- c(rnorm(100), rnorm(100, 4))
#' fit <- nsp_wrapper(x, M = 100, seed = 1)
#' cpt_regions(fit)
#' ggplot2::autoplot(fit)
nsp_wrapper <- function(x, alpha = 0.1,
                        variant = c("poly", "selfnorm", "ar", "tvreg"),
                        change_in = c("mean", "slope"), deg = NULL,
                        M = 1000, covariates = NULL, ord = 1, seed = NULL,
                        ...) {
  need_pkg("nsp")
  variant <- match.arg(variant)
  change_in <- match.arg(change_in)
  validate_scalar(alpha, "alpha", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_scalar(M, "M", min = 1)
  validate_data(x)
  data_vec <- as_uni_vector(x, "nsp")
  n <- length(data_vec)

  if (is.null(deg)) deg <- if (change_in == "slope") 1L else 0L
  validate_scalar(deg, "deg", min = 0)

  if (!is.null(seed)) set.seed(seed)

  fit <- switch(variant,
    poly = nsp::nsp_poly(data_vec, M = M, alpha = alpha, deg = deg, ...),
    selfnorm = nsp::nsp_poly_selfnorm(data_vec, M = M, alpha = alpha,
                                      deg = deg, ...),
    ar = nsp::nsp_poly_ar(data_vec, ord = ord, M = M, alpha = alpha,
                          deg = deg, ...),
    tvreg = {
      if (is.null(covariates)) {
        stop("`variant = \"tvreg\"` fits a linear model whose coefficients ",
             "change, so it needs `covariates`: a design matrix with one ",
             "row per observation.", call. = FALSE)
      }
      Xd <- as.matrix(covariates)
      if (nrow(Xd) != n) {
        stop("`covariates` must have one row per observation: the series has ",
             n, " but `covariates` has ", nrow(Xd), ".", call. = FALSE)
      }
      nsp::nsp_tvreg(data_vec, x = Xd, M = M, alpha = alpha, ...)
    }
  )

  iv <- fit$intervals
  if (is.null(iv) || nrow(iv) == 0) {
    res <- ggcpt_build(data_vec, integer(0), method = "nsp",
                       change_in = change_in,
                       penalty = list(type = "alpha", value = alpha),
                       fit = fit, call = match.call(),
                       regions = tibble::tibble(start = integer(),
                                                end = integer()))
    res$region_level <- alpha
    res$cp_from_regions <- TRUE
    return(res)
  }

  starts <- as.integer(round(iv$starts))
  ends <- as.integer(round(iv$ends))
  # `midpoints` is what nsp itself reports; recompute only if it is absent.
  mids <- if (!is.null(iv$midpoints)) {
    as.integer(round(iv$midpoints))
  } else {
    as.integer(round((starts + ends) / 2))
  }
  regions <- tibble::tibble(start = starts, end = ends,
                            value = as.numeric(iv$values %||% NA_real_))

  res <- ggcpt_build(
    data_vec, mids,
    method = "nsp",
    change_in = change_in,
    penalty = list(type = "alpha", value = alpha),
    fit = fit,
    call = match.call(),
    extra_cp_cols = list(
      region_start = starts,
      region_end = ends,
      cp_source = rep("region_midpoint", length(mids))
    ),
    regions = regions
  )
  res$region_level <- alpha
  res$cp_from_regions <- TRUE
  res$threshold_used <- as.numeric(fit$threshold.used %||% NA_real_)
  res
}
