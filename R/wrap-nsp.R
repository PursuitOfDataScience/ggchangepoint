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
#'   reproducible only with one. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
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
#' @family changepoint engines
nsp_wrapper <- function(x, alpha = 0.1,
                        variant = c("poly", "selfnorm", "ar", "tvreg"),
                        change_in = c("mean", "slope"), deg = NULL,
                        M = 1000, covariates = NULL, ord = 1, seed = NULL,
                        ...) {
  need_pkg("nsp")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(ord, "ord", min = 0)
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

  local_seed(seed)

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
      # The tvreg variant fits a linear model whose COEFFICIENTS change, so
      # `change_in` -- match.arg'ed from c("mean", "slope") and reported as
      # given -- labelled a regression-coefficient change as a change in the
      # mean. The label follows the variant, the way not_wrapper() derives
      # its own from `contrast`; "regression" is in nsp's registry row so
      # the result is one validate_method_change_in() accepts.
      if (!identical(change_in, "slope")) change_in <- "regression"
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

  # Keep the changepoint rows and the regions in step.
  #
  # The two paths used to diverge: `mids` goes through ggcpt_build(), which
  # deduplicates `cp` and drops anything outside 1..(n-1); `regions` goes
  # through normalise_regions(), which does neither. Nested or overlapping
  # intervals are the *normal* output of the narrowest-significance
  # construction and two of them can round to one midpoint -- [10, 20] and
  # [11, 19] both give 15 -- so the dedup silently dropped one changepoint
  # row, taking its `region_start`/`region_end` with it, while `$regions`
  # kept both.
  #
  # Everything downstream then disagreed with itself: print() reported one
  # fewer changepoint than the regions table it printed underneath, and
  # `native_bounds()` reads the bounds off `$changepoints`, so
  # `cpt_confint()` returned one interval fewer than there were regions --
  # under-reporting the object this method exists to produce.
  #
  # Resolved here rather than in normalise_regions(), because which region
  # to keep is a question about NSP: the narrowest is the informative one,
  # that being the whole point of narrowest-significance pursuit. Warned
  # about rather than done silently, since a collision means two distinct
  # significance statements landed on one location.
  drop_lo <- mids < 1L | mids > length(data_vec) - 1L
  width <- ends - starts
  ord <- order(mids, width)
  dup <- duplicated(mids[ord])
  collapsed <- ord[dup]
  if (length(collapsed) > 0 || any(drop_lo)) {
    warning("`nsp` returned ", length(mids), " significance regions but ",
            length(mids) - length(union(collapsed, which(drop_lo))),
            " distinct usable midpoint(s): ",
            if (length(collapsed) > 0) {
              paste0(length(collapsed),
                     " region(s) share a midpoint with a narrower one")
            } else "",
            if (length(collapsed) > 0 && any(drop_lo)) ", and " else "",
            if (any(drop_lo)) {
              paste0(sum(drop_lo), " midpoint(s) fall outside 1..",
                     length(data_vec) - 1L)
            } else "",
            ". The regions slot keeps only the rows a changepoint could be ",
            "keyed to, so `nrow(fit$regions)` and `nrow(fit$changepoints)` ",
            "agree; read `$fit` for everything the engine returned.",
            call. = FALSE)
  }
  keep_region <- setdiff(seq_along(mids), union(collapsed, which(drop_lo)))
  keep_region <- keep_region[order(mids[keep_region])]
  mids <- mids[keep_region]
  starts <- starts[keep_region]
  ends <- ends[keep_region]
  regions <- regions[keep_region, , drop = FALSE]

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
