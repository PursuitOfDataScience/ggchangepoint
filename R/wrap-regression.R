#' Bai-Perron structural break wrapper (strucchange)
#'
#' Wraps \code{strucchange::breakpoints()} (Zeileis et al., 2002), the
#' dynamic-programming implementation of the Bai and Perron (1998, 2003)
#' multiple structural break estimator. Called with a bare numeric vector it
#' dates mean shifts (\code{y ~ 1}); called with a formula and data it dates
#' breaks in arbitrary regression coefficients. Break-date confidence
#' intervals from \code{confint()} populate \code{ci_lower}/\code{ci_upper}
#' and render via \code{autoplot(show_ci = TRUE)}.
#'
#' @param x A numeric vector (mean-shift mode), or a model formula
#'   (regression mode; supply \code{data} too).
#' @param data Optional data frame for formula input.
#' @param breaks Maximum number of breaks; when \code{NULL} the number is
#'   chosen by BIC.
#' @param h Minimal segment size, as a fraction of the sample size (or an
#'   integer count). Defaults to \code{0.15}.
#' @param conf_level Confidence level for the break-date intervals. Defaults
#'   to \code{0.95}.
#' @param ... Additional arguments passed to
#'   \code{strucchange::breakpoints()}.
#' @return A \code{ggcpt} object with \code{ci_lower}/\code{ci_upper} columns
#'   on the changepoints tibble.
#' @references
#' \insertRef{bai2003computation}{ggchangepoint}
#'
#' \insertRef{zeileis2002strucchange}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("strucchange", quietly = TRUE)
#' set.seed(2026)
#' res <- strucchange_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
strucchange_wrapper <- function(x, data = NULL, breaks = NULL, h = 0.15,
                                conf_level = 0.95, ...) {
  need_pkg("strucchange")

  if (inherits(x, "formula")) {
    if (is.null(data)) {
      stop("`data` must be supplied when `x` is a formula.", call. = FALSE)
    }
    response <- all.vars(x)[1]
    data_vec <- as.numeric(data[[response]])
    fml <- x
  } else {
    validate_data(x)
    data_vec <- as.numeric(x)
    data <- data.frame(.y = data_vec)
    fml <- stats::as.formula(".y ~ 1")
  }

  args <- list(formula = fml, data = data, h = h, ...)
  if (!is.null(breaks)) args$breaks <- breaks
  fit <- do.call(strucchange::breakpoints, args)

  bp <- fit$breakpoints
  if (length(bp) == 1 && is.na(bp)) bp <- integer(0)
  cp_indices <- as.integer(bp)

  ci_lower <- NULL
  ci_upper <- NULL
  if (length(cp_indices) > 0) {
    ci <- tryCatch(
      stats::confint(fit, level = conf_level)$confint,
      error = function(e) NULL
    )
    if (!is.null(ci) && nrow(ci) == length(cp_indices)) {
      ci_lower <- pmax(1L, as.integer(ci[, 1]))
      ci_upper <- pmin(length(data_vec) - 1L, as.integer(ci[, 3]))
    }
  }

  ggcpt_build(
    data_vec, cp_indices,
    method = "strucchange",
    change_in = if (identical(deparse(fml), ".y ~ 1")) "mean" else "regression",
    penalty = list(type = "BIC", value = NA_real_),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (!is.null(ci_lower)) {
      list(ci_lower = ci_lower, ci_upper = ci_upper)
    }
  )
}

#' Broken-line regression wrapper (segmented)
#'
#' Wraps \code{segmented::segmented()} (Muggeo, 2003, 2008): maximum
#' likelihood estimation of \emph{continuous} piecewise-linear
#' ("broken-line") regressions, with standard errors and confidence
#' intervals for the breakpoint locations. Where the step-change engines
#' model jumps in the level, \code{segmented} models kinks in the trend, so
#' \code{change_in} is \code{"slope"} and the fitted broken line is stored in
#' the \code{fitted} column for \code{autoplot(show_fit = TRUE)}.
#'
#' @param x A numeric vector; a linear model of \code{x} on time
#'   \code{1:length(x)} is segmented.
#' @param npsi Number of breakpoints to estimate. Defaults to \code{1}.
#' @param conf_level Confidence level for breakpoint intervals. Defaults to
#'   \code{0.95}.
#' @param seed Optional seed (the estimator uses bootstrap restarting).
#' @param ... Additional arguments passed to \code{segmented::segmented()}.
#' @return A \code{ggcpt} object with \code{ci_lower}/\code{ci_upper} columns
#'   and the fitted broken line in \code{$data$fitted}. Breakpoints are
#'   rounded to the nearest index; for a continuous fit the reported location
#'   is the kink itself.
#' @references
#' \insertRef{muggeo2003segmented}{ggchangepoint}
#'
#' \insertRef{muggeo2008segmented}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("segmented", quietly = TRUE)
#' set.seed(2026)
#' y <- cumsum(c(rep(0.5, 100), rep(-0.3, 100))) + rnorm(200)
#' res <- segmented_wrapper(y, npsi = 1)
#' res$changepoints
#' ggplot2::autoplot(res, show_fit = TRUE, show_ci = TRUE)
segmented_wrapper <- function(x, npsi = 1, conf_level = 0.95, seed = NULL,
                              ...) {
  need_pkg("segmented")

  validate_data(x)
  data_vec <- as.numeric(x)
  df <- data.frame(.y = data_vec, .t = seq_along(data_vec))

  if (!is.null(seed)) set.seed(seed)

  base_fit <- stats::lm(.y ~ .t, data = df)
  fit <- segmented::segmented(base_fit, seg.Z = ~.t, npsi = npsi, ...)

  if (!inherits(fit, "segmented") || is.null(fit$psi)) {
    return(ggcpt_build(
      data_vec, integer(0),
      method = "segmented", change_in = "slope",
      penalty = list(type = "npsi", value = npsi),
      fit = fit, call = match.call()
    ))
  }

  psi <- fit$psi[, "Est."]
  cp_indices <- as.integer(round(psi))

  ci <- tryCatch(segmented::confint.segmented(fit, level = conf_level),
                 error = function(e) NULL)
  ci_lower <- NULL
  ci_upper <- NULL
  if (!is.null(ci)) {
    ci <- as.matrix(ci)
    if (nrow(ci) == length(cp_indices) && ncol(ci) >= 3) {
      ci_lower <- pmax(1L, as.integer(floor(ci[, 2])))
      ci_upper <- pmin(length(data_vec) - 1L, as.integer(ceiling(ci[, 3])))
    }
  }

  ggcpt_build(
    data_vec, cp_indices,
    method = "segmented",
    change_in = "slope",
    penalty = list(type = "npsi", value = npsi),
    fit = fit,
    call = match.call(),
    extra_cp_cols = if (!is.null(ci_lower)) {
      list(ci_lower = ci_lower, ci_upper = ci_upper)
    },
    fitted = as.numeric(stats::fitted(fit))
  )
}

#' EnvCpt wrapper — changepoints versus trends versus autocorrelation
#'
#' Wraps \code{EnvCpt::envcpt()} (Beaulieu and Killick, 2018), which fits up
#' to twelve competing models — constant mean or linear trend, each with or
#' without changepoints, and with white-noise, AR(1) or AR(2) errors — and
#' lets an information criterion decide whether the series really contains
#' changepoints or merely trend/autocorrelation ("memory"). The changepoints
#' of the winning model (if any) are returned, and the winning model's name
#' is recorded, guarding against the classic false positive of running a
#' mean-shift detector on autocorrelated data.
#'
#' @param x A numeric vector.
#' @param models Character vector of models to fit; see
#'   \code{EnvCpt::envcpt()}. Defaults to all twelve.
#' @param criterion Model selection criterion: \code{"AIC"} (default) or
#'   \code{"BIC"}.
#' @param minseglen Minimum segment length. Defaults to \code{5}.
#' @param ... Additional arguments passed to \code{EnvCpt::envcpt()}.
#' @return A \code{ggcpt} object. \code{$fit} holds the full \code{envcpt}
#'   output; the selected model name is stored in the penalty descriptor and
#'   printed by \code{glance()} via \code{penalty_type}.
#' @references
#' \insertRef{beaulieu2018envcpt}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("EnvCpt", quietly = TRUE)
#' set.seed(2026)
#' res <- envcpt_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
envcpt_wrapper <- function(x, models = c("mean", "meancpt", "meanar1",
                                         "meanar2", "meanar1cpt",
                                         "meanar2cpt", "trend", "trendcpt",
                                         "trendar1", "trendar2",
                                         "trendar1cpt", "trendar2cpt"),
                           criterion = c("AIC", "BIC"), minseglen = 5, ...) {
  need_pkg("EnvCpt")
  criterion <- match.arg(criterion)

  validate_data(x)
  data_vec <- as.numeric(x)

  fit <- EnvCpt::envcpt(data_vec, models = models, minseglen = minseglen,
                        verbose = FALSE, ...)

  crit_vals <- if (criterion == "AIC") stats::AIC(fit) else stats::BIC(fit)
  crit_vals <- crit_vals[is.finite(crit_vals)]
  if (length(crit_vals) == 0) {
    stop("envcpt did not successfully fit any of the requested models.",
         call. = FALSE)
  }
  best <- names(which.min(crit_vals))

  cp_indices <- if (grepl("cpt", best)) {
    tryCatch(as.integer(changepoint::cpts(fit[[best]])),
             error = function(e) integer(0))
  } else {
    integer(0)
  }

  ggcpt_build(
    data_vec, cp_indices,
    method = "envcpt",
    change_in = if (grepl("^trend", best)) "trend" else "mean",
    penalty = list(type = paste0(criterion, ": ", best),
                   value = unname(min(crit_vals))),
    fit = fit,
    call = match.call()
  )
}
