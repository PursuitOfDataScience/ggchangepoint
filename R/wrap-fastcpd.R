#' fastcpd wrapper: fast changepoint detection via sequential gradient descent
#'
#' Wraps the \pkg{fastcpd} package (Li and Zhang, 2024), a modern PELT-family
#' engine that pairs pruning with sequential gradient descent so that exact or
#' near-exact segmentations of many model families run in near-linear time.
#' Every family the engine documents is reachable except its user-supplied
#' \code{custom} cost: Gaussian mean, variance and both; the count and
#' binary families (\code{"poisson"}, \code{"binomial"}) and waiting times
#' (\code{"exponential"}); linear and penalised regression (\code{"lm"},
#' \code{"lasso"}) and generalised linear models (\code{"poisson"},
#' \code{"binomial"} with \code{covariates}); and the time-series models
#' \code{"ar"}, \code{"arma"}, \code{"arima"}, \code{"garch"} and, for a
#' multivariate series, \code{"var"}.
#'
#' @param x A numeric vector, or (for \code{family} \code{"mean"},
#'   \code{"variance"}, \code{"meanvariance"} and \code{"var"}) a matrix
#'   with one row per time point for multivariate detection.
#' @param family Model family. \code{"mean"} (the default),
#'   \code{"variance"}, \code{"meanvariance"}; \code{"poisson"},
#'   \code{"binomial"}, \code{"exponential"} (a change in the rate or
#'   probability of a count, binary or waiting-time series, or in a
#'   regression on \code{covariates}); \code{"lm"}, \code{"lasso"}
#'   (regression, which needs \code{covariates}); \code{"ar"},
#'   \code{"arma"}, \code{"arima"}, \code{"garch"}, \code{"var"}.
#' @param order Model order for \code{"ar"} and \code{"var"} (a single
#'   integer), \code{"arma"} and \code{"garch"} (length 2) or
#'   \code{"arima"} (length 3). Defaults to \code{1} for AR and VAR,
#'   \code{c(1, 1)} for ARMA and GARCH and \code{c(1, 0, 0)} for ARIMA.
#' @param covariates Optional numeric matrix of regressors, one row per
#'   observation, for \code{"lm"}, \code{"lasso"}, \code{"poisson"} and
#'   \code{"binomial"}. Include a column of ones for an intercept. Without
#'   it the count and binary families fit an intercept only: a change in
#'   the rate or probability. \code{\link{cpt_detect}()} builds it from a
#'   formula.
#' @param ... Additional arguments passed to the corresponding
#'   \code{fastcpd::fastcpd.*()} function (e.g. \code{beta}, \code{trim}).
#'   \code{beta} is \pkg{fastcpd}'s penalty: a number, or one of its own
#'   names (\code{"MBIC"}, the default, \code{"BIC"}, \code{"MDL"}). It is
#'   recorded on the result, so \code{print()} and \code{glance()} report
#'   the penalty the fit actually used. \code{\link{cpt_detect}()} forwards
#'   a numeric \code{penalty} here and translates the three names it shares
#'   with \pkg{fastcpd}; see the penalty-semantics section of
#'   \code{\link{cpt_penalty}()}.
#' @return A \code{ggcpt} object. The engine's per-segment parameter
#'   estimates (\code{thetas}) are kept as \code{$coefficients}, one row
#'   per segment and parameter, for the families where they are
#'   coefficients (the regressions and the intercept-only count, binary and
#'   waiting-time models).
#' @references
#' \insertRef{li2024fastcpd}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("fastcpd", quietly = TRUE)
#' set.seed(2026)
#' res <- fastcpd_wrapper(c(rnorm(100), rnorm(100, 4)))
#' res$changepoints
#'
#' # A change in a Poisson rate
#' counts <- fastcpd_wrapper(c(rpois(100, 3), rpois(100, 9)),
#'                           family = "poisson")
#' counts$changepoints
#' @family changepoint engines
fastcpd_wrapper <- function(x, family = c("mean", "variance", "meanvariance",
                                          "ar", "arma", "arima", "garch",
                                          "var", "lm", "lasso", "poisson",
                                          "binomial", "exponential"),
                            order = NULL, covariates = NULL, ...) {
  need_pkg("fastcpd")
  family <- cpt_match_arg(family)
  # fastcpd names a bad `order` itself, except `NA` ("missing value where
  # TRUE/FALSE needed") and a string ("non-numeric argument to mathematical
  # function").
  if (!is.null(order) && (!is.numeric(order) || anyNA(order))) {
    cpt_abort("`order` must be numeric: one integer for \"ar\" and \"var\", ",
              "two for \"arma\" and \"garch\", three for \"arima\" (got ",
              paste(format(order), collapse = ", "), ").",
              class = "bad_argument")
  }

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  mv_ok <- c("mean", "variance", "meanvariance", "var")
  if (is_mv && ncol(as.matrix(x)) > 1L && !family %in% mv_ok) {
    cpt_abort("Multivariate input is only supported for `family` \"mean\", ",
              "\"variance\", \"meanvariance\" or \"var\".",
              class = "unsupported",
              data = list(method = "fastcpd", requested = family,
                          supported = mv_ok))
  }
  if (family == "var" && (!is_mv || ncol(as.matrix(x)) < 2L)) {
    cpt_abort("`family = \"var\"` is a vector autoregression and needs a ",
              "series of at least two columns.", class = "wrong_dimension")
  }
  X <- if (is_mv) as_mv_matrix(x) else as.numeric(x)
  data_vec <- if (is_mv) as.numeric(X[, 1]) else X

  needs_cov <- c("lm", "lasso")
  glm_fams <- c("poisson", "binomial")
  if (family %in% needs_cov && is.null(covariates)) {
    cpt_abort("`family = \"", family, "\"` is a regression and needs ",
              "`covariates` (or a formula through cpt_detect(y ~ x, data = ",
              "d, method = \"fastcpd\")).", class = "bad_argument")
  }
  if (!is.null(covariates)) {
    if (!family %in% c(needs_cov, glm_fams)) {
      cpt_abort("`covariates` are used by the regression families (\"lm\", ",
                "\"lasso\", \"poisson\", \"binomial\"), not by \"", family,
                "\".", class = "bad_argument")
    }
    covariates <- as.matrix(covariates)
    if (!is.numeric(covariates) || nrow(covariates) != length(data_vec)) {
      cpt_abort("`covariates` must be a numeric matrix with one row per ",
                "observation (", length(data_vec), ").",
                class = "bad_argument")
    }
  }
  if (family %in% glm_fams) {
    check_family_data(data_vec, family, "fastcpd")
  }
  if (family == "exponential") check_family_data(data_vec, family, "fastcpd")

  design <- if (family %in% c(needs_cov, glm_fams)) {
    cbind(data_vec, covariates %||% matrix(1, nrow = length(data_vec),
                                           ncol = 1))
  }

  # cv.glmnet, which the lasso family calls per segment, warns "Option
  # grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold"
  # for every short candidate segment: advice about an internal
  # cross-validation the caller never ran. Muffled by message; anything
  # else still reaches the caller.
  fit <- withCallingHandlers(switch(family,
    mean = fastcpd::fastcpd.mean(X, r.progress = FALSE, ...),
    variance = fastcpd::fastcpd.variance(X, r.progress = FALSE, ...),
    meanvariance = fastcpd::fastcpd.meanvariance(X, r.progress = FALSE, ...),
    ar = fastcpd::fastcpd.ar(X, order = order %||% 1, r.progress = FALSE, ...),
    arma = fastcpd::fastcpd.arma(X, order = order %||% c(1, 1),
                                 r.progress = FALSE, ...),
    arima = fastcpd::fastcpd.arima(X, order = order %||% c(1, 0, 0),
                                   r.progress = FALSE, ...),
    garch = fastcpd::fastcpd.garch(X, order = order %||% c(1, 1),
                                   r.progress = FALSE, ...),
    var = fastcpd::fastcpd.var(X, order = order %||% 1, r.progress = FALSE,
                               ...),
    lm = fastcpd::fastcpd.lm(design, r.progress = FALSE, ...),
    lasso = fastcpd::fastcpd.lasso(design, r.progress = FALSE, ...),
    poisson = fastcpd::fastcpd.poisson(design, r.progress = FALSE, ...),
    binomial = fastcpd::fastcpd.binomial(design, r.progress = FALSE, ...),
    exponential = fastcpd::fastcpd.exponential(X, r.progress = FALSE, ...)
  ), warning = function(w) {
    if (grepl("grouped=FALSE enforced", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })

  change_lab <- switch(family,
    mean = "mean", variance = "var", meanvariance = "meanvar",
    ar = "model (AR)", arma = "model (ARMA)", arima = "model (ARIMA)",
    garch = "model (GARCH)", var = "model (VAR)",
    lm = "regression", lasso = "regression",
    poisson = if (is.null(covariates)) "mean" else "regression",
    binomial = if (is.null(covariates)) "mean" else "regression",
    exponential = "mean"
  )

  # fastcpd's penalty argument is `beta`, and it reaches the engine through
  # `...`. Hard-coding "MBIC" here meant `fastcpd_wrapper(x, beta = 20)` ran
  # at 20 while print() reported `Penalty: MBIC` and glance()$penalty_type
  # said "MBIC" -- the object misdescribing the fit it holds. Report what
  # was used; "MBIC" is right only when nothing was passed, which is
  # fastcpd's own default.
  beta <- list(...)[["beta"]]
  pen <- if (is.null(beta)) {
    list(type = "MBIC", value = NA_real_)
  } else if (is.numeric(beta)) {
    list(type = "Manual", value = as.numeric(beta)[1])
  } else {
    list(type = as.character(beta)[1], value = NA_real_)
  }

  res <- ggcpt_build(
    data_vec, as.integer(fit@cp_set),
    method = "fastcpd",
    change_in = change_lab,
    penalty = pen,
    fit = fit,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(X)
  )
  if (family %in% c("poisson", "binomial", "exponential")) {
    res$family <- family
  }
  if (family %in% c("lm", "lasso", "poisson", "binomial", "exponential")) {
    terms <- if (family == "exponential") "rate" else
      colnames(covariates) %||% if (is.null(covariates)) "(Intercept)" else
        paste0("x", seq_len(ncol(covariates)))
    res$coefficients <- fastcpd_thetas(fit, terms, res$changepoints$cp,
                                       length(data_vec))
  }
  res
}

# Internal: fastcpd's per-segment parameter estimates as a coefficient
# table. The engine gives estimates only, so the interval columns are NA.
#' @noRd
fastcpd_thetas <- function(fit, terms, cps, n) {
  th <- tryCatch(as.matrix(fit@thetas), error = function(e) NULL)
  bounds <- c(0L, sort(cps), n)
  k <- length(bounds) - 1L
  if (is.null(th) || ncol(th) != k || nrow(th) != length(terms)) return(NULL)
  do.call(rbind, lapply(seq_len(k), function(s) {
    tibble::tibble(segment = s, start = bounds[s] + 1L, end = bounds[s + 1L],
                   term = terms, estimate = as.numeric(th[, s]),
                   std_error = NA_real_, conf_low = NA_real_,
                   conf_high = NA_real_)
  }))
}
