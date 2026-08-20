#' DeCAFS wrapper — changes amid drift and autocorrelated noise
#'
#' Wraps \code{DeCAFS::DeCAFS()} (Romano, Rigaill, Runge and Fearnhead,
#' 2022), which detects abrupt mean changes when the underlying signal also
#' drifts (random-walk fluctuations) and the noise is AR(1)-autocorrelated —
#' the two regimes in which plain change-in-mean methods over-detect. Model
#' parameters are estimated automatically unless supplied.
#'
#' @param x A numeric vector.
#' @param penalty Penalty \eqn{\beta} for adding a changepoint. Defaults to
#'   \code{2 * log(length(x))}. \code{\link{cpt_detect}} resolves its own
#'   \code{"MBIC"} default to a stronger numeric value — on a five-changepoint
#'   series that is 3 changepoints through the dispatcher against 5 here — so
#'   pass \code{penalty} explicitly when the two must agree.
#' @param model_param Optional list of model parameters
#'   (\code{sdEta}, \code{sdNu}, \code{phi}) as accepted by
#'   \code{DeCAFS::DeCAFS()}; when \code{NULL} they are estimated from the
#'   data.
#' @param ... Additional arguments passed to \code{DeCAFS::DeCAFS()}.
#' @return A \code{ggcpt} object. The \code{data} tibble carries the
#'   estimated signal in its \code{fitted} column.
#' @references
#' \insertRef{romano2022decafs}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("DeCAFS", quietly = TRUE)
#' set.seed(2026)
#' res <- decafs_wrapper(c(rnorm(100), rnorm(100, 5)))
#' res$changepoints
decafs_wrapper <- function(x, penalty = NULL, model_param = NULL, ...) {
  need_pkg("DeCAFS")

  validate_data(x)
  data_vec <- as_uni_vector(x, "decafs")

  if (is.null(penalty)) {
    penalty <- 2 * log(length(data_vec))
  }

  args <- list(data = data_vec, beta = penalty, warningMessage = FALSE, ...)
  if (!is.null(model_param)) args$modelParam <- model_param
  fit <- do.call(DeCAFS::DeCAFS, args)

  ggcpt_build(
    data_vec, as.integer(fit$changepoints),
    method = "decafs",
    change_in = "mean",
    penalty = list(type = "Manual", value = penalty),
    fit = fit,
    call = match.call(),
    fitted = as.numeric(fit$signal)
  )
}

#' Self-normalisation wrapper (SNSeg)
#'
#' Wraps \code{SNSeg::SNSeg_Uni()} (Zhao, Jiang and Shao, 2022):
#' self-normalised segmentation with nested local windows. Self-normalisation
#' avoids estimating the long-run variance, is robust to temporal dependence,
#' and detects changes in general parameters — mean, variance, quantiles,
#' autocorrelation, or bivariate correlation — within one framework.
#'
#' @param x A numeric vector (or a two-column matrix for
#'   \code{parameter = "bivcor"}).
#' @param parameter Which parameter to test for changes: \code{"mean"},
#'   \code{"variance"}, \code{"acf"}, or \code{"bivcor"} (bivariate
#'   correlation). Defaults to \code{"mean"}.
#' @param confidence Confidence level of the self-normalised test, one of
#'   0.9, 0.95, 0.99, 0.995 or 0.999. Defaults to \code{0.9}.
#' @param grid_size Grid size controlling the local-window sweep; when
#'   \code{NULL} the engine's default is used.
#' @param ... Additional arguments passed to \code{SNSeg::SNSeg_Uni()}.
#' @return A \code{ggcpt} object. About 20 observations are needed for the
#'   nested local windows at the default \code{grid_size}; a constant series
#'   returns an empty result rather than an engine error.
#' @references
#' \insertRef{zhao2022snseg}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("SNSeg", quietly = TRUE)
#' \donttest{
#' set.seed(2026)
#' res <- sn_wrapper(c(rnorm(150), rnorm(150, 3)))
#' res$changepoints
#' }
sn_wrapper <- function(x, parameter = c("mean", "variance", "acf", "bivcor"),
                       confidence = 0.9, grid_size = NULL, ...) {
  need_pkg("SNSeg")
  parameter <- match.arg(parameter)

  validate_data(x)
  is_mv <- is.matrix(x) || is.data.frame(x)
  if (parameter == "bivcor") {
    if (!is_mv || ncol(as.matrix(x)) != 2) {
      stop("`parameter = \"bivcor\"` requires a two-column matrix.",
           call. = FALSE)
    }
    X <- as_mv_matrix(x)
    input <- X
    data_vec <- as.numeric(X[, 1])
  } else {
    if (is_mv) {
      stop("`parameter = \"", parameter, "\"` requires a numeric vector; ",
           "use `parameter = \"bivcor\"` for bivariate input.", call. = FALSE)
    }
    input <- as.numeric(x)
    data_vec <- input
  }

  change_lab <- switch(parameter,
    mean = "mean", variance = "var", acf = "acf", bivcor = "correlation"
  )

  # A flat series leaves the self-normalised statistic undefined (the engine
  # fails with "missing value where TRUE/FALSE needed"); it also plainly has
  # no changepoint.
  if (any(constant_cols(as.matrix(input)))) {
    return(ggcpt_build(data_vec, integer(0), method = "sn",
                       change_in = change_lab,
                       penalty = list(type = "confidence", value = confidence),
                       call = match.call(),
                       data_wide = if (parameter == "bivcor") {
                         mv_data_wide(as.matrix(input))
                       }))
  }

  # Below roughly 20 observations there is no room for the nested local
  # windows and the engine fails with "only 0's may be mixed with negative
  # subscripts"; say what is actually wrong.
  fit <- tryCatch(
    SNSeg::SNSeg_Uni(ts = input, paras_to_test = parameter,
                     confidence = confidence, grid_size = grid_size,
                     plot_SN = FALSE, ...),
    error = function(e) {
      if (grepl("only 0's may be mixed with negative subscripts",
                conditionMessage(e), fixed = TRUE)) {
        stop("`sn` needs a longer series: ", length(data_vec),
             " observations leave no room for the self-normalisation ",
             "windows (about 20 are needed at the default `grid_size`).",
             call. = FALSE)
      }
      stop(e)
    }
  )

  ggcpt_build(
    data_vec, as.integer(fit$est_cp),
    method = "sn",
    change_in = change_lab,
    penalty = list(type = "confidence", value = confidence),
    fit = fit,
    call = match.call(),
    data_wide = if (parameter == "bivcor") mv_data_wide(input)
  )
}
