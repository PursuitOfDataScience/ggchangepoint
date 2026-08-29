# Internal: did `mcp` actually sample? An `mcpfit` built without JAGS still
# looks like a fit -- it carries the model, the priors and the JAGS code, and
# only the posterior is missing.
#' @noRd
mcp_has_samples <- function(fit) {
  post <- if (is.list(fit)) fit[["mcmc_post", exact = TRUE]] else NULL
  !is.null(post) && length(post) > 0 && NROW(post[[1]]) > 0
}

#' Bayesian formula-based changepoint regression (mcp)
#'
#' Wraps \code{mcp::mcp()} (Lindeløv): a Bayesian multiple-changepoint
#' regression specified as a \emph{list of formulas}, one per segment. This
#' is the most expressive detector in the package — each segment can have its
#' own intercept, slope, variance and autocorrelation, and the changepoints
#' themselves get full posterior distributions rather than point estimates,
#' which \code{\link{ggcpt_posterior}()} already knows how to draw.
#'
#' @param x A numeric vector.
#' @param change_in Shorthand for the segment model when \code{model} is not
#'   given: \code{"mean"} fits a change in intercept
#'   (\code{list(y ~ 1, ~ 1)}), \code{"slope"} a change in slope
#'   (\code{list(y ~ 1 + t, ~ 0 + t)}), \code{"var"} a change in residual
#'   standard deviation (\code{list(y ~ 1, ~ 0 + sigma(1))}).
#' @param n_changepoints Number of changepoints in the shorthand model.
#'   Defaults to \code{1}.
#' @param model An explicit \pkg{mcp} model: a list of formulas. Overrides
#'   \code{change_in} and \code{n_changepoints}, and is the reason to reach
#'   for this engine at all.
#' @param prior Optional named list of priors, passed to \code{mcp::mcp()}.
#' @param iter,adapt,chains Sampler settings, passed through.
#' @param seed Optional seed.
#' @param ... Additional arguments passed to \code{mcp::mcp()}.
#'
#' @section JAGS is a system dependency:
#' \pkg{mcp} samples through JAGS, a separate program installed outside R.
#' Having the \emph{package} is not the same as being able to \emph{run} it:
#' \pkg{mcp} imports \pkg{rjags}, and on some platforms \pkg{rjags}
#' installs happily and only fails when it looks for the JAGS library at run
#' time — in which case \code{mcp::mcp()} returns a fit carrying no
#' posterior samples, with a warning rather than an error. This wrapper
#' checks for that and says so plainly instead of failing several frames
#' later inside \code{summary()}. Everything else in the package works
#' without JAGS.
#'
#' @return A \code{ggcpt} object. The changepoints tibble carries the
#'   posterior mean location together with \code{ci_lower}/\code{ci_upper}
#'   from the posterior quantiles, and \code{$data$fitted} holds the
#'   posterior predictive mean, so \code{autoplot(show_ci = TRUE,
#'   show_fit = TRUE)} shows both.
#' @references
#' \insertRef{lindelov2020mcp}{ggchangepoint}
#' @export
#' @examples
#' # Not run by R CMD check: whether this works depends on a *system*
#' # library, and no test of installed R packages predicts that reliably --
#' # `rjags` can be present and still fail to find JAGS at run time.
#' \dontrun{
#' set.seed(2026)
#' fit <- mcp_wrapper(c(rnorm(60), rnorm(60, 4)), iter = 500, adapt = 200)
#' fit
#' }
mcp_wrapper <- function(x, change_in = c("mean", "slope", "var"),
                        n_changepoints = 1, model = NULL, prior = list(),
                        iter = 3000, adapt = 1000, chains = 3, seed = NULL,
                        ...) {
  # `mcp` imports `rjags`, so a successful requireNamespace("mcp") already
  # implies JAGS is present. Naming `rjags` here instead would be an
  # undeclared dependency, and declaring it would add a second package that
  # cannot install without the system library.
  if (!requireNamespace("mcp", quietly = TRUE)) {
    stop("Package 'mcp' is required, and it samples through JAGS -- a ",
         "separate program installed outside R. Install JAGS from ",
         "https://mcmc-jags.sourceforge.io and then ",
         "install.packages('mcp').", call. = FALSE)
  }
  change_in <- match.arg(change_in)
  validate_scalar(n_changepoints, "n_changepoints", min = 1)
  validate_data(x)
  data_vec <- as_uni_vector(x, "mcp")
  n <- length(data_vec)
  if (!is.null(seed)) set.seed(seed)

  df <- data.frame(t = seq_len(n), y = data_vec)
  if (is.null(model)) {
    seg2 <- switch(change_in,
      mean = ~ 1,
      slope = ~ 0 + t,
      var = ~ 0 + sigma(1)
    )
    first <- switch(change_in,
      mean = y ~ 1,
      slope = y ~ 1 + t,
      var = y ~ 1
    )
    model <- c(list(first),
               rep(list(seg2), as.integer(n_changepoints)))
  }

  # `mcp` derives the x-axis variable from the segment formulas, and the
  # default models here are plateau-only (`list(y ~ 1, ~ 1)`), so there is
  # nothing in them to derive it from: mcp::mcp() stops with "This is a
  # plateau-only model so no x-axis variable could be derived". The data
  # frame built above always calls that column `t`, so name it explicitly --
  # unless the caller already passed their own `par_x` through `...`.
  dots <- list(...)
  if (is.null(dots[["par_x", exact = TRUE]])) dots$par_x <- "t"
  fit <- do.call(mcp::mcp,
                 c(list(model, data = df, prior = prior, iter = iter,
                        adapt = adapt, chains = chains), dots))

  # `mcp` warns rather than errors when JAGS cannot be reached, and hands
  # back an `mcpfit` with no posterior samples; `summary()` on that dies with
  # "subscript out of bounds" several frames down. Checking the invariant --
  # a fit either has samples or it is not a fit -- is what makes this
  # reportable, and it holds however JAGS came to be unavailable.
  if (!mcp_has_samples(fit)) {
    stop("`mcp` returned a fit with no posterior samples, which means JAGS ",
         "could not be reached. The 'mcp' package is installed, but JAGS ",
         "itself is a separate program: install it from ",
         "https://mcmc-jags.sourceforge.io and make sure 'rjags' can load ",
         "(`requireNamespace(\"rjags\")`).", call. = FALSE)
  }

  # The changepoint parameters are named cp_1, cp_2, ...
  smry <- as.data.frame(summary(fit))
  cp_rows <- grep("^cp_", smry$name)
  if (length(cp_rows) == 0) {
    return(ggcpt_build(data_vec, integer(0), method = "mcp",
                       change_in = change_in,
                       penalty = list(type = "posterior", value = NA_real_),
                       fit = fit, call = match.call()))
  }
  cp <- as.integer(round(smry$mean[cp_rows]))
  lo <- as.integer(floor(smry$lower[cp_rows]))
  hi <- as.integer(ceiling(smry$upper[cp_rows]))
  ord <- order(cp)

  fitted <- tryCatch({
    pr <- stats::fitted(fit)
    as.numeric(pr$fitted %||% pr$estimate)[seq_len(n)]
  }, error = function(e) NULL)
  if (!is.null(fitted) && length(fitted) != n) fitted <- NULL

  ggcpt_build(
    data_vec, cp[ord],
    method = "mcp",
    change_in = change_in,
    penalty = list(type = "posterior", value = NA_real_),
    fit = fit,
    call = match.call(),
    extra_cp_cols = list(
      ci_lower = pmax(1L, pmin(lo[ord], n - 1L)),
      ci_upper = pmax(1L, pmin(hi[ord], n - 1L))
    ),
    fitted = fitted
  )
}
