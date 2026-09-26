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
#' @section Result size:
#' \code{$fit} is the \code{breakpoints} object itself, and that object is
#' quadratic in the series length: it keeps \code{RSS.triang}, the triangular
#' table of segment residual sums of squares, which is what lets
#' \code{strucchange} return the optimal segmentation for \emph{any} number
#' of breaks without refitting. Measured here, the whole result is about
#' 1.7 MB at \code{n = 200}, 5.9 MB at \code{n = 400} and 22.6 MB at
#' \code{n = 800}, roughly four times larger each time the series doubles,
#' and that one table outweighs everything else in the fit put together, by a
#' margin that widens as the series grows. A single fit is not a problem; a
#' few hundred of them are, so when running this engine over a panel with
#' \code{\link{cpt_batch}()} keep what you need
#' (\code{res$changepoints}) rather than the whole list of results. No other
#' engine here behaves this way: the median result across the others is
#' under ten times the size of the series it was given.
#' @references
#' \insertRef{bai2003computation}{ggchangepoint}
#'
#' \insertRef{zeileis2002strucchange}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("strucchange", quietly = TRUE)
#' set.seed(2026)
#' res <- strucchange_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
#' @family changepoint engines
strucchange_wrapper <- function(x, data = NULL, breaks = NULL, h = 0.15,
                                conf_level = 0.95, ...) {
  need_pkg("strucchange")
  reject_renamed_args(list(...), "strucchange")
  # A confidence level outside (0, 1) is meaningless, and `level = 2` makes
  # stats::confint() on a breakpoints fit spin without ever returning -- a
  # tryCatch() cannot rescue a call that does not terminate, so it has to be
  # refused up front.
  validate_scalar(conf_level, "conf_level", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  # A fraction of the series below 1, a number of observations from 1 up.
  # `h = NA` failed with "missing value where TRUE/FALSE needed" and a
  # vector with "the condition has length > 1".
  validate_scalar(h, "h", min = 0, min_open = TRUE)
  # `breaks = 0` warned "number of breaks must be at least 1" and then fitted
  # one, `2.5` failed with "compute RSS.table with enough breaks before", and
  # `NA` or a vector failed without naming the argument.
  if (!is.null(breaks)) {
    validate_scalar(breaks, "breaks", min = 1)
    if (breaks != round(breaks)) {
      cpt_abort("`breaks` must be a whole number of breaks (got ", breaks, ").",
                class = "bad_argument")
    }
  }

  if (inherits(x, "formula")) {
    if (is.null(data)) {
      cpt_abort("`data` must be supplied when `x` is a formula.",
                class = "bad_argument")
    }
    response <- all.vars(x)[1]
    # The formula interface reads the response column straight out of `data`,
    # which as.numeric() would silently turn into level codes for a factor --
    # the one series-bearing path in the package that the type guard on `x`
    # cannot see, because here `x` is the formula.
    data_vec <- coerce_series_values(data[[response]], arg = response)
    fml <- x
  } else {
    validate_data(x)
    data_vec <- as_uni_vector(x, "strucchange")
    data <- data.frame(.y = data_vec)
    fml <- stats::as.formula(".y ~ 1")
  }

  args <- list(formula = fml, data = data, h = h, ...)
  if (!is.null(breaks)) args$breaks <- breaks
  fit <- engine_short_series(
    do.call(strucchange::breakpoints, args),
    "strucchange", length(data_vec),
    paste0("The minimum segment is `h` * n = ", h, " * ", length(data_vec),
           " = ", floor(h * length(data_vec)),
           " observations; raise `h` or lengthen the series."))

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
#' Called with a numeric vector it segments a line in time. Called with a
#' formula and \code{data} it does what the engine exists for: breakpoints
#' in the relationship between the response and a covariate,
#' \code{seg_z}. The result is then ordered by that covariate, which
#' becomes its index, so \code{tidy()}'s \code{cp_index} and the plot speak
#' in the covariate's units, and \code{psi}, \code{psi_lower} and
#' \code{psi_upper} give the breakpoints on that scale exactly.
#'
#' @param x A numeric vector (a line in time is segmented), or a model
#'   formula (supply \code{data}).
#' @param data A data frame, for formula input.
#' @param seg_z For formula input, the covariate whose relationship with
#'   the response breaks: its name, or a one-sided formula (\code{~ t}) as
#'   \pkg{segmented}'s own \code{seg.Z} takes it. Defaults to the formula's
#'   only numeric covariate, and must be named when there are several.
#' @param npsi Number of breakpoints to estimate. Defaults to \code{1}.
#' @param family \code{"gaussian"} (a linear model, the default),
#'   \code{"poisson"} or \code{"binomial"} (a generalised linear model on
#'   the log or logit scale).
#' @param conf_level Confidence level for breakpoint intervals. Defaults to
#'   \code{0.95}.
#' @param seed Optional seed (the estimator uses bootstrap restarting). The
#'   seed is scoped to this call: \code{.Random.seed} is saved and restored,
#'   so a seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Additional arguments passed to \code{segmented::segmented()},
#'   for example \code{psi} (starting values) or \code{fixed.psi}.
#' @return A \code{ggcpt} object with \code{ci_lower}/\code{ci_upper} columns
#'   and the fitted broken line in \code{$data$fitted}. Breakpoints are
#'   rounded to the nearest index; for a continuous fit the reported location
#'   is the kink itself. A constant series has no kink and returns an empty
#'   result, rather than the arbitrary breakpoint a singular fit would give.
#'   Formula input adds \code{psi}, \code{psi_lower} and \code{psi_upper}
#'   (the breakpoints on the covariate's scale) and a \code{$coefficients}
#'   table with the slope of \code{seg_z} in each segment.
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
#'
#' # A breakpoint in a dose-response relationship
#' d <- data.frame(dose = runif(150, 0, 10))
#' d$response <- 2 + 0.8 * pmin(d$dose, 6) + rnorm(150, 0, 0.4)
#' fit <- segmented_wrapper(response ~ dose, data = d)
#' fit$changepoints[, c("cp", "psi", "psi_lower", "psi_upper")]
#' @family changepoint engines
segmented_wrapper <- function(x, npsi = 1, conf_level = 0.95, seed = NULL,
                              data = NULL, seg_z = NULL,
                              family = c("gaussian", "poisson", "binomial"),
                              ...) {
  need_pkg("segmented")
  reject_renamed_args(list(...), "segmented")
  family <- cpt_match_arg(family)
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(npsi, "npsi", min = 1)
  validate_scalar(conf_level, "conf_level", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  fam_obj <- switch(family, gaussian = stats::gaussian(),
                    poisson = stats::poisson(), binomial = stats::binomial())

  if (inherits(x, "formula")) {
    return(segmented_formula(x, data, seg_z, npsi, family, fam_obj,
                             conf_level, seed, match.call(), ...))
  }
  if (!is.null(data) || !is.null(seg_z)) {
    cpt_abort("`data` and `seg_z` are for formula input: ",
              "`segmented_wrapper(y ~ x, data = d)`.", class = "bad_argument")
  }

  validate_data(x)
  data_vec <- as_uni_vector(x, "segmented")
  if (family != "gaussian") check_family_data(data_vec, family, "segmented")
  # Every segment of a broken line needs two points for its slope. Past
  # that the engine either refuses with "psi starting values too close each
  # other" or, for `npsi = 1e6` on 120 observations, runs without returning.
  max_psi <- floor(length(data_vec) / 2) - 1
  if (npsi > max_psi) {
    cpt_abort("`npsi = ", format(npsi), "` asks for more breakpoints than a ",
               "series ", "of ", length(data_vec), " can hold: each of the ",
               "npsi + 1 segments ", "needs two observations, so at most ",
              max_psi, ".", class = "bad_argument")
  }

  # A flat line has no kink. Left to itself the estimator returns an
  # arbitrary breakpoint from a singular fit (with Lapack warnings), i.e. a
  # spurious changepoint on data that plainly has none.
  if (is_constant(data_vec)) {
    return(ggcpt_build(data_vec, integer(0), method = "segmented",
                       change_in = "slope",
                       penalty = list(type = "npsi", value = npsi),
                       call = match.call(), fitted = data_vec))
  }

  df <- data.frame(.y = data_vec, .t = seq_along(data_vec))

  local_seed(seed)

  base_fit <- if (family == "gaussian") {
    stats::lm(.y ~ .t, data = df)
  } else {
    stats::glm(.y ~ .t, data = df, family = fam_obj)
  }
  fit <- segmented::segmented(base_fit, seg.Z = ~.t, npsi = npsi, ...)

  if (!inherits(fit, "segmented") || is.null(fit$psi)) {
    return(ggcpt_build(
      data_vec, integer(0),
      method = "segmented", change_in = "slope",
      penalty = list(type = "npsi", value = npsi),
      fit = fit, call = match.call()
    ))
  }
  attr(fit, "ggcpt_base") <- base_fit

  psi <- fit$psi[, "Est."]
  # `round()`, not the truncation as_cp_locations() and as_ggcpt() use: a
  # `segmented` breakpoint is a KINK POSITION estimated on the continuous
  # scale, so the nearest observation is the honest reading of it, where
  # truncating would bias every breakpoint left by half an observation on
  # average. It is the one place in the package that rounds rather than
  # truncates a fractional location, so @return says so.
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

  res <- ggcpt_build(
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
  if (family != "gaussian") res$family <- family
  res$coefficients <- segmented_coefficients(fit, ".t", c(.t = "time"),
                                             res$changepoints$cp,
                                             length(data_vec), conf_level)
  res
}

# Internal: the formula route of segmented_wrapper(). The rows are put in
# the order of the segmentation covariate, which becomes the result's
# index: a breakpoint in a dose-response curve is a dose, not a time.
#' @noRd
segmented_formula <- function(formula, data, seg_z, npsi, family, fam_obj,
                              conf_level, seed, call, ...) {
  if (is.null(data) || !is.data.frame(data)) {
    cpt_abort("`data` must be supplied as a data frame when `x` is a ",
              "formula.", class = "bad_argument")
  }
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  y <- stats::model.response(mf)
  refuse_special_values(y, "response")
  y <- coerce_series_values(y, arg = "response")
  if (family != "gaussian") check_family_data(y, family, "segmented")
  term_labels <- attr(stats::terms(mf), "term.labels")
  numeric_terms <- term_labels[vapply(term_labels, function(t) {
    t %in% names(mf) && is.numeric(mf[[t]])
  }, logical(1))]
  # segmented's own spelling is a one-sided formula, `seg.Z = ~ t`; a user
  # who knows the engine writes it that way.
  if (inherits(seg_z, "formula")) {
    zv <- all.vars(seg_z)
    seg_z <- if (length(zv) == 1L) zv else seg_z
  }
  if (is.null(seg_z)) {
    if (length(numeric_terms) != 1L) {
      cpt_abort("The formula has ", length(numeric_terms), " numeric ",
                "covariates", if (length(numeric_terms)) {
                  paste0(" (", paste(numeric_terms, collapse = ", "), ")")
                } else "", "; name the one whose relationship breaks with ",
                "`seg_z`.", class = "bad_argument")
    }
    seg_z <- numeric_terms
  }
  if (!is.character(seg_z) || length(seg_z) != 1L ||
      !seg_z %in% numeric_terms) {
    cpt_abort("`seg_z` must name one numeric covariate of the formula (",
              paste(numeric_terms, collapse = ", "), ").",
              class = "bad_argument")
  }
  ord <- order(mf[[seg_z]])
  mf <- mf[ord, , drop = FALSE]
  y <- y[ord]
  z <- mf[[seg_z]]
  n <- length(y)
  max_psi <- floor(n / 2) - 1
  if (npsi > max_psi) {
    cpt_abort("`npsi = ", format(npsi), "` asks for more breakpoints than ",
              n, " rows can hold: at most ", max_psi, ".",
              class = "bad_argument")
  }
  local_seed(seed)
  model_data <- mf
  names(model_data)[1] <- ".y"
  rhs <- paste(deparse(formula[[3]]), collapse = "")
  base_formula <- stats::as.formula(paste(".y ~", rhs))
  environment(base_formula) <- environment(formula)
  base_fit <- if (family == "gaussian") {
    stats::lm(base_formula, data = model_data)
  } else {
    stats::glm(base_formula, data = model_data, family = fam_obj)
  }
  fit <- segmented::segmented(base_fit,
                              seg.Z = stats::as.formula(paste("~", seg_z)),
                              npsi = npsi, ...)
  if (!inherits(fit, "segmented") || is.null(fit$psi)) {
    res <- ggcpt_build(y, integer(0), method = "segmented",
                       change_in = "regression",
                       penalty = list(type = "npsi", value = npsi),
                       fit = fit, call = call)
    return(attach_index(res, z, seg_z))
  }
  attr(fit, "ggcpt_base") <- base_fit
  psi <- as.numeric(fit$psi[, "Est."])
  # A breakpoint on the covariate's scale sits between two ordered rows:
  # the changepoint is the last row at or below it.
  cp_indices <- vapply(psi, function(p) sum(z <= p), numeric(1))
  ci <- tryCatch(as.matrix(segmented::confint.segmented(fit,
                                                        level = conf_level)),
                 error = function(e) NULL)
  extra <- list(psi = psi)
  if (!is.null(ci) && nrow(ci) == length(psi) && ncol(ci) >= 3) {
    extra$psi_lower <- as.numeric(ci[, 2])
    extra$psi_upper <- as.numeric(ci[, 3])
    extra$ci_lower <- pmax(1L, vapply(ci[, 2], function(p) sum(z <= p),
                                      numeric(1)))
    extra$ci_upper <- pmin(n - 1L, vapply(ci[, 3], function(p) sum(z <= p),
                                          numeric(1)))
  }
  res <- ggcpt_build(y, as.integer(cp_indices), method = "segmented",
                     change_in = "regression",
                     penalty = list(type = "npsi", value = npsi), fit = fit,
                     call = call, extra_cp_cols = extra,
                     fitted = as.numeric(stats::fitted(fit)))
  res$coefficients <- segmented_coefficients(fit, seg_z,
                                             stats::setNames(seg_z, seg_z),
                                             res$changepoints$cp, n,
                                             conf_level)
  res$regression <- list(formula = formula, response = names(mf)[1],
                         seg_z = seg_z, family = family, row_order = ord)
  if (family != "gaussian") res$family <- family
  attach_index(res, z, seg_z)
}

# Internal: the per-segment intercepts and slopes of a segmented fit, in
# the coefficient-table shape (segment, start, end, term, estimate,
# std_error, conf_low, conf_high). The slope of the segmentation variable
# changes at each breakpoint and has intervals; the intercepts are implied
# by continuity and have none.
#' @noRd
segmented_coefficients <- function(fit, var, label, cps, n, conf_level) {
  sl <- tryCatch(segmented::slope(fit, conf.level = conf_level)[[var]],
                 error = function(e) NULL)
  ic <- tryCatch(segmented::intercept(fit)[[var]], error = function(e) NULL)
  if (is.null(sl)) return(NULL)
  k <- nrow(sl)
  bounds <- c(0L, sort(cps), n)
  if (length(bounds) != k + 1L) bounds <- c(0L, rep(NA_integer_, k - 1L), n)
  starts <- bounds[-length(bounds)] + 1L
  ends <- bounds[-1L]
  slope_rows <- tibble::tibble(
    segment = seq_len(k), start = starts, end = ends,
    term = unname(label[var]), estimate = as.numeric(sl[, 1]),
    std_error = as.numeric(sl[, 2]),
    conf_low = as.numeric(sl[, ncol(sl) - 1L]),
    conf_high = as.numeric(sl[, ncol(sl)]))
  if (is.null(ic) || nrow(ic) != k) return(slope_rows)
  int_rows <- tibble::tibble(
    segment = seq_len(k), start = starts, end = ends,
    term = "(Intercept)", estimate = as.numeric(ic[, 1]),
    std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_)
  out <- rbind(int_rows, slope_rows)
  out[order(out$segment, out$term != "(Intercept)"), , drop = FALSE]
}

#' EnvCpt wrapper: changepoints versus trends versus autocorrelation
#'
#' Wraps \code{EnvCpt::envcpt()} (Beaulieu and Killick, 2018), which fits up
#' to twelve competing models (constant mean or linear trend, each with or
#' without changepoints, and with white-noise, AR(1) or AR(2) errors) and
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
#'   printed by \code{glance()} via \code{penalty_type}. Individual model
#'   fits that fail are expected (the criterion ignores them), so the
#'   engine's own \code{try()} output is not passed on; genuine warnings
#'   still are, and a series on which no model fits at all raises an error.
#'   A constant series has no changepoints and is not handed to the engine,
#'   so \code{$fit} is \code{NULL} there.
#' @references
#' \insertRef{beaulieu2018envcpt}{ggchangepoint}
#' @export
#' @examplesIf requireNamespace("EnvCpt", quietly = TRUE)
#' set.seed(2026)
#' res <- envcpt_wrapper(c(rnorm(100), rnorm(100, 3)))
#' res$changepoints
#' @family changepoint engines
envcpt_wrapper <- function(x, models = c("mean", "meancpt", "meanar1",
                                         "meanar2", "meanar1cpt",
                                         "meanar2cpt", "trend", "trendcpt",
                                         "trendar1", "trendar2",
                                         "trendar1cpt", "trendar2cpt"),
                           criterion = c("AIC", "BIC"), minseglen = 5, ...) {
  need_pkg("EnvCpt")
  # Forwarded to the engine, which reported a bad value from deep inside
  # itself -- "missing value where TRUE/FALSE needed", "negative length
  # vectors are not allowed", "NAs in foreign function call" and the like,
  # none of which names the argument. Measured across all 64 wrapper
  # argument slots; these are the ones that needed it.
  validate_scalar(minseglen, "minseglen", min = 2)
  reject_managed_args(list(...), "envcpt", c(
    verbose = paste("the wrapper keeps the engine's \"Fitting 12 models\"",
                    "narration and its progress bar out of the result")))
  criterion <- cpt_match_arg(criterion)

  validate_data(x)
  data_vec <- as_uni_vector(x, "envcpt")

  # A constant series plainly has no changepoints, and the engine fits its
  # twelve models to it anyway: every `lm()` in there is an exact fit, so
  # `summary.lm()` warned "essentially perfect fit: summary may be
  # unreliable" twice per call -- the only two warnings the whole test suite
  # raised. Advice about an internal regression the caller never ran. Every
  # other wrapper that meets a flat series reports none; so does this one.
  if (is_constant(data_vec)) {
    return(ggcpt_build(data_vec, integer(0), method = "envcpt",
                       change_in = "mean",
                       penalty = list(type = criterion, value = NA_real_),
                       call = match.call()))
  }

  # EnvCpt fits up to twelve models with try(), and a try() that is not
  # silent prints its error straight to stderr, where it reads as a failure
  # even though the call succeeded and the criterion simply ignores the
  # non-finite fits (a run where nothing fits gets its own error below).
  #
  # Re-measured: the cited example -- "Error in arima(...): non-stationary
  # AR part from CSS" -- would not reproduce on any of eight series chosen
  # to provoke it (a random walk, an explosive AR(1), a doubly-integrated
  # series, one scaled to 1e6, a numerically-constant one, an exact step,
  # and two ordinary two-segment series): zero stderr lines each. What the
  # engine does still write to stderr is its own "Fitting 12 models"
  # narration, and that is already off because this call passes
  # `verbose = FALSE`. So the diversion is now a cheap safety net rather
  # than a fix for an observed leak -- kept because an upstream try() that
  # stops being silent would otherwise print into the caller's console,
  # and because it costs nothing.
  #
  # What the diversion must NOT do is swallow real conditions, and it does
  # not: a genuine warning ("possible convergence problem: optim gave
  # code = 1", from a trending series) is deferred past the diversion and
  # still reaches the user.
  utils::capture.output(
    fit <- engine_short_series(
      EnvCpt::envcpt(data_vec, models = models, minseglen = minseglen,
                     verbose = FALSE, ...),
      "envcpt", length(data_vec),
      paste0("Every model must fit two segments of `minseglen` = ",
             minseglen, ", so the series needs more than ", 2 * minseglen,
             " observations; lower `minseglen` or lengthen the series.")),
    type = "message"
  )

  crit_vals <- if (criterion == "AIC") stats::AIC(fit) else stats::BIC(fit)
  crit_vals <- crit_vals[is.finite(crit_vals)]
  if (length(crit_vals) == 0) {
    cpt_abort("envcpt did not successfully fit any of the requested models.",
              class = "engine_error")
  }
  best <- names(which.min(crit_vals))

  # The autoregressive changepoint models fit a regression on lagged values,
  # so their locations are rows of a design that starts `p` observations
  # into the series: measured, `meanar1cpt` put a change at 100 at 99 and
  # `meanar2cpt` at 98, and each model also reports the design's last row
  # as a changepoint, which is not one. Shifted back by the lag, and the
  # end marker dropped.
  lag <- if (grepl("ar2", best)) 2L else if (grepl("ar1", best)) 1L else 0L
  cp_indices <- if (grepl("cpt", best)) {
    tryCatch({
      cps <- as.integer(changepoint::cpts(fit[[best]]))
      # NROW(): a mean model's data.set is the series itself, a vector.
      rows <- tryCatch(NROW(changepoint::data.set(fit[[best]])),
                       error = function(e) length(data_vec))
      cps <- cps[cps < rows]
      cps + lag
    }, error = function(e) integer(0))
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
    call = match.call(),
    fitted = envcpt_fitted(fit[[best]], best, data_vec, lag)
  )
}

# Internal: the winning EnvCpt model's fitted signal, on the series'
# positions (the lagged models' first `lag` values have no fit and are NA).
# NULL when the model's form is not one of the eight EnvCpt returns.
#' @noRd
envcpt_fitted <- function(model, name, y, lag) {
  n <- length(y)
  out <- tryCatch({
    if (identical(name, "mean")) {
      rep(mean(y), n)
    } else if (inherits(model, "Arima")) {
      as.numeric(y - stats::residuals(model))
    } else if (inherits(model, "lm")) {
      f <- as.numeric(stats::fitted(model))
      c(rep(NA_real_, n - length(f)), f)
    } else if (inherits(model, "cpt.reg")) {
      d <- changepoint::data.set(model)
      beta <- changepoint::param.est(model)$beta
      if (is.null(dim(beta))) beta <- matrix(beta, nrow = 1)
      rows <- nrow(d)
      cps <- as.integer(changepoint::cpts(model))
      cps <- cps[cps < rows]
      bounds <- c(0L, cps, rows)
      f <- numeric(rows)
      X <- d[, -1, drop = FALSE]
      for (k in seq_len(length(bounds) - 1L)) {
        r <- (bounds[k] + 1L):bounds[k + 1L]
        f[r] <- X[r, , drop = FALSE] %*% beta[min(k, nrow(beta)), ]
      }
      c(rep(NA_real_, n - rows), f)
    } else if (inherits(model, "cpt")) {
      means <- changepoint::param.est(model)$mean
      cps <- as.integer(changepoint::cpts(model))
      bounds <- c(0L, cps[cps < n], n)
      rep(means[seq_len(length(bounds) - 1L)], diff(bounds))
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (!is.null(out) && length(out) != n) NULL else out
}
