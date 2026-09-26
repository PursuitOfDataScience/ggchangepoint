# ---------------------------------------------------------------------------
# Regression breakpoints: the formula route of cpt_detect(), coefficients
# per segment, and the segment-model layer (cpt_segment_models(),
# predict()).
#
# The two regression engines are the package's two most downloaded, and
# until 0.6.0 the dispatcher could reach neither in its regression form:
# `cpt_detect(y ~ x, data = d, method = "strucchange")` died in
# as.numeric(), and segmented_wrapper() hard-coded a line in time, which
# throws away the point of an engine built to find breakpoints in the
# relationship between a response and a covariate. A breakpoint in a model
# is a change in its coefficients, so the result now carries them.
# ---------------------------------------------------------------------------

#' @noRd
detect_regression <- function(spec, method, change_in, penalty, family,
                              na_action, fixed, within, min_segment,
                              min_effect, keep_fit, user_call, ...) {
  dots <- list(...)
  fm <- formula_methods()
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    cpt_abort("`method` must be one method name.", class = "bad_argument")
  }
  if (!is.null(registry_get(method))) {
    cpt_abort("`", method, "` is a registered method, which is handed a ",
              "series. A formula with covariates needs one of ",
              paste(fm, collapse = ", "), "; for the response alone, write `",
              spec$response, " ~ 1`.", class = "unsupported",
              data = list(method = method, requested = "formula",
                          supported = fm))
  }
  builtin <- builtin_registry()$method
  method <- cpt_match_arg(method, builtin, class = "unknown_method",
                          hint = "Run `cpt_methods()` for the full list.")
  if (!method %in% fm) {
    cpt_abort("Method `", method, "` detects changes in a series, not in a ",
              "regression, so it cannot use the covariates in the formula. ",
              "With covariates use one of ", paste(fm, collapse = ", "),
              "; for the response alone, write `", spec$response, " ~ 1`.",
              class = "unsupported",
              data = list(method = method, requested = "formula",
                          supported = fm))
  }
  change_in <- cpt_match_arg(change_in, cpt_change_in_levels())
  if (!change_in %in% c("mean", "regression")) {
    cpt_abort("A formula with covariates detects a change in the ",
              "regression's coefficients: `change_in = \"regression\"` (the ",
              "default \"mean\" is read the same way), not \"", change_in,
              "\".", class = "unsupported",
              data = list(method = method, requested = change_in,
                          supported = "regression"))
  }
  if (!is.null(min_effect)) {
    cpt_abort("`min_effect` filters a shift in a mean; a regression result ",
              "reports changes in coefficients. Read them with `tidy(fit, ",
              "\"coefficients\")`.", class = "unsupported",
              data = list(method = method, requested = "min_effect",
                          supported = character(0)))
  }
  # `seed` as in cpt_detect(): scoped around the fit when the wrapper has
  # no argument of its own for it.
  wrapper <- builtin_registry()$wrapper[match(method, builtin)]
  if ("seed" %in% names(dots) &&
      !"seed" %in% names(formals(get(wrapper, asNamespace("ggchangepoint"))))) {
    seed_arg <- dots$seed
    dots$seed <- NULL
    local_seed(seed_arg)
  }
  fam <- resolve_family(method, "regression", family %||% "gaussian")

  y <- spec$y
  X <- spec$X
  n <- length(y)
  if (any(is.infinite(y)) || any(is.infinite(X))) {
    cpt_abort("The response and covariates must be finite; ",
              sum(is.infinite(y)) + sum(is.infinite(X)),
              " infinite value(s) found.", class = "non_finite")
  }
  incomplete <- is.na(y) | !stats::complete.cases(X)
  na_info <- NULL
  if (any(incomplete)) {
    if (na_action == "error") {
      cpt_abort(sum(incomplete), " of ", n, " rows have a missing response ",
                "or covariate. Pass `na_action = \"omit\"` to drop them, ",
                "detect, and report locations in the original rows.",
                class = "non_finite",
                data = list(n_missing = sum(incomplete)))
    }
    if (na_action == "engine") check_engine_na(method, NULL)
    keep <- !incomplete
    na_info <- list(keep = keep, pos = which(keep), n = n, full = y)
    y <- y[keep]
    X <- X[keep, , drop = FALSE]
  }
  n_fit <- length(y)
  if (n_fit < max(3L, ncol(X) + 2L)) {
    cpt_abort("The regression has ", ncol(X), " coefficient(s) and ", n_fit,
              " complete row(s): too few to fit one segment, let alone ",
              "compare two.", class = "short_series",
              data = list(n = n_fit, method = method))
  }
  check_family_data(y, fam$family, method)

  if (method == "segmented") {
    if (!is.null(fixed) || !is.null(within) || !is.null(min_segment)) {
      cpt_abort("For `segmented` the breakpoint lives on the covariate's ",
                "scale, not in time, so `fixed`, `within` and `min_segment` ",
                "(positions) do not apply. Pass segmented's own `psi` ",
                "(starting values) or `fixed.psi` through `...`.",
                class = "unsupported",
                data = list(method = method,
                            requested = "fixed/within/min_segment",
                            supported = character(0)))
    }
    res <- do.call(segmented_wrapper,
                   c(list(spec$formula, data = spec$vars_data,
                          family = fam$family), dots))
    res$call <- user_call
    res$regression$terms <- colnames(X)
    res$family <- fam$family
    if (!is.null(na_info)) res$diagnostics$na_omitted <- list(
      positions = which(!na_info$keep), n_observed = length(na_info$pos))
    if (!keep_fit) res$fit <- NULL
    return(res)
  }

  keep_mask <- if (!is.null(na_info)) na_info$keep else NULL
  to_fit_positions <- function(p) {
    if (is.null(p) || is.null(keep_mask)) return(p)
    as.integer(pmax(1, pmin(to_compact_positions(p, keep_mask), n_fit - 1L)))
  }
  fixed_pos <- to_fit_positions(constraint_positions(fixed, spec$index, n,
                                                     "fixed"))
  windows <- within_windows(within, spec$index, n)
  if (!is.null(windows) && !is.null(keep_mask)) {
    windows[] <- to_fit_positions(windows)
  }

  run_piece <- function(rows) {
    fit_regression_engine(method, y[rows], X[rows, , drop = FALSE], fam,
                          penalty, min_segment, dots)
  }
  t0 <- proc.time()[["elapsed"]]
  if (!is.null(fixed_pos)) {
    parts <- detect_fixed(seq_len(n_fit), fixed_pos, function(rows) {
      run_piece(rows)
    })
    cps <- parts$changepoints
    pieces <- parts$pieces
  } else {
    one <- run_piece(seq_len(n_fit))
    cps <- one$changepoints
    pieces <- list(one)
  }
  template <- run_template(pieces, method, "regression")
  res <- ggcpt_build(
    y, cps$cp, method = method, change_in = "regression",
    penalty = template$penalty, call = user_call,
    fit = if (is.null(fixed_pos)) pieces[[1]]$fit,
    extra_cp_cols = as.list(cps[, setdiff(names(cps), c("cp", "cp_value")),
                                drop = FALSE]))
  if (!is.null(fixed_pos)) {
    res$pieces <- pieces
    res$constraints <- list(fixed = fixed_pos)
  }
  if (!is.null(windows)) {
    res$constraints$within <- windows
    res <- apply_within(res, windows)
  }
  coefs <- segment_coefficients(y, X, res$changepoints$cp, fam$family)
  res$coefficients <- coefs
  res$data$fitted <- piecewise_fitted(y, X, res$changepoints$cp, fam$family)
  res$regression <- list(formula = spec$formula, response = spec$response,
                         terms = colnames(X), family = fam$family, X = X)
  res$family <- fam$family
  res$runtime <- proc.time()[["elapsed"]] - t0
  if (!is.null(na_info)) res <- restore_missing(res, na_info)
  res <- attach_index(res, spec$index, spec$index_label)
  warn_implausible_count(res)
  if (!keep_fit) res$fit <- NULL else warn_large_fit(res)
  res
}

# Internal: the model matrix as a data frame an engine's formula can read,
# with syntactic names and the response last, plus the formula
# `.y ~ . - 1` (the intercept, if any, is a column of the matrix).
#' @noRd
design_frame <- function(y, X) {
  nm <- make.names(colnames(X), unique = TRUE)
  nm[nm == ".y"] <- ".y.1"
  data <- as.data.frame(X, optional = TRUE)
  names(data) <- nm
  data$.y <- y
  list(data = data, formula = stats::as.formula(".y ~ . - 1"),
       names = stats::setNames(colnames(X), nm))
}

#' @noRd
fit_regression_engine <- function(method, y, X, fam, penalty, min_segment,
                                  dots) {
  if (method == "strucchange") {
    df <- design_frame(y, X)
    h <- if (!is.null(min_segment)) as.integer(min_segment) else
      dots$h %||% 0.15
    args <- c(list(df$formula, data = df$data, h = h),
              dots[setdiff(names(dots), "h")])
    return(do.call(strucchange_wrapper, args))
  }
  # fastcpd: its own penalty `beta`, translated as for a series.
  args <- list(y, family = fam$args$family, covariates = X)
  if (is.numeric(penalty)) {
    args$beta <- as.numeric(penalty)[1]
  } else if (is.character(penalty) && length(penalty) == 1L &&
             toupper(penalty) %in% c("MBIC", "BIC", "SIC", "MDL")) {
    args$beta <- if (identical(toupper(penalty), "SIC")) "BIC" else
      toupper(penalty)
  }
  args <- utils::modifyList(args, dots)
  if (!is.null(min_segment)) {
    cpt_abort("Method `fastcpd` has no minimum-segment argument, so ",
              "`min_segment` cannot be passed to it.", class = "unsupported")
  }
  do.call(fastcpd_wrapper, args)
}

# Internal: coefficients per segment, refitted by least squares (or by
# maximum likelihood for a Poisson or binomial family) on each segment's
# rows, with standard errors and Wald intervals. A coefficient the segment
# cannot identify (a covariate constant within it, or more coefficients
# than rows) is NA rather than an error.
#' @noRd
segment_coefficients <- function(y, X, cps, family = "gaussian",
                                 conf_level = 0.95) {
  bounds <- c(0L, sort(cps), length(y))
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  out <- lapply(seq_len(length(bounds) - 1L), function(s) {
    rows <- (bounds[s] + 1L):bounds[s + 1L]
    ys <- y[rows]
    Xs <- X[rows, , drop = FALSE]
    ok <- !is.na(ys) & stats::complete.cases(Xs)
    ys <- ys[ok]
    Xs <- Xs[ok, , drop = FALSE]
    terms <- colnames(X)
    est <- se <- rep(NA_real_, length(terms))
    crit <- z_crit
    if (length(ys) > 0L) {
      fit <- tryCatch(suppressWarnings(
        if (identical(family, "gaussian") || is.null(family)) {
          stats::lm.fit(Xs, ys)
        } else {
          stats::glm.fit(Xs, ys, family = switch(family,
            poisson = stats::poisson(), binomial = stats::binomial(),
            stats::gaussian()))
        }), error = function(e) NULL)
      if (!is.null(fit)) {
        est <- unname(fit$coefficients)
        p <- fit$rank
        piv <- fit$qr$pivot[seq_len(p)]
        R <- fit$qr$qr[seq_len(p), seq_len(p), drop = FALSE]
        R[lower.tri(R)] <- 0
        inv <- tryCatch(chol2inv(R), error = function(e) NULL)
        if (!is.null(inv)) {
          if (identical(family, "gaussian") || is.null(family)) {
            df_res <- length(ys) - p
            if (df_res > 0) {
              sigma2 <- sum(fit$residuals^2) / df_res
              se[piv] <- sqrt(diag(inv) * sigma2)
              crit <- stats::qt(1 - (1 - conf_level) / 2, df_res)
            }
          } else {
            # glm.fit's qr is of the weighted matrix, so its inverse is the
            # covariance with the dispersion fixed at 1.
            se[piv] <- sqrt(diag(inv))
          }
        }
        est[!is.finite(est)] <- NA_real_
      }
    }
    tibble::tibble(segment = s, start = bounds[s] + 1L, end = bounds[s + 1L],
                   term = terms, estimate = est, std_error = se,
                   conf_low = est - crit * se, conf_high = est + crit * se)
  })
  do.call(rbind, out)
}

# Internal: the piecewise model's fitted values, on the response scale.
#' @noRd
piecewise_fitted <- function(y, X, cps, family = "gaussian") {
  coefs <- segment_coefficients(y, X, cps, family)
  bounds <- c(0L, sort(cps), length(y))
  out <- rep(NA_real_, length(y))
  for (s in seq_len(length(bounds) - 1L)) {
    rows <- (bounds[s] + 1L):bounds[s + 1L]
    b <- coefs$estimate[coefs$segment == s]
    b[is.na(b)] <- 0
    eta <- as.numeric(X[rows, , drop = FALSE] %*% b)
    out[rows] <- switch(family %||% "gaussian",
      poisson = exp(eta),
      binomial = stats::plogis(eta),
      eta)
  }
  out
}

# Internal: the design a series fit implies for its segment coefficients:
# an intercept (the segment level) for a change in mean, and a line in time
# for a change in slope.
#' @noRd
implied_design <- function(object) {
  n <- nrow(object$data)
  if (identical(scalar_chr(object$change_in), "slope")) {
    X <- cbind(`(Intercept)` = 1, time = seq_len(n))
  } else {
    X <- matrix(1, nrow = n, ncol = 1, dimnames = list(NULL, "(Intercept)"))
  }
  X
}

# Internal: tidy(fit, "coefficients").
#' @noRd
tidy_coefficients <- function(x, conf_level = 0.95) {
  if (!is.null(x$coefficients) && is.null(x$regression$X) &&
      identical(conf_level, 0.95)) {
    return(x$coefficients)
  }
  if (!is.null(x$regression$X)) {
    y <- x$data$value
    X <- x$regression$X
    if (!is.null(x$na_map)) y <- y[x$na_map]
    cps <- x$changepoints$cp
    if (!is.null(x$na_map)) cps <- match(cps, x$na_map)
    out <- segment_coefficients(y, X, cps, x$regression$family, conf_level)
    if (!is.null(x$na_map)) {
      out$start <- x$na_map[out$start]
      out$end <- x$na_map[out$end]
    }
    return(out)
  }
  if (n_coordinates(x) > 1L) {
    w <- x$data_wide
    cols <- setdiff(names(w), c("index", "index_value"))
    out <- lapply(cols, function(cl) {
      tb <- segment_coefficients(w[[cl]], implied_design(x),
                                 x$changepoints$cp,
                                 x$family %||% "gaussian", conf_level)
      tibble::add_column(tb, coordinate = cl, .before = 1)
    })
    return(do.call(rbind, out))
  }
  fam <- x$family %||% "gaussian"
  if (!fam %in% c("gaussian", "poisson", "binomial")) fam <- "gaussian"
  segment_coefficients(x$data$value, implied_design(x), x$changepoints$cp,
                       fam, conf_level)
}

#' Fit a model to every segment of a segmentation
#'
#' Detection says where the regimes change; this fits \emph{your} model to
#' each regime. Every segment of \code{fit} gets its own copy of
#' \code{model}, fitted by \code{engine} to that segment's rows, and the
#' result is a table with one fitted model per segment that
#' \code{tidy()}, \code{glance()} and \code{predict()} read.
#'
#' @param fit A \code{ggcpt} object.
#' @param model A formula. For a result from a formula fit it defaults to
#'   that formula; for a series it defaults to \code{value ~ 1} (the
#'   segment level), or \code{value ~ time} for a change in slope. The
#'   series is available as \code{value}, its position as \code{time}, and
#'   the time index (when the result carries one) as \code{index}.
#' @param data Optional data frame with one row per observation, supplying
#'   covariates the model uses. For a formula fit the covariates are taken
#'   from the result.
#' @param engine The fitting function, called as \code{engine(model, data =
#'   segment_rows, ...)}. Defaults to \code{stats::lm}; \code{stats::glm}
#'   with a \code{family} works the same way.
#' @param ... Further arguments for \code{engine}.
#' @return A \code{ggcpt_segment_models} tibble with one row per segment:
#'   \code{segment}, \code{start}, \code{end}, \code{n} and \code{model} (a
#'   list-column of fitted models, \code{NULL} where the fit failed).
#'   \code{tidy()} gives the coefficient table with a \code{segment}
#'   column; \code{glance()} one row per segment with \code{n},
#'   \code{sigma}, \code{r_squared}, \code{AIC}, \code{BIC} and
#'   \code{logLik}.
#' @seealso \code{\link{predict.ggcpt}()} to forecast from a segment,
#'   \code{\link{tidy.ggcpt}(what = "coefficients")} for the coefficients
#'   the detection itself implies.
#' @export
#' @family segment models
#' @examplesIf requireNamespace("strucchange", quietly = TRUE)
#' set.seed(1)
#' d <- data.frame(x = runif(200))
#' d$y <- ifelse(seq_len(200) <= 100, 1 + 2 * d$x, 3 - d$x) + rnorm(200, 0, 0.3)
#' fit <- cpt_detect(y ~ x, data = d, method = "strucchange")
#' sm <- cpt_segment_models(fit)
#' tidy(sm)
#' glance(sm)
cpt_segment_models <- function(fit, model = NULL, data = NULL,
                               engine = stats::lm, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  if (!is.function(engine)) {
    cpt_abort("`engine` must be a model-fitting function such as stats::lm.",
              class = "bad_argument")
  }
  frame <- segment_model_frame(fit, data)
  if (is.null(model)) {
    model <- if (!is.null(fit$regression$formula)) {
      stats::as.formula(paste0(".y ~ ", paste(deparse(
        fit$regression$formula[[3]]), collapse = "")))
    } else if (identical(scalar_chr(fit$change_in), "slope")) {
      value ~ time
    } else {
      value ~ 1
    }
  }
  if (!inherits(model, "formula")) {
    cpt_abort("`model` must be a formula.", class = "bad_argument")
  }
  seg <- fit$segments
  models <- lapply(seq_len(nrow(seg)), function(s) {
    rows <- seg$start[s]:seg$end[s]
    tryCatch(engine(model, data = frame[rows, , drop = FALSE], ...),
             error = function(e) NULL)
  })
  out <- tibble::tibble(segment = seg$seg_id, start = seg$start,
                        end = seg$end, n = seg$n, model = models)
  structure(out, class = c("ggcpt_segment_models", class(out)),
            formula = model, method = scalar_chr(fit$method))
}

# Internal: the data a segment model is fitted on: the series, its
# position and index, the covariates of a formula fit, and any `data`.
#' @noRd
segment_model_frame <- function(fit, data = NULL) {
  n <- nrow(fit$data)
  frame <- data.frame(value = fit$data$value, time = seq_len(n))
  if (!is.null(fit$index)) frame$index <- fit$index
  if (!is.null(fit$regression)) {
    frame$.y <- fit$data$value
    if (!is.null(fit$regression$X)) {
      X <- fit$regression$X
      full <- matrix(NA_real_, nrow = n, ncol = ncol(X),
                     dimnames = list(NULL, colnames(X)))
      rows <- fit$na_map %||% seq_len(n)
      full[rows, ] <- X
      covs <- as.data.frame(full, optional = TRUE)
      names(covs) <- colnames(X)
      keep <- setdiff(names(covs), "(Intercept)")
      frame <- cbind(frame, covs[, keep, drop = FALSE])
    } else if (!is.null(fit$fit$model)) {
      mf <- fit$fit$model
      keep <- setdiff(names(mf), c(".y", names(frame)))
      if (nrow(mf) == n) frame <- cbind(frame, mf[, keep, drop = FALSE])
    }
  }
  if (!is.null(data)) {
    if (!is.data.frame(data) || nrow(data) != n) {
      cpt_abort("`data` must be a data frame with one row per observation ",
                "(", n, "); got ", if (is.data.frame(data)) nrow(data) else
                  class(data)[1], ".", class = "bad_argument")
    }
    frame <- cbind(frame, data[, setdiff(names(data), names(frame)),
                               drop = FALSE])
  }
  frame
}

#' @rdname cpt_segment_models
#' @param x A \code{ggcpt_segment_models} object.
#' @param conf_level Confidence level for the coefficient intervals.
#' @export
tidy.ggcpt_segment_models <- function(x, conf_level = 0.95, ...) {
  rows <- lapply(seq_len(nrow(x)), function(i) {
    m <- x$model[[i]]
    if (is.null(m)) return(NULL)
    tab <- tryCatch(stats::coef(summary(m)), error = function(e) NULL)
    if (is.null(tab) || !nrow(tab)) return(NULL)
    stat_col <- if (ncol(tab) >= 4L) tab[, 4] else NA_real_
    df_res <- tryCatch(stats::df.residual(m), error = function(e) Inf)
    crit <- if (inherits(m, "glm") || !is.finite(df_res)) {
      stats::qnorm(1 - (1 - conf_level) / 2)
    } else {
      stats::qt(1 - (1 - conf_level) / 2, df_res)
    }
    tibble::tibble(segment = x$segment[i], term = rownames(tab),
                   estimate = tab[, 1], std_error = tab[, 2],
                   statistic = tab[, 3], p_value = stat_col,
                   conf_low = tab[, 1] - crit * tab[, 2],
                   conf_high = tab[, 1] + crit * tab[, 2])
  })
  out <- do.call(rbind, rows)
  out %||% tibble::tibble(segment = integer(), term = character(),
                          estimate = numeric(), std_error = numeric(),
                          statistic = numeric(), p_value = numeric(),
                          conf_low = numeric(), conf_high = numeric())
}

#' @rdname cpt_segment_models
#' @export
glance.ggcpt_segment_models <- function(x, ...) {
  num <- function(f) vapply(x$model, function(m) {
    if (is.null(m)) return(NA_real_)
    v <- tryCatch(suppressWarnings(as.numeric(f(m))[1]),
                  error = function(e) NA_real_)
    if (length(v) != 1L) NA_real_ else v
  }, numeric(1))
  tibble::tibble(
    segment = x$segment, n = x$n,
    sigma = num(stats::sigma),
    r_squared = num(function(m) {
      s <- summary(m)
      if (is.null(s$r.squared)) NA_real_ else s$r.squared
    }),
    AIC = num(stats::AIC), BIC = num(stats::BIC),
    logLik = num(stats::logLik)
  )
}

#' @rdname cpt_segment_models
#' @export
print.ggcpt_segment_models <- function(x, ...) {
  cat("ggcpt_segment_models (", nrow(x), " segments, model: ",
      paste(deparse(attr(x, "formula")), collapse = ""), ")\n", sep = "")
  ok <- !vapply(x$model, is.null, logical(1))
  print(tibble::tibble(segment = x$segment, start = x$start, end = x$end,
                       n = x$n, fitted = ok))
  invisible(x)
}

#' Forecast from a segment of a segmentation
#'
#' The point of detecting a regime change is usually that the earlier
#' regime should no longer inform what happens next. \code{predict()} on a
#' \code{ggcpt} forecasts from one segment only (by default the last), with
#' the segment's own model.
#'
#' @param object A \code{ggcpt} object.
#' @param newdata Optional data frame of covariates (for a formula fit, or a
#'   segment model with covariates), or of \code{time} positions.
#' @param h Forecast horizon, when \code{newdata} is not given: the
#'   positions \code{n + 1} to \code{n + h}. Defaults to \code{1}.
#' @param segment Which segment to forecast from: \code{"last"} (the
#'   default) or a segment number.
#' @param models Optional \code{\link{cpt_segment_models}()} result; built
#'   with its defaults when omitted.
#' @param level Level of the prediction interval. Defaults to \code{0.95}.
#' @param ... Further arguments for the model's \code{predict()} method.
#' @return A tibble with \code{time} (when forecasting by horizon),
#'   \code{.pred}, and \code{.lower}/\code{.upper} when the model supplies
#'   a prediction interval.
#' @export
#' @family segment models
#' @examples
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(100), rnorm(100, 3)), method = "pelt")
#' predict(fit, h = 3)
predict.ggcpt <- function(object, newdata = NULL, h = 1, segment = "last",
                          models = NULL, level = 0.95, ...) {
  models <- models %||% cpt_segment_models(object)
  if (!inherits(models, "ggcpt_segment_models")) {
    cpt_abort("`models` must come from cpt_segment_models().",
              class = "bad_argument")
  }
  s <- if (identical(segment, "last")) nrow(models) else {
    validate_scalar(segment, "segment", min = 1, max = nrow(models))
    as.integer(segment)
  }
  m <- models$model[[s]]
  if (is.null(m)) {
    cpt_abort("The model for segment ", s, " could not be fitted, so there ",
              "is nothing to forecast from.", class = "engine_error")
  }
  n <- nrow(object$data)
  by_horizon <- is.null(newdata)
  if (by_horizon) {
    validate_scalar(h, "h", min = 1)
    newdata <- data.frame(time = n + seq_len(as.integer(h)))
  }
  pred <- tryCatch(
    stats::predict(m, newdata = newdata, interval = "prediction",
                   level = level, ...),
    error = function(e) NULL)
  out <- if (is.matrix(pred)) {
    tibble::tibble(.pred = unname(pred[, "fit"]),
                   .lower = unname(pred[, "lwr"]),
                   .upper = unname(pred[, "upr"]))
  } else {
    p <- stats::predict(m, newdata = newdata, type = "response", ...)
    tibble::tibble(.pred = unname(as.numeric(p)))
  }
  if (by_horizon) out <- tibble::add_column(out, time = newdata$time,
                                            .before = 1)
  attr(out, "segment") <- s
  out
}
