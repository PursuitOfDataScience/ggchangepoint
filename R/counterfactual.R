# ---------------------------------------------------------------------------
# §148.4 (0.7.0): observed against counterfactual. The figure that goes in
# the paper in epidemiology (an interrupted time series), in finance (the
# out-of-sample comparison) and in policy evaluation is the same one: the
# series, and what it would have done had the change not happened. It is
# what makes an effect size concrete, and none of the engines draw it.
#
# The counterfactual for the stretch after a changepoint is the segment
# before it, carried forward with its own model: its level for a change in
# mean, its line for a change in slope, its coefficients applied to the
# new covariates for a regression. cpt_segment_models() already fits those
# models, so this is a prediction from them, with its interval.
# ---------------------------------------------------------------------------

#' What would have happened without the change? Observed against counterfactual
#'
#' For each changepoint, carries the segment before it forward over the
#' segment after it, using that segment's own model, and sets the
#' prediction beside what was observed. The gap is the effect of the
#' change, observation by observation, in the units of the data: the
#' interrupted-time-series exhibit.
#'
#' @param fit A \code{ggcpt} object.
#' @param model,data,engine,... Passed to \code{\link{cpt_segment_models}()}:
#'   the model fitted to each segment (by default its level, its line for a
#'   change in slope, or the formula of a formula fit), covariates it uses,
#'   and the fitting function.
#' @param level Level of the prediction interval around the counterfactual.
#'   Defaults to \code{0.95}.
#'
#' @section Reading it:
#' The counterfactual is a prediction from the pre-change segment alone, so
#' its interval widens the further it is carried: a level is carried at
#' constant width, a line fans out. The interval describes the
#' pre-change model's uncertainty, not the location's: the changepoint is
#' taken as given, and \code{\link{cpt_confint}()} is the place to ask how
#' sure that location is. Where the detector chose the location from the
#' same data, the effect is the naive one, and \code{\link{cpt_effect}()}
#' measures it on held-out observations instead.
#'
#' @return A \code{ggcpt_counterfactual} tibble with one row per
#'   observation after each changepoint, up to the next: \code{cp} (the
#'   change it belongs to), \code{position}, \code{index} (the time index,
#'   or the position when there is none), \code{observed},
#'   \code{counterfactual}, \code{lower} and \code{upper} (the prediction
#'   interval, \code{NA} when the model supplies none) and \code{effect}
#'   (\code{observed - counterfactual}). \code{autoplot()} draws the series
#'   with each counterfactual and its band; \code{glance()} gives one row
#'   per change with the mean and cumulative effect; \code{print()} and
#'   \code{tidy()} work as usual.
#' @seealso \code{\link{cpt_effect}()}, \code{\link{cpt_segment_models}()},
#'   \code{\link{predict.ggcpt}()}.
#' @export
#' @family segment models
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(80, 10), rnorm(60, 13))
#' fit <- cpt_detect(x, method = "pelt", change_in = "meanvar")
#' cf <- cpt_counterfactual(fit)
#' glance(cf)
#' ggplot2::autoplot(cf)
cpt_counterfactual <- function(fit, model = NULL, data = NULL,
                               engine = stats::lm, level = 0.95, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  validate_scalar(level, "level", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  idx <- fit$index %||% seq_len(nrow(fit$data))
  if (!nrow(fit$changepoints)) {
    return(empty_counterfactual(fit, level, idx))
  }
  models <- cpt_segment_models(fit, model = model, data = data,
                               engine = engine, ...)
  frame <- segment_model_frame(fit, data)
  seg <- fit$segments
  rows <- vector("list", nrow(seg) - 1L)
  failed <- integer(0)
  for (s in seq_len(nrow(seg) - 1L)) {
    after <- seq.int(seg$start[s + 1L], seg$end[s + 1L])
    m <- models$model[[s]]
    pred <- if (is.null(m)) NULL else
      counterfactual_predict(m, frame[after, , drop = FALSE], level)
    if (is.null(pred)) {
      failed <- c(failed, seg$end[s])
      next
    }
    obs <- fit$data$value[after]
    rows[[s]] <- tibble::tibble(
      cp = seg$end[s], position = after, index = idx[after],
      observed = obs, counterfactual = pred$fit, lower = pred$lwr,
      upper = pred$upr, effect = obs - pred$fit)
  }
  if (length(failed)) {
    cpt_warn("The model of the segment before changepoint(s) ",
             paste(failed, collapse = ", "), " could not be fitted or carried ",
             "forward, so ", if (length(failed) > 1) "those changes have" else
               "that change has", " no counterfactual.",
             class = "engine_failed")
  }
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(empty_counterfactual(fit, level, idx))
  structure(out, class = c("ggcpt_counterfactual", class(tibble::tibble())),
            level = level, method = scalar_chr(fit$method),
            series = tibble::tibble(position = seq_len(nrow(fit$data)),
                                    index = idx, value = fit$data$value))
}

# Internal: predict a pre-change segment model over the rows after the
# change, with a prediction interval where the model gives one.
#' @noRd
counterfactual_predict <- function(m, newdata, level) {
  pred <- tryCatch(suppressWarnings(stats::predict(
    m, newdata = newdata, interval = "prediction", level = level)),
    error = function(e) NULL)
  if (is.matrix(pred) && all(c("fit", "lwr", "upr") %in% colnames(pred))) {
    return(list(fit = unname(pred[, "fit"]), lwr = unname(pred[, "lwr"]),
                upr = unname(pred[, "upr"])))
  }
  p <- tryCatch(suppressWarnings(as.numeric(stats::predict(
    m, newdata = newdata, type = "response"))), error = function(e) NULL)
  if (is.null(p) || length(p) != nrow(newdata)) return(NULL)
  list(fit = p, lwr = rep(NA_real_, length(p)), upr = rep(NA_real_, length(p)))
}

# Internal: the zero-row result, with the columns a non-empty one has.
#' @noRd
empty_counterfactual <- function(fit, level, idx) {
  out <- tibble::tibble(cp = integer(), position = integer(),
                        index = idx[integer(0)], observed = numeric(),
                        counterfactual = numeric(), lower = numeric(),
                        upper = numeric(), effect = numeric())
  structure(out, class = c("ggcpt_counterfactual", class(tibble::tibble())),
            level = level, method = scalar_chr(fit$method),
            series = tibble::tibble(position = seq_len(nrow(fit$data)),
                                    index = idx, value = fit$data$value))
}

#' @rdname cpt_counterfactual
#' @param x A \code{ggcpt_counterfactual} object.
#' @export
print.ggcpt_counterfactual <- function(x, ...) {
  cat("ggcpt_counterfactual (observed against the segment before each ",
      "change, carried forward)\n", sep = "")
  g <- glance.ggcpt_counterfactual(x)
  for (i in seq_len(nrow(g))) {
    cat_field(paste0("Change at ", g$cp[i]),
              paste0("mean effect ", format(signif(g$mean_effect[i], 4)),
                     " over ", g$n[i], " observations"))
  }
  if (!nrow(g)) cat("\nNo changepoints, so no counterfactual.\n")
  cat("\n")
  print(tibble::as_tibble(x), n = 6)
  invisible(x)
}

#' @rdname cpt_counterfactual
#' @export
tidy.ggcpt_counterfactual <- function(x, ...) {
  tibble::as_tibble(x)
}

#' @rdname cpt_counterfactual
#' @export
glance.ggcpt_counterfactual <- function(x, ...) {
  d <- tibble::as_tibble(x)
  cps <- unique(d$cp)
  tibble::tibble(
    cp = cps,
    n = vapply(cps, function(k) sum(d$cp == k), integer(1)),
    mean_effect = vapply(cps, function(k) mean(d$effect[d$cp == k]),
                         numeric(1)),
    cumulative_effect = vapply(cps, function(k) sum(d$effect[d$cp == k]),
                               numeric(1)),
    share_outside = vapply(cps, function(k) {
      r <- d[d$cp == k, , drop = FALSE]
      ok <- !is.na(r$lower) & !is.na(r$upper)
      if (!any(ok)) return(NA_real_)
      mean(r$observed[ok] < r$lower[ok] | r$observed[ok] > r$upper[ok])
    }, numeric(1))
  )
}

#' @rdname cpt_counterfactual
#' @param object A \code{ggcpt_counterfactual} object.
#' @export
autoplot.ggcpt_counterfactual <- function(object, ...) {
  series <- attr(object, "series")
  d <- tibble::as_tibble(object)
  p <- ggplot2::ggplot(series, ggplot2::aes(x = index, y = value)) +
    ggplot2::geom_line(colour = "grey35")
  if (nrow(d)) {
    d$.change <- factor(d$cp)
    if (any(!is.na(d$lower))) {
      p <- p + ggplot2::geom_ribbon(
        data = d, ggplot2::aes(x = index, ymin = lower, ymax = upper,
                               group = .change),
        inherit.aes = FALSE, fill = "#e66101", alpha = 0.18, na.rm = TRUE)
    }
    cps <- unique(d$cp)
    p <- p +
      ggplot2::geom_line(data = d, ggplot2::aes(x = index, y = counterfactual,
                                                group = .change),
                         inherit.aes = FALSE, colour = "#e66101",
                         linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = series$index[cps], colour = "blue",
                          linewidth = 0.4)
  }
  p + ggplot2::labs(x = NULL, y = NULL,
                    title = "Observed and counterfactual",
                    subtitle = paste0("Dashed: the segment before each ",
                                      "change, carried forward, with its ",
                                      format(100 * attr(object, "level")),
                                      "% prediction band")) +
    theme_ggcpt()
}
