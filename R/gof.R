# ---------------------------------------------------------------------------
# Diagnostics after the segmentation.
#
# A regression package that gave coefficients and no residual plots would be
# called unfinished. The commonest way a changepoint analysis goes wrong is
# not a faulty search but an inappropriate model: dependent noise, a
# variance that changes, tails a Gaussian cost cannot absorb. These put the
# segment-wise checks in numbers (cpt_gof()) and in one picture
# (autoplot(fit, type = "diagnostics")).
# ---------------------------------------------------------------------------

# Internal: the residuals of a univariate fit against its fitted signal (the
# engine's when it supplied one, the segment means otherwise), with segment
# ids.
#' @noRd
fit_residuals <- function(fit, values = NULL) {
  v <- values %||% fit$data$value
  n <- length(v)
  seg <- fit$segments
  seg_id <- rep(NA_integer_, n)
  fitted <- rep(NA_real_, n)
  for (i in seq_len(nrow(seg))) {
    rows <- seq(seg$start[i], seg$end[i])
    seg_id[rows] <- seg$seg_id[i]
    fitted[rows] <- if (is.null(values)) seg$param_estimate[i] else
      mean(v[rows], na.rm = TRUE)
  }
  eng <- fit$data[["fitted"]]
  if (is.null(values) && !is.null(eng) && length(eng) == n) fitted <- eng
  tibble::tibble(index = seq_len(n), value = v, seg_id = seg_id,
                 fitted = fitted, resid = v - fitted)
}

#' Goodness of fit, segment by segment
#'
#' The checks behind a segmentation, as numbers: for every segment, its
#' length, level and spread, whether its residuals look independent
#' (Ljung-Box) and Gaussian (Shapiro-Wilk), and a flag where the segment is
#' too short for either test to mean anything.
#'
#' @param fit A \code{ggcpt} object.
#' @param lag Ljung-Box lag. Defaults to \code{min(10, n / 5)} per segment.
#' @return A \code{ggcpt_gof} tibble with one row per segment (per segment
#'   and coordinate for a multivariate fit): \code{seg_id}, \code{start},
#'   \code{end}, \code{n}, \code{mean}, \code{sd}, \code{acf1} (lag-1
#'   autocorrelation of the residuals), \code{ljung_box_p},
#'   \code{shapiro_p}, and \code{too_short} (fewer than 12 observations,
#'   where the tests are left \code{NA}). The whole-series Ljung-Box result
#'   is attached as the \code{"overall"} attribute, and \code{print()}
#'   summarises what the columns say.
#' @seealso \code{autoplot(fit, type = "diagnostics")} draws the same
#'   checks; \code{\link{cpt_assumptions}()} reports them with the
#'   alternatives.
#' @export
#' @family inference
#' @examples
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(100), rnorm(100, 3, 2)), method = "pelt")
#' cpt_gof(fit)
cpt_gof <- function(fit, lag = NULL) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  if (!is.null(lag)) validate_scalar(lag, "lag", min = 1)
  series <- if (n_coordinates(fit) > 1L) {
    w <- fit$data_wide
    as.list(w[, setdiff(names(w), c("index", "index_value")), drop = FALSE])
  } else {
    list(value = NULL)
  }
  rows <- lapply(names(series), function(nm) {
    r <- fit_residuals(fit, series[[nm]])
    out <- lapply(split(r, r$seg_id), function(d) {
      res <- d$resid[is.finite(d$resid)]
      m <- length(res)
      short <- m < 12L
      l <- lag %||% max(1L, min(10L, floor(m / 5)))
      lb <- if (!short && stats::sd(res) > 0) {
        tryCatch(stats::Box.test(res, lag = l, type = "Ljung-Box")$p.value,
                 error = function(e) NA_real_)
      } else NA_real_
      sw <- if (!short && m <= 5000 && stats::sd(res) > 0) {
        tryCatch(stats::shapiro.test(res)$p.value, error = function(e) NA_real_)
      } else NA_real_
      acf1 <- if (m > 2 && stats::sd(res) > 0) {
        stats::acf(res, lag.max = 1, plot = FALSE)$acf[2]
      } else NA_real_
      tibble::tibble(seg_id = d$seg_id[1], start = min(d$index),
                     end = max(d$index), n = nrow(d),
                     mean = mean(d$value, na.rm = TRUE),
                     sd = if (m > 1) stats::sd(d$value, na.rm = TRUE) else
                       NA_real_,
                     acf1 = acf1, ljung_box_p = lb, shapiro_p = sw,
                     too_short = short)
    })
    tb <- do.call(rbind, out)
    if (length(series) > 1L) tb <- tibble::add_column(tb, coordinate = nm,
                                                       .before = 1)
    tb
  })
  out <- do.call(rbind, rows)
  overall <- residual_dependence(fit$data$value, fit$changepoints$cp,
                                 fitted = fit$data[["fitted"]], lag = lag)
  structure(out, class = c("ggcpt_gof", class(tibble::tibble())),
            overall = overall, method = scalar_chr(fit$method))
}

#' @rdname cpt_gof
#' @param x A \code{ggcpt_gof} object.
#' @param ... Ignored.
#' @export
print.ggcpt_gof <- function(x, ...) {
  cat("ggcpt_gof (method: ", attr(x, "method") %||% "?", ", ",
      nrow(x), " segment rows)\n", sep = "")
  ov <- attr(x, "overall")
  if (!is.null(ov)) {
    cat("Whole series: Ljung-Box p = ", format(signif(ov$p_value, 2)),
        " at lag ", ov$lag, ", lag-1 autocorrelation ",
        format(round(ov$acf1, 2)), "\n", sep = "")
  }
  notes <- character(0)
  if (any(x$ljung_box_p < 0.05, na.rm = TRUE)) {
    notes <- c(notes, paste0(sum(x$ljung_box_p < 0.05, na.rm = TRUE),
                             " segment(s) with autocorrelated residuals"))
  }
  if (any(x$shapiro_p < 0.05, na.rm = TRUE)) {
    notes <- c(notes, paste0(sum(x$shapiro_p < 0.05, na.rm = TRUE),
                             " segment(s) with non-Gaussian residuals"))
  }
  sds <- x$sd[is.finite(x$sd) & x$sd > 0]
  if (length(sds) > 1L && max(sds) / min(sds) > 2) {
    notes <- c(notes, paste0("segment spreads differ by a factor of ",
                             format(round(max(sds) / min(sds), 1))))
  }
  if (any(x$too_short)) {
    notes <- c(notes, paste0(sum(x$too_short),
                             " segment(s) too short to test"))
  }
  if (length(notes)) cat(paste0("Note: ", notes, collapse = "\n"), "\n\n") else
    cat("\n")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

# Internal: the four-panel diagnostics display.
#' @noRd
autoplot_diagnostics <- function(object, max_lag = 10) {
  r <- fit_residuals(object)
  r$segment <- factor(r$seg_id)
  cps <- object$changepoints$cp
  panels <- c("Residuals", "Residual autocorrelation", "Normal Q-Q",
              "Spread by segment")
  lvl <- function(p) factor(p, levels = panels)
  res_d <- data.frame(x = r$index, y = r$resid, panel = lvl(panels[1]))
  acf_d <- do.call(rbind, lapply(split(r, r$seg_id), function(d) {
    res <- d$resid[is.finite(d$resid)]
    if (length(res) < 4L || stats::sd(res) == 0) return(NULL)
    k <- min(max_lag, length(res) - 1L)
    a <- stats::acf(res, lag.max = k, plot = FALSE)$acf[-1]
    data.frame(x = seq_along(a), y = a, segment = factor(d$seg_id[1],
                                                         levels = levels(r$segment)),
               panel = lvl(panels[2]))
  }))
  qq_d <- do.call(rbind, lapply(split(r, r$seg_id), function(d) {
    res <- d$resid[is.finite(d$resid)]
    if (length(res) < 3L || stats::sd(res) == 0) return(NULL)
    z <- sort((res - mean(res)) / stats::sd(res))
    data.frame(x = stats::qnorm(stats::ppoints(length(z))), y = z,
               segment = factor(d$seg_id[1], levels = levels(r$segment)),
               panel = lvl(panels[3]))
  }))
  sd_d <- do.call(rbind, lapply(split(r, r$seg_id), function(d) {
    data.frame(x = d$seg_id[1],
               y = if (nrow(d) > 1) stats::sd(d$value, na.rm = TRUE) else NA,
               segment = factor(d$seg_id[1], levels = levels(r$segment)),
               panel = lvl(panels[4]))
  }))
  n <- nrow(r)
  band <- if (!is.null(acf_d)) {
    data.frame(y = c(-1, 1) * stats::qnorm(0.975) / sqrt(n / max(1, length(cps) + 1)),
               panel = lvl(panels[2]))
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(data = data.frame(y = 0, panel = lvl(panels[1:2])),
                        ggplot2::aes(yintercept = y), colour = "grey60") +
    ggplot2::geom_line(data = res_d, ggplot2::aes(x = x, y = y),
                       colour = "grey40") +
    ggplot2::facet_wrap(~panel, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL,
                  title = paste0("Diagnostics (", scalar_chr(object$method),
                                 ")"),
                  colour = "Segment") +
    theme_ggcpt()
  if (length(cps)) {
    p <- p + ggplot2::geom_vline(
      data = data.frame(x = cps, panel = lvl(panels[1])),
      ggplot2::aes(xintercept = x), colour = "blue", linewidth = 0.4)
  }
  if (!is.null(band)) {
    p <- p + ggplot2::geom_hline(data = band, ggplot2::aes(yintercept = y),
                                 linetype = "dashed", colour = "grey50")
  }
  if (!is.null(acf_d) && nrow(acf_d)) {
    p <- p + ggplot2::geom_point(data = acf_d, ggplot2::aes(x = x, y = y,
                                                             colour = segment)) +
      ggplot2::geom_line(data = acf_d, ggplot2::aes(x = x, y = y,
                                                    colour = segment))
  }
  if (!is.null(qq_d) && nrow(qq_d)) {
    p <- p + ggplot2::geom_abline(
      data = data.frame(panel = lvl(panels[3]), a = 0, b = 1),
      ggplot2::aes(intercept = a, slope = b), colour = "grey60") +
      ggplot2::geom_point(data = qq_d, ggplot2::aes(x = x, y = y,
                                                    colour = segment),
                          size = 0.8)
  }
  # Lollipops rather than columns: geom_col() computes a bar width in
  # every panel, and warned in the three where this layer has no data.
  p + ggplot2::geom_segment(data = sd_d, ggplot2::aes(x = x, xend = x,
                                                      y = 0, yend = y,
                                                      colour = segment),
                            show.legend = FALSE) +
    ggplot2::geom_point(data = sd_d, ggplot2::aes(x = x, y = y,
                                                  colour = segment),
                        size = 2, show.legend = FALSE) +
    scale_colour_cpt()
}
