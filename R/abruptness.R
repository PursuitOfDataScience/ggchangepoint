# ---------------------------------------------------------------------------
# Theme AC (0.7.0): gradual change. Every engine in the package looks for a
# jump, and a jump detector given a transition that took twenty observations
# reports one confidently located changepoint somewhere in the middle of
# it. Nothing in the result says the change was not abrupt, and in climate,
# ecology and land cover the honest description is often "over a decade",
# not "in 1997".
#
# cpt_abruptness() asks the question the detector cannot: around each
# changepoint, does a linear ramp of some width fit the level better than
# the step does? The ramp nests the step (width 0), so the comparison is a
# likelihood ratio, and profiling the width gives an interval for how long
# the transition took.
# ---------------------------------------------------------------------------

#' Was the change abrupt? A step against a ramp, at every changepoint
#'
#' For each changepoint of a change in mean, fits the window around it
#' (between the neighbouring changepoints) twice: as a step, which is what
#' every detector assumes, and as a linear ramp from the old level to the
#' new one, with its centre and width estimated. When the ramp fits
#' clearly better, the change took time, and the reported location is the
#' middle of a transition rather than a moment.
#'
#' @param fit A \code{ggcpt} object with a change in mean (or mean and
#'   variance).
#' @param level Level of the interval for the transition width, and
#'   \code{1 - level} the threshold at which the step is rejected. Defaults
#'   to \code{0.95}.
#' @param max_width Widest ramp considered, in observations. Defaults to
#'   half the window.
#'
#' @section How it decides:
#' For a ramp of width \eqn{w} centred at \eqn{c}, the level is \eqn{a}
#' before \eqn{c - w/2}, \eqn{b} after \eqn{c + w/2}, and linear between,
#' so for fixed \eqn{(c, w)} the fit is ordinary least squares in
#' \eqn{(a, b)}. The residual sum of squares is profiled over the centre
#' for each width on a grid (every width up to 8, then geometric), and the
#' step is the width-0 member of the family. The statistic
#' \eqn{m \log(RSS_{step} / RSS_{ramp})} over the window's \eqn{m}
#' observations is compared with a chi-squared distribution on one degree
#' of freedom. The step sits on the boundary of the family, which makes
#' that reference conservative, and the search over widths and centres,
#' which makes it liberal; measured on true steps (jumps of 1 to 8 noise
#' standard deviations, \eqn{n} of 300 and 1,000, Gaussian and \eqn{t_3}
#' noise, 400 replicates each, detected with \code{pelt}), the step was
#' rejected in 1.0 to 5.2 per cent of fits at \code{level = 0.95}. A ramp
#' of 2 standard deviations over 30 observations was called gradual in 53
#' per cent of fits at \eqn{n = 300}, over 60 observations in 70 per cent.
#' The width interval collects every width whose profile statistic is
#' within the chi-squared quantile of the best.
#'
#' A ramp also fits a step plus a few outliers near it, and a window that
#' holds a second, undetected change; read \code{"gradual"} as "a step is
#' not an adequate description", and look at \code{autoplot()}.
#'
#' @return A \code{ggcpt_abruptness} tibble with one row per changepoint:
#'   \code{cp}, \code{window_start}, \code{window_end}, \code{width} (the
#'   best ramp width, in observations), \code{width_lower} and
#'   \code{width_upper} (its interval), \code{centre} (the ramp's centre),
#'   \code{statistic} (the likelihood-ratio statistic against the step),
#'   \code{p_value} and \code{verdict} (\code{"abrupt"} when the step is
#'   not rejected, \code{"gradual"} when it is). \code{autoplot()} draws
#'   each window with the step and the ramp; \code{print()} and
#'   \code{tidy()} work as usual.
#' @seealso \code{\link{cpt_simulate}()} with \code{transition}, to measure
#'   how a detector places a gradual change; \code{\link{cpt_gof}()}.
#' @family inference
#' @export
#' @examples
#' set.seed(2026)
#' ramp <- c(rep(0, 100), seq(0, 4, length.out = 30), rep(4, 100))
#' fit <- cpt_detect(ramp + rnorm(230, 0, 0.5), method = "pelt")
#' cpt_abruptness(fit)
#' step <- cpt_detect(c(rnorm(100), rnorm(100, 4)), method = "pelt")
#' cpt_abruptness(step)
cpt_abruptness <- function(fit, level = 0.95, max_width = NULL) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  validate_scalar(level, "level", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  if (!is.null(max_width)) validate_scalar(max_width, "max_width", min = 1)
  ch <- scalar_chr(fit$change_in)
  if (!sub("^running ", "", ch %||% "") %in% c("mean", "meanvar")) {
    cpt_abort("cpt_abruptness() compares a step in the level with a ramp, so ",
              "it needs a change in mean (or mean and variance); this result ",
              "is a change in `", ch, "`.", class = "capability_absent")
  }
  if (n_coordinates(fit) > 1L) {
    cpt_abort("cpt_abruptness() works on one series; this result has ",
              n_coordinates(fit), " coordinates. Run it on a univariate fit ",
              "of the coordinate you care about.",
              class = "wrong_dimension")
  }
  y <- fit$data$value
  n <- length(y)
  cp <- sort(unique(as.integer(fit$changepoints$cp)))
  crit <- stats::qchisq(level, df = 1)
  if (!length(cp)) return(empty_abruptness(level))
  bounds <- c(0L, cp, n)
  rows <- lapply(seq_along(cp), function(k) {
    lo <- bounds[k] + 1L
    hi <- bounds[k + 2L]
    pos <- seq.int(lo, hi)
    keep <- !is.na(y[pos])
    r <- ramp_profile(y[pos][keep], pos[keep], cp[k], crit, max_width)
    if (is.null(r)) {
      return(tibble::tibble(cp = cp[k], window_start = lo, window_end = hi,
                            width = NA_real_, width_lower = NA_real_,
                            width_upper = NA_real_, centre = NA_real_,
                            statistic = NA_real_, p_value = NA_real_,
                            verdict = NA_character_))
    }
    p <- stats::pchisq(r$statistic, df = 1, lower.tail = FALSE)
    tibble::tibble(cp = cp[k], window_start = lo, window_end = hi,
                   width = r$width, width_lower = r$lower,
                   width_upper = r$upper, centre = r$centre,
                   statistic = r$statistic, p_value = p,
                   verdict = if (p < 1 - level) "gradual" else "abrupt")
  })
  out <- do.call(rbind, rows)
  structure(out, class = c("ggcpt_abruptness", class(tibble::tibble())),
            level = level, series = y, method = scalar_chr(fit$method))
}

#' @noRd
empty_abruptness <- function(level) {
  out <- tibble::tibble(cp = integer(), window_start = integer(),
                        window_end = integer(), width = numeric(),
                        width_lower = numeric(), width_upper = numeric(),
                        centre = numeric(), statistic = numeric(),
                        p_value = numeric(), verdict = character())
  structure(out, class = c("ggcpt_abruptness", class(tibble::tibble())),
            level = level, series = numeric(0), method = NA_character_)
}

# Internal: the ramp's design column for a centre and width, at positions
# `t`: 0 before the ramp, 1 after, linear across it. Width 0 is the step
# after `centre` (a changepoint at c has its last old observation at c).
#' @noRd
ramp_design <- function(t, centre, width) {
  if (width <= 0) return(as.numeric(t > centre))
  pmin(1, pmax(0, (t - (centre + 0.5 - width / 2)) / width))
}

# Internal: the residual sum of squares of the best ramp of width `w` at
# each centre in `centres`, from prefix sums, so no design matrix is
# formed: before the ramp the design is 0, after it 1, and across it
# (t - s) / w, whose sums over any run of positions are differences of the
# cumulative sums of 1, t, t^2, y and t * y.
#' @noRd
ramp_rss <- function(pre, centres, w) {
  m <- pre$m
  at <- function(v, k) c(0, v)[k + 1L]
  if (w <= 0) {
    nb <- findInterval(centres, pre$t)
    sf <- m - nb
    sff <- sf
    sfy <- pre$ytot - at(pre$cy, nb)
  } else {
    s0 <- centres + 0.5 - w / 2
    e0 <- s0 + w
    nb <- findInterval(s0, pre$t)
    nr <- findInterval(e0, pre$t, left.open = TRUE)
    cnt <- nr - nb
    st <- at(pre$ct, nr) - at(pre$ct, nb)
    st2 <- at(pre$ct2, nr) - at(pre$ct2, nb)
    sy <- at(pre$cy, nr) - at(pre$cy, nb)
    sty <- at(pre$cty, nr) - at(pre$cty, nb)
    na <- m - nr
    sf <- (st - s0 * cnt) / w + na
    sff <- (st2 - 2 * s0 * st + s0^2 * cnt) / w^2 + na
    sfy <- (sty - s0 * sy) / w + (pre$ytot - at(pre$cy, nr))
  }
  ff <- sff - sf^2 / m
  fy <- sfy - sf * pre$ybar
  ifelse(ff > 1e-12, pre$syy - fy^2 / ff, pre$syy)
}

# Internal: profile the residual sum of squares over centre and width.
#' @noRd
ramp_profile <- function(y, t, cp, crit, max_width = NULL) {
  m <- length(y)
  if (m < 8L) return(NULL)
  syy <- sum((y - mean(y))^2)
  if (syy <= 0) return(NULL)
  max_w <- max_width %||% floor(m / 2)
  max_w <- max(1, min(max_w, m - 4L))
  widths <- unique(c(0, seq_len(min(8L, max_w)),
                     round(8 * 1.25^seq_len(60))))
  widths <- widths[widths <= max_w]
  centres <- seq(t[2], t[m - 2L], by = 0.5)
  t <- as.numeric(t)
  pre <- list(m = m, t = t, ybar = mean(y), ytot = sum(y), syy = syy,
              cy = cumsum(y), ct = cumsum(t), ct2 = cumsum(t^2),
              cty = cumsum(t * y))
  rss <- lapply(widths, function(w) ramp_rss(pre, centres, w))
  best_rss <- pmax(vapply(rss, min, numeric(1)), 1e-12 * syy)
  i <- which.min(best_rss)
  prof <- m * log(best_rss / best_rss[i])
  inside <- widths[prof <= crit]
  list(width = widths[i], lower = min(inside), upper = max(inside),
       centre = centres[which.min(rss[[i]])],
       statistic = m * log(best_rss[widths == 0] / best_rss[i]))
}

#' @rdname cpt_abruptness
#' @param x A \code{ggcpt_abruptness} object.
#' @param ... Ignored.
#' @export
print.ggcpt_abruptness <- function(x, ...) {
  cat("ggcpt_abruptness (a step against a ramp at each changepoint)\n")
  if (nrow(x)) {
    for (i in seq_len(nrow(x))) {
      cat_field(paste0("Changepoint ", x$cp[i]),
                if (is.na(x$verdict[i])) "window too short to tell" else
                  paste0(x$verdict[i], ", transition width ",
                         format(x$width[i]), " (", format(x$width_lower[i]),
                         " to ", format(x$width_upper[i]), ")"))
    }
    cat("\n")
    print(tibble::as_tibble(x))
  } else {
    cat("\nNo changepoints to examine.\n")
  }
  invisible(x)
}

#' @rdname cpt_abruptness
#' @export
tidy.ggcpt_abruptness <- function(x, ...) {
  tibble::as_tibble(x)
}

#' @rdname cpt_abruptness
#' @param object A \code{ggcpt_abruptness} object.
#' @export
autoplot.ggcpt_abruptness <- function(object, ...) {
  y <- attr(object, "series")
  d <- tibble::as_tibble(object)
  if (!nrow(d)) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No changepoints to examine"))
  }
  fits <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    t <- seq.int(d$window_start[i], d$window_end[i])
    v <- y[t]
    ok <- !is.na(v)
    lab <- paste0("Changepoint ", d$cp[i], ": ", d$verdict[i])
    step <- ramp_fit(v[ok], t[ok], d$cp[i], 0)
    ramp <- if (is.na(d$width[i])) step else
      ramp_fit(v[ok], t[ok], d$centre[i], d$width[i])
    rbind(
      tibble::tibble(panel = lab, index = t[ok], value = v[ok],
                     kind = "data"),
      tibble::tibble(panel = lab, index = t[ok], value = step,
                     kind = "step"),
      tibble::tibble(panel = lab, index = t[ok], value = ramp,
                     kind = "ramp"))
  }))
  pts <- fits[fits$kind == "data", , drop = FALSE]
  lines <- fits[fits$kind != "data", , drop = FALSE]
  ggplot2::ggplot(pts, ggplot2::aes(x = index, y = value)) +
    ggplot2::geom_point(colour = "grey55", size = 0.9) +
    ggplot2::geom_line(data = lines,
                       ggplot2::aes(colour = kind, linetype = kind),
                       linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c(step = "#5e3c99",
                                            ramp = "#e66101"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c(step = "solid",
                                              ramp = "dashed"), name = NULL) +
    ggplot2::facet_wrap(~panel, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Step or ramp: was each change abrupt?") +
    theme_ggcpt()
}

# Internal: fitted values of the ramp (or step) with given centre and width.
#' @noRd
ramp_fit <- function(y, t, centre, width) {
  F <- ramp_design(t, centre, width)
  if (stats::var(F) <= 0) return(rep(mean(y), length(y)))
  stats::fitted(stats::lm(y ~ F))
}
