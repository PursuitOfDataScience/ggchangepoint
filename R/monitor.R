# ---------------------------------------------------------------------------
# Theme G: streaming and online monitoring.
#
# cpm, ocd and BOCPD are *online* methods that 0.4.0 ran in batch-replay mode
# and reported as if they were retrospective. The output the online
# literature cares about -- the alarm sequence, the detection delay, the
# average run length -- was not first-class, and there was no way to feed new
# observations to an existing fit. A monitor is a genuinely different object
# from a segmentation, so it gets its own class rather than another slot.
#
# `edetector` is the one method in this package implemented here rather than
# wrapped. That is a deliberate exception to the wrap-don't-implement rule
# (see ?cpt_monitor), taken because no R package implements e-detectors and
# the construction is short enough to be auditable in one screen.
# ---------------------------------------------------------------------------

#' A stateful sequential changepoint monitor
#'
#' Creates a detector that consumes observations as they arrive and raises
#' alarms, rather than segmenting a series that is already complete. Feed it
#' with \code{\link{cpt_update}()}, read its alarm log with
#' \code{\link{alarms}()}, and score it with \code{\link{cpt_delay}()} —
#' because for an online method "did you find the location?" is the wrong
#' question and "how long did you take, and how often do you false-alarm?"
#' is the right one.
#'
#' @param method Which sequential detector:
#'   \describe{
#'     \item{\code{"edetector"}}{(default) a mixture Shiryaev–Roberts
#'       e-detector; see the section below.}
#'     \item{\code{"cpm"}}{\pkg{cpm}'s sequential change-point model, tuned
#'       by \code{ARL0}.}
#'     \item{\code{"ocd"}}{\pkg{ocd}'s high-dimensional online detector.
#'       \strong{Multivariate only} -- it tracks a projection of the whole
#'       vector and needs at least two coordinates, so it refuses a single
#'       series rather than falling back to a univariate statistic.}
#'   }
#' @param baseline A numeric vector (or, for \code{"ocd"}, a matrix with rows
#'   as time points) of pre-change training data used to estimate the
#'   in-control mean and scale. Required for \code{"edetector"} and
#'   \code{"ocd"}; optional for \code{"cpm"}, which has its own start-up
#'   period.
#' @param alpha Target false-alarm probability for \code{"edetector"}: the
#'   average run length under the null is at least \code{1 / alpha}, by
#'   optional stopping on the martingale \eqn{M_t - t} (see Details).
#'   Defaults to \code{0.01}.
#' @param arl0 Target in-control average run length for \code{"cpm"}.
#'   Defaults to \code{500}.
#' @param cpm_type Statistic for \code{"cpm"}. Defaults to
#'   \code{"Mann-Whitney"}.
#' @param patience Target patience (average run length) for \code{"ocd"}.
#'   Defaults to \code{5000}.
#' @param deltas Shift sizes, in baseline standard deviations, mixed over by
#'   \code{"edetector"}. Defaults to \code{c(0.5, 1, 2)}, each taken in both
#'   directions. The mixture is a uniform average, so adding shifts costs
#'   power at the ones already there rather than inflating the false-alarm
#'   rate; mix over the range you think the change could fall in, not over
#'   everything.
#' @param reset After an alarm, restart the detector (\code{TRUE}, the
#'   default) or leave it running? Restarting is what makes a monitor report
#'   \emph{repeated} changes rather than latching on the first one.
#' @param relearn How many observations after an alarm are used to re-learn
#'   the in-control baseline, during which no further alarm can fire.
#'   Defaults to \code{20}. This matters more than it looks: a real change
#'   is \emph{persistent}, so a detector that restarts against the stale
#'   pre-change baseline alarms again on the very next observation and keeps
#'   alarming for the rest of the series — the monitor reports one change as
#'   hundreds, and \code{\link{cpt_delay}()} then counts them all as false
#'   alarms. Set \code{relearn = 0} to switch the behaviour off and see
#'   every threshold crossing.
#' @param thresh Threshold rule for \code{"ocd"}: \code{"MC"} (default)
#'   calibrates by Monte Carlo against \code{patience}, or supply a numeric
#'   vector of three thresholds. The Monte Carlo calibration is the
#'   expensive part of building an \code{"ocd"} monitor — a minute or more
#'   at the default \code{patience} — so pass thresholds directly when you
#'   already have them, or lower \code{mc_reps} while exploring.
#' @param mc_reps Monte Carlo repetitions for the \code{"ocd"} threshold.
#' @param ... Additional arguments passed to the engine's constructor.
#'
#' @section The e-detector, and why it is implemented rather than wrapped:
#' Shin, Ramdas and Rinaldo (2023) give a nonparametric sequential framework
#' with non-asymptotic control of the average run length, and it has no R
#' implementation. The construction used here is the mixture Shiryaev–Roberts
#' e-detector for a sub-Gaussian shift. For each candidate shift
#' \eqn{\delta} the increment is the likelihood ratio
#' \eqn{e_t^{(\delta)} = \exp(\delta (X_t - \mu_0)/\sigma^2 -
#' \delta^2/(2\sigma^2))}, which has unit mean under the null; the running
#' statistic is \eqn{R_t^{(\delta)} = (1 + R_{t-1}^{(\delta)})
#' e_t^{(\delta)}}; the shifts are combined by \emph{averaging},
#' \eqn{M_t = K^{-1} \sum_\delta R_t^{(\delta)}}; and an alarm is raised
#' the first time \eqn{M_t \ge 1/\alpha}.
#'
#' The averaging is not a detail. Under the null \eqn{M_t - t} is a
#' mean-zero martingale, so optional stopping at the alarm time \eqn{\tau}
#' gives \eqn{E_\infty[\tau] = E_\infty[M_\tau] \ge 1/\alpha}: a
#' finite-sample lower bound on the in-control average run length, with no
#' asymptotics and no calibration run. A convex combination of e-detectors
#' is an e-detector; a \emph{maximum} of them is not, and taking one
#' silently multiplies the false-alarm rate by roughly the number of shifts
#' mixed over. Every other detector in this package wraps published,
#' separately maintained code; this one does not, and is labelled as such
#' wherever it appears.
#'
#' @return A \code{ggcpt_monitor} object.
#' @references
#' \insertRef{shin2023edetectors}{ggchangepoint}
#' @seealso \code{\link{cpt_update}()}, \code{\link{alarms}()},
#'   \code{\link{cpt_replay}()}, \code{\link{cpt_delay}()}.
#' @export
#' @examples
#' set.seed(2026)
#' mon <- cpt_monitor("edetector", baseline = rnorm(100))
#' mon <- cpt_update(mon, rnorm(50))          # still in control
#' mon <- cpt_update(mon, rnorm(50, 3))       # a change arrives
#' alarms(mon)
cpt_monitor <- function(method = c("edetector", "cpm", "ocd"),
                        baseline = NULL, alpha = 0.01, arl0 = 500,
                        cpm_type = "Mann-Whitney", patience = 5000,
                        deltas = c(0.5, 1, 2), reset = TRUE, relearn = 20,
                        thresh = "MC", mc_reps = 100, ...) {
  method <- match.arg(method)

  # The three detectors are calibrated in different currencies and each
  # ignores the others': setting `arl0` on an e-detector, or `alpha` on
  # `cpm`, changes nothing at all. The documentation and the vignette both
  # say so, which does not help the user who set one and is now reading an
  # unchanged answer. Only an argument the caller supplied is reported --
  # the defaults supply all of them.
  governs <- c(alpha = "edetector", deltas = "edetector",
               arl0 = "cpm", cpm_type = "cpm",
               patience = "ocd", thresh = "ocd", mc_reps = "ocd")
  supplied <- c(alpha = !missing(alpha), deltas = !missing(deltas),
                arl0 = !missing(arl0), cpm_type = !missing(cpm_type),
                patience = !missing(patience), thresh = !missing(thresh),
                mc_reps = !missing(mc_reps))
  ignored <- names(governs)[supplied[names(governs)] & governs != method]
  if (length(ignored) > 0) {
    warning("`", paste(ignored, collapse = "`, `"), "` ",
            if (length(ignored) > 1) "do" else "does",
            " not affect `method = \"", method, "\"`, which is tuned by `",
            paste(names(governs)[governs == method], collapse = "`, `"),
            "`. See ?cpt_monitor.", call. = FALSE)
  }

  validate_scalar(alpha, "alpha", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_scalar(relearn, "relearn", min = 0)
  validate_flag(reset, "reset")

  # Normalise the series ONCE, before the switch. Each branch used to coerce
  # `baseline` its own way -- as.numeric() for edetector and cpm, a matrix
  # for ocd -- so a factor reached two of the three detectors as its level
  # codes. Guarding here means every method enters through the same door,
  # which is the only way this stays true when a fourth branch is added.
  if (!is.null(baseline)) {
    if (is.matrix(baseline) || is.data.frame(baseline)) {
      # `edetector` and `cpm` monitor ONE series, and length() on a
      # data.frame counts its columns -- so a 60-row, 2-column baseline was
      # refused for having "at least 5 pre-change observations", naming a
      # count of 2 for 60 observations. Refuse the shape instead, since a
      # multi-column baseline for a univariate detector is outside the
      # documented contract either way.
      if (!identical(method, "ocd")) {
        reject_multicolumn(baseline, "baseline",
                           paste0("`method = \"", method, "\"` monitors one ",
                                  "series; use `method = \"ocd\"` for a ",
                                  "multivariate stream."))
        baseline <- as_mv_matrix(baseline, arg = "baseline")[, 1]
      }
    } else {
      baseline <- coerce_series_values(baseline, arg = "baseline")
    }
  }

  state <- switch(method,
    edetector = {
      if (is.null(baseline) || NROW(baseline) < 5) {
        stop("`edetector` estimates the in-control mean and scale from ",
             "`baseline`, so it needs at least 5 pre-change observations.",
             call. = FALSE)
      }
      b <- as.numeric(baseline)
      # Check finiteness first: sd() of a vector holding an NA is itself NA,
      # so the variability test below would blame a flat baseline for what is
      # really a missing value.
      if (anyNA(b) || any(!is.finite(b))) {
        stop_nonfinite(b, "baseline")
      }
      sd0 <- stats::sd(b)
      if (!is.finite(sd0) || sd0 <= 0) {
        stop("`baseline` has zero variability, so the e-detector's scale is ",
             "undefined.", call. = FALSE)
      }
      deltas <- sort(unique(c(-abs(deltas), abs(deltas)))) * sd0
      list(mu0 = mean(b), sd0 = sd0, deltas = deltas,
           R = rep(0, length(deltas)),
           threshold = 1 / alpha)
    },
    cpm = {
      need_pkg("cpm")
      m <- cpm::makeChangePointModel(cpmType = cpm_type, ARL0 = arl0, ...)
      if (!is.null(baseline)) {
        for (v in as.numeric(baseline)) m <- cpm::processObservation(m, v)
      }
      list(model = m, cpm_type = cpm_type, arl0 = arl0)
    },
    ocd = {
      need_pkg("ocd")
      if (is.null(baseline)) {
        stop("`ocd` needs `baseline` to estimate the pre-change mean and ",
             "standard deviation.", call. = FALSE)
      }
      B <- if (is.matrix(baseline) || is.data.frame(baseline)) {
        as_mv_matrix(baseline, arg = "baseline")
      } else {
        matrix(baseline, ncol = 1)
      }
      p <- ncol(B)
      if (p < 2) {
        stop("Method `ocd` is high-dimensional and needs at least two ",
             "coordinates, but `baseline` has ", p,
             ". Use method = \"edetector\" or \"cpm\" for one series.",
             call. = FALSE)
      }
      det <- ocd::ChangepointDetector(dim = p, method = "ocd",
                                      thresh = thresh, beta = 1,
                                      patience = patience,
                                      MC_reps = mc_reps, ...)
      det <- ocd::setBaselineMean(det, colMeans(B))
      det <- ocd::setBaselineSD(det, apply(B, 2, stats::sd))
      det <- ocd::setStatus(det, "monitoring")
      list(detector = det, p = p, patience = patience,
           mean = colMeans(B),
           sd = { sdv <- apply(B, 2, stats::sd)
                  sdv[!is.finite(sdv) | sdv == 0] <- 1
                  sdv })
    }
  )

  structure(
    list(method = method, state = state, reset = reset,
         relearn = as.integer(relearn),
         n_baseline = if (is.null(baseline)) 0L else NROW(baseline),
         t = 0L,
         relearn_left = 0L,
         relearn_buffer = NULL,
         data = numeric(0),
         alarms = tibble::tibble(time = integer(), statistic = numeric(),
                                 threshold = numeric()),
         alpha = alpha),
    class = "ggcpt_monitor"
  )
}

#' Feed observations to a monitor
#'
#' Pushes new data through a \code{\link{cpt_monitor}()}, updating its
#' internal state and appending to its alarm log. The monitor is returned, so
#' the idiom is \code{mon <- cpt_update(mon, new_obs)}.
#'
#' @param monitor A \code{ggcpt_monitor} object.
#' @param new_obs New observations: a numeric vector, or a matrix with rows
#'   as time points for a multivariate monitor.
#' @return The updated \code{ggcpt_monitor}.
#' @seealso \code{\link{cpt_monitor}()}, \code{\link{alarms}()}.
#' @export
#' @examples
#' set.seed(2026)
#' mon <- cpt_monitor("edetector", baseline = rnorm(100))
#' mon <- cpt_update(mon, c(rnorm(50), rnorm(50, 3)))
#' alarms(mon)
cpt_update <- function(monitor, new_obs) {
  if (!inherits(monitor, "ggcpt_monitor")) {
    stop("`monitor` must be a ggcpt_monitor from cpt_monitor().",
         call. = FALSE)
  }
  X <- if (is.matrix(new_obs) || is.data.frame(new_obs)) {
    as_mv_matrix(new_obs, arg = "new_obs")
  } else {
    # a streaming caller passes observations one at a time, so this is the
    # most likely place for a factor level code to enter unnoticed
    matrix(coerce_series_values(new_obs, arg = "new_obs"), ncol = 1)
  }
  if (nrow(X) == 0) return(monitor)
  if (anyNA(X) || any(!is.finite(X))) {
    stop_nonfinite(X, "new_obs")
  }
  # A monitor is stateful and dimensioned by its baseline. Feeding it a
  # different width is a mistake, not a coercion: `ocd` would consume the
  # wrong coordinates and the univariate detectors would silently read
  # column 1 and ignore the rest.
  expected <- monitor_width(monitor)
  if (!is.na(expected) && ncol(X) != expected) {
    stop("This monitor was built on ", expected, " coordinate(s) but ",
         "`new_obs` has ", ncol(X),
         ". A monitor cannot change width once it is running.",
         call. = FALSE)
  }

  st <- monitor$state
  new_alarms <- list()

  for (i in seq_len(nrow(X))) {
    monitor$t <- monitor$t + 1L
    xt <- X[i, ]
    monitor$data <- c(monitor$data, xt[1])

    # Re-learning window after an alarm: collect the new regime and stay
    # silent, then adopt it as the in-control baseline. Without this a
    # persistent change makes the detector alarm on every subsequent
    # observation (see the `relearn` argument of cpt_monitor()).
    if (monitor$relearn_left > 0L) {
      monitor$relearn_buffer <- rbind(monitor$relearn_buffer, xt)
      monitor$relearn_left <- monitor$relearn_left - 1L
      if (monitor$relearn_left == 0L) {
        st <- monitor_relearn(monitor$method, st, monitor$relearn_buffer)
        monitor$relearn_buffer <- NULL
      }
      next
    }

    hit <- FALSE
    stat <- NA_real_
    thr <- NA_real_

    if (monitor$method == "edetector") {
      inc <- exp(st$deltas * (xt[1] - st$mu0) / st$sd0^2 -
                   st$deltas^2 / (2 * st$sd0^2))
      st$R <- (1 + st$R) * inc
      # AVERAGE, not max: a convex combination of e-detectors is an
      # e-detector and keeps E[M_t] = t, which is what the average-run-length
      # bound rests on. A maximum over K shifts crosses the threshold roughly
      # K times as often under the null -- measured here at ~1.8x the bound
      # with six shifts before this was corrected.
      stat <- mean(st$R)
      thr <- st$threshold
      hit <- is.finite(stat) && stat >= thr
      if (hit && monitor$reset) st$R <- rep(0, length(st$deltas))
    } else if (monitor$method == "cpm") {
      st$model <- cpm::processObservation(st$model, xt[1])
      stat <- suppressWarnings(as.numeric(
        utils::tail(cpm::getStatistics(st$model), 1)
      ))
      if (length(stat) == 0) stat <- NA_real_
      hit <- isTRUE(cpm::changeDetected(st$model))
      if (hit && monitor$reset) {
        st$model <- cpm::cpmReset(st$model)
      }
    } else {
      # ocd monitors a standardised stream, so the baseline enters here
      # rather than through the detector object.
      z <- (as.numeric(xt) - st$mean) / st$sd
      utils::capture.output(st$detector <- ocd::getData(st$detector, z))
      # `normalisedStatistics()` already divides each statistic by its own
      # threshold, so the comparison point is 1. Reporting the raw
      # thresholds alongside the normalised statistic would print an alarm
      # at 1.10 against a threshold of 16.4, which reads as a bug.
      stat <- suppressWarnings(max(as.numeric(
        unlist(ocd::normalisedStatistics(st$detector))
      ), na.rm = TRUE))
      thr <- 1
      hit <- !identical(ocd::status(st$detector), "monitoring")
      if (hit && monitor$reset) {
        st$detector <- ocd::reset(st$detector)
        st$detector <- ocd::setStatus(st$detector, "monitoring")
      }
    }

    if (isTRUE(hit)) {
      new_alarms[[length(new_alarms) + 1L]] <-
        tibble::tibble(time = monitor$t, statistic = stat, threshold = thr)
      if (monitor$relearn > 0L) {
        monitor$relearn_left <- monitor$relearn
        monitor$relearn_buffer <- NULL
      }
    }
  }

  monitor$state <- st
  if (length(new_alarms) > 0) {
    monitor$alarms <- rbind(monitor$alarms, do.call(rbind, new_alarms))
  }
  monitor
}

# Internal: how many coordinates this monitor was built for. `edetector`
# and `cpm` read a single series; `ocd` carries the dimension it was
# constructed with.
#' @noRd
monitor_width <- function(monitor) {
  switch(monitor$method,
    ocd = monitor$state$p,
    edetector = 1L,
    cpm = 1L,
    NA_integer_
  )
}

# Internal: adopt a post-alarm window as the new in-control baseline. Only
# the detectors that hold an explicit baseline need this; cpm re-derives its
# own reference from the stream after a reset.
#' @noRd
monitor_relearn <- function(method, st, buffer) {
  if (is.null(buffer) || nrow(buffer) == 0) return(st)
  if (method == "edetector") {
    v <- as.numeric(buffer[, 1])
    sd0 <- stats::sd(v)
    if (is.finite(sd0) && sd0 > 0) {
      st$deltas <- st$deltas / st$sd0 * sd0
      st$sd0 <- sd0
    }
    st$mu0 <- mean(v)
    st$R <- rep(0, length(st$deltas))
  } else if (method == "ocd") {
    st$mean <- colMeans(buffer)
    sdv <- apply(buffer, 2, stats::sd)
    sdv[!is.finite(sdv) | sdv == 0] <- 1
    st$sd <- sdv
  }
  st
}

#' The alarm log of a monitor
#'
#' @param x A \code{ggcpt_monitor} object.
#' @param ... Ignored.
#' @return A tibble with one row per alarm: \code{time} (the observation at
#'   which it fired, counted from the first one fed to the monitor),
#'   \code{statistic} and \code{threshold}.
#' @seealso \code{\link{cpt_monitor}()}, \code{\link{cpt_delay}()}.
#' @export
#' @examples
#' set.seed(2026)
#' mon <- cpt_monitor("edetector", baseline = rnorm(100))
#' mon <- cpt_update(mon, c(rnorm(50), rnorm(50, 3)))
#' alarms(mon)
alarms <- function(x, ...) {
  UseMethod("alarms")
}

#' @rdname alarms
#' @export
alarms.ggcpt_monitor <- function(x, ...) x$alarms

#' @rdname cpt_monitor
#' @export
tidy.ggcpt_monitor <- function(x, ...) {
  alarms(x)
}

#' @rdname cpt_monitor
#' @param x A \code{ggcpt_monitor} object (for \code{print()}).
#' @export
print.ggcpt_monitor <- function(x, ...) {
  cat("ggcpt_monitor (", x$method,
      if (identical(x$method, "edetector")) " -- native implementation" else "",
      ")\n", sep = "")
  cat("  Baseline observations: ", x$n_baseline, "\n", sep = "")
  cat("  Monitored observations: ", x$t, "\n", sep = "")
  cat("  Alarms: ", nrow(x$alarms), "\n", sep = "")
  cat("  Restart after alarm: ", x$reset,
      if (x$relearn > 0L) paste0(" (re-learning ", x$relearn,
                                 " observations)") else "",
      "\n", sep = "")
  if (nrow(x$alarms) > 0) {
    cat("\n")
    print(x$alarms, n = 10)
  }
  invisible(x)
}

#' @rdname cpt_monitor
#' @param object A \code{ggcpt_monitor} object (for \code{autoplot()}).
#' @param plot_type \code{"timeline"} (the monitored series with the alarms
#'   marked), \code{"statistic"} (the running detection statistic against its
#'   threshold) or \code{"runlength"} (the gaps between alarms, which
#'   estimate the run length). For a multivariate monitor the timeline draws
#'   the first coordinate — the alarms are shared, so the rules are right
#'   whichever coordinate is shown, but the line is one of several.
#' @export
autoplot.ggcpt_monitor <- function(object,
                                   plot_type = c("timeline", "statistic",
                                                 "runlength"),
                                   ...) {
  plot_type <- match.arg(plot_type)
  al <- object$alarms
  d <- tibble::tibble(time = seq_along(object$data), value = object$data)

  if (plot_type == "timeline") {
    p <- ggplot2::ggplot(d, ggplot2::aes(time, value)) +
      ggplot2::geom_line(colour = "grey40") +
      ggplot2::labs(x = "Observation", y = "Value",
                    title = paste0("Monitoring timeline (", object$method,
                                   ")"),
                    subtitle = paste0(nrow(al), " alarm(s) in ", object$t,
                                      " observations"))
    if (nrow(al) > 0) {
      p <- p + ggplot2::geom_vline(xintercept = al$time, colour = "#D55E00",
                                   linetype = "dashed", linewidth = 0.5)
    }
    return(p)
  }

  if (plot_type == "statistic") {
    if (nrow(al) == 0) {
      stop("No alarm has fired, so there is no statistic history to draw. ",
           "The running statistic is only recorded at alarms; use ",
           "cpt_replay() for a full statistic trace.", call. = FALSE)
    }
    return(
      ggplot2::ggplot(al, ggplot2::aes(time, statistic)) +
        ggplot2::geom_point(size = 2, colour = "#0072B2") +
        ggplot2::geom_hline(ggplot2::aes(yintercept = threshold),
                            linetype = "dotted", colour = "grey30",
                            na.rm = TRUE) +
        ggplot2::labs(x = "Observation", y = "Detection statistic",
                      title = "Statistic at each alarm")
    )
  }

  # runlength
  if (nrow(al) < 2) {
    stop("At least two alarms are needed to show run lengths; this monitor ",
         "has ", nrow(al), ".", call. = FALSE)
  }
  rl <- tibble::tibble(run_length = diff(c(0L, al$time)))
  ggplot2::ggplot(rl, ggplot2::aes(run_length)) +
    ggplot2::geom_histogram(bins = max(5, min(30, nrow(rl))),
                            fill = "#0072B2", colour = "white") +
    ggplot2::geom_vline(xintercept = mean(rl$run_length), colour = "#D55E00",
                        linetype = "dashed") +
    ggplot2::labs(x = "Run length (observations between alarms)",
                  y = "Count",
                  title = "Run-length distribution",
                  subtitle = paste0("Mean run length ",
                                    format(mean(rl$run_length), digits = 4)))
}

#' Replay a series through a sequential detector
#'
#' Runs a whole series through a monitor in one call, so the alarm timeline
#' of an online method can be studied retrospectively without hand-rolling
#' the update loop. This is the honest version of what 0.4.0's \code{cpm},
#' \code{ocd} and \code{bocpd} wrappers do implicitly: it reports
#' \emph{alarms}, which arrive after the change, rather than pretending they
#' are estimated changepoint locations.
#'
#' @param x A numeric vector, or a matrix with rows as time points.
#' @param method Sequential detector; see \code{\link{cpt_monitor}()}.
#' @param baseline Number of leading observations used as pre-change training
#'   data (an integer), or an explicit baseline vector. Defaults to
#'   \code{min(100, floor(n / 4))}.
#' @param ... Additional arguments passed to \code{\link{cpt_monitor}()}.
#' @return A \code{ggcpt_monitor} that has already consumed the series.
#' @seealso \code{\link{cpt_delay}()}, \code{\link{cpt_monitor}()}.
#' @export
#' @examples
#' set.seed(2026)
#' mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")
#' alarms(mon)
cpt_replay <- function(x, method = c("edetector", "cpm", "ocd"),
                       baseline = NULL, ...) {
  method <- match.arg(method)
  # coerce_series_values()/as_mv_matrix(), not a bare as.numeric(): a factor
  # coerces to its LEVEL CODES, which for labels like "10", "2", "30" is the
  # alphabetical order 3, 1, 2 rather than the numbers -- so a replay ran on
  # a series the user never supplied. cpt_detect() has refused that since
  # 0.4.0, and as_mv_matrix() already refuses a factor *column*; only the
  # vector path was open.
  X <- if (is.matrix(x) || is.data.frame(x)) as_mv_matrix(x) else {
    matrix(coerce_series_values(x), ncol = 1)
  }
  n <- nrow(X)
  if (n < 10) {
    stop("A replay needs at least 10 observations; got ", n, ".",
         call. = FALSE)
  }
  # Without this, a non-finite value was caught downstream by whichever of
  # cpt_monitor() or cpt_update() happened to receive the slice containing
  # it -- so `cpt_replay(x)` reported a problem with `baseline` for an NA at
  # position 20 and with `new_obs` for one at 95, named an argument the
  # caller never passed, and counted "1 of 45 values" against the baseline
  # slice rather than the series. Checked here, the message describes `x`.
  if (anyNA(X) || any(!is.finite(X))) {
    stop_nonfinite(X)
  }
  if (is.null(baseline)) baseline <- min(100L, floor(n / 4))
  # `baseline` is documented two ways -- a count of leading observations, or
  # an explicit series -- so it needs the same type guard as `x` above before
  # either reading is applied. The explicit-series branch used a bare
  # as.numeric(), and it hands cpt_monitor() an already-numeric vector, so
  # cpt_monitor()'s own guard could never see the factor.
  if (!is.matrix(baseline) && !is.data.frame(baseline)) {
    baseline <- coerce_series_values(baseline, arg = "baseline")
  }
  if (length(baseline) == 1L) {
    # A lone number is the count. It used to fall through to the
    # explicit-series branch whenever it was fractional or out of range,
    # which turned it into a ONE-point baseline: cpt_replay(x, baseline =
    # 500) on a 180-point series then failed with "needs at least 5
    # pre-change observations" and never mentioned the 500.
    if (is.na(baseline) || baseline %% 1 != 0 || baseline < 1 ||
        baseline >= n) {
      stop("`baseline` as a single number is the count of leading ",
           "observations to train on, so it must be a whole number in ",
           "1..", n - 1L, "; the series has ", n, " observations and ",
           "`baseline` is ", format(baseline), ". Pass an explicit ",
           "baseline series if you meant a value rather than a count.",
           call. = FALSE)
    }
    n_base <- as.integer(baseline)
    base_data <- X[seq_len(n_base), , drop = FALSE]
    rest <- X[seq.int(n_base + 1L, n), , drop = FALSE]
  } else {
    base_data <- if (is.matrix(baseline) || is.data.frame(baseline)) {
      as_mv_matrix(baseline, arg = "baseline")
    } else {
      matrix(baseline, ncol = ncol(X))
    }
    n_base <- 0L
    rest <- X
  }
  mon <- cpt_monitor(method,
                     baseline = if (ncol(base_data) == 1) {
                       as.numeric(base_data)
                     } else {
                       base_data
                     },
                     ...)
  mon <- cpt_update(mon, rest)
  mon$offset <- n_base
  mon
}

#' Detection delay and false-alarm rate
#'
#' Scores an online result the way the sequential literature does: how long
#' after each true change did the first alarm arrive, and how many alarms
#' were raised with no change behind them. \code{\link{cpt_metrics}()} is the
#' wrong tool for an online detector — it asks whether the \emph{location}
#' was recovered, which a sequential procedure never claims — and warns if
#' you point it at one.
#'
#' @param object A \code{ggcpt_monitor}, or a tibble of alarms with a
#'   \code{time} column.
#' @param truth Integer vector of true changepoint positions, on the same
#'   clock as the alarms. When \code{object} is a monitor built by
#'   \code{\link{cpt_replay}()} the baseline offset is applied automatically.
#' @param max_delay Alarms further than this after a change are treated as
#'   false alarms rather than late detections. Defaults to \code{Inf}.
#' @return A \code{ggcpt_delay} object: a list with \code{per_change} (one
#'   row per true change: \code{truth}, \code{alarm}, \code{delay},
#'   \code{detected}), \code{false_alarms}, and the summary statistics
#'   \code{mean_delay}, \code{median_delay}, \code{n_false_alarms} and
#'   \code{arl} (mean observations per false alarm).
#' @seealso \code{\link{cpt_monitor}()}, \code{\link{cpt_replay}()}.
#' @export
#' @examples
#' set.seed(2026)
#' mon <- cpt_replay(c(rnorm(200), rnorm(200, 3)), method = "edetector")
#' cpt_delay(mon, truth = 200)
cpt_delay <- function(object, truth, max_delay = Inf) {
  if (missing(truth)) {
    stop("`truth` is required: detection delay is measured from the true ",
         "changepoint(s), so there is nothing to measure without them. Pass ",
         "the location(s) as an integer vector.", call. = FALSE)
  }
  al <- if (inherits(object, "ggcpt_monitor")) {
    a <- object$alarms
    off <- object$offset %||% 0L
    a$time <- a$time + off
    a
  } else if (is.data.frame(object) && "time" %in% names(object)) {
    tibble::as_tibble(object)
  } else {
    stop("`object` must be a ggcpt_monitor or a tibble with a `time` column.",
         call. = FALSE)
  }
  truth <- as_cp_locations(truth, "truth", sort = TRUE)
  if (length(truth) == 0 || anyNA(truth) || any(truth < 1)) {
    stop("`truth` must be one or more positive changepoint locations.",
         call. = FALSE)
  }
  n_obs <- if (inherits(object, "ggcpt_monitor")) {
    object$t + (object$offset %||% 0L)
  } else {
    max(c(al$time, truth), na.rm = TRUE)
  }
  # A truth past the end of the stream can never be detected, so scoring it
  # would report a miss that says more about the argument than the detector.
  if (inherits(object, "ggcpt_monitor") && any(truth > n_obs)) {
    stop("`truth` runs past the end of the stream: ",
         paste(utils::head(truth[truth > n_obs], 5), collapse = ", "),
         " vs ", n_obs, " observations seen.", call. = FALSE)
  }

  used <- rep(FALSE, nrow(al))
  rows <- lapply(seq_along(truth), function(i) {
    k <- truth[i]
    next_change <- if (i < length(truth)) truth[i + 1] else Inf
    cand <- which(!used & al$time >= k & al$time < next_change &
                    al$time - k <= max_delay)
    if (length(cand) == 0) {
      return(tibble::tibble(truth = k, alarm = NA_integer_,
                            delay = NA_real_, detected = FALSE))
    }
    j <- cand[1]
    used[j] <<- TRUE
    tibble::tibble(truth = k, alarm = as.integer(al$time[j]),
                   delay = as.numeric(al$time[j] - k), detected = TRUE)
  })
  per_change <- do.call(rbind, rows)
  false_alarms <- al[!used, , drop = FALSE]

  structure(
    list(
      per_change = per_change,
      false_alarms = false_alarms,
      mean_delay = mean(per_change$delay, na.rm = TRUE),
      median_delay = stats::median(per_change$delay, na.rm = TRUE),
      n_false_alarms = nrow(false_alarms),
      n_detected = sum(per_change$detected),
      n_changes = length(truth),
      arl = if (nrow(false_alarms) > 0) n_obs / nrow(false_alarms) else Inf,
      n_obs = n_obs
    ),
    class = "ggcpt_delay"
  )
}

#' @rdname cpt_delay
#' @export
tidy.ggcpt_delay <- function(x, ...) {
  x$per_change
}

#' @rdname cpt_delay
#' @export
glance.ggcpt_delay <- function(x, ...) {
  tibble::tibble(n_changes = x$n_changes, n_detected = x$n_detected,
                 mean_delay = x$mean_delay, median_delay = x$median_delay,
                 n_false_alarms = x$n_false_alarms, arl = x$arl,
                 n_obs = x$n_obs)
}

#' @rdname cpt_delay
#' @param x A \code{ggcpt_delay} object.
#' @param ... Ignored.
#' @export
print.ggcpt_delay <- function(x, ...) {
  cat("ggcpt_delay\n")
  cat("  True changes:      ", x$n_changes, "\n", sep = "")
  cat("  Detected:          ", x$n_detected, "\n", sep = "")
  cat("  Mean delay:        ",
      if (is.nan(x$mean_delay)) "-" else format(x$mean_delay, digits = 4),
      "\n", sep = "")
  cat("  Median delay:      ",
      if (is.na(x$median_delay)) "-" else format(x$median_delay, digits = 4),
      "\n", sep = "")
  cat("  False alarms:      ", x$n_false_alarms, "\n", sep = "")
  cat("  Average run length: ",
      if (is.infinite(x$arl)) "no false alarms" else format(x$arl, digits = 4),
      "\n", sep = "")
  cat("\n")
  print(x$per_change, n = 10)
  invisible(x)
}

#' @rdname cpt_delay
#' @param object A \code{ggcpt_delay} object (for \code{autoplot()}).
#' @export
autoplot.ggcpt_delay <- function(object, ...) {
  d <- object$per_change
  if (all(!d$detected)) {
    stop("No change was detected, so there are no delays to draw.",
         call. = FALSE)
  }
  ggplot2::ggplot(d, ggplot2::aes(factor(truth), delay)) +
    ggplot2::geom_col(ggplot2::aes(fill = detected), width = 0.6,
                      na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#0072B2",
                                          `FALSE` = "grey70"),
                               name = "Detected") +
    ggplot2::labs(x = "True changepoint", y = "Detection delay (observations)",
                  title = "Detection delay",
                  subtitle = paste0(object$n_false_alarms,
                                    " false alarm(s); average run length ",
                                    if (is.infinite(object$arl)) {
                                      "infinite"
                                    } else {
                                      format(object$arl, digits = 4)
                                    }))
}
