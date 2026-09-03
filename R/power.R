# ---------------------------------------------------------------------------
# Theme M: simulation, power and study design.
#
# cpt_simulate() answers "what does a series with a change look like?".
# These answer the question that should come first: "would I be able to
# detect a change this small at all?" -- and the study-design version,
# "how big would the change have to be?".
# ---------------------------------------------------------------------------

#' Detection power for a changepoint scenario
#'
#' Simulates replicate series under a scenario and reports how often the
#' detector finds the change, how accurately it locates it, and how often it
#' reports changes that are not there. This is the calculation that should
#' precede an analysis, and the one applied papers are increasingly asked
#' for.
#'
#' @param n Series length.
#' @param jump Size of the change, in units of \code{sigma}. A vector runs
#'   one scenario per value.
#' @param sigma Noise standard deviation. Defaults to \code{1}.
#' @param method Detection method. Defaults to \code{"pelt"}.
#' @param location Changepoint position, as a fraction of \code{n} in
#'   \eqn{(0, 1)} or an integer position. Defaults to \code{0.5}. A vector
#'   runs one scenario per value.
#' @param n_sim Replicates per scenario. Defaults to \code{200}.
#' @param tolerance A detection counts as finding the change when it falls
#'   within this many positions of it. Defaults to \code{5}.
#' @param change_in What changes. Defaults to \code{"mean"}.
#' @param noise Noise model, passed to \code{\link{cpt_simulate}()}.
#' @param rho AR(1) parameter when \code{noise = "ar1"}.
#' @param df Degrees of freedom when \code{noise = "t"}.
#' @param seed Optional seed.
#' @param parallel Use \code{future::plan()} when \pkg{future.apply} is
#'   available? Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()}.
#'
#' @return A \code{ggcpt_power} object: a tibble with one row per scenario —
#'   \code{n}, \code{jump}, \code{sigma}, \code{location}, \code{power}
#'   (proportion of replicates detecting the change within
#'   \code{tolerance}), \code{mc_se} (the Monte Carlo standard error of that
#'   proportion), \code{mean_abs_error} (location error among detections),
#'   \code{false_positive_rate} (mean number of \emph{extra} changepoints per
#'   replicate) and \code{n_sim} — with \code{print()} and
#'   \code{autoplot()}.
#' @seealso \code{\link{cpt_min_detectable}()}, \code{\link{cpt_scenarios}()},
#'   \code{\link{cpt_simulate}()}.
#' @export
#' @examples
#' \donttest{
#' pw <- cpt_power(n = 200, jump = c(0.5, 1, 2), n_sim = 30, seed = 1)
#' pw
#' ggplot2::autoplot(pw)
#' }
cpt_power <- function(n, jump, sigma = 1, method = "pelt", location = 0.5,
                      n_sim = 200, tolerance = 5, change_in = "mean",
                      noise = "gauss", rho = 0, df = 3, seed = NULL,
                      parallel = TRUE, ...) {
  validate_scalar(n_sim, "n_sim", min = 1)
  validate_scalar(tolerance, "tolerance", min = 0)
  validate_flag(parallel, "parallel")
  change_in <- match.arg(change_in, c("mean", "var", "meanvar", "slope"))
  # Fail here rather than in every replicate. An unsupported combination
  # (pelt with change_in = "slope", say) makes each detection error, each
  # error is caught, and the run reports a power of NaN -- which reads like
  # "no power" rather than "you asked for something this method cannot do".
  validate_method_change_in(method, change_in)
  scen <- expand.grid(n = as.integer(n), jump = as.numeric(jump),
                      sigma = as.numeric(sigma),
                      location = as.numeric(location),
                      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  if (!is.null(seed)) set.seed(seed)

  has_future <- isTRUE(parallel) &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  run_scenario <- function(i) {
    ni <- scen$n[i]
    cp <- if (scen$location[i] > 0 && scen$location[i] < 1) {
      as.integer(round(scen$location[i] * ni))
    } else {
      as.integer(scen$location[i])
    }
    cp <- max(2L, min(cp, ni - 2L))
    params <- switch(change_in,
      mean = c(0, scen$jump[i] * scen$sigma[i]),
      var = c(scen$sigma[i], scen$sigma[i] * (1 + scen$jump[i])),
      meanvar = list(list(mean = 0, sd = scen$sigma[i]),
                     list(mean = scen$jump[i] * scen$sigma[i],
                          sd = scen$sigma[i])),
      slope = list(list(intercept = 0, slope = 0),
                   list(intercept = 0,
                        slope = scen$jump[i] * scen$sigma[i] / ni))
    )
    reps <- lapply(seq_len(as.integer(n_sim)), function(b) {
      d <- cpt_simulate(ni, changepoints = cp, change_in = change_in,
                        params = params, noise = noise,
                        sd = scen$sigma[i], rho = rho, df = df)
      det <- tryCatch(
        cpt_detect(d$value, method = method, change_in = change_in,
                   ...)$changepoints$cp,
        error = function(e) NULL
      )
      if (is.null(det)) return(c(hit = NA, err = NA, extra = NA))
      near <- abs(det - cp) <= tolerance
      c(hit = as.numeric(any(near)),
        err = if (any(near)) min(abs(det - cp)) else NA_real_,
        extra = sum(!near))
    })
    m <- do.call(rbind, reps)
    hits <- m[, "hit"]
    ok <- sum(!is.na(hits))
    power <- mean(hits, na.rm = TRUE)
    tibble::tibble(
      n = ni, jump = scen$jump[i], sigma = scen$sigma[i],
      location = cp,
      power = power,
      mc_se = if (ok > 0) sqrt(power * (1 - power) / ok) else NA_real_,
      mean_abs_error = mean(m[, "err"], na.rm = TRUE),
      false_positive_rate = mean(m[, "extra"], na.rm = TRUE),
      n_sim = ok
    )
  }

  rows <- if (has_future) {
    future.apply::future_lapply(seq_len(nrow(scen)),
                                with_session_registry(run_scenario),
                                future.seed = seed %||% TRUE)
  } else {
    lapply(seq_len(nrow(scen)), run_scenario)
  }
  out <- do.call(rbind, rows)
  attr(out, "method") <- method
  attr(out, "change_in") <- change_in
  attr(out, "tolerance") <- tolerance
  attr(out, "noise") <- noise
  class(out) <- c("ggcpt_power", class(out))
  out
}

#' @rdname cpt_power
#' @export
tidy.ggcpt_power <- function(x, ...) {
  # The object already is one row per scenario; tidy() drops the subclass so
  # the result behaves like any other tibble downstream.
  tibble::as_tibble(unclass_keep_tbl(x))
}

#' @rdname cpt_power
#' @param x A \code{ggcpt_power} object.
#' @export
print.ggcpt_power <- function(x, ...) {
  cat("ggcpt_power (method: ", attr(x, "method"), ", change in ",
      attr(x, "change_in"), ", ", attr(x, "noise"), " noise, tolerance ",
      attr(x, "tolerance"), ")\n", sep = "")
  cat("  ", nrow(x), " scenario(s), ", max(x$n_sim),
      " replicates each\n\n", sep = "")
  print(tibble::as_tibble(x), n = 15)
  cat("\nMonte Carlo standard errors are in `mc_se`; a power of 0.80 from ",
      max(x$n_sim), "\nreplicates is only known to about +/- ",
      format(1.96 * sqrt(0.8 * 0.2 / max(x$n_sim)), digits = 2), ".\n",
      sep = "")
  invisible(x)
}

#' @rdname cpt_power
#' @param object A \code{ggcpt_power} object (for \code{autoplot()}).
#' @export
autoplot.ggcpt_power <- function(object, ...) {
  d <- tibble::as_tibble(object)
  d$lower <- pmax(0, d$power - 1.96 * d$mc_se)
  d$upper <- pmin(1, d$power + 1.96 * d$mc_se)
  multi_n <- length(unique(d$n)) > 1

  p <- ggplot2::ggplot(d, ggplot2::aes(jump, power))
  if (multi_n) {
    p <- p + ggplot2::aes(colour = factor(n), group = factor(n)) +
      ggplot2::labs(colour = "n")
  }
  # A single change size is one point per curve: a ribbon and a line both
  # draw nothing there, and geom_line() advises adjusting the group
  # aesthetic on a plot that is already right. Show the Monte Carlo interval
  # as a range instead, so the one thing the figure promises is visible.
  single <- length(unique(d$jump)) < 2
  p <- p + if (single) {
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper),
                            linewidth = 0.7, na.rm = TRUE)
  } else {
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.15, colour = NA, na.rm = TRUE)
  }
  if (!single) {
    p <- p + ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)
  }
  p +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dotted",
                        colour = "grey40") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Change size (standard deviations)",
                  y = "Detection probability",
                  title = paste0("Power curve (", attr(object, "method"),
                                 ")"),
                  subtitle = paste0(
                    if (single) "Vertical range: " else "Shaded band: ",
                    "95% Monte Carlo interval; dotted line at 0.80"))
}

#' The smallest detectable change
#'
#' Inverts \code{\link{cpt_power}()}: searches for the change size at which
#' the detector reaches a target power. The study-design counterpart of a
#' power curve, and the number that belongs in a pre-registration.
#'
#' @inheritParams cpt_power
#' @param power Target detection probability. Defaults to \code{0.8}.
#' @param range Search range for the change size, in standard deviations.
#'   Defaults to \code{c(0.1, 5)}.
#' @param n_sim Replicates per evaluation. Defaults to \code{100}; the answer
#'   is only as precise as this makes it, and the returned object records the
#'   Monte Carlo interval at the solution.
#' @param tol Bisection tolerance on the change size. Defaults to
#'   \code{0.05}.
#' @param max_iter Maximum bisection steps. Defaults to \code{12}.
#' @return A list with \code{jump} (the smallest change reaching
#'   \code{power}), \code{achieved_power}, \code{mc_se}, and the
#'   \code{trace} of evaluations, with a \code{print()} method.
#' @seealso \code{\link{cpt_power}()}.
#' @export
#' @examples
#' \donttest{
#' cpt_min_detectable(n = 200, n_sim = 30, max_iter = 4, seed = 1)
#' }
cpt_min_detectable <- function(n, sigma = 1, method = "pelt", power = 0.8,
                               range = c(0.1, 5), n_sim = 100,
                               tolerance = 5, change_in = "mean",
                               location = 0.5, noise = "gauss", rho = 0,
                               df = 3, tol = 0.05, max_iter = 12,
                               seed = NULL, ...) {
  validate_scalar(power, "power", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_scalar(tol, "tol", min = 0, min_open = TRUE)
  validate_scalar(max_iter, "max_iter", min = 1)
  if (length(range) != 2 || range[1] >= range[2] || range[1] <= 0) {
    stop("`range` must be two increasing positive numbers.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  eval_at <- function(j) {
    r <- cpt_power(n = n, jump = j, sigma = sigma, method = method,
                   location = location, n_sim = n_sim,
                   tolerance = tolerance, change_in = change_in,
                   noise = noise, rho = rho, df = df, parallel = FALSE, ...)
    c(jump = j, power = r$power[1], mc_se = r$mc_se[1])
  }

  lo <- range[1]; hi <- range[2]
  trace <- list(eval_at(lo), eval_at(hi))
  if (trace[[2]]["power"] < power) {
    out <- structure(
      list(jump = NA_real_, achieved_power = trace[[2]][["power"]],
           mc_se = trace[[2]][["mc_se"]], target = power,
           trace = tibble::as_tibble(do.call(rbind, trace)),
           note = paste0("Even the largest change tried (", hi,
                         " sd) reached only ",
                         format(trace[[2]][["power"]], digits = 3),
                         " power. Widen `range`, lengthen the series, or ",
                         "loosen `tolerance`.")),
      class = "ggcpt_min_detectable")
    return(out)
  }
  if (trace[[1]]["power"] >= power) {
    out <- structure(
      list(jump = lo, achieved_power = trace[[1]][["power"]],
           mc_se = trace[[1]][["mc_se"]], target = power,
           trace = tibble::as_tibble(do.call(rbind, trace)),
           note = paste0("The smallest change tried (", lo,
                         " sd) already reaches the target; the answer is ",
                         "at or below it.")),
      class = "ggcpt_min_detectable")
    return(out)
  }

  best <- hi
  best_row <- trace[[2]]
  for (k in seq_len(as.integer(max_iter))) {
    if (hi - lo <= tol) break
    mid <- (lo + hi) / 2
    r <- eval_at(mid)
    trace[[length(trace) + 1L]] <- r
    if (r[["power"]] >= power) {
      hi <- mid; best <- mid; best_row <- r
    } else {
      lo <- mid
    }
  }

  structure(
    list(jump = best, achieved_power = best_row[["power"]],
         mc_se = best_row[["mc_se"]], target = power,
         trace = tibble::as_tibble(do.call(rbind, trace)),
         note = NULL),
    class = "ggcpt_min_detectable")
}

#' @rdname cpt_min_detectable
#' @param x A \code{ggcpt_min_detectable} object.
#' @param ... Ignored.
#' @export
print.ggcpt_min_detectable <- function(x, ...) {
  cat("Smallest detectable change\n")
  cat("  Target power:      ", format(x$target), "\n", sep = "")
  cat("  Change size:       ",
      if (is.na(x$jump)) "not reached" else paste0(format(x$jump, digits = 3),
                                                   " standard deviations"),
      "\n", sep = "")
  cat("  Achieved power:    ", format(x$achieved_power, digits = 3),
      " (Monte Carlo SE ", format(x$mc_se, digits = 2), ")\n", sep = "")
  if (!is.null(x$note)) cat("\n  ", x$note, "\n", sep = "")
  cat("\n")
  print(x$trace[order(x$trace$jump), ], n = 15)
  invisible(x)
}

#' A grid of simulation scenarios
#'
#' Builds the scenario grid a simulation study varies over, as data rather
#' than as nested loops, so it can be inspected, filtered, subsetted and
#' passed straight to \code{\link{cpt_benchmark}()}.
#'
#' @param n Series lengths.
#' @param jump Change sizes, in standard deviations.
#' @param location Change positions, as fractions of \code{n}.
#' @param noise Noise models; any value \code{\link{cpt_simulate}()} accepts.
#' @param rho AR(1) parameters (used by \code{noise = "ar1"}).
#' @param change_in Change types.
#' @param n_rep Replicates per scenario. Defaults to \code{1}.
#' @param seed Base seed; replicate \code{r} of scenario \code{i} uses
#'   \code{seed + (i - 1) * n_rep + r}, so the whole grid is reproducible and
#'   every cell is independent.
#' @param as_datasets Return simulated datasets in
#'   \code{\link{cpt_benchmark}()}'s shape (the default), or just the
#'   scenario table?
#' @return A named list of datasets, or a tibble of scenarios when
#'   \code{as_datasets = FALSE}.
#' @seealso \code{\link{cpt_power}()}, \code{\link{cpt_benchmark}()}.
#' @export
#' @examples
#' scen <- cpt_scenarios(n = 200, jump = c(1, 3), as_datasets = FALSE)
#' scen
cpt_scenarios <- function(n = 500, jump = c(0.5, 1, 2), location = 0.5,
                          noise = "gauss", rho = 0.5, change_in = "mean",
                          n_rep = 1, seed = 1, as_datasets = TRUE) {
  validate_flag(as_datasets, "as_datasets")
  validate_scalar(n_rep, "n_rep", min = 1)
  scen <- expand.grid(n = as.integer(n), jump = as.numeric(jump),
                      location = as.numeric(location), noise = noise,
                      change_in = change_in, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  scen$rho <- ifelse(scen$noise == "ar1", rho, 0)
  scen$scenario <- paste0(scen$change_in, "_n", scen$n, "_j", scen$jump,
                          "_", scen$noise)
  scen$scenario <- make.unique(scen$scenario)
  if (!as_datasets) return(tibble::as_tibble(scen))

  out <- list()
  clamped <- character()
  for (i in seq_len(nrow(scen))) {
    want <- as.integer(round(scen$location[i] * scen$n[i]))
    cp <- max(2L, min(want, scen$n[i] - 2L))
    # The table keeps the requested fraction, so a clamp makes the scenario
    # row and the data it generated disagree about where the change is.
    if (!identical(cp, want)) {
      clamped <- c(clamped, sprintf("location %s x n %d -> %d, not %d",
                                    format(scen$location[i]), scen$n[i],
                                    cp, want))
    }
    params <- switch(scen$change_in[i],
      mean = c(0, scen$jump[i]),
      var = c(1, 1 + scen$jump[i]),
      meanvar = list(list(mean = 0, sd = 1),
                     list(mean = scen$jump[i], sd = 1 + scen$jump[i] / 2)),
      slope = list(list(intercept = 0, slope = 0),
                   list(intercept = 0, slope = scen$jump[i] / scen$n[i]))
    )
    for (r in seq_len(as.integer(n_rep))) {
      d <- cpt_simulate(scen$n[i], changepoints = cp,
                        change_in = scen$change_in[i], params = params,
                        noise = scen$noise[i], rho = scen$rho[i],
                        seed = seed + (i - 1L) * as.integer(n_rep) + r)
      nm <- if (n_rep == 1) scen$scenario[i] else {
        paste0(scen$scenario[i], "_r", r)
      }
      out[[nm]] <- list(series = as.numeric(d$value),
                        annotations = list(as.integer(cp)))
    }
  }
  if (length(clamped) > 0) {
    warning("`location` is a fraction of `n`; ", length(clamped),
            " scenario(s) asked for a position outside 2..(n - 2) and were ",
            "moved: ", paste(unique(clamped), collapse = "; "),
            ". The scenario table still reports the requested fraction.",
            call. = FALSE)
  }
  out
}
