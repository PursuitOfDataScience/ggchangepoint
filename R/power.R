# ---------------------------------------------------------------------------
# Theme M: simulation, power and study design.
#
# cpt_simulate() answers "what does a series with a change look like?".
# These answer the question that should come first: "would I be able to
# detect a change this small at all?" -- and the study-design version,
# "how big would the change have to be?".
# ---------------------------------------------------------------------------

# Internal: `location` in the power helpers is either a fraction of n in
# (0, 1) or an absolute integer position, elementwise, and may be a vector.
#' @noRd
validate_location <- function(location, n) {
  if (!is.numeric(location) || length(location) == 0L ||
      anyNA(location) || any(!is.finite(location))) {
    stop("`location` must be finite numbers: a fraction of `n` in (0, 1), ",
         "or an integer position. Got ",
         paste(format(utils::head(location, 4)), collapse = ", "), ".",
         call. = FALSE)
  }
  # The SMALLEST n, not the largest: an integer position is checked against
  # every scenario it will be used in, and `n = c(50, 500)` with
  # `location = 400` passed against 500 and was then silently clamped to 48
  # in the n = 50 scenario. A fraction of n is scale-free and needs no such
  # check.
  nmin <- suppressWarnings(min(as.numeric(n)))
  frac <- location > 0 & location < 1
  pos <- !frac & location == round(location) &
    location >= 1 & location <= nmin - 1
  bad <- !(frac | pos)
  if (any(bad)) {
    stop("`location` must be a fraction of `n` in (0, 1) or an integer ",
         "position in [1, ", format(nmin - 1),
         if (length(n) > 1) paste0("] (the shortest of the ", length(n),
                                   " series lengths is ", format(nmin))
         else "", "]; ",
         paste(format(location[bad]), collapse = ", "),
         if (sum(bad) > 1) " are not." else " is not.", call. = FALSE)
  }
  invisible(TRUE)
}

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
#' @param seed Optional seed. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
#' @param parallel Use \code{future::plan()} when \pkg{future.apply} is
#'   available? Defaults to \code{TRUE}. It has no effect unless a
#'   non-sequential plan is set, but when one is it changes where the
#'   replicates' random numbers come from -- see the section below, which
#'   matters if the power figure is going into a paper.
#' @param ... Additional arguments passed to \code{\link{cpt_detect}()}.
#'
#' @section Reproducibility under a parallel plan:
#' A seeded call is reproducible \strong{for a given} \code{future::plan()},
#' and not across plans. Under a parallel plan the replicates' random numbers
#' come from \pkg{future.apply}'s parallel-safe L'Ecuyer streams, derived
#' from \code{seed}; run sequentially they come from the calling stream that
#' \code{seed} set. Both are deterministic, and they are not the same
#' numbers. Measured on two scenarios at \code{n_sim = 8}, one and the same
#' \code{seed = 11} gave \code{power = 0, 1} sequentially and
#' \code{0.125, 0.875} on two workers.
#'
#' So the guarantee is: same seed and same plan, same answer -- every time,
#' whichever plan it is. If a power figure needs to be reproducible by
#' someone else, pin the execution as well as the seed: pass
#' \code{parallel = FALSE}, or state the plan alongside the seed. Raising
#' \code{n_sim} narrows the gap, because it is Monte Carlo error rather
#' than disagreement -- both estimates are of the same quantity, and
#' \code{mc_se} says how precisely.
#'
#' This is specific to \code{cpt_power()}, which is the one function here
#' whose parallel tasks consume random numbers. The other six that dispatch
#' on \code{future::plan()} -- \code{\link{cpt_benchmark}()},
#' \code{\link{cpt_batch}()}, \code{\link{cpt_consensus}()},
#' \code{\link{cpt_influence}()}, \code{\link{cpt_sensitivity}()} and
#' \code{\link{ggcpt_compare}()} -- farm out work that is deterministic
#' given its input, and were measured to return identical results under a
#' sequential and a two-worker plan, stochastic engines included.
#'
#' @return A \code{ggcpt_power} object: a tibble with one row per scenario —
#'   \code{n}, \code{jump}, \code{sigma}, \code{location}, \code{power}
#'   (proportion of replicates detecting the change within
#'   \code{tolerance}), \code{mc_se} (the Monte Carlo standard error of that
#'   proportion), \code{mean_abs_error} (location error among detections),
#'   \code{mean_abs_error} is \code{NaN} when no replicate detected a
#'   changepoint within \code{tolerance} of the true one --- there is no
#'   distance to average --- and \code{power} reads \code{0} in the same
#'   row.
#'   \code{false_positives} (mean number of \emph{extra} changepoints per
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
  # `sigma` reaches cpt_simulate() as its `sd`, so without a check here a bad
  # value was reported as "`sd` must be a single finite number" -- naming an
  # argument neither this function nor cpt_min_detectable() has.
  validate_scalar(sigma, "sigma", min = 0)
  # `location` is documented as "a fraction of n in (0, 1) or an integer
  # position", and a vector runs one scenario per value -- so it cannot go
  # through validate_scalar(). It went through nothing at all, and the
  # scenario loop clamps with max(2, min(cp, n - 2)), so an out-of-range
  # integer was silently moved and a power figure reported for a changepoint
  # nobody asked about: `location = 1e6` at n = 200 answered "power 0.75"
  # for a change at 198. The clamp is right for an extreme *fraction* (0.001
  # rounds to 0, and a changepoint needs observations either side); what was
  # missing is refusing positions that are not positions.
  validate_location(location, n)
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
  local_seed(seed)

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
    # A single replicate may legitimately fail -- an engine can error on one
    # unlucky draw -- and one bad draw must not abort a 500-replicate run,
    # which is why the tryCatch is here. But when EVERY replicate fails the
    # rate below is mean(all-NA) = NaN, and returning that silently is the
    # problem: a bad argument forwarded through `...` reaches cpt_detect(),
    # every call dies, and the caller gets `power = NaN` -- a number they
    # could plot or publish -- instead of the perfectly clear "unused
    # argument" the engine already raised. So the first error is kept.
    first_err <- NULL
    reps <- lapply(seq_len(as.integer(n_sim)), function(b) {
      d <- cpt_simulate(ni, changepoints = cp, change_in = change_in,
                        params = params, noise = noise,
                        sd = scen$sigma[i], rho = rho, df = df)
      det <- tryCatch(
        cpt_detect(d$value, method = method, change_in = change_in,
                   ...)$changepoints$cp,
        error = function(e) {
          if (is.null(first_err)) first_err <<- conditionMessage(e)
          NULL
        }
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
    if (ok == 0L) {
      warning("No replicate completed for n = ", ni, ", jump = ",
              format(scen$jump[i]), ": all ", as.integer(n_sim),
              " detection calls failed, so `power` is NaN rather than a ",
              "rate. The first error was: ",
              if (is.null(first_err)) "unavailable" else first_err,
              call. = FALSE)
    }
    power <- mean(hits, na.rm = TRUE)
    tibble::tibble(
      n = ni, jump = scen$jump[i], sigma = scen$sigma[i],
      location = cp,
      power = power,
      mc_se = if (ok > 0) sqrt(power * (1 - power) / ok) else NA_real_,
      # NaN when no replicate ever detected within the tolerance: there is
      # no distance to average. Deliberately NOT a warning -- a power sweep
      # is expected to include jumps too small to find, and the `power`
      # column already reads 0 in exactly those rows, so a warning per row
      # would fire on correct output. @return says so instead.
      mean_abs_error = mean(m[, "err"], na.rm = TRUE),
      # A COUNT, not a rate: `extra` is the number of detections outside the
      # tolerance window per replicate, so on a scale-mismatched run this
      # reads 137, and `false_positive_rate = 137` invites the reader to see
      # a percentage. @return always described it correctly; the name did
      # not.
      false_positives = mean(m[, "extra"], na.rm = TRUE),
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
  # Checked here as well as in cpt_power(), so the message names `sigma`
  # before the search has spent a single simulation on a value it will
  # reject.
  validate_scalar(sigma, "sigma", min = 0)
  if (length(range) != 2 || range[1] >= range[2] || range[1] <= 0) {
    stop("`range` must be two increasing positive numbers.", call. = FALSE)
  }
  local_seed(seed)

  eval_at <- function(j) {
    r <- cpt_power(n = n, jump = j, sigma = sigma, method = method,
                   location = location, n_sim = n_sim,
                   tolerance = tolerance, change_in = change_in,
                   noise = noise, rho = rho, df = df, parallel = FALSE, ...)
    p <- r$power[1]
    # Without this the NaN from an all-failed scenario reaches
    # `if (trace[[2]]["power"] < power)` and R reports "missing value where
    # TRUE/FALSE needed", which says nothing about the argument at fault.
    if (!is.finite(p)) {
      stop("`cpt_power()` returned a non-finite power at jump = ", format(j),
           ", so the search has nothing to bracket. That happens when every ",
           "replicate fails -- most often because an argument passed through ",
           "`...` is not one `cpt_detect()` accepts. See the warning above ",
           "for the error the detector raised.", call. = FALSE)
    }
    c(jump = j, power = p, mc_se = r$mc_se[1])
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
#'   \code{seed + (i - 1) * n_rep + r}, so the whole grid is reproducible
#'   and every cell is independent. The seed is scoped to this call:
#'   \code{.Random.seed} is saved and restored, so a seeded call inside a
#'   simulation loop does not pin the loop's own stream.
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
  # `seed` is used arithmetically (seed + (i - 1) * n_rep + r) to give each
  # scenario its own stream, so a character seed died in `+` with
  # "non-numeric argument to binary operator" and a length-2 seed silently
  # vectorised.
  validate_scalar(seed, "seed")
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
