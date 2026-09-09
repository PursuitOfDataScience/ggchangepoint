# ---------------------------------------------------------------------------
# Theme A: inference. Two generics with one contract each.
#
#   cpt_confint()  "where could this changepoint be?" -- four provenances
#                  (native / nsp / bootstrap / posterior) behind one tibble,
#                  with a `source` column so the difference is visible in the
#                  data and not only in the docs.
#   cpt_test()     "is this changepoint real?" -- native tests where an
#                  engine has one, an explicitly unadjusted fallback where it
#                  does not, and a `selection_adjusted` column that never
#                  lets the two be confused.
# ---------------------------------------------------------------------------

#' Confidence intervals for changepoint locations
#'
#' Answers "where could this changepoint be?" for any result, and says which
#' of four routes it used. \code{show_ci = TRUE} in \code{autoplot()} works
#' only for the handful of engines that ship intervals of their own; this
#' generic covers the rest, and — because the four routes mean genuinely
#' different things — reports the provenance in a \code{source} column
#' rather than presenting them as interchangeable.
#'
#' @param object A \code{ggcpt} object.
#' @param level Confidence/credible level. Defaults to \code{0.95}. Honoured
#'   by the routes that compute an interval -- \code{"bootstrap"},
#'   \code{"posterior"} and \code{"nsp"} -- and \strong{ignored by}
#'   \code{"native"}, which reports the interval the engine already
#'   computed at whatever level it was asked for. Note that
#'   \code{method = "auto"} resolves to \code{"native"} whenever the engine
#'   supplied one, so an explicit \code{level} can go unused there too: it
#'   is reported in the \code{level} column either way, and supplying a
#'   level the answer does not carry now warns rather than passing
#'   silently. To choose the level yourself, name a computing route.
#' @param method Which route to use:
#'   \describe{
#'     \item{\code{"auto"}}{(default) native if the engine supplied
#'       intervals, else posterior if it supplied one, else bootstrap.}
#'     \item{\code{"native"}}{the engine's own \code{ci_lower}/\code{ci_upper}
#'       (SMUCE/HSMUCE simultaneous confidence sets, \pkg{strucchange}
#'       break-date intervals, \pkg{segmented} breakpoint intervals,
#'       \pkg{mcp} posterior intervals, \pkg{bfast} break confidence
#'       intervals, and \code{taylor}'s bootstrap confidence limits). The
#'       engines that supply them are the ones
#'       \code{\link{cpt_methods}()} marks in its \code{ci} column;
#'       \code{nsp} is marked there too but is reported under its own
#'       provenance below, because its regions are not intervals around an
#'       estimate.}
#'     \item{\code{"nsp"}}{Narrowest Significance Pursuit regions computed on
#'       the same series and matched to the changepoints. These are
#'       \emph{not} intervals around an estimate — see
#'       \code{\link{nsp_wrapper}()} — but they are the strongest guarantee
#'       available, so the mapping is reported as
#'       \code{source = "nsp_region"} and a changepoint in no region gets
#'       \code{NA}.}
#'     \item{\code{"bootstrap"}}{within-segment residual resampling (the
#'       \code{\link{cpt_stability}()} scheme), re-running the detector and
#'       taking quantiles of the re-detected location. Model-agnostic and
#'       available for every engine; it measures the sampling variability of
#'       the \emph{procedure}, conditional on the fitted segmentation, and it
#'       is not exact.}
#'     \item{\code{"posterior"}}{a credible interval from the engine's
#'       posterior changepoint-probability profile (\code{bcp},
#'       \code{beast}).}
#'   }
#' @param B Bootstrap replicates for \code{method = "bootstrap"}. Defaults to
#'   \code{200}.
#' @param seed Optional seed (bootstrap and NSP are both random). The seed
#'   is scoped to this call: \code{.Random.seed} is saved and restored, so a
#'   seeded call inside a simulation loop does not pin the loop's own
#'   stream.
#' @param ... Passed to \code{\link{cpt_detect}()} on the bootstrap
#'   replicates, or to \code{\link{nsp_wrapper}()}.
#'
#' @return A tibble with one row per changepoint: \code{cp},
#'   \code{ci_lower}, \code{ci_upper}, \code{level}, \code{source}, plus
#'   \code{cp_index}/\code{ci_lower_index}/\code{ci_upper_index} on the
#'   original scale when the result carries a time index, and
#'   \code{n_replicates} for \code{method = "bootstrap"} (how many
#'   replicates actually contributed a draw for that changepoint).
#'   \code{method = "nsp"} returns \code{NA} bounds for a changepoint that
#'   falls in no region, which is a finding rather than a failure.
#' @seealso \code{\link{cpt_test}()}, \code{\link{cpt_stability}()},
#'   \code{\link{nsp_wrapper}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(80), rnorm(80, 4))
#' fit <- cpt_detect(x, method = "pelt")
#' cpt_confint(fit, method = "bootstrap", B = 25, seed = 1)
cpt_confint <- function(object, level = 0.95,
                        method = c("auto", "native", "nsp", "bootstrap",
                                   "posterior"),
                        B = 200, seed = NULL, ...) {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  method <- match.arg(method)
  # Whether the caller asked for a level, as opposed to taking the default.
  # Used at the end: only an explicit request is worth warning about.
  level_supplied <- !missing(level)
  requested <- if (level_supplied) level else NA_real_
  validate_scalar(level, "level", min = 0, max = 1,
                  min_open = TRUE, max_open = TRUE)
  validate_scalar(B, "B", min = 1)

  cp <- object$changepoints$cp
  if (length(cp) == 0) {
    return(empty_confint(object))
  }

  native <- native_bounds(object)
  has_native <- !is.null(native)
  has_post <- !is.null(posterior_prob_profile(object))

  if (method == "auto") {
    method <- if (has_native) {
      "native"
    } else if (has_post) {
      "posterior"
    } else {
      "bootstrap"
    }
    # The bootstrap route re-runs the detector by name, which a result built
    # by as_ggcpt() from an outside source cannot support. Say that in terms
    # of what the caller asked for rather than letting the bootstrap branch
    # complain about a method the caller never named.
    if (method == "bootstrap" && !bootstrap_possible(object)) {
      stop("`method = \"auto\"` has nothing to fall back on for this ",
           "result: the engine supplied no interval and no posterior, and ",
           "the bootstrap route needs to re-run the detector, but `",
           scalar_chr(object$method), "` is not a method cpt_detect() ",
           "knows. Supply the interval yourself via as_ggcpt(ci = ), or ",
           "register the detector with cpt_register_method() so it can be ",
           "re-run.", call. = FALSE)
    }
  }

  # Outside the `auto` block on purpose: `auto` would have picked "native"
  # for the engine this matters to (strucchange supplies its own intervals),
  # so the reachable call is the EXPLICIT `method = "bootstrap"` the review
  # named, which skipped every check in that block.
  if (method == "bootstrap" && !rerun_matches_result(object)) {
    stop("This result reports `change_in = \"",
         scalar_chr(object$change_in), "\"`, which `",
         scalar_chr(object$method),
         "` cannot be asked for through cpt_detect() -- so the bootstrap ",
         "would re-run a different model than the one that produced it and ",
         "report the spread of the wrong search as this result's interval. ",
         "A formula fit keeps neither the formula nor `data` on the object. ",
         "Use the engine's own intervals with `method = \"native\"`, or ",
         "supply the interval yourself via as_ggcpt(ci = ).", call. = FALSE)
  }

  out <- switch(method,
    native = {
      if (!has_native) {
        stop("This result carries no engine confidence intervals. Engines ",
             "that supply them: ",
             paste(subset(cpt_methods(), ci %in% TRUE)$method,
                   collapse = ", "),
             ". Use method = \"bootstrap\" for a model-agnostic interval.",
             call. = FALSE)
      }
      tibble::tibble(cp = cp,
                     ci_lower = as.integer(native$lower),
                     ci_upper = as.integer(native$upper),
                     level = native$level, source = native$source)
    },
    posterior = confint_posterior(object, level),
    bootstrap = confint_bootstrap(object, level, B = B, seed = seed, ...),
    nsp = confint_nsp(object, level, seed = seed, ...)
  )

  # Never report an interval that does not contain the point it belongs to,
  # and never one that runs off the series.
  n <- nrow(object$data)
  out$ci_lower <- pmax(1L, pmin(as.integer(out$ci_lower), out$cp))
  out$ci_upper <- pmin(n - 1L, pmax(as.integer(out$ci_upper), out$cp))

  if (!is.null(object$index)) {
    idx <- object$index
    out$cp_index <- idx[out$cp]
    out$ci_lower_index <- idx[out$ci_lower]
    out$ci_upper_index <- idx[out$ci_upper]
  }

  # A route that reports the engine's own level cannot honour the caller's,
  # and `method = "auto"` is where that bites: an `nsp` result carries
  # regions, so "auto" resolves to "native" and reports them at the level
  # nsp_wrapper()'s `alpha` produced -- 0.9 by default. Asking for
  # `level = 0.95` there returned 0.9 with nothing said about it, and the
  # only signal was the `level` column the caller would have to inspect.
  #
  # `?cpt_confint` documented that "native" ignores `level`, which is true
  # and is not the problem: the caller who is surprised asked for "auto".
  # So the check is on the answer rather than on the route -- if the level
  # reported differs from the one requested, say so, whichever branch got
  # there. cpt_monitor() already warns on this pattern for `arl0` and
  # friends; this is the same courtesy.
  if (level_supplied) {
    got <- unique(out$level[is.finite(out$level)])
    if (length(got) > 0 && !any(abs(got - requested) < 1e-9)) {
      warning("`level = ", format(requested), "` was not applied: `method = ",
              "\"", method, "\"` reports the interval the engine already ",
              "computed, at level ", paste(format(got), collapse = "/"),
              ". Use `method = \"bootstrap\"` or `\"nsp\"` for an interval ",
              "computed at the level you ask for.", call. = FALSE)
    }
  }
  out
}

# Internal: the interval an engine supplied itself, in whatever shape it
# supplied it. Most engines fill `ci_lower`/`ci_upper`; NSP instead reports
# an interval that provably contains a change, under `region_start` and
# `region_end`. Reading only the first pair sent every NSP result down the
# bootstrap path -- 200 re-runs of the detector, to produce a weaker
# statement than the one already sitting on the object.
#' @noRd
native_bounds <- function(object) {
  cp_tbl <- object$changepoints
  if (all(c("ci_lower", "ci_upper") %in% names(cp_tbl))) {
    return(list(lower = cp_tbl$ci_lower, upper = cp_tbl$ci_upper,
                level = NA_real_, source = "native"))
  }
  if (all(c("region_start", "region_end") %in% names(cp_tbl))) {
    lvl <- object$region_level
    return(list(lower = cp_tbl$region_start, upper = cp_tbl$region_end,
                level = if (is.null(lvl)) NA_real_ else 1 - as.numeric(lvl),
                source = "nsp_region"))
  }
  NULL
}

# Internal: can this result's detector be re-run? The bootstrap route needs
# to call cpt_detect() by name, which rules out a result assembled by
# as_ggcpt() from somewhere the package cannot reach.
#' @noRd
bootstrap_possible <- function(object) {
  method <- scalar_chr(object$method)
  if (is.na(method)) return(FALSE)
  method %in% builtin_registry()$method || !is.null(registry_get(method))
}

# Internal: can the detector be re-run to reproduce THIS result? Being in the
# registry is not enough. `strucchange_wrapper(y ~ x1 + x2, data = d)` returns
# `change_in = "regression"`, and neither the formula nor `data` is
# recoverable from the object -- `$data$value` is the response alone. So the
# bootstrap resampled the response and re-ran an INTERCEPT-ONLY breakpoint
# search, on a series whose changepoints came from a multi-regressor model,
# and reported the spread of the wrong search as the interval of the right
# one. bootstrap_possible() said TRUE, because the method is in the registry.
#
# The discriminator is the MODEL CONTEXT, not the registry: `"regression"` is
# the one label in cpt_change_in_levels() that describes a fit to covariates
# the result does not carry. (`"covariance"` and `"network"` results keep
# their full input in `$data_wide`, so those re-run correctly.) Keyed on the
# label rather than on `supports`, because `supports` answers a different
# question -- what you may ASK cpt_detect() for -- and widening it to make
# this label "legal" would claim a request route that does not exist.
#' @noRd
rerun_matches_result <- function(object) {
  !identical(scalar_chr(object$change_in), "regression")
}

#' @noRd
empty_confint <- function(object) {
  out <- tibble::tibble(cp = integer(), ci_lower = integer(),
                        ci_upper = integer(), level = numeric(),
                        source = character())
  if (!is.null(object$index)) {
    out$cp_index <- object$index[0]
    out$ci_lower_index <- object$index[0]
    out$ci_upper_index <- object$index[0]
  }
  out
}

# Internal: credible interval from a posterior changepoint-probability
# profile. The mass is allocated within the window bounded by the
# neighbouring changepoints, so two nearby changepoints cannot claim the
# same probability twice, and the interval is the narrowest set of
# consecutive positions around `cp` holding `level` of that window's mass.
#' @noRd
confint_posterior <- function(object, level) {
  prob <- posterior_prob_profile(object)
  if (is.null(prob)) {
    stop("This result carries no posterior changepoint-probability profile; ",
         "bcp and beast supply one. Use method = \"bootstrap\" instead.",
         call. = FALSE)
  }
  cp <- object$changepoints$cp
  n <- length(prob)
  bounds <- c(0L, cp, n)

  lo <- integer(length(cp))
  hi <- integer(length(cp))
  for (i in seq_along(cp)) {
    left <- if (i == 1L) 1L else as.integer(floor((cp[i - 1] + cp[i]) / 2)) + 1L
    right <- if (i == length(cp)) {
      n
    } else {
      as.integer(floor((cp[i] + cp[i + 1]) / 2))
    }
    win <- seq.int(max(1L, left), max(max(1L, left), right))
    p <- prob[win]
    total <- sum(p, na.rm = TRUE)
    if (!is.finite(total) || total <= 0) {
      lo[i] <- cp[i]; hi[i] <- cp[i]; next
    }
    # Grow outwards from the changepoint until `level` of the window's mass
    # is covered: a contiguous interval, which is what a reader expects a
    # location interval to be.
    centre <- match(cp[i], win)
    if (is.na(centre)) centre <- which.max(p)
    l <- centre; r <- centre
    covered <- p[centre]
    while (covered / total < level && (l > 1L || r < length(win))) {
      take_left <- if (l > 1L && r < length(win)) {
        p[l - 1L] >= p[r + 1L]
      } else {
        l > 1L
      }
      if (take_left) {
        l <- l - 1L; covered <- covered + p[l]
      } else {
        r <- r + 1L; covered <- covered + p[r]
      }
    }
    lo[i] <- win[l]; hi[i] <- win[r]
  }
  tibble::tibble(cp = cp, ci_lower = lo, ci_upper = hi, level = level,
                 source = "posterior")
}

# Internal: bootstrap interval. Resamples residuals within the fitted
# segments (preserving the regime structure), re-runs the detector, and for
# each original changepoint records the nearest re-detection. Quantiles of
# that distribution are the interval.
#' @noRd
confint_bootstrap <- function(object, level, B = 200, seed = NULL, ...) {
  method <- object$method
  # Inherit the change type from the result, not just the method.
  #
  # Every entry point that takes raw data forwards `change_in` to
  # cpt_detect() explicitly; every one that takes a finished `ggcpt` read
  # `object$method` and stopped there, leaving `change_in` at its own
  # default of "mean" -- so a meanvar or var fit was silently re-detected as
  # a change in the MEAN. `object$change_in` was on the object the whole
  # time and never read.
  #
  # The symptom pointed away from the cause. On a pure variance change the
  # mean detector finds nothing, every replicate is discarded, and the
  # caller gets a zero-width interval plus a warning blaming the detector
  # for finding no changepoints.
  #
  # `...` still wins, so an explicit `change_in` overrides the object --
  # same precedence cpt_detect() gives `dots` over `derived_args_for()`.
  dots <- list(...)
  if (is.null(dots$change_in)) dots$change_in <- object$change_in %||% "mean"
  if (isTRUE(object$registered) && is.null(registry_get(method))) {
    stop("This result came from a registered method (`", method,
         "`) that is no longer registered, so it cannot be re-run for a ",
         "bootstrap interval.", call. = FALSE)
  }
  if (!bootstrap_possible(object)) {
    stop("A bootstrap interval re-runs the detector, but `", method,
         "` is not a method cpt_detect() knows. Use method = \"native\" or ",
         "\"posterior\" if the engine supplied one, or register the ",
         "detector with cpt_register_method() so it can be re-run.",
         call. = FALSE)
  }

  data_vec <- object$data$value
  n <- length(data_vec)
  cp <- object$changepoints$cp
  seg <- object$segments
  fitted_step <- rep(seg$param_estimate, times = seg$n)
  resid <- data_vec - fitted_step
  seg_id <- rep(seq_len(nrow(seg)), times = seg$n)

  local_seed(seed)

  draws <- matrix(NA_real_, nrow = B, ncol = length(cp))
  for (b in seq_len(B)) {
    resampled <- resid
    for (s in seq_len(nrow(seg))) {
      idx <- which(seg_id == s)
      # `sample.int()` on the index, not `sample()` on the values: R's
      # classic pitfall is that `sample(x, n)` means `sample.int(x, n)` when
      # `x` is a single number >= 1, so a one-observation segment resamples
      # `1:round(resid)` instead of the residual itself. It is currently
      # safe only by accident -- a length-1 segment's residual against its
      # own mean is exactly 0, and `0 >= 1` is FALSE -- which couples this
      # bootstrap to `param_estimate` staying the exact segment mean.
      resampled[idx] <- resid[idx][sample.int(length(idx), length(idx),
                                              replace = TRUE)]
    }
    rep_cp <- tryCatch(
      do.call(cpt_detect,
              c(list(fitted_step + resampled, method = method),
                dots))$changepoints$cp,
      error = function(e) integer(0)
    )
    if (length(rep_cp) == 0) next
    # Nearest re-detection to each original changepoint. A replicate that
    # detects nothing contributes nothing rather than being counted as a
    # detection at the original location, which would shrink the interval.
    draws[b, ] <- vapply(cp, function(k) {
      rep_cp[which.min(abs(rep_cp - k))]
    }, numeric(1))
  }

  a <- (1 - level) / 2
  lo <- integer(length(cp))
  hi <- integer(length(cp))
  n_eff <- integer(length(cp))
  for (j in seq_along(cp)) {
    d <- draws[, j]
    d <- d[is.finite(d)]
    n_eff[j] <- length(d)
    if (length(d) == 0) {
      lo[j] <- cp[j]; hi[j] <- cp[j]; next
    }
    q <- stats::quantile(d, probs = c(a, 1 - a), names = FALSE, type = 1)
    lo[j] <- as.integer(floor(q[1]))
    hi[j] <- as.integer(ceiling(q[2]))
  }
  if (any(n_eff < B)) {
    warning("The detector found no changepoints in ", sum(B - n_eff),
            " of ", B * length(cp), " (replicate, changepoint) draws; those ",
            "draws are excluded, so the interval is based on fewer than `B` ",
            "replicates. `n_replicates` records how many were used.",
            call. = FALSE)
  }

  tibble::tibble(cp = cp, ci_lower = lo, ci_upper = hi, level = level,
                 source = "bootstrap", n_replicates = n_eff)
}

# Internal: NSP regions matched to the changepoints. A changepoint inside a
# region takes that region; one outside every region gets NA rather than
# being handed the nearest, because "no region covers this" is a finding.
#' @noRd
confint_nsp <- function(object, level, seed = NULL, ...) {
  need_pkg("nsp")
  cp <- object$changepoints$cp
  alpha <- 1 - level
  fit <- nsp_wrapper(object$data$value, alpha = alpha, seed = seed, ...)
  reg <- fit$regions

  lo <- rep(NA_integer_, length(cp))
  hi <- rep(NA_integer_, length(cp))
  if (!is.null(reg) && nrow(reg) > 0) {
    for (i in seq_along(cp)) {
      inside <- which(reg$start <= cp[i] & reg$end >= cp[i])
      if (length(inside) > 0) {
        # The narrowest covering region is the sharpest statement available.
        j <- inside[which.min(reg$end[inside] - reg$start[inside])]
        lo[i] <- reg$start[j]
        hi[i] <- reg$end[j]
      }
    }
  }
  if (anyNA(lo)) {
    message(sum(is.na(lo)), " of ", length(cp), " changepoint(s) fall in no ",
            "NSP region at global level ", format(alpha),
            "; those rows are NA. A changepoint in no region is not ",
            "supported by NSP at this level.")
  }
  out <- tibble::tibble(cp = cp, ci_lower = lo, ci_upper = hi, level = level,
                        source = "nsp_region")
  # The clipping in cpt_confint() would turn an NA row into `cp`; keep the NA.
  out$ci_lower[is.na(lo)] <- NA_integer_
  out
}

#' Test detected changepoints
#'
#' Attaches a test to each detected changepoint (\code{type = "jump"}) or to
#' each fitted segment (\code{type = "segment"}), using the engine's own
#' test where it has one and an explicitly \emph{unadjusted} two-sample test
#' where it does not.
#'
#' @param object A \code{ggcpt} object.
#' @param type \code{"jump"} (one row per changepoint: is the change at this
#'   location real?) or \code{"segment"} (one row per segment: does this
#'   segment differ from the one before it?).
#' @param correction Multiple-testing correction applied across the rows, one
#'   of the \code{\link[stats]{p.adjust}} methods (\code{"none"},
#'   \code{"bonferroni"}, \code{"holm"}, \code{"BH"}, ...). Defaults to
#'   \code{"none"}; a \code{p_adjusted} column is added when it is not.
#'
#' @section Selection bias — read this before quoting a p-value:
#' Testing a changepoint at a location that was \emph{chosen because the data
#' looked like it changed there} is circular, and the resulting p-values are
#' anti-conservative, often severely. The \code{selection_adjusted} column
#' records, per row, whether the test accounts for that:
#' \itemize{
#'   \item \code{TRUE} for \pkg{segmented}'s Davies test, which is built
#'     for a nuisance parameter present only under the alternative. It is
#'     one \emph{global} test of "is there a breakpoint", not a test per
#'     breakpoint, so on a multi-break fit every row carries the same
#'     statistic and p-value --- the method string says so.
#'   \item \code{FALSE} for the generic Welch two-sample fallback, which
#'     compares the segments either side of the changepoint as if the
#'     location had been fixed in advance. Useful as a descriptive effect
#'     size with a scale attached; not a valid significance test for the
#'     existence of the change.
#'   \item \code{FALSE} for \pkg{strucchange}'s route as well, which is
#'     the Chow F evaluated \emph{at} each estimated break date. The Chow
#'     statistic's reference distribution assumes the date was fixed in
#'     advance, so quoting it at a date the Bai–Perron dynamic program chose
#'     is exactly the circularity this column exists to flag --- reporting
#'     it is conventional in that literature, which does not make it
#'     adjusted. The selection-adjusted objects there are the sup-type
#'     statistics and the Bai–Perron critical values.
#' }
#' Two further limits worth knowing. \code{type = "segment"} is
#' \strong{always} the unadjusted Welch test: the native routes above apply
#' only to \code{type = "jump"}, so a \pkg{segmented} fit tested
#' per-segment does not use Davies' test. And the split above is by
#' \emph{engine and type}, not by engine alone.
#' For a guarantee that survives selection, use \code{\link{nsp_wrapper}()}
#' (regions with exact global coverage) or \code{\link{cpt_confint}()} with
#' \code{method = "nsp"}. The canonical post-detection tests of Jewell,
#' Fearnhead and Witten (2022) are implemented in \code{ChangepointInference},
#' which is not on CRAN; \code{\link{cpt_register_method}()} is the supported
#' way to bring it in.
#'
#' @return A tibble with columns \code{cp} (or \code{seg_id}),
#'   \code{estimate}, \code{statistic}, \code{p_value}, \code{method} and
#'   \code{selection_adjusted}.
#' @seealso \code{\link{cpt_confint}()}, \code{\link{nsp_wrapper}()}.
#' @export
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(60), rnorm(60, 4)), method = "pelt")
#' cpt_test(fit)
cpt_test <- function(object, type = c("jump", "segment"),
                     correction = "none") {
  if (!is_ggcpt(object)) {
    stop("`object` must be a ggcpt object.", call. = FALSE)
  }
  type <- match.arg(type)
  correction <- match.arg(correction, stats::p.adjust.methods)

  out <- if (type == "segment") {
    test_segments(object)
  } else {
    native <- native_jump_test(object)
    native %||% naive_jump_test(object)
  }

  if (nrow(out) > 0 && correction != "none") {
    out$p_adjusted <- stats::p.adjust(out$p_value, method = correction)
    attr(out, "correction") <- correction
  }
  if (nrow(out) > 0 && any(!out$selection_adjusted)) {
    warning("`selection_adjusted` is FALSE for ",
            sum(!out$selection_adjusted), " of ", nrow(out), " row(s): the ",
            "changepoint locations were chosen from these data, so those ",
            "p-values are anti-conservative. See the selection-bias section ",
            "of ?cpt_test.", call. = FALSE)
  }
  out
}

# Internal: the engine's own test, when it has one. NULL otherwise.
#' @noRd
native_jump_test <- function(object) {
  fit <- object$fit
  cp <- object$changepoints$cp
  if (length(cp) == 0 || is.null(fit)) return(NULL)

  if (inherits(fit, "breakpoints") &&
      requireNamespace("strucchange", quietly = TRUE)) {
    return(strucchange_jump_test(object, fit))
  }
  if (inherits(fit, "segmented") &&
      requireNamespace("segmented", quietly = TRUE)) {
    return(segmented_jump_test(object, fit))
  }
  NULL
}

# Internal: a Chow F test at each estimated break date. `sctest()` with
# `type = "Chow"` is strucchange's own single-break test; running it at the
# dates the Bai-Perron dynamic program selected is the standard reporting
# convention in that literature -- but the convention does not make the
# p-value selection-adjusted, and this column used to claim it was. The
# Chow F's reference distribution assumes the break date was fixed IN
# ADVANCE; evaluating it at a date chosen because the data looked like it
# broke there is precisely the circularity `selection_adjusted` exists to
# flag. The selection-adjusted objects in this framework are the sup-type
# statistics (supF / Fstats) and the Bai-Perron critical values, not a
# pointwise Chow F, so the flag is FALSE and the method string says
# "unadjusted" out loud.
#' @noRd
strucchange_jump_test <- function(object, fit) {
  cp <- object$changepoints$cp
  y <- object$data$value
  d <- data.frame(.y = y)
  rows <- lapply(cp, function(k) {
    tst <- tryCatch(
      strucchange::sctest(.y ~ 1, data = d, type = "Chow", point = k),
      error = function(e) NULL
    )
    if (is.null(tst)) return(NULL)
    left <- y[seq_len(k)]
    right <- y[seq.int(k + 1L, length(y))]
    tibble::tibble(cp = k,
                   estimate = mean(right) - mean(left),
                   statistic = as.numeric(tst$statistic),
                   p_value = as.numeric(tst$p.value),
                   method = "Chow F at estimated break (unadjusted)",
                   selection_adjusted = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Internal: segmented's own inference. The Davies test addresses exactly the
# nuisance-parameter-under-the-alternative problem that makes a naive test
# invalid, so it is the selection-adjusted route; the per-break estimate and
# statistic come from the U-terms of the fitted model.
#' @noRd
segmented_jump_test <- function(object, fit) {
  cp <- object$changepoints$cp
  dav <- tryCatch(
    segmented::davies.test(fit, seg.Z = ~.t),
    error = function(e) NULL
  )
  tt <- tryCatch(summary(fit)$Ttable, error = function(e) NULL)
  u_rows <- if (!is.null(tt)) grep("^U[0-9]+", rownames(tt)) else integer(0)

  tibble::tibble(
    cp = cp,
    estimate = if (length(u_rows) == length(cp)) {
      as.numeric(tt[u_rows, 1])
    } else {
      rep(NA_real_, length(cp))
    },
    statistic = if (!is.null(dav)) {
      rep(as.numeric(dav$statistic), length(cp))
    } else {
      rep(NA_real_, length(cp))
    },
    p_value = if (!is.null(dav)) {
      rep(as.numeric(dav$p.value), length(cp))
    } else {
      rep(NA_real_, length(cp))
    },
    # Davies' test is a single test of "is there A breakpoint", not a test
    # per breakpoint, so on a three-break fit these rows carry the same
    # statistic and p-value three times -- which reads as three
    # per-changepoint tests. It IS the right selection-adjusted object; the
    # row-wise presentation was the problem, so the method string names the
    # scope.
    method = "Davies test (segmented; global, one test per fit)",
    selection_adjusted = TRUE
  )
}

# Internal: the generic fallback. A Welch two-sample t-test across each
# changepoint, comparing the stretches bounded by the neighbouring
# changepoints. Honest about what it is: `selection_adjusted = FALSE`.
#' @noRd
naive_jump_test <- function(object) {
  cp <- object$changepoints$cp
  y <- object$data$value
  n <- length(y)
  if (length(cp) == 0) {
    return(tibble::tibble(cp = integer(), estimate = numeric(),
                          statistic = numeric(), p_value = numeric(),
                          method = character(),
                          selection_adjusted = logical()))
  }
  bounds <- c(0L, cp, n)
  rows <- lapply(seq_along(cp), function(i) {
    left <- y[seq.int(bounds[i] + 1L, bounds[i + 1L])]
    right <- y[seq.int(bounds[i + 1L] + 1L, bounds[i + 2L])]
    tt <- tryCatch(stats::t.test(right, left), error = function(e) NULL)
    tibble::tibble(
      cp = cp[i],
      estimate = mean(right) - mean(left),
      statistic = if (is.null(tt)) NA_real_ else as.numeric(tt$statistic),
      p_value = if (is.null(tt)) NA_real_ else as.numeric(tt$p.value),
      method = "Welch two-sample t (unadjusted)",
      selection_adjusted = FALSE
    )
  })
  do.call(rbind, rows)
}

# Internal: one row per segment, comparing it with the segment before.
#' @noRd
test_segments <- function(object) {
  seg <- object$segments
  y <- object$data$value
  if (nrow(seg) < 2) {
    return(tibble::tibble(seg_id = integer(), estimate = numeric(),
                          statistic = numeric(), p_value = numeric(),
                          method = character(),
                          selection_adjusted = logical()))
  }
  rows <- lapply(seq.int(2L, nrow(seg)), function(i) {
    prev <- y[seq.int(seg$start[i - 1], seg$end[i - 1])]
    cur <- y[seq.int(seg$start[i], seg$end[i])]
    tt <- tryCatch(stats::t.test(cur, prev), error = function(e) NULL)
    tibble::tibble(
      seg_id = seg$seg_id[i],
      estimate = mean(cur) - mean(prev),
      statistic = if (is.null(tt)) NA_real_ else as.numeric(tt$statistic),
      p_value = if (is.null(tt)) NA_real_ else as.numeric(tt$p.value),
      method = "Welch two-sample t (unadjusted)",
      selection_adjusted = FALSE
    )
  })
  do.call(rbind, rows)
}
