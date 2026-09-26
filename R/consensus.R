# ---------------------------------------------------------------------------
# Theme J: ensembles, consensus and method choice.
#
# With fifty wired methods the user's real question is "which one?".
# ggcpt_compare() shows the disagreement; cpt_consensus() resolves it into a
# single segmentation with a vote count, and cpt_recommend() answers the
# question before any detector is run.
# ---------------------------------------------------------------------------

#' Consensus changepoints across several detectors
#'
#' Runs a set of detectors on one series and reports the locations they agree
#' on. Two detections count as the same changepoint when they fall within
#' \code{tolerance} of each other, the same tolerance window
#' \code{\link{cpt_metrics}()} matches on (van den Burg and Williams, 2020),
#' so the package has one notion of "close enough" and not two. The
#' \emph{grouping} necessarily differs: \code{cpt_metrics()} matches two
#' sets one-to-one, while consensus has to cluster \eqn{K} of them, so
#' detections are swept in order and a new cluster opens as soon as one lies
#' more than \code{tolerance} from the cluster's first member. That cap stops
#' a chain of near-neighbours merging into one arbitrarily wide cluster.
#'
#' @param x A numeric vector (the series).
#' @param methods Character vector of method names. A method that fails on
#'   this series (its engine errors or is not installed, or it does not
#'   offer \code{change_in}) is left out of the vote with a warning; invalid
#'   data or arguments stop the call.
#' @param tolerance Matching window, in positions. Defaults to \code{5}.
#' @param min_votes Minimum number of methods that must find a location for
#'   it to enter the consensus. Defaults to \code{"majority"}: more than
#'   half of the methods that ran (two of three, three of four or five), so
#'   the rule does not silently change with the size of the panel, as a
#'   fixed count of two did (a bare majority of three, a fifth of ten).
#'   \code{"all"} asks for unanimity.
#'
#'   A value \strong{strictly between 0 and 1} is read as a proportion of
#'   the methods that ran; anything else is a count, rounded \emph{up}
#'   (\code{2.5} needs three methods, never two). The boundary is worth
#'   knowing, because it falls exactly where a reader thinking in
#'   proportions would write \dQuote{unanimous}: with three methods,
#'   \code{min_votes = 0.99} needs all three, while
#'   \code{min_votes = 1} (and \code{1.0}, which is the same number)
#'   is a count of one and so the \emph{least} strict setting there is. For
#'   unanimity, pass the number of methods, or a fraction just below 1.
#'
#'   A count larger than the number of methods that ran cannot be reached,
#'   so the consensus would be empty by construction; that warns rather
#'   than returning a result indistinguishable from \dQuote{the methods
#'   agreed on nothing}.
#' @param change_in Passed to each detector.
#' @param index Optional time index, carried onto the result.
#' @param seed Optional seed for reproducibility. The seed is scoped to this
#'   call: \code{.Random.seed} is saved and restored, so a seeded call
#'   inside a simulation loop does not pin the loop's own stream.
#' @param ... Additional arguments passed to each detector.
#'
#' @section Consensus does not remove shared false positives:
#' Voting removes the idiosyncrasies of individual algorithms. It does not
#' remove a false positive that every algorithm makes for the same reason,
#' and a violated assumption is exactly such a reason: measured under
#' AR(1) noise, every one of \code{pelt}'s spurious changepoints was
#' reproduced by \code{wbs} within five observations, and the 2-of-3
#' consensus of \code{pelt}, \code{binseg} and \code{wbs} had more false
#' positives (1.40) than two of its own members. The useful signal in a
#' panel is its \strong{disagreement}: the \code{disagreement} value in
#' the \code{consensus} attribute is the share of the locations any member
#' found that not every member found, and a high value says the members'
#' shared assumption may be what is wrong (see
#' \code{\link{cpt_assumptions}()} and \code{\link{cpt_robustness}()}).
#'
#' @section Consensus is not inference:
#' Agreement among detectors is \strong{not} a p-value, and a location found
#' by six of seven methods is not thereby significant at any level: the
#' methods are run on the same data and are strongly correlated, several of
#' them share an engine, and none of the votes is independent. Read the vote
#' count as a robustness display ("this feature does not depend on which
#' detector I picked"), and use \code{\link{nsp_wrapper}()} or
#' \code{\link{cpt_confint}()} when you need a guarantee.
#'
#' @return A \code{ggcpt} object (so it plots and tidies like any other
#'   result) whose changepoints tibble carries \code{votes} and
#'   \code{methods} (a comma-separated list of the methods that found each
#'   location), with the per-method detections kept in a
#'   \code{consensus} attribute and drawn by \code{autoplot(plot_type =
#'   "agreement")}.
#' @references
#' \insertRef{vandenburg2020evaluation}{ggchangepoint}
#' @seealso \code{\link{ggcpt_compare}()}, \code{\link{cpt_recommend}()},
#'   \code{\link{cpt_metrics}()}.
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(80), rnorm(80, 4))
#' cons <- cpt_consensus(x, methods = c("pelt", "binseg", "amoc"))
#' tidy(cons)
#' ggplot2::autoplot(cons)
cpt_consensus <- function(x, methods = c("pelt", "binseg", "amoc"),
                          tolerance = 5, min_votes = "majority",
                          change_in = "mean", index = NULL, seed = NULL,
                          ...) {
  methods <- unique(as.character(methods))
  if (length(methods) < 2) {
    cpt_abort("`methods` needs at least two detectors to reach a consensus; ",
               "got ", length(methods), ".", class = "bad_argument")
  }
  validate_scalar(tolerance, "tolerance", min = 0)
  if (is.character(min_votes)) {
    min_votes <- cpt_match_arg(min_votes, c("majority", "all"),
                               name = "min_votes")
  } else {
    validate_scalar(min_votes, "min_votes", min = 0)
  }
  series <- as_cpt_series(x, index = index)
  data_vec <- as_uni_vector(series$values, "cpt_consensus")
  validate_data(data_vec)
  n <- length(data_vec)

  local_seed(seed)
  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  # A method that fails is left out of the vote; bad input or a bug is not
  # a vote to leave out, so fanout_failure() raises those.
  run_one <- function(m) {
    tryCatch(
      cpt_detect(data_vec, method = m, change_in = change_in,
                 ...)$changepoints$cp,
      error = function(e) fanout_failure(e, "Method `", m, "`: ")
    )
  }
  results <- if (has_future) {
    future.apply::future_lapply(methods, with_session_registry(run_one),
                                future.seed = seed %||% TRUE)
  } else {
    lapply(methods, run_one)
  }
  names(results) <- methods

  failed <- vapply(results, inherits, logical(1), "cpt_failed")
  if (any(failed)) {
    cpt_warn("These methods errored and are excluded from the vote: ",
             paste(methods[failed], collapse = ", "), ".",
             class = "engine_failed")
  }
  ok <- methods[!failed]
  if (length(ok) < 2) {
    cpt_abort("Fewer than two methods ran successfully, so there is no ",
               "consensus ", "to take.", class = "engine_error")
  }
  detections <- results[ok]

  threshold <- if (identical(min_votes, "majority")) {
    floor(length(ok) / 2) + 1L
  } else if (identical(min_votes, "all")) {
    length(ok)
  } else if (min_votes > 0 && min_votes < 1) {
    max(1, ceiling(min_votes * length(ok)))
  } else {
    # ceiling(), not as.integer(): "at least 2.5 methods" means three, and
    # truncating made a fractional count LESS strict than the number asked
    # for (2.5 resolved to a threshold of 2).
    # The tolerance keeps a count computed in floating point (0.1 * 30 is
    # 3.0000000000000004) from being rounded up past the number it means.
    max(1, as.integer(ceiling(min_votes - 1e-8)))
  }
  # A threshold no method can reach makes the empty result certain, and an
  # empty consensus is indistinguishable from "the methods agreed on
  # nothing" -- which is a finding, where this is an arithmetic mistake.
  # `cpt_consensus(x, methods = c("pelt", "binseg"), min_votes = 3)` asked
  # three of two. Same reason `cpt_benchmark()` refuses an empty `methods`.
  if (threshold > length(ok)) {
    cpt_warn("`min_votes` resolves to a threshold of ", threshold, " but only ",
             length(ok), " method(s) ran, so no location can ", "reach it and ",
              "the consensus is empty by construction. Note that ",
             "a value of 1 or more is a count of methods, not a proportion: ",
             "only a fraction strictly between 0 and 1 is read as a ",
             "proportion.", class = "warning")
  }

  clusters <- cluster_changepoints(detections, tolerance)
  # The panel's disagreement: the share of the locations any member found
  # that not every member found. Computed before the vote filters them.
  disagreement <- if (nrow(clusters)) {
    mean(clusters$votes < length(ok))
  } else {
    0
  }
  keep <- clusters$votes >= threshold
  clusters <- clusters[keep, , drop = FALSE]

  res <- ggcpt_build(
    data_vec, clusters$cp,
    method = "consensus",
    change_in = change_in,
    penalty = list(type = paste0("votes >= ", threshold),
                   value = as.numeric(threshold)),
    fit = NULL,
    call = match.call(),
    extra_cp_cols = if (nrow(clusters) > 0) {
      list(votes = clusters$votes, methods = clusters$methods,
           spread = clusters$spread)
    }
  )
  attr(res, "consensus") <- list(detections = detections,
                                 methods = ok, tolerance = tolerance,
                                 threshold = threshold,
                                 disagreement = disagreement,
                                 counts = vapply(detections, length,
                                                 integer(1)))
  class(res) <- c("ggcpt_consensus", class(res))
  attach_index(res, series$index, series$index_label)
}

# Internal: group detections from several methods into clusters, on the same
# tolerance window cpt_metrics() matches within. Locations are swept in order
# and a new cluster opens whenever the next detection is more than
# `tolerance` from the current cluster's FIRST member -- anchoring on the
# first, not the previous, is what stops a chain of near-neighbours (1, 6,
# 11, 16, ...) merging into one cluster of unbounded width. The reported
# location is the (rounded) median of the cluster, which is robust to one
# method landing a few positions out.
#' @noRd
cluster_changepoints <- function(detections, tolerance) {
  all_cp <- unlist(lapply(names(detections), function(m) {
    v <- detections[[m]]
    if (length(v) == 0) return(NULL)
    stats::setNames(v, rep(m, length(v)))
  }))
  if (length(all_cp) == 0) {
    return(tibble::tibble(cp = integer(), votes = integer(),
                          methods = character(), spread = integer()))
  }
  ord <- order(all_cp)
  all_cp <- all_cp[ord]
  owner <- names(all_cp)

  cluster_id <- integer(length(all_cp))
  cid <- 1L
  anchor <- all_cp[1]
  for (i in seq_along(all_cp)) {
    if (all_cp[i] - anchor > tolerance) {
      cid <- cid + 1L
      anchor <- all_cp[i]
    }
    cluster_id[i] <- cid
  }

  rows <- lapply(split(seq_along(all_cp), cluster_id), function(ii) {
    v <- as.integer(all_cp[ii])
    ms <- unique(owner[ii])
    tibble::tibble(cp = as.integer(round(stats::median(v))),
                   votes = length(ms),
                   methods = paste(sort(ms), collapse = ", "),
                   spread = as.integer(diff(range(v))))
  })
  out <- do.call(rbind, rows)
  out[order(out$cp), , drop = FALSE]
}

#' @rdname cpt_consensus
#' @param object A \code{ggcpt_consensus} object (for \code{autoplot()}).
#' @param plot_type \code{"series"} (the consensus segmentation, votes shown
#'   by line width) or \code{"agreement"} (a method-by-location dot matrix
#'   showing exactly who voted for what).
#' @param ... Passed on to \code{\link{autoplot.ggcpt}()} for
#'   \code{plot_type = "series"}.
#' @export
autoplot.ggcpt_consensus <- function(object,
                                     plot_type = c("series", "agreement"),
                                     ...) {
  plot_type <- cpt_match_arg(plot_type)
  info <- attr(object, "consensus")
  idx_vals <- plot_index(object)
  x_lab <- plot_index_label(object)

  if (plot_type == "series") {
    p <- autoplot.ggcpt(unclass_ggcpt(object), ...) +
      ggplot2::labs(title = paste0("Consensus of ",
                                   length(info$methods), " methods"),
                    subtitle = paste0("A location enters when at least ",
                                      info$threshold,
                                      " method(s) find it within ",
                                      info$tolerance, " positions"))
    return(p)
  }

  rows <- do.call(rbind, lapply(names(info$detections), function(m) {
    v <- info$detections[[m]]
    if (length(v) == 0) return(NULL)
    tibble::tibble(method = m, cp = v)
  }))
  if (is.null(rows)) {
    cpt_abort("No method detected a changepoint, so there is no agreement ",
               "matrix ", "to draw.", class = "input_error")
  }
  rows$method <- factor(rows$method, levels = rev(info$methods))
  rows$cp_x <- idx_vals[rows$cp]
  cons <- object$changepoints

  p <- ggplot2::ggplot(rows, ggplot2::aes(cp_x, method))
  if (nrow(cons) > 0) {
    band <- tibble::tibble(
      xmin = idx_vals[pmax(1L, cons$cp - info$tolerance)],
      xmax = idx_vals[pmin(length(idx_vals), cons$cp + info$tolerance)]
    )
    p <- p + geom_cpt_region(ggplot2::aes(xmin = xmin, xmax = xmax),
                             data = band, fill = "#009E73", alpha = 0.15)
  }
  p +
    ggplot2::geom_point(size = 2.4, colour = "#0072B2") +
    ggplot2::labs(x = x_lab, y = NULL,
                  title = "Method agreement",
                  subtitle = "Shaded bands are the consensus locations")
}

# Internal: a ggcpt_consensus is a ggcpt with an extra class, and
# autoplot.ggcpt() must be called on it without re-dispatching.
#' @noRd
unclass_ggcpt <- function(x) {
  class(x) <- "ggcpt"
  x
}

#' @rdname cpt_consensus
#' @param x A \code{ggcpt_consensus} object (for \code{print()}).
#' @export
print.ggcpt_consensus <- function(x, ...) {
  info <- attr(x, "consensus")
  cat("ggcpt_consensus (", length(info$methods), " methods, tolerance ",
      info$tolerance, ", threshold ", info$threshold, " vote(s))\n", sep = "")
  cat("  Methods: ", paste(info$methods, collapse = ", "), "\n", sep = "")
  if (!is.null(info$counts)) {
    cat("  Found by each: ", paste0(names(info$counts), " ", info$counts,
                                    collapse = ", "), "\n", sep = "")
  }
  if (!is.null(info$disagreement)) {
    cat("  Disagreement: ", format(round(info$disagreement, 2)),
        " (share of locations not found by every method)\n", sep = "")
  }
  cat("  Consensus changepoints: ", nrow(x$changepoints), "\n", sep = "")
  if (nrow(x$changepoints) > 0) {
    cat("\n")
    print(x$changepoints, n = 12)
  }
  cat("\nAgreement is a robustness display, not a significance test; ",
      "see ?cpt_consensus.\n", sep = "")
  invisible(x)
}

#' Recommend a detection method
#'
#' Turns the capability matrix and the package's measurements into an
#' answer. Given what the analyst knows about their problem (how many
#' dimensions, what kind of change, what the noise and the data look like,
#' how long the series is, how many changes they expect, whether they need
#' uncertainty or an online alarm), this returns the methods that fit, as
#' calls, with the power and false-alarm rate measured for each in that
#' situation, a reason and the caveats. It is a decision table, not a
#' model: everything it knows is in \code{\link{cpt_methods}()} and the
#' measurement tables (\code{\link{cpt_noise_benchmark}},
#' \code{\link{cpt_data_types}}, \code{\link{cpt_runtimes}}), and making that
#' explicit and printable is the point.
#'
#' @param dimension \code{"univariate"} (default) or \code{"multivariate"}.
#' @param change_in What kind of change is expected: any value accepted by
#'   \code{\link{cpt_detect}()}. Defaults to \code{"mean"}.
#' @param noise Noise structure: \code{"iid"} (default), \code{"heavy"}
#'   (heavy-tailed), \code{"autocorrelated"}, or \code{"heteroscedastic"}.
#' @param n Series length. Methods the runtime table saw fail to finish at
#'   that length are left out (and listed), and the slow ones carry a
#'   caveat. Optional.
#' @param need_uncertainty Does the answer have to come with a confidence
#'   interval or significance region? Defaults to \code{FALSE}.
#' @param online Is detection sequential (alarms as data arrive) rather than
#'   retrospective? Defaults to \code{FALSE}.
#' @param installed_only Restrict to engines that are installed. Defaults to
#'   \code{FALSE}, so the recommendation names the right method even when it
#'   needs an install.
#' @param n_expected How many changes you expect, if you know. The
#'   single-change designs (\code{amoc}, \code{pettitt}, \code{buishand},
#'   \code{snht}) cannot find a second one, so they are ranked down unless
#'   this is \code{1}.
#' @param data_type What the values are: \code{"continuous"} (default),
#'   \code{"counts"}, \code{"binary"} or \code{"proportion"}. Measured, a
#'   Gaussian cost raises about 165 times the false positives on binary data
#'   that it raises on Gaussian data, so engines are scored on their
#'   measured false positives for this data type, and those that can fit a
#'   cost for it (\code{family = "poisson"} or \code{"binomial"}) are
#'   preferred and recommended with that argument.
#' @param family Optional distribution family the method must fit (see
#'   \code{\link{cpt_families}()}).
#' @param fit Optional \code{ggcpt} fit already made. Its dimension, length
#'   and change type fill in the arguments not given; its values set the
#'   data type; and when its residuals are autocorrelated (see
#'   \code{\link{cpt_assumptions}()}) the noise is taken as
#'   \code{"autocorrelated"} unless \code{noise} was given.
#' @param jump Optional expected change size, in noise standard deviations.
#'   With \code{n}, the top three candidates' power to find it is simulated
#'   (\code{\link{cpt_power}()} with 50 replicates), and the printout says
#'   so when no method is likely to.
#'
#' @return A \code{ggcpt_recommendation} tibble ordered by suitability, with
#'   columns \code{method}, \code{engine}, \code{installed}, \code{score},
#'   \code{tie} (candidates with the same score share a number: the
#'   recommender cannot tell them apart on what was supplied), \code{call}
#'   (the \code{cpt_detect()} call to run, including the noise-model or
#'   family argument the recommendation depends on), \code{hits} and
#'   \code{false_positives} (measured in the stated regime: mean real
#'   changes found out of two, and mean spurious ones), \code{power} (when
#'   \code{jump} is given), \code{why} and \code{caveat}. \code{print()}
#'   reads as advice and \code{autoplot()} shows the scores and the ties.
#'
#' @section How the score is built:
#' Where the method was measured in the stated noise regime, the score
#' starts from \eqn{3 - E}, where \eqn{E} is the expected number of errors
#' per series in that benchmark (real changes missed plus false alarms),
#' taking the method's best setting of its noise-model argument (which is
#' then the one in \code{call}). A method not measured there starts at 1.
#' Then: minus 3 for a single-change design unless \code{n_expected = 1};
#' minus the log of one plus its measured false positives on the stated
#' data type, and plus 1.5 if it fits that data type's family; plus 1 for a
#' location interval and 0.5 for a posterior when uncertainty is needed;
#' at \eqn{n \ge 5000}, plus 0.5 for a fast engine and minus 1 for a slow
#' one; and small tie-breakers for the general-purpose methods (0.25) and
#' for an installed engine (0.1). Sort by \code{false_positives} instead if
#' a false alarm costs you more than a miss.
#' @seealso \code{\link{cpt_methods}()}, \code{\link{cpt_consensus}()},
#'   \code{\link{cpt_cite}()}.
#' @export
#' @examples
#' cpt_recommend(noise = "autocorrelated")
#' cpt_recommend(data_type = "counts", n_expected = 2)
#' cpt_recommend(dimension = "multivariate", change_in = "covariance")
cpt_recommend <- function(dimension = c("univariate", "multivariate"),
                          change_in = "mean",
                          noise = c("iid", "heavy", "autocorrelated",
                                    "heteroscedastic"),
                          n = NULL, need_uncertainty = FALSE, online = FALSE,
                          installed_only = FALSE, n_expected = NULL,
                          data_type = c("continuous", "counts", "binary",
                                        "proportion"),
                          family = NULL, fit = NULL, jump = NULL) {
  notes <- character(0)
  noise_given <- !missing(noise)
  if (!is.null(fit)) {
    if (!is_ggcpt(fit)) {
      cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
    }
    if (missing(dimension)) {
      dimension <- if (n_coordinates(fit) > 1L) "multivariate" else
        "univariate"
    }
    if (is.null(n)) n <- nrow(fit$data)
    if (missing(change_in) &&
        scalar_chr(fit$change_in) %in% cpt_change_in_levels()) {
      change_in <- scalar_chr(fit$change_in)
    }
    if (missing(data_type)) data_type <- detect_data_type(fit$data$value)
    dep <- fit$diagnostics$residual_dependence %||%
      residual_dependence(fit$data$value, fit$changepoints$cp)
    if (!noise_given && !is.null(dep) && isTRUE(dep$p_value < 0.05)) {
      noise <- "autocorrelated"
      notes <- c(notes, paste0("the fit's residuals are autocorrelated ",
                               "(Ljung-Box p = ", format(signif(dep$p_value,
                                                                2)),
                               "), so the noise is taken as autocorrelated"))
    }
  }
  dimension <- cpt_match_arg(dimension)
  noise <- cpt_match_arg(noise)
  data_type <- cpt_match_arg(data_type)
  change_in <- cpt_match_arg(change_in, cpt_change_in_levels())
  validate_flag(need_uncertainty, "need_uncertainty")
  validate_flag(online, "online")
  validate_flag(installed_only, "installed_only")
  if (!is.null(n)) validate_scalar(n, "n", min = 1)
  if (!is.null(n_expected)) validate_scalar(n_expected, "n_expected", min = 0)
  if (!is.null(jump)) validate_scalar(jump, "jump", min = 0, min_open = TRUE)
  if (!is.null(family)) {
    family <- cpt_match_arg(family, family_levels(), name = "family")
  }

  reg <- full_registry()
  tab <- cpt_methods()
  reg$installed <- tab$installed[match(reg$method, tab$method)]

  # The same rule validate_method_change_in() applies, and for the same
  # reason: `change_in = "mean"` is accepted by every method, because a
  # method targeting something else (distribution, slope) treats a "mean"
  # request as its native change type. Reading `supports` strictly here
  # dropped every distribution-only method -- np, ecp, geomcp, npmojo -- so
  # the two canonical univariate nonparametric answers were filtered out
  # before scoring and could not be recommended at all.
  supports <- vapply(reg$supports, function(s) {
    identical(change_in, "mean") || change_in %in% s
  }, logical(1))
  reg <- reg[supports, , drop = FALSE]
  if (dimension == "multivariate") {
    reg <- reg[reg$multivariate, , drop = FALSE]
  } else {
    # A univariate user must not be pointed at a high-dimensional engine,
    # however well it matches on everything else. Nine of the fourteen do
    # error on a single column; the other four (npmojo, inspect, esac,
    # pilliat) would run and return something, which is exactly why the
    # filter is on intent rather than on whether the call happens to fail.
    reg <- reg[reg$univariate, , drop = FALSE]
  }
  if (online) reg <- reg[reg$online, , drop = FALSE]
  if (need_uncertainty) reg <- reg[reg$ci | reg$posterior, , drop = FALSE]
  if (installed_only) reg <- reg[isTRUE_vec(reg$installed), , drop = FALSE]
  if (!is.null(family)) {
    reg <- reg[reg$method %in% methods_with_family(family), , drop = FALSE]
  }

  # A method the runtime table saw fail to finish at this length (or a
  # shorter one) is not a recommendation at this length: it is a
  # recommendation to wait. §208 measured five engines that cannot reach
  # n = 10,000 in two minutes.
  excluded <- character(0)
  rt <- measured_data("cpt_runtimes")
  if (!is.null(n) && !is.null(rt) && nrow(reg)) {
    too_slow <- vapply(reg$method, function(m) {
      r <- rt[rt$method == m, , drop = FALSE]
      if (!nrow(r)) return(FALSE)
      failed <- r$n[r$status != "ok" | !is.finite(r$seconds)]
      length(failed) > 0 && min(failed) <= n
    }, logical(1))
    if (any(too_slow)) {
      excluded <- reg$method[too_slow]
      reg <- reg[!too_slow, , drop = FALSE]
    }
  }

  if (nrow(reg) == 0) {
    cpt_abort("No wired method matches that combination (", dimension, ", ",
              change_in, ", ", noise,
              if (need_uncertainty) ", with uncertainty" else "",
              if (online) ", online" else "",
              if (!is.null(family)) paste0(", family ", family) else "",
              if (length(excluded)) paste0(", finishing at n = ", n) else "",
              "). Relax a requirement, or register an external detector ",
              "with cpt_register_method().", class = "unsupported")
  }

  k <- nrow(reg)
  score <- rep(1, k)
  why <- rep(paste0("handles change_in = \"", change_in, "\""), k)
  caveat <- rep(NA_character_, k)
  add_caveat <- function(i, txt) {
    caveat[i] <<- ifelse(is.na(caveat[i]), txt, paste0(caveat[i], "; ", txt))
  }
  call_args <- rep("", k)
  hits <- rep(NA_real_, k)
  fps <- rep(NA_real_, k)

  # ---- the measured noise regime ----------------------------------------------
  regime <- switch(noise, iid = "iid", heavy = "heavy", autocorrelated = "ar1",
                   heteroscedastic = "hetero")
  bench <- measured_data("cpt_noise_benchmark")
  if (!is.null(bench)) {
    b <- bench[bench$regime == regime, , drop = FALSE]
    for (i in seq_len(k)) {
      rows <- b[b$method == reg$method[i], , drop = FALSE]
      if (!nrow(rows)) next
      err <- (2 - rows$hits) + rows$fp
      j <- which.min(err)
      hits[i] <- rows$hits[j]
      fps[i] <- rows$fp[j]
      score[i] <- 3 - min(err[j], 6)
      if (!identical(rows$setting[j], "default")) {
        call_args[i] <- paste0(", ", rows$setting[j])
        why[i] <- paste0(why[i], "; its best setting under ", noise,
                         " noise is ", rows$setting[j])
      }
      # One spurious changepoint per series is already as many as a real
      # change: worth saying next to the call, whatever the score.
      if (rows$fp[j] >= 1) {
        add_caveat(i, paste0(format(rows$fp[j]), " spurious changepoints per ",
                             "series measured under ", noise, " noise; check ",
                             "the fit with cpt_assumptions()"))
      }
    }
    if (noise != "iid") {
      unmeasured <- is.na(hits)
      why[!unmeasured] <- paste0(why[!unmeasured], "; measured under ",
                                 noise, " noise")
    }
  }

  # ---- how many changes -------------------------------------------------------
  single <- reg$method %in% names(max_cp_table())[max_cp_table() == 1L] &
    !reg$method %in% c("segmented", "mcp")
  if (is.null(n_expected) || n_expected != 1) {
    score[single] <- score[single] - 3
    for (i in which(single)) {
      add_caveat(i, "finds at most one changepoint")
    }
  } else {
    score[single] <- score[single] + 0.5
    why[single] <- paste0(why[single], "; a single-change design")
  }

  # ---- the data type ------------------------------------------------------------
  if (data_type != "continuous") {
    dt_key <- switch(data_type, counts = "poisson", binary = "bernoulli",
                     proportion = "proportion")
    fam_for <- switch(data_type, counts = "poisson", binary = "binomial",
                      proportion = "binomial")
    dtab <- measured_data("cpt_data_types")
    fam_ok <- reg$method %in% methods_with_family(fam_for)
    for (i in seq_len(k)) {
      if (fam_ok[i] && data_type != "proportion") {
        score[i] <- score[i] + 1.5
        call_args[i] <- paste0(call_args[i], ", family = \"", fam_for, "\"")
        why[i] <- paste0(why[i], "; fits a ", fam_for, " cost")
        next
      }
      if (is.null(dtab)) next
      r <- dtab[dtab$method == reg$method[i] & dtab$data_type == dt_key, ,
                drop = FALSE]
      if (!nrow(r)) next
      score[i] <- score[i] - log1p(r$fp[1])
      if (r$fp[1] > 5) {
        add_caveat(i, paste0(format(r$fp[1]), " spurious changepoints ",
                             "measured on ", data_type, " data"))
      }
    }
  }

  if (need_uncertainty) {
    score[reg$ci] <- score[reg$ci] + 1
    why[reg$ci] <- paste0(why[reg$ci], "; supplies location intervals")
    score[reg$posterior] <- score[reg$posterior] + 0.5
    why[reg$posterior] <- paste0(why[reg$posterior], "; supplies a posterior")
  }

  # ---- length ----------------------------------------------------------------------
  if (!is.null(n) && n >= 5000) {
    meas <- measured_registry_columns(reg$method)
    fast <- meas$cost %in% "fast"
    slow <- meas$cost %in% "slow" | reg$method %in%
      c("segneigh", "ecp", "bcp", "kcp", "mcp", "fabisearch", "hdcov",
        "network", "var", "hdreg")
    score[fast] <- score[fast] + 0.5
    why[fast] <- paste0(why[fast], "; fast at this length")
    score[slow] <- score[slow] - 1
    for (i in which(slow)) {
      add_caveat(i, paste0("slow at n = ", format(n, big.mark = ",")))
    }
    # A few engines return a fit far bigger than the series: strucchange
    # keeps a triangular O(n^2) RSS matrix (~135 MB at n = 2000), and bfast
    # and bocpd are in the tens of MB.
    for (i in which(reg$method %in% c("strucchange", "bfast", "bocpd"))) {
      add_caveat(i, paste0("returns a large raw fit at this length; pass ",
                           "keep_fit = FALSE"))
    }
  }
  # HDCD 1.1's Pilliat() is unusable at power-of-two dimensions, and the
  # wrapper refuses there, so say it before the user picks the method.
  for (i in which(reg$method == "pilliat")) {
    add_caveat(i, paste0("unavailable when the number of coordinates is an ",
                         "exact power of two (upstream threshold bug)"))
  }

  gp <- reg$method %in% general_purpose_methods()
  score[gp] <- score[gp] + 0.25
  installed <- isTRUE_vec(reg$installed)
  score[installed] <- score[installed] + 0.1

  calls <- paste0("cpt_detect(x, method = \"", reg$method, "\"", call_args,
                  ")")
  out <- tibble::tibble(
    method = reg$method, engine = reg$engine, installed = reg$installed,
    score = round(score, 2), tie = NA_integer_, call = calls,
    hits = hits, false_positives = fps, power = NA_real_,
    why = why, caveat = caveat
  )
  # Scores equal to the second decimal share a tie number: the recommender
  # cannot tell those candidates apart on the information supplied, which
  # is a finding to report rather than a ranking to invent (§186.3).
  out <- out[order(-out$score, !gp[match(out$method, reg$method)],
                   out$method), , drop = FALSE]
  out$tie <- match(out$score, unique(out$score))

  if (!is.null(jump) && !is.null(n)) {
    top <- utils::head(which(isTRUE_vec(out$installed) |
                               is.na(out$installed)), 3)
    for (i in top) {
      pw <- tryCatch(suppressWarnings(cpt_power(
        n = n, jump = jump, method = out$method[i], n_sim = 50,
        change_in = if (change_in %in% c("mean", "var", "meanvar", "slope"))
          change_in else "mean", parallel = FALSE))$power[1],
        error = function(e) NA_real_)
      out$power[i] <- pw
    }
  }

  attr(out, "query") <- list(dimension = dimension, change_in = change_in,
                             noise = noise, n = n,
                             need_uncertainty = need_uncertainty,
                             online = online, n_expected = n_expected,
                             data_type = data_type, family = family,
                             jump = jump)
  attr(out, "excluded") <- excluded
  attr(out, "notes") <- notes
  class(out) <- c("ggcpt_recommendation", class(out))
  out
}

#' @noRd
isTRUE_vec <- function(v) !is.na(v) & v

#' @rdname cpt_recommend
#' @export
tidy.ggcpt_recommendation <- function(x, ...) {
  tibble::as_tibble(unclass_keep_tbl(x))
}

#' @rdname cpt_recommend
#' @param x A \code{ggcpt_recommendation} object (for \code{print()}).
#' @param top How many candidates to print. Defaults to \code{5}.
#' @param ... Ignored.
#' @export
print.ggcpt_recommendation <- function(x, top = 5, ...) {
  # The only numeric argument in this file that skipped validate_scalar():
  # `top = -1` reached seq_len(-1) and failed with base R's "argument of
  # length 0"-adjacent complaint about a negative length.
  validate_scalar(top, "top", min = 0)
  q <- attr(x, "query")
  cat("Recommended methods for: ", q$dimension, " series, change in ",
      q$change_in, ", ", q$noise, " noise",
      if (!identical(q$data_type, "continuous") && !is.null(q$data_type)) {
        paste0(", ", q$data_type, " data")
      } else "",
      if (!is.null(q$n)) paste0(", n = ", q$n) else "",
      if (!is.null(q$n_expected)) paste0(", ", q$n_expected,
                                         " change(s) expected") else "",
      if (isTRUE(q$need_uncertainty)) ", uncertainty required" else "",
      if (isTRUE(q$online)) ", online" else "", "\n", sep = "")
  for (note in attr(x, "notes")) cat("Note: ", note, ".\n", sep = "")
  cat("\n")
  n_show <- min(top, nrow(x))
  # `installed` is NA for a registered method BY DESIGN -- a registration
  # supplies the detector itself, so there is no package to look for -- and
  # `isTRUE(NA)` is FALSE, so a detector the user wrote and registered this
  # session was advertised as not installed. Only an explicit FALSE means
  # missing.
  tag <- function(i) if (identical(x$installed[i], FALSE)) {
    "  [not installed]"
  } else {
    ""
  }
  for (i in seq_len(n_show)) {
    cat(i, ". ", x$call[i] %||% x$method[i], tag(i), "\n", sep = "")
    if (!is.null(x$hits) && !is.na(x$hits[i])) {
      cat("   measured: ", format(x$hits[i]), " of 2 changes found, ",
          format(x$false_positives[i]), " spurious per series\n", sep = "")
    }
    if (!is.null(x$power) && !is.na(x$power[i])) {
      cat("   power for a shift of ", q$jump, " sd at n = ", q$n, ": ",
          format(round(x$power[i], 2)), "\n", sep = "")
    }
    cat("   why: ", x$why[i], "\n", sep = "")
    if (!is.na(x$caveat[i])) cat("   caveat: ", x$caveat[i], "\n", sep = "")
  }
  # Ties are reported, not ordered: under the default query 31 of 32
  # candidates used to share one score and were printed alphabetically,
  # which read as a ranking that did not exist.
  if (!is.null(x$tie) && nrow(x) > 1L) {
    tied <- x$method[x$tie == x$tie[1]]
    if (length(tied) > 1L) {
      cat("\n", length(tied), " candidates tie for first on the information ",
          "supplied: ", paste(utils::head(tied, 8), collapse = ", "),
          if (length(tied) > 8) ", ..." else "", ". What would separate ",
          "them: `n_expected`, `n`, `data_type`, `need_uncertainty`, or a ",
          "`fit` to read the residuals of.\n", sep = "")
    }
  }
  if (!is.null(x$power) && any(!is.na(x$power)) &&
      max(x$power, na.rm = TRUE) < 0.8) {
    cat("\nNo candidate is likely to find a shift of ", q$jump, " sd at n = ",
        q$n, " (best power ", format(round(max(x$power, na.rm = TRUE), 2)),
        "): collect more data, or treat an empty answer as uninformative.\n",
        sep = "")
  }
  ex <- attr(x, "excluded")
  if (length(ex)) {
    cat("\nLeft out because the runtime table saw them fail to finish at ",
        "this length: ", paste(ex, collapse = ", "), ".\n", sep = "")
  }
  if (nrow(x) > n_show) {
    cat("\n(", nrow(x) - n_show, " further candidate(s); the full table is ",
        "the return value.)\n", sep = "")
  }
  cat("\nCite the method you use with cpt_cite(). Cross-check the choice ",
      "with\ncpt_robustness() and cpt_assumptions().\n", sep = "")
  invisible(x)
}

#' @rdname cpt_recommend
#' @param object A \code{ggcpt_recommendation} object (for
#'   \code{autoplot()}).
#' @export
autoplot.ggcpt_recommendation <- function(object, top = 15, ...) {
  validate_scalar(top, "top", min = 1)
  d <- tibble::as_tibble(unclass_keep_tbl(object))
  d <- utils::head(d, top)
  d$method <- factor(d$method, levels = rev(d$method))
  d$tie_group <- factor(d$tie)
  d$flag <- ifelse(is.na(d$caveat), "", "!")
  n_top <- sum(d$tie == d$tie[1])
  ggplot2::ggplot(d, ggplot2::aes(x = score, y = method, colour = tie_group)) +
    ggplot2::geom_segment(ggplot2::aes(x = min(score) - 0.5, xend = score,
                                       yend = method), linewidth = 0.4) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = flag), nudge_x = 0.15,
                       colour = "#D55E00", fontface = "bold") +
    scale_colour_cpt(guide = "none") +
    ggplot2::labs(
      x = "Score", y = NULL, title = "Recommended methods",
      subtitle = if (n_top > 1) {
        paste0(n_top, " candidates tie for first (same colour = same ",
               "score): the recommender cannot tell them apart on what was ",
               "supplied")
      } else {
        "Same colour = same score; ! = a caveat applies (see print())"
      }) +
    theme_ggcpt()
}
