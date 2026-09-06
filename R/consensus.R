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
#' \code{tolerance} of each other — the same tolerance window
#' \code{\link{cpt_metrics}()} matches on (van den Burg and Williams, 2020),
#' so the package has one notion of "close enough" and not two. The
#' \emph{grouping} necessarily differs: \code{cpt_metrics()} matches two
#' sets one-to-one, while consensus has to cluster \eqn{K} of them, so
#' detections are swept in order and a new cluster opens as soon as one lies
#' more than \code{tolerance} from the cluster's first member. That cap stops
#' a chain of near-neighbours merging into one arbitrarily wide cluster.
#'
#' @param x A numeric vector (the series).
#' @param methods Character vector of method names.
#' @param tolerance Matching window, in positions. Defaults to \code{5}.
#' @param min_votes Minimum number of methods that must find a location for
#'   it to enter the consensus. Defaults to \code{2}; pass a fraction in
#'   \eqn{(0, 1)} for a proportion of the methods that ran.
#' @param change_in Passed to each detector.
#' @param index Optional time index, carried onto the result.
#' @param seed Optional seed for reproducibility.
#' @param ... Additional arguments passed to each detector.
#'
#' @section Consensus is not inference:
#' Agreement among detectors is \strong{not} a p-value, and a location found
#' by six of seven methods is not thereby significant at any level: the
#' methods are run on the same data and are strongly correlated, several of
#' them share an engine, and none of the votes is independent. Read the vote
#' count as a robustness display — "this feature does not depend on which
#' detector I picked" — and use \code{\link{nsp_wrapper}()} or
#' \code{\link{cpt_confint}()} when you need a guarantee.
#'
#' @return A \code{ggcpt} object (so it plots and tidies like any other
#'   result) whose changepoints tibble carries \code{votes} and
#'   \code{methods} (a comma-separated list of the methods that found each
#'   location), with the per-method detections kept in a
#'   \code{consensus} attribute and printed by \code{autoplot(type =
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
                          tolerance = 5, min_votes = 2, change_in = "mean",
                          index = NULL, seed = NULL, ...) {
  methods <- unique(as.character(methods))
  if (length(methods) < 2) {
    stop("`methods` needs at least two detectors to reach a consensus; got ",
         length(methods), ".", call. = FALSE)
  }
  validate_scalar(tolerance, "tolerance", min = 0)
  validate_scalar(min_votes, "min_votes", min = 0)
  series <- as_cpt_series(x, index = index)
  data_vec <- as_uni_vector(series$values, "cpt_consensus")
  validate_data(data_vec)
  n <- length(data_vec)

  if (!is.null(seed)) set.seed(seed)
  has_future <- requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")

  run_one <- function(m) {
    tryCatch(
      cpt_detect(data_vec, method = m, change_in = change_in,
                 ...)$changepoints$cp,
      error = function(e) structure(conditionMessage(e), class = "cpt_failed")
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
    warning("These methods errored and are excluded from the vote: ",
            paste(methods[failed], collapse = ", "), ".", call. = FALSE)
  }
  ok <- methods[!failed]
  if (length(ok) < 2) {
    stop("Fewer than two methods ran successfully, so there is no consensus ",
         "to take.", call. = FALSE)
  }
  detections <- results[ok]

  threshold <- if (min_votes > 0 && min_votes < 1) {
    max(1, ceiling(min_votes * length(ok)))
  } else {
    max(1, as.integer(min_votes))
  }

  clusters <- cluster_changepoints(detections, tolerance)
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
                                 threshold = threshold)
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
  plot_type <- match.arg(plot_type)
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
    stop("No method detected a changepoint, so there is no agreement matrix ",
         "to draw.", call. = FALSE)
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
#' Turns the capability matrix into an answer. Given what the analyst knows
#' about their problem — how many dimensions, what kind of change, what the
#' noise looks like, how long the series is, whether they need uncertainty or
#' an online alarm — this returns the shortlist of methods that actually fit,
#' with a reason for each and the reference to cite. It is a decision table,
#' not a model: everything it knows is in \code{\link{cpt_methods}()}, and
#' making that explicit and printable is the point.
#'
#' @param dimension \code{"univariate"} (default) or \code{"multivariate"}.
#' @param change_in What kind of change is expected: any value accepted by
#'   \code{\link{cpt_detect}()}. Defaults to \code{"mean"}.
#' @param noise Noise structure: \code{"iid"} (default), \code{"heavy"}
#'   (heavy-tailed), \code{"autocorrelated"}, or \code{"heteroscedastic"}.
#' @param n Series length, used to flag methods that are impractical at that
#'   size. Optional.
#' @param need_uncertainty Does the answer have to come with a confidence
#'   interval or significance region? Defaults to \code{FALSE}.
#' @param online Is detection sequential (alarms as data arrive) rather than
#'   retrospective? Defaults to \code{FALSE}.
#' @param installed_only Restrict to engines that are installed. Defaults to
#'   \code{FALSE}, so the recommendation names the right method even when it
#'   needs an install.
#'
#' @return A tibble of candidate methods ordered by suitability, with columns
#'   \code{method}, \code{engine}, \code{installed}, \code{score},
#'   \code{why} and \code{caveat}, and a \code{print()} that reads as advice.
#' @seealso \code{\link{cpt_methods}()}, \code{\link{cpt_consensus}()},
#'   \code{\link{cpt_cite}()}.
#' @export
#' @examples
#' cpt_recommend(noise = "autocorrelated")
#' cpt_recommend(dimension = "multivariate", change_in = "covariance")
cpt_recommend <- function(dimension = c("univariate", "multivariate"),
                          change_in = "mean",
                          noise = c("iid", "heavy", "autocorrelated",
                                    "heteroscedastic"),
                          n = NULL, need_uncertainty = FALSE, online = FALSE,
                          installed_only = FALSE) {
  dimension <- match.arg(dimension)
  noise <- match.arg(noise)
  change_in <- match.arg(change_in, cpt_change_in_levels())
  validate_flag(need_uncertainty, "need_uncertainty")
  validate_flag(online, "online")
  validate_flag(installed_only, "installed_only")
  if (!is.null(n)) validate_scalar(n, "n", min = 1)

  reg <- full_registry()
  tab <- cpt_methods()
  reg$installed <- tab$installed[match(reg$method, tab$method)]

  supports <- vapply(reg$supports, function(s) change_in %in% s, logical(1))
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

  if (nrow(reg) == 0) {
    stop("No wired method matches that combination (", dimension, ", ",
         change_in, ", ", noise,
         if (need_uncertainty) ", with uncertainty" else "",
         if (online) ", online" else "",
         "). Relax a requirement, or register an external detector with ",
         "cpt_register_method().", call. = FALSE)
  }

  # Noise structure is the part the capability matrix does not encode, and
  # it is the part that most often makes an otherwise-sensible choice wrong.
  noise_pref <- switch(noise,
    iid = character(0),
    heavy = c("np", "ecp", "npmojo", "cpm", "kcp", "geomcp", "nsp", "sn"),
    autocorrelated = c("decafs", "envcpt", "npmojo", "sn", "nsp", "mcp",
                       "var", "wbsts"),
    heteroscedastic = c("hsmuce", "nsp", "sn", "not", "fastcpd", "np")
  )
  noise_warn <- switch(noise,
    iid = character(0),
    heavy = c("pelt", "binseg", "segneigh", "amoc", "fpop", "smuce", "cpop"),
    autocorrelated = c("pelt", "binseg", "segneigh", "amoc", "fpop", "smuce",
                       "wbs", "wbs2", "not", "mosum", "idetect", "tguh"),
    heteroscedastic = c("pelt", "binseg", "amoc", "fpop", "smuce", "wbs")
  )

  score <- rep(1, nrow(reg))
  why <- rep(paste0("handles change_in = \"", change_in, "\""), nrow(reg))
  caveat <- rep(NA_character_, nrow(reg))

  pref <- reg$method %in% noise_pref
  score[pref] <- score[pref] + 3
  why[pref] <- paste0(why[pref], "; built for ", noise, " noise")
  warn <- reg$method %in% noise_warn
  score[warn] <- score[warn] - 2
  caveat[warn] <- paste0("assumes independent, homoscedastic Gaussian noise",
                         " -- questionable under ", noise, " noise")

  if (need_uncertainty) {
    score[reg$ci] <- score[reg$ci] + 2
    why[reg$ci] <- paste0(why[reg$ci], "; supplies location intervals")
    score[reg$posterior] <- score[reg$posterior] + 1
    why[reg$posterior] <- paste0(why[reg$posterior], "; supplies a posterior")
  }
  if (!is.null(n)) {
    slow <- c("segneigh", "ecp", "bcp", "kcp", "mcp", "fabisearch",
              "changepoints")
    big <- n >= 5000 & reg$method %in% slow
    score[big] <- score[big] - 2
    caveat[big] <- paste0(ifelse(is.na(caveat[big]), "", paste0(caveat[big],
                                                               "; ")),
                          "quadratic or sampling-based: slow at n = ", n)
    fast <- c("pelt", "fpop", "binsegrcpp", "wbs2", "mosum")
    quick <- n >= 5000 & reg$method %in% fast
    score[quick] <- score[quick] + 1
    why[quick] <- paste0(why[quick], "; near-linear at this length")
    # A few engines return a fit far bigger than the series: strucchange
    # keeps a triangular O(n^2) RSS matrix (~135 MB at n = 2000), and bfast
    # and bocpd are in the tens of MB. That is the engine's object, not this
    # package's overhead, but it decides whether a panel fits in memory.
    heavy <- c("strucchange", "bfast", "bocpd")
    bulky <- n >= 5000 & reg$method %in% heavy
    caveat[bulky] <- paste0(ifelse(is.na(caveat[bulky]), "",
                                   paste0(caveat[bulky], "; ")),
                            "returns a large raw fit at this length; use ",
                            "cpt_batch(keep_fit = FALSE) for a panel")
  }
  # HDCD 1.1's Pilliat() is unusable at power-of-two dimensions, and the
  # wrapper refuses there, so say it before the user picks the method.
  pil <- reg$method == "pilliat"
  if (any(pil)) {
    caveat[pil] <- paste0(ifelse(is.na(caveat[pil]), "",
                                  paste0(caveat[pil], "; ")),
                          "unavailable when the number of coordinates is an ",
                          "exact power of two (upstream threshold bug)")
  }
  installed <- isTRUE_vec(reg$installed)
  score[installed] <- score[installed] + 0.5

  out <- tibble::tibble(
    method = reg$method, engine = reg$engine, installed = reg$installed,
    score = score, why = why, caveat = caveat
  )
  out <- out[order(-out$score, out$method), , drop = FALSE]
  attr(out, "query") <- list(dimension = dimension, change_in = change_in,
                             noise = noise, n = n,
                             need_uncertainty = need_uncertainty,
                             online = online)
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
  q <- attr(x, "query")
  cat("Recommended methods for: ", q$dimension, " series, change in ",
      q$change_in, ", ", q$noise, " noise",
      if (!is.null(q$n)) paste0(", n = ", q$n) else "",
      if (q$need_uncertainty) ", uncertainty required" else "",
      if (q$online) ", online" else "", "\n\n", sep = "")
  n_show <- min(top, nrow(x))
  for (i in seq_len(n_show)) {
    cat(i, ". ", x$method[i], " (", x$engine[i], ")",
        if (isTRUE(x$installed[i])) "" else "  [not installed]", "\n",
        sep = "")
    cat("   why: ", x$why[i], "\n", sep = "")
    if (!is.na(x$caveat[i])) cat("   caveat: ", x$caveat[i], "\n", sep = "")
  }
  if (nrow(x) > n_show) {
    cat("\n(", nrow(x) - n_show, " further candidate(s); the full table is ",
        "the return value.)\n", sep = "")
  }
  cat("\nCite the method you use with cpt_cite(). Cross-check the choice ",
      "with\ncpt_consensus() and cpt_sensitivity().\n", sep = "")
  invisible(x)
}
