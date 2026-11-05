# ---------------------------------------------------------------------------
# Theme AH (0.7.0): epidemic changepoints. The ggcpt contract describes a
# series as regimes that follow one another, so a stretch that departs from
# a baseline and RETURNS to it has to be described as two changepoints and
# three segments of which the first and third merely happen to agree. CAPA
# (Fisch, Eckley and Fearnhead) models exactly that object: collective
# anomalies (intervals away from a persistent baseline) and point anomalies
# (single outliers), with linear-time penalised-cost search, and in the
# multivariate case the subset of coordinates each anomaly affects.
#
# The changepoints are the anomaly boundaries, so every plot, metric and
# tool keeps working; the anomalies themselves live in the `regions` slot,
# typed "collective" or "point", where a reader looking for the windows
# finds them.
# ---------------------------------------------------------------------------

#' CAPA wrapper: collective and point anomalies (epidemic changepoints)
#'
#' Wraps \code{anomaly::capa()} (Fisch, Eckley and Fearnhead 2022): finds
#' stretches of the series that depart from a persistent baseline and
#' return to it (\emph{collective} anomalies: an outbreak that subsides, a
#' fault that is repaired, a campaign that ends) and single outlying
#' observations (\emph{point} anomalies), for one series or many. For a
#' multivariate series it also reports which coordinates each anomaly
#' affects.
#'
#' @param x A numeric vector, or a matrix or data frame with one column per
#'   coordinate.
#' @param change_in \code{"mean"} (default) for anomalies in the mean,
#'   \code{"meanvar"} for anomalies in mean and variance.
#' @param robust For \code{change_in = "mean"} on a single series, use
#'   CAPA's robust cost (\code{type = "robustmean"}), which tolerates
#'   outliers inside an anomalous stretch. Defaults to \code{FALSE}.
#' @param beta Penalty for a collective anomaly: one number, or one per
#'   coordinate. \code{NULL} (default) uses the engine's own penalty.
#' @param beta_tilde Penalty for a point anomaly. \code{NULL} (default) uses
#'   the engine's \code{3 log(np)}.
#' @param min_seg_len Shortest collective anomaly, at least 2. Defaults to
#'   \code{10}, the engine's default.
#' @param max_seg_len Longest collective anomaly. Defaults to \code{Inf}.
#' @param max_lag For a multivariate series, how far an anomaly's start and
#'   end may differ between coordinates. Defaults to \code{0}.
#' @param standardise Centre and scale each coordinate by its median and
#'   median absolute deviation before the search. Defaults to \code{TRUE}.
#'   CAPA assumes a baseline of mean 0 and variance 1, so leave this on
#'   unless the data are already on that scale.
#' @param ... Not used; present so the dispatcher can pass its own
#'   arguments through. Anything supplied is refused by name.
#'
#' @section How the result reads:
#' The baseline is everything that is not anomalous, and it is one regime,
#' which is what separates this model from an ordinary segmentation. Each
#' collective anomaly from \code{s} to \code{e} contributes two
#' changepoints, \code{s - 1} (the last baseline observation before it) and
#' \code{e} (its last observation), so the segments alternate between the
#' baseline and the anomalies and every tool built on changepoints works
#' unchanged. The anomalies themselves are in the \code{regions} slot,
#' read with \code{\link{cpt_regions}()}: one row per anomaly with
#' \code{start}, \code{end}, \code{type} (\code{"collective"} or
#' \code{"point"}), \code{coordinates} (the affected coordinates, comma
#' separated), \code{mean_change} (the engine's squared standardised mean
#' change for a collective anomaly) and \code{strength} (the engine's
#' strength for a point anomaly). Point anomalies are outliers, not
#' changes, so they add no changepoint. \code{autoplot()} shades the
#' collective anomalies and rings the point ones, and
#' \code{\link{cpt_attribute}()} can certify, with error control, which
#' coordinates changed at each boundary.
#'
#' A series whose two halves differ, with no baseline to return to, is not
#' what CAPA models: the median of such a series lies between the two
#' levels, so both halves look anomalous. Use an ordinary detector for a
#' step.
#'
#' @return A \code{ggcpt} object whose changepoints are the anomaly
#'   boundaries and whose \code{regions} slot lists the anomalies, plus a
#'   \code{baseline} element holding the centre and scale used to
#'   standardise each coordinate.
#' @references
#' \insertRef{fisch2022capa}{ggchangepoint}
#'
#' \insertRef{fisch2022mvcapa}{ggchangepoint}
#' @seealso \code{\link{cpt_regions}()}, \code{\link{cpt_attribute}()}.
#' @export
#' @examplesIf requireNamespace("anomaly", quietly = TRUE)
#' set.seed(2026)
#' x <- c(rnorm(150), rnorm(40, 3), rnorm(150))
#' x[60] <- 9
#' fit <- capa_wrapper(x)
#' cpt_regions(fit)
#' ggplot2::autoplot(fit)
#' @family changepoint engines
capa_wrapper <- function(x, change_in = c("mean", "meanvar"), robust = FALSE,
                         beta = NULL, beta_tilde = NULL, min_seg_len = 10,
                         max_seg_len = Inf, max_lag = 0, standardise = TRUE,
                         ...) {
  need_pkg("anomaly")
  change_in <- cpt_match_arg(change_in)
  validate_flag(robust, "robust")
  validate_flag(standardise, "standardise")
  dots <- list(...)
  if (length(dots)) {
    nm <- names(dots)
    cpt_abort("`capa` takes no further engine arguments, but got ",
              if (length(nm) && all(nzchar(nm))) {
                paste0("`", paste(nm, collapse = "`, `"), "`")
              } else {
                paste(length(dots), "unnamed argument(s)")
              }, ". Its tuning arguments are `beta`, `beta_tilde`, ",
              "`min_seg_len`, `max_seg_len` and `max_lag`.",
              class = "bad_argument")
  }
  validate_scalar(min_seg_len, "min_seg_len", min = 2)
  if (!identical(max_seg_len, Inf)) {
    validate_scalar(max_seg_len, "max_seg_len", min = min_seg_len)
  }
  validate_scalar(max_lag, "max_lag", min = 0)
  if (!is.null(beta_tilde)) validate_scalar(beta_tilde, "beta_tilde", min = 0)
  validate_data(x)
  X <- if (is.matrix(x) || is.data.frame(x)) as_mv_matrix(x) else
    matrix(coerce_series_values(x), ncol = 1L,
           dimnames = list(NULL, "V1"))
  n <- nrow(X)
  p <- ncol(X)
  if (!is.null(beta)) {
    validate_grid(beta, "beta", min = 0)
    if (!length(beta) %in% c(1L, p)) {
      cpt_abort("`beta` must be one number or one per coordinate (", p,
                "), not ", length(beta), ".", class = "bad_argument")
    }
  }
  if (robust && (p > 1L || change_in != "mean")) {
    cpt_abort("`robust = TRUE` is CAPA's robust mean cost, which the engine ",
              "offers for a change in mean on a single series only.",
              class = "bad_argument")
  }
  if (n < 2L * min_seg_len) {
    cpt_abort("`capa` needs at least twice `min_seg_len` observations (",
              2 * min_seg_len, ") to tell an anomaly from its baseline, but ",
              "the series has ", n, ". Lower `min_seg_len`, or use a longer ",
              "series.", class = "short_series", data = list(n = n))
  }
  data_vec <- as.numeric(X[, 1L])
  centre <- apply(X, 2L, stats::median)
  scale <- apply(X, 2L, stats::mad)
  flat <- !is.finite(scale) | scale <= 0
  if (any(flat)) {
    sdv <- apply(X, 2L, stats::sd)
    scale[flat] <- sdv[flat]
  }
  if (any(!is.finite(scale) | scale <= 0)) {
    bad <- colnames(X)[!is.finite(scale) | scale <= 0]
    cpt_abort("`capa` measures anomalies against each coordinate's spread, ",
              "and ", paste(bad, collapse = ", "), " ",
              if (length(bad) > 1) "do" else "does", " not vary. Drop ",
              if (length(bad) > 1) "those coordinates" else "that coordinate",
              ".", class = "input_error")
  }
  Z <- if (standardise) sweep(sweep(X, 2L, centre), 2L, scale, "/") else X
  type <- if (robust) "robustmean" else change_in
  args <- list(x = if (p == 1L) as.numeric(Z) else unname(Z), type = type,
               min_seg_len = as.integer(min_seg_len),
               max_seg_len = max_seg_len, max_lag = as.integer(max_lag))
  if (!is.null(beta)) args$beta <- if (length(beta) == 1L) rep(beta, p) else
    beta
  if (!is.null(beta_tilde)) args$beta_tilde <- beta_tilde
  fit <- with_engine_errors(do.call(anomaly::capa, args), "capa")

  coll <- as.data.frame(anomaly::collective_anomalies(fit))
  pts <- as.data.frame(anomaly::point_anomalies(fit))
  regions <- capa_regions(coll, pts, colnames(X))
  coll_reg <- regions[regions$type == "collective", , drop = FALSE]
  cps <- sort(unique(c(coll_reg$start - 1L, coll_reg$end)))
  cps <- cps[cps >= 1L & cps < n]

  res <- ggcpt_build(
    data_vec, cps, method = "capa", change_in = change_in,
    penalty = list(type = "beta",
                   value = if (is.null(beta)) NA_real_ else beta[1]),
    fit = fit, call = match.call(),
    data_wide = if (p > 1L) mv_data_wide(X) else NULL,
    regions = regions
  )
  res$region_kind <- "anomaly"
  res$baseline <- list(centre = centre, scale = scale,
                       standardised = standardise)
  res
}

# Internal: CAPA's two accessor tables as one regions tibble. A collective
# anomaly is reported once per affected coordinate (with that coordinate's
# lags), so rows sharing a start and end are one anomaly.
#' @noRd
capa_regions <- function(coll, pts, coord_names) {
  name_of <- function(v) {
    v <- as.integer(v)
    ifelse(v >= 1L & v <= length(coord_names), coord_names[v],
           as.character(v))
  }
  rows <- list()
  if (nrow(coll)) {
    key <- paste(coll$start, coll$end)
    for (k in unique(key)) {
      g <- coll[key == k, , drop = FALSE]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        start = as.integer(g$start[1]), end = as.integer(g$end[1]),
        type = "collective",
        coordinates = paste(name_of(g$variate), collapse = ", "),
        mean_change = if ("mean.change" %in% names(g)) {
          sum(as.numeric(g$mean.change))
        } else NA_real_,
        strength = NA_real_)
    }
  }
  if (nrow(pts)) {
    key <- as.integer(pts$location)
    for (k in unique(key)) {
      g <- pts[key == k, , drop = FALSE]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        start = k, end = k, type = "point",
        coordinates = paste(name_of(g$variate), collapse = ", "),
        mean_change = NA_real_,
        strength = max(as.numeric(g$strength)))
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(start = integer(), end = integer(),
                          type = character(), coordinates = character(),
                          mean_change = numeric(), strength = numeric()))
  }
  out <- do.call(rbind, rows)
  out[order(out$start, out$end), , drop = FALSE]
}
