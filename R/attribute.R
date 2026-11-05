# ---------------------------------------------------------------------------
# Theme O (0.7.0): attribution. A multivariate detector says WHEN something
# changed; the question a user with forty sensors has next is WHICH of them
# changed. Testing each coordinate at the detected location is the obvious
# answer and an invalid one: the location was chosen where the aggregate
# evidence peaks, so a coordinate that never changed is tested at the one
# split most favourable to it. Peng, Wu, Yan, Chen and Shen (2026) measure
# the family-wise error of that practice at 0.33 for five coordinates and
# 0.89 for twenty.
#
# ARM (Attribution by Rank Maxima) scores each coordinate by the MAXIMUM of
# a rank statistic over every split of the window, not by its value at the
# detected split. The maximum dominates the value at any split the detector
# could have picked, so the certificate does not depend on the detector at
# all, and because it is built from within-coordinate ranks its null law is
# a permutation law: exact in finite samples, distribution-free.
# ---------------------------------------------------------------------------

#' Which coordinates changed? Certified attribution for a multivariate fit
#'
#' For each changepoint of a multivariate result, tests every coordinate
#' for a change in its window and returns the set certified to have
#' changed, with family-wise error or false discovery rate control and a
#' location or scale label for each certified coordinate. This is ARM,
#' attribution by rank maxima (Peng et al. 2026). It accepts the
#' changepoints of any detector, including a registered one or a result
#' built with \code{\link{as_ggcpt}()}, and its guarantee does not depend
#' on how, or how well, they were located.
#'
#' @param object A multivariate \code{ggcpt} object (one that carries two
#'   or more coordinates in \code{$data_wide}).
#' @param alpha Error level. Defaults to \code{0.05}.
#' @param control What \code{alpha} controls, within each changepoint's
#'   window:
#'   \describe{
#'     \item{\code{"fwer"}}{(default) the family-wise error rate, by a
#'       Westfall-Young single-step maxT over joint row permutations, which
#'       keeps whatever dependence the coordinates have. Exact when the
#'       coordinates that did not change have the same joint distribution
#'       before and after the change.}
#'     \item{\code{"holm"}}{the family-wise error rate by Holm's procedure
#'       on the per-coordinate permutation p-values: valid with no
#'       assumption on how the coordinates depend on each other, at some
#'       cost in power.}
#'     \item{\code{"fdr"}}{the false discovery rate, by Benjamini and
#'       Yekutieli's procedure, which holds under any dependence between
#'       coordinates. The choice for many coordinates.}
#'   }
#' @param B Number of permutations. Defaults to \code{999}. The smallest
#'   attainable p-value is \code{1 / (B + 1)}, so Holm and the false
#'   discovery rate need \code{B} of the order of the number of
#'   coordinates divided by \code{alpha}; the function warns when
#'   \code{B} is too small for \code{control} to certify anything.
#' @param margin Smallest number of observations on either side of a
#'   split, within the window. Defaults to five per cent of the window,
#'   and at least 2. It changes power, not validity.
#' @param seed Optional seed for the permutations, scoped to this call.
#'
#' @section What is certified, and what is not:
#' Each changepoint is examined in its own \emph{window}: the stretch of
#' the series between its neighbouring changepoints (the whole series for
#' a single changepoint). Coordinate \eqn{j} is tested for the null
#' hypothesis that its values in the window are identically distributed,
#' that is, that it did not change there. For every split \eqn{t} of the
#' window the location evidence is the standardised Wilcoxon rank sum and
#' the scale evidence the standardised Mood statistic (squared centred
#' ranks), each computed from that coordinate's own ranks; the
#' coordinate's statistic is the largest of the two over every split. That
#' maximum is what makes the result independent of the detector: no
#' estimate of the location can make a coordinate that did not change
#' look more changed than the maximum already allows for.
#'
#' The guarantees are per changepoint. With \eqn{K} changepoints the
#' chance of any false attribution anywhere is at most \eqn{K} times
#' \code{alpha} under \code{"fwer"} and \code{"holm"}. They also take the
#' window as given: the neighbouring changepoints were located from the
#' same data, so for a fit with several changepoints the guarantee holds
#' conditionally on those windows, and exactly for a single changepoint.
#' Rows of the window are assumed independent (permutation is exchangeable
#' only then), so serially dependent data need care, as everywhere else.
#'
#' The type label is descriptive and carries no error control of its own:
#' \code{"scale"} when the scale evidence is the larger, \code{"location"}
#' otherwise.
#'
#' @return A \code{ggcpt_attribution} tibble with one row per changepoint
#'   and coordinate: \code{cp}, \code{cp_index} (when the result has a time
#'   index), \code{coordinate}, \code{statistic} (the coordinate's maximum
#'   over splits and channels), \code{statistic_location},
#'   \code{statistic_scale}, \code{p_value} (the per-coordinate permutation
#'   p-value), \code{p_adjusted} (adjusted for \code{control}),
#'   \code{changed} (\code{TRUE} when certified at \code{alpha}),
#'   \code{type} (\code{"location"} or \code{"scale"} for a certified
#'   coordinate, \code{NA} otherwise), \code{window_start},
#'   \code{window_end}, \code{n_window} (rows used, after dropping any with
#'   a missing value) and \code{source} (\code{"arm"}). \code{print()},
#'   \code{autoplot()} (a coordinate-by-changepoint map with the certified
#'   cells outlined), \code{tidy()} and \code{glance()} work on it.
#' @references
#' \insertRef{peng2026arm}{ggchangepoint}
#' @seealso \code{\link{cpt_confint}()} for \emph{where} a changepoint is;
#'   \code{\link{cpt_attribute_event}()} for attributing a change to a
#'   named event rather than to a coordinate.
#' @family inference
#' @export
#' @examples
#' set.seed(2026)
#' n <- 120
#' X <- matrix(rnorm(n * 6), n, 6,
#'             dimnames = list(NULL, paste0("sensor", 1:6)))
#' X[61:n, 2] <- X[61:n, 2] + 2   # a shift in sensor2
#' X[61:n, 5] <- X[61:n, 5] * 3   # a scale change in sensor5
#' fit <- cpt_detect(X, method = "ecp")
#' att <- cpt_attribute(fit, B = 199, seed = 1)
#' att
#' ggplot2::autoplot(att)
cpt_attribute <- function(object, alpha = 0.05,
                          control = c("fwer", "holm", "fdr"),
                          B = 999, margin = NULL, seed = NULL) {
  if (!is_ggcpt(object)) {
    cpt_abort("`object` must be a ggcpt object.", class = "bad_argument")
  }
  control <- cpt_match_arg(control)
  validate_scalar(alpha, "alpha", min = 0, max = 1, min_open = TRUE,
                  max_open = TRUE)
  validate_scalar(B, "B", min = 1)
  B <- as.integer(B)
  if (!is.null(margin)) validate_scalar(margin, "margin", min = 1)

  X <- attribution_matrix(object)
  d <- ncol(X)
  n <- nrow(X)
  min_b <- attribution_min_B(control, alpha, d)
  if (B < min_b) {
    cpt_warn("With `B = ", B, "` the smallest attainable p-value is 1/",
             B + 1L, ", so `control = \"", control, "\"` cannot certify ",
             "any of the ", d, " coordinates at `alpha = ", format(alpha),
             "`. Use `B >= ", min_b, "`.", class = "argument_ignored",
             data = list(B = B, needed = min_b))
  }
  local_seed(seed)

  cp <- sort(unique(as.integer(object$changepoints$cp)))
  cp <- cp[cp >= 1L & cp < n]
  if (!length(cp)) return(empty_attribution(object, alpha, control, B))
  bounds <- c(0L, cp, n)
  rows <- vector("list", length(cp))
  short <- character(0)
  for (k in seq_along(cp)) {
    lo <- bounds[k] + 1L
    hi <- bounds[k + 2L]
    W <- X[lo:hi, , drop = FALSE]
    W <- W[stats::complete.cases(W), , drop = FALSE]
    res <- arm_window(W, B = B, margin = margin)
    if (is.null(res)) {
      short <- c(short, as.character(cp[k]))
      res <- list(M = rep(NA_real_, d), loc = rep(NA_real_, d),
                  sc = rep(NA_real_, d), p = rep(NA_real_, d),
                  p_wy = rep(NA_real_, d))
    }
    p_adj <- switch(control,
      fwer = res$p_wy,
      holm = stats::p.adjust(res$p, method = "holm"),
      fdr = stats::p.adjust(res$p, method = "BY")
    )
    changed <- !is.na(p_adj) & p_adj <= alpha
    type <- ifelse(changed, ifelse(res$sc > res$loc, "scale", "location"),
                   NA_character_)
    rows[[k]] <- tibble::tibble(
      cp = cp[k], coordinate = colnames(X), statistic = res$M,
      statistic_location = res$loc, statistic_scale = res$sc,
      p_value = res$p, p_adjusted = p_adj, changed = changed, type = type,
      window_start = lo, window_end = hi, n_window = nrow(W),
      source = "arm")
  }
  if (length(short)) {
    cpt_warn("The window around changepoint(s) ", paste(short, collapse = ", "),
             " is too short to test (a split needs `margin` complete rows ",
             "on each side, and at least 5 in all), so those rows are NA.",
             class = "short_series")
  }
  out <- do.call(rbind, rows)
  if (!is.null(object$index)) {
    out <- tibble::add_column(out, cp_index = object$index[out$cp],
                              .after = "cp")
  }
  structure(out, class = c("ggcpt_attribution", class(tibble::tibble())),
            alpha = alpha, control = control, B = B,
            method = scalar_chr(object$method))
}

# Internal: the coordinates of a multivariate result as a matrix, refusing
# a univariate one with the tools that answer its question.
#' @noRd
attribution_matrix <- function(object) {
  if (n_coordinates(object) < 2L) {
    cpt_abort("Attribution asks which of several coordinates changed, and ",
              "this result has one. For a single series use cpt_test() ",
              "(is this changepoint real?) or cpt_test_null() (did ",
              "anything change?).", class = "wrong_dimension")
  }
  wide <- object$data_wide
  coords <- setdiff(names(wide), c("index", "index_value"))
  X <- as.matrix(as.data.frame(wide)[, coords, drop = FALSE])
  storage.mode(X) <- "double"
  colnames(X) <- coords
  X
}

# Internal: the smallest B for which `control` can certify one coordinate.
#' @noRd
attribution_min_B <- function(control, alpha, d) {
  need <- switch(control,
    fwer = 1 / alpha,
    holm = d / alpha,
    fdr = d * sum(1 / seq_len(d)) / alpha
  )
  as.integer(ceiling(need - 1))
}

# Internal: an empty attribution table, for a fit with no changepoints.
#' @noRd
empty_attribution <- function(object, alpha, control, B) {
  out <- tibble::tibble(cp = integer(), coordinate = character(),
                        statistic = numeric(), statistic_location = numeric(),
                        statistic_scale = numeric(), p_value = numeric(),
                        p_adjusted = numeric(), changed = logical(),
                        type = character(), window_start = integer(),
                        window_end = integer(), n_window = integer(),
                        source = character())
  if (!is.null(object$index)) {
    out <- tibble::add_column(out, cp_index = object$index[integer(0)],
                              .after = "cp")
  }
  structure(out, class = c("ggcpt_attribution", class(tibble::tibble())),
            alpha = alpha, control = control, B = B,
            method = scalar_chr(object$method))
}

# Internal: ARM on one window. `W` holds the window's complete rows, one
# column per coordinate. Returns the observed statistics, the marginal
# permutation p-values and the Westfall-Young adjusted ones, all from ONE
# set of joint row permutations: each permuted column is a uniform
# permutation of that coordinate's own values, so the same draws calibrate
# every coordinate marginally (valid with ties, since a coordinate's own
# tie pattern travels with it) and their row-wise maximum calibrates the
# family. NULL when the window is too short for any split.
#' @noRd
arm_window <- function(W, B, margin = NULL) {
  m <- nrow(W)
  if (is.null(margin)) margin <- max(2L, floor(0.05 * m))
  margin <- as.integer(margin)
  if (m < 2L * margin + 1L || m < 5L) return(NULL)
  grid <- seq.int(margin, m - margin)
  ch <- arm_prepare(W)
  obs <- arm_channels(ch, seq_len(m), grid)
  d <- ncol(W)
  null_M <- matrix(0, B, d)
  for (b in seq_len(B)) {
    null_M[b, ] <- arm_channels(ch, sample.int(m), grid)$M
  }
  # A permuted statistic equal to the observed one counts against it; the
  # tolerance keeps a floating-point tie on the conservative side.
  tol <- 1e-10 * pmax(1, abs(obs$M))
  ge <- null_M >= rep(obs$M - tol, each = B)
  p <- (1 + colSums(ge)) / (B + 1)
  row_max <- apply(null_M, 1L, max)
  p_wy <- vapply(seq_len(d), function(j) {
    (1 + sum(row_max >= obs$M[j] - tol[j])) / (B + 1)
  }, numeric(1))
  list(M = obs$M, loc = obs$loc, sc = obs$sc, p = p, p_wy = p_wy)
}

# Internal: the two centred rank channels of a window, and their sums of
# squares, which permutation leaves unchanged.
#' @noRd
arm_prepare <- function(W) {
  R <- apply(W, 2L, rank, ties.method = "average")
  if (!is.matrix(R)) R <- matrix(R, ncol = ncol(W))
  C <- sweep(R, 2L, colMeans(R))
  S <- C^2
  S <- sweep(S, 2L, colMeans(S))
  list(C = C, S = S, ss_c = colSums(C^2), ss_s = colSums(S^2), m = nrow(W))
}

# Internal: max-over-splits statistics for the rows of a window taken in
# the order `ord`. For a split after t rows the location evidence is
# |sum of the first t centred ranks| over its permutation standard
# deviation, sqrt(t (m - t) / (m (m - 1)) * sum(C^2)), and the scale
# evidence the same for the centred squared ranks: the standardised
# Wilcoxon and Mood statistics. A constant coordinate has no evidence.
#' @noRd
arm_channels <- function(ch, ord, grid) {
  m <- ch$m
  f <- grid * (m - grid) / (m * (m - 1))
  cs_c <- apply(ch$C[ord, , drop = FALSE], 2L, cumsum)
  cs_s <- apply(ch$S[ord, , drop = FALSE], 2L, cumsum)
  if (!is.matrix(cs_c)) {
    cs_c <- matrix(cs_c, ncol = ncol(ch$C))
    cs_s <- matrix(cs_s, ncol = ncol(ch$C))
  }
  zl <- abs(cs_c[grid, , drop = FALSE]) / sqrt(outer(f, ch$ss_c))
  zs <- abs(cs_s[grid, , drop = FALSE]) / sqrt(outer(f, ch$ss_s))
  zl[!is.finite(zl)] <- 0
  zs[!is.finite(zs)] <- 0
  loc <- apply(zl, 2L, max)
  sc <- apply(zs, 2L, max)
  list(M = pmax(loc, sc), loc = loc, sc = sc)
}

#' @rdname cpt_attribute
#' @param x A \code{ggcpt_attribution} object.
#' @param ... Ignored.
#' @export
print.ggcpt_attribution <- function(x, ...) {
  ctl <- switch(attr(x, "control") %||% "fwer",
    fwer = "family-wise error (Westfall-Young)",
    holm = "family-wise error (Holm)",
    fdr = "false discovery rate (Benjamini-Yekutieli)"
  )
  cat("ggcpt_attribution (ARM: attribution by rank maxima)\n")
  cat_field("Controls", paste0(ctl, " at ", format(attr(x, "alpha"))))
  cat_field("Permutations", format(attr(x, "B")))
  if (nrow(x)) {
    cps <- unique(x$cp)
    for (k in cps) {
      rows <- x[x$cp == k, , drop = FALSE]
      hit <- rows$changed %in% TRUE
      lab <- if (any(hit)) {
        paste0(rows$coordinate[hit], " (", rows$type[hit], ")",
               collapse = ", ")
      } else {
        "none certified"
      }
      cat_field(paste0("Changepoint ", k),
                paste0(sum(hit), " of ", nrow(rows), ": ", lab))
    }
    cat("\n")
    print(tibble::as_tibble(x), n = 10)
  } else {
    cat("\nNo changepoints, so nothing to attribute.\n")
  }
  invisible(x)
}

#' @rdname cpt_attribute
#' @export
tidy.ggcpt_attribution <- function(x, ...) {
  tibble::as_tibble(x)
}

#' @rdname cpt_attribute
#' @export
glance.ggcpt_attribution <- function(x, ...) {
  d <- tibble::as_tibble(x)
  if (!nrow(d)) {
    return(tibble::tibble(cp = integer(), n_coordinates = integer(),
                          n_changed = integer(), n_location = integer(),
                          n_scale = integer(), alpha = numeric(),
                          control = character()))
  }
  cps <- unique(d$cp)
  tibble::tibble(
    cp = cps,
    n_coordinates = vapply(cps, function(k) sum(d$cp == k), integer(1)),
    n_changed = vapply(cps, function(k) sum(d$changed[d$cp == k] %in% TRUE),
                       integer(1)),
    n_location = vapply(cps, function(k) {
      sum(d$type[d$cp == k] %in% "location")
    }, integer(1)),
    n_scale = vapply(cps, function(k) {
      sum(d$type[d$cp == k] %in% "scale")
    }, integer(1)),
    alpha = attr(x, "alpha") %||% NA_real_,
    control = attr(x, "control") %||% NA_character_
  )
}

#' @rdname cpt_attribute
#' @export
autoplot.ggcpt_attribution <- function(object, ...) {
  d <- tibble::as_tibble(object)
  if (!nrow(d)) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No changepoints, so nothing to attribute"))
  }
  lab <- if ("cp_index" %in% names(d)) {
    paste0(d$cp, " (", format(d$cp_index), ")")
  } else {
    as.character(d$cp)
  }
  d$.label <- factor(lab, levels = unique(lab))
  d$coordinate <- factor(d$coordinate, levels = rev(unique(d$coordinate)))
  d$evidence <- -log10(pmax(d$p_adjusted, 1e-300))
  cert <- d[d$changed %in% TRUE, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = .label, y = coordinate,
                                  fill = evidence)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_tile(data = cert, colour = "black", linewidth = 1,
                       fill = NA) +
    ggplot2::geom_text(data = cert, ggplot2::aes(label = substr(type, 1, 1)),
                       size = 3.5, colour = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#f2f0f7", high = "#54278f",
                                 name = "-log10 adjusted p") +
    ggplot2::labs(x = "Changepoint", y = NULL,
                  title = "Which coordinates changed (ARM)",
                  subtitle = paste0("Outlined: certified at ",
                                    format(attr(object, "alpha")),
                                    "; l = location, s = scale")) +
    theme_ggcpt()
}
