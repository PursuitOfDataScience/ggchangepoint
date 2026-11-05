# ---------------------------------------------------------------------------
# Theme BF (0.7.0): uncertainty about K. cpt_select() scores every candidate
# number of changepoints and returns the winner, and nothing said how much
# better the winner was. An information criterion on the deviance scale
# turns into weights the standard way (Akaike weights; for BIC, approximate
# posterior model probabilities under equal prior weight), so "K = 3" can
# become "K = 3 has 0.55 of the weight and K = 4 has 0.31", and a flat
# bottom (the data do not determine K) becomes visible. Averaging the
# candidate segmentations under those weights gives every location a
# model-averaged probability of being a changepoint, for any method that
# can produce a ladder, not only the two Bayesian engines that report one.
# ---------------------------------------------------------------------------

#' How sure is the number of changepoints? Weights over K
#'
#' Turns the criterion curve of \code{\link{cpt_select}()} into weights over
#' the candidate numbers of changepoints, and averages the candidate
#' segmentations under those weights into a probability, at each location,
#' that it is a changepoint.
#'
#' @param sel A \code{ggcpt_selection} from \code{\link{cpt_select}()} with
#'   \code{criterion = "bic"}, \code{"mbic"} or \code{"aic"}: the criteria
#'   on the deviance scale, where weights have their meaning.
#'
#' @section The weights:
#' With \eqn{\Delta_K} the criterion of \eqn{K} changepoints minus the
#' smallest, the weight of \eqn{K} is \eqn{\exp(-\Delta_K / 2)} normalised
#' over the candidates. For \code{"aic"} these are Akaike weights; for
#' \code{"bic"} and \code{"mbic"} they approximate posterior probabilities
#' of each \eqn{K} under equal prior weight. They are conditional on the
#' candidate segmentations: each \eqn{K} is represented by the one
#' segmentation the ladder found for it, so the location probabilities
#' reflect uncertainty about how many changes there are, not about where a
#' given one falls (\code{\link{cpt_confint}()} answers that).
#'
#' @return A \code{ggcpt_k_weights} tibble with one row per candidate:
#'   \code{k}, \code{value} (the criterion), \code{delta}, \code{weight}
#'   and \code{chosen}. \code{tidy(x, what = "locations")} returns the
#'   model-averaged probability of a changepoint at each location any
#'   candidate uses (\code{position}, \code{index}, \code{probability});
#'   \code{autoplot(x)} draws the weights and \code{autoplot(x, what =
#'   "locations")} the probabilities. \code{print()} and \code{glance()}
#'   (the chosen K, its weight, and the smallest set of K holding 95\% of
#'   the weight) work on it.
#' @seealso \code{\link{cpt_select}()}.
#' @family inference
#' @export
#' @examples
#' set.seed(2026)
#' x <- c(rnorm(80), rnorm(80, 1.2), rnorm(80, 0.4))
#' sel <- cpt_select(x, criterion = "bic", k_max = 6)
#' w <- cpt_k_weights(sel)
#' w
#' tidy(w, what = "locations")
#' ggplot2::autoplot(w)
cpt_k_weights <- function(sel) {
  if (!inherits(sel, "ggcpt_selection")) {
    cpt_abort("`sel` must be a ggcpt_selection from cpt_select().",
              class = "bad_argument")
  }
  crit <- sel$criterion
  if (!crit %in% c("bic", "mbic", "aic")) {
    cpt_abort("Weights over K need a criterion on the deviance scale, and `",
              crit, "` is not one: use cpt_select(criterion = \"bic\"), ",
              "\"mbic\" or \"aic\".", class = "capability_absent")
  }
  tab <- sel$criterion_table
  ok <- is.finite(tab$value)
  delta <- rep(NA_real_, nrow(tab))
  weight <- rep(0, nrow(tab))
  if (any(ok)) {
    delta[ok] <- tab$value[ok] - min(tab$value[ok])
    raw <- exp(-delta[ok] / 2)
    weight[ok] <- raw / sum(raw)
  }
  out <- tibble::tibble(k = tab$k, value = tab$value, delta = delta,
                        weight = weight, chosen = tab$chosen)
  idx <- sel$index %||% seq_along(sel$data)
  pos <- sort(unique(unlist(tab$cpts[ok])))
  pos <- pos[!is.na(pos)]
  prob <- vapply(pos, function(p) {
    sum(weight[vapply(tab$cpts, function(cp) p %in% cp, logical(1))])
  }, numeric(1))
  locations <- tibble::tibble(position = as.integer(pos), index = idx[pos],
                              probability = prob)
  structure(out, class = c("ggcpt_k_weights", class(tibble::tibble())),
            criterion = crit, locations = locations)
}

#' @rdname cpt_k_weights
#' @param x A \code{ggcpt_k_weights} object.
#' @param ... Ignored.
#' @export
print.ggcpt_k_weights <- function(x, ...) {
  g <- glance.ggcpt_k_weights(x)
  cat("ggcpt_k_weights (weights over the number of changepoints, from ",
      attr(x, "criterion"), ")\n", sep = "")
  cat_field("Chosen", paste0("K = ", g$k, ", weight ",
                             format(round(g$weight, 3))))
  top <- x[order(-x$weight), , drop = FALSE]
  top <- top[seq_len(min(3L, nrow(top))), , drop = FALSE]
  cat_field("Largest weights", paste0("K = ", top$k, ": ",
                                      format(round(top$weight, 3)),
                                      collapse = "; "))
  cat_field("95% of the weight", paste0("K in {", g$k_set_95, "}"))
  cat("\n")
  print(tibble::as_tibble(x))
  invisible(x)
}

#' @rdname cpt_k_weights
#' @param what \code{"k"} (default) for the weights, \code{"locations"} for
#'   the model-averaged location probabilities.
#' @export
tidy.ggcpt_k_weights <- function(x, what = c("k", "locations"), ...) {
  what <- cpt_match_arg(what)
  if (what == "locations") return(attr(x, "locations"))
  tibble::as_tibble(x)
}

#' @rdname cpt_k_weights
#' @export
glance.ggcpt_k_weights <- function(x, ...) {
  o <- order(-x$weight)
  cum <- cumsum(x$weight[o])
  keep <- o[seq_len(min(length(o), which(cum >= 0.95)[1] %||% length(o)))]
  chosen <- which(x$chosen)[1]
  tibble::tibble(k = x$k[chosen], weight = x$weight[chosen],
                 k_set_95 = paste(sort(x$k[keep]), collapse = ", "),
                 criterion = attr(x, "criterion"))
}

#' @rdname cpt_k_weights
#' @param object A \code{ggcpt_k_weights} object.
#' @export
autoplot.ggcpt_k_weights <- function(object, what = c("k", "locations"),
                                     ...) {
  what <- cpt_match_arg(what)
  if (what == "locations") {
    loc <- attr(object, "locations")
    return(ggplot2::ggplot(loc, ggplot2::aes(x = index, y = probability)) +
             ggplot2::geom_segment(ggplot2::aes(xend = index, yend = 0),
                                   colour = "#5e3c99") +
             ggplot2::geom_point(colour = "#5e3c99", size = 2) +
             ggplot2::coord_cartesian(ylim = c(0, 1)) +
             ggplot2::labs(x = NULL, y = "Probability of a changepoint",
                           title = "Model-averaged changepoint locations") +
             theme_ggcpt())
  }
  d <- tibble::as_tibble(object)
  d$status <- ifelse(d$chosen, "chosen", "candidate")
  ggplot2::ggplot(d, ggplot2::aes(x = factor(k), y = weight, fill = status)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(chosen = "#5e3c99",
                                          candidate = "grey70"), name = NULL) +
    ggplot2::labs(x = "Number of changepoints (K)", y = "Weight",
                  title = "How sure is K?",
                  subtitle = paste0("Weights from ", attr(object, "criterion"))) +
    theme_ggcpt()
}
