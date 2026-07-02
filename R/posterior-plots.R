#' Posterior probability plot for Bayesian results
#'
#' Draws the classic Bayesian changepoint display: the series with its
#' posterior mean (top panel) and the per-location posterior probability of
#' a changepoint (bottom panel). Works with results from
#' \code{\link{bcp_wrapper}()} and \code{\link{beast_wrapper}()}.
#'
#' @param x A \code{ggcpt} object produced by a Bayesian wrapper.
#' @param prob_threshold Probability cutoff drawn as a horizontal reference
#'   line in the probability panel; defaults to the threshold recorded on
#'   the object (or 0.5).
#' @return A ggplot object (two facets sharing the x axis).
#' @export
#' @examplesIf requireNamespace("bcp", quietly = TRUE)
#' res <- bcp_wrapper(c(rnorm(60), rnorm(60, 4)), seed = 2026)
#' ggcpt_posterior(res)
ggcpt_posterior <- function(x, prob_threshold = NULL) {
  if (!is_ggcpt(x)) {
    stop("`x` must be a ggcpt object.", call. = FALSE)
  }

  prob <- posterior_prob_profile(x)
  if (is.null(prob)) {
    stop("No posterior probability profile found on this object. ",
         "ggcpt_posterior() supports results from bcp_wrapper() and ",
         "beast_wrapper().", call. = FALSE)
  }

  if (is.null(prob_threshold)) {
    prob_threshold <- if (identical(x$penalty$type, "prob_threshold")) {
      x$penalty$value
    } else {
      0.5
    }
  }

  n <- nrow(x$data)
  top <- tibble::tibble(index = x$data$index, y = x$data$value,
                        panel = "Series and posterior mean")
  bottom <- tibble::tibble(index = seq_len(n), y = prob,
                           panel = "Posterior changepoint probability")
  both <- rbind(top, bottom)
  both$panel <- factor(both$panel, levels = unique(both$panel))

  p <- ggplot2::ggplot(both, ggplot2::aes(index, y)) +
    ggplot2::geom_line(data = both[both$panel == levels(both$panel)[1], ],
                       color = "grey40") +
    ggplot2::geom_col(data = both[both$panel == levels(both$panel)[2], ],
                      fill = "steelblue", width = 1) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = "Index", y = NULL,
                  title = paste0("Bayesian changepoint posterior (",
                                 x$method, ")"))

  if ("fitted" %in% names(x$data)) {
    fit_df <- tibble::tibble(index = x$data$index, y = x$data$fitted,
                             panel = levels(both$panel)[1])
    p <- p + ggplot2::geom_line(data = fit_df, color = "darkred",
                                linewidth = 0.8)
  }

  thresh_df <- tibble::tibble(yint = prob_threshold,
                              panel = levels(both$panel)[2])
  p + ggplot2::geom_hline(data = thresh_df,
                          ggplot2::aes(yintercept = yint),
                          linetype = "dashed", color = "grey30")
}

# Internal: extract a length-n posterior probability profile from a
# Bayesian wrapper fit, or NULL when the engine did not provide one.
#' @noRd
posterior_prob_profile <- function(x) {
  n <- nrow(x$data)
  fit <- x$fit
  if (is.null(fit)) return(NULL)

  if (inherits(fit, "bcp")) {
    prob <- as.numeric(fit$posterior.prob)
    prob[is.na(prob)] <- 0
    return(prob[seq_len(n)])
  }

  if (inherits(fit, "beast")) {
    prob <- rep(0, n)
    cp <- as.numeric(fit$trend$cp)
    pr <- as.numeric(fit$trend$cpPr)
    keep <- !is.na(cp) & !is.na(pr)
    idx <- pmin(pmax(as.integer(round(cp[keep])) - 1L, 1L), n)
    prob[idx] <- pr[keep]
    return(prob)
  }

  NULL
}

#' Run-length posterior heatmap for Bayesian online results
#'
#' Draws the signature BOCPD graphic: the posterior distribution of the run
#' length (time since the last changepoint) at every observation, as a
#' heatmap, with the series overlaid on top. Works with results from
#' \code{\link{bocpd_wrapper}()}.
#'
#' @param x A \code{ggcpt} object produced by \code{bocpd_wrapper()}.
#' @param prob_floor Posterior probabilities below this value are not drawn
#'   (keeps the heatmap legible). Defaults to \code{1e-3}.
#' @return A ggplot object.
#' @export
#' @examplesIf requireNamespace("ocp", quietly = TRUE)
#' res <- bocpd_wrapper(c(rnorm(60), rnorm(60, 4)))
#' ggcpt_runlength(res)
ggcpt_runlength <- function(x, prob_floor = 1e-3) {
  if (!is_ggcpt(x) || !inherits(x$fit, "ocp")) {
    stop("`x` must be a ggcpt object produced by bocpd_wrapper().",
         call. = FALSE)
  }

  R <- x$fit$R
  if (is.null(R)) {
    stop("The ocp fit does not carry the run-length matrix; call ",
         "bocpd_wrapper() with getR = TRUE.", call. = FALSE)
  }

  R <- as.matrix(R)
  # ocp stores one time point per COLUMN (each column is a probability
  # distribution over run lengths, the rows).
  df <- do.call(rbind, lapply(seq_len(ncol(R)), function(t) {
    probs <- R[, t]
    keep <- which(is.finite(probs) & probs > prob_floor)
    if (length(keep) == 0) return(NULL)
    data.frame(time = t, run_length = keep - 1L, prob = probs[keep])
  }))

  if (is.null(df)) {
    stop("No run-length posterior mass exceeds `prob_floor` = ", prob_floor,
         "; lower it (probabilities are at most 1).", call. = FALSE)
  }

  ggplot2::ggplot(df, ggplot2::aes(time, run_length, fill = prob)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient(low = "grey95", high = "darkblue",
                                 name = "Posterior") +
    ggplot2::labs(x = "Index", y = "Run length",
                  title = "BOCPD run-length posterior") +
    ggplot2::geom_vline(xintercept = x$changepoints$cp, color = "red",
                        linetype = "dashed", linewidth = 0.4)
}

#' Interactive changepoint plot
#'
#' Renders a \code{ggcpt} result (or any ggplot built from one) as an
#' interactive HTML widget via \pkg{plotly}, with values on hover. A thin
#' convenience wrapper: the static \code{autoplot()} path is untouched.
#'
#' @param x A \code{ggcpt} object or a ggplot object.
#' @param ... Additional arguments passed to \code{autoplot()} when \code{x}
#'   is a \code{ggcpt} object.
#' @return A \code{plotly} htmlwidget.
#' @export
#' @examplesIf requireNamespace("plotly", quietly = TRUE) && interactive()
#' res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
#' ggcpt_interactive(res)
ggcpt_interactive <- function(x, ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required. ",
         "Install it with install.packages('plotly').", call. = FALSE)
  }
  p <- if (is_ggcpt(x)) autoplot.ggcpt(x, ...) else x
  if (!inherits(p, "ggplot")) {
    stop("`x` must be a ggcpt object or a ggplot.", call. = FALSE)
  }
  plotly::ggplotly(p)
}
