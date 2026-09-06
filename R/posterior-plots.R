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
    # `cpt_methods()` reports posterior = TRUE for bocpd and mcp as well,
    # because both are Bayesian and both quantify the location -- but
    # neither exposes the per-location profile this function draws, so
    # arriving here from that column used to be a dead end. Name the
    # accessor that does work instead.
    extra <- switch(
      as.character(x$method),
      bocpd = paste0(" A BOCPD result carries a posterior over run lengths",
                     " rather than over locations: see ggcpt_runlength()."),
      mcp = paste0(" An mcp result carries the posterior as",
                   " `ci_lower`/`ci_upper` on the changepoints tibble and",
                   " `fitted` on $data, not as a per-location profile."),
      "")
    stop("No posterior probability profile found on this object. ",
         "ggcpt_posterior() supports results from bcp_wrapper() and ",
         "beast_wrapper().", extra, call. = FALSE)
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
#' interactive HTML widget, with values on hover. A thin convenience
#' wrapper: the static \code{autoplot()} path is untouched.
#'
#' @param x A \code{ggcpt} object or a ggplot object.
#' @param engine Which renderer: \code{"plotly"} (the default) rebuilds the
#'   plot in plotly's own model, which is richer but loses layers plotly
#'   does not know; \code{"ggiraph"} renders the ggplot itself to
#'   interactive SVG, so faceting and every layer survive and the result
#'   composes with other htmlwidgets. Neither is a dependency; whichever you
#'   ask for must be installed.
#' @param width_svg,height_svg Figure size in inches for
#'   \code{engine = "ggiraph"}.
#' @param ... Additional arguments passed to \code{autoplot()} when \code{x}
#'   is a \code{ggcpt} object.
#' @return A \pkg{plotly} or \pkg{ggiraph} htmlwidget.
#' @export
#' @examplesIf requireNamespace("plotly", quietly = TRUE) && interactive()
#' res <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "pelt")
#' ggcpt_interactive(res)
#' @family plotting
ggcpt_interactive <- function(x, engine = c("plotly", "ggiraph"),
                              width_svg = 8, height_svg = 5, ...) {
  engine <- match.arg(engine)
  if (!requireNamespace(engine, quietly = TRUE)) {
    stop("Package '", engine, "' is required for ",
         "`engine = \"", engine, "\"`. Install it with ",
         "install.packages('", engine, "').", call. = FALSE)
  }
  p <- if (is_ggcpt(x)) autoplot.ggcpt(x, ...) else x
  if (!inherits(p, "ggplot")) {
    stop("`x` must be a ggcpt object or a ggplot.", call. = FALSE)
  }
  if (engine == "plotly") {
    return(plotly::ggplotly(p))
  }
  # Checked on the ggiraph path only, because that is the only path that uses
  # them (the plotly branch above ignores both, as documented). Without this,
  # girafe() answers "`width` must be a scalar positive number" -- naming its
  # own internal argument rather than the `width_svg` the caller passed.
  validate_scalar(width_svg, "width_svg", min = 0, min_open = TRUE)
  validate_scalar(height_svg, "height_svg", min = 0, min_open = TRUE)
  ggiraph::girafe(ggobj = p, width_svg = width_svg,
                  height_svg = height_svg)
}
