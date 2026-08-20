#' Changepoint accuracy metrics
#'
#' Computes standard accuracy metrics comparing predicted changepoints to
#' ground truth, including precision/recall/F1 with margin, covering metric,
#' Hausdorff distance, adjusted Rand index, annotation error, and MAE/RMSE
#' of matched locations.
#'
#' @param pred Predicted changepoint indices (integer vector).
#' @param truth Ground truth changepoint indices (integer vector).
#' @param n Length of the series.
#' @param margin Tolerance margin for matching (default 5).
#'
#' @details Precision/recall use a one-to-one matching: each truth may be
#' claimed by at most one prediction (predictions are scanned in order and
#' take the earliest unmatched truth within \code{margin}, which yields a
#' maximum matching for interval-structured problems). When \code{pred} and
#' \code{truth} are both empty the segmentation is exactly right, so
#' precision, recall, and F1 are all 1. The covering metric follows
#' van den Burg and Williams (2020): the prediction-side partition is always
#' well defined, so an empty \code{pred} scores the covering of the trivial
#' single-segment partition rather than 0.
#'
#' @return A tibble with columns: \code{n}, \code{n_pred}, \code{n_truth},
#'   \code{precision}, \code{recall}, \code{f1}, \code{covering},
#'   \code{hausdorff}, \code{rand_index}, \code{annotation_error},
#'   \code{mae_matched}, \code{rmse_matched}.
#' @export
#'
#' @examples
#' cpt_metrics(c(100, 200), c(100, 200), n = 300)
#' cpt_metrics(c(101, 205), c(100, 200), n = 300, margin = 5)
cpt_metrics <- function(pred, truth, n, margin = 5) {

  validate_scalar(n, "n", min = 1)
  validate_scalar(margin, "margin", min = 0)
  pred <- sort(unique(as.integer(pred)))
  truth <- sort(unique(as.integer(truth)))
  n <- as.integer(n)

  # Changepoints follow the "left" convention, so valid locations are
  # 1..(n-1); out-of-range entries would corrupt the partition metrics.
  bad_pred <- pred[pred < 1 | pred >= n]
  bad_truth <- truth[truth < 1 | truth >= n]
  if (length(bad_pred) > 0 || length(bad_truth) > 0) {
    warning("Dropping changepoint indices outside 1..(n-1): ",
            paste(unique(c(bad_pred, bad_truth)), collapse = ", "),
            call. = FALSE)
    pred <- pred[pred >= 1 & pred < n]
    truth <- truth[truth >= 1 & truth < n]
  }

  m <- match_changepoints(pred, truth, margin)
  tp <- nrow(m)

  both_empty <- length(pred) == 0 && length(truth) == 0
  precision <- if (both_empty) 1 else if (length(pred) == 0) 0 else tp / length(pred)
  recall    <- if (both_empty) 1 else if (length(truth) == 0) 0 else tp / length(truth)
  f1        <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)

  covering <- calc_covering(pred, truth, n)
  hausdorff <- calc_hausdorff(pred, truth)
  rand_index <- calc_adjusted_rand(pred, truth, n)
  annotation_error <- abs(length(pred) - length(truth))

  if (tp > 0) {
    errors <- abs(m$pred - m$truth)
    mae_matched <- mean(errors)
    rmse_matched <- sqrt(mean(errors^2))
  } else {
    mae_matched <- NA_real_
    rmse_matched <- NA_real_
  }

  tibble::tibble(
    n = n,
    n_pred = length(pred),
    n_truth = length(truth),
    precision = precision,
    recall = recall,
    f1 = f1,
    covering = covering,
    hausdorff = hausdorff,
    rand_index = rand_index,
    annotation_error = annotation_error,
    mae_matched = mae_matched,
    rmse_matched = rmse_matched
  )
}

#' Multi-annotator evaluation
#'
#' Computes averaged covering and F1 scores against multiple annotation sets,
#' as used in the Turing Change Point Dataset benchmark.
#'
#' @param pred Predicted changepoint indices.
#' @param annotations A list of ground-truth annotation vectors.
#' @param n Length of the series.
#' @param margin Tolerance margin (default 5).
#'
#' @return A tibble with averaged metrics.
#' @export
cpt_metrics_annotated <- function(pred, annotations, n, margin = 5) {

  if (!is.list(annotations)) {
    annotations <- list(annotations)
  }

  results <- lapply(annotations, function(truth) {
    cpt_metrics(pred, truth, n, margin)
  })

  avg <- do.call(rbind, results)

  tibble::tibble(
    n = n,
    n_annotators = length(annotations),
    n_pred = avg$n_pred[1],
    precision = mean(avg$precision, na.rm = TRUE),
    recall = mean(avg$recall, na.rm = TRUE),
    f1 = mean(avg$f1, na.rm = TRUE),
    covering = mean(avg$covering, na.rm = TRUE)
  )
}

#' Evaluation visualization
#'
#' Overlays predictions and ground truth on the series with tolerance windows,
#' colouring true positives, false positives, and misses. Uses the same
#' one-to-one matching as \code{\link{cpt_metrics}()}, so the plot and the
#' metrics agree.
#'
#' @param pred Predicted changepoint indices.
#' @param truth Ground truth changepoint indices.
#' @param data_vec The original data vector (for context).
#' @param margin Tolerance margin (default 5).
#'
#' @return A ggplot object.
#' @export
ggcpt_eval <- function(pred, truth, data_vec, margin = 5) {

  pred <- sort(unique(as.integer(pred)))
  truth <- sort(unique(as.integer(truth)))

  data_tbl <- tibble::tibble(
    index = seq_along(data_vec),
    value = as.numeric(data_vec)
  )

  # One-to-one matching, shared with cpt_metrics()
  m <- match_changepoints(pred, truth, margin)
  pred_class <- ifelse(pred %in% m$pred, "TP", "FP")
  truth_class <- ifelse(truth %in% m$truth, "TP", "FN")

  pred_df <- tibble::tibble(x = pred, type = pred_class)
  fn_df <- tibble::tibble(x = truth[truth_class == "FN"],
                          type = rep("FN", sum(truth_class == "FN")))

  yrange <- diff(range(data_vec, na.rm = TRUE))
  if (yrange == 0) yrange <- 1
  ymin <- min(data_vec, na.rm = TRUE) - 0.1 * yrange
  ymax <- max(data_vec, na.rm = TRUE) + 0.1 * yrange

  p <- ggplot2::ggplot(data_tbl, ggplot2::aes(index, value)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Index", y = "Value", title = "Changepoint Evaluation")

  # Add tolerance windows around truth
  for (t in truth) {
    p <- p + ggplot2::annotate("rect",
      xmin = t - margin, xmax = t + margin,
      ymin = ymin, ymax = ymax,
      fill = "green", alpha = 0.1
    )
  }

  # Vertical lines for predictions (TP/FP) and misses (FN), all mapped to
  # `type` so the legend shows every class that appears.
  lines_df <- rbind(pred_df, fn_df)
  if (nrow(lines_df) > 0) {
    lines_df <- dplyr::mutate(lines_df, .ymin = ymin, .ymax = ymax)
    p <- p + ggplot2::geom_linerange(
      data = lines_df,
      ggplot2::aes(x = x, ymin = .ymin, ymax = .ymax, color = type,
                   linetype = type),
      inherit.aes = FALSE, linewidth = 1
    )
  }

  # The manual scales only make sense once something is mapped to `type`;
  # adding them to a plot with no prediction and no miss makes ggplot warn
  # ("No shared levels found ...") on what is a perfectly good evaluation.
  if (nrow(lines_df) == 0) return(p)

  p +
    ggplot2::scale_color_manual(
      values = c(TP = "blue", FP = "orange", FN = "red"),
      labels = c(TP = "True Positive", FP = "False Positive", FN = "Miss"),
      name = "type"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(TP = "solid", FP = "solid", FN = "dashed"),
      labels = c(TP = "True Positive", FP = "False Positive", FN = "Miss"),
      name = "type"
    )
}


# Internal helpers --------------------------------------------------------

# One-to-one matching of predictions to truths within a margin. Predictions
# are scanned in increasing order and take the earliest unmatched truth
# within [p - margin, p + margin]; for points on a line this greedy rule
# yields a maximum matching.
#' @noRd
match_changepoints <- function(pred, truth, margin) {
  matched_pred <- integer(0)
  matched_truth <- integer(0)
  available <- rep(TRUE, length(truth))

  for (p in sort(pred)) {
    cand <- which(available & abs(p - truth) <= margin)
    if (length(cand) > 0) {
      j <- cand[1]
      matched_pred <- c(matched_pred, p)
      matched_truth <- c(matched_truth, truth[j])
      available[j] <- FALSE
    }
  }

  data.frame(pred = matched_pred, truth = matched_truth)
}

calc_covering <- function(pred, truth, n) {
  # Both partitions are always well defined: an empty changepoint set is the
  # trivial single-segment partition, not a zero score.
  truth_breaks <- sort(unique(c(0, truth, n)))
  pred_breaks <- sort(unique(c(0, pred, n)))

  b_start <- pred_breaks[-length(pred_breaks)] + 1
  b_end <- pred_breaks[-1]

  covering <- 0
  for (i in seq_len(length(truth_breaks) - 1)) {
    a_start <- truth_breaks[i] + 1
    a_end <- truth_breaks[i + 1]
    a_len <- a_end - a_start + 1

    # Only prediction segments that actually overlap this truth segment can
    # win: a disjoint pair has intersection 0, so its Jaccard is 0, and both
    # partitions tile 1..n so at least one overlapping segment always exists.
    # Both break vectors are sorted, so the overlapping range is two
    # findInterval() lookups. Scanning every prediction segment for every
    # truth segment instead made the metric quadratic -- 7.5 s for 3000
    # changepoints, against a few hundredths here -- for identical numbers.
    j_lo <- findInterval(a_start - 1, b_end) + 1L
    j_hi <- findInterval(a_end, b_start)
    if (j_lo > j_hi) next

    j <- j_lo:j_hi
    inter <- pmax(0, pmin(a_end, b_end[j]) - pmax(a_start, b_start[j]) + 1)
    union <- pmax(a_end, b_end[j]) - pmin(a_start, b_start[j]) + 1
    covering <- covering + a_len * max(inter / union)
  }

  covering / n
}

calc_hausdorff <- function(pred, truth) {
  if (length(pred) == 0 || length(truth) == 0) return(NA_real_)

  d1 <- max(vapply(pred, function(p) min(abs(p - truth)), numeric(1)))
  d2 <- max(vapply(truth, function(t) min(abs(t - pred)), numeric(1)))
  max(d1, d2)
}

calc_adjusted_rand <- function(pred, truth, n) {
  # Build segment labelling vectors
  pred_labels <- label_segments(pred, n)
  truth_labels <- label_segments(truth, n)

  # Contingency table
  tbl <- table(pred_labels, truth_labels)
  n_points <- sum(tbl)

  if (n_points <= 1) return(1)

  # Sum of combinations
  sum_comb <- function(x) sum(x * (x - 1) / 2)

  a <- sum_comb(rowSums(tbl))
  b <- sum_comb(colSums(tbl))
  index <- sum_comb(tbl)

  expected <- a * b / sum_comb(n_points)
  max_index <- (a + b) / 2

  # Identical partitions (including two trivial ones) have a degenerate
  # denominator; they agree perfectly. Any other case uses the ARI formula
  # directly — index == expected is chance-level agreement (ARI 0), not 1.
  if (abs(max_index - expected) < 1e-15) return(1)

  (index - expected) / (max_index - expected)
}

label_segments <- function(cp, n) {
  cp <- cp[cp >= 1 & cp < n]
  breaks <- sort(unique(c(0, cp, n)))
  labels <- rep(seq_len(length(breaks) - 1), diff(breaks))
  labels
}
