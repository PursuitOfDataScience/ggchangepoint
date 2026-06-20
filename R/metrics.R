#' Changepoint accuracy metrics
#'
#' Computes standard accuracy metrics comparing predicted changepoints to
#' ground truth, including precision/recall/F1 with margin, covering metric,
#' Hausdorff distance, (adjusted) Rand index, annotation error, and MAE/RMSE
#' of matched locations.
#'
#' @param pred Predicted changepoint indices (integer vector).
#' @param truth Ground truth changepoint indices (integer vector).
#' @param n Length of the series.
#' @param margin Tolerance margin for matching (default 5).
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

  pred <- sort(unique(as.integer(pred)))
  truth <- sort(unique(as.integer(truth)))
  n <- as.integer(n)

  # Precision / Recall / F1 with margin
  tp <- 0
  matched_pred <- c()
  matched_truth <- c()

  for (p in pred) {
    dists <- abs(p - truth)
    min_dist <- min(dists)
    if (min_dist <= margin) {
      tp <- tp + 1
      matched_pred <- c(matched_pred, p)
      matched_truth <- c(matched_truth, truth[which.min(dists)])
    }
  }

  fp <- length(pred) - tp
  fn <- length(truth) - tp

  precision <- if (length(pred) == 0) 0 else tp / length(pred)
  recall    <- if (length(truth) == 0) 0 else tp / length(truth)
  f1        <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)

  # Covering metric
  covering <- calc_covering(pred, truth, n)

  # Hausdorff distance
  hausdorff <- calc_hausdorff(pred, truth)

  # (Adjusted) Rand index between segment labellings
  rand_index <- calc_adjusted_rand(pred, truth, n)

  # Annotation error
  annotation_error <- abs(length(pred) - length(truth))

  # MAE / RMSE of matched locations
  if (length(matched_pred) > 0) {
    errors <- abs(matched_pred - matched_truth)
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
#' colouring true positives, false positives, and misses.
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

  # Classify predictions
  pred_class <- vapply(pred, function(p) {
    dists <- abs(p - truth)
    if (min(dists) <= margin) "TP" else "FP"
  }, character(1))

  # Classify truths
  truth_class <- vapply(truth, function(t) {
    dists <- abs(t - pred)
    if (min(dists) <= margin) "TP" else "FN"
  }, character(1))

  pred_df <- tibble::tibble(x = pred, type = pred_class)
  truth_df <- tibble::tibble(x = truth, type = truth_class)

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

  # Add vertical lines for predictions (TP/FP)
  if (nrow(pred_df) > 0) {
    pred_df <- dplyr::mutate(pred_df, .ymin = ymin, .ymax = ymax)
    p <- p + ggplot2::geom_linerange(
      data = pred_df,
      ggplot2::aes(x = x, ymin = .ymin, ymax = .ymax, color = type),
      inherit.aes = FALSE, linewidth = 1
    )
  }

  # Add dashed vertical lines for misses (FN)
  fn_df <- dplyr::filter(truth_df, type == "FN")
  if (nrow(fn_df) > 0) {
    fn_df <- dplyr::mutate(fn_df, .ymin = ymin, .ymax = ymax)
    p <- p + ggplot2::geom_linerange(
      data = fn_df,
      ggplot2::aes(x = x, ymin = .ymin, ymax = .ymax),
      inherit.aes = FALSE, color = "red", linewidth = 1, linetype = "dashed"
    )
  }

  p + ggplot2::scale_color_manual(
    values = c(TP = "blue", FP = "orange", FN = "red"),
    labels = c(TP = "True Positive", FP = "False Positive", FN = "Miss")
  )
}


# Internal helpers --------------------------------------------------------

calc_covering <- function(pred, truth, n) {
  if (length(truth) == 0) {
    return(if (length(pred) == 0) 1 else 0)
  }
  if (length(pred) == 0) return(0)

  truth_breaks <- sort(unique(c(0, truth, n)))
  pred_breaks <- sort(unique(c(0, pred, n)))

  covering <- 0
  for (i in seq_len(length(truth_breaks) - 1)) {
    a_start <- truth_breaks[i] + 1
    a_end <- truth_breaks[i + 1]
    a_len <- a_end - a_start + 1

    best_j <- 0
    for (j in seq_len(length(pred_breaks) - 1)) {
      b_start <- pred_breaks[j] + 1
      b_end <- pred_breaks[j + 1]

      inter <- max(0, min(a_end, b_end) - max(a_start, b_start) + 1)
      union <- max(a_end, b_end) - min(a_start, b_start) + 1
      jaccard <- if (union == 0) 0 else inter / union
      if (jaccard > best_j) best_j <- jaccard
    }
    covering <- covering + a_len * best_j
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

  if (abs(index - expected) < 1e-15) return(1)

  (index - expected) / (max_index - expected)
}

label_segments <- function(cp, n) {
  breaks <- sort(unique(c(0, cp, n)))
  labels <- rep(seq_len(length(breaks) - 1), diff(breaks))
  labels
}
