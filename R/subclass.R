# ---------------------------------------------------------------------------
# Keeping tibble subclasses honest under `[`.
#
# Several 0.5.0 results are tibbles carrying an extra class and a `print()`
# method that reads specific columns: ggcpt_benchmark, ggcpt_batch,
# ggcpt_recommendation, ggcpt_label_curve, cpt_labels, cpt_label_error.
# Base `[` keeps the class, so `bm[, c("dataset", "method")]` came back
# still claiming to be a benchmark, and printing it then reached for a
# `error` column that was no longer there -- "Unknown or uninitialised
# column: `error`" on what looked like an ordinary column selection.
#
# The tidyverse answer is that a subclass survives only while its invariants
# do. These methods drop back to a plain tibble the moment a required column
# is selected away, which is also what makes such an object safe to hand to
# dplyr, ggplot2 or anything else that subsets it.
# ---------------------------------------------------------------------------

# Internal: drop this package's subclass from a tibble while keeping it a
# tibble. `tidy()` on an object that already *is* one row per thing should
# hand back a plain tibble, so downstream code is not surprised by a print
# method that reads columns it may have dropped.
#' @noRd
unclass_keep_tbl <- function(x) {
  class(x) <- intersect(c("tbl_df", "tbl", "data.frame"), class(x))
  attrs <- setdiff(names(attributes(x)),
                   c("names", "row.names", "class"))
  for (a in attrs) attr(x, a) <- NULL
  x
}

# Internal: decide what class the result of a subset should carry. Keeps the
# subclass only when every required column survived and the result is still
# a data frame; otherwise falls back to the plain tibble classes.
#
# `NextMethod()` cannot live in here -- it reads the dispatch context of the
# frame it is called from, so each method has to call it and hand the result
# over.
#' @noRd
reclass_subset <- function(x, out, required) {
  if (!is.data.frame(out)) return(out)
  if (all(required %in% names(out))) {
    class(out) <- class(x)
  } else {
    class(out) <- intersect(c("tbl_df", "tbl", "data.frame"), class(x))
  }
  out
}

#' @export
`[.ggcpt_benchmark` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("dataset", "method", attr(x, "metrics")))
}

#' @export
`[.ggcpt_batch` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("series", "n_changepoints", "changepoints",
                           "result"))
}

#' @export
`[.ggcpt_recommendation` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("method", "engine", "installed", "score", "why",
                           "caveat"))
}

#' @export
`[.ggcpt_label_curve` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("penalty", "n_cp", "errors", "false_positive",
                           "false_negative"))
}

#' @export
`[.cpt_labels` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("label_id", "series", "start", "end", "change"))
}

#' @export
`[.cpt_label_error` <- function(x, ...) {
  out <- NextMethod()
  reclass_subset(x, out, c("label_id", "start", "end", "change",
                           "n_changes", "status"))
}
