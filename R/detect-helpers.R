# ---------------------------------------------------------------------------
# The pieces cpt_detect() is assembled from in 0.6.0: the formula and
# grouped-data front doors, the missing-value policy, the constraints
# (`fixed`, `within`, `min_segment`, `min_effect`), and the checks that run
# on the data before a fit and on the answer after it.
# ---------------------------------------------------------------------------

# ---- formula interface ----------------------------------------------------

# Internal: turn `y ~ x1 + x2` plus `data` into the response, the model
# matrix and (optionally) an index column. `y ~ 1` is intercept-only: the
# response alone, which every univariate method can take as a series.
#' @noRd
formula_series <- function(formula, data, index_expr, caller) {
  if (!is.null(data) && !is.data.frame(data)) {
    cpt_abort("`data` must be a data frame (got ", class(data)[1], ").",
              class = "bad_argument")
  }
  if (length(formula) != 3L) {
    cpt_abort("A formula needs a response: `y ~ x` (or `y ~ 1` for the ",
              "response alone), not a one-sided `~ x`.",
              class = "bad_argument")
  }
  mf <- tryCatch(
    stats::model.frame(formula, data = data, na.action = stats::na.pass),
    error = function(e) {
      cpt_abort("Could not evaluate the formula: ", conditionMessage(e),
                class = "bad_argument", parent = e)
    })
  y <- stats::model.response(mf)
  if (is.null(y)) {
    cpt_abort("The formula has no response.", class = "bad_argument")
  }
  resp_name <- paste(deparse(formula[[2]]), collapse = "")
  if (is.matrix(y) || is.data.frame(y)) {
    cpt_abort("The response `", resp_name, "` has several columns; a ",
              "formula takes one response.", class = "wrong_dimension")
  }
  refuse_special_values(y, resp_name)
  y <- coerce_series_values(y, arg = resp_name)
  tt <- stats::terms(mf)
  labels <- attr(tt, "term.labels")
  X <- stats::model.matrix(tt, mf)
  idx <- NULL
  if (!is.null(index_expr)) {
    if (is.null(data)) {
      cpt_abort("`index` selects a column of `data`, and no `data` was ",
                "given.", class = "bad_argument")
    }
    idx <- df_column(as.data.frame(data), index_expr, "index", caller)
  }
  # The variables the formula names, untransformed, for an engine that
  # refits the formula itself (segmented): the model frame holds `log(x)`
  # under that name, and a formula cannot be re-evaluated on it.
  vars_data <- if (!is.null(data)) {
    data
  } else {
    vars <- all.vars(formula)
    as.data.frame(mget(vars, envir = environment(formula), inherits = TRUE))
  }
  list(formula = formula, y = as.numeric(y), X = X, terms = tt,
       labels = labels, intercept_only = length(labels) == 0L,
       has_intercept = attr(tt, "intercept") == 1L, response = resp_name,
       index = idx, vars_data = vars_data,
       index_label = if (!is.null(index_expr)) deparse(index_expr) else
         "Index")
}

# ---- grouped and long data frames -----------------------------------------

# Internal: the grouping variables that make `x` several series, or NULL.
# A dplyr-grouped frame carries them, a keyed tsibble carries them as its
# key, and a long frame names them through `group =`.
#' @noRd
detect_groups <- function(x, group_expr, caller) {
  explicit <- NULL
  if (!is.null(group_expr)) {
    if (!is.data.frame(x)) {
      cpt_abort("`group` selects a column of a data frame, and `x` is a ",
                class(x)[1], ".", class = "bad_argument")
    }
    explicit <- group_names(x, group_expr, caller)
  }
  if (inherits(x, "tbl_ts") && requireNamespace("tsibble", quietly = TRUE)) {
    keys <- tsibble::key_vars(x)
    if (length(keys)) return(unique(c(explicit, keys)))
  }
  if (inherits(x, "grouped_df")) {
    gv <- dplyr::group_vars(x)
    if (length(gv)) return(unique(c(explicit, gv)))
  }
  explicit
}

# Internal: resolve `group =` (a bare name, a string, a character vector or
# `c(a, b)`) to column names of `df`.
#' @noRd
group_names <- function(df, expr, caller) {
  # A bare name that is not a column may be a variable holding the column
  # names; one that is neither is a typo, reported against the columns.
  lookup <- function(sym) {
    nm <- as.character(sym)
    if (nm %in% names(df)) return(nm)
    val <- tryCatch(eval(sym, caller), error = function(e) NULL)
    if (is.character(val)) val else nm
  }
  nms <- if (is.symbol(expr)) {
    lookup(expr)
  } else if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
    unlist(lapply(as.list(expr)[-1], function(e) {
      if (is.symbol(e)) lookup(e) else as.character(eval(e, caller))
    }))
  } else {
    eval(expr, caller)
  }
  if (!is.character(nms) || !length(nms)) {
    cpt_abort("`group` must name one or more columns of the data frame.",
              class = "bad_argument")
  }
  missing <- setdiff(nms, names(df))
  if (length(missing)) {
    cpt_abort("`group`: no column called ", paste0("\"", missing, "\"",
                                                   collapse = ", "),
              " in the data frame (columns: ", paste(names(df),
                                                     collapse = ", "), ").",
              class = "bad_argument")
  }
  nms
}

# Internal: split a grouped or long frame into one series per group and run
# them through cpt_batch(). Rows are put in index order within each group,
# since long data from a database seldom arrives sorted and the index is
# what says which order the observations belong in.
#' @noRd
detect_grouped <- function(x, groups, y_expr, index_expr, caller, args) {
  df <- as.data.frame(x)
  tsib <- inherits(x, "tbl_ts") && requireNamespace("tsibble", quietly = TRUE)
  index_col <- if (!is.null(index_expr)) {
    df_column(df, index_expr, "index", caller)
  } else if (tsib) {
    df[[tsibble::index_var(x)]]
  } else {
    NULL
  }
  index_name <- if (!is.null(index_expr)) deparse(index_expr) else if (tsib)
    tsibble::index_var(x) else NULL
  yv <- if (!is.null(y_expr)) {
    df_column(df, y_expr, "y", caller)
  } else {
    skip <- c(groups, index_name)
    num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], skip)
    if (length(num) != 1L) {
      cpt_abort("`x` is grouped by ", paste(groups, collapse = ", "),
                ", and ", if (length(num)) {
                  paste0("has ", length(num), " numeric columns (",
                         paste(num, collapse = ", "), ")")
                } else {
                  "has no numeric column"
                }, " to detect on. Name one with `y =`.",
                class = "bad_argument")
    }
    df[[num]]
  }
  if (!is.numeric(yv)) {
    cpt_abort("`y` must select a numeric column; got ", class(yv)[1], ".",
              class = "bad_type")
  }
  keys <- df[, groups, drop = FALSE]
  key_str <- do.call(paste, c(lapply(keys, as.character), sep = "/"))
  ukeys <- unique(keys)
  ukeys <- ukeys[do.call(order, unname(as.list(ukeys))), , drop = FALSE]
  ustr <- do.call(paste, c(lapply(ukeys, as.character), sep = "/"))
  series <- list()
  idx_list <- list()
  for (k in seq_along(ustr)) {
    rows <- which(key_str == ustr[k])
    if (!is.null(index_col)) rows <- rows[order(index_col[rows])]
    series[[ustr[k]]] <- yv[rows]
    if (!is.null(index_col)) idx_list[[ustr[k]]] <- index_col[rows]
  }
  batch_args <- c(list(series), args)
  batch_args$index <- if (length(idx_list)) idx_list else NULL
  out <- do.call(cpt_batch, batch_args)
  attach_group_keys(out, ukeys, groups)
}

# Internal: put the grouping columns on a batch result, before `series`, so
# the result joins back to the data it came from.
#' @noRd
attach_group_keys <- function(out, keys, groups) {
  cls <- class(out)
  method <- attr(out, "method")
  change_in <- attr(out, "change_in")
  keys <- tibble::as_tibble(keys)
  if (!any(groups %in% names(out))) {
    out <- dplyr::bind_cols(keys, tibble::as_tibble(out))
    class(out) <- cls
  }
  attr(out, "method") <- method
  attr(out, "change_in") <- change_in
  attr(out, "group_vars") <- groups
  out
}

# ---- missing values -------------------------------------------------------

# Internal: drop missing observations (whole rows for a matrix), keeping
# what is needed to put the answer back in the original positions. Returns
# NULL when nothing is missing.
#' @noRd
omit_missing <- function(x) {
  is_mv <- is.matrix(x) || is.data.frame(x)
  keep <- if (is_mv) stats::complete.cases(as.matrix(x)) else !is.na(x)
  if (all(keep)) return(NULL)
  if (sum(keep) < 3L) {
    cpt_abort("`x` has ", sum(keep), " complete observation(s) once the ",
              "missing values are dropped; at least 3 are needed.",
              class = "short_series", data = list(n = sum(keep)))
  }
  list(x = if (is_mv) x[keep, , drop = FALSE] else x[keep], keep = keep,
       pos = which(keep), n = length(keep), full = x)
}

# Internal: translate positions in the original series to positions in the
# series with its missing values removed. A changepoint "after original
# position p" is after the last observed value at or before p.
#' @noRd
to_compact_positions <- function(p, keep) {
  vapply(p, function(v) sum(keep[seq_len(v)]), numeric(1))
}

# Internal: expand a result fitted on the compacted series back to the
# original positions. Changepoints map to the last observed position of
# their left segment, per-observation columns gain NA at the gaps, and the
# translation itself goes in `$diagnostics$na_omitted`.
#' @noRd
restore_missing <- function(res, info) {
  pos <- info$pos
  n <- info$n
  map <- function(v) {
    out <- rep(NA_integer_, length(v))
    ok <- !is.na(v) & v >= 1 & v <= length(pos)
    out[ok] <- pos[v[ok]]
    out
  }
  expand <- function(v) {
    out <- rep(if (is.numeric(v)) NA_real_ else NA, n)
    out[pos] <- v
    out
  }
  full <- info$full
  is_mv <- is.matrix(full) || is.data.frame(full)
  value <- if (is_mv) as.numeric(as.matrix(full)[, 1]) else as.numeric(full)
  cp <- res$changepoints
  if (nrow(cp)) {
    cp$cp <- map(cp$cp)
    for (col in intersect(c("ci_lower", "ci_upper", "region_start",
                            "region_end"), names(cp))) {
      cp[[col]] <- map(cp[[col]])
    }
    cp$cp_value <- value[cp$cp]
  }
  res$changepoints <- cp
  old_data <- res$data
  data_tbl <- tibble::tibble(index = seq_len(n), value = value)
  for (col in setdiff(names(old_data), c("index", "value"))) {
    data_tbl[[col]] <- expand(old_data[[col]])
  }
  res$data <- data_tbl
  if (!is.null(res$data_wide)) {
    wide <- res$data_wide
    new_wide <- tibble::tibble(index = seq_len(n))
    for (col in setdiff(names(wide), "index")) {
      new_wide[[col]] <- expand(wide[[col]])
    }
    res$data_wide <- new_wide
  }
  if (!is.null(res$regions) && nrow(res$regions)) {
    res$regions$start <- map(res$regions$start)
    res$regions$end <- map(res$regions$end)
  }
  res$segments <- rebuild_segments(value, res$changepoints$cp,
                                   res$segments)
  res$diagnostics <- res$diagnostics %||% list()
  if (!is.null(res$diagnostics$statistic)) {
    st <- res$diagnostics$statistic
    if (is.list(st) && !is.null(st$statistic) &&
        length(st$statistic) == length(pos)) {
      st$statistic <- expand(st$statistic)
    } else if (is.numeric(st) && length(st) == length(pos)) {
      st <- expand(st)
    }
    res$diagnostics$statistic <- st
  }
  if (!is.null(res$diagnostics$solution_path) &&
      "cp" %in% names(res$diagnostics$solution_path)) {
    sp <- res$diagnostics$solution_path
    sp$cp <- map(sp$cp)
    for (col in intersect(c("start", "end"), names(sp))) sp[[col]] <- map(sp[[col]])
    res$diagnostics$solution_path <- sp
  }
  res$diagnostics$na_omitted <- list(positions = which(!info$keep),
                                     n_observed = length(pos),
                                     observed_positions = pos)
  res$na_map <- pos
  res
}

# Internal: segments recomputed on the original positions, keeping any
# engine columns the old table carried when the count still matches.
#' @noRd
rebuild_segments <- function(value, cps, old = NULL) {
  seg <- build_segments(value, sort(cps))
  if (!is.null(old) && nrow(old) == nrow(seg)) {
    for (col in setdiff(names(old), names(seg))) seg[[col]] <- old[[col]]
  }
  seg
}

# Internal: `na_action = "engine"` passes the gaps to engines that handle
# them in place. Everything else is refused, naming the ones that do.
#' @noRd
check_engine_na <- function(method, registered) {
  if (!is.null(registered)) return(invisible(TRUE))
  reg <- builtin_registry()
  how <- reg$na_handling[match(method, reg$method)]
  if (identical(how, "native")) return(invisible(TRUE))
  natives <- reg$method[reg$na_handling == "native"]
  cpt_abort("`na_action = \"engine\"` passes missing values to the engine, ",
            "and `", method, "` does not handle them in place (measured: ",
            switch(how,
              compacts = paste("it drops them and reports positions in the",
                               "shortened series, so every location after a",
                               "gap would be wrong"),
              silent_loss = paste("it returns a well-formed answer that",
                                  "loses the changepoints"),
              "it refuses them"),
            "). Use `na_action = \"omit\"`, which drops them and maps the ",
            "answer back, or one of the methods that handles them: ",
            paste(natives, collapse = ", "), ".", class = "unsupported",
            data = list(method = method, requested = "na_action = engine",
                        supported = natives))
}

# ---- constraints ----------------------------------------------------------

# Internal: read locations given by the caller (`fixed`, a `within` bound,
# `when`, an event) as positions of the series. One rule for all of them,
# the one cpt_annotate_events() has used since 0.5.0:
#   * a value of the index's own class (a Date, a timestamp, a label) is an
#     index value;
#   * a number, when the index is not numeric, is a position;
#   * a number, when the index is numeric too (a `ts`, a column of years),
#     is an index value if it lies outside 1..n, where it cannot be a
#     position, and a position otherwise, with a warning when the index is
#     not 1..n itself, because the reading is then a guess.
# `side` says which observation an index value between two observations
# maps to: "before" (the last at or before it: a changepoint, the last
# observation of the old regime) or "after" (the first at or after it:
# `when`, the first observation of the new one).
#' @noRd
locate_on_series <- function(v, idx, n, arg, side = c("before", "after")) {
  side <- match.arg(side)
  same_class <- !is.null(idx) && !is.numeric(v) &&
    (inherits(v, class(idx)[1]) ||
       (inherits(v, "POSIXt") && inherits(idx, "POSIXt")))
  if (!same_class && !is.numeric(v)) {
    cpt_abort("`", arg, "` must hold positions (numbers) or values of the ",
              "series' index (",
              if (is.null(idx)) "the series has none" else class(idx)[1],
              "); got ", class(v)[1], ".", class = "bad_argument")
  }
  on_index <- same_class
  if (!on_index && !is.null(idx) && is.numeric(idx)) {
    vv <- as.numeric(v)
    if (any(vv < 1 | vv > n, na.rm = TRUE)) {
      on_index <- TRUE
    } else if (!identical(as.numeric(idx), as.numeric(seq_len(n)))) {
      cpt_warn("`", arg, "` is numeric and so is the series' index, and its ",
               "values fall inside 1..", n, ", so they are read as ",
               "positions. Give index values outside that range, or look ",
               "them up first with match(value, index).",
               class = "warning")
    }
  }
  if (!on_index) return(as.numeric(v))
  if (!is_numeric_like_index(idx)) {
    return(as.numeric(match(as.character(v), as.character(idx))))
  }
  num_idx <- as.numeric(idx)
  vapply(as.numeric(v), function(val) {
    hit <- if (side == "before") which(num_idx <= val) else
      which(num_idx >= val)
    if (!length(hit)) NA_real_ else if (side == "before") max(hit) else
      min(hit)
  }, numeric(1))
}

# Internal: read `fixed` or a `within` bound as positions (see
# locate_on_series()). Changepoints follow the package convention, the
# last observation of the left segment.
#' @noRd
constraint_positions <- function(v, idx, n, arg) {
  if (is.null(v)) return(NULL)
  p <- locate_on_series(v, idx, n, arg, side = "before")
  if (anyNA(p)) {
    cpt_abort("`", arg, "` has a value before the start of the series",
              if (!is.null(idx)) paste0(" (", format_index_range(idx), ")"),
              ".", class = "bad_argument")
  }
  p <- as.integer(round(p))
  bad <- p < 1L | p >= n
  if (any(bad)) {
    cpt_abort("`", arg, "` must lie within 1..", n - 1L, " (a changepoint ",
              "is the last observation of a segment, and the last ",
              "observation of the series cannot be one); got ",
              paste(p[bad], collapse = ", "), ".", class = "bad_argument")
  }
  p
}

# Internal: normalise `within` to a two-column integer matrix of [lo, hi]
# position windows.
#' @noRd
within_windows <- function(within, idx, n) {
  if (is.null(within)) return(NULL)
  pairs <- if (is.data.frame(within) || is.matrix(within)) {
    if (ncol(within) != 2L) {
      cpt_abort("`within` as a table needs exactly two columns (start, ",
                "end).", class = "bad_argument")
    }
    lapply(seq_len(nrow(within)), function(i) {
      if (is.data.frame(within)) {
        c(within[[1]][i], within[[2]][i])
      } else {
        within[i, ]
      }
    })
  } else if (is.list(within) && !inherits(within, c("Date", "POSIXt"))) {
    within
  } else {
    list(within)
  }
  out <- do.call(rbind, lapply(pairs, function(w) {
    if (length(w) != 2L) {
      cpt_abort("Each `within` window is a start and an end: two values, ",
                "not ", length(w), ".", class = "bad_argument")
    }
    p <- constraint_positions(w, idx, n, "within")
    sort(p)
  }))
  colnames(out) <- c("lo", "hi")
  out
}

# Internal: run the detector on each stretch between fixed changepoints and
# put the answers together. The fixed locations are always in the result;
# the search happens only inside the stretches, so each engine sees data
# with no change it has been told about, and its penalty is on the length
# of the stretch it is given.
#' @noRd
detect_fixed <- function(x, fixed, run_piece) {
  is_mv <- is.matrix(x) || is.data.frame(x)
  n <- if (is_mv) nrow(x) else length(x)
  bounds <- c(0L, sort(unique(fixed)), n)
  pieces <- list()
  rows_all <- list()
  for (i in seq_len(length(bounds) - 1L)) {
    rows <- (bounds[i] + 1L):bounds[i + 1L]
    xi <- if (is_mv) x[rows, , drop = FALSE] else x[rows]
    fit_i <- if (length(rows) < 3L) NULL else tryCatch(
      run_piece(xi),
      ggchangepoint_short_series = function(e) NULL)
    pieces[[i]] <- fit_i
    if (!is.null(fit_i) && nrow(fit_i$changepoints)) {
      cp_i <- fit_i$changepoints
      off <- bounds[i]
      for (col in intersect(c("cp", "ci_lower", "ci_upper", "region_start",
                              "region_end"), names(cp_i))) {
        cp_i[[col]] <- cp_i[[col]] + off
      }
      cp_i$cp_index <- NULL
      cp_i$fixed <- FALSE
      rows_all[[length(rows_all) + 1L]] <- cp_i
    }
  }
  found <- if (length(rows_all)) dplyr::bind_rows(rows_all) else NULL
  fixed_rows <- tibble::tibble(cp = as.integer(sort(unique(fixed))),
                               fixed = TRUE)
  cps <- dplyr::bind_rows(found, fixed_rows)
  cps <- cps[!duplicated(cps$cp) | cps$fixed, , drop = FALSE]
  cps <- cps[order(cps$cp, -cps$fixed), , drop = FALSE]
  cps <- cps[!duplicated(cps$cp), , drop = FALSE]
  list(changepoints = cps, pieces = pieces)
}

# Internal: keep only the changepoints inside a `within` window (and every
# fixed one). The engine's search ran over the whole series; this is a
# restriction of what is reported, not a re-optimisation, which is why the
# ones it removed are recorded.
#' @noRd
apply_within <- function(res, windows) {
  cp <- res$changepoints
  if (!nrow(cp)) return(res)
  inside <- vapply(cp$cp, function(v) {
    any(v >= windows[, "lo"] & v <= windows[, "hi"])
  }, logical(1))
  keep <- inside | (if ("fixed" %in% names(cp)) cp$fixed %in% TRUE else FALSE)
  res$constraints$dropped_outside_within <- cp$cp[!keep]
  res$changepoints <- cp[keep, , drop = FALSE]
  res$segments <- build_segments(res$data$value, res$changepoints$cp)
  res
}

# Internal: the noise standard deviation of a series, estimated from its
# first differences, which a changepoint barely disturbs (one large
# difference among n): the MAD of the differences over sqrt(2). Falls back
# to the plain standard deviation when more than half the differences are
# zero, where the MAD would be 0.
#' @noRd
noise_sd <- function(value) {
  d <- diff(value[!is.na(value)])
  if (length(d) < 2L) return(NA_real_)
  s <- stats::mad(d) / sqrt(2)
  if (!is.finite(s) || s == 0) s <- stats::sd(d) / sqrt(2)
  s
}

# Internal: the standardised size of every changepoint's mean shift: the
# difference of the two adjacent segment means in units of the series'
# noise standard deviation (noise_sd()). Pooling only the two adjacent
# segments instead let two short spurious segments, whose own spread is
# small by chance, pass any threshold.
#' @noRd
standardised_shifts <- function(value, cps, sigma = noise_sd(value)) {
  if (!length(cps)) return(numeric(0))
  bounds <- c(0L, cps, length(value))
  vapply(seq_along(cps), function(k) {
    a <- value[(bounds[k] + 1L):bounds[k + 1L]]
    b <- value[(bounds[k + 1L] + 1L):bounds[k + 2L]]
    a <- a[!is.na(a)]
    b <- b[!is.na(b)]
    if (!length(a) || !length(b)) return(NA_real_)
    delta <- mean(b) - mean(a)
    if (!is.finite(sigma) || sigma == 0) {
      return(if (delta == 0) 0 else sign(delta) * Inf)
    }
    delta / sigma
  }, numeric(1))
}

# Internal: drop changepoints whose standardised shift is below
# `min_effect`, one at a time from the smallest, recomputing after each
# removal because merging two segments changes their neighbours' effects.
# Fixed changepoints are never removed.
#' @noRd
apply_min_effect <- function(res, min_effect) {
  change <- scalar_chr(res$change_in)
  if (!identical(change, "mean")) {
    cpt_abort("`min_effect` filters on the standardised shift in the mean, ",
              "and this result detected a change in ", change, ", which a ",
              "mean shift does not measure. Use it with a change in mean, ",
              "or inspect `cpt_effect()` for this fit.",
              class = "unsupported",
              data = list(method = scalar_chr(res$method),
                          requested = "min_effect", supported = "mean"))
  }
  cp <- res$changepoints
  if (!nrow(cp)) return(res)
  wide <- n_coordinates(res) > 1L
  series <- if (wide) {
    w <- res$data_wide
    as.matrix(w[, setdiff(names(w), c("index", "index_value")),
                drop = FALSE])
  } else {
    res$data$value
  }
  fixed <- if ("fixed" %in% names(cp)) cp$fixed %in% TRUE else
    rep(FALSE, nrow(cp))
  removed <- integer(0)
  repeat {
    eff <- if (wide) {
      m <- vapply(seq_len(ncol(series)), function(j) {
        abs(standardised_shifts(series[, j], cp$cp))
      }, numeric(nrow(cp)))
      m <- matrix(m, nrow = nrow(cp))
      m[is.na(m)] <- -Inf
      apply(m, 1, max)
    } else {
      abs(standardised_shifts(series, cp$cp))
    }
    eff[is.na(eff) | eff == -Inf] <- Inf
    eff[fixed] <- Inf
    if (!length(eff) || min(eff) >= min_effect) break
    drop <- which.min(eff)
    removed <- c(removed, cp$cp[drop])
    cp <- cp[-drop, , drop = FALSE]
    fixed <- fixed[-drop]
    if (!nrow(cp)) break
  }
  res$changepoints <- cp
  res$segments <- build_segments(res$data$value, cp$cp)
  res$constraints$min_effect <- min_effect
  res$constraints$dropped_below_min_effect <- sort(removed)
  res
}

# ---- a missing engine ---------------------------------------------------------

# Internal: the installed built-in methods that detect the same kind of
# change on the same kind of series, for a method whose engine is missing.
#' @noRd
engine_alternatives <- function(method, change_in) {
  reg <- builtin_registry_core()
  i <- match(method, reg$method)
  if (is.na(i)) return(character(0))
  tab <- family_translation()
  cand <- unique(tab$method[tab$change_in == change_in])
  cand <- setdiff(cand, method)
  j <- match(cand, reg$method)
  same_shape <- reg$univariate[j] == reg$univariate[i] |
    reg$multivariate[j] == reg$multivariate[i]
  cand <- cand[same_shape]
  j <- match(cand, reg$method)
  cand[vapply(reg$engine[j], engine_installed, logical(1))]
}

# Internal: re-raise a missing-engine error with what can run instead. An
# engine can be missing because it was never installed or because CRAN
# archived it (the roadmap's §100 policy); either way the analysis should
# not stop at "install this".
#' @noRd
abort_with_alternatives <- function(e, method, change_in) {
  alt <- engine_alternatives(method, change_in)
  fields <- unclass(e)[setdiff(names(unclass(e)),
                               c("message", "call", "alternatives"))]
  msg <- conditionMessage(e)
  if (length(alt)) {
    shown <- utils::head(alt, 6)
    msg <- paste0(msg, " Installed methods that detect a change in ",
                  change_in, ": ", paste(shown, collapse = ", "),
                  if (length(alt) > length(shown)) ", ..." else "",
                  " (see cpt_methods()).")
  }
  # Only a ggchangepoint_engine_missing condition reaches this.
  cpt_abort(msg, class = "engine_missing",
            data = c(fields, list(alternatives = alt)))
}

# ---- checks before and after the fit ----------------------------------------

# Internal: a result this short is not interpretable, whatever the engine
# returns (§172.1: `pelt` puts two changepoints in `c(1, 5, 9)`).
#' @noRd
warn_short_series <- function(n) {
  if (n >= 10L) return(invisible(FALSE))
  cpt_warn("`x` has only ", n, " observations. A segmentation of a series ",
           "this short is not interpretable: every segment holds a handful ",
           "of points, and most engines' guarantees assume far more.",
           class = "short_series", data = list(n = n))
  invisible(TRUE)
}

# Internal: say what kind of data this looks like when the family assumed
# is Gaussian. Measured (§246): Bernoulli series raise 165 times the false
# positives of Gaussian ones, and six engines put a changepoint at almost
# every 0/1 transition. A count series whose variance tracks its mean has
# the same heteroscedastic trap in milder form.
#' @noRd
detect_data_type <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) < 10L) return("continuous")
  u <- unique(v)
  # Noise, not a clean step: a 0/1 series with one or two transitions is a
  # step function a Gaussian cost handles; the failure measured is Bernoulli
  # noise, which flips often.
  if (length(u) <= 2L && all(u %in% c(0, 1))) {
    return(if (sum(diff(v) != 0) >= 5L) "binary" else "continuous")
  }
  if (length(u) < 3L) return("continuous")
  whole <- all(abs(v - round(v)) < 1e-8)
  if (whole && all(v >= 0)) {
    # Local dispersion (half the variance of the differences) against the
    # mean: about 1 for Poisson noise whatever the changes, and near 0 for a
    # noiseless integer step series, which the global variance would
    # mistake for overdispersed counts.
    m <- mean(v)
    local <- stats::var(diff(v)) / 2
    if (m > 0 && m < 50 && local / m > 0.5 && local / m < 3) {
      return("counts")
    }
  }
  if (all(v >= 0 & v <= 1) && length(u) <= 21L &&
      all(abs(u * 20 - round(u * 20)) < 1e-8)) {
    return("proportion")
  }
  "continuous"
}

#' @noRd
warn_data_type <- function(x, family, method) {
  # A family named explicitly, Gaussian included, is a decision the caller
  # made; the warning is for the call that never considered one.
  if (!is.null(family)) return(invisible(NULL))
  if (method %in% distribution_free_methods()) return(invisible(NULL))
  v <- if (is.matrix(x) || is.data.frame(x)) as.numeric(as.matrix(x)[, 1]) else
    as.numeric(x)
  type <- detect_data_type(v)
  if (type == "continuous") return(invisible(type))
  fams <- methods_with_family(switch(type, binary = "binomial",
                                     counts = "poisson", "binomial"))
  cpt_warn("`x` looks like ", switch(type,
    binary = "binary (0/1) data",
    counts = "counts whose variance tracks their mean",
    proportion = "proportions on a coarse grid"),
    ", and `", method, "` is fitting a Gaussian cost. Measured on such ",
    "series, Gaussian costs raise many spurious changepoints (on binary ",
    "data some engines mark nearly every 0/1 transition). ",
    switch(type,
      binary = "Pass `family = \"binomial\"`",
      counts = "Pass `family = \"poisson\"`",
      proportion = "Model the counts behind them, or pass `family = \"binomial\"`"),
    " with one of ", paste(fams, collapse = ", "), ", or use a ",
    "distribution-free method (see cpt_recommend(data_type = )).",
    class = "data_type", data = list(data_type = type, method = method))
  invisible(type)
}

# Internal: more changepoints than the series can plausibly hold. A
# segmentation whose segments average fewer than ten observations, with at
# least ten of them, is the signature of a mis-scaled penalty or of
# dependence the engine does not model (§179: `pelt` returns 139 on data
# multiplied by ten), not of a series that really changes that often.
#' @noRd
warn_implausible_count <- function(res) {
  n <- nrow(res$data)
  k <- nrow(res$changepoints)
  if (k < 10L || n / (k + 1) >= 10) return(invisible(FALSE))
  # A changepoint after every observation has its own, more specific
  # warning (warn_if_degenerate()); saying it twice is noise.
  if (k >= n - 1L) return(invisible(FALSE))
  cpt_warn("`", scalar_chr(res$method), "` reported ", k, " changepoints in ",
           n, " observations: segments of ", format(round(n / (k + 1), 1)),
           " observations on average. That is more often the signature of a ",
           "penalty on the wrong scale or of autocorrelated noise than of a ",
           "series that changes this often; see cpt_assumptions() for this ",
           "fit.", class = "implausible_count", data = list(n = n, k = k))
  invisible(TRUE)
}

# Internal: a `$fit` much larger than the answer. Measured (§222): the
# engine object is up to 1,957 times the size of what the result reports,
# and `strucchange` keeps an O(n^2) table. Once per method per session: the
# point is that the user learns `keep_fit` exists, and a loop of fits would
# otherwise repeat the same sentence hundreds of times.
#' @noRd
warn_large_fit <- function(res, limit = 10 * 1024^2) {
  if (is.null(res$fit)) return(invisible(FALSE))
  method <- scalar_chr(res$method)
  warned <- .cpt_state$large_fit_warned %||% character(0)
  if (method %in% warned) return(invisible(FALSE))
  size <- tryCatch(as.numeric(utils::object.size(res$fit)),
                   error = function(e) NA_real_)
  if (!is.finite(size) || size < limit) return(invisible(FALSE))
  .cpt_state$large_fit_warned <- c(warned, method)
  cpt_warn("`", method, "` returned an engine object of ",
           format(round(size / 1024^2, 1)), " MB, kept in `$fit`. Nothing ",
           "reads it but the engine-specific accessors; pass `keep_fit = ",
           "FALSE` to drop it, or `res$fit <- NULL` before saving. (Said ",
           "once per method per session.)",
           class = "large_fit", data = list(bytes = size, method = method))
  invisible(TRUE)
}

# Internal: refuse a `...` name that is a near miss of a real argument. The
# names reaching a wrapper through `...` are otherwise forwarded to engines
# that end their own signatures in `...` and drop what they do not know, so
# `n_interval = 50` for `n_intervals` ran at the default without a word.
# Only near misses are refused: a name that resembles nothing may be a
# legitimate engine argument this check cannot see.
#' @noRd
check_dots_names <- function(dots_names, method, wrapper) {
  nms <- dots_names[nzchar(dots_names)]
  if (!length(nms)) return(invisible(TRUE))
  accepted <- method_arg_names(method, wrapper)
  own <- setdiff(names(formals(cpt_detect)), "...")
  known <- unique(c(accepted, own))
  for (nm in setdiff(nms, known)) {
    # One- and two-letter names (`n`, `h`, `M`) are too short to call a
    # near miss: everything is within an edit of them.
    if (nchar(nm) <= 2L) next
    d <- utils::adist(tolower(nm), tolower(known))[1, ]
    limit <- if (nchar(nm) <= 6L) 1L else 2L
    prefix <- nchar(nm) >= 3L & startsWith(known, nm)
    near <- known[d <= limit | prefix]
    if (length(near)) {
      best <- near[order(d[match(near, known)])][1]
      cpt_abort("`", nm, "` is not an argument of cpt_detect() or of the `",
                method, "` method. Did you mean `", best, "`?",
                class = "bad_argument",
                data = list(argument = nm, suggestion = best,
                            method = method))
    }
  }
  invisible(TRUE)
}

# Internal: every argument name the method's wrapper accepts, plus those of
# the engine functions it calls, read from their formals (and cached). An
# engine that is not installed contributes nothing, which only makes the
# near-miss check above more cautious.
#' @noRd
method_arg_names <- function(method, wrapper) {
  cached(paste0("argnames_", method), {
    ns <- asNamespace("ggchangepoint")
    f <- get(wrapper, envir = ns)
    out <- setdiff(names(formals(f)), "...")
    reg <- builtin_registry_core()
    engine <- reg$engine[match(method, reg$method)]
    calls <- engine_calls_in(body(f), engine)
    out <- c(out, dots_fields_in(body(f)))
    for (fun in calls) {
      g <- tryCatch(getExportedValue(engine, fun), error = function(e) NULL)
      if (is.function(g)) out <- c(out, setdiff(names(formals(g)), "..."))
    }
    if (method %in% c("pelt", "binseg", "segneigh", "amoc")) {
      out <- c(out, names(formals(changepoint::cpt.mean)),
               names(formals(changepoint::cpt.meanvar)),
               names(formals(changepoint::cpt.var)))
    }
    if (method == "np") {
      out <- c(out, names(formals(changepoint.np::cpt.np)))
    }
    unique(out)
  })
}

# Internal: the `engine::fun` references in a function body.
#' @noRd
engine_calls_in <- function(expr, engine) {
  found <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("::")) || identical(e[[1]],
                                                       as.name(":::"))) {
        if (identical(as.character(e[[2]]), engine)) {
          found <<- c(found, as.character(e[[3]]))
        }
      }
      for (a in as.list(e)) if (!missing(a) && !is.null(a)) walk(a)
    }
  }
  walk(expr)
  unique(found)
}

# Internal: the names a wrapper reads out of its captured `...`
# (`dots$burnin`, `list(...)[["beta"]]`), which are arguments it accepts
# without declaring them.
#' @noRd
dots_fields_in <- function(expr) {
  found <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      head <- e[[1]]
      if ((identical(head, as.name("$")) || identical(head, as.name("[["))) &&
          length(e) == 3L) {
        obj <- e[[2]]
        is_dots <- (is.name(obj) && as.character(obj) %in%
                      c("dots", "extra", "args")) ||
          (is.call(obj) && identical(obj[[1]], as.name("list")))
        key <- e[[3]]
        if (is_dots && (is.name(key) || is.character(key))) {
          found <<- c(found, as.character(key))
        }
      }
      for (a in as.list(e)) if (!missing(a) && !is.null(a)) walk(a)
    }
  }
  walk(expr)
  unique(found)
}
