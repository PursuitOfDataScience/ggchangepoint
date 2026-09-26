# ---------------------------------------------------------------------------
# Versions: which engine produced a result, and has it changed since?
#
# A `ggcpt` recorded the method, the penalty, the call and the runtime, and
# not which version of the engine computed it. With thirty-five engines
# maintained by other people that is the reproducibility story failing at
# the last step: 0.5.0 already met three version-specific upstream
# behaviours (HDCD's threshold bug, binsegRcpp's cost set, ChangePointTaylor's
# bootstrap range), so the package reasoned about versions in its code and
# never recorded them in its results.
# ---------------------------------------------------------------------------

# Internal: the installed version of a package as a string, or NA. Reads the
# DESCRIPTION rather than loading the namespace, for the reason
# engine_installed() gives: loading is not free and can fail for reasons
# that have nothing to do with the version.
#' @noRd
engine_version <- function(pkg) {
  if (length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) return(NA_character_)
  if (identical(pkg, "ggchangepoint")) {
    return(as.character(utils::packageVersion("ggchangepoint")))
  }
  path <- suppressWarnings(find.package(pkg, quiet = TRUE))
  if (length(path) == 0L) return(NA_character_)
  v <- tryCatch(read.dcf(file.path(path[1], "DESCRIPTION"),
                         fields = "Version")[1, 1],
                error = function(e) NA_character_)
  if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v)
}

# Internal: the version stamp a result carries. Built once, when the result
# is made, so it records the engine that ran rather than the one installed
# when the object is later read.
#' @noRd
version_stamp <- function(method, engine = NULL) {
  if (is.null(engine)) {
    reg <- full_registry()
    engine <- reg$engine[match(method, reg$method)]
  }
  engine <- if (length(engine) == 1L && !is.na(engine)) engine else NA_character_
  list(
    engine = engine,
    engine_version = engine_version(engine),
    ggchangepoint = as.character(utils::packageVersion("ggchangepoint")),
    r = paste(R.version$major, R.version$minor, sep = "."),
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

# Internal: a one-line note when the engine installed now is not the one
# that produced a result, or NULL when they agree (or there is nothing to
# compare). print() shows it; a result that reproduces needs no comment.
#' @noRd
version_drift <- function(object) {
  v <- object$versions
  if (is.null(v) || is.null(v$engine) || is.na(v$engine) ||
      is.null(v$engine_version) || is.na(v$engine_version)) {
    return(NULL)
  }
  now <- engine_version(v$engine)
  if (identical(now, v$engine_version)) return(NULL)
  paste0(v$engine, " ", v$engine_version, " made this result; ",
         if (is.na(now)) "it is not installed now" else
           paste0(now, " is installed now"),
         ". See cpt_verify().")
}

#' Re-run a result against the engines installed now
#'
#' A result records which version of its engine produced it (see
#' \code{glance()}'s \code{engine_version}). \code{cpt_verify()} answers the
#' question that record exists for: \emph{does the answer still hold?} It
#' re-runs the same method on the same series with the settings the result
#' recorded, using whatever engines are installed today, and reports whether
#' the changepoints moved and which versions changed in between.
#'
#' Upstream engines change: a default is revised, a bug is fixed, a
#' tie-breaking rule moves. None of that raises an error, so a result
#' produced last year and re-run today can differ without anything saying
#' so. This is the check to run when re-opening an analysis, and before
#' quoting an old result as current.
#'
#' @param object A \code{ggcpt} object from \code{\link{cpt_detect}()}.
#' @param ... Further arguments for the re-run, overriding what was recovered
#'   from the result (for instance an engine argument the recorded call held
#'   as a variable, which cannot be recovered from the object).
#' @param tolerance Positions a changepoint may move and still count as
#'   the same. Defaults to \code{0}: an exact match.
#'
#' @details
#' The re-run uses the series stored on the result, its method, the change
#' type it can be asked for, its penalty, and every engine argument of the
#' original call that was written as a literal (\code{seed = 42},
#' \code{n_intervals = 500}). An argument written as a variable cannot be
#' recovered from the object, and is listed in \code{not_recovered}; pass
#' it again through \code{...} if it mattered.
#'
#' @return A \code{ggcpt_verification} object: a list with \code{verified}
#'   (\code{TRUE} when every changepoint reproduces within
#'   \code{tolerance}), \code{then} and \code{now} (the two sets of
#'   locations), \code{added} and \code{removed} (locations in one set with
#'   no partner in the other), \code{versions} (a tibble with
#'   \code{component}, \code{then}, \code{now} and \code{changed}),
#'   \code{not_recovered} (call arguments that could not be replayed) and
#'   \code{refit}, the new \code{ggcpt}. With a \code{print()} method.
#' @seealso \code{\link{glance.ggcpt}()} for the recorded
#'   \code{engine_version}.
#' @export
#' @family result class
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(80), rnorm(80, 4)), method = "pelt")
#' cpt_verify(fit)
cpt_verify <- function(object, ..., tolerance = 0) {
  if (!is_ggcpt(object)) {
    cpt_abort("`object` must be a ggcpt object.", class = "bad_argument")
  }
  validate_scalar(tolerance, "tolerance", min = 0)
  if (!bootstrap_possible(object)) {
    cpt_abort("`cpt_verify()` re-runs the detector, but `",
              scalar_chr(object$method), "` is not a method cpt_detect() ",
              "knows, so there is nothing to re-run. A result built with ",
              "as_ggcpt() records someone else's changepoints; register the ",
              "detector with cpt_register_method() to make it re-runnable.",
              class = "capability_absent",
              data = list(method = scalar_chr(object$method)))
  }
  if (!rerun_matches_result(object)) {
    cpt_abort("This result was fitted from a formula, and the covariates ",
              "are not stored on it, so it cannot be re-run from the object ",
              "alone. Re-run the original cpt_detect(y ~ ..., data = ) call.",
              class = "capability_absent",
              data = list(method = scalar_chr(object$method)))
  }
  method <- scalar_chr(object$method)
  dots <- list(...)
  replay <- replayable_call_args(object$call)
  args <- utils::modifyList(replay$args, dots)
  if (is.null(args[["change_in", exact = TRUE]])) {
    ci <- rerun_change_in(object)
    if (!is.null(ci)) args$change_in <- ci
  }
  if (is.null(args[["penalty", exact = TRUE]])) {
    pen <- rerun_penalty(object)
    if (!is.null(pen)) args$penalty <- pen
  }
  series <- if (n_coordinates(object) > 1L) {
    wide <- object$data_wide
    as.matrix(wide[, setdiff(names(wide), c("index", "index_value")),
                   drop = FALSE])
  } else {
    object$data$value
  }
  refit <- do.call(cpt_detect, c(list(series, method = method), args))

  then <- object$changepoints$cp
  now <- refit$changepoints$cp
  m <- match_changepoints(now, then, tolerance)
  added <- setdiff(now, m$pred)
  removed <- setdiff(then, m$truth)
  vt <- object$versions %||% list()
  vn <- refit$versions %||% list()
  comp <- c("engine_version", "ggchangepoint", "r")
  versions <- tibble::tibble(
    component = c(paste0(vt$engine %||% method, " (engine)"),
                  "ggchangepoint", "R"),
    then = vapply(comp, function(k) scalar_chr(vt[[k]] %||% NA_character_),
                  character(1)),
    now = vapply(comp, function(k) scalar_chr(vn[[k]] %||% NA_character_),
                 character(1))
  )
  versions$changed <- !is.na(versions$then) & !is.na(versions$now) &
    versions$then != versions$now
  structure(
    list(verified = length(added) == 0L && length(removed) == 0L,
         then = then, now = now, added = added, removed = removed,
         tolerance = tolerance, versions = versions,
         not_recovered = replay$not_recovered, refit = refit,
         method = method),
    class = "ggcpt_verification"
  )
}

# Internal: the arguments of a recorded cpt_detect() call that can be
# replayed from the call alone: the ones written as literals. `x`, `method`,
# `index`, `y` and `data` describe the series and are supplied from the
# object instead.
#' @noRd
replayable_call_args <- function(call) {
  out <- list(args = list(), not_recovered = character(0))
  if (!is.call(call)) return(out)
  a <- as.list(call)[-1]
  if (length(a) == 0L) return(out)
  nms <- names(a) %||% rep("", length(a))
  skip <- c("", "x", "method", "index", "y", "data", "group")
  for (i in seq_along(a)) {
    nm <- nms[i]
    if (nm %in% skip) next
    v <- a[[i]]
    if (is.atomic(v) && length(v) >= 1L) {
      out$args[[nm]] <- v
    } else if (is.call(v) && identical(v[[1]], as.name("c")) &&
               all(vapply(as.list(v)[-1], is.atomic, logical(1)))) {
      out$args[[nm]] <- eval(v, baseenv())
    } else if (is.call(v) && identical(v[[1]], as.name("-")) &&
               length(v) == 2L && is.atomic(v[[2]])) {
      out$args[[nm]] <- eval(v, baseenv())
    } else {
      out$not_recovered <- c(out$not_recovered, nm)
    }
  }
  out
}

#' @rdname cpt_verify
#' @param x A \code{ggcpt_verification} object.
#' @export
print.ggcpt_verification <- function(x, ...) {
  cat("ggcpt_verification (method: ", x$method, ")\n", sep = "")
  cat_field("Verdict", if (x$verified) {
    "reproduces with the engines installed now"
  } else {
    "CHANGED with the engines installed now"
  })
  cat_field("Changepoints then", if (length(x$then)) {
    paste(x$then, collapse = ", ")
  } else "none")
  cat_field("Changepoints now", if (length(x$now)) {
    paste(x$now, collapse = ", ")
  } else "none")
  if (!x$verified) {
    if (length(x$removed)) cat_field("No longer found", paste(x$removed,
                                                             collapse = ", "))
    if (length(x$added)) cat_field("Newly found", paste(x$added,
                                                       collapse = ", "))
  }
  cat("\nVersions:\n")
  print(x$versions, n = 5)
  if (length(x$not_recovered)) {
    cat("\nNot replayed (written as variables in the original call): ",
        paste0("`", x$not_recovered, "`", collapse = ", "),
        ".\nPass them through `...` if they affect the answer.\n", sep = "")
  }
  invisible(x)
}
