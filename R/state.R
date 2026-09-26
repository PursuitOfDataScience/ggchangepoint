# ---------------------------------------------------------------------------
# State the package keeps for itself during a session: a cache for tables
# that depend only on the code (the translation table is rebuilt from the
# registry, and cpt_detect() consults it several times per call), and the
# one piece of dynamic scope, the missing-value allowance that
# `na_action = "engine"` switches on for the duration of a fit.
# ---------------------------------------------------------------------------

#' @noRd
.cpt_state <- new.env(parent = emptyenv())

#' @noRd
.cpt_cache <- new.env(parent = emptyenv())

# Internal: compute a code-derived table once per session.
#' @noRd
cached <- function(key, expr) {
  if (!exists(key, envir = .cpt_cache, inherits = FALSE)) {
    assign(key, force(expr), envir = .cpt_cache)
  }
  get(key, envir = .cpt_cache, inherits = FALSE)
}

# Internal: are missing values being passed through to the engine?
#' @noRd
na_allowed <- function() isTRUE(.cpt_state$allow_na)

# Internal: evaluate `expr` with missing values allowed through
# validate_data(), restoring the previous setting however `expr` exits.
#' @noRd
with_na_allowed <- function(expr) {
  old <- .cpt_state$allow_na
  .cpt_state$allow_na <- TRUE
  on.exit(.cpt_state$allow_na <- old, add = TRUE)
  force(expr)
}

# Internal: refuse the inputs that as.numeric() converts without complaint
# into a series that means something else. A survival object is a matrix of
# times and censoring flags, so it would be read as two series, the second
# one the censoring indicator. A vector of dates or timestamps converts to
# days or seconds since 1970: an increasing line, on which every detector
# reports the drift of the calendar. Both are refused with the route that
# does answer the question.
#' @noRd
refuse_special_values <- function(x, arg = "x") {
  if (inherits(x, "Surv")) {
    cpt_abort("`", arg, "` is a survival object (times with censoring ",
              "flags). Detection would read the censoring indicator as a ",
              "second series. Changes in a hazard with censoring are not ",
              "supported; for fully observed waiting times, pass the times ",
              "themselves with `family = \"exponential\"`.",
              class = "bad_type")
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    cpt_abort("`", arg, "` holds dates or timestamps, which convert to an ",
              "increasing count of days or seconds: every detector would ",
              "report the calendar. If these are the times of events, count ",
              "them per period and detect on the counts (with `family = ",
              "\"poisson\"`); if they label observations, pass them as ",
              "`index`.", class = "bad_type")
  }
  invisible(TRUE)
}
