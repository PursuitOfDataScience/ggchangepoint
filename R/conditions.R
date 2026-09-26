# ---------------------------------------------------------------------------
# Typed conditions.
#
# Until 0.6.0 every failure this package could produce was a bare
# `simpleError` whose only distinguishing feature was English text, so a
# program calling it (a Shiny app, a batch pipeline, another package) could
# tell "this engine is not installed" from "your series has an NA" only by
# matching the message, and messages are not an API. The measured state
# before the change: 220 conditions signalled, none of them classed, and 207
# tests pinning the prose, which made every message improvement a
# test-breaking change.
#
# Every error now carries a class from a small, documented hierarchy (see
# ?ggchangepoint-conditions), always ending in `ggchangepoint_error`, plus
# the data a caller needs to render its own message. The text is unchanged:
# the classes are metadata. `call = NULL` is kept deliberately, because the
# provenance test in the suite relies on it: a condition this package means
# to raise has no call, and one that leaked from base R or an engine does.
# ---------------------------------------------------------------------------

# Internal: the class hierarchy. Each entry lists the classes a condition of
# that kind carries, most specific first; the base class is appended by
# cpt_condition_class().
#' @noRd
cpt_error_classes <- function() {
  list(
    input_error      = "ggchangepoint_input_error",
    short_series     = c("ggchangepoint_short_series",
                         "ggchangepoint_input_error"),
    wrong_dimension  = c("ggchangepoint_wrong_dimension",
                         "ggchangepoint_input_error"),
    non_finite       = c("ggchangepoint_non_finite",
                         "ggchangepoint_input_error"),
    bad_type         = c("ggchangepoint_bad_type",
                         "ggchangepoint_input_error"),
    bad_argument     = c("ggchangepoint_bad_argument",
                         "ggchangepoint_input_error"),
    unsupported      = "ggchangepoint_unsupported",
    capability_absent = c("ggchangepoint_capability_absent",
                          "ggchangepoint_unsupported"),
    planned_method   = c("ggchangepoint_planned_method",
                         "ggchangepoint_unsupported"),
    unknown_method   = c("ggchangepoint_unknown_method",
                         "ggchangepoint_unsupported"),
    engine_missing   = "ggchangepoint_engine_missing",
    engine_error     = "ggchangepoint_engine_error",
    upstream_bug     = "ggchangepoint_upstream_bug",
    internal         = "ggchangepoint_internal_error"
  )
}

# Internal: the warning hierarchy, the same shape as the errors'.
#' @noRd
cpt_warning_classes <- function() {
  list(
    warning            = character(0),
    cp_dropped         = "ggchangepoint_cp_dropped",
    degenerate         = "ggchangepoint_degenerate_segmentation",
    assumption         = "ggchangepoint_assumption",
    scale_sensitive    = c("ggchangepoint_scale_sensitive",
                           "ggchangepoint_assumption"),
    data_type          = c("ggchangepoint_data_type",
                           "ggchangepoint_assumption"),
    dependence         = c("ggchangepoint_dependence",
                           "ggchangepoint_assumption"),
    short_series       = "ggchangepoint_short_series_warning",
    implausible_count  = "ggchangepoint_implausible_count",
    change_in_routed   = "ggchangepoint_change_in_routed",
    argument_ignored   = "ggchangepoint_argument_ignored",
    level_not_applied  = "ggchangepoint_level_not_applied",
    selection          = "ggchangepoint_selection_unadjusted",
    collapsed_ladder   = "ggchangepoint_collapsed_ladder",
    replicates_failed  = "ggchangepoint_replicates_failed",
    irregular_index    = "ggchangepoint_irregular_index",
    dropped_input      = "ggchangepoint_dropped_input",
    large_fit          = "ggchangepoint_large_fit",
    engine_failed      = "ggchangepoint_engine_failed",
    recycled           = "ggchangepoint_recycled_input",
    wide_interval      = "ggchangepoint_wide_interval",
    na_omitted         = "ggchangepoint_na_omitted",
    constraint         = "ggchangepoint_constraint",
    deprecated         = "ggchangepoint_deprecated"
  )
}

# Internal: the full class vector for a condition kind.
#' @noRd
cpt_condition_class <- function(kind, type = c("error", "warning",
                                               "message")) {
  type <- cpt_match_arg(type)
  table <- switch(type,
    error = cpt_error_classes(),
    warning = cpt_warning_classes(),
    message = list()
  )
  specific <- table[[kind]]
  if (is.null(specific)) {
    # A class the table does not know is a bug in the calling code, but a
    # condition that fails to be raised is worse than one raised with the
    # generic class, so fall back rather than erroring about the error.
    specific <- paste0("ggchangepoint_", kind)
  }
  base <- switch(type,
    error = c("ggchangepoint_error", "error", "condition"),
    warning = c("ggchangepoint_warning", "warning", "condition"),
    message = c("ggchangepoint_message", "message", "condition")
  )
  unique(c(specific, base))
}

#' Raise a classed ggchangepoint error
#'
#' The one way this package signals an error. The message is built like
#' \code{stop()}'s, from the unnamed arguments, and \code{call.} is accepted
#' and ignored so a converted call site reads as it did: every condition is
#' raised without a call, which is what lets the test suite tell a
#' deliberate refusal from a leak.
#' @param ... Message parts, pasted together.
#' @param class Condition kind; a name from \code{cpt_error_classes()}.
#' @param data Named list of fields attached to the condition.
#' @param parent Optional condition this one wraps (an engine's error).
#' @param call. Ignored.
#' @noRd
cpt_abort <- function(..., class = "input_error", data = list(),
                      parent = NULL, call. = FALSE) {
  msg <- .makeMessage(...)
  cond <- c(list(message = msg, call = NULL), data)
  if (!is.null(parent)) cond$parent <- parent
  stop(structure(cond, class = cpt_condition_class(class, "error")))
}

# Internal: the kind name of a condition raised by cpt_abort(), or NULL for
# a condition from anywhere else. Read off the most specific class, so a
# re-raise keeps exactly the classification the original carried.
#' @noRd
cpt_condition_kind <- function(cond) {
  cls <- class(cond)
  table <- cpt_error_classes()
  firsts <- vapply(table, `[[`, character(1), 1L)
  hit <- match(cls, firsts)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0L) return(NULL)
  names(table)[hit[1]]
}

# Internal: raise again with more context in front of the message, keeping
# the original's class and fields. A panel member that fails reports which
# member it was without turning "the series is too short" into a generic
# error: the kind a caller branches on survives the re-raise. A condition
# that did not come from this package is classed as an engine failure, the
# only way one can reach a caller here.
#' @noRd
cpt_rethrow <- function(cond, ..., class = NULL) {
  kind <- class %||% cpt_condition_kind(cond) %||% "engine_error"
  fields <- unclass(cond)
  fields <- fields[setdiff(names(fields), c("message", "call", "parent"))]
  cpt_abort(..., conditionMessage(cond), class = kind, data = fields,
            parent = cond)
}

# Internal: run an engine and class whatever leaks out of it. An error this
# package raised on purpose is already a `ggchangepoint_error` and passes
# through untouched; anything else came from the engine (or from base R
# inside it) and is re-signalled as `ggchangepoint_engine_error`, with the
# engine's own condition as `parent` and its message unchanged. The call is
# kept rather than cleared: it is what tells a reader (and the provenance
# test in the suite) that the failure happened inside the engine rather
# than being a refusal written here.
#' @noRd
with_engine_errors <- function(expr, method) {
  withCallingHandlers(expr, error = function(e) {
    if (inherits(e, "ggchangepoint_error")) return(invisible(NULL))
    reg <- builtin_registry()
    engine <- reg$engine[match(method, reg$method)]
    if (is.na(engine)) engine <- NA_character_
    stop(structure(
      list(message = conditionMessage(e),
           call = compact_call(conditionCall(e), engine),
           method = method, engine = engine,
           engine_version = engine_version(engine), parent = e),
      class = cpt_condition_class("engine_error", "error")))
  })
}

# Internal: a call short enough to print. An engine reached through
# do.call() with a function object reports that whole function as its
# call, so "Error in" was followed by forty lines of the engine's source.
# The head is kept when it names the function (`cpt.mean(...)`), and
# replaced by the engine's name when it is an anonymous function; the
# arguments become `...`. Never NULL for a non-NULL call: a call is what
# marks the error as having come from inside the engine.
#' @noRd
compact_call <- function(call, engine = NA_character_) {
  if (is.null(call) || !is.call(call)) return(call)
  head <- call[[1]]
  named <- is.name(head) ||
    (is.call(head) && (identical(head[[1]], as.name("::")) ||
                         identical(head[[1]], as.name(":::"))))
  if (!named) {
    label <- if (is.na(engine) || !nzchar(engine)) "engine" else engine
    head <- as.name(paste0(label, " engine"))
  }
  as.call(list(head, quote(...)))
}

# Internal: is this failure a result about one method, which a fan-out
# records and moves past, or something the caller would hit whichever method
# ran? An engine that failed, is not installed, does not offer what was
# asked, carries a guarded upstream bug, or cannot take a series of this
# shape or length is a finding about that method. Bad data or a bad
# argument is the caller's to fix, and an unclassed error can only be a bug
# in this package; a benchmark that filed either in its `error` column
# would report them as facts about an engine.
#' @noRd
is_method_failure <- function(e) {
  # A misspelt or unwired method name is the caller's mistake, not a result
  # about the method.
  if (inherits(e, c("ggchangepoint_unknown_method",
                    "ggchangepoint_planned_method"))) {
    return(FALSE)
  }
  inherits(e, c("ggchangepoint_engine_error", "ggchangepoint_engine_missing",
                "ggchangepoint_unsupported", "ggchangepoint_upstream_bug",
                "ggchangepoint_wrong_dimension",
                "ggchangepoint_short_series"))
}

# Internal: the error handler every fan-out gives its runs. A method failure
# comes back as a `cpt_failed` marker holding the message and the
# condition's class; anything else is raised again, with `...` in front of
# its message to say which run it was, or untouched if this package did not
# raise it.
#' @noRd
fanout_failure <- function(e, ...) {
  if (is_method_failure(e)) {
    return(structure(conditionMessage(e), class = "cpt_failed",
                     condition_class = class(e)[1]))
  }
  if (inherits(e, "ggchangepoint_error")) cpt_rethrow(e, ...)
  stop(e)
}

#' Raise a classed ggchangepoint warning
#' @inheritParams cpt_abort
#' @noRd
cpt_warn <- function(..., class = "warning", data = list(), call. = FALSE) {
  msg <- .makeMessage(...)
  cond <- c(list(message = msg, call = NULL), data)
  warning(structure(cond, class = cpt_condition_class(class, "warning")))
}

#' Emit a classed ggchangepoint message
#' @inheritParams cpt_abort
#' @noRd
cpt_inform <- function(..., class = "message", data = list(),
                       appendLF = TRUE) {
  msg <- .makeMessage(..., appendLF = appendLF)
  cond <- c(list(message = msg, call = NULL), data)
  message(structure(cond, class = cpt_condition_class(class, "message")))
}

#' Conditions raised by ggchangepoint
#'
#' Every error, warning and message this package raises carries a class, so
#' code that calls it can react to the \emph{kind} of failure rather than
#' matching its wording. The text is written for a person and may be
#' improved between releases; the classes and the fields listed here are the
#' interface, and follow the package's deprecation policy.
#'
#' @section Errors:
#' Every error inherits from \code{ggchangepoint_error} (and from
#' \code{error} and \code{condition}). The subclasses, most specific first:
#' \describe{
#'   \item{\code{ggchangepoint_input_error}}{The call itself is not usable:
#'     a bad argument or data that violates a precondition. Its subclasses
#'     say which precondition:
#'     \code{ggchangepoint_short_series} (too few observations, for the
#'     package or for the engine that was asked; field \code{n}),
#'     \code{ggchangepoint_wrong_dimension} (a univariate method handed a
#'     matrix, or the reverse),
#'     \code{ggchangepoint_non_finite} (\code{NA}, \code{NaN} or \code{Inf}
#'     where the method needs finite data),
#'     \code{ggchangepoint_bad_type} (a factor, text, a survival object or
#'     timestamps where a numeric series was expected) and
#'     \code{ggchangepoint_bad_argument} (an argument value outside its
#'     documented range or vocabulary).}
#'   \item{\code{ggchangepoint_unsupported}}{The request is well formed but
#'     not something this method offers: a \code{change_in}, \code{family}
#'     or capability the registry does not list for it. Fields
#'     \code{method}, \code{requested} and \code{supported} where they
#'     apply. Subclasses: \code{ggchangepoint_capability_absent} (the
#'     result carries no interval, statistic, path or posterior to read),
#'     \code{ggchangepoint_planned_method} (named in \code{cpt_methods()}
#'     but not wired) and \code{ggchangepoint_unknown_method}.}
#'   \item{\code{ggchangepoint_engine_missing}}{A suggested engine package
#'     is not installed. Field \code{package}, and \code{install}, the call
#'     that installs it.}
#'   \item{\code{ggchangepoint_engine_error}}{The upstream engine failed.
#'     Fields \code{engine}, \code{method} and, where it was caught,
#'     \code{parent}, the engine's own condition.}
#'   \item{\code{ggchangepoint_upstream_bug}}{A known, version-guarded
#'     defect in an engine, refused rather than returned. Fields
#'     \code{package} and \code{version}.}
#'   \item{\code{ggchangepoint_internal_error}}{A state the package should
#'     never reach: a bug here, worth reporting.}
#' }
#'
#' The functions that run many fits (\code{\link{cpt_benchmark}()},
#' \code{\link{cpt_consensus}()}) record a failure that belongs to one
#' method (\code{ggchangepoint_engine_error},
#' \code{ggchangepoint_engine_missing}, \code{ggchangepoint_unsupported},
#' \code{ggchangepoint_upstream_bug}, or a series of the wrong shape or
#' length for that method) and carry on; any other error stops them, since
#' every method would have hit it.
#'
#' @section Warnings:
#' Every warning inherits from \code{ggchangepoint_warning}. The ones worth
#' handling by class: \code{ggchangepoint_assumption} and its subclasses
#' \code{ggchangepoint_scale_sensitive}, \code{ggchangepoint_data_type} and
#' \code{ggchangepoint_dependence} (the method's assumptions look violated
#' for this series); \code{ggchangepoint_degenerate_segmentation} (a
#' changepoint after every observation); \code{ggchangepoint_implausible_count}
#' (more changepoints than the series can plausibly hold);
#' \code{ggchangepoint_change_in_routed} (the request was answered with the
#' method's native change type); \code{ggchangepoint_argument_ignored};
#' \code{ggchangepoint_level_not_applied}; \code{ggchangepoint_selection_unadjusted}
#' (p-values computed at data-chosen locations);
#' \code{ggchangepoint_collapsed_ladder}; \code{ggchangepoint_replicates_failed};
#' \code{ggchangepoint_cp_dropped} (locations normalised away by
#' \code{\link{as_ggcpt}()}); \code{ggchangepoint_irregular_index};
#' \code{ggchangepoint_na_omitted}; \code{ggchangepoint_wide_interval};
#' \code{ggchangepoint_large_fit}; \code{ggchangepoint_short_series_warning}
#' (a series too short for the result to be interpretable);
#' \code{ggchangepoint_dropped_input} (constant coordinates or out-of-range
#' locations left out); \code{ggchangepoint_engine_failed} (some methods
#' failed and were left out); \code{ggchangepoint_recycled_input};
#' \code{ggchangepoint_constraint} and \code{ggchangepoint_deprecated}.
#' Anything else is plain \code{ggchangepoint_warning}.
#'
#' @section Messages:
#' Informational messages inherit from \code{ggchangepoint_message}, so
#' \code{suppressMessages(classes = "ggchangepoint_message")} silences this
#' package without silencing the engines.
#'
#' @examples
#' res <- tryCatch(cpt_detect(c(1, 2), method = "pelt"),
#'                 ggchangepoint_short_series = function(e) "too short")
#' res
#'
#' # Branch on the kind of failure, not on its wording:
#' tryCatch(
#'   cpt_detect(rnorm(50), method = "pelt", change_in = "network"),
#'   ggchangepoint_unsupported = function(e) {
#'     paste("unsupported:", e$method, "offers", toString(e$supported))
#'   }
#' )
#' @name ggchangepoint-conditions
#' @aliases ggchangepoint_error ggchangepoint_warning
#' @family result class
NULL
