# ---------------------------------------------------------------------------
# Machine-readable output.
#
# A growing share of calls come from software (pipelines, reporting agents,
# tool-using models) that has to read print() output or reach into the
# object's internals. `as_json()` is the whole result under a documented,
# versioned schema, and the schema's field names are part of the package's
# stability contract: a pipeline that depends on them is not broken by a
# minor release. `cpt_export()` and `cpt_import()` make the round trip a
# collaborator in another language can use.
# ---------------------------------------------------------------------------

#' The version of the JSON schema `as_json()` writes
#'
#' Bumped in its minor part when a field is added (readers ignore fields
#' they do not know) and in its major part when a field changes meaning or
#' is removed, which the deprecation policy allows only at a major release.
#' @noRd
cpt_json_schema_version <- function() "1.0.0"

#' Convert a result to JSON
#'
#' The whole result as JSON, under a documented and versioned schema, for
#' software that consumes changepoint results rather than a person reading
#' them.
#'
#' @param x A \code{ggcpt} object.
#' @param pretty Indent the output? Defaults to \code{TRUE}.
#' @param data Include the series itself (\code{index}, \code{value} and,
#'   where the engine supplied one, \code{fitted})? Defaults to \code{TRUE};
#'   without it the output is the answer alone, a few hundred bytes.
#' @param assumptions Include \code{\link{cpt_assumptions}()}? Defaults to
#'   \code{TRUE}.
#' @param digits Significant digits for numbers. Defaults to \code{NA}, the
#'   full precision needed for an exact round trip.
#' @param ... Ignored.
#' @return A single string of class \code{"json"}.
#'
#' @section Schema 1.0.0:
#' One object with these fields. Every field is always present; one that
#' does not apply is \code{null}, so a reader never has to test for a key.
#' \describe{
#'   \item{\code{schema}, \code{schema_version}}{\code{"ggchangepoint.ggcpt"}
#'     and the version, \code{"1.0.0"}.}
#'   \item{\code{method}, \code{engine}, \code{engine_version},
#'     \code{ggchangepoint_version}, \code{r_version}, \code{created}}{what
#'     produced the result, and when (ISO 8601).}
#'   \item{\code{change_in}, \code{family}}{what was detected, and the
#'     distribution family if one was asked for.}
#'   \item{\code{penalty}}{\code{{"type", "value"}}.}
#'   \item{\code{cp_convention}}{\code{"left"}: a changepoint is the last
#'     observation of its segment.}
#'   \item{\code{n}}{the series length.}
#'   \item{\code{index}}{\code{{"class", "label"}} of the time index, or
#'     \code{null}. Dates are written as ISO 8601 strings and timestamps
#'     with their offset.}
#'   \item{\code{changepoints}}{an array of objects, one per changepoint:
#'     \code{cp} (position) and \code{cp_value}, with \code{cp_index} when
#'     there is an index, and every engine-specific column
#'     (\code{ci_lower}, \code{ci_upper}, \code{posterior_prob}, ...).}
#'   \item{\code{segments}}{\code{seg_id}, \code{start}, \code{end},
#'     \code{n}, \code{param_estimate}.}
#'   \item{\code{regions}, \code{coefficients}, \code{constraints}}{the
#'     optional slots, as arrays of objects (or an object), or \code{null}.}
#'   \item{\code{diagnostics}}{\code{residual_dependence} (Ljung-Box
#'     \code{p_value}, \code{lag}, \code{acf1}), \code{na_omitted} and
#'     \code{expected_false_positives} where recorded.}
#'   \item{\code{assumptions}}{the \code{cpt_assumptions()} rows, or
#'     \code{null}.}
#'   \item{\code{call}}{the call that made the result, as text.}
#'   \item{\code{data}}{\code{{"index", "value", "fitted"}} arrays, or
#'     \code{null}.}
#' }
#' @seealso \code{\link{cpt_export}()} to write a file,
#'   \code{\link{cpt_import}()} to read one back,
#'   \code{\link{cpt_report}(format = "json")}.
#' @export
#' @family result class
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(50), rnorm(50, 3)), method = "pelt")
#' cat(as_json(fit, data = FALSE))
as_json <- function(x, ...) {
  UseMethod("as_json")
}

#' @rdname as_json
#' @export
as_json.ggcpt <- function(x, pretty = TRUE, data = TRUE, assumptions = TRUE,
                          digits = NA, ...) {
  need_pkg("jsonlite")
  validate_flag(pretty, "pretty")
  validate_flag(data, "data")
  validate_flag(assumptions, "assumptions")
  jsonlite::toJSON(json_payload(x, data = data, assumptions = assumptions),
                   auto_unbox = TRUE, pretty = pretty, digits = digits,
                   null = "null", na = "null", POSIXt = "ISO8601",
                   Date = "ISO8601")
}

#' @rdname as_json
#' @export
as_json.default <- function(x, ...) {
  cpt_abort("`as_json()` writes a ggcpt result; `x` is a ", class(x)[1], ".",
            class = "bad_argument")
}

# Internal: the schema as a list, before serialisation.
#' @noRd
json_payload <- function(x, data = TRUE, assumptions = TRUE) {
  v <- x$versions %||% list()
  table_rows <- function(tb) {
    if (is.null(tb) || !NROW(tb)) return(NULL)
    tb <- as.data.frame(tb, stringsAsFactors = FALSE)
    for (nm in names(tb)) {
      if (inherits(tb[[nm]], "Date")) tb[[nm]] <- format(tb[[nm]], "%Y-%m-%d")
      if (inherits(tb[[nm]], "POSIXt")) {
        tb[[nm]] <- format(tb[[nm]], "%Y-%m-%dT%H:%M:%S%z")
      }
      if (is.list(tb[[nm]])) tb[[nm]] <- NULL
    }
    tb
  }
  diag <- x$diagnostics %||% list()
  diag_out <- list(
    residual_dependence = diag$residual_dependence,
    na_omitted = if (!is.null(diag$na_omitted)) {
      list(positions = as.integer(diag$na_omitted$positions),
           n_observed = diag$na_omitted$n_observed)
    },
    expected_false_positives = diag$expected_false_positives)
  diag_out <- diag_out[!vapply(diag_out, is.null, logical(1))]
  constraints <- x$constraints
  if (!is.null(constraints$within)) {
    constraints$within <- as.data.frame(constraints$within)
  }
  idx <- x$index
  list(
    schema = "ggchangepoint.ggcpt",
    schema_version = cpt_json_schema_version(),
    method = scalar_chr(x$method),
    engine = v$engine %||% NA_character_,
    engine_version = v$engine_version %||% NA_character_,
    ggchangepoint_version = v$ggchangepoint %||%
      as.character(utils::packageVersion("ggchangepoint")),
    r_version = v$r %||% NA_character_,
    created = v$created %||% NA_character_,
    change_in = scalar_chr(x$change_in),
    family = x$family %||% NA_character_,
    penalty = list(type = scalar_chr(x$penalty$type %||% NA_character_),
                   value = suppressWarnings(as.numeric(
                     x$penalty$value %||% NA_real_))[1]),
    cp_convention = x$cp_convention %||% "left",
    n = nrow(x$data),
    index = if (!is.null(idx)) {
      list(class = class(idx)[1], label = x$index_label %||% "Index")
    },
    changepoints = table_rows(x$changepoints) %||% list(),
    segments = table_rows(x$segments) %||% list(),
    regions = table_rows(x$regions),
    coefficients = table_rows(x$coefficients),
    constraints = constraints,
    diagnostics = if (length(diag_out)) diag_out,
    assumptions = if (assumptions) {
      a <- tryCatch(cpt_assumptions(x), error = function(e) NULL)
      table_rows(a)
    },
    call = if (!is.null(x$call)) paste(deparse(x$call), collapse = " "),
    data = if (data) {
      d <- list(value = x$data$value)
      d$index <- if (!is.null(idx)) {
        if (inherits(idx, "Date")) format(idx, "%Y-%m-%d") else
          if (inherits(idx, "POSIXt")) format(idx, "%Y-%m-%dT%H:%M:%S%z") else
            idx
      } else {
        x$data$index
      }
      if (!is.null(x$data[["fitted"]])) d$fitted <- x$data$fitted
      d
    }
  )
}

#' Write a result to a file, and read it back
#'
#' \code{cpt_export()} writes a result where software in any language can
#' read it; \code{cpt_import()} rebuilds a working \code{ggcpt} from the
#' file. A saved \code{.rds} already reads in any R session without this
#' package (a \code{ggcpt} is a plain list of plain data), so these are for
#' leaving R, or for a format a person can open.
#'
#' @param fit A \code{ggcpt} object.
#' @param file Path to write (\code{cpt_export()}) or read
#'   (\code{cpt_import()}).
#' @param format \code{"json"} (the full result, see \code{\link{as_json}()})
#'   or \code{"csv"} (one row per observation: \code{index}, \code{value},
#'   the time index as \code{index_value} when there is one,
#'   \code{seg_id}, \code{fitted} and \code{is_changepoint}, which is
#'   everything needed to rebuild the segmentation but not its metadata).
#'   Defaults to the file's extension.
#' @param ... For \code{cpt_export()}, further arguments for
#'   \code{\link{as_json}()}.
#' @return \code{cpt_export()} returns \code{file}, invisibly.
#'   \code{cpt_import()} returns a \code{ggcpt} object; the engine's own
#'   \code{$fit} is not stored in either format, so it is \code{NULL}.
#' @export
#' @family result class
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(50), rnorm(50, 3)), method = "pelt")
#' path <- tempfile(fileext = ".json")
#' cpt_export(fit, path)
#' back <- cpt_import(path)
#' back$changepoints
#' unlink(path)
cpt_export <- function(fit, file, format = NULL, ...) {
  if (!is_ggcpt(fit)) {
    cpt_abort("`fit` must be a ggcpt object.", class = "bad_argument")
  }
  validate_report_path(file)
  format <- export_format(file, format)
  if (format == "json") {
    writeLines(as_json(fit, ...), file)
  } else {
    d <- augment.ggcpt(fit)
    keep <- intersect(c("index", "index_value", "value", "seg_id", ".fitted",
                        "is_changepoint"), names(d))
    out <- as.data.frame(d[, keep, drop = FALSE])
    names(out)[names(out) == ".fitted"] <- "fitted"
    utils::write.csv(out, file, row.names = FALSE)
  }
  invisible(file)
}

#' @rdname cpt_export
#' @param method,change_in For a CSV, which carries no metadata, the method
#'   and change type to record on the rebuilt result. Defaults to
#'   \code{"imported"} and \code{"mean"}.
#' @export
cpt_import <- function(file, format = NULL, method = "imported",
                       change_in = "mean") {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    cpt_abort("`file` must be the path of an existing file.",
              class = "bad_argument")
  }
  format <- export_format(file, format)
  if (format == "csv") {
    d <- utils::read.csv(file, stringsAsFactors = FALSE)
    if (!all(c("value", "is_changepoint") %in% names(d))) {
      cpt_abort("A CSV from cpt_export() has `value` and `is_changepoint` ",
                "columns; this one has ", paste(names(d), collapse = ", "),
                ".", class = "bad_argument")
    }
    cp <- which(as.logical(d$is_changepoint))
    idx <- if ("index_value" %in% names(d)) restore_index(d$index_value) else
      NULL
    return(as_ggcpt(cp, d$value, method = method, change_in = change_in,
                    index = idx))
  }
  need_pkg("jsonlite")
  j <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  if (!identical(j$schema, "ggchangepoint.ggcpt")) {
    cpt_abort("This JSON was not written by as_json() (no ",
              "`\"schema\": \"ggchangepoint.ggcpt\"` field).",
              class = "bad_argument")
  }
  major <- as.integer(strsplit(j$schema_version %||% "1.0.0", ".",
                               fixed = TRUE)[[1]][1])
  if (!identical(major, 1L)) {
    cpt_abort("Schema version ", j$schema_version, " is newer than this ",
              "release reads (1.x). Update ggchangepoint.",
              class = "unsupported")
  }
  if (is.null(j$data)) {
    cpt_abort("The JSON has no `data` (it was written with `data = ",
              "FALSE`), so there is no series to rebuild the result on.",
              class = "capability_absent")
  }
  value <- as.numeric(j$data$value)
  idx <- NULL
  if (!is.null(j$index)) {
    idx <- restore_index(j$data$index, j$index$class)
  }
  cps <- if (length(j$changepoints) && NROW(j$changepoints)) {
    as.data.frame(j$changepoints)
  } else {
    data.frame(cp = integer(0))
  }
  extra <- setdiff(names(cps), c("cp", "cp_value", "cp_index"))
  res <- as_ggcpt(as.integer(cps$cp), value, method = j$method,
                  change_in = j$change_in,
                  penalty = list(type = j$penalty$type %||% NA_character_,
                                 value = j$penalty$value %||% NA_real_),
                  index = idx,
                  fitted = if (!is.null(j$data$fitted)) {
                    as.numeric(j$data$fitted)
                  })
  for (col in extra) res$changepoints[[col]] <- cps[[col]]
  if (!is.null(j$family)) res$family <- j$family
  if (!is.null(j$coefficients) && NROW(j$coefficients)) {
    res$coefficients <- tibble::as_tibble(as.data.frame(j$coefficients))
  }
  res$versions <- list(engine = j$engine, engine_version = j$engine_version,
                       ggchangepoint = j$ggchangepoint_version,
                       r = j$r_version, created = j$created)
  res
}

# Internal: the format from `format` or the file's extension.
#' @noRd
export_format <- function(file, format) {
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- if (ext %in% c("json", "csv")) ext else {
      cpt_abort("Cannot tell the format from the extension of `", file,
                "`; pass `format = \"json\"` or `\"csv\"`.",
                class = "bad_argument")
    }
  }
  cpt_match_arg(format, c("json", "csv"), name = "format")
}

# Internal: turn an index written as text back into its class.
#' @noRd
restore_index <- function(v, cls = NULL) {
  if (is.null(v)) return(NULL)
  if (identical(cls, "Date") || (is.null(cls) && is.character(v) &&
                                 all(grepl("^\\d{4}-\\d{2}-\\d{2}$", v)))) {
    return(as.Date(v))
  }
  if (identical(cls, "POSIXct") || (is.null(cls) && is.character(v) &&
                                    all(grepl("^\\d{4}-\\d{2}-\\d{2}T", v)))) {
    return(as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
  }
  v
}
