#' Create a ggcpt object
#'
#' The low-level constructor for the class every detector in this package
#' returns. It assembles the components into a \code{ggcpt} without checking
#' them, which is what makes it useful inside a wrapper and unsuitable as the
#' entry point for hand-built input -- use \code{\link{as_ggcpt}()} for that.
#'
#' @param changepoints A tibble with columns \code{cp} and \code{cp_value}.
#' @param segments A tibble with segment information: \code{seg_id}, \code{start},
#'   \code{end}, \code{n}, \code{param_estimate}. \code{param_estimate} is
#'   the segment \strong{mean} for every method in the package, including
#'   the variance, distribution and model-change detectors --- it is the
#'   segment level, not the parameter that changed. A \code{change_in =
#'   "var"} result therefore has a \code{param_estimate} column that may
#'   barely move; read the variance off the data with the segment bounds if
#'   that is the quantity you want. Everything derived from this column
#'   inherits the convention: \code{augment()}'s \code{.fitted}/
#'   \code{.resid}, \code{\link{cpt_gt}()}'s level columns,
#'   \code{summary()}, and the residual construction the bootstrap in
#'   \code{\link{cpt_confint}()} and \code{\link{cpt_stability}()} uses.
#' @param data A tibble with \code{index} and \code{value}.
#' @param method Character. The detection method used. A length-one string;
#'   defaults to \code{NA_character_}. (A zero-length value would make
#'   \code{glance()} return zero rows instead of its documented single row,
#'   because every other column would be recycled against it.)
#' @param change_in Character. What was detected (e.g. "mean", "var",
#'   "meanvar"). A length-one string; defaults to \code{NA_character_}.
#' @param penalty A list with \code{type} and \code{value}.
#' @param fit The raw upstream object. Every wrapper stores one except
#'   \code{"ecp"}: \code{ecp::e.agglo()} returns a cluster-progression
#'   matrix that is quadratic in the series length, so keeping it by
#'   default would make the result object explode on a long series. Call
#'   \code{ecp::e.divisive()} or \code{ecp::e.agglo()} directly if you
#'   need it. A few of the engines that \emph{are} kept are still large
#'   relative to the data — measured on a 2000-point series,
#'   \code{strucchange} costs about 135 MB (a triangular \eqn{O(n^2)} RSS
#'   matrix), \code{bfast} about 53 MB and \code{bocpd} about 31 MB, while
#'   every other engine stays under 4 MB. That is the engine's own object,
#'   not overhead this package adds, and it matters mainly when many results
#'   are held at once: \code{\link{cpt_batch}(keep_fit = FALSE)} drops
#'   them, or assign \code{res$fit <- NULL} yourself.
#' @param call The matched call.
#' @param cp_convention Character. The convention for reporting changepoint
#'   locations: \code{"left"} (last index of left segment, used by
#'   \code{changepoint}) or \code{"right"} (first index of right segment, used
#'   by \code{ecp}). Defaults to \code{"left"}.
#' @param runtime Numeric. Elapsed detection time in seconds, if measured.
#'   Defaults to \code{NA}.
#'
#' @section Optional slots:
#' Beyond the components in the signature, a \code{ggcpt} may carry any of
#' these, each present only when something supplied it and each safe to test
#' for with \code{is.null()}:
#' \describe{
#'   \item{\code{data_wide}}{index plus one column per coordinate, for a
#'     multivariate result.}
#'   \item{\code{index}, \code{index_label}}{a time index (one value per
#'     observation) and its axis label; see the \code{index} argument of
#'     \code{\link{cpt_detect}()}.}
#'   \item{\code{regions}}{a tibble of significance regions
#'     (\code{start}, \code{end}, ...) for the interval-valued methods —
#'     see \code{\link{nsp_wrapper}()} and \code{\link{geom_cpt_region}()}.}
#'   \item{\code{diagnostics}}{a named list of engine internals rendered by
#'     \code{\link{ggcpt_statistic}()},
#'     \code{\link{ggcpt_solution_path}()} and
#'     \code{\link{ggcpt_scale_space}()}.}
#'   \item{\code{registered}}{\code{TRUE} when the result came from a
#'     user-registered detector rather than a wired engine.}
#' }
#' \code{\link{as_ggcpt}()} is the validating way to build one of these from
#' the outside; this constructor does not check its arguments.
#'
#' @return An object of class \code{ggcpt}.
#' @export
#' @family result class
#' @examples
#' set.seed(2026)
#' new_ggcpt(
#'   changepoints = tibble::tibble(cp = 50L, cp_value = 0.1),
#'   data = tibble::tibble(index = 1:100,
#'                         value = c(rnorm(50), rnorm(50, 4))),
#'   method = "manual", change_in = "mean")
new_ggcpt <- function(changepoints = tibble::tibble(cp = integer(), cp_value = numeric()),
                       segments = tibble::tibble(seg_id = integer(),
                                                  start = integer(),
                                                  end = integer(),
                                                  n = integer(),
                                                  param_estimate = numeric()),
                       data = tibble::tibble(index = integer(), value = numeric()),
                       method = NA_character_,
                       change_in = NA_character_,
                       penalty = list(type = NA_character_, value = NA_real_),
                       fit = NULL,
                       call = NULL,
                       cp_convention = "left",
                       runtime = NA_real_) {
  structure(
    list(
      changepoints = changepoints,
      segments = segments,
      data = data,
      method = method,
      change_in = change_in,
      penalty = penalty,
      fit = fit,
      call = call,
      cp_convention = cp_convention,
      runtime = runtime
    ),
    class = "ggcpt"
  )
}

# Internal: render a penalty descriptor for printing. Numeric values are
# rounded to something readable rather than shown at full double precision,
# and a penalty that has no numeric value (a name, threshold type, or model
# selector) prints as just its type instead of trailing " = NA".
#' @noRd
format_penalty <- function(penalty) {
  if (!is.list(penalty)) return(as.character(penalty))
  val <- penalty$value
  if (length(val) != 1 || !is.numeric(val) || !is.finite(val)) {
    return(as.character(penalty$type))
  }
  paste0(penalty$type, " = ", format(val, digits = 5))
}

#' Test if an object is a ggcpt object
#'
#' A class check, useful when a function accepts either a detection result or
#' the raw series. It tests for \code{ggcpt} in the class vector, so a
#' genuine \code{ggcpt} subclass --- \code{ggcpt_consensus} is the one ---
#' returns \code{TRUE}. The other \code{ggcpt_*} classes in the package
#' (\code{ggcpt_batch}, \code{ggcpt_benchmark}, \code{ggcpt_monitor},
#' \code{ggcpt_selection} and the rest) are \emph{not} subclasses of
#' \code{ggcpt} --- most are tibble subclasses --- and return
#' \code{FALSE}.
#'
#' @param x An object to test.
#' @return \code{TRUE} if \code{x} inherits from \code{ggcpt}.
#' @export
#' @family result class
#' @examples
#' set.seed(2026)
#' fit <- cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt")
#' is_ggcpt(fit)
#' is_ggcpt(fit$changepoints)
is_ggcpt <- function(x) {
  inherits(x, "ggcpt")
}

# Internal: one "  Label:  value" line of a print() header, with the values
# aligned on a single column and no trailing space.
#
# Written once because four print methods padded their labels by hand and
# all four had drifted. print.ggcpt() put its values in three different
# columns (19, 20 and 22) because `Changepoints found:` is longer than the
# pad the others use; print.ggcpt_delay() had five of six right and
# `Average run length:` one column out; print.ggcpt_path() left
# `Distinct segmentations:` unpadded entirely. And every line built with
# cat()'s default separator ended in a space, because the separator lands
# between the value and the "\n" -- 5 of 13 lines of a ggcpt and 7 of a
# summary.
#
# `width` is the widest label plus its colon, so a label that outgrows it
# extends the line rather than being truncated.
#' @noRd
cat_field <- function(label, value, width = 19L) {
  cat("  ", formatC(paste0(label, ":"), width = width, flag = "-"), " ",
      paste0(value, collapse = ""), "\n", sep = "")
}

#' Print a ggcpt object
#'
#' A compact header -- method, what changed, how many changepoints, the
#' convention their locations follow, the penalty and the series length --
#' followed by the first ten changepoints. For the segment table and the
#' fitted parameters use \code{\link[base]{summary}()}; for the changepoints
#' as data use \code{\link{tidy}()}.
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly. Called for the side effect of printing.
#' @export
#' @family result class
#' @seealso \code{\link{summary.ggcpt}()}, \code{\link{tidy.ggcpt}()}.
#' @examples
#' set.seed(2026)
#' print(cpt_detect(c(rnorm(40), rnorm(40, 4)), method = "pelt"))
print.ggcpt <- function(x, ...) {
  cat("ggcpt (changepoint detection result)\n")
  cat_field("Method", paste0(x$method,
            if (isTRUE(x$registered)) "  [user-registered]" else ""))
  cat_field("Change in", x$change_in)
  cat_field("Changepoints found", nrow(x$changepoints))
  cat_field("CP convention", x$cp_convention)
  cat_field("Penalty", format_penalty(x$penalty))
  cat_field("Series length", nrow(x$data))
  if (!is.null(x$index)) {
    cat_field("Index", format_index_range(x$index))
  }
  if (nrow(x$changepoints) > 0) {
    cat("\nChangepoints:\n")
    print(x$changepoints, n = 10)
  } else {
    cat("\nNo changepoints detected.\n")
  }
  if (!is.null(x$regions)) {
    cat("\nSignificance regions (each contains at least one changepoint",
        if (!is.null(x$region_level)) {
          paste0(" at global level ", format(x$region_level))
        } else {
          ""
        }, "):\n", sep = "")
    print(x$regions, n = 10)
    if (isTRUE(x$cp_from_regions)) {
      cat("\nNote: `cp` is the midpoint of each region, not a point ",
          "estimate.\n      The region is the inferential object; see ",
          "?nsp_wrapper.\n", sep = "")
    }
  }
  if (isTRUE(x$registered)) {
    cat("\nThis result came from a user-registered detector; the package ",
        "validated\nits shape, not its statistics. See ",
        "?cpt_register_method.\n", sep = "")
  }
  invisible(x)
}

# Internal: a compact "first to last" description of a time index for
# print(). Dates and datetimes format themselves sensibly; numeric indices
# are rounded rather than shown at full precision.
#' @noRd
format_index_range <- function(idx) {
  if (length(idx) == 0) return("(empty)")
  fmt <- function(v) {
    if (inherits(v, c("Date", "POSIXct", "POSIXt"))) return(format(v))
    if (is.numeric(v)) return(format(v, digits = 6))
    as.character(v)
  }
  paste0(fmt(idx[1]), " to ", fmt(idx[length(idx)]))
}
