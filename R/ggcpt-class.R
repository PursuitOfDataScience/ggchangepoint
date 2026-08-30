#' Create a ggcpt object
#'
#' @param changepoints A tibble with columns \code{cp} and \code{cp_value}.
#' @param segments A tibble with segment information: \code{seg_id}, \code{start},
#'   \code{end}, \code{n}, \code{param_estimate}.
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

#' Print a ggcpt object
#'
#' @param x A \code{ggcpt} object.
#' @param ... Additional arguments (ignored).
#' @export
print.ggcpt <- function(x, ...) {
  cat("ggcpt (changepoint detection result)\n")
  cat("  Method:         ", x$method,
      if (isTRUE(x$registered)) "  [user-registered]" else "", "\n", sep = "")
  cat("  Change in:      ", x$change_in, "\n")
  cat("  Changepoints found:", nrow(x$changepoints), "\n")
  cat("  CP convention:  ", x$cp_convention, "\n")
  cat("  Penalty:        ", format_penalty(x$penalty), "\n")
  cat("  Series length:  ", nrow(x$data), "\n")
  if (!is.null(x$index)) {
    cat("  Index:          ", format_index_range(x$index), "\n")
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
