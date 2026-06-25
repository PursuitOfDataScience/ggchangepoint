#' Unified changepoint detection dispatcher
#'
#' Runs one or more changepoint detection methods on a sequence and returns
#' a tidy \code{ggcpt} result object. This is the recommended entry point
#' for most users.
#'
#' @param x A numeric vector (the data series).
#' @param method Detection method. See \code{\link{cpt_methods}()} for the
#'   complete table. Methods available in this release:
#'   \code{"pelt"}, \code{"binseg"}, \code{"segneigh"}, \code{"amoc"},
#'   \code{"np"}, \code{"ecp"}, \code{"fpop"}, \code{"wbs"}, \code{"wbs2"},
#'   \code{"not"}, \code{"mosum"}, \code{"idetect"}, \code{"tguh"}.
#'   Methods that ship with optional (\code{Suggests}) engine packages will
#'   prompt you to install them if missing. Planned methods — \code{"smuce"},
#'   \code{"hsmuce"}, \code{"kcp"}, \code{"cpm"}, \code{"robust"},
#'   \code{"decafs"}, \code{"sn"}, \code{"inspect"}, \code{"sbs"},
#'   \code{"bcp"}, \code{"bocpd"}, \code{"strucchange"}, \code{"segmented"} —
#'   are listed in \code{cpt_methods()} with their target release.
#' @param change_in What to detect change in. One of \code{"mean"},
#'   \code{"var"}, \code{"meanvar"}, \code{"slope"}, \code{"distribution"}.
#'   Defaults to \code{"mean"}.
#' @param penalty Penalty type or value. Either a character string
#'   (\code{"MBIC"}, \code{"BIC"}, \code{"AIC"}, \code{"Hannan-Quinn"})
#'   or a numeric penalty value. Defaults to \code{"MBIC"}. Numeric penalties
#'   are honoured by the functional-pruning method (\code{fpop});
#'   the \code{changepoint}-based methods expect one of the
#'   character options.
#' @param ... Additional arguments passed to the specific wrapper.
#'
#' @return A \code{ggcpt} object.
#' @export
#'
#' @examples
#' set.seed(2022)
#' x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
#' result <- cpt_detect(x, method = "pelt", change_in = "mean")
#' result
#' ggplot2::autoplot(result)
cpt_detect <- function(x,
                       method = "pelt",
                       change_in = "mean",
                       penalty = "MBIC",
                       ...) {

  # Only wire methods that are implemented. Planned methods are documented
  # in cpt_methods() but excluded from match.arg() so the user gets a clear
  # error rather than "not yet implemented".
  method <- match.arg(method, c(
    "pelt", "binseg", "segneigh", "amoc", "fpop", "wbs", "wbs2",
    "not", "mosum", "idetect", "tguh", "np", "ecp"
  ))

  change_in <- match.arg(change_in, c("mean", "var", "meanvar", "slope", "distribution"))

  validate_data(x)
  data_vec <- as.numeric(x)

  # Validate method × change_in compatibility before dispatching
  validate_method_change_in(method, change_in)

  t0 <- proc.time()[["elapsed"]]

  if (method %in% c("pelt", "binseg", "segneigh", "amoc", "np")) {
    ci <- change_in_mapping(change_in)
    cp_method <- switch(method,
      pelt    = "PELT",
      binseg  = "BinSeg",
      segneigh = "SegNeigh",
      amoc    = "AMOC",
      np      = "PELT"
    )
    if (method == "np") ci <- "np"
    # The changepoint package does not implement the MBIC penalty for the
    # Segment Neighbourhood method; fall back to SIC (which it does support)
    # when the user keeps the default penalty.
    if (method == "segneigh" && identical(penalty, "MBIC")) {
      penalty <- "SIC"
    }
    # Pass penalty string directly so upstream changepoint pkg handles it
    res <- wrap_cpt_to_ggcpt(data_vec, ci, cp_method, method, penalty = penalty, ...)
  } else if (method == "ecp") {
    # Pass original x (not flattened) for multivariate support
    res <- wrap_ecp_to_ggcpt(x, ...)
  } else {
    # Convert penalty to numeric for methods that need it
    pen_val <- if (is.numeric(penalty)) {
      penalty
    } else if (is.character(penalty) && penalty %in% c("BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn")) {
      cpt_penalty(penalty, n = length(data_vec))
    } else {
      NULL
    }

    res <- switch(method,
      fpop     = fpop_wrapper(data_vec, penalty = pen_val, ...),
      wbs      = wbs_wrapper(data_vec, ...),
      wbs2     = wbs2_wrapper(data_vec, ...),
      not      = not_wrapper(data_vec, ...),
      mosum    = mosum_wrapper(data_vec, ...),
      idetect  = idetect_wrapper(data_vec, ...),
      tguh     = tguh_wrapper(data_vec, ...),
      stop("Method '", method, "' not yet implemented in this release. ",
           "It will be added in a future version.", call. = FALSE)
    )
  }

  runtime <- proc.time()[["elapsed"]] - t0
  res$runtime <- runtime
  res
}

#' Introspect available changepoint detection methods
#'
#' Returns a tibble describing every method the package knows about — those
#' that are wired and those that are planned — along with their capabilities
#' and installation status. Useful for discovering what can be run and what
#' needs to be installed.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{method}{Method name as passed to \code{cpt_detect()}.}
#'   \item{change_in}{What types of change the method can detect.}
#'   \item{engine}{The upstream R package that implements the method.}
#'   \item{status}{\code{"available"} (wired in this release) or \code{"planned"} (future).}
#'   \item{installed}{\code{TRUE} if the engine package is installed,
#'         \code{FALSE} if it is a \code{Suggests} engine that is missing,
#'         \code{NA} for planned methods.}
#'   \item{target_release}{The release that plans to wire this method,
#'         or \code{NA} for currently available methods.}
#' }
#' @export
#'
#' @examples
#' cpt_methods()
cpt_methods <- function() {
  methods <- tibble::tribble(
    ~method,       ~change_in,                           ~engine,           ~status,      ~target_release,
    "pelt",        "mean, var, meanvar",                  "changepoint",     "available",  NA_character_,
    "binseg",      "mean, var, meanvar",                  "changepoint",     "available",  NA_character_,
    "segneigh",    "mean, var, meanvar",                  "changepoint",     "available",  NA_character_,
    "amoc",        "mean, var, meanvar",                  "changepoint",     "available",  NA_character_,
    "np",          "distribution",                        "changepoint.np",  "available",  NA_character_,
    "ecp",         "distribution",                        "ecp",             "available",  NA_character_,
    "fpop",        "mean",                                "fpop",            "available",  NA_character_,
    "wbs",         "mean",                                "wbs",             "available",  NA_character_,
    "wbs2",        "mean",                                "breakfast",       "available",  NA_character_,
    "not",         "mean, var, slope",                    "not",             "available",  NA_character_,
    "mosum",       "mean",                                "mosum",           "available",  NA_character_,
    "idetect",     "mean",                                "IDetect",         "available",  NA_character_,
    "tguh",        "mean",                                "breakfast",       "available",  NA_character_,
    "smuce",       "mean",                                "stepR",           "planned",    "0.3.0",
    "hsmuce",      "mean (heteroskedastic)",              "stepR",           "planned",    "0.3.0",
    "robust",      "mean (robust)",                       "robseg",          "planned",    "0.3.0",
    "decafs",      "mean (drift + AR)",                   "DeCAFS",          "planned",    "0.3.0",
    "sn",          "mean, var, acf, correlation",         "SNSeg",           "planned",    "0.3.0",
    "kcp",         "running statistics",                  "kcpRS",           "planned",    "0.4.0",
    "cpm",         "distribution (sequential)",           "cpm",             "planned",    "0.5.0",
    "inspect",     "mean (high-dimensional)",             "InspectChangepoint", "planned", "0.4.0",
    "sbs",         "mean (high-dimensional)",             "hdbinseg",        "planned",    "0.4.0",
    "bcp",         "mean (Bayesian)",                     "bcp",             "planned",    "0.4.0",
    "bocpd",       "mean (Bayesian online)",              "ocp",             "planned",    "0.4.0",
    "strucchange", "regression coefficients",             "strucchange",     "planned",    "0.4.0",
    "segmented",   "regression (piecewise linear)",       "segmented",       "planned",    "0.4.0"
  )

  # Check installation status for each engine
  methods$installed <- vapply(methods$engine, function(pkg) {
    if (pkg %in% c("changepoint", "changepoint.np", "ecp")) return(TRUE)  # Imports
    if (is.na(pkg)) return(NA)
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  methods
}

change_in_mapping <- function(change_in) {
  switch(change_in,
    mean         = "mean",
    var          = "var",
    meanvar      = "mean_var",
    slope        = "mean",
    distribution = "np"
  )
}

# Validate that change_in is compatible with the requested method.
# For search/pruning wrappers (fpop, wbs, wbs2, not, mosum, idetect, tguh)
# only mean is supported unless the method explicitly supports others.
validate_method_change_in <- function(method, change_in) {
  if (method %in% c("fpop", "wbs", "wbs2", "idetect", "tguh")) {
    if (change_in != "mean") {
      stop("`change_in = \"", change_in, "\"` is not supported for method `",
           method, "`. Only \"mean\" is supported.", call. = FALSE)
    }
  }
  if (method == "mosum" && change_in != "mean") {
    stop("`change_in = \"", change_in, "\"` is not supported for method `",
         "mosum`. Only \"mean\" is supported.", call. = FALSE)
  }
  if (method == "not") {
    valid <- c("mean", "var", "slope")
    if (!change_in %in% valid) {
      stop("`change_in = \"", change_in, "\"` is not supported for method `",
           "not`. Supported: ", paste(valid, collapse = ", "), ".", call. = FALSE)
    }
  }
  if (change_in == "slope") {
    stop("`change_in = \"slope\"` is not directly supported. ",
         "Use `not_wrapper(contrast = \"pcwsLinContMean\")` for slope changes.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: describe a penalty argument as a list(type, value)
penalty_descriptor <- function(penalty) {
  if (is.numeric(penalty)) {
    list(type = "Manual", value = as.numeric(penalty))
  } else {
    list(type = as.character(penalty), value = NA_real_)
  }
}

# Internal: wrap cpt_wrapper result into a ggcpt object
wrap_cpt_to_ggcpt <- function(x, change_in, cp_method, method_name = NULL,
                              penalty = "MBIC", ...) {
  data_vec <- as.numeric(x)
  tbl <- cpt_wrapper(data_vec, change_in = change_in, cp_method = cp_method,
                     penalty = penalty, ...)

  data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)

  if (is.null(method_name)) method_name <- tolower(cp_method)

  if (nrow(tbl) == 0) {
    res <- ggcpt_empty(data_vec, method_name)
    res$change_in <- change_in
    res$penalty <- penalty_descriptor(penalty)
    return(res)
  }

  cp_indices <- sort(tbl$cp)
  segments <- build_segments(data_vec, cp_indices)

  new_ggcpt(
    changepoints = tbl,
    segments = segments,
    data = data_tbl,
    method = method_name,
    change_in = change_in,
    penalty = penalty_descriptor(penalty),
    fit = NULL,
    call = match.call(),
    cp_convention = "left"
  )
}

# Internal: wrap ecp_wrapper result into a ggcpt object
wrap_ecp_to_ggcpt <- function(x, ...) {
  # Preserve multivariate structure for ecp
  is_mv <- is.matrix(x) || is.data.frame(x)

  if (is_mv) {
    data_mat <- as.matrix(x)
    data_vec <- as.numeric(data_mat[, 1])
    data_tbl <- tibble::tibble(index = seq_len(nrow(data_mat)),
                                value = data_vec)
  } else {
    data_vec <- as.numeric(x)
    data_tbl <- tibble::tibble(index = seq_along(data_vec), value = data_vec)
  }

  tbl <- ecp_wrapper(x, ...)

  if (nrow(tbl) == 0) {
    return(ggcpt_empty(data_vec, "ecp"))
  }

  cp_indices <- sort(tbl$cp)
  # Normalize ecp convention (first index of right segment) to
  # changepoint convention (last index of left segment)
  cp_indices_left <- cp_indices - 1
  cp_indices_left <- cp_indices_left[cp_indices_left > 0]

  if (length(cp_indices_left) == 0) {
    return(ggcpt_empty(data_vec, "ecp"))
  }

  cp_value_left <- data_vec[cp_indices_left]
  segments <- build_segments(data_vec, cp_indices_left)

  cp_tbl <- tibble::tibble(cp = cp_indices_left, cp_value = cp_value_left)

  new_ggcpt(
    changepoints = cp_tbl,
    segments = segments,
    data = data_tbl,
    method = "ecp",
    change_in = "distribution",
    penalty = list(type = "permutation", value = NA_real_),
    fit = NULL,
    call = match.call(),
    cp_convention = "left"
  )
}

#' Construct changepoint penalties
#'
#' Helper to construct standard penalty values for use with changepoint
#' detection methods. Returns a numeric penalty value.
#'
#' @param type Penalty type: \code{"None"}, \code{"BIC"} (or \code{"SIC"}),
#'   \code{"MBIC"}, \code{"AIC"}, \code{"Hannan-Quinn"}, \code{"sSIC"}, or
#'   \code{"Manual"}.
#' @param n Series length. Required for BIC, MBIC, AIC, Hannan-Quinn, sSIC.
#' @param k Number of parameters per changepoint (typically 2 for
#'   mean+variance, 1 for mean-only). Defaults to 1.
#' @param value Numeric value for \code{Manual} type.
#'
#' @section Penalty semantics across engines:
#' The same penalty name may be interpreted differently by different engines:
#' \itemize{
#'   \item \strong{changepoint-based methods} (PELT, BinSeg, SegNeigh, AMOC):
#'     accept character penalties (\code{"MBIC"}, \code{"BIC"}, \code{"AIC"},
#'     \code{"Hannan-Quinn"}, \code{"None"}) and pass them to the upstream
#'     \pkg{changepoint} package. These methods do \emph{not} accept raw numeric
#'     penalty values.
#'   \item \strong{Functional-pruning methods} (\code{fpop}): accept numeric
#'     penalties only. When a character penalty is supplied via
#'     \code{cpt_detect()}, it is resolved to a numeric value using
#'     \code{cpt_penalty()} before dispatch.
#'   \item \strong{Search-based methods} (WBS, WBS2, NOT, MOSUM, IDetect,
#'     TGUH): use internal model-selection criteria (e.g., sSIC, threshold)
#'     and generally \emph{ignore} the \code{penalty} argument. Specify
#'     thresholds via the wrapper's own arguments.
#'   \item \strong{\code{MBIC}} in \code{cpt_penalty()} uses the
#'     Zhang-Siegmund (2007) formula \eqn{0.5(k+1)\log n + \log{n \choose k}},
#'     which differs from the \pkg{changepoint} package's MBIC. Use the
#'     character \code{"MBIC"} with \pkg{changepoint}-based methods to get
#'     the engine's native MBIC.
#' }
#'
#' @return A numeric penalty value.
#' @export
#'
#' @examples
#' cpt_penalty("BIC", n = 100)
#' cpt_penalty("AIC", n = 100)
#' cpt_penalty("Manual", value = 5)
cpt_penalty <- function(type, n = NULL, k = 1, value = NULL) {
  type <- match.arg(type, c("None", "BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn", "sSIC", "Manual"))

  if (type == "None") return(0)
  if (type == "Manual") {
    if (is.null(value)) stop("`value` must be supplied for Manual type.", call. = FALSE)
    return(value)
  }

  if (is.null(n)) stop("`n` must be supplied for ", type, " penalty.", call. = FALSE)

  switch(type,
    BIC            = k * log(n),
    SIC            = k * log(n),
    MBIC           = 0.5 * (k + 1) * log(n) + lchoose(n, k),
    AIC            = 2 * k,
    `Hannan-Quinn` = 2 * k * log(log(n)),
    sSIC           = k * log(n) * 0.5
  )
}
