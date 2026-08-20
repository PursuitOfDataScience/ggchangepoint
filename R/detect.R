#' Unified changepoint detection dispatcher
#'
#' Runs a changepoint detection method on a sequence and returns a tidy
#' \code{ggcpt} result object. This is the recommended entry point for most
#' users. See \code{\link{cpt_methods}()} for the full method table with
#' engines and capabilities.
#'
#' @param x A numeric vector for univariate methods, or a numeric
#'   matrix/data frame (rows are time points) for the multivariate methods
#'   (\code{"ecp"}, \code{"inspect"}, \code{"geomcp"}, \code{"ocd"},
#'   \code{"npmojo"}, \code{"kcp"}, \code{"fastcpd"}).
#' @param method Detection method. One of \code{"pelt"}, \code{"binseg"},
#'   \code{"segneigh"}, \code{"amoc"}, \code{"np"}, \code{"ecp"},
#'   \code{"fpop"}, \code{"wbs"}, \code{"wbs2"}, \code{"not"},
#'   \code{"mosum"}, \code{"idetect"}, \code{"tguh"}, \code{"smuce"},
#'   \code{"hsmuce"}, \code{"cpop"}, \code{"bcp"}, \code{"bocpd"},
#'   \code{"beast"}, \code{"cpm"}, \code{"kcp"}, \code{"npmojo"},
#'   \code{"decafs"}, \code{"sn"}, \code{"inspect"}, \code{"ocd"},
#'   \code{"geomcp"}, \code{"strucchange"}, \code{"segmented"},
#'   \code{"envcpt"}, or \code{"fastcpd"}. Methods whose engines live in
#'   \code{Suggests} prompt for installation when missing.
#' @param change_in What to detect change in. One of \code{"mean"},
#'   \code{"var"}, \code{"meanvar"}, \code{"slope"}, or
#'   \code{"distribution"}. Defaults to \code{"mean"}. The requested value
#'   is validated against the method's capabilities
#'   (see \code{cpt_methods()}); incompatible combinations error rather than
#'   silently running something else.
#' @param penalty Penalty type or value. Either a character string
#'   (\code{"MBIC"}, \code{"BIC"}, \code{"SIC"}, \code{"AIC"},
#'   \code{"Hannan-Quinn"}, \code{"None"}) or a numeric penalty value.
#'   Defaults to \code{"MBIC"}. See the penalty-semantics section of
#'   \code{\link{cpt_penalty}} for how each engine interprets it; methods
#'   that use thresholds, significance levels, or posteriors instead of
#'   penalties ignore this argument, and \code{"segneigh"} falls back to
#'   \code{"SIC"} because \pkg{changepoint} does not implement MBIC for
#'   Segment Neighbourhood. Note also that the default \code{"MBIC"} is
#'   resolved to a \emph{numeric} value for the numeric-penalty engines
#'   (\code{"fpop"}, \code{"cpop"}, \code{"decafs"}), and that value is
#'   stronger than those wrappers' own \code{2 * log(n)} default — 19.9
#'   against 11.8 at \eqn{n = 360} — so \code{cpt_detect(x, method =
#'   "decafs")} can report fewer changepoints than \code{decafs_wrapper(x)}
#'   on the same series. Pass \code{penalty} explicitly to make the two
#'   entry points agree.
#' @param ... Additional arguments passed to the specific wrapper (see the
#'   wrapper's help page for engine-specific options). Where an argument is
#'   also derived from \code{change_in} (\code{not}'s \code{contrast},
#'   \code{cpm}'s \code{cpm_type}, \code{kcp}'s \code{running_stat},
#'   \code{sn}'s \code{parameter}, \code{fastcpd}'s \code{family}), a value
#'   supplied here takes precedence. Check the spelling against the wrapper's
#'   help page: several engines end their own signature in \code{...}
#'   (\pkg{wbs}, \pkg{not}, \pkg{Rbeast}, \pkg{strucchange},
#'   \pkg{segmented}, \pkg{fastcpd}), so for those a misspelt argument name
#'   is silently discarded upstream and the engine quietly uses its default
#'   rather than reporting the typo.
#'
#' @section Scale sensitivity of the penalised change-in-mean engines:
#' \code{"pelt"}, \code{"binseg"}, \code{"segneigh"} and \code{"fpop"}
#' compare a penalty against a \emph{raw} segment cost when
#' \code{change_in = "mean"}: \pkg{changepoint}'s Normal cost assumes a
#' noise standard deviation of 1, and \pkg{fpop}'s \code{lambda} is an
#' absolute penalty on the residual sum of squares. Neither rescales the
#' data, so on a series whose noise is much wider than 1 the penalty is
#' effectively negligible and the segmentation shatters. On one true
#' changepoint with a jump of five standard deviations, \code{"pelt"}
#' returns 1 changepoint at \eqn{\sigma = 1}, 29 at \eqn{\sigma = 3} and
#' 138 at \eqn{\sigma = 10}. Three ways to avoid it, in order of
#' convenience:
#' \itemize{
#'   \item standardise the series first
#'     (\code{cpt_detect(scale(x)[, 1], method = "pelt")});
#'   \item pass a penalty on the data's own scale, for example
#'     \code{penalty = 2 * log(length(x)) * stats::var(diff(x)) / 2};
#'   \item use \code{change_in = "meanvar"}, which estimates a variance per
#'     segment and is unaffected.
#' }
#' The other engines are unaffected: SMUCE, WBS, WBS2, NOT, MOSUM,
#' Isolate-Detect, TGUH, CPOP, DeCAFS and the Bayesian, nonparametric and
#' multivariate methods all estimate or cancel the noise scale internally,
#' and return the same segmentation whatever the units.
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

  # `cpt_methods()` advertises four engines as "planned", but match.arg()
  # answered a request for one of them with the generic "'arg' should be one
  # of ..." list, which does not contain it -- so the table said the name
  # exists and the dispatcher said it does not. Name the situation instead.
  if (is.character(method) && length(method) == 1L &&
      method %in% planned_methods()$method) {
    pl <- planned_methods()
    row <- pl[pl$method == method, ]
    stop("`", method, "` is planned but not wired in this release: see the ",
         "\"planned\" rows of `cpt_methods()`. It will be built on the ",
         row$engine, " package, and is waiting on ",
         if (identical(row$target_release, "when on CRAN")) {
           paste0(row$engine, " being available from CRAN")
         } else {
           paste0("the ", row$target_release)
         }, ".", call. = FALSE)
  }
  method <- match.arg(method, cpt_methods_table()$method)
  change_in <- match.arg(change_in,
                         c("mean", "var", "meanvar", "slope", "distribution"))

  validate_data(x)
  validate_method_change_in(method, change_in)

  is_mv <- is.matrix(x) || is.data.frame(x)
  mv_methods <- c("ecp", "inspect", "geomcp", "ocd", "npmojo", "kcp",
                  "fastcpd")
  if (is_mv && ncol(as.matrix(x)) > 1 && !method %in% mv_methods) {
    stop("Method `", method, "` is univariate, but `x` has ",
         ncol(as.matrix(x)), " columns. Multivariate methods: ",
         paste(mv_methods, collapse = ", "), ".", call. = FALSE)
  }
  data_vec <- if (is_mv) as.numeric(as.matrix(x)[, 1]) else as.numeric(x)

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
    user_change_in <- change_in
    if (method == "np") {
      ci <- "np"
      # changepoint.np is a distribution-change detector; report that rather
      # than the (accepted) default request.
      user_change_in <- "distribution"
    }
    # The changepoint package does not implement the MBIC penalty for the
    # Segment Neighbourhood method; fall back to SIC (which it does support)
    # when the user keeps the default penalty.
    if (method == "segneigh" && identical(penalty, "MBIC")) {
      penalty <- "SIC"
    }
    res <- wrap_cpt_to_ggcpt(data_vec, ci, cp_method, method,
                             penalty = penalty,
                             user_change_in = user_change_in, ...)
  } else if (method == "ecp") {
    # Pass the original object (not flattened) for multivariate support
    res <- wrap_ecp_to_ggcpt(x, ...)
  } else {
    # Convert penalty to numeric for methods that need it
    pen_val <- resolve_numeric_penalty(penalty, n = length(data_vec))

    dots <- list(...)
    # Call a wrapper with the arguments this dispatcher derives (from
    # `change_in`, `penalty`, or the method name). A value the caller passed
    # through `...` wins over the derived one, so
    # `cpt_detect(x, method = "not", contrast = "pcwsLinMean")` overrides the
    # contrast instead of erroring with "matched by multiple actual
    # arguments". `x` is passed as a symbol so the wrapper's `match.call()`
    # stays compact rather than inlining the whole series.
    run <- function(wrapper, derived = list()) {
      derived <- derived[setdiff(names(derived), names(dots))]
      do.call(wrapper, c(list(x = quote(x)), derived, dots),
              envir = environment())
    }

    res <- switch(method,
      fpop     = run("fpop_wrapper", list(penalty = pen_val)),
      wbs      = run("wbs_wrapper"),
      wbs2     = run("wbs2_wrapper"),
      not      = run("not_wrapper",
                     list(contrast = not_contrast_for(change_in))),
      mosum    = run("mosum_wrapper"),
      idetect  = run("idetect_wrapper"),
      tguh     = run("tguh_wrapper"),
      smuce    = run("smuce_wrapper"),
      hsmuce   = run("smuce_wrapper", list(family = "hsmuce")),
      cpop     = run("cpop_wrapper", list(penalty = pen_val)),
      bcp      = run("bcp_wrapper"),
      bocpd    = run("bocpd_wrapper"),
      beast    = run("beast_wrapper"),
      cpm      = run("cpm_wrapper",
                     list(cpm_type = cpm_type_for(change_in))),
      kcp      = run("kcp_wrapper",
                     list(running_stat = kcp_stat_for(change_in))),
      npmojo   = run("npmojo_wrapper"),
      decafs   = run("decafs_wrapper", list(penalty = pen_val)),
      sn       = run("sn_wrapper", list(parameter = sn_param_for(change_in))),
      inspect  = run("inspect_wrapper"),
      ocd      = run("ocd_wrapper"),
      geomcp   = run("geomcp_wrapper"),
      strucchange = run("strucchange_wrapper"),
      segmented = run("segmented_wrapper"),
      envcpt   = run("envcpt_wrapper"),
      fastcpd  = run("fastcpd_wrapper",
                     list(family = fastcpd_family_for(change_in))),
      stop("Method '", method, "' is not wired to a wrapper. ",
           "This is an internal error; please report it.", call. = FALSE)
    )
  }

  runtime <- proc.time()[["elapsed"]] - t0
  res$runtime <- runtime
  # Record the call the user actually made. The wrappers each store their own
  # match.call(), which for a dispatched run is an internal, unexported helper
  # (`wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)`) that the reader
  # can neither recognise nor re-run.
  res$call <- match.call()
  res
}

# Internal: the methods this release names but does not wire. Kept in one
# place so `cpt_detect()` can recognise the name and say so, rather than
# denying it exists. `robseg` and `FOCuS` have never been on CRAN and `gfpop`
# was removed from it, so those three wait on the archive; `hdbinseg` is back
# (1.0.3), so `sbs` waits only on the wrapper.
#' @noRd
planned_methods <- function() {
  tibble::tribble(
    ~method,   ~change_in,                 ~engine,    ~status,   ~target_release,
    "gfpop",   "mean (graph-constrained)", "gfpop",    "planned", "when on CRAN",
    "robust",  "mean (robust loss)",       "robseg",   "planned", "when on CRAN",
    "focus",   "mean (online)",            "FOCuS",    "planned", "when on CRAN",
    "sbs",     "mean (high-dimensional)",  "hdbinseg", "planned", "next release"
  )
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
#'   \item{target_release}{What a planned method is waiting on: a release,
#'         or \code{"when on CRAN"} when the engine package itself is not
#'         available from CRAN. \code{NA} for methods that are already
#'         wired. Asking \code{cpt_detect()} for a planned method reports
#'         this rather than claiming the name does not exist.}
#' }
#' @export
#'
#' @examples
#' cpt_methods()
cpt_methods <- function() {
  methods <- rbind(cpt_methods_table(), planned_methods())

  # Installation status: TRUE/FALSE for wired engines, NA for planned ones.
  methods$installed <- ifelse(
    methods$status == "planned",
    NA,
    vapply(methods$engine, function(pkg) {
      if (pkg %in% c("changepoint", "changepoint.np", "ecp")) return(TRUE)  # Imports
      requireNamespace(pkg, quietly = TRUE)
    }, logical(1))
  )

  methods
}

# Internal: the single source of truth for wired methods.
#' @noRd
cpt_methods_table <- function() {
  tibble::tribble(
    ~method,       ~change_in,                            ~engine,              ~status,      ~target_release,
    "pelt",        "mean, var, meanvar",                  "changepoint",        "available",  NA_character_,
    "binseg",      "mean, var, meanvar",                  "changepoint",        "available",  NA_character_,
    "segneigh",    "mean, var, meanvar",                  "changepoint",        "available",  NA_character_,
    "amoc",        "mean, var, meanvar",                  "changepoint",        "available",  NA_character_,
    "np",          "distribution",                        "changepoint.np",     "available",  NA_character_,
    "ecp",         "distribution (multivariate)",         "ecp",                "available",  NA_character_,
    "fpop",        "mean",                                "fpop",               "available",  NA_character_,
    "wbs",         "mean",                                "wbs",                "available",  NA_character_,
    "wbs2",        "mean",                                "breakfast",          "available",  NA_character_,
    "not",         "mean, var, slope",                    "not",                "available",  NA_character_,
    "mosum",       "mean",                                "mosum",              "available",  NA_character_,
    "idetect",     "mean",                                "IDetect",            "available",  NA_character_,
    "tguh",        "mean",                                "breakfast",          "available",  NA_character_,
    "smuce",       "mean (with CIs)",                     "stepR",              "available",  NA_character_,
    "hsmuce",      "mean (heteroskedastic, with CIs)",    "stepR",              "available",  NA_character_,
    "cpop",        "slope",                               "cpop",               "available",  NA_character_,
    "bcp",         "mean (Bayesian)",                     "bcp",                "available",  NA_character_,
    "bocpd",       "mean (Bayesian online)",              "ocp",                "available",  NA_character_,
    "beast",       "mean/trend (Bayesian)",               "Rbeast",             "available",  NA_character_,
    "cpm",         "distribution (sequential)",           "cpm",                "available",  NA_character_,
    "kcp",         "running statistics (kernel)",         "kcpRS",              "available",  NA_character_,
    "npmojo",      "distribution (multivariate)",         "CptNonPar",          "available",  NA_character_,
    "decafs",      "mean (drift + AR noise)",             "DeCAFS",             "available",  NA_character_,
    "sn",          "mean, var, acf, correlation",         "SNSeg",              "available",  NA_character_,
    "inspect",     "mean (high-dimensional)",             "InspectChangepoint", "available",  NA_character_,
    "ocd",         "mean (high-dimensional, online)",     "ocd",                "available",  NA_character_,
    "geomcp",      "distribution (multivariate)",         "changepoint.geo",    "available",  NA_character_,
    "strucchange", "mean, regression (with CIs)",         "strucchange",        "available",  NA_character_,
    "segmented",   "slope (with CIs)",                    "segmented",          "available",  NA_character_,
    "envcpt",      "mean/trend vs autocorrelation",       "EnvCpt",             "available",  NA_character_,
    "fastcpd",     "mean, var, meanvar, AR/ARMA/GARCH",   "fastcpd",            "available",  NA_character_
  )
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

# Internal: change_in -> engine-specific argument translations
#' @noRd
not_contrast_for <- function(change_in) {
  switch(change_in,
    mean = "pcwsConstMean",
    var = "pcwsConstMeanVar",
    slope = "pcwsLinContMean",
    "pcwsConstMean"
  )
}

#' @noRd
sn_param_for <- function(change_in) {
  switch(change_in, mean = "mean", var = "variance", "mean")
}

#' @noRd
cpm_type_for <- function(change_in) {
  switch(change_in,
    mean = "Mann-Whitney",
    var = "Mood",
    distribution = "Kolmogorov-Smirnov",
    "Mann-Whitney"
  )
}

#' @noRd
kcp_stat_for <- function(change_in) {
  switch(change_in, mean = "mean", var = "var", "mean")
}

#' @noRd
fastcpd_family_for <- function(change_in) {
  switch(change_in, mean = "mean", var = "variance",
         meanvar = "meanvariance", "mean")
}

# Internal: what each method can detect (used by validate_method_change_in).
#' @noRd
method_change_in_support <- function() {
  list(
    pelt = c("mean", "var", "meanvar"),
    binseg = c("mean", "var", "meanvar"),
    segneigh = c("mean", "var", "meanvar"),
    amoc = c("mean", "var", "meanvar"),
    np = "distribution",
    ecp = "distribution",
    fpop = "mean",
    wbs = "mean",
    wbs2 = "mean",
    not = c("mean", "var", "slope"),
    mosum = "mean",
    idetect = "mean",
    tguh = "mean",
    smuce = "mean",
    hsmuce = "mean",
    cpop = "slope",
    bcp = "mean",
    bocpd = "mean",
    beast = "mean",
    cpm = c("distribution", "mean", "var"),
    kcp = c("mean", "var"),
    npmojo = "distribution",
    decafs = "mean",
    sn = c("mean", "var"),
    inspect = "mean",
    ocd = "mean",
    geomcp = "distribution",
    strucchange = "mean",
    segmented = "slope",
    envcpt = "mean",
    fastcpd = c("mean", "var", "meanvar")
  )
}

# Validate that change_in is compatible with the requested method, erroring
# with the legal set rather than silently running something else. Methods
# whose natural target differs from the default "mean" (np, ecp, cpop,
# segmented, ...) accept the default and route it to their native change
# type, so `cpt_detect(x, method = "np")` keeps working.
validate_method_change_in <- function(method, change_in) {
  support <- method_change_in_support()[[method]]
  if (is.null(support)) return(invisible(TRUE))

  # The default change_in = "mean" is accepted by every method: methods that
  # target something else (distribution, slope) treat a "mean" request as
  # their native change type, since a mean change is a special case of both.
  if (change_in == "mean") return(invisible(TRUE))

  if (!change_in %in% support) {
    stop("`change_in = \"", change_in, "\"` is not supported for method `",
         method, "`. Supported: ",
         paste(support, collapse = ", "), ". ",
         "See cpt_methods() for the full capability table.", call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: resolve a penalty argument to a numeric value (or NULL to let
# the wrapper use its own default).
#' @noRd
resolve_numeric_penalty <- function(penalty, n) {
  if (is.null(penalty)) return(NULL)
  if (is.numeric(penalty)) return(as.numeric(penalty))
  if (is.character(penalty)) {
    if (penalty %in% c("BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn", "None",
                       "sSIC")) {
      # k = 2 matches the Gaussian change-in-mean convention the
      # numeric-penalty engines use (fpop/cpop/DeCAFS default to
      # 2 * log(n) for BIC), so "BIC" means the same thing whether it is
      # resolved here or applied natively by the changepoint package.
      return(cpt_penalty(penalty, n = n, k = 2))
    }
    # Unknown string: fall back to the wrapper default rather than guessing.
    return(NULL)
  }
  NULL
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
                              penalty = "MBIC", user_change_in = NULL, ...) {
  data_vec <- as.numeric(x)
  tbl <- cpt_wrapper(data_vec, change_in = change_in, cp_method = cp_method,
                     penalty = penalty, ...)

  if (is.null(method_name)) method_name <- tolower(cp_method)
  # Report the user's vocabulary ("meanvar", "distribution"), not the
  # internal upstream one ("mean_var", "np").
  reported_change_in <- user_change_in %||% change_in

  ggcpt_build(
    data_vec, tbl$cp,
    method = method_name,
    change_in = reported_change_in,
    penalty = penalty_descriptor(penalty),
    fit = attr(tbl, "ggcpt_fit"),
    call = match.call()
  )
}

# Internal: wrap ecp_wrapper result into a ggcpt object
wrap_ecp_to_ggcpt <- function(x, ...) {
  # Preserve multivariate structure for ecp
  is_mv <- is.matrix(x) || is.data.frame(x)

  if (is_mv) {
    data_mat <- as_mv_matrix(x)
    data_vec <- as.numeric(data_mat[, 1])
  } else {
    data_vec <- as.numeric(x)
  }

  tbl <- ecp_wrapper(x, ...)

  # Normalize ecp convention (first index of right segment) to
  # changepoint convention (last index of left segment); ggcpt_build drops
  # out-of-range indices.
  cp_indices_left <- sort(tbl$cp) - 1L

  ggcpt_build(
    data_vec, cp_indices_left,
    method = "ecp",
    change_in = "distribution",
    penalty = list(type = "permutation", value = NA_real_),
    fit = NULL,
    call = match.call(),
    data_wide = if (is_mv) mv_data_wide(data_mat)
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
#' @param n Series length (at least 3 for the \eqn{\log n}-based
#'   penalties). Required for BIC, MBIC, AIC, Hannan-Quinn, sSIC.
#' @param k Number of parameters per changepoint (typically 2 for
#'   mean+variance, 1 for mean-only). Defaults to 1. The \code{"MBIC"}
#'   penalty additionally reads \code{k} as the number of changepoints being
#'   placed, in its \eqn{\log{n \choose k}} term.
#' @param value Numeric value for \code{Manual} type.
#' @param alpha Exponent of the strengthened SIC (\code{"sSIC"}) penalty
#'   \eqn{k (\log n)^\alpha}; must exceed 1. Defaults to \code{1.01}
#'   (Fryzlewicz, 2014).
#'
#' @section Penalty semantics across engines:
#' The same penalty name may be interpreted differently by different engines:
#' \itemize{
#'   \item \strong{changepoint-based methods} (PELT, BinSeg, SegNeigh, AMOC):
#'     accept character penalties (\code{"MBIC"}, \code{"BIC"}, \code{"AIC"},
#'     \code{"Hannan-Quinn"}, \code{"None"}) and pass them to the upstream
#'     \pkg{changepoint} package. A numeric penalty is translated to that
#'     package's \code{penalty = "Manual"} plus \code{pen.value}. The one
#'     exception is Segment Neighbourhood, for which \pkg{changepoint} does
#'     not implement MBIC: \code{cpt_detect(method = "segneigh")} and
#'     \code{cpt_wrapper(cp_method = "SegNeigh")} therefore fall back to
#'     \code{"SIC"} when the default penalty is left in place, so a segneigh
#'     result is not directly penalty-comparable with a PELT one. Pass
#'     \code{penalty} explicitly to pin it. For a change in \emph{mean}
#'     these engines also read the penalty on the data's own scale rather
#'     than a standardised one; see the scale-sensitivity section of
#'     \code{\link{cpt_detect}}.
#'   \item \strong{Functional-pruning methods} (\code{fpop}, \code{cpop},
#'     \code{decafs}): accept numeric penalties only. When a character penalty
#'     is supplied via \code{cpt_detect()}, it is resolved to a numeric value
#'     using \code{cpt_penalty()} before dispatch.
#'   \item \strong{Search-based methods} (WBS, WBS2, NOT, MOSUM, IDetect,
#'     TGUH): use internal model-selection criteria (e.g., sSIC, threshold)
#'     and generally \emph{ignore} the \code{penalty} argument. Specify
#'     thresholds via the wrapper's own arguments.
#'   \item \strong{Inference/Bayesian methods} (\code{smuce}, \code{bcp},
#'     \code{bocpd}, \code{beast}, \code{cpm}, \code{sn}): are tuned by a
#'     significance level, posterior-probability threshold, hazard, or
#'     average run length rather than a penalty; see each wrapper.
#'   \item \strong{\code{MBIC}} in \code{cpt_penalty()} is a BIC-type penalty
#'     that adds a combinatorial term for the number of ways \code{k}
#'     changepoints can be placed in \code{n} observations,
#'     \eqn{0.5(k+1)\log n + \log{n \choose k}}. It is deliberately stronger
#'     than \code{"BIC"}. It is \emph{not} the modified BIC of Zhang and
#'     Siegmund (2007), whose penalty
#'     \eqn{1.5 k \log n + 0.5 \sum_i \log(l_i / n)} depends on the segment
#'     lengths \eqn{l_i} and so cannot be expressed by a function of
#'     \code{n} and \code{k} alone. Use the character \code{"MBIC"} with
#'     \pkg{changepoint}-based methods to get the engine's native MBIC.
#' }
#'
#' @return A numeric penalty value.
#' @export
#'
#' @examples
#' cpt_penalty("BIC", n = 100)
#' cpt_penalty("AIC", n = 100)
#' cpt_penalty("Manual", value = 5)
cpt_penalty <- function(type, n = NULL, k = 1, value = NULL, alpha = 1.01) {
  type <- match.arg(type, c("None", "BIC", "SIC", "MBIC", "AIC",
                            "Hannan-Quinn", "sSIC", "Manual"))

  if (type == "None") return(0)
  if (type == "Manual") {
    if (is.null(value)) stop("`value` must be supplied for Manual type.", call. = FALSE)
    return(value)
  }

  if (is.null(n)) stop("`n` must be supplied for ", type, " penalty.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n)) {
    stop("`n` must be a single finite number.", call. = FALSE)
  }
  # Below n = 3 the log-based penalties stop being penalties: log(n) is 0 at
  # n = 1, and log(log(n)) is -Inf there and negative at n = 2, so the
  # "penalty" would reward extra changepoints instead of discouraging them.
  # (AIC = 2k does not involve n, so it is exempt.) Series that short are
  # rejected upstream by validate_data() anyway.
  if (type != "AIC" && n < 3) {
    stop("`n` must be at least 3 for the ", type,
         " penalty; log(n) and log(log(n)) stop being penalties below that.",
         call. = FALSE)
  }
  # MBIC's log C(n, k) term is -Inf once k exceeds n, which would silently
  # turn the penalty into -Inf rather than erroring.
  if (type == "MBIC" &&
      (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 0 || k > n)) {
    stop("`k` must be a single number between 0 and `n` for the MBIC ",
         "penalty (it counts the changepoints being placed among `n` ",
         "observations).", call. = FALSE)
  }
  # alpha <= 1 makes sSIC weaker than BIC, i.e. no longer the *strengthened*
  # SIC the argument names; the definition (Fryzlewicz 2014) requires
  # alpha > 1.
  if (type == "sSIC" &&
      (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
       alpha <= 1)) {
    stop("`alpha` must be a single number greater than 1 for the sSIC ",
         "penalty (got ", paste(format(alpha), collapse = ", "), ").",
         call. = FALSE)
  }

  switch(type,
    BIC            = k * log(n),
    SIC            = k * log(n),
    # BIC plus a combinatorial term for placing k changepoints among n
    # points. Stronger than BIC; not Zhang-Siegmund's segment-length mBIC
    # (see the penalty-semantics section).
    MBIC           = 0.5 * (k + 1) * log(n) + lchoose(n, k),
    AIC            = 2 * k,
    `Hannan-Quinn` = 2 * k * log(log(n)),
    # Strengthened SIC (Fryzlewicz 2014): k * (log n)^alpha with alpha > 1,
    # strictly stronger than BIC.
    sSIC           = k * log(n)^alpha
  )
}
