# ---------------------------------------------------------------------------
# The engine registry: one declarative source of truth for every method.
#
# Before 0.5.0 the same facts were spelled out three times -- in
# `cpt_methods_table()` (the user-visible table), in
# `method_change_in_support()` (the capability check), and in `cpt_detect()`'s
# switch (the dispatch). Adding an engine meant editing all three and hoping
# they agreed. They are now derived from `builtin_registry()`, which also
# carries the capability flags the 0.5.0 diagnostics need (does the engine
# supply a confidence interval? a fitted signal? a solution path?) and is the
# *same* structure `cpt_register_method()` appends to, so a user-registered
# detector travels the identical code path as a built-in one.
# ---------------------------------------------------------------------------

# Internal: the built-in engine table.
#
# Columns:
#   method        name passed to cpt_detect()
#   change_in     human-readable capability string (shown by cpt_methods())
#   engine        upstream package
#   supports      character vector of legal `change_in` values
#   wrapper       the exported wrapper function's name (the arguments the
#                 dispatcher derives from `change_in` and `penalty` live in
#                 `derived_args_for()` below, so the table itself stays a
#                 plain data frame)
#   multivariate  accepts a matrix/data frame with more than one column
#   univariate    is a univariate method -- appropriate on a single series,
#                 and what cpt_recommend(dimension = "univariate") offers.
#
#                 The stronger reading, "errors on a single column", is not
#                 quite what FALSE means, and measuring it is how that got
#                 pinned down. Of the fourteen FALSE engines, nine do error
#                 on a vector and now all nine name the requirement when
#                 they do (ocd, geomcp, hdreg, fmean, fcov, fabisearch
#                 always did; hdcov, network, var and kwc used to answer
#                 "non-conformable arrays", "'x' must be an array of at
#                 least two dimensions", "incorrect number of dimensions"
#                 and "dim(X) must have a positive length"). The other
#                 five -- npmojo, inspect, esac, pilliat -- *run* on a
#                 vector and even recover the changepoint, but they are
#                 high-dimensional procedures whose whole point is
#                 aggregating evidence across coordinates, so recommending
#                 them for one series would be poor advice. FALSE records
#                 the intent, not a promise that the call will fail.
#   online        a sequential/online detector (batch-replayed offline)
#   ci            reports interval uncertainty for a changepoint location:
#                 ci_lower/ci_upper on the changepoints tibble for most, or
#                 a significance region for `nsp` (region_start/region_end
#                 plus the $regions slot). Both are what
#                 cpt_recommend(need_uncertainty = TRUE) is asking for,
#                 which is why nsp is TRUE here despite carrying no
#                 ci_lower column.
#   fitted        populates a length-n fitted signal on $data
#   posterior     the method is Bayesian and quantifies the location with a
#                 posterior. Only `bcp` and `beast` expose the per-location
#                 profile that ggcpt_posterior() draws; `bocpd`'s posterior
#                 is over run lengths (ggcpt_runlength()) and `mcp`'s is
#                 summarised as ci_lower/ci_upper. Verified by sweeping
#                 every method against the accessors.
#   statistic     exposes a detector statistic as a function of location
#   path          exposes a solution path (candidate splits, in order)
#   scale_space   has a bandwidth/scale parameter worth sweeping
#' @noRd
builtin_registry <- function() {
  reg <- tibble::tribble(
    ~method,       ~change_in,                            ~engine,              ~supports,                 ~wrapper,               ~multivariate, ~univariate, ~online, ~ci,   ~fitted, ~posterior, ~statistic, ~path, ~scale_space,
    "pelt",        "mean, var, meanvar",                  "changepoint",        "mean,var,meanvar",       "cpt_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "binseg",      "mean, var, meanvar",                  "changepoint",        "mean,var,meanvar",       "cpt_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,
    "segneigh",    "mean, var, meanvar",                  "changepoint",        "mean,var,meanvar",       "cpt_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,
    "amoc",        "mean, var, meanvar",                  "changepoint",        "mean,var,meanvar",       "cpt_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,
    "np",          "distribution",                        "changepoint.np",     "distribution",           "cpt_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "ecp",         "distribution (multivariate)",         "ecp",                "distribution",           "ecp_wrapper",          TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "fpop",        "mean",                                "fpop",               "mean",                   "fpop_wrapper",         FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "wbs",         "mean",                                "wbs",                "mean",                   "wbs_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE,
    "wbs2",        "mean",                                "breakfast",          "mean",                   "wbs2_wrapper",         FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,
    "not",         "mean, var, meanvar, slope",           "not",                "mean,var,meanvar,slope", "not_wrapper",          FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE,
    "mosum",       "mean",                                "mosum",              "mean",                   "mosum_wrapper",        FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE,
    "idetect",     "mean",                                "IDetect",            "mean",                   "idetect_wrapper",      FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "tguh",        "mean",                                "breakfast",          "mean",                   "tguh_wrapper",         FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE,
    "smuce",       "mean (with CIs)",                     "stepR",              "mean",                   "smuce_wrapper",        FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE,
    "hsmuce",      "mean (heteroskedastic, with CIs)",    "stepR",              "mean",                   "smuce_wrapper",        FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE,
    "cpop",        "slope",                               "cpop",               "slope",                  "cpop_wrapper",         FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE,
    "bcp",         "mean (Bayesian)",                     "bcp",                "mean",                   "bcp_wrapper",          FALSE, TRUE,  FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
    "bocpd",       "mean (Bayesian online)",              "ocp",                "mean",                   "bocpd_wrapper",        FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
    "beast",       "mean/trend (Bayesian)",               "Rbeast",             "mean",                   "beast_wrapper",        FALSE, TRUE,  FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
    "cpm",         "distribution (sequential)",           "cpm",                "distribution,mean,var",  "cpm_wrapper",          FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "kcp",         "running statistics (kernel)",         "kcpRS",              "mean,var",               "kcp_wrapper",          TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "npmojo",      "distribution (multivariate)",         "CptNonPar",          "distribution",           "npmojo_wrapper",       TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    "decafs",      "mean (drift + AR noise)",             "DeCAFS",             "mean",                   "decafs_wrapper",       FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE,
    "sn",          "mean, var, acf, correlation",         "SNSeg",              "mean,var",               "sn_wrapper",           FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "inspect",     "mean (high-dimensional)",             "InspectChangepoint", "mean",                   "inspect_wrapper",      TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "ocd",         "mean (high-dimensional, online)",     "ocd",                "mean",                   "ocd_wrapper",          TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "geomcp",      "distribution (multivariate)",         "changepoint.geo",    "distribution",           "geomcp_wrapper",       TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "strucchange", "mean, regression (with CIs)",         "strucchange",        "mean",                   "strucchange_wrapper",  FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
    "segmented",   "slope (with CIs)",                    "segmented",          "slope",                  "segmented_wrapper",    FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE,
    "envcpt",      "mean/trend vs autocorrelation",       "EnvCpt",             "mean",                   "envcpt_wrapper",       FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "fastcpd",     "mean, var, meanvar, AR/ARMA/GARCH",   "fastcpd",            "mean,var,meanvar",       "fastcpd_wrapper",      TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,

    # ---- 0.5.0 engine wave -------------------------------------------------
    "nsp",         "mean (significance regions)",         "nsp",                "mean,slope",             "nsp_wrapper",          FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE,
    "mcp",         "mean, slope, var (Bayesian formula)", "mcp",                "mean,slope,var",         "mcp_wrapper",          FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE,
    "esac",        "mean (high-dimensional, sparse)",     "HDCD",               "mean",                   "esac_wrapper",         TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "pilliat",     "mean (high-dimensional, sparse)",     "HDCD",               "mean",                   "pilliat_wrapper",      TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "hdcov",       "covariance (high-dimensional)",       "changepoints",       "covariance",             "hdcov_wrapper",        TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "network",     "network structure",                   "changepoints",       "network",                "network_wrapper",      TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "var",         "VAR(1) transition matrix",            "changepoints",       "regression",             "var_wrapper",          TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "hdreg",       "high-dimensional regression",         "changepoints",       "regression",             "hdreg_wrapper",        TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "fmean",       "functional mean",                     "fChange",            "mean",                   "fmean_wrapper",        TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "fcov",        "functional covariance",               "fChange",            "covariance",             "fcov_wrapper",         TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "kwc",         "covariance (robust, functional)",     "KWCChangepoint",     "covariance,distribution", "kwc_wrapper",          TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "fabisearch",  "network structure (NMF)",             "fabisearch",         "network",                "fabisearch_wrapper",   TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "wbsts",       "mean (nonstationary)",                "wbsts",              "mean",                   "wbsts_wrapper",        FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE,
    "bfast",       "trend and seasonality",               "bfast",              "mean,slope,seasonality", "bfast_wrapper",        FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE,
    "pettitt",     "mean (single change, rank test)",     "trend",              "mean",                   "trend_wrapper",        FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,
    "buishand",    "mean (single change, range test)",    "trend",              "mean",                   "trend_wrapper",        FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,
    "snht",        "mean (standard normal homogeneity)",  "trend",              "mean",                   "trend_wrapper",        FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE,
    "taylor",      "mean (Taylor's analyzer)",            "ChangePointTaylor",  "mean",                   "taylor_wrapper",       FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
    "binsegrcpp",  "mean, meanvar (fast BinSeg)",         "binsegRcpp",         "mean,meanvar",           "binsegrcpp_wrapper",   FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  )
  # `supports` is stored as a comma-separated string so the table stays a
  # plain, printable, diffable tibble; a tribble list-column would deparse
  # the expression into a string anyway.
  reg$supports <- strsplit(reg$supports, ",", fixed = TRUE)
  reg$status <- "available"
  reg$target_release <- NA_character_
  reg
}

# Internal: which dispatcher-derived arguments a method takes. Kept as a
# lookup rather than a registry column so the registry stays a plain data
# table (printable, testable, diffable).
#' @noRd
derived_args_for <- function(method, change_in, pen_val) {
  switch(method,
    fpop      = list(penalty = pen_val),
    not       = list(contrast = not_contrast_for(change_in)),
    hsmuce    = list(family = "hsmuce"),
    cpop      = list(penalty = pen_val),
    cpm       = list(cpm_type = cpm_type_for(change_in)),
    kcp       = list(running_stat = kcp_stat_for(change_in)),
    decafs    = list(penalty = pen_val),
    sn        = list(parameter = sn_param_for(change_in)),
    fastcpd   = list(family = fastcpd_family_for(change_in)),
    mcp       = list(change_in = if (change_in %in% c("mean", "slope",
                                                      "var")) {
                       change_in
                     } else {
                       "mean"
                     }),
    nsp       = list(change_in = if (identical(change_in, "slope")) {
                       "slope"
                     } else {
                       "mean"
                     }),
    bfast     = list(change_in = if (change_in %in% c("mean", "slope",
                                                      "seasonality")) {
                       change_in
                     } else {
                       "mean"
                     }),
    # kwc detects changes in the covariance/distribution; a dispatcher
    # default of "mean" (which every method accepts) must route to its
    # native target rather than being handed through and rejected.
    kwc       = list(change_in = if (change_in %in% c("covariance",
                                                      "distribution")) {
                       change_in
                     } else {
                       "covariance"
                     }),
    pettitt   = list(test = "pettitt"),
    buishand  = list(test = "buishand"),
    snht      = list(test = "snht"),
    binsegrcpp = list(change_in = if (change_in %in% c("mean", "meanvar")) {
                        change_in
                      } else {
                        "mean"
                      }),
    list()
  )
}

# ---------------------------------------------------------------------------
# User-registered methods (§11 of the roadmap)
# ---------------------------------------------------------------------------

# Session-scoped state. Deliberately not persisted: a registration is a
# property of the running session, and writing it to disk would make a script
# behave differently depending on what some earlier script had run.
#' @noRd
.cpt_registry <- new.env(parent = emptyenv())

#' Register an external changepoint detector
#'
#' Teaches \code{\link{cpt_detect}()} about a detector this package does not
#' (and often cannot) depend on: an engine that is not on CRAN, a Python
#' detector reached through \pkg{reticulate}, a deep-learning model, a
#' proprietary in-house method, or a hand-curated set of changepoints. The
#' registered method then works with everything built on the \code{ggcpt}
#' contract — \code{autoplot()}, the geoms, \code{tidy()}/\code{glance()}/
#' \code{augment()}, \code{cpt_metrics()}, \code{ggcpt_compare()},
#' \code{cpt_stability()}, \code{cpt_consensus()}, \code{cpt_benchmark()}
#' and \code{cpt_report()}.
#'
#' @param name Method name, as it will be passed to \code{cpt_detect()}. Must
#'   not clash with a built-in method.
#' @param fn A function called as \code{fn(x, ...)}. It may return either a
#'   \code{ggcpt} object (built with \code{\link{as_ggcpt}()}, say) or a bare
#'   vector of changepoint indices, which is coerced with
#'   \code{\link{as_ggcpt}()}.
#'
#'   A returned \code{ggcpt} may carry a \code{$diagnostics} list, which is
#'   what makes \code{\link{cpt_statistic}()} and
#'   \code{\link{cpt_solution_path}()} work for the method. Two
#'   conventions apply to it. \code{diagnostics$solution_path} needs a
#'   \code{cp} column and may add \code{contrast}, \code{start} and
#'   \code{end}; it goes through the same filtering and step-numbering as a
#'   built-in path, so out-of-range candidates are dropped and
#'   \code{selected} is computed rather than trusted.
#'   \code{diagnostics$statistic} (a numeric vector, or a list with
#'   \code{statistic}, \code{label} and \code{threshold}) is padded with
#'   \code{NA} to the length of the series when it is shorter, and the pad
#'   is \strong{centred} --- a moving-window statistic is trimmed at both
#'   ends, so a left-aligned pad would shift every value by the bandwidth.
#'   Supply a full-length vector if that is not the alignment you want.
#' @param change_in Character vector of \code{change_in} values the detector
#'   supports. Defaults to \code{"mean"}.
#' @param engine Name of the package or system supplying the detector, for
#'   display in \code{cpt_methods()}. Defaults to \code{"user"}.
#' @param citation Optional citation string returned by
#'   \code{\link{cpt_cite}()}. When \code{NULL}, \code{cpt_cite()} says
#'   plainly that the registration supplied none rather than inventing one.
#' @param capabilities Optional named list of capability flags overriding the
#'   defaults (all \code{FALSE}): \code{multivariate}, \code{online},
#'   \code{ci}, \code{fitted}, \code{posterior}, \code{statistic},
#'   \code{path}, \code{scale_space}.
#' @param cp_convention \code{"left"} (the changepoint is the last index of
#'   the left segment, this package's convention) or \code{"right"}.
#'   Used when \code{fn} returns bare indices.
#' @param overwrite Replace an existing registration of the same name?
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly, the method name.
#'
#' @section Registered methods are labelled, not endorsed:
#' A registered method is visibly user-supplied: \code{cpt_methods()} gives it
#' \code{status = "registered"}, \code{print()} on its results marks it, and
#' \code{cpt_cite()} reports the citation you supplied or states that none
#' was given. The package validates the \emph{shape} of what your function
#' returns (through \code{\link{as_ggcpt}()}, which runs the same contract
#' checks as every built-in wrapper); it does not and cannot validate the
#' method.
#'
#' @seealso \code{\link{as_ggcpt}()} to turn changepoints into a result
#'   object without registering a method;
#'   \code{\link{cpt_unregister_method}()};
#'   \code{\link{cpt_registered_methods}()}.
#' @export
#' @examples
#' # A deliberately trivial detector: split at the largest jump.
#' cpt_register_method(
#'   "biggest_jump",
#'   fn = function(x, ...) which.max(abs(diff(x))),
#'   change_in = "mean",
#'   engine = "example",
#'   citation = "No citation supplied (illustration only)."
#' )
#' set.seed(1)
#' fit <- cpt_detect(c(rnorm(50), rnorm(50, 5)), method = "biggest_jump")
#' fit
#' cpt_unregister_method("biggest_jump")
cpt_register_method <- function(name, fn, change_in = "mean",
                                engine = "user", citation = NULL,
                                capabilities = list(),
                                cp_convention = c("left", "right"),
                                overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function called as fn(x, ...).", call. = FALSE)
  }
  validate_flag(overwrite, "overwrite")
  cp_convention <- match.arg(cp_convention)

  if (name %in% builtin_registry()$method) {
    stop("`", name, "` is a built-in method and cannot be overridden. ",
         "Pick another name.", call. = FALSE)
  }
  if (name %in% planned_methods()$method) {
    stop("`", name, "` is the name this package reserves for a planned ",
         "built-in method (see the \"planned\" rows of `cpt_methods()`). ",
         "Pick another name so the two cannot be confused.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && exists(name, envir = .cpt_registry,
                                   inherits = FALSE)) {
    stop("`", name, "` is already registered. Pass `overwrite = TRUE` to ",
         "replace it, or call cpt_unregister_method(\"", name, "\") first.",
         call. = FALSE)
  }

  change_in <- as.character(change_in)
  unknown <- setdiff(change_in, cpt_change_in_levels())
  if (length(unknown) > 0) {
    stop("Unknown `change_in` value(s): ", paste(unknown, collapse = ", "),
         ". Known values: ", paste(cpt_change_in_levels(), collapse = ", "),
         ".", call. = FALSE)
  }

  caps <- utils::modifyList(default_capabilities(), as.list(capabilities))
  bad_caps <- setdiff(names(caps), names(default_capabilities()))
  if (length(bad_caps) > 0) {
    stop("Unknown capability flag(s): ", paste(bad_caps, collapse = ", "),
         ". Known flags: ",
         paste(names(default_capabilities()), collapse = ", "), ".",
         call. = FALSE)
  }
  # The NAMES were checked and the VALUES were not, and every flag is read
  # downstream through isTRUE() -- so `capabilities = list(ci = 1)`
  # registered a method reporting `ci = FALSE`, and cpt_confint(fit, method
  # = "native") then told the user their own engine supplies no intervals.
  not_flag <- names(caps)[!vapply(caps, function(v) {
    is.logical(v) && length(v) == 1L && !is.na(v)
  }, logical(1))]
  if (length(not_flag) > 0) {
    stop("Capability flag(s) ", paste0("`", not_flag, "`", collapse = ", "),
         " must be TRUE or FALSE: they are read as flags, so a 1 or a ",
         "\"yes\" registers as FALSE and the capability silently ",
         "disappears.", call. = FALSE)
  }
  # `citation` is put in a tibble column by cpt_cite() and cat()ed, so a
  # list or a function reached the user as `argument 1 (type 'list') cannot
  # be handled by 'cat'`.
  if (!is.null(citation) &&
      !(is.character(citation) && length(citation) == 1L)) {
    stop("`citation` must be NULL or a single string -- cpt_cite() prints ",
         "it verbatim. For a BibTeX entry or a citation object, pass ",
         "format(citation) or a one-line reference.", call. = FALSE)
  }

  assign(name, list(method = name, fn = fn, change_in = change_in,
                    engine = as.character(engine)[1],
                    citation = citation, capabilities = caps,
                    cp_convention = cp_convention),
         envir = .cpt_registry)
  invisible(name)
}

#' @rdname cpt_register_method
#' @export
cpt_unregister_method <- function(name) {
  # cpt_register_method() checks this thoroughly and registry_get() checks
  # it too; this door handed a non-string straight to exists(), which
  # answers with base R's "invalid first argument" or a "first element
  # used" warning.
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string. ",
         "See cpt_registered_methods().", call. = FALSE)
  }
  if (!exists(name, envir = .cpt_registry, inherits = FALSE)) {
    stop("`", name, "` is not a registered method. ",
         "See cpt_registered_methods().", call. = FALSE)
  }
  rm(list = name, envir = .cpt_registry)
  invisible(name)
}

#' @rdname cpt_register_method
#' @return \code{cpt_registered_methods()} returns a tibble with one row per
#'   registered method (columns \code{method}, \code{change_in},
#'   \code{engine}, \code{has_citation}), or a zero-row tibble when none are
#'   registered.
#' @export
cpt_registered_methods <- function() {
  nms <- sort(ls(envir = .cpt_registry))
  if (length(nms) == 0) {
    return(tibble::tibble(method = character(), change_in = character(),
                          engine = character(), has_citation = logical()))
  }
  entries <- lapply(nms, function(nm) get(nm, envir = .cpt_registry))
  tibble::tibble(
    method = vapply(entries, function(e) e$method, character(1)),
    change_in = vapply(entries,
                       function(e) paste(e$change_in, collapse = ", "),
                       character(1)),
    engine = vapply(entries, function(e) e$engine, character(1)),
    has_citation = vapply(entries, function(e) !is.null(e$citation),
                          logical(1))
  )
}

# Internal: fetch a registered method, or NULL.
#' @noRd
registry_get <- function(name) {
  if (!is.character(name) || length(name) != 1L) return(NULL)
  if (!exists(name, envir = .cpt_registry, inherits = FALSE)) return(NULL)
  get(name, envir = .cpt_registry)
}

# Internal: registrations made with cpt_register_method() live in an
# environment inside the package namespace, and a parallel worker loads the
# package fresh -- so a session-registered method is invisible on the far
# side and cpt_batch()/cpt_consensus()/... under a multisession plan fail
# with "'arg' should be one of ...".  Snapshotting the registry and letting
# the worker closure carry it is what makes registered methods work in
# parallel; the entries are plain lists holding a function, so they
# serialise like any other future global.
#' @noRd
registry_snapshot <- function() {
  as.list(.cpt_registry, all.names = TRUE)
}

# Internal: re-create a snapshot's entries in this session's registry.
#' @noRd
registry_restore <- function(snapshot) {
  if (!length(snapshot)) return(invisible(FALSE))
  for (nm in names(snapshot)) {
    assign(nm, snapshot[[nm]], envir = .cpt_registry)
  }
  invisible(TRUE)
}

# Internal: wrap a worker function so it re-creates the parent's registry
# before running.  With nothing registered the function is returned
# untouched, so the common path keeps exactly the globals it had before.
#' @noRd
with_session_registry <- function(fun) {
  snapshot <- registry_snapshot()
  if (!length(snapshot)) return(fun)
  function(...) {
    registry_restore(snapshot)
    fun(...)
  }
}

# Internal: the capability flags, all off by default.
#' @noRd
default_capabilities <- function() {
  list(multivariate = FALSE, univariate = TRUE, online = FALSE, ci = FALSE,
       fitted = FALSE, posterior = FALSE, statistic = FALSE, path = FALSE,
       scale_space = FALSE)
}

# Internal: every `change_in` value the package recognises. Extended in
# 0.5.0 for the new data types (covariance, network, regression,
# seasonality); the original five are unchanged.
#' @noRd
cpt_change_in_levels <- function() {
  c("mean", "var", "meanvar", "slope", "distribution",
    "covariance", "network", "regression", "seasonality")
}

# Internal: registered methods as registry rows, so cpt_methods() and the
# capability lookups can rbind them onto the built-ins.
#' @noRd
registered_registry <- function() {
  nms <- sort(ls(envir = .cpt_registry))
  if (length(nms) == 0) return(NULL)
  entries <- lapply(nms, function(nm) get(nm, envir = .cpt_registry))
  caps <- function(f) vapply(entries, function(e) isTRUE(e$capabilities[[f]]),
                             logical(1))
  tibble::tibble(
    method = vapply(entries, function(e) e$method, character(1)),
    change_in = vapply(entries,
                       function(e) paste(e$change_in, collapse = ", "),
                       character(1)),
    engine = vapply(entries, function(e) e$engine, character(1)),
    supports = lapply(entries, function(e) e$change_in),
    wrapper = NA_character_,
    multivariate = caps("multivariate"),
    univariate = caps("univariate"),
    online = caps("online"),
    ci = caps("ci"),
    fitted = caps("fitted"),
    posterior = caps("posterior"),
    statistic = caps("statistic"),
    path = caps("path"),
    scale_space = caps("scale_space"),
    status = "registered",
    target_release = NA_character_
  )
}

# Internal: built-ins plus whatever the session has registered.
#' @noRd
full_registry <- function() {
  reg <- builtin_registry()
  extra <- registered_registry()
  if (is.null(extra)) return(reg)
  rbind(reg, extra[, names(reg)])
}
