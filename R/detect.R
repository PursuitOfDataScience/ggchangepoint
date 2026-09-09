#' Unified changepoint detection dispatcher
#'
#' Runs a changepoint detection method on a sequence and returns a tidy
#' \code{ggcpt} result object. This is the recommended entry point for most
#' users. See \code{\link{cpt_methods}()} for the full method table with
#' engines and capabilities.
#'
#' @param x The series. A numeric vector for univariate methods, or a
#'   numeric matrix/data frame (rows are time points) for the multivariate
#'   methods — run \code{subset(cpt_methods(), multivariate)$method} for the
#'   list. A \code{ts}, \code{xts}, \code{zoo} or (unkeyed) \code{tsibble}
#'   is accepted directly and its time index is carried through to
#'   \code{tidy()} and \code{autoplot()}; so is a data frame together with
#'   \code{y} (and optionally \code{index}).
#' @param method Detection method: any \code{method} in
#'   \code{cpt_methods()} whose \code{status} is \code{"available"} or
#'   \code{"registered"}. Methods whose engines live in \code{Suggests}
#'   report what to install when missing;
#'   \code{\link{cpt_register_method}()} adds detectors this package does
#'   not wrap.
#' @param change_in What to detect change in. One of \code{"mean"},
#'   \code{"var"}, \code{"meanvar"}, \code{"slope"},
#'   \code{"distribution"}, \code{"covariance"}, \code{"network"},
#'   \code{"regression"} or \code{"seasonality"}. Defaults to
#'   \code{"mean"}. The requested value is validated against the method's
#'   capabilities (see \code{cpt_methods()}); incompatible combinations
#'   error rather than silently running something else.
#'
#'   A \emph{compatible} request may still be routed to the method's own
#'   native change type, because several engines have no separate estimator
#'   for the thing being asked about. That is never silent: the result's
#'   \code{change_in} records what was actually detected, so compare it
#'   with what you asked for. Measured across every method and every value
#'   its \code{supports} entry lists, six pairs are routed --
#'   \code{not}'s \code{"var"} becomes \code{"meanvar"} (its variance
#'   contrast is piecewise-constant in mean \emph{and} variance),
#'   \code{cpm}'s \code{"mean"} and \code{"var"} both become
#'   \code{"distribution"}, \code{kcp}'s become \code{"running mean"} and
#'   \code{"running var"}, and \code{wbsts}'s \code{"mean"} becomes
#'   \code{"var"} (it detects change in the wavelet spectrum). Every other
#'   listed combination returns the change type it was asked for.
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
#' @param index Optional time index, one value per observation (dates, say).
#'   Detection still runs on observation positions — every wrapped engine
#'   assumes an equally spaced sequence — but the index is stored on the
#'   result and threaded through \code{tidy()} (as \code{cp_index}),
#'   \code{augment()}, \code{autoplot()} and \code{\link{cpt_report}()},
#'   so the output speaks in the user's own units. An index that is not
#'   equally spaced warns. When \code{x} is a data frame and \code{y} is
#'   given, \code{index} selects a column of that data frame instead of
#'   being a vector.
#' @param y Column selection for the data-frame interface:
#'   \code{cpt_detect(df, y = value, index = date, method = "pelt")}. A bare
#'   column name, a string, or a column position. Only meaningful when
#'   \code{x} is a data frame; a data frame passed without \code{y} keeps
#'   its 0.4.0 meaning (one column per coordinate).
#' @param ... Additional arguments passed to the specific wrapper (see the
#'   wrapper's help page for engine-specific options). Where an argument is
#'   also derived from \code{change_in} (\code{not}'s \code{contrast},
#'   \code{cpm}'s \code{cpm_type}, \code{kcp}'s \code{running_stat},
#'   \code{sn}'s \code{parameter}, \code{fastcpd}'s \code{family}), a value
#'   supplied here takes precedence. Check the spelling against the wrapper's
#'   help page: several engines end their own signature in \code{...}
#'   (\pkg{wbs}, \pkg{not}, \pkg{Rbeast}, \pkg{strucchange},
#'   \pkg{segmented}, \pkg{fastcpd}, \pkg{fChange}, \pkg{bfast}), so for
#'   those a misspelt argument name is silently discarded upstream and the
#'   engine quietly uses its default rather than reporting the typo. Every
#'   other wired method rejects an unknown argument by name.
#'
#' @section Scale sensitivity of the penalised change-in-mean engines:
#' \code{"pelt"}, \code{"binseg"}, \code{"segneigh"} and \code{"fpop"}
#' compare a penalty against a \emph{raw} segment cost when
#' \code{change_in = "mean"}: \pkg{changepoint}'s Normal cost assumes a
#' noise standard deviation of 1, and \pkg{fpop}'s \code{lambda} is an
#' absolute penalty on the residual sum of squares. Neither rescales the
#' data, so on a series whose noise is much wider than 1 the penalty is
#' effectively negligible and the segmentation shatters. On 200 observations
#' with one true changepoint in the middle and a jump of five standard
#' deviations, \code{"pelt"} returns 1 changepoint at \eqn{\sigma = 1}, 39
#' at \eqn{\sigma = 3} and 141 at \eqn{\sigma = 10} --- means over 20
#' draws, because a single draw is not stable here: the same three settings
#' gave 21/75 at \eqn{n = 100} and 57/266 at \eqn{n = 400}, so the effect
#' grows with the series as well as with the noise. Three ways to avoid it, in order of
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
#' @seealso \code{\link{cpt_methods}()} for what is available and what each
#'   method can do. To get the result out: \code{\link{tidy.ggcpt}()},
#'   \code{\link{glance.ggcpt}()}, \code{\link{augment.ggcpt}()},
#'   \code{\link{summary.ggcpt}()} and \code{\link{print.ggcpt}()}. To
#'   draw it: \code{\link{autoplot.ggcpt}()}. For the penalty:
#'   \code{\link{cpt_penalty}()}.
#' @export
#'
#' @examples
#' set.seed(2022)
#' x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
#' result <- cpt_detect(x, method = "pelt", change_in = "mean")
#' result
#' ggplot2::autoplot(result)
#'
#' # A date index: detection is unchanged, but the report speaks in dates.
#' dates <- as.Date("2000-01-01") + 0:199
#' dated <- cpt_detect(x, method = "pelt", index = dates)
#' tidy(dated)
#'
#' # The data-frame interface.
#' df <- data.frame(day = dates, value = x)
#' cpt_detect(df, y = value, index = day, method = "pelt")
cpt_detect <- function(x,
                       method = "pelt",
                       change_in = "mean",
                       penalty = "MBIC",
                       index = NULL,
                       y = NULL,
                       ...) {

  user_call <- match.call()
  y_expr <- substitute(y)
  index_expr <- substitute(index)
  caller <- parent.frame()

  # ---- data-frame interface -----------------------------------------------
  # `cpt_detect(df, y = value, index = date)`. `y` and `index` are column
  # selections here (bare name, string, or position), not vectors, and the
  # branch is entered only when `y` is supplied -- a bare data frame keeps
  # its 0.4.0 meaning (a one-column series, or a multivariate matrix).
  if (!is.null(y_expr) && is.data.frame(x)) {
    df <- as.data.frame(x)
    yv <- df_column(df, y_expr, "y", caller)
    if (!is.numeric(yv)) {
      stop("`y` must select a numeric column; got ", class(yv)[1], ".",
           call. = FALSE)
    }
    index <- if (!is.null(index_expr)) {
      df_column(df, index_expr, "index", caller)
    } else {
      NULL
    }
    x <- as.numeric(yv)
  } else if (!is.null(y_expr)) {
    stop("`y` selects a column and is only meaningful when `x` is a data ",
         "frame. Pass the series itself as `x`.", call. = FALSE)
  }

  # ---- planned and registered methods --------------------------------------
  # `cpt_methods()` advertises five engines as "planned", but match.arg()
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

  registered <- if (is.character(method) && length(method) == 1L) {
    registry_get(method)
  } else {
    NULL
  }
  if (is.null(registered)) {
    method <- match.arg(method, builtin_registry()$method)
  }
  change_in <- match.arg(change_in, cpt_change_in_levels())

  # ---- coerce the series, keeping any time index it carries ---------------
  series <- as_cpt_series(x, index = index)
  x <- series$values
  idx <- series$index

  validate_data(x)
  validate_method_change_in(method, change_in)

  # A learned penalty (see cpt_learn_penalty()) is resolved to a number for
  # this series before anything else looks at `penalty`, so every engine --
  # the ones taking a character penalty and the ones taking a numeric one --
  # sees a value it understands.
  if (inherits(penalty, "ggcpt_penalty_model")) {
    penalty <- unname(stats::predict(penalty,
                                     if (is.matrix(x) || is.data.frame(x)) {
                                       as.numeric(as.matrix(x)[, 1])
                                     } else {
                                       as.numeric(x)
                                     }))[1]
  }

  is_mv <- is.matrix(x) || is.data.frame(x)
  reg <- full_registry()
  mv_methods <- reg$method[reg$multivariate]
  if (is_mv && ncol(as.matrix(x)) > 1 && !method %in% mv_methods) {
    stop("Method `", method, "` is univariate, but `x` has ",
         ncol(as.matrix(x)), " columns. Multivariate methods: ",
         paste(mv_methods, collapse = ", "), ".", call. = FALSE)
  }
  data_vec <- if (is_mv) as.numeric(as.matrix(x)[, 1]) else as.numeric(x)

  t0 <- proc.time()[["elapsed"]]

  if (!is.null(registered)) {
    res <- run_registered_method(registered, x, change_in = change_in,
                                 penalty = penalty, ...)
  } else if (method %in% c("pelt", "binseg", "segneigh", "amoc", "np")) {
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
    # Call the registry's wrapper with the arguments this dispatcher derives
    # (from `change_in`, `penalty`, or the method name). A value the caller
    # passed through `...` wins over the derived one, so
    # `cpt_detect(x, method = "not", contrast = "pcwsLinMean")` overrides the
    # contrast instead of erroring with "matched by multiple actual
    # arguments". `x` is passed as a symbol so the wrapper's `match.call()`
    # stays compact rather than inlining the whole series.
    wrapper <- reg$wrapper[match(method, reg$method)]
    if (is.na(wrapper)) {
      stop("Method '", method, "' is not wired to a wrapper. ",
           "This is an internal error; please report it.", call. = FALSE)
    }
    derived <- derived_args_for(method, change_in, pen_val)
    # A seasonal frequency the input carried (see as_cpt_series()) is one of
    # the derived arguments: the series has been reduced to a bare vector by
    # now, so an engine that needs a frequency would otherwise fall back to
    # its own default -- bfast's is 12, which silently re-seasoned a
    # quarterly `ts` as monthly. Only engines that take a `frequency` get
    # it, and only when the caller did not name one.
    if (!is.null(series$frequency) &&
        "frequency" %in% names(formals(match.fun(wrapper)))) {
      derived$frequency <- series$frequency
    }
    # fastcpd takes its penalty as `beta`, on its own scale -- so the
    # resolved `pen_val`, computed on the Gaussian change-in-mean scale the
    # changepoint-family engines use, is not it, and `derived_args_for()`
    # returned only the family. `cpt_detect(x, method = "fastcpd", penalty =
    # 5)` therefore resolved the 5 and threw it away. A number is
    # unambiguous, and three of the names are shared with fastcpd verbatim;
    # anything else is left to the engine's own default, which the
    # penalty-semantics section of ?cpt_penalty now states.
    if (identical(method, "fastcpd")) {
      if (is.numeric(penalty)) {
        derived$beta <- as.numeric(penalty)[1]
      } else if (is.character(penalty) && length(penalty) == 1L &&
                 toupper(penalty) %in% c("MBIC", "BIC", "SIC", "MDL")) {
        derived$beta <- if (identical(toupper(penalty), "SIC")) {
          "BIC"
        } else {
          toupper(penalty)
        }
      }
    }
    derived <- derived[setdiff(names(derived), names(dots))]
    res <- do.call(wrapper, c(list(x = quote(x)), derived, dots),
                   envir = environment())
  }

  runtime <- proc.time()[["elapsed"]] - t0
  res$runtime <- runtime
  # Record the call the user actually made. The wrappers each store their own
  # match.call(), which for a dispatched run is an internal, unexported helper
  # (`wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)`) that the reader
  # can neither recognise nor re-run.
  res$call <- user_call
  attach_index(res, idx, series$index_label)
}

# Internal: run a user-registered detector and normalise whatever it returns
# to a validated ggcpt. A registration may return a finished ggcpt (built
# with as_ggcpt(), say) or a bare vector of indices; either way the result
# goes through the same contract checks as a built-in wrapper, and the
# method name and engine are taken from the registration rather than from
# whatever the function decided to call itself.
#' @noRd
run_registered_method <- function(entry, x, change_in, penalty, ...) {
  if (!change_in %in% entry$change_in) {
    stop("`change_in = \"", change_in, "\"` is not supported by the ",
         "registered method `", entry$method, "`. Supported: ",
         paste(entry$change_in, collapse = ", "), ".", call. = FALSE)
  }
  out <- entry$fn(x, ...)
  if (is_ggcpt(out)) {
    # A returned ggcpt used to be taken entirely on trust, which let
    # cpt_detect(x, method = <registered>) hand back a result about a
    # different series -- wrong `$data` to plot, wrong row count from
    # augment(), wrong n for every metric. The bare-vector branch below goes
    # through as_ggcpt(); this is the one shape check the other branch gets
    # for free.
    n_in <- if (is.matrix(x) || is.data.frame(x)) {
      nrow(as.matrix(x))
    } else {
      length(x)
    }
    n_out <- nrow(out$data)
    if (!identical(as.integer(n_out), as.integer(n_in))) {
      stop("The function registered for `", entry$method, "` returned a ",
           "result for a different series: `x` has ", n_in,
           " observation(s), the ggcpt it returned has ", n_out,
           ". A registered method must detect on the series it is given.",
           call. = FALSE)
    }
    out$method <- entry$method
    out$registered <- TRUE
    return(out)
  }
  if (!is.numeric(out) && !is.integer(out)) {
    stop("The function registered for `", entry$method,
         "` must return a ggcpt object or a numeric vector of changepoint ",
         "indices; it returned an object of class ", class(out)[1], ".",
         call. = FALSE)
  }
  # `as_ggcpt()` reports what it drops from `cp`, because there it is a
  # person's transcription of published breaks. Here the indices came from
  # the registered detector itself, which is the wrapper case: normalising
  # an engine's output is what the builder is for, and telling the caller to
  # "check the values against the series" would be advice about code they
  # did not write. Muffled by class, so only this one report is suppressed
  # and every other warning as_ggcpt() might raise still reaches them.
  res <- withCallingHandlers(
    as_ggcpt(out, x, method = entry$method, change_in = change_in,
             penalty = penalty, cp_convention = entry$cp_convention),
    ggchangepoint_cp_dropped = function(w) invokeRestart("muffleWarning"))
  res$registered <- TRUE
  res
}

# Internal: the methods this release names but does not wire. Kept in one
# place so `cpt_detect()` can recognise the name and say so, rather than
# denying it exists.
#
# All five wait on CRAN availability, and that is now the whole list: every
# method that was waiting only on a wrapper got one in 0.5.0. `gfpop` and
# `cpss` were removed from CRAN, `robseg`, `FOCuS` and `changeforest` have
# never been on it, and `hdbinseg` -- which the 0.5.0 roadmap recorded as
# back at 1.0.3 -- is in the CRAN archive again as of this release, so
# `sbs` stays here rather than moving to the wired table.
#
# The point of `cpt_register_method()` (see R/registry.R) is that none of
# these is a hard blocker any more: install the engine from wherever it
# lives and register it, and `cpt_detect()` dispatches to it with the whole
# ggcpt toolchain attached.
#' @noRd
planned_methods <- function() {
  tibble::tribble(
    ~method,        ~change_in,                     ~engine,        ~status,   ~target_release,
    "gfpop",        "mean (graph-constrained)",     "gfpop",        "planned", "when on CRAN",
    "robust",       "mean (robust loss)",           "robseg",       "planned", "when on CRAN",
    "focus",        "mean (online)",                "FOCuS",        "planned", "when on CRAN",
    "sbs",          "mean (high-dimensional)",      "hdbinseg",     "planned", "when on CRAN",
    "changeforest", "distribution (random forest)", "changeforest", "planned", "when on CRAN"
  )
}

#' Introspect available changepoint detection methods
#'
#' Returns a tibble describing every method the package knows about — those
#' that are wired, those a user has registered with
#' \code{\link{cpt_register_method}()}, and those that are planned — along
#' with their capabilities and installation status. Useful for discovering
#' what can be run, what needs to be installed, and which methods expose the
#' extras the diagnostics need (confidence intervals, a fitted signal, a
#' posterior, a detector statistic, a solution path, a bandwidth to sweep).
#'
#' @param capabilities Include the capability flag columns? Defaults to
#'   \code{TRUE}. Set \code{FALSE} for the compact 0.4.0-shaped table.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{method}{Method name as passed to \code{cpt_detect()}.}
#'   \item{change_in}{What types of change the method can detect.}
#'   \item{engine}{The upstream R package that implements the method.}
#'   \item{status}{\code{"available"} (wired in this release),
#'         \code{"registered"} (supplied by the user this session — see
#'         \code{\link{cpt_register_method}()}), or \code{"planned"}
#'         (future).}
#'   \item{installed}{\code{TRUE} if the engine package is installed,
#'         \code{FALSE} if it is a \code{Suggests} engine that is missing,
#'         \code{NA} for planned and registered methods.}
#'   \item{target_release}{What a planned method is waiting on: a release,
#'         or \code{"when on CRAN"} when the engine package itself is not
#'         available from CRAN. \code{NA} for methods that are already
#'         wired. Asking \code{cpt_detect()} for a planned method reports
#'         this rather than claiming the name does not exist.}
#'   \item{multivariate, univariate, online, ci, fitted, posterior,
#'         statistic, path, scale_space}{Capability flags (omitted when
#'         \code{capabilities = FALSE}). \code{ci} means the engine
#'         supplies changepoint-location confidence intervals; \code{fitted}
#'         a length-\eqn{n} fitted signal; \code{posterior} a per-location
#'         posterior probability; \code{statistic} and \code{path} the
#'         internals rendered by \code{\link{ggcpt_statistic}()} and
#'         \code{\link{ggcpt_solution_path}()}, which error with the list
#'         of supporting engines when a result does not carry them.
#'
#'         \code{scale_space} is not one of those, despite sitting beside
#'         them. Nothing stores a scale space on a result:
#'         \code{\link{cpt_scale_space}()} computes one on demand by
#'         sweeping a multiscale detector's bandwidth over the series, so
#'         it works on \emph{any} series and any result -- a \code{pelt}
#'         fit included. What this column marks is the two engines that
#'         sweep can be run \emph{with}, i.e. the domain of that
#'         function's own \code{method} argument:
#'         \code{subset(cpt_methods(), scale_space)$method}.
#'
#'         \code{online} means the \emph{algorithm} is sequential -- it
#'         consumes observations one at a time -- and this table reports it
#'         because it governs how the method behaves in batch: an online
#'         detector's threshold is a rate per observation, so run over a
#'         whole series through \code{\link{cpt_detect}()} it reports
#'         roughly \eqn{n / \mathrm{arl0}} changepoints by construction.
#'         It does \strong{not} mean the method can be passed to
#'         \code{\link{cpt_monitor}()}, which takes its own three:
#'         \code{"edetector"}, \code{"cpm"} and \code{"ocd"}. The two
#'         sets overlap without coinciding -- \code{bocpd} is an online
#'         algorithm this table marks but the monitor does not offer, and
#'         \code{edetector} is native to this package rather than a
#'         wrapped engine, so it has no row here at all.}
#' }
#' @seealso \code{\link{cpt_install_engines}()} to install a whole family of
#'   the engines this table reports on; \code{\link{cpt_detect}()} to run
#'   one; \code{\link{cpt_register_method}()} to add your own.
#' @export
#'
#' @examples
#' cpt_methods()
#' # which methods can draw a confidence interval?
#' subset(cpt_methods(), ci)$method
cpt_methods <- function(capabilities = TRUE) {
  validate_flag(capabilities, "capabilities")
  reg <- full_registry()
  cap_cols <- c("multivariate", "univariate", "online", "ci", "fitted",
                "posterior", "statistic", "path", "scale_space")
  keep <- c("method", "change_in", "engine", "status", "target_release",
            if (capabilities) cap_cols)
  wired <- reg[, keep, drop = FALSE]

  planned <- planned_methods()[, c("method", "change_in", "engine", "status",
                                   "target_release"), drop = FALSE]
  if (capabilities) {
    for (cl in cap_cols) planned[[cl]] <- NA
    planned <- planned[, keep, drop = FALSE]
  }

  methods <- rbind(wired, planned)

  # Installation status: TRUE/FALSE for wired engines, NA for planned and
  # registered ones (a registration supplies the detector itself, so there is
  # no package for this package to look for).
  # Assign NA first and fill only the `available` subset: ifelse() evaluates
  # BOTH arms, so this used to call find.package() for all 55 rows -- the
  # five planned engines and every registered one included -- and then throw
  # those answers away. A registration made with `engine = NULL` stores
  # NA_character_, which find.package() is then handed.
  methods$installed <- NA
  avail <- methods$status == "available"
  if (any(avail)) {
    methods$installed[avail] <- vapply(methods$engine[avail],
                                       engine_installed, logical(1))
  }

  # Keep the 0.4.0 column order: the capability flags go after `installed`.
  front <- c("method", "change_in", "engine", "status", "installed",
             "target_release")
  methods[, c(front, setdiff(names(methods), front)), drop = FALSE]
}

# Internal: is an engine installed?
#
# This asks the library, it does not load the package. `requireNamespace()`
# would be the obvious call, but loading is the wrong operation for a
# question about installation, and it is not free: building the
# `cpt_methods()` table would load thirty-five namespaces, and a namespace
# can do anything on the way in. `fabisearch` pulls in `rgl`, which warns
# about the X11 display on every headless machine, and on macOS fails in
# `dyn.load()` outright because the runner has no `libGLU`. Neither is
# information about whether the engine is installed.
#
# `find.package()` answers the actual question and touches nothing.
#' @noRd
engine_installed <- function(pkg) {
  if (pkg %in% c("changepoint", "changepoint.np", "ecp")) return(TRUE)  # Imports
  length(suppressWarnings(find.package(pkg, quiet = TRUE))) > 0L
}

# Internal: the wired-method table in its 0.4.0 shape, derived from the
# registry so there is one source of truth rather than three.
#' @noRd
cpt_methods_table <- function() {
  full_registry()[, c("method", "change_in", "engine", "status",
                      "target_release"), drop = FALSE]
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
  # `pcwsConstMeanVar` changes the mean AND the variance, and not_wrapper()
  # labels its result `change_in = "meanvar"` for that reason -- so a "var"
  # request produced a result whose own `change_in` was not in `not`'s
  # `supports`, a value validate_method_change_in() would refuse and
  # cpt_detect() could never be asked for. "meanvar" is now an accepted
  # request routed to the same contrast, so the label the wrapper writes is
  # one the registry lists.
  switch(change_in,
    mean = "pcwsConstMean",
    var = "pcwsConstMeanVar",
    meanvar = "pcwsConstMeanVar",
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
# Derived from the registry rather than repeated, so a new engine declares
# its capabilities once.
#' @noRd
method_change_in_support <- function() {
  reg <- full_registry()
  stats::setNames(reg$supports, reg$method)
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
  if (inherits(penalty, "ggcpt_penalty_model")) {
    # cpt_detect() resolves a learned penalty up front; reaching here means a
    # wrapper was called directly with one, and it has no series to predict
    # from at this point.
    stop("A learned penalty must be resolved against the series. Call ",
         "cpt_detect(x, penalty = model), or predict(model, x) and pass the ",
         "number.", call. = FALSE)
  }
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

# Internal: resolve a `penalty` argument that may be a learned model.
# cpt_detect() does this up front, but the wrappers are exported and can be
# called directly, and a model reaching an engine surfaces as "'list' object
# cannot be coerced to type 'double'". Resolving here means
# `fpop_wrapper(x, penalty = model)` simply works, on the same series the
# wrapper is about to segment.
#' @noRd
resolve_penalty_model <- function(penalty, data_vec) {
  if (!inherits(penalty, "ggcpt_penalty_model")) return(penalty)
  unname(stats::predict(penalty, as.numeric(data_vec)))[1]
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
#' @param series The series a learned penalty is predicted for. Required
#'   only when \code{type} is a \code{ggcpt_penalty_model} from
#'   \code{\link{cpt_learn_penalty}()}, in which case every other argument
#'   is ignored and the model's prediction for this series is returned.
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
#'   \item \strong{\code{fastcpd}} takes its penalty as \code{beta}, on its
#'     own scale, and defaults to its native \code{"MBIC"}.
#'     \code{cpt_detect()} forwards a numeric \code{penalty} as
#'     \code{beta}, and translates the three names the two packages share
#'     (\code{"MBIC"}, \code{"BIC"}/\code{"SIC"}, \code{"MDL"}). Any other
#'     character penalty --- \code{"AIC"}, \code{"Hannan-Quinn"},
#'     \code{"sSIC"}, \code{"None"} --- has no \pkg{fastcpd} equivalent and
#'     is left to the engine's default rather than being silently
#'     approximated; pass \code{beta} yourself to pin it. Whatever is used
#'     is recorded on the result, so \code{print()} and \code{glance()}
#'     report the penalty of the fit in hand.
#'   \item \strong{Inference/Bayesian methods} (\code{smuce}, \code{bcp},
#'     \code{bocpd}, \code{beast}, \code{cpm}, \code{sn}): are tuned by a
#'     significance level, posterior-probability threshold, hazard, or
#'     average run length rather than a penalty; see each wrapper.
#'   \item \strong{\code{MBIC}} in \code{cpt_penalty()} is a BIC-type penalty
#'     that adds a combinatorial term for the number of ways \code{k}
#'     changepoints can be placed in \code{n} observations,
#'     \eqn{0.5(k+1)\log n + \log{n \choose k}}. It is deliberately stronger
#'     than \code{"BIC"}. It is \emph{not} the modified BIC of Zhang and
#'     Siegmund (2007), whose penalty is
#'     \eqn{1.5 k \log n + 0.5 \sum_i \log(l_i / n)} on the
#'     \strong{log-likelihood} scale (equivalently
#'     \eqn{3 k \log n + \sum_i \log(l_i / n)} on the deviance scale,
#'     which is how \code{\link{cpt_select}()} states it and the scale its
#'     \code{cost} column uses). It depends on the segment lengths
#'     \eqn{l_i} and so cannot be expressed by a function of \code{n} and
#'     \code{k} alone. Use the character \code{"MBIC"} with
#'     \pkg{changepoint}-based methods to get the engine's native MBIC, and
#'     \code{cpt_select(criterion = "mbic")} for the Zhang–Siegmund one.
#' }
#'
#' @return A numeric penalty value.
#' @export
#'
#' @examples
#' cpt_penalty("BIC", n = 100)
#' cpt_penalty("AIC", n = 100)
#' cpt_penalty("Manual", value = 5)
cpt_penalty <- function(type, n = NULL, k = 1, value = NULL, alpha = 1.01,
                        series = NULL) {
  # A learned penalty is a model, not a name: predict it for this series and
  # return the number, so `cpt_penalty()` remains the one place a penalty is
  # turned into a value whatever its provenance.
  if (inherits(type, "ggcpt_penalty_model")) {
    if (is.null(series)) {
      stop("A learned penalty depends on the series' features, so `series` ",
           "must be supplied: cpt_penalty(model, series = x).", call. = FALSE)
    }
    # coerce_series_values(), not as.numeric(): the model predicts from
    # features of this series, so a factor would have it predict a penalty
    # for the level codes -- silently, since the answer is just a number.
    return(unname(stats::predict(
      type, coerce_series_values(series, arg = "series")))[1])
  }
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
