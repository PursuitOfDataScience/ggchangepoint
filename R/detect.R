#' Unified changepoint detection dispatcher
#'
#' Runs a changepoint detection method on a sequence and returns a tidy
#' \code{ggcpt} result object. This is the recommended entry point for most
#' users. See \code{\link{cpt_methods}()} for the full method table with
#' engines and capabilities.
#'
#' @param x The series. A numeric vector for univariate methods, or a
#'   numeric matrix/data frame (rows are time points) for the multivariate
#'   methods (run \code{subset(cpt_methods(), multivariate)$method} for the
#'   list). A \code{ts}, \code{xts}, \code{zoo} or \code{tsibble}
#'   is accepted directly and its time index is carried through to
#'   \code{tidy()} and \code{autoplot()}; so is a data frame together with
#'   \code{y} (and optionally \code{index}).
#'
#'   Also a \strong{formula} with \code{data}: \code{y ~ x1 + x2} fits
#'   breakpoints in a regression (methods \code{"strucchange"},
#'   \code{"segmented"} and \code{"fastcpd"}; see \code{cpt_methods()}'s
#'   \code{formula} column), and \code{y ~ 1} is the response alone, which
#'   every univariate method takes.
#'
#'   Several series at once: a \pkg{dplyr}-grouped data frame, a keyed
#'   \code{tsibble}, or a long data frame with \code{group} naming the
#'   column(s) that identify each series. Each group is detected separately
#'   and the answer is a \code{\link{cpt_batch}()} result carrying the
#'   grouping columns.
#'
#'   Refused rather than coerced: a factor (its level codes are not data),
#'   a survival object, and dates or timestamps as values (they convert to
#'   an increasing count; pass them as \code{index}).
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
#'   its \code{supports} entry lists, six pairs are routed:
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
#'   stronger than those wrappers' own \code{2 * log(n)} default (19.9
#'   against 11.8 at \eqn{n = 360}), so \code{cpt_detect(x, method =
#'   "decafs")} can report fewer changepoints than \code{decafs_wrapper(x)}
#'   on the same series. Pass \code{penalty} explicitly to make the two
#'   entry points agree.
#' @param index Optional time index, one value per observation (dates, say).
#'   Detection still runs on observation positions (every wrapped engine
#'   assumes an equally spaced sequence), but the index is stored on the
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
#'   other wired method rejects an unknown argument by name. A name that
#'   is a near miss of a real argument of \code{cpt_detect()}, the wrapper
#'   or its engine (\code{n_interval} for \code{n_intervals}) is refused
#'   with the suggestion, and a modelling-choice value outside the method's
#'   vocabulary (\code{cpt_methods()$choices}, e.g. \code{test.stat =
#'   "Poison"}) is refused with the nearest valid value. \code{seed} is
#'   honoured by every method: passed to a wrapper that has one, and
#'   otherwise used to scope the random stream around the fit (restored
#'   afterwards); \code{seed = NULL} means unset.
#' @param data A data frame for the formula interface.
#' @param family The distribution a segment is modelled with:
#'   \code{"gaussian"}, \code{"poisson"}, \code{"binomial"},
#'   \code{"exponential"}, \code{"gamma"}, \code{"laplace"} or
#'   \code{"l1"}. \code{NULL} (the default) runs each method's own
#'   default, which for every parametric method is Gaussian. A
#'   single-parameter family changes in its rate or probability, so it is
#'   used with \code{change_in = "mean"}. Which methods fit which
#'   families, and the engine call each combination becomes, is in
#'   \code{\link{cpt_families}()}; a combination not listed there is
#'   refused. The data are checked against the family (counts must be
#'   whole and non-negative, binary data 0/1, waiting times positive).
#'   \pkg{fastcpd}'s and \pkg{stepR}'s own \code{family} values
#'   (\code{"ar"}, \code{"hsmuce"}, ...) still reach those engines.
#' @param group For a long data frame holding several series, the column(s)
#'   identifying each one: a bare name, a string, or \code{c(a, b)}.
#' @param na_action What to do with missing values: \code{"error"} (the
#'   default) refuses them; \code{"omit"} drops them, detects on what is
#'   observed, and reports every location in the \strong{original}
#'   positions (a changepoint lands on the last observed point of its left
#'   segment), keeping the gaps in \code{$data} so plots show them and the
#'   translation in \code{$diagnostics$na_omitted}; \code{"engine"}
#'   passes them to an engine that handles them in place, which measured
#'   for \code{beast} and \code{segmented} only (see
#'   \code{cpt_methods()$na_handling}), and is refused for the rest.
#'   There is no \code{"impute"}: imputing and then detecting pulls the
#'   estimated location towards the imputed stretch, by more the longer the
#'   gap.
#' @param fixed Changepoints known in advance, which are kept and the rest
#'   estimated around them: the detector runs separately on each stretch
#'   between them. Positions, or values of the series' index (a date is
#'   placed at the last observation on or before it; for a numeric index
#'   such as a \code{ts}'s years, a number outside \code{1..n} is read as
#'   an index value and one inside as a position).
#' @param within Windows the changepoints must fall in ("the policy took
#'   effect sometime in Q2"): a pair \code{c(start, end)}, a list of pairs,
#'   or a two-column table, in positions or index values. The engine
#'   searches the whole series and changepoints outside every window are
#'   dropped (recorded in \code{$constraints}); it restricts what is
#'   reported rather than re-optimising.
#' @param min_segment Minimum segment length in observations, translated
#'   into each engine's own argument (\code{minseglen}, \code{min_size},
#'   \code{h}, \code{delta}, ...: \code{cpt_methods()$min_segment} names
#'   it). A method without one refuses it. When \code{min_segment} is not
#'   given, the engines whose own default allows one-observation segments
#'   (the change-in-mean \pkg{changepoint} methods other than SegNeigh,
#'   \code{np} and \code{binsegrcpp}) get a floor of two, so a short or
#'   monotone series is not split after every point.
#' @param min_effect Drop changepoints whose mean shift is smaller than this
#'   many noise standard deviations (estimated from the series' first
#'   differences, which a changepoint barely moves), removing the smallest
#'   first and re-measuring its neighbours after each removal. For a change
#'   in mean only. The size is measured on the same data that located the
#'   change, so it is biased upward; see \code{\link{cpt_effect}()}.
#' @param keep_fit Keep the engine's own object in \code{$fit}? Defaults to
#'   \code{TRUE}. A few engines return objects far larger than the answer
#'   (a warning says so above 10 MB, once per method per session);
#'   \code{FALSE} drops it, which only the engine-specific accessors notice.
#'   Everything else in the result is plain data, so a saved result reads
#'   back in any R session, with no packages installed.
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
#' deviations, \code{"pelt"} returns 1 changepoint at \eqn{\sigma = 1}, 27
#' at \eqn{\sigma = 3} and 76 at \eqn{\sigma = 10}. These are means over 20
#' draws, because a single draw is not stable here: the same settings gave
#' 15/38 at \eqn{n = 100} and 47/152 at \eqn{n = 400}, so the effect grows
#' with the series as well as with the noise. (0.5.0, before the two-point
#' minimum segment, reported 39 and 141 at \eqn{n = 200}: the floor halves
#' the damage and does not remove it.) A measured scale-sensitive engine
#' given noise far from unit scale warns (class
#' \code{ggchangepoint_scale_sensitive}). Three ways to avoid it, in order
#' of convenience:
#' \itemize{
#'   \item standardise the series first
#'     (\code{cpt_detect(scale(x)[, 1], method = "pelt")});
#'   \item pass a penalty on the data's own scale, for example
#'     \code{penalty = 2 * log(length(x)) * stats::var(diff(x)) / 2};
#'   \item use \code{change_in = "meanvar"}, which estimates a variance per
#'     segment and is unaffected.
#' }
#' Most other engines are unaffected: SMUCE, WBS, WBS2, NOT, MOSUM,
#' Isolate-Detect, TGUH, CPOP, \code{"bcp"}, \code{"beast"} and the
#' nonparametric and multivariate methods estimate or cancel the noise scale
#' internally, and returned the same segmentation at a thousandth, one and
#' a thousand times the units. Three did not, on the same series:
#' \code{"geomcp"} runs PELT on its mapped distance and angle series and so
#' inherits the sensitivity above; \code{"decafs"} floors its noise
#' estimate at about 0.03, so it under-segments a series whose noise is
#' smaller than that; and \code{"bocpd"}'s default prior is on the data's
#' own scale. At a thousandth of the units the last two found nothing.
#' Standardising first avoids all three.
#'
#' @return A \code{ggcpt} object: a list with \code{changepoints}
#'   (\code{cp}, \code{cp_value}), \code{segments} (\code{seg_id},
#'   \code{start}, \code{end}, \code{n}, \code{param_estimate}),
#'   \code{data} (\code{index}, \code{value}), the \code{method},
#'   \code{change_in}, \code{penalty}, \code{cp_convention} and
#'   \code{runtime} that produced it, the matched \code{call}, and
#'   \code{fit}, the raw upstream object. Optional slots
#'   (\code{data_wide}, \code{regions}, \code{diagnostics}, ...) appear
#'   only when an engine supplies them; \code{\link{new_ggcpt}()}
#'   documents all of them, and \code{\link{tidy.ggcpt}()},
#'   \code{\link{glance.ggcpt}()} and \code{\link{augment.ggcpt}()}
#'   are the supported way to read one.
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
#'
#' # Counts, with a cost written for them.
#' counts <- c(rpois(100, 2), rpois(100, 6))
#' cpt_detect(counts, method = "pelt", family = "poisson")
#'
#' # Missing values, located in the original positions.
#' gappy <- x
#' gappy[c(20, 21, 150)] <- NA
#' cpt_detect(gappy, method = "pelt", na_action = "omit")$changepoints
#'
#' # A changepoint known in advance, and a window for another.
#' cpt_detect(x, method = "pelt", fixed = 50)$changepoints
#'
#' # Breakpoints in a regression.
#' if (requireNamespace("strucchange", quietly = TRUE)) {
#'   d <- data.frame(t = 1:200, z = rnorm(200))
#'   d$y <- ifelse(d$t <= 120, 1 + d$z, 3 - d$z) + rnorm(200, 0, 0.5)
#'   fit <- cpt_detect(y ~ z, data = d, method = "strucchange")
#'   tidy(fit, "coefficients")
#' }
cpt_detect <- function(x,
                       method = "pelt",
                       change_in = "mean",
                       penalty = "MBIC",
                       index = NULL,
                       y = NULL,
                       ...,
                       data = NULL,
                       family = NULL,
                       group = NULL,
                       na_action = c("error", "omit", "engine"),
                       fixed = NULL,
                       within = NULL,
                       min_segment = NULL,
                       min_effect = NULL,
                       keep_fit = TRUE) {

  user_call <- match.call()
  y_expr <- substitute(y)
  index_expr <- substitute(index)
  group_expr <- substitute(group)
  caller <- parent.frame()
  na_action <- cpt_match_arg(na_action)
  validate_flag(keep_fit, "keep_fit")
  if (!is.null(min_segment)) {
    validate_scalar(min_segment, "min_segment", min = 1)
  }
  if (!is.null(min_effect)) validate_scalar(min_effect, "min_effect", min = 0)
  index_label <- NULL

  # ---- formula interface ---------------------------------------------------
  # `cpt_detect(y ~ x1 + x2, data = d, method = "strucchange")`. The formula
  # interface existed inside strucchange_wrapper() and the dispatcher could
  # not reach it: the formula went to as.numeric() and came back as "'language'
  # object cannot be coerced to type 'double'". An intercept-only formula is
  # the response as a series, so every univariate method takes it.
  if (inherits(x, "formula")) {
    spec <- formula_series(x, data, index_expr, caller)
    if (!spec$intercept_only) {
      return(detect_regression(
        spec, method = method, change_in = change_in, penalty = penalty,
        family = family, na_action = na_action, fixed = fixed,
        within = within, min_segment = min_segment, min_effect = min_effect,
        keep_fit = keep_fit, user_call = user_call, ...))
    }
    x <- spec$y
    if (!is.null(spec$index)) {
      index <- spec$index
      index_label <- spec$index_label
    }
    y_expr <- NULL
    index_expr <- NULL
  } else if (!is.null(data)) {
    cpt_abort("`data` is only used with a formula: `cpt_detect(y ~ x, data ",
              "= d)`. For a data frame of series, pass it as `x` (with `y =` ",
              "to pick the column).", class = "bad_argument")
  }

  # ---- grouped and long data frames ------------------------------------------
  # A dplyr-grouped frame, a keyed tsibble, or a long frame with `group =`
  # holds several series; each group becomes one, and the answer is a
  # `ggcpt_batch` like cpt_batch()'s.
  groups <- if (is.data.frame(x)) detect_groups(x, group_expr, caller) else {
    if (!is.null(group_expr)) {
      cpt_abort("`group` selects a column of a data frame, and `x` is a ",
                class(x)[1], ".", class = "bad_argument")
    }
    NULL
  }
  if (!is.null(groups)) {
    args <- c(list(method = method, change_in = change_in,
                   penalty = penalty, family = family,
                   na_action = na_action, fixed = fixed, within = within,
                   min_segment = min_segment, min_effect = min_effect,
                   keep_fit = keep_fit), list(...))
    args <- args[!vapply(args, is.null, logical(1))]
    return(detect_grouped(x, groups, y_expr, index_expr, caller, args))
  }

  # ---- data-frame interface -----------------------------------------------
  # `cpt_detect(df, y = value, index = date)`. `y` and `index` are column
  # selections here (bare name, string, or position), not vectors, and the
  # branch is entered only when `y` is supplied -- a bare data frame keeps
  # its 0.4.0 meaning (a one-column series, or a multivariate matrix).
  if (!is.null(y_expr) && is.data.frame(x)) {
    df <- as.data.frame(x)
    yv <- df_column(df, y_expr, "y", caller)
    if (!is.numeric(yv)) {
      cpt_abort("`y` must select a numeric column; got ", class(yv)[1], ".",
                class = "bad_argument")
    }
    index <- if (!is.null(index_expr)) {
      df_column(df, index_expr, "index", caller)
    } else {
      NULL
    }
    x <- as.numeric(yv)
  } else if (!is.null(y_expr)) {
    cpt_abort("`y` selects a column and is only meaningful when `x` is a data ",
              "frame. Pass the series itself as `x`.", class = "bad_argument")
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
    cpt_abort("`", method, "` is planned but not wired in this release: see the ",
              "\"planned\" rows of `cpt_methods()`. It will be built on the ",
              row$engine, " package, and is waiting on ",
              if (identical(row$target_release, "when on CRAN")) {
                paste0(row$engine, " being available from CRAN")
              } else {
                paste0("the ", row$target_release)
              }, ".", class = "planned_method")
  }

  registered <- if (is.character(method) && length(method) == 1L) {
    registry_get(method)
  } else {
    NULL
  }
  if (is.null(registered)) {
    builtin <- builtin_registry()$method
    method <- cpt_match_arg(
      method, builtin, class = "unknown_method",
      suggest_from = unique(c(builtin, full_registry()$method)),
      hint = paste0("Run `cpt_methods()` for all ", length(builtin),
                    " methods and what each supports."))
  }
  change_in <- cpt_match_arg(change_in, cpt_change_in_levels())

  # ---- family ------------------------------------------------------------------
  # `family` names the distribution a segment is modelled with (see
  # cpt_families()). Two engines already had an argument of that name for
  # their own vocabulary (fastcpd's "ar"/"garch", stepR's "hsmuce"), and a
  # value from it still reaches the engine unchanged.
  dots <- list(...)
  if (is.null(registered) && is.character(family) && length(family) == 1L &&
      !family %in% family_levels()) {
    engine_fams <- method_choices()[[method]]$family
    if (!is.null(engine_fams) && !is.na(pmatch(family, engine_fams))) {
      dots$family <- family
      family <- NULL
    }
  }
  fam <- resolve_family(method, change_in, family,
                        registered = !is.null(registered))

  # ---- coerce the series, keeping any time index it carries ---------------
  series <- as_cpt_series(x, index = index)
  x <- series$values
  idx <- series$index
  if (!is.null(index_label)) series$index_label <- index_label

  # ---- missing values ------------------------------------------------------
  na_info <- NULL
  if (na_action == "omit") {
    na_info <- omit_missing(x)
    if (!is.null(na_info)) x <- na_info$x
  } else if (na_action == "engine") {
    check_engine_na(method, registered)
  }
  if (na_action == "engine") with_na_allowed(validate_data(x)) else
    validate_data(x)
  validate_method_change_in(method, change_in)
  if (!is.null(fam)) check_family_data(x, fam$family, method)

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

  # `penalty` reaches the engines by half a dozen routes, and a malformed
  # value fell through all of them: `NA` or a misspelt name silently became
  # the fpop, cpop, decafs or fastcpd default, a vector was cut to its first
  # element (or printed as two penalties), and a negative number put a
  # changepoint at every observation. A registered method only records the
  # penalty, so its own vocabulary is left alone.
  if (is.null(registered) && !is.null(penalty)) {
    if (is.numeric(penalty)) {
      validate_scalar(penalty, "penalty", min = 0)
    } else if (!is.character(penalty) || length(penalty) != 1L ||
               is.na(penalty)) {
      cpt_abort("`penalty` must be one penalty name (\"MBIC\", \"BIC\", ...) ",
                 "or ", "one non-negative number (got ",
                paste(format(penalty), collapse = ", "), ").",
                class = "bad_argument")
    }
  }

  is_mv <- is.matrix(x) || is.data.frame(x)
  reg <- full_registry()
  mv_methods <- reg$method[reg$multivariate]
  if (is_mv && ncol(as.matrix(x)) > 1 && !method %in% mv_methods) {
    cpt_abort("Method `", method, "` is univariate, but `x` has ",
              ncol(as.matrix(x)), " columns. Multivariate methods: ",
              paste(mv_methods, collapse = ", "), ".",
              class = "wrong_dimension")
  }
  n <- if (is_mv) nrow(as.matrix(x)) else length(x)

  # ---- what the data look like ---------------------------------------------
  warn_short_series(n)
  warn_data_type(x, family, method)
  if (is.null(fam) || identical(fam$family, "gaussian")) {
    warn_scale_sensitive(x, method, change_in)
  }

  # ---- engine arguments ------------------------------------------------------
  derived_extra <- list()
  if (is.null(registered)) {
    wrapper <- reg$wrapper[match(method, reg$method)]
    # `seed` means the same thing for every method: reproduce this call.
    # A wrapper with its own `seed` takes it; for the rest it scopes the
    # random stream around the fit, so it is honoured (and harmless for a
    # deterministic engine) instead of reaching an engine that does not
    # know the name. NULL is "not set", as everywhere in the package.
    if ("seed" %in% names(dots) &&
        !"seed" %in% names(formals(get(wrapper, asNamespace("ggchangepoint"))))) {
      seed_arg <- dots$seed
      dots$seed <- NULL
      local_seed(seed_arg)
    }
    dots <- validate_choice_args(method, dots)
    check_dots_names(names(dots), method, wrapper)
    ms_spec <- min_segment_args()[[method]]
    if (!is.null(min_segment)) {
      if (!is.null(ms_spec) && ms_spec$arg %in% names(dots)) {
        cpt_abort("`min_segment` and `", ms_spec$arg, "` both set the ",
                  "minimum segment length; pass one.", class = "bad_argument")
      }
      derived_extra <- min_segment_translate(method, min_segment, n)
    }
  } else if (!is.null(min_segment)) {
    cpt_abort("`min_segment` is translated into an engine's own argument, ",
              "and a registered method declares none. Pass the argument ",
              "your detector takes through `...`.", class = "unsupported")
  }
  if (!is.null(fam)) {
    fam_args <- fam$args[setdiff(names(fam$args), ".cpt_ci")]
    for (nm in intersect(names(fam_args), names(dots))) {
      if (!identical(dots[[nm]], fam_args[[nm]])) {
        cpt_abort("`family = \"", fam$family, "\"` sets `", nm, " = ",
                  format(fam_args[[nm]]), "` for `", method, "`, and `", nm,
                  " = ", format(dots[[nm]]), "` was passed too. Pass one.",
                  class = "bad_argument")
      }
    }
  }

  # ---- constraints -----------------------------------------------------------
  keep_mask <- if (!is.null(na_info)) na_info$keep else NULL
  n_orig <- if (!is.null(na_info)) na_info$n else n
  to_fit_positions <- function(p) {
    if (is.null(p) || is.null(keep_mask)) return(p)
    as.integer(pmax(1, pmin(to_compact_positions(p, keep_mask), n - 1L)))
  }
  fixed_pos <- to_fit_positions(constraint_positions(fixed, idx, n_orig,
                                                     "fixed"))
  windows <- within_windows(within, idx, n_orig)
  if (!is.null(windows) && !is.null(keep_mask)) {
    windows[] <- to_fit_positions(windows)
  }

  run_one <- function(xx) {
    tryCatch(
      dispatch_method(method, registered, xx, change_in = change_in,
                      penalty = penalty, frequency = series$frequency,
                      dots = dots, fam = fam, derived_extra = derived_extra,
                      na_engine = identical(na_action, "engine")),
      ggchangepoint_engine_missing = function(e) {
        if (!is.null(registered)) stop(e)
        abort_with_alternatives(e, method, change_in)
      })
  }

  t0 <- proc.time()[["elapsed"]]
  if (!is.null(fixed_pos)) {
    parts <- detect_fixed(x, fixed_pos, run_one)
    template <- run_template(parts$pieces, method, change_in)
    data_vec <- if (is_mv) as.numeric(as.matrix(x)[, 1]) else as.numeric(x)
    res <- ggcpt_build(
      data_vec, parts$changepoints$cp, method = method,
      change_in = template$change_in, penalty = template$penalty,
      call = user_call,
      extra_cp_cols = as.list(parts$changepoints[, setdiff(
        names(parts$changepoints), c("cp", "cp_value")), drop = FALSE]),
      data_wide = if (is_mv) mv_data_wide(as_mv_matrix(x)))
    res$pieces <- parts$pieces
    res$constraints <- list(fixed = fixed_pos)
    if (!is.null(registered)) res$registered <- TRUE
  } else {
    res <- run_one(x)
  }
  if (!is.null(windows)) {
    res$constraints$within <- windows
    res <- apply_within(res, windows)
  }
  if (!is.null(min_effect)) res <- apply_min_effect(res, min_effect)

  runtime <- proc.time()[["elapsed"]] - t0
  res$runtime <- runtime
  # Record the call the user actually made. The wrappers each store their own
  # match.call(), which for a dispatched run is an internal, unexported helper
  # (`wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)`) that the reader
  # can neither recognise nor re-run.
  res$call <- user_call
  if (!is.null(fam)) {
    res$family <- fam$family
    if (!is.null(fam$reported) && identical(scalar_chr(res$change_in),
                                            "meanvar") &&
        identical(fam$args$.cpt_ci, "mean_var")) {
      res$change_in <- fam$reported
    }
  }
  if (!is.null(na_info)) res <- restore_missing(res, na_info)
  res <- attach_index(res, idx, series$index_label)
  # Recorded, not warned: autocorrelated residuals are most real series,
  # and a warning on every one would be tuned out within a week (§201.2).
  # cpt_assumptions() and cpt_report() read it.
  dep <- residual_dependence(res$data$value, res$changepoints$cp,
                             fitted = res$data[["fitted"]])
  if (!is.null(dep)) {
    res$diagnostics <- c(res$diagnostics %||% list(),
                         list(residual_dependence = dep))
  }
  warn_implausible_count(res)
  if (!keep_fit) {
    res$fit <- NULL
  } else {
    warn_large_fit(res)
  }
  res
}

# Internal: one dispatch, the part of cpt_detect() that calls an engine.
# Kept separate so the fixed-changepoint route can run it on each stretch.
#' @noRd
dispatch_method <- function(method, registered, x, change_in, penalty,
                            frequency, dots, fam, derived_extra,
                            na_engine = FALSE) {
  is_mv <- is.matrix(x) || is.data.frame(x)
  data_vec <- if (is_mv) as.numeric(as.matrix(x)[, 1]) else as.numeric(x)
  reg <- full_registry()
  fam_args <- if (!is.null(fam)) fam$args else list()
  run <- function() {
    # Anything that escapes the engine unclassed is re-signalled as a
    # `ggchangepoint_engine_error`; see with_engine_errors().
    with_engine_errors(if (!is.null(registered)) {
      do.call(run_registered_method,
              c(list(registered, x, change_in = change_in,
                     penalty = penalty), fam_args, dots))
    } else if (method %in% c("pelt", "binseg", "segneigh", "amoc", "np")) {
      ci <- fam_args$.cpt_ci %||% change_in_mapping(change_in)
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
        # changepoint.np is a distribution-change detector; report that
        # rather than the (accepted) default request.
        user_change_in <- "distribution"
      }
      # The changepoint package does not implement the MBIC penalty for the
      # Segment Neighbourhood method; fall back to SIC (which it does
      # support) when the user keeps the default penalty.
      if (method == "segneigh" && identical(penalty, "MBIC")) {
        penalty <- "SIC"
      }
      extra <- c(fam_args[setdiff(names(fam_args), ".cpt_ci")],
                 derived_extra)
      extra <- extra[setdiff(names(extra), names(dots))]
      do.call(wrap_cpt_to_ggcpt,
              c(list(data_vec, ci, cp_method, method, penalty = penalty,
                     user_change_in = user_change_in), extra, dots))
    } else if (method == "ecp") {
      # Pass the original object (not flattened) for multivariate support
      extra <- derived_extra[setdiff(names(derived_extra), names(dots))]
      do.call(wrap_ecp_to_ggcpt, c(list(x), extra, dots))
    } else {
      # Convert penalty to numeric for methods that need it
      pen_val <- resolve_numeric_penalty(penalty, n = length(data_vec))
      # A name this cannot translate used to fall back to the wrapper's own
      # default without a word, so `penalty = "mbic"` ran at 2 * log(n) and
      # said "Manual". fastcpd translates a different set, below.
      if (method %in% c("fpop", "cpop", "decafs") && is.character(penalty) &&
          is.null(pen_val)) {
        cpt_abort("Method `", method, "` takes a numeric penalty, and \"",
                  penalty, "\" is not a name cpt_detect() can translate into ",
                   "one. Use ",
                  paste0("\"", numeric_penalty_names(), "\"", collapse = ", "),
                  ", or a number.", class = "bad_argument")
      }
      if (identical(method, "fastcpd") && is.character(penalty) &&
          !toupper(penalty) %in% toupper(c("MDL", numeric_penalty_names()))) {
        cpt_abort("Method `fastcpd` does not recognise the penalty \"",
                  penalty, "\". Use \"MBIC\", \"BIC\", \"SIC\" or \"MDL\", ",
                  "which it shares, one of the names left to its default ",
                  "(\"AIC\", \"Hannan-Quinn\", \"sSIC\", \"None\"), or a ",
                  "number.", class = "bad_argument")
      }

      # Call the registry's wrapper with the arguments this dispatcher
      # derives (from `change_in`, `family`, `penalty`, or the method
      # name). A value the caller passed through `...` wins over the
      # derived one, so `cpt_detect(x, method = "not", contrast =
      # "pcwsLinMean")` overrides the contrast instead of erroring with
      # "matched by multiple actual arguments". `x` is passed as a symbol
      # so the wrapper's `match.call()` stays compact rather than inlining
      # the whole series.
      wrapper <- reg$wrapper[match(method, reg$method)]
      if (is.na(wrapper)) {
        cpt_abort("Method '", method, "' is not wired to a wrapper. ",
                  "This is an internal error; please report it.",
                  class = "internal")
      }
      derived <- derived_args_for(method, change_in, pen_val)
      derived <- utils::modifyList(derived, fam_args, keep.null = TRUE)
      derived <- utils::modifyList(derived, derived_extra, keep.null = TRUE)
      # A seasonal frequency the input carried (see as_cpt_series()) is one
      # of the derived arguments: the series has been reduced to a bare
      # vector by now, so an engine that needs a frequency would otherwise
      # fall back to its own default -- bfast's is 12, which silently
      # re-seasoned a quarterly `ts` as monthly. Only engines that take a
      # `frequency` get it, and only when the caller did not name one.
      if (!is.null(frequency) &&
          "frequency" %in% names(formals(match.fun(wrapper)))) {
        derived$frequency <- frequency
      }
      # fastcpd takes its penalty as `beta`, on its own scale -- so the
      # resolved `pen_val`, computed on the Gaussian change-in-mean scale
      # the changepoint-family engines use, is not it, and
      # `derived_args_for()` returned only the family. `cpt_detect(x,
      # method = "fastcpd", penalty = 5)` therefore resolved the 5 and threw
      # it away. A number is unambiguous, and three of the names are shared
      # with fastcpd verbatim; anything else is left to the engine's own
      # default, which the penalty-semantics section of ?cpt_penalty now
      # states.
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
      do.call(wrapper, c(list(x = quote(x)), derived, dots),
              envir = environment())
    }, method)
  }
  if (na_engine) with_na_allowed(run()) else run()
}

# Internal: the change type and penalty a fixed-changepoint fit reports,
# read off the first stretch that ran (every stretch ran the same request).
#' @noRd
run_template <- function(pieces, method, change_in) {
  first <- Filter(Negate(is.null), pieces)
  if (!length(first)) {
    return(list(change_in = change_in,
                penalty = list(type = NA_character_, value = NA_real_)))
  }
  list(change_in = first[[1]]$change_in, penalty = first[[1]]$penalty)
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
    cpt_abort("`change_in = \"", change_in, "\"` is not supported by the ",
              "registered method `", entry$method, "`. Supported: ",
              paste(entry$change_in, collapse = ", "), ".",
              class = "unsupported",
              data = list(method = entry$method, requested = change_in,
                          supported = entry$change_in))
  }
  # A registration is arbitrary user code, so it can fail in two different
  # ways and they want different treatment. An error it raises deliberately
  # (`stop(..., call. = FALSE)`, which leaves no call) is the author's own
  # message and passes through with its text untouched; cpt_detect() then
  # classes it as an engine error. An error that LEAKS
  # from base R or from a package the detector called carries the call that
  # raised it (see the provenance note in the sweep tests), and arrived here
  # as e.g. "non-numeric argument to mathematical function" with nothing to
  # say which method produced it. Every built-in wrapper names itself when
  # its engine fails; the registered path did not.
  out <- withCallingHandlers(
    entry$fn(x, ...),
    error = function(e) {
      if (!is.null(conditionCall(e))) {
        cpt_abort("The function registered for `", entry$method, "` failed: ",
                  conditionMessage(e), class = "engine_error",
                  data = list(method = entry$method, engine = entry$engine),
                  parent = e)
      }
    })
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
      cpt_abort("The function registered for `", entry$method, "` returned a ",
                "result for a different series: `x` has ", n_in,
                " observation(s), the ggcpt it returned has ", n_out,
                ". A registered method must detect on the series it is given.",
                class = "engine_error")
    }
    out$method <- entry$method
    out$registered <- TRUE
    out$versions <- version_stamp(entry$method, engine = entry$engine)
    return(out)
  }
  if (!is.numeric(out) && !is.integer(out)) {
    cpt_abort("The function registered for `", entry$method, "` must return a ",
               "ggcpt object or a numeric vector of changepoint ",
              "indices; it returned an object of class ", class(out)[1], ".",
              class = "engine_error")
  }
  # `as_ggcpt()` reports what it drops from `cp`, and its advice -- "check
  # the values against the series rather than relying on this
  # normalisation" -- is aimed at a person transcribing published breaks.
  # That is the wrong reader here, which is why this report used to be
  # muffled outright. But silence is the wrong answer too: a registered
  # detector is code the CALLER wrote (an in-house method, a Python
  # detector through reticulate), so an index this package cannot use is a
  # bug in their detector, and dropping it without a word hides exactly the
  # thing they need to see. Measured before this changed: a registration
  # returning `c(30, NA, 60)` gave two changepoints and no warning, while
  # `as_ggcpt(c(30, NA, 60), x)` warned -- so "the same contract checks as
  # every built-in wrapper" was not what the registered path got.
  #
  # So: same information, re-aimed. The class-specific report is replaced
  # rather than suppressed, and every other warning as_ggcpt() might raise
  # still reaches the caller untouched.
  n_supplied <- length(out)
  res <- withCallingHandlers(
    as_ggcpt(out, x, method = entry$method, change_in = change_in,
             penalty = penalty, cp_convention = entry$cp_convention),
    ggchangepoint_cp_dropped = function(w) invokeRestart("muffleWarning"))
  n_kept <- nrow(res$changepoints)
  # Two ways the detector's output can be altered, and a count catches only
  # the first: values DROPPED (out of range, missing, duplicated) and values
  # TRUNCATED (a fractional index becomes the whole number below it). The
  # count-only version of this check was silent on `c(30.7, 60.2)` -- two
  # supplied, two kept, and both quietly moved.
  num <- suppressWarnings(as.numeric(out))
  truncated <- sum(is.finite(num) & num != trunc(num))
  if (n_kept < n_supplied || truncated > 0L) {
    cpt_warn("The function registered for `", entry$method, "` returned ",
             n_supplied, " changepoint(s); ",
             if (n_kept < n_supplied) {
               paste0(n_supplied - n_kept, " could not be used and ",
                      if (n_kept == 1L) "1 was" else paste0(n_kept, " were"),
                      " kept")
             } else {
               "all were kept"
             },
             if (truncated > 0L) {
               paste0(", and ", truncated,
                      " fractional value(s) were truncated to whole numbers")
             } else {
               ""
             },
             ". A location must be a whole number in 1..",
             nrow(res$data) - 1L, ", and duplicates collapse. Check what ",
             "the detector returns against that range.",
             class = "dropped_input")
  }
  res$registered <- TRUE
  res$versions <- version_stamp(entry$method, engine = entry$engine)
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
#' Returns a tibble describing every method the package knows about (those
#' that are wired, those a user has registered with
#' \code{\link{cpt_register_method}()}, and those that are planned), along
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
#'         \code{"registered"} (supplied by the user this session; see
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
#'         it works on \emph{any} series and any result, a \code{pelt}
#'         fit included. What this column marks is the two engines that
#'         sweep can be run \emph{with}, i.e. the domain of that
#'         function's own \code{method} argument:
#'         \code{subset(cpt_methods(), scale_space)$method}.
#'
#'         \code{online} means the \emph{algorithm} is sequential (it
#'         consumes observations one at a time), and this table reports it
#'         because it governs how the method behaves in batch: an online
#'         detector's threshold is a rate per observation, so run over a
#'         whole series through \code{\link{cpt_detect}()} it reports
#'         roughly \eqn{n / \mathrm{arl0}} changepoints by construction.
#'         It does \strong{not} mean the method can be passed to
#'         \code{\link{cpt_monitor}()}, which takes its own three:
#'         \code{"edetector"}, \code{"cpm"} and \code{"ocd"}. The two
#'         sets overlap without coinciding: \code{bocpd} is an online
#'         algorithm this table marks but the monitor does not offer, and
#'         \code{edetector} is native to this package rather than a
#'         wrapped engine, so it has no row here at all.}
#'   \item{families, choices, formula, min_segment, na_handling,
#'         cp_convention_upstream}{The modelling vocabulary (omitted when
#'         \code{capabilities = FALSE}): the distribution families
#'         \code{cpt_detect(family = )} accepts (see
#'         \code{\link{cpt_families}()}; \code{NA} for a distribution-free
#'         method); the engine's modelling-choice arguments and their legal
#'         values, which \code{cpt_detect()} validates; whether it takes a
#'         formula with covariates; the engine argument
#'         \code{min_segment} is translated into; what the engine does with
#'         a missing value, \emph{measured} rather than read from its
#'         documentation (\code{"native"}, \code{"compacts"},
#'         \code{"silent_loss"} or \code{"reject"}; see
#'         \code{cpt_detect(na_action = )}); and the engine's own
#'         changepoint convention, which the wrapper translates to
#'         \code{"left"} (\code{"right"}: it reports the first observation
#'         after the change; \code{"continuous"}: a location it estimates
#'         on a continuous scale; \code{"design"}: rows of a lagged design).}
#'   \item{scale_invariant, sequential, max_cp, tier, noise_model_arg,
#'         rate_arg, cost, max_n}{Measured and recorded properties (omitted
#'         when \code{capabilities = FALSE}): whether the answer is the same
#'         at \code{x}, \code{10 * x} and \code{0.1 * x}; whether it
#'         depends on the direction of time; how many changepoints the
#'         method can return where that is fixed; \code{"general"} for the
#'         general-purpose workhorses; the argument that selects the noise
#'         model and the one that sets the false-alarm rate; the runtime
#'         class at \eqn{n = 10{,}000} (\code{"fast"} under a second,
#'         \code{"moderate"} under ten, \code{"slow"}); and the longest
#'         series it finished within the measurement's time cap (\code{0}
#'         when it did not finish 1,000 observations). The
#'         measurements behind them are the \code{\link{cpt_runtimes}} and
#'         \code{\link{cpt_invariances}} data sets.}
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
  # 0.6.0's vocabulary, rendered as text so the table stays printable: the
  # families `family =` accepts, the modelling choices and their values,
  # whether a formula with covariates is taken, the argument `min_segment`
  # becomes, what the engine does with a missing value, and the engine's
  # own changepoint convention.
  reg$families <- vapply(reg$families, function(f) {
    if (length(f)) paste(f, collapse = ", ") else NA_character_
  }, character(1))
  reg$choices <- vapply(reg$choices, format_choices, character(1))
  vocab_cols <- c("families", "choices", "formula", "min_segment",
                  "na_handling", "cp_convention_upstream")
  measured <- measured_registry_columns(reg$method)
  for (cl in names(measured)) reg[[cl]] <- measured[[cl]]
  keep <- c("method", "change_in", "engine", "status", "target_release",
            if (capabilities) c(cap_cols, vocab_cols, names(measured)))
  wired <- reg[, keep, drop = FALSE]

  planned <- planned_methods()[, c("method", "change_in", "engine", "status",
                                   "target_release"), drop = FALSE]
  if (capabilities) {
    for (cl in setdiff(keep, names(planned))) planned[[cl]] <- NA
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
    cpt_abort("`change_in = \"", change_in, "\"` is not supported for method `",
              method, "`. Supported: ", paste(support, collapse = ", "), ". ",
              "See cpt_methods() for the full capability table.",
              class = "unsupported",
              data = list(method = method, requested = change_in,
                          supported = support))
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
    cpt_abort("A learned penalty must be resolved against the series. Call ",
              "cpt_detect(x, penalty = model), or predict(model, x) and pass the ",
              "number.", class = "bad_argument")
  }
  if (is.numeric(penalty)) return(as.numeric(penalty))
  if (is.character(penalty)) {
    if (penalty %in% numeric_penalty_names()) {
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

# Internal: the penalty names resolve_numeric_penalty() translates.
#' @noRd
numeric_penalty_names <- function() {
  c("BIC", "SIC", "MBIC", "AIC", "Hannan-Quinn", "None", "sSIC")
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
#'     character penalty (\code{"AIC"}, \code{"Hannan-Quinn"},
#'     \code{"sSIC"}, \code{"None"}) has no \pkg{fastcpd} equivalent and
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
#'     \code{cpt_select(criterion = "mbic")} for the Zhang-Siegmund one.
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
      cpt_abort("A learned penalty depends on the series' features, so ",
                 "`series` ", "must be supplied: cpt_penalty(model, series = ",
                 "x).", class = "bad_argument")
    }
    # coerce_series_values(), not as.numeric(): the model predicts from
    # features of this series, so a factor would have it predict a penalty
    # for the level codes -- silently, since the answer is just a number.
    return(unname(stats::predict(
      type, coerce_series_values(series, arg = "series")))[1])
  }
  type <- cpt_match_arg(type, c("None", "BIC", "SIC", "MBIC", "AIC",
                            "Hannan-Quinn", "sSIC", "Manual"))

  if (type == "None") return(0)
  if (type == "Manual") {
    if (is.null(value)) cpt_abort("`value` must be supplied for Manual type.",
                                  class = "bad_argument")
    return(value)
  }

  if (is.null(n)) cpt_abort("`n` must be supplied for ", type, " penalty.",
                            class = "bad_argument")
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n)) {
    cpt_abort("`n` must be a single finite number.", class = "bad_argument")
  }
  # Below n = 3 the log-based penalties stop being penalties: log(n) is 0 at
  # n = 1, and log(log(n)) is -Inf there and negative at n = 2, so the
  # "penalty" would reward extra changepoints instead of discouraging them.
  # (AIC = 2k does not involve n, so it is exempt.) Series that short are
  # rejected upstream by validate_data() anyway.
  if (type != "AIC" && n < 3) {
    cpt_abort("`n` must be at least 3 for the ", type, " penalty; log(n) and ",
               "log(log(n)) stop being penalties below that.",
              class = "bad_argument")
  }
  # MBIC's log C(n, k) term is -Inf once k exceeds n, which would silently
  # turn the penalty into -Inf rather than erroring.
  if (type == "MBIC" &&
      (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 0 || k > n)) {
    cpt_abort("`k` must be a single number between 0 and `n` for the MBIC ",
              "penalty (it counts the changepoints being placed among `n` ",
              "observations).", class = "bad_argument")
  }
  # `k` scales every one of these, and only MBIC checked it: a negative k
  # returned a negative "penalty" that rewards changepoints, NA returned NA,
  # and a vector returned a vector.
  validate_scalar(k, "k", min = 0)
  # alpha <= 1 makes sSIC weaker than BIC, i.e. no longer the *strengthened*
  # SIC the argument names; the definition (Fryzlewicz 2014) requires
  # alpha > 1.
  if (type == "sSIC" &&
      (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
       alpha <= 1)) {
    cpt_abort("`alpha` must be a single number greater than 1 for the sSIC ",
              "penalty (got ", paste(format(alpha), collapse = ", "), ").",
              class = "bad_argument")
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
