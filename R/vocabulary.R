# ---------------------------------------------------------------------------
# The modelling vocabulary: families, engine choices, and the translation
# table between them.
#
# Fifteen engines take an argument that expresses a modelling choice (which
# family, which cost, which kernel, which test statistic), and until 0.6.0
# thirteen of them could be reached only through `...`, under the engine's
# own name, unvalidated: a typo in `test.stat` was a silent revert to the
# default. Three `match.arg()` lists and one wrapper's fixed distribution
# narrowed the rest (`fastcpd`'s fifteen families to six, `binsegRcpp`'s
# five costs to two). The non-Gaussian costs were there all along; what was
# missing was a name for them.
#
# So this file declares, once:
#   * which distribution families each method can fit (`family =`);
#   * the translation table: every legal (method, change_in, family)
#     combination and the engine call it becomes. It is the table a future
#     parameter-marking grammar would sit on (see the 1.0 note in
#     ?cpt_families), built now because `family` needs it either way;
#   * each method's modelling choices, so a misspelt value is refused by
#     name instead of swallowed;
#   * the engine argument that holds a minimum segment length;
#   * which methods take a formula.
# ---------------------------------------------------------------------------

# Internal: the distribution families cpt_detect() can be asked for. `l1`
# is a cost rather than a family (absolute loss: a change in the median),
# kept here because it is the robust alternative a user reaches for in the
# same breath.
#' @noRd
family_levels <- function() {
  c("gaussian", "poisson", "binomial", "exponential", "gamma", "laplace",
    "l1")
}

# Internal: methods whose estimator assumes no distribution at all. They
# have no family to choose, so `family =` is refused for them rather than
# silently ignored.
#' @noRd
distribution_free_methods <- function() {
  c("np", "ecp", "kcp", "npmojo", "kwc", "pettitt", "taylor", "sn",
    "fmean", "fcov", "network", "fabisearch")
}

# Internal: what a family requires of the data, checked before the engine
# sees it. The engines' own refusals ("Poisson test statistic requires
# integer data") name neither the argument nor the fix.
#' @noRd
family_data_rule <- function(family) {
  switch(family,
    poisson = "counts",
    binomial = "binary",
    exponential = "positive",
    gamma = "positive",
    NULL
  )
}

# Internal: one row of the translation table.
#' @noRd
tr_row <- function(method, change_in, family, call, args = list(),
                   reported = change_in) {
  tibble::tibble(method = method, change_in = change_in, family = family,
                 engine_call = call, args = list(args),
                 reported_change_in = reported)
}

# Internal: the rows that are not the method's plain Gaussian route: the
# families beyond Gaussian, and the Gaussian routes that differ from what
# `family = NULL` runs (cpm's parametric statistics).
#' @noRd
family_rows_explicit <- function() {
  cp_engines <- c("pelt", "binseg", "segneigh", "amoc")
  rows <- list()
  for (m in cp_engines) {
    rows[[length(rows) + 1]] <- tr_row(
      m, "mean", "poisson",
      "changepoint::cpt.meanvar(test.stat = \"Poisson\")",
      list(.cpt_ci = "mean_var", test.stat = "Poisson"))
    rows[[length(rows) + 1]] <- tr_row(
      m, "mean", "exponential",
      "changepoint::cpt.meanvar(test.stat = \"Exponential\")",
      list(.cpt_ci = "mean_var", test.stat = "Exponential"))
  }
  # changepoint's Gamma cost fails inside BinSeg and SegNeigh ("subscript
  # out of bounds", measured on changepoint 2.3), so it is offered for the
  # two search methods where it runs.
  for (m in c("pelt", "amoc")) {
    rows[[length(rows) + 1]] <- tr_row(
      m, "mean", "gamma",
      "changepoint::cpt.meanvar(test.stat = \"Gamma\", shape = <shape>)",
      list(.cpt_ci = "mean_var", test.stat = "Gamma"))
  }
  rows <- c(rows, list(
    tr_row("binsegrcpp", "mean", "poisson",
           "binsegRcpp::binseg(\"poisson\")", list(distribution = "poisson")),
    tr_row("binsegrcpp", "mean", "l1",
           "binsegRcpp::binseg(\"l1\")", list(distribution = "l1")),
    tr_row("binsegrcpp", "meanvar", "laplace",
           "binsegRcpp::binseg(\"laplace\")",
           list(distribution = "laplace")),
    tr_row("fastcpd", "mean", "poisson",
           "fastcpd::fastcpd.poisson(cbind(y, 1))",
           list(family = "poisson")),
    tr_row("fastcpd", "mean", "binomial",
           "fastcpd::fastcpd.binomial(cbind(y, 1))",
           list(family = "binomial")),
    tr_row("fastcpd", "mean", "exponential",
           "fastcpd::fastcpd.exponential(y)",
           list(family = "exponential")),
    tr_row("fastcpd", "regression", "gaussian",
           "fastcpd::fastcpd.lm(cbind(y, X))", list(family = "lm")),
    tr_row("fastcpd", "regression", "poisson",
           "fastcpd::fastcpd.poisson(cbind(y, X))",
           list(family = "poisson")),
    tr_row("fastcpd", "regression", "binomial",
           "fastcpd::fastcpd.binomial(cbind(y, X))",
           list(family = "binomial")),
    tr_row("smuce", "mean", "poisson",
           "stepR::smuceR(family = \"poisson\")",
           list(family = "poisson")),
    tr_row("smuce", "mean", "binomial",
           "stepR::smuceR(family = \"binomial\", param = 1)",
           list(family = "binomial")),
    tr_row("cpm", "mean", "gaussian",
           "cpm::processStream(cpmType = \"Student\")",
           list(cpm_type = "Student")),
    tr_row("cpm", "var", "gaussian",
           "cpm::processStream(cpmType = \"Bartlett\")",
           list(cpm_type = "Bartlett")),
    tr_row("cpm", "mean", "exponential",
           "cpm::processStream(cpmType = \"Exponential\")",
           list(cpm_type = "Exponential")),
    tr_row("cpm", "mean", "binomial",
           "cpm::processStream(cpmType = \"FET\", lambda = <lambda>)",
           list(cpm_type = "FET")),
    tr_row("bocpd", "mean", "poisson",
           "ocp::onlineCPD(probModel = list(\"p\"))",
           list(probModel = list("p"),
                init_params = list(list(a = 1, b = 1)))),
    tr_row("segmented", "slope", "poisson",
           "segmented::segmented(glm(y ~ t, family = poisson))",
           list(family = "poisson")),
    tr_row("segmented", "slope", "binomial",
           "segmented::segmented(glm(y ~ t, family = binomial))",
           list(family = "binomial")),
    tr_row("segmented", "regression", "gaussian",
           "segmented::segmented(lm(formula), seg.Z = <seg_z>)",
           list(family = "gaussian")),
    tr_row("segmented", "regression", "poisson",
           "segmented::segmented(glm(formula, family = poisson))",
           list(family = "poisson")),
    tr_row("segmented", "regression", "binomial",
           "segmented::segmented(glm(formula, family = binomial))",
           list(family = "binomial")),
    tr_row("strucchange", "regression", "gaussian",
           "strucchange::breakpoints(formula)", list())
  ))
  do.call(rbind, rows)
}

# Internal: describe a method's default (Gaussian) route as an engine call,
# from the arguments the dispatcher derives for it.
#' @noRd
default_route_text <- function(method, change_in, engine, wrapper) {
  derived <- derived_args_for(method, change_in, NULL)
  derived <- derived[!vapply(derived, is.null, logical(1))]
  if (method %in% c("pelt", "binseg", "segneigh", "amoc")) {
    fun <- switch(change_in, mean = "cpt.mean", var = "cpt.var",
                  meanvar = "cpt.meanvar", "cpt.mean")
    return(paste0("changepoint::", fun, "(method = \"",
                  switch(method, pelt = "PELT", binseg = "BinSeg",
                         segneigh = "SegNeigh", amoc = "AMOC"), "\")"))
  }
  if (method == "binsegrcpp") {
    return(paste0("binsegRcpp::binseg(\"",
                  if (change_in == "meanvar") "meanvar_norm" else "mean_norm",
                  "\")"))
  }
  args <- if (length(derived)) {
    paste0(names(derived), " = ",
           vapply(derived, function(v) {
             if (is.character(v)) paste0("\"", v, "\"") else format(v)
           }, character(1)), collapse = ", ")
  } else {
    ""
  }
  paste0(wrapper, "(", args, ")")
}

# Internal: the full translation table, one row per legal (method,
# change_in, family). `family` is NA for a distribution-free method, whose
# rows say what `family = NULL` runs.
#' @noRd
family_translation <- function() {
  reg <- builtin_registry_core()
  free <- distribution_free_methods()
  base <- lapply(seq_len(nrow(reg)), function(i) {
    m <- reg$method[i]
    cis <- reg$supports[[i]]
    do.call(rbind, lapply(cis, function(ci) {
      tr_row(m, ci, if (m %in% free) NA_character_ else "gaussian",
             default_route_text(m, ci, reg$engine[i], reg$wrapper[i]))
    }))
  })
  base <- do.call(rbind, base)
  explicit <- family_rows_explicit()
  # An explicit row replaces the generated one for the same combination.
  key <- function(d) paste(d$method, d$change_in, d$family)
  base <- base[!key(base) %in% key(explicit), , drop = FALSE]
  out <- rbind(base, explicit)
  out <- out[order(match(out$method, reg$method), out$change_in,
                   match(out$family, family_levels())), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Internal: the families each built-in method accepts, derived from the
# translation table so there is one list rather than two.
#' @noRd
method_families <- function() {
  tab <- family_translation()
  reg <- builtin_registry_core()
  stats::setNames(lapply(reg$method, function(m) {
    f <- unique(tab$family[tab$method == m])
    f <- f[!is.na(f)]
    f[order(match(f, family_levels()))]
  }), reg$method)
}

#' Distribution families, and what each request runs
#'
#' The \code{family} argument of \code{\link{cpt_detect}()} names the
#' distribution a segment is modelled with, so count data, waiting times
#' and binary series can be segmented with a cost written for them rather
#' than a Gaussian one. \code{cpt_families()} is the table behind it: every
#' legal combination of \code{method}, \code{change_in} and \code{family},
#' and the engine call each becomes.
#'
#' @param method Optional method name(s) to restrict the table to.
#' @return A tibble with one row per legal combination: \code{method},
#'   \code{change_in}, \code{family} (\code{NA} for a distribution-free
#'   method, which takes no family), \code{engine_call} (what is run) and
#'   \code{data} (what the family requires of the series: \code{"counts"},
#'   \code{"binary"}, \code{"positive"} or \code{NA}).
#'
#' @section The families:
#' \describe{
#'   \item{\code{"gaussian"}}{The default of every parametric method. On
#'     \code{"cpm"} it also switches from the nonparametric statistics to
#'     the Gaussian ones (Student, Bartlett).}
#'   \item{\code{"poisson"}}{Counts: \code{change_in = "mean"} is a change
#'     in the rate. Needs non-negative whole numbers.}
#'   \item{\code{"binomial"}}{Binary outcomes: a change in the success
#'     probability. Needs 0/1 data.}
#'   \item{\code{"exponential"}}{Waiting times: a change in the rate (the
#'     hazard), so \code{change_in = "mean"}. Needs positive data.}
#'   \item{\code{"gamma"}}{Positive data with a known shape (pass
#'     \code{shape}, default 1): a change in the scale.}
#'   \item{\code{"laplace"}}{Heavy tails: a change in the median and the
#'     scale (\code{change_in = "meanvar"}).}
#'   \item{\code{"l1"}}{The absolute-loss cost: a change in the median,
#'     robust to outliers (\code{change_in = "mean"}).}
#' }
#' A single-parameter family has one thing to change, so Poisson,
#' binomial, exponential and gamma requests use \code{change_in = "mean"}.
#'
#' @section Why an argument and not a model grammar:
#' A grammar that marks the changing parameter
#' (\code{poisson(rate = NA)}, \code{normal(mean = NA, sd = 1)}) would unify
#' \code{change_in}, \code{family} and the formula interface in one
#' expression, and measured against the fifty methods it fits 42 of them
#' cleanly. It is deferred to 1.0, where \code{change_in} can be replaced
#' rather than joined by a second idiom, and this table is the first half of
#' it: \code{change_in = "mean", family = "poisson"} already means
#' \code{poisson(rate = NA)} internally.
#'
#' @seealso \code{\link{cpt_detect}()}, \code{\link{cpt_methods}()} (its
#'   \code{families} column), \code{\link{cpt_simulate}()} to generate
#'   data from each family.
#' @export
#' @family method registry
#' @examples
#' cpt_families("pelt")
#' # every method that can fit a Poisson rate
#' unique(subset(cpt_families(), family == "poisson")$method)
#'
#' set.seed(1)
#' counts <- c(rpois(100, 3), rpois(100, 9))
#' cpt_detect(counts, method = "pelt", family = "poisson")
cpt_families <- function(method = NULL) {
  tab <- family_translation()
  if (!is.null(method)) {
    method <- as.character(method)
    unknown <- setdiff(method, tab$method)
    if (length(unknown)) {
      cpt_match_arg(unknown[1], unique(tab$method), name = "method",
                    class = "unknown_method",
                    hint = "Run `cpt_methods()` for the full list.")
    }
    tab <- tab[tab$method %in% method, , drop = FALSE]
  }
  data_rule <- vapply(tab$family, function(f) {
    if (is.na(f)) return(NA_character_)
    family_data_rule(f) %||% NA_character_
  }, character(1))
  tibble::tibble(method = tab$method, change_in = tab$change_in,
                 family = tab$family, engine_call = tab$engine_call,
                 data = unname(data_rule))
}

# Internal: resolve a (method, change_in, family) request to the arguments
# the dispatcher derives. Returns NULL for `family = NULL` (no translation:
# the method's own default). Refuses, by name, a family the method cannot
# fit or a combination it does not offer.
#' @noRd
resolve_family <- function(method, change_in, family, registered = FALSE) {
  if (is.null(family)) return(NULL)
  family <- cpt_match_arg(family, family_levels(), name = "family")
  if (registered) {
    # A registration declares no families; the request goes to the
    # detector as an ordinary argument and it decides.
    return(list(family = family, args = list(family = family),
                reported = change_in, route = NA_character_))
  }
  if (method %in% distribution_free_methods()) {
    cpt_abort("`", method, "` is distribution-free: it models no family, so ",
              "`family = \"", family, "\"` has nothing to change. Drop the ",
              "argument, or pick a method that fits a ", family, " cost: ",
              paste(methods_with_family(family), collapse = ", "), ".",
              class = "unsupported",
              data = list(method = method, requested = family,
                          supported = character(0)))
  }
  tab <- family_translation()
  fams <- unique(tab$family[tab$method == method & !is.na(tab$family)])
  if (!family %in% fams) {
    cpt_abort("Method `", method, "` does not fit a ", family, " cost; its ",
              "families are ", paste(fams, collapse = ", "), ". Methods ",
              "that do: ", paste(methods_with_family(family), collapse = ", "),
              ". See cpt_families().", class = "unsupported",
              data = list(method = method, requested = family,
                          supported = fams))
  }
  hit <- tab[tab$method == method & tab$family %in% family &
               tab$change_in == change_in, , drop = FALSE]
  if (nrow(hit) == 0L) {
    legal <- tab$change_in[tab$method == method & tab$family %in% family]
    cpt_abort("`change_in = \"", change_in, "\"` with `family = \"", family,
              "\"` is not offered by `", method, "`. With that family it ",
              "detects: ", paste0("\"", unique(legal), "\"", collapse = ", "),
              if (family %in% c("poisson", "binomial", "exponential",
                                "gamma")) {
                paste0(" (a ", family, " model has one parameter, so its ",
                       "change is `change_in = \"mean\"`)")
              } else "", ".", class = "unsupported",
              data = list(method = method, requested = change_in,
                          supported = unique(legal), family = family))
  }
  list(family = family, args = hit$args[[1]],
       reported = hit$reported_change_in[1], route = hit$engine_call[1])
}

# Internal: the built-in methods that fit a family.
#' @noRd
methods_with_family <- function(family) {
  tab <- family_translation()
  unique(tab$method[tab$family %in% family])
}

# Internal: check a series against what its family needs, naming the
# problem the engine would otherwise report in its own words.
#' @noRd
check_family_data <- function(x, family, method) {
  rule <- family_data_rule(family)
  if (is.null(rule)) return(invisible(TRUE))
  v <- if (is.matrix(x) || is.data.frame(x)) as.numeric(as.matrix(x)) else
    as.numeric(x)
  v <- v[!is.na(v)]
  bad <- switch(rule,
    counts = sum(v < 0 | abs(v - round(v)) > 1e-8),
    binary = sum(!v %in% c(0, 1)),
    positive = sum(v <= 0)
  )
  if (bad > 0) {
    what <- switch(rule,
      counts = "non-negative whole numbers (counts)",
      binary = "0/1 outcomes",
      positive = "positive values"
    )
    cpt_abort("`family = \"", family, "\"` needs ", what, "; ", bad, " of ",
              length(v), " values are not. Check the family, or transform ",
              "the series first.", class = "bad_type",
              data = list(family = family, method = method))
  }
  invisible(TRUE)
}

# Internal: each method's modelling choices, argument by argument. A value
# outside the set is refused by name (with the nearest valid value) instead
# of reaching the engine, where six of these arguments are swallowed by an
# open `...` and a typo becomes a silent revert to the default.
#' @noRd
method_choices <- function() {
  cp_stats <- c("Normal", "CUSUM", "CSS", "Poisson", "Gamma", "Exponential")
  cp <- list(test.stat = cp_stats)
  trend <- list(test = c("pettitt", "buishand", "snht"))
  fun <- list(statistic = c("Tn", "Mn"),
              critical = c("simulation", "resample", "welch"),
              type = c("segmentation", "single"))
  breakfast <- list(type = c("const", "lin.cont", "lin.discont"))
  list(
    pelt = cp, binseg = cp, segneigh = cp, amoc = cp,
    ecp = list(algorithm = c("divisive", "agglo")),
    wbs2 = breakfast, tguh = breakfast,
    smuce = list(family = c("gauss", "hsmuce", "poisson", "binomial")),
    hsmuce = list(family = c("gauss", "hsmuce")),
    bcp = list(boundaryType = c("node", "edge")),
    beast = list(precPriorType = c("componentwise", "uniform", "constant",
                                   "orderwise"),
                 season = c("harmonic", "svd", "dummy", "none")),
    cpm = list(cpm_type = cpm_types()),
    kcp = list(running_stat = c("mean", "var", "autocorr", "corr")),
    npmojo = list(kernel.f = c("quad.exp", "gauss", "euclidean", "laplace",
                               "sine")),
    sn = list(parameter = c("mean", "variance", "acf", "bivcor")),
    geomcp = list(test.stat = c("Normal", "Empirical"),
                  mapping = c("both", "distance", "angle")),
    envcpt = list(models = c("mean", "meancpt", "meanar1", "meanar2",
                             "meanar1cpt", "meanar2cpt", "trend", "trendcpt",
                             "trendar1", "trendar2", "trendar1cpt",
                             "trendar2cpt"),
                  criterion = c("AIC", "BIC")),
    fastcpd = list(family = fastcpd_families()),
    nsp = list(variant = c("poly", "selfnorm", "ar", "tvreg"),
               thresh.type = c("univ", "sim")),
    kwc = list(algorithm = c("fkwc", "dwbs")),
    fmean = fun,
    fcov = c(list(target = c("covariance", "trace", "eigenjoint",
                             "eigensingle")), fun),
    fabisearch = list(testtype = c("t-test", "ks", "wilcox")),
    bfast = list(season = c("harmonic", "dummy", "none")),
    pettitt = trend, buishand = trend, snht = trend,
    binsegrcpp = list(distribution = c("mean_norm", "meanvar_norm",
                                       "poisson", "laplace", "l1"))
  )
}

# Internal: the arguments that take several values at once.
#' @noRd
multi_choice_args <- function() c("models")

# Internal: render a method's choices for cpt_methods().
#' @noRd
format_choices <- function(ch) {
  if (is.null(ch) || length(ch) == 0L) return(NA_character_)
  paste(vapply(names(ch), function(nm) {
    paste0(nm, " = ", paste(ch[[nm]], collapse = " | "))
  }, character(1)), collapse = "; ")
}

# Internal: validate every choice argument the caller passed through `...`
# against the method's vocabulary, returning `dots` with any partial match
# completed (an engine receives the value it documents, not an
# abbreviation it may not accept).
#' @noRd
validate_choice_args <- function(method, dots) {
  ch <- method_choices()[[method]]
  if (is.null(ch) || length(dots) == 0L) return(dots)
  for (nm in intersect(names(dots), names(ch))) {
    v <- dots[[nm]]
    if (!is.character(v)) next
    dots[[nm]] <- cpt_match_arg(v, ch[[nm]],
                                several.ok = nm %in% multi_choice_args(),
                                name = nm)
  }
  dots
}

# Internal: the engine argument that holds each method's minimum segment
# length, and its unit. `min_segment =` on cpt_detect() is translated into
# it; a method with no such argument refuses the request by name.
#' @noRd
min_segment_args <- function() {
  # Not segneigh: changepoint implements no minimum segment for SegNeigh
  # and refuses one above 1.
  cp <- list(arg = "minseglen", unit = "count")
  list(
    pelt = cp, binseg = cp, amoc = cp, np = cp,
    ecp = list(arg = "min_size", unit = "count"),
    cpop = list(arg = "minseglen", unit = "count"),
    beast = list(arg = "tseg.min", unit = "count"),
    geomcp = list(arg = "msl", unit = "count"),
    strucchange = list(arg = "h", unit = "count"),
    envcpt = list(arg = "minseglen", unit = "count"),
    bfast = list(arg = "h", unit = "fraction"),
    hdcov = list(arg = "delta", unit = "count"),
    network = list(arg = "delta", unit = "count"),
    var = list(arg = "delta", unit = "count"),
    hdreg = list(arg = "delta", unit = "count"),
    fabisearch = list(arg = "min_dist", unit = "count"),
    binsegrcpp = list(arg = "min_segment_length", unit = "count")
  )
}

# Internal: translate `min_segment` (observations) into the method's own
# argument. Returns a named list to merge into the derived arguments.
#' @noRd
min_segment_translate <- function(method, min_segment, n) {
  spec <- min_segment_args()[[method]]
  if (is.null(spec)) {
    have <- names(min_segment_args())
    cpt_abort("Method `", method, "` has no minimum-segment argument, so ",
              "`min_segment` cannot be passed to it. Methods that take one: ",
              paste(have, collapse = ", "), ".", class = "unsupported",
              data = list(method = method, requested = "min_segment",
                          supported = have))
  }
  value <- if (identical(spec$unit, "fraction")) min_segment / n else
    as.integer(min_segment)
  stats::setNames(list(value), spec$arg)
}

# Internal: the methods that take a formula with covariates. Every
# univariate method takes an intercept-only formula (`y ~ 1`), which is
# just the response as a series.
#' @noRd
formula_methods <- function() c("strucchange", "segmented", "fastcpd")

# Internal: fastcpd's families, as its documentation lists them, less
# `custom` (a user-supplied cost is a different design, see
# cpt_register_method()).
#' @noRd
fastcpd_families <- function() {
  c("mean", "variance", "meanvariance", "ar", "arma", "arima", "garch",
    "var", "lm", "lasso", "poisson", "binomial", "exponential")
}

# Internal: what each engine does with a missing value, as measured by
# data-raw/na_handling.R (see there for the probe and its thresholds).
# Filled from the measurement, never from documentation: §171's point was
# that the engines' behaviours are four, not two, and that the two built
# for gappy data were the ones the blanket refusal disabled.
#' @noRd
na_handling_table <- function() {
  c(beast = "native", segmented = "native",
    strucchange = "compacts", fastcpd = "compacts",
    mosum = "silent_loss", bcp = "silent_loss", esac = "silent_loss",
    pilliat = "silent_loss", var = "silent_loss", kwc = "silent_loss",
    bfast = "silent_loss")
}

# Internal: each engine's own changepoint convention, which its wrapper
# translates to this package's "left" (the last observation before the
# change): "right" engines report the first observation after it,
# "continuous" ones a location on a continuous scale that is rounded, and
# "design" (EnvCpt) rows of a lagged regression design for its
# autoregressive models. test-060-convention.R checks the translations on
# a change no engine can misplace.
#' @noRd
upstream_conventions <- function() {
  c(ecp = "right", kcp = "right", bocpd = "right", beast = "right",
    taylor = "right", mcp = "continuous", segmented = "continuous",
    cpop = "continuous", envcpt = "design")
}
