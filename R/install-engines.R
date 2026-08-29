#' Install the engines behind a family of methods
#'
#' The package wraps a lot of upstream engines, all of them in
#' \code{Suggests} so that installing \pkg{ggchangepoint} does not drag in
#' dozens of packages nobody asked for. The cost of that choice is a
#' discovery problem: \code{cpt_detect(x, method = "smuce")} tells you to
#' install \pkg{stepR}, but only one package at a time. This installs a whole
#' family in one call.
#'
#' @param bundle Which family to install:
#'   \describe{
#'     \item{\code{"core"}}{the engines the common methods need: \pkg{fpop},
#'       \pkg{wbs}, \pkg{breakfast}, \pkg{not}, \pkg{mosum}, \pkg{IDetect},
#'       \pkg{stepR}.}
#'     \item{\code{"bayesian"}}{\pkg{bcp}, \pkg{ocp}, \pkg{Rbeast}.}
#'     \item{\code{"nonparametric"}}{\pkg{cpm}, \pkg{kcpRS},
#'       \pkg{CptNonPar}, \pkg{SNSeg}.}
#'     \item{\code{"highdim"}}{\pkg{InspectChangepoint}, \pkg{ocd},
#'       \pkg{changepoint.geo}, \pkg{HDCD}, \pkg{changepoints}.}
#'     \item{\code{"functional"}}{\pkg{fChange}, \pkg{KWCChangepoint},
#'       \pkg{fabisearch}.}
#'     \item{\code{"regression"}}{\pkg{strucchange}, \pkg{segmented},
#'       \pkg{EnvCpt}, \pkg{DeCAFS}, \pkg{cpop}, \pkg{fastcpd}.}
#'     \item{\code{"inference"}}{\pkg{nsp}, \pkg{crossvalidationCP},
#'       \pkg{changepoint.influence}, \pkg{penaltyLearning}.}
#'     \item{\code{"applied"}}{\pkg{trend}, \pkg{ChangePointTaylor},
#'       \pkg{bfast}, \pkg{wbsts}, \pkg{binsegRcpp}.}
#'     \item{\code{"time"}}{index and coercion support: \pkg{zoo},
#'       \pkg{xts}, \pkg{tsibble}, \pkg{tsbox}.}
#'     \item{\code{"reporting"}}{\pkg{gt}, \pkg{ggrepel}, \pkg{plotly},
#'       \pkg{ggiraph}, \pkg{progressr}, \pkg{jsonlite}.}
#'     \item{\code{"all"}}{every engine and extra the package knows about.}
#'   }
#'   Several bundles may be given at once.
#' @param dry_run Report what would be installed without installing
#'   anything. Defaults to \code{FALSE}.
#' @param ... Passed to \code{\link[utils]{install.packages}()}.
#'
#' @return Invisibly, a tibble with one row per package (\code{package},
#'   \code{bundle}, \code{installed_before}, \code{installed_after}).
#' @seealso \code{\link{cpt_methods}()} for the per-method installation
#'   status.
#' @export
#' @examples
#' cpt_install_engines("bayesian", dry_run = TRUE)
cpt_install_engines <- function(bundle = "core", dry_run = FALSE, ...) {
  validate_flag(dry_run, "dry_run")
  bundles <- engine_bundles()
  bundle <- match.arg(bundle, c(names(bundles), "all"), several.ok = TRUE)
  if ("all" %in% bundle) bundle <- names(bundles)

  want <- do.call(rbind, lapply(bundle, function(b) {
    tibble::tibble(package = bundles[[b]], bundle = b)
  }))
  want <- want[!duplicated(want$package), , drop = FALSE]
  want$installed_before <- vapply(want$package, engine_installed,
                                  logical(1))

  todo <- want$package[!want$installed_before]
  if (length(todo) == 0) {
    message("Every package in ", paste(bundle, collapse = ", "),
            " is already installed.")
    want$installed_after <- TRUE
    return(invisible(want))
  }
  if (isTRUE(dry_run)) {
    message("Would install: ", paste(todo, collapse = ", "))
    want$installed_after <- want$installed_before
    return(invisible(want))
  }
  utils::install.packages(todo, ...)
  want$installed_after <- vapply(want$package, engine_installed,
                                 logical(1))
  failed <- want$package[!want$installed_after]
  if (length(failed) > 0) {
    warning("These packages are still not installed: ",
            paste(failed, collapse = ", "),
            ". Some need system libraries (JAGS for mcp, for example).",
            call. = FALSE)
  }
  invisible(want)
}

# Internal: the bundle definitions. Derived from the registry where it can
# be -- the engine column is the source of truth for which package a method
# needs -- and hand-grouped only for the non-engine extras.
#' @noRd
engine_bundles <- function() {
  reg <- builtin_registry()
  eng <- function(methods) {
    unique(reg$engine[reg$method %in% methods])
  }
  list(
    core = eng(c("fpop", "wbs", "wbs2", "not", "mosum", "idetect", "tguh",
                 "smuce", "hsmuce")),
    bayesian = eng(c("bcp", "bocpd", "beast", "mcp")),
    nonparametric = eng(c("cpm", "kcp", "npmojo", "sn")),
    highdim = eng(c("inspect", "ocd", "geomcp", "esac", "pilliat", "hdcov",
                    "network", "var")),
    functional = eng(c("fmean", "fcov", "kwc", "fabisearch")),
    regression = eng(c("strucchange", "segmented", "envcpt", "decafs",
                       "cpop", "fastcpd")),
    inference = c("nsp", "crossvalidationCP", "changepoint.influence",
                  "penaltyLearning"),
    applied = eng(c("pettitt", "buishand", "snht", "taylor", "bfast",
                    "wbsts", "binsegrcpp")),
    time = c("zoo", "xts", "tsibble", "tsbox"),
    reporting = c("gt", "ggrepel", "plotly", "ggiraph", "progressr",
                  "jsonlite", "patchwork")
  )
}
