# Scaffold a new engine wrapper (roadmap §83.1-2): the wrapper file, a test
# file, and the registry row to paste into builtin_registry_core(). Run from
# the package root:
#
#   Rscript data-raw/use_cpt_wrapper.R <method> <package> <function>
#
# e.g. `Rscript data-raw/use_cpt_wrapper.R jointseg jointseg jointSeg`.
# Nothing is overwritten: the script stops if either file exists. The
# skeleton marks every place that needs a decision with TODO; see
# .github/CONTRIBUTING.md for what each one is.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("usage: Rscript data-raw/use_cpt_wrapper.R <method> <package> ",
       "<function>", call. = FALSE)
}
method <- args[1]
pkg <- args[2]
fun <- args[3]
if (!grepl("^[a-z][a-z0-9]*$", method)) {
  stop("`method` must be lower-case letters and digits, as cpt_detect() ",
       "takes it: got ", method, call. = FALSE)
}
wrapper <- paste0(method, "_wrapper")
r_file <- file.path("R", paste0("wrap-", method, ".R"))
t_file <- file.path("tests", "testthat", paste0("test-wrap-", method, ".R"))
for (f in c(r_file, t_file)) {
  if (file.exists(f)) stop(f, " exists; not overwriting it.", call. = FALSE)
}

fill <- function(lines) {
  lines <- gsub("@METHOD@", method, lines, fixed = TRUE)
  lines <- gsub("@WRAPPER@", wrapper, lines, fixed = TRUE)
  lines <- gsub("@PKG@", pkg, lines, fixed = TRUE)
  gsub("@FUN@", fun, lines, fixed = TRUE)
}

wrapper_src <- fill(c(
  "#' @PKG@ wrapper: TODO one line on what the method detects",
  "#'",
  "#' Wraps \\code{@PKG@::@FUN@()}. TODO: what it detects, what it assumes",
  "#' about the noise, and what it returns beyond the changepoints.",
  "#'",
  "#' @param x A numeric vector.",
  "#' @param ... Additional arguments passed to \\code{@PKG@::@FUN@()}.",
  "#' @return A \\code{ggcpt} object.",
  "#' @references",
  "#' \\insertRef{TODO-bibtex-key}{ggchangepoint}",
  "#' @export",
  "#' @family changepoint engines",
  "#' @examplesIf requireNamespace(\"@PKG@\", quietly = TRUE)",
  "#' set.seed(1)",
  "#' @WRAPPER@(c(rnorm(100), rnorm(100, 3)))",
  "@WRAPPER@ <- function(x, ...) {",
  "  # 1. Check the input and load the engine.",
  "  validate_data(x)",
  "  data_vec <- as_uni_vector(x, \"@METHOD@\")",
  "  need_pkg(\"@PKG@\")",
  "",
  "  # 2. Call the engine.",
  "  fit <- @PKG@::@FUN@(data_vec, ...)",
  "",
  "  # 3. The changepoints as the LAST observation before each change. TODO:",
  "  # if the engine reports the first observation after, subtract one and",
  "  # record \"right\" in upstream_conventions().",
  "  cp <- integer(0)  # TODO: extract from `fit`",
  "",
  "  # 4. Build the result.",
  "  ggcpt_build(",
  "    data_vec, cp,",
  "    method = \"@METHOD@\",",
  "    change_in = \"mean\",  # TODO",
  "    penalty = list(type = NA_character_, value = NA_real_),  # TODO",
  "    fit = fit,",
  "    call = match.call()",
  "  )",
  "}"
))

test_src <- fill(c(
  "test_that(\"@METHOD@ finds an unmistakable change where it is\", {",
  "  skip_on_cran()",
  "  skip_if_not(engine_usable(\"@PKG@\"))",
  "  set.seed(1)",
  "  x <- c(stats::rnorm(100), stats::rnorm(100, 5))",
  "  fit <- cpt_detect(x, method = \"@METHOD@\")",
  "  expect_s3_class(fit, \"ggcpt\")",
  "  expect_identical(fit$method, \"@METHOD@\")",
  "  # The package's convention: the last observation of the old segment.",
  "  expect_true(any(abs(fit$changepoints$cp - 100) <= 1))",
  "  expect_equal(sum(fit$segments$end - fit$segments$start + 1), 200)",
  "})",
  "",
  "test_that(\"@METHOD@ refuses what it cannot fit, by class\", {",
  "  skip_if_not(engine_usable(\"@PKG@\"))",
  "  expect_error(cpt_detect(c(1, NA, 3, 4, 5), method = \"@METHOD@\"),",
  "               class = \"ggchangepoint_error\")",
  "})"
))

writeLines(wrapper_src, r_file)
writeLines(test_src, t_file)
message("Wrote ", r_file, " and ", t_file, ".")
message("Paste this row into builtin_registry_core() in R/registry.R and set ",
        "the flags:")
cat(sprintf(paste0('    "%s", %s"mean", %s"%s", %s"mean", %s"%s", ',
                   'FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, ',
                   'FALSE,\n'),
            method, "", "", pkg, "", "", wrapper))
message("Then: a cpt_references() row in R/cite.R, a BibTeX entry in ",
        "inst/REFERENCES.bib, the vocabulary in R/vocabulary.R, and the ",
        "measurements (see .github/CONTRIBUTING.md).")
