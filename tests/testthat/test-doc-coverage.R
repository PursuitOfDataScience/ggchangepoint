test_that("all exported functions appear in README", {
  skip_on_cran()

  ns_path <- system.file("NAMESPACE", package = "ggchangepoint")
  ns_lines <- readLines(ns_path)
  exports <- gsub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", ns_lines, value = TRUE))

  readme_paths <- c(
    file.path("..", "..", "README.md"),
    "README.md"
  )
  readme_path <- readme_paths[file.exists(readme_paths)][1]
  if (is.na(readme_path)) skip("README.md not found")
  readme <- readLines(readme_path)

  missing <- character()
  for (ex in exports) {
    # For S3 methods (e.g. summary.ggcpt), also check the generic name (summary)
    search_names <- c(ex, sub("\\..*$", "", ex))
    backtick_call <- any(vapply(search_names, function(n) {
      any(grepl(sprintf("`%s(", n), readme, fixed = TRUE))
    }, logical(1)))
    backtick_name <- any(vapply(search_names, function(n) {
      any(grepl(sprintf("`%s`", n), readme, fixed = TRUE))
    }, logical(1)))
    bare_call <- any(vapply(search_names, function(n) {
      any(grepl(sprintf("%s(", n), readme, fixed = TRUE))
    }, logical(1)))
    in_readme <- backtick_call || backtick_name || bare_call
    if (!in_readme) missing <- c(missing, ex)
  }

  if (length(missing) > 0) {
    fail(paste(
      sprintf("Exports missing from README (%d):", length(missing)),
      paste(missing, collapse = ", "),
      sep = "\n"
    ))
  } else {
    succeed()
  }
})

test_that("every registry wrapper is exported and documented", {
  ns_path <- system.file("NAMESPACE", package = "ggchangepoint")
  skip_if(!nzchar(ns_path))
  exports <- gsub("^export\\(([^)]+)\\)$", "\\1",
                  grep("^export\\(", readLines(ns_path), value = TRUE))

  reg <- ggchangepoint:::builtin_registry()
  wrappers <- unique(reg$wrapper)
  # A wrapper reached only through cpt_detect() still has to be callable and
  # documented on its own: an @export block that silently detaches (because
  # a helper was inserted between it and its function) leaves dispatch
  # working while the public entry point disappears.
  expect_setequal(intersect(wrappers, exports), wrappers)
  for (w in wrappers) {
    expect_true(nzchar(utils::help(w, package = "ggchangepoint")[1]),
                info = paste("no help page for", w))
  }

  # and nothing internal leaked out
  internal <- c("is_power_of_two", "registry_snapshot", "registry_restore",
                "with_session_registry", "native_bounds", "cpt_alt_text",
                "cpt_label_palette", "normalise_dataset",
                "pilliat_dimension_guard")
  expect_equal(intersect(internal, exports), character(0))
})

test_that("every registry engine is declared in Imports or Suggests", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "ggchangepoint"))
  field <- function(nm) {
    if (!nm %in% colnames(desc)) return(character(0))
    trimws(gsub("\\s*\\(.*\\)", "", strsplit(desc[1, nm], ",")[[1]]))
  }
  declared <- c(field("Imports"), field("Suggests"), field("Depends"),
                "stats", "utils", "tools", "methods", "graphics", "grDevices")
  engines <- unique(ggchangepoint:::builtin_registry()$engine)
  # A wrapper reaches its engine through requireNamespace(<variable>), which
  # R CMD check cannot see, so an engine dropped from DESCRIPTION would only
  # surface as a runtime failure on a machine that happens not to have it.
  expect_equal(setdiff(engines, declared), character(0))

  # and the planned engines must NOT be declared: they are not on CRAN
  planned <- ggchangepoint:::planned_methods()$engine
  expect_equal(intersect(planned, c(field("Imports"), field("Suggests"))),
               character(0))
})

test_that("every column a result tibble returns is named in its @return", {
  skip_on_cran()
  # A documented @return that omits a column is a gap R CMD check cannot
  # see: cpt_regions() carried nsp_wrapper()'s `value`, cpt_scale_space()
  # returned `detected` alongside `significant`, cpt_label_error() returned
  # `series` and cpt_benchmark() returned `n_annotators` -- four columns a
  # reader of the help page did not know existed.
  db <- tryCatch(tools::Rd_db("ggchangepoint"), error = function(e) NULL)
  if (is.null(db) || length(db) == 0) {
    db <- tryCatch(tools::Rd_db(dir = testthat::test_path("..", "..")),
                   error = function(e) NULL)
  }
  skip_if(is.null(db) || length(db) == 0, "no Rd database available")

  value_text <- function(topic) {
    rd <- db[[paste0(topic, ".Rd")]]
    if (is.null(rd)) return(NA_character_)
    tags <- vapply(rd, function(z) attr(z, "Rd_tag") %||% "", character(1))
    i <- which(tags == "\\value")
    if (length(i) == 0) return(NA_character_)
    paste(unlist(rd[[i[1]]]), collapse = " ")
  }

  set.seed(61)
  x <- c(stats::rnorm(120), stats::rnorm(120, 4))
  fit <- cpt_detect(x, method = "pelt")
  labs <- cpt_labels(c(110, 200), c(130, 220))

  results <- list(
    cpt_metrics = cpt_metrics(120, 120, n = 240),
    cpt_test = suppressWarnings(cpt_test(fit)),
    cpt_confint = cpt_confint(fit),
    cpt_label_error = cpt_label_error(fit, labs),
    cpt_labels = labs,
    cpt_leverage = cpt_leverage(fit),
    cpt_simulate = cpt_simulate(200, changepoints = 100, seed = 1),
    signal_blocks = signal_blocks(n = 300, seed = 1),
    cpt_batch = cpt_batch(cbind(a = x, b = rev(x)), method = "pelt"),
    glance.ggcpt = glance(fit)
  )
  if (requireNamespace("mosum", quietly = TRUE)) {
    results$cpt_scale_space <- cpt_scale_space(x, bandwidths = c(20, 40))
  }
  if (requireNamespace("nsp", quietly = TRUE)) {
    results$cpt_regions <- cpt_regions(nsp_wrapper(x))
  }

  for (topic in names(results)) {
    val <- value_text(topic)
    if (is.na(val)) next
    undocumented <- setdiff(names(results[[topic]]),
                            unlist(strsplit(val, "[^A-Za-z0-9_.]+")))
    expect_equal(undocumented, character(0), info = topic)
  }
})
