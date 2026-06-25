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
  }
})
