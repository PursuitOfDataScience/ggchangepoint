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

test_that("no test reaches a Suggests engine outside a guarded block", {
  skip_on_cran()
  # The DESCRIPTION promises the package works with its Imports alone, and
  # only an Imports-only check exercises that promise. CI installs every
  # suggested package, so a test that calls a Suggests engine without a
  # guard passes on all five CI jobs and fails only there -- which has now
  # happened three times running ('ggrepel', then 'cpm', then 'fpop'). This
  # catches it in the ordinary suite instead.
  #
  # Exact, not heuristic, in three ways. The engine of every method comes
  # from the registry, so a method whose engine is an Imports package needs
  # no guard. Only real call forms count -- `method = "x"` and
  # `x_wrapper()` -- because the bare name collides with other vocabulary
  # (`change_in = "var"` is not a call to the `var` method, and several
  # tests list method names as data). And a call inside `expect_error()` is
  # exempt: it asserts a refusal, which has to work *without* the engine,
  # so guarding it would delete the coverage that matters most on a minimal
  # installation.
  desc <- read.dcf(system.file("DESCRIPTION", package = "ggchangepoint"))
  imports <- trimws(gsub("\\s*\\(.*\\)", "",
                         strsplit(desc[1, "Imports"], ",")[[1]]))
  reg <- ggchangepoint:::builtin_registry()
  free <- reg$engine %in% c(imports, "ggchangepoint")
  needs <- unique(reg$method[!free])
  wrap_needs <- unique(reg$wrapper[!free])
  call_forms <- function(m) {
    c(sprintf("method\\s*=\\s*\"%s\"", m),
      sprintf("methods\\s*=\\s*c\\([^)]*\"%s\"", m),
      sprintf("methods\\s*=\\s*\"%s\"", m))
  }

  offenders <- character()
  files <- list.files(test_path(), pattern = "^test-.*[.]R$", full.names = TRUE)
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    starts <- grep("^test_that\\(", src)
    if (length(starts) == 0) next
    ends <- c(starts[-1] - 1L, length(src))
    for (i in seq_along(starts)) {
      lines <- src[starts[i]:ends[i]]
      body <- paste(lines, collapse = "\n")
      if (grepl("skip_if_not_installed|requireNamespace|skip_if\\(", body)) next
      hits <- character()
      probe <- function(pats, label) {
        at <- unique(unlist(lapply(pats, function(p) grep(p, lines))))
        if (length(at) == 0) return(invisible(NULL))
        in_expect_error <- vapply(at, function(k) {
          any(grepl("expect_error\\(", lines[max(1, k - 1):k]))
        }, logical(1))
        if (!all(in_expect_error)) hits <<- c(hits, label)
        invisible(NULL)
      }
      for (m in needs) probe(call_forms(m), m)
      for (wp in wrap_needs) probe(sprintf("%s\\(", wp), wp)
      if (length(hits) > 0) {
        offenders <- c(offenders,
                       sprintf("%s:%d uses %s without skip_if_not_installed()",
                               basename(f), starts[i],
                               paste(unique(hits), collapse = ", ")))
      }
    }
  }
  expect_equal(offenders, character(0))
})

test_that("the vignette's planned-engine list matches the registry", {
  skip_on_cran()
  # This count has gone stale twice. The 0.5.0 audit found "a stale
  # planned-engine count" (S30), and `ggchangepoint.Rmd` still said "Four
  # rows carry status planned" with four engines listed while the table had
  # five -- and the chunk directly below the sentence prints cpt_methods()
  # in full, so a reader met the contradiction immediately.
  paths <- c(system.file("doc", "ggchangepoint.Rmd",
                         package = "ggchangepoint"),
             test_path("..", "..", "vignettes", "ggchangepoint.Rmd"))
  path <- paths[nzchar(paths) & file.exists(paths)][1]
  skip_if(is.na(path), "vignette source not available")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  planned <- ggchangepoint:::planned_methods()
  # the sentence that enumerates them, and the engines it names
  m <- regmatches(txt, regexpr(
    "rows carry status `\"planned\"`[^.]*\\.", txt))
  expect_length(m, 1L)
  named <- gsub("`", "", regmatches(m, gregexpr("`[A-Za-z][A-Za-z0-9.]*`", m))[[1]])
  expect_setequal(named, planned$engine)

  # and the number word in front of it
  words <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")
  said <- sub("^.*\\b(One|Two|Three|Four|Five|Six|Seven|Eight) rows carry.*$",
              "\\1", regmatches(txt, regexpr(
                "\\b(One|Two|Three|Four|Five|Six|Seven|Eight) rows carry", txt)))
  expect_equal(match(said, words), nrow(planned))
})

test_that("every vignette is reachable from another vignette", {
  skip_on_cran()
  # `monitoring` was linked from nothing: the feature tour's opening said
  # "Four companion vignettes" while listing five, and the six that exist
  # omitted streaming entirely, so a reader working through the vignettes in
  # R never reached a whole 0.5.0 feature area. Second instance of a
  # hand-written count contradicting the package, after the planned-engine
  # count above.
  dirs <- c(system.file("doc", package = "ggchangepoint"),
            test_path("..", "..", "vignettes"))
  dir <- dirs[nzchar(dirs) & dir.exists(dirs)][1]
  skip_if(is.na(dir), "vignette sources not available")
  files <- list.files(dir, pattern = "[.]Rmd$", full.names = TRUE)
  skip_if(length(files) < 2, "vignette sources not available")

  names(files) <- sub("[.]Rmd$", "", basename(files))
  referenced <- character()
  for (nm in names(files)) {
    txt <- paste(readLines(files[[nm]], warn = FALSE), collapse = "\n")
    targets <- unique(regmatches(txt, gregexpr('vignette\\("[a-z_]+"', txt))[[1]])
    targets <- gsub('vignette\\("|"', "", targets)
    referenced <- c(referenced, setdiff(targets, nm))
  }
  expect_equal(setdiff(names(files), unique(referenced)), character(0))

  # and the tour's own count of its companions
  tour <- files[["ggchangepoint"]]
  if (!is.na(tour)) {
    txt <- paste(readLines(tour, warn = FALSE), collapse = "\n")
    words <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")
    said <- regmatches(txt, regexpr(
      "\\b(One|Two|Three|Four|Five|Six|Seven|Eight) companion vignettes", txt))
    expect_length(said, 1L)
    said <- sub(" companion vignettes", "", said)
    expect_equal(match(said, words), length(files) - 1L)
  }
})

test_that("every prose method count matches the registry", {
  skip_on_cran()
  # `introduction.Rmd`'s opening paragraph still advertised "31 detection
  # methods" -- the 0.4.0 number, in the first sentence of the first
  # vignette a reader opens, for the release whose headline is 31 -> 50.
  # This number changes every release, so it is worth a test rather than a
  # proofread.
  #
  # Only *totalising* phrasings count. A first attempt matched any number
  # before "methods" and flagged the benchmarks article's "Ten methods
  # raise zero false alarms" -- a measured subset, not a claim about the
  # package. The patterns below are the five constructions the vignettes
  # actually use to state the total.
  dirs <- c(system.file("doc", package = "ggchangepoint"),
            test_path("..", "..", "vignettes"))
  dir <- dirs[nzchar(dirs) & dir.exists(dirs)][1]
  skip_if(is.na(dir), "vignette sources not available")
  files <- c(list.files(dir, pattern = "[.]Rmd$", full.names = TRUE),
             list.files(file.path(dir, "articles"), pattern = "[.]Rmd$",
                        full.names = TRUE))
  files <- files[file.exists(files)]
  skip_if(length(files) == 0, "vignette sources not available")

  live <- sum(cpt_methods()$status == "available")
  words <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
             seven = 7, eight = 8, nine = 9, ten = 10, twenty = 20,
             thirty = 30, forty = 40, fifty = 50, sixty = 60)
  patterns <- c(
    "covering ([A-Za-z0-9]+) (?:detection )?methods",
    "all ([A-Za-z0-9]+) wired methods",
    "wraps ([A-Za-z0-9]+) detectors",
    "([A-Za-z0-9]+) methods share one interface",
    "([A-Za-z0-9]+) methods in this\\b",
    "reaches ([A-Za-z0-9]+) methods"
  )
  # DESCRIPTION and the package-level Rd state the same total and cannot
  # compute it, so the literal has to stay -- which makes it exactly the
  # kind that goes stale on the next engine wave. Guarded here alongside
  # the vignettes.
  extra <- c(system.file("DESCRIPTION", package = "ggchangepoint"),
             test_path("..", "..", "DESCRIPTION"),
             system.file("help", "ggchangepoint-package.Rd",
                         package = "ggchangepoint"),
             test_path("..", "..", "man", "ggchangepoint-package.Rd"))
  files <- c(files, extra[nzchar(extra) & file.exists(extra)])
  wrong <- character()
  for (f in files) {
    # Normalise whitespace before matching. DESCRIPTION indents its
    # continuation lines and the vignettes wrap mid-phrase, so joining with
    # a single space leaves runs of blanks and "covering\n  fifty" never
    # matched a pattern written with literal spaces.
    txt <- gsub("[[:space:]]+", " ", paste(readLines(f, warn = FALSE),
                                           collapse = " "))
    for (pat in patterns) {
      for (m in regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]) {
        # plain string ops: an earlier version fed regexpr()'s offsets
        # from the substituted string to regmatches() on the original and
        # silently extracted "co" instead of "31", so the guard never fired
        tok <- tolower(sub("^(?:covering|all|wraps|reaches)\\s+", "", m))
        tok <- sub("\\s.*$", "", tok)
        n <- if (grepl("^[0-9]+$", tok)) {
          as.integer(tok)
        } else if (tok %in% names(words)) {
          words[[tok]]
        } else {
          NA_integer_
        }
        if (!is.na(n) && n != live) {
          wrong <- c(wrong, sprintf("%s: '%s' but the registry has %d",
                                    basename(f), m, live))
        }
      }
    }
  }
  expect_equal(wrong, character(0))
})
