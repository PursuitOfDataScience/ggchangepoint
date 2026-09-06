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
      # `engine_usable()` counts too: it is the suite's own availability
      # predicate, and for a loop over methods it is the better guard -- it
      # skips the one method rather than aborting the whole test. (This
      # check caught the capability and supports guards the moment they
      # were added, which is the detector working; it just did not know
      # that spelling.)
      #
      # `engine_installed()` deliberately does NOT count. It answers "is
      # this on disk", via find.package(), and a package can be on disk and
      # unloadable -- which is how {mosum} passed the guard and then failed
      # to load on the macOS runner, turning a Suggests engine's broken
      # system library into one red CI job. Gating a run on it is the bug
      # this check exists to prevent, so recognising it as a guard would
      # license the mistake. Both blocks that still name it carry a real
      # guard as well, so nothing is grandfathered in.
      if (grepl("skip_if_not_installed|requireNamespace|skip_if\\(|engine_usable",
                body)) next
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

test_that("a registry sweep that asserts success gates on engine_usable()", {
  skip_on_cran()
  # The check above finds *literal* engine calls -- `method = "mosum"`,
  # `mosum_wrapper()`. It cannot see a loop over the registry that
  # dispatches on a variable, and that form reaches every Suggests engine
  # while naming none of them, so it was invisible to every guard the suite
  # had. It is also the form that failed on the macOS CI runner: the
  # change_in sweep asserted `expect_false(is.null(res))` for mosum on a
  # machine where mosum cannot be loaded at all.
  #
  # Most registry sweeps are safe by construction. They wrap the call in
  # tryCatch(error = function(e) NULL) and `next` on NULL, so an engine
  # that cannot load is skipped rather than reported -- three blocks do
  # exactly that and need nothing further. The dangerous shape is the one
  # that turns NULL into a failed expectation, because there "engine
  # unavailable" and "engine broke its contract" become the same result.
  # Those, and only those, must gate each iteration on engine_usable().
  files <- list.files(test_path(), pattern = "^test-.*[.]R$",
                      full.names = TRUE)
  offenders <- character()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    starts <- grep("^test_that\\(", src)
    if (length(starts) == 0) next
    ends <- c(starts[-1] - 1L, length(src))
    for (i in seq_along(starts)) {
      body <- paste(src[starts[i]:ends[i]], collapse = "\n")
      if (!grepl("builtin_registry()", body, fixed = TRUE)) next
      # dispatch on a variable, not on a quoted method name
      if (!grepl("method\\s*=\\s*[a-zA-Z_.]", body)) next
      # ... and an assertion that the dispatch succeeded
      if (!grepl(paste0("expect_false\\(\\s*is\\.null\\(",
                        "|expect_true\\(\\s*!\\s*is\\.null\\("),
                 body)) next
      if (!grepl("engine_usable(", body, fixed = TRUE)) {
        offenders <- c(offenders,
                       sprintf("%s:%d asserts a variable-dispatch result without engine_usable()",
                               basename(f), starts[i]))
      }
    }
  }
  expect_equal(offenders, character(0))
  # the sweep must be finding blocks at all, or it proves nothing
  n_sweeps <- 0L
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    n_sweeps <- n_sweeps + sum(grepl("engine_usable(", src, fixed = TRUE))
  }
  expect_gt(n_sweeps, 20L)
})

test_that("no shipped page points at a vignette that is not shipped", {
  skip_on_cran()
  # vignettes/articles/ is excluded from the tarball by .Rbuildignore, so
  # the "Benchmarks" article is never installed:
  # vignette("benchmarks", package = "ggchangepoint") warns "not found",
  # and the built tarball holds seven vignette sources and no articles/
  # entry at all. ?taylor_wrapper sent the reader there anyway, with a call
  # that cannot work anywhere off the website. Being web-only is correct --
  # _pkgdown.yml files it that way because the sweep behind it runs for
  # over twenty minutes -- so the fix was the pointer, not the exclusion.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  vdir <- file.path(root, "vignettes")
  if (!dir.exists(vdir)) skip("vignette sources not available")
  # top level only: that is exactly what ships
  shipped <- sub("\\.Rmd$", "", list.files(vdir, "\\.Rmd$"))
  expect_gte(length(shipped), 7L)
  # and the article really is excluded, or this guards nothing
  ignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
  expect_true(any(grepl("vignettes/articles", ignore, fixed = TRUE)))
  expect_true(dir.exists(file.path(vdir, "articles")))

  files <- c(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE),
             list.files(file.path(root, "man"), "\\.Rd$", full.names = TRUE),
             list.files(vdir, "\\.Rmd$", full.names = TRUE))
  bad <- character()
  for (f in files) {
    l <- readLines(f, warn = FALSE)
    refs <- unlist(regmatches(l, gregexpr(
      "vignette\\(\\s*\"[A-Za-z0-9_.-]+\"", l)))
    refs <- gsub("vignette\\(\\s*\"|\"", "", refs)
    miss <- setdiff(unique(refs), shipped)
    if (length(miss)) {
      bad <- c(bad, sprintf("%s -> %s", basename(f),
                            paste(miss, collapse = ", ")))
    }
  }
  expect_equal(bad, character(0))
})

test_that("the 0.5.0 method count agrees across registry, NEWS and the tour", {
  skip_on_cran()
  # The feature tour's "New in 0.5.0" box said "Nineteen further engines".
  # The number was right and the noun was not. This package uses `engine`
  # for the upstream package -- a registry column with 38 distinct values,
  # 35 of them in Suggests and 3 Imports -- whereas 19 is the delta in
  # *methods*: 31 at 0.4.0, 50 now. NEWS.md gets it right in its own
  # heading ("Engine wave #2 - 19 new methods"), so the two documents
  # disagreed about what was being counted, and a reader checking
  # `length(unique(cpt_methods()$engine))` got 38 with no way to reach 19.
  #
  # No magic numbers below: every figure comes from the live registry or
  # from NEWS.md's own sentences, so the three can only drift together.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  news_p <- file.path(root, "NEWS.md")
  vig_p <- file.path(root, "vignettes", "ggchangepoint.Rmd")
  if (!file.exists(news_p) || !file.exists(vig_p)) {
    skip("NEWS.md / vignette sources not available")
  }
  news <- readLines(news_p, warn = FALSE)
  reg <- ggchangepoint:::builtin_registry()
  live <- sum(reg$status == "available")

  grab <- function(pat) {
    h <- grep(pat, news, value = TRUE)[1]
    if (is.na(h)) return(NA_integer_)
    as.integer(sub(pat, "\\1", h))
  }
  reached  <- grab(".*reaches ([0-9]+) wired methods.*")
  baseline <- grab(".*from 13 to ([0-9]+) wired methods.*")
  delta    <- grab(".*Engine wave #2 . ([0-9]+) new methods.*")
  expect_false(anyNA(c(reached, baseline, delta)))

  expect_identical(reached, live)
  expect_identical(baseline + delta, live)

  # the tour's number word, and the noun it attaches to
  words <- c("Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen",
             "Eighteen", "Nineteen", "Twenty", "Twenty-one")
  box <- grep("New in 0[.]5[.]0", readLines(vig_p, warn = FALSE), value = TRUE)
  expect_length(box, 1L)
  w <- words[vapply(words, function(z) grepl(z, box, fixed = TRUE),
                    logical(1))]
  expect_length(w, 1L)
  expect_identical((13:21)[match(w, words)], delta)
  expect_match(box, paste0(w, " further methods"), fixed = TRUE)
  # and the quantity it is not: engines are a different, larger count
  expect_gt(length(unique(reg$engine)), delta)
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

test_that("cpt_detect()'s list of typo-swallowing engines matches behaviour", {
  skip_on_cran()
  # ?cpt_detect names the engines whose own signature ends in `...`, so that
  # a misspelt argument is discarded upstream instead of reported. That list
  # is a promise about every other method -- and it was measured, once, and
  # then drifted: fmean and fcov (both fChange) and bfast also swallow an
  # unknown argument and were not named, so a reader checking the list would
  # conclude their typo had been caught. Sweeping every installed engine
  # with an impossible argument name is the only way to keep the two in
  # step, so the sweep is the test.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  rd <- file.path(root, "R", "detect.R")
  if (!file.exists(rd)) skip("package sources not available")

  txt <- gsub("[[:space:]]+", " ", paste(readLines(rd, warn = FALSE),
                                         collapse = " "))
  claim <- regmatches(txt, regexpr(
    "several engines end their own signature in .{0,260}", txt, perl = TRUE))
  expect_length(claim, 1L)
  named <- unique(unlist(regmatches(claim,
    gregexpr("(?<=pkg\\{)[A-Za-z][A-Za-z0-9.]*", claim, perl = TRUE))))
  expect_true(length(named) >= 6)

  reg <- builtin_registry()
  # The sweep only means something on a library that actually has the
  # Suggests engines. Against an Imports-only library -- which is how the
  # R-devel run is done -- none of the six installed engines is one that
  # swallows, so "nothing swallowed" would be a true observation and a
  # false failure.
  n_installed <- sum(vapply(unique(reg$engine), engine_usable, logical(1)))
  if (n_installed < 20) {
    skip(paste("only", n_installed, "engines installed; the swallow sweep",
               "needs the Suggests engines to be meaningful"))
  }
  set.seed(7)
  uni <- c(rnorm(50), rnorm(50, 5))
  mv <- cbind(a = uni, b = c(rnorm(50), rnorm(50, 4)),
              c = c(rnorm(50), rnorm(50, 3)))
  swallows <- character()
  for (i in seq_len(nrow(reg))) {
    m <- reg$method[i]
    if (!engine_usable(reg$engine[i])) next
    # kcp and the functional engines are slow enough to dominate the suite,
    # and hdreg/fabisearch need a shape this generic input does not give;
    # they are covered by the recorded measurement in ?cpt_detect instead.
    if (m %in% c("kcp", "fcov", "fmean", "hdreg", "fabisearch", "ecp",
                 "bocpd", "cpop", "taylor", "np", "segneigh", "smuce",
                 "hsmuce", "wbs2", "mcp", "beast")) next
    dat <- if (isTRUE(reg$univariate[i])) uni else mv
    ok <- tryCatch({
      suppressWarnings(suppressMessages(
        utils::capture.output(cpt_detect(dat, method = m))))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) next
    got <- tryCatch({
      suppressWarnings(suppressMessages(
        utils::capture.output(cpt_detect(dat, method = m,
                                         zzz_not_an_argument = 99))))
      "swallowed"
    }, error = function(e) "errors")
    if (identical(got, "swallowed")) swallows <- c(swallows, m)
  }
  expect_true(length(swallows) > 0)
  # every engine that swallows must be traceable to a package the help
  # page names
  engines <- unique(reg$engine[match(swallows, reg$method)])
  expect_true(all(engines %in% named),
              info = paste("swallow but unnamed in ?cpt_detect:",
                           paste(setdiff(engines, named), collapse = ", ")))
})

test_that("the vignettes agree on how many method families there are", {
  skip_on_cran()
  # introduction.Rmd and ggchangepoint.Rmd both describe the same 50
  # methods, and they disagreed: "six algorithmic families" against "nine
  # methodological families". "Family" is a prose taxonomy, not a registry
  # column, so neither number can be derived -- but introduction.Rmd
  # *enumerates* its six and has one section per family plus two for the
  # cross-cutting concerns, so that is the checkable one. The nine had no
  # enumeration and nothing in the package to count to it.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  vdir <- file.path(root, "vignettes")
  if (!dir.exists(vdir)) skip("vignette sources not available")

  words <- c(one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
             six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L,
             eleven = 11L, twelve = 12L)
  counts <- list()
  for (f in list.files(vdir, "\\.Rmd$", full.names = TRUE)) {
    txt <- gsub("[[:space:]]+", " ", paste(readLines(f, warn = FALSE),
                                           collapse = " "))
    for (m in regmatches(txt, gregexpr(
      "([A-Za-z0-9]+) (?:algorithmic|methodological) families", txt,
      perl = TRUE))[[1]]) {
      tok <- tolower(sub(" .*$", "", m))
      n <- if (grepl("^[0-9]+$", tok)) as.integer(tok) else
        if (tok %in% names(words)) words[[tok]] else NA_integer_
      if (!is.na(n)) counts[[length(counts) + 1]] <-
        list(file = basename(f), n = n, phrase = m)
    }
  }
  expect_true(length(counts) >= 2)
  ns <- vapply(counts, function(z) z$n, integer(1))
  expect_equal(length(unique(ns)), 1L,
               info = paste(vapply(counts, function(z)
                 paste0(z$file, ": '", z$phrase, "'"), character(1)),
                 collapse = " | "))

  # ...and the number must match the list introduction.Rmd actually spells
  # out, so it stays a claim a reader can verify rather than a bare figure.
  intro <- gsub("[[:space:]]+", " ",
                paste(readLines(file.path(vdir, "introduction.Rmd"),
                                warn = FALSE), collapse = " "))
  enum <- regmatches(intro, regexpr(
    "six families [^.]{0,240}", intro, perl = TRUE))
  expect_length(enum, 1L)
  # Split on commas only. Four of the six family names contain their own
  # "and" ("penalised and optimal partitioning", "multiscale and search",
  # "nonparametric and sequential", "multivariate and high-dimensional"),
  # so splitting on " and " too counted ten families in a list of six.
  listed <- sub("^six families . ", "", enum)
  listed <- sub(" . plus .*$", "", listed)
  parts <- trimws(strsplit(listed, ",")[[1]])
  parts <- sub("^and ", "", parts)
  parts <- parts[nzchar(parts)]
  expect_equal(length(parts), unique(ns),
               info = paste("enumerated:", paste(parts, collapse = " / ")))
})

test_that("every registry capability flag is delivered by the accessor", {
  skip_on_cran()
  # `cpt_methods()` publishes one boolean per capability, and each of
  # `cpt_statistic()`, `cpt_solution_path()` and `ggcpt_posterior()` builds
  # its "these do:" error message from those very columns. So a wrong flag
  # does not just mislead -- it makes the error message name the method it
  # is refusing. Sweeping every method against the real accessors found
  # exactly that for `wbsts` and `binsegrcpp`, which advertised `path` and
  # then errored with a list that included themselves.
  #
  # Two flags are deliberately broader than the accessor, and are listed
  # here so they cannot rot into the silent kind of inconsistency:
  #   nsp   ci = TRUE, but its uncertainty is a significance region
  #         (region_start/region_end plus $regions), not ci_lower/ci_upper.
  #         cpt_recommend(need_uncertainty = TRUE) asks for "interval or
  #         region", which is why the flag is right.
  #   bocpd, mcp  posterior = TRUE, because both are Bayesian and quantify
  #         the location -- but bocpd's posterior is over run lengths
  #         (ggcpt_runlength()) and mcp's is summarised as
  #         ci_lower/ci_upper, so neither exposes the per-location profile
  #         ggcpt_posterior() draws.
  ci_exempt <- "nsp"
  posterior_exempt <- c("bocpd", "mcp")

  reg <- builtin_registry()
  ns <- asNamespace("ggchangepoint")
  set.seed(5)
  v <- c(rnorm(120), rnorm(120, 5))

  wanted <- reg$ci | reg$fitted | reg$posterior | reg$statistic | reg$path
  # Proportional, not a fixed minimum -- see the note in the renamed-argument
  # test in test-050-registry.R for why a magic threshold is wrong here.
  available <- sum(vapply(which(wanted), function(i)
    engine_usable(reg$engine[i]) && isTRUE(reg$univariate[i]), logical(1)))
  tested <- 0L
  for (i in which(wanted)) {
    m <- reg$method[i]
    if (!engine_usable(reg$engine[i])) next
    # the multivariate-only and slow engines carry none of these flags, so
    # a univariate series suffices for every row reached here
    if (!isTRUE(reg$univariate[i])) next
    res <- tryCatch(
      suppressWarnings(suppressMessages({
        utils::capture.output(z <- cpt_detect(v, method = m)); z })),
      error = function(e) NULL)
    if (is.null(res)) next
    tested <- tested + 1L

    if (isTRUE(reg$ci[i]) && !(m %in% ci_exempt)) {
      expect_true(all(c("ci_lower", "ci_upper") %in% names(res$changepoints)),
                  info = paste(m, "claims ci but has no ci_lower/ci_upper"))
    }
    if (isTRUE(reg$fitted[i])) {
      expect_true("fitted" %in% names(res$data),
                  info = paste(m, "claims fitted but $data has no fitted"))
    }
    if (isTRUE(reg$posterior[i]) && !(m %in% posterior_exempt)) {
      expect_false(is.null(get("posterior_prob_profile", envir = ns)(res)),
                   info = paste(m, "claims posterior but exposes no profile"))
    }
    if (isTRUE(reg$statistic[i])) {
      st <- tryCatch(get("extract_statistic", envir = ns)(res),
                     error = function(e) NULL)
      expect_true(!is.null(st) && NROW(st) > 0,
                  info = paste(m, "claims statistic but exposes none"))
    }
    if (isTRUE(reg$path[i])) {
      sp <- tryCatch(get("extract_solution_path", envir = ns)(res),
                     error = function(e) NULL)
      expect_true(!is.null(sp) && NROW(sp) > 0,
                  info = paste(m, "claims path but exposes none"))
    }
  }
  expect_lte(tested, available)
  expect_gt(available, 0L)

  # The exemptions must stay exemptions: if upstream or this package ever
  # starts delivering the narrow form, this test should be the thing that
  # says so rather than the list quietly becoming wrong.
  if (engine_usable("nsp")) {
    r <- tryCatch(suppressWarnings(cpt_detect(v, method = "nsp")),
                  error = function(e) NULL)
    if (!is.null(r)) {
      expect_false(all(c("ci_lower", "ci_upper") %in% names(r$changepoints)))
      expect_false(is.null(r$regions))
    }
  }
  if (engine_usable("ocp")) {
    r <- tryCatch(suppressWarnings(cpt_detect(v, method = "bocpd")),
                  error = function(e) NULL)
    if (!is.null(r)) {
      expect_null(get("posterior_prob_profile", envir = ns)(r))
      # ...and the dead end now names the accessor that does work
      expect_error(ggcpt_posterior(r), "ggcpt_runlength")
    }
  }
})

test_that("every change_in a method claims to support actually works", {
  skip_on_cran()
  # `supports` is published in cpt_methods() and drives
  # validate_method_change_in(), which refuses anything outside it. So each
  # listed value is a promise, and the `path` flag of §370 showed what an
  # unchecked promise looks like. Sweeping every (method, listed change_in)
  # pair -- 73 of them -- found zero that fail, which is worth keeping true.
  #
  # It also found six pairs where a *compatible* request is routed to the
  # method's own native change type. That is not silent (the result's
  # `change_in` says what was detected) and it is now documented on
  # ?cpt_detect, so the set is pinned here: a seventh appearing means the
  # help page has gone stale.
  routed <- list(
    c("not", "var", "meanvar"),
    c("cpm", "mean", "distribution"),
    c("cpm", "var", "distribution"),
    c("kcp", "mean", "running mean"),
    c("kcp", "var", "running var"),
    c("wbsts", "mean", "var")
  )
  reg <- builtin_registry()
  set.seed(6)
  v <- c(rnorm(110), rnorm(110, 5))

  # Only the univariate, installed, reasonably quick engines: the point is
  # the change_in contract, and the shape-specific engines are covered by
  # the recorded sweep. ocd is excluded because one fit costs ~189 s.
  skip_slow <- c("ocd", "fcov", "fmean", "fabisearch", "network", "hdreg",
                 "hdcov", "esac", "pilliat", "var", "inspect", "geomcp",
                 "npmojo", "ecp", "mcp", "bocpd", "cpop", "taylor")
  seen_routes <- list()
  tested <- 0L
  for (i in seq_len(nrow(reg))) {
    m <- reg$method[i]
    if (m %in% skip_slow || !isTRUE(reg$univariate[i])) next
    if (!engine_usable(reg$engine[i])) next
    for (ci in reg$supports[[i]]) {
      res <- tryCatch(
        suppressWarnings(suppressMessages({
          utils::capture.output(z <- cpt_detect(v, method = m,
                                                change_in = ci)); z })),
        error = function(e) NULL)
      # a listed change_in must not error
      expect_false(is.null(res),
                   info = paste(m, "claims change_in =", ci, "but errored"))
      if (is.null(res)) next
      tested <- tested + 1L
      if (!identical(res$change_in, ci)) {
        seen_routes[[length(seen_routes) + 1]] <- c(m, ci, res$change_in)
      }
    }
  }
  expect_gt(tested, 10L)

  # the routed pairs must be exactly the documented ones, for the methods
  # this run actually reached
  reachable <- Filter(function(z) {
    i <- match(z[1], reg$method)
    !is.na(i) && !(z[1] %in% skip_slow) && isTRUE(reg$univariate[i]) &&
      engine_usable(reg$engine[i])
  }, routed)
  fmt <- function(l) sort(vapply(l, paste, character(1), collapse = "|"))
  expect_setequal(fmt(seen_routes), fmt(reachable))
})

test_that("a multivariate-only engine refuses a single series by name", {
  skip_on_cran()
  # `univariate = FALSE` in the registry means "not a univariate method",
  # which cpt_recommend(dimension = "univariate") filters on. Measuring the
  # stronger reading -- "errors on one column" -- across all fourteen FALSE
  # engines separated them into two groups, and found that four of the nine
  # that do error named nothing while doing it.
  refuses_by_name <- c("ocd", "geomcp", "hdcov", "network", "var", "kwc",
                       "fmean", "fcov", "fabisearch")
  # These run on a vector and even recover the changepoint. They stay FALSE
  # because they are high-dimensional procedures, not because the call
  # fails -- so the registry comment says so, and this pins the set.
  runs_anyway <- c("npmojo", "inspect", "esac", "pilliat")

  reg <- builtin_registry()
  expect_setequal(reg$method[!reg$univariate],
                  c(refuses_by_name, runs_anyway, "hdreg"))

  set.seed(8)
  v <- c(rnorm(110), rnorm(110, 5))
  tested <- 0L
  for (m in refuses_by_name) {
    i <- match(m, reg$method)
    if (!engine_usable(reg$engine[i])) next
    tested <- tested + 1L
    err <- tryCatch({
      suppressWarnings(suppressMessages(
        utils::capture.output(cpt_detect(v, method = m))))
      NULL
    }, error = function(e) conditionMessage(e))
    expect_true(!is.null(err), info = paste(m, "accepted a vector"))
    if (is.null(err)) next
    # the refusal has to name the method, so the reader knows which of
    # their arguments is wrong
    expect_match(err, m, fixed = TRUE, info = m)
    # and must not be one of the base-R messages these used to give
    for (opaque in c("non-conformable arrays", "incorrect number of dimensions",
                     "dim(X) must have a positive length",
                     "must be an array of at least two dimensions")) {
      expect_false(grepl(opaque, err, fixed = TRUE), info = paste(m, opaque))
    }
  }
  expect_gt(tested, 0L)

  # the multivariate path must be untouched by the new guards
  X <- cbind(a = v, b = c(rnorm(110), rnorm(110, 4)),
             c = c(rnorm(110), rnorm(110, 3)))
  for (m in c("hdcov", "var", "kwc")) {
    if (!engine_usable(reg$engine[match(m, reg$method)])) next
    expect_s3_class(
      suppressWarnings(suppressMessages(cpt_detect(X, method = m))), "ggcpt")
  }
  # a one-column *matrix* is refused too, not just a bare vector
  if (engine_usable("changepoints")) {
    expect_error(cpt_detect(matrix(v, ncol = 1), method = "hdcov"),
                 "needs at least two")
  }
})

test_that("no evaluated vignette chunk installs packages or hits the network", {
  skip_on_cran()
  # Two exported functions have side effects a check must never trigger:
  # cpt_install_engines() installs packages, and cpt_load_tcpd() downloads
  # from the Turing Change Point Dataset's GitHub repository. Both are
  # currently safe -- the only bare cpt_install_engines("bayesian") sits in
  # an `eval = FALSE` chunk, the evaluated call passes `dry_run = TRUE`, and
  # cpt_load_tcpd() is prose-only -- but nothing was holding that in place.
  # Deleting one chunk option would make `R CMD check` install packages
  # while building a vignette, which is the kind of thing a rejected
  # submission tells you about.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  vdir <- file.path(root, "vignettes")
  if (!dir.exists(vdir)) skip("vignette sources not available")

  files <- c(list.files(vdir, "\\.Rmd$", full.names = TRUE),
             list.files(file.path(vdir, "articles"), "\\.Rmd$",
                        full.names = TRUE))
  offenders <- character()
  checked <- 0L
  for (f in files) {
    tx <- readLines(f, warn = FALSE)
    starts <- grep("^```\\{r", tx)
    for (s in starts) {
      rest <- grep("^```\\s*$", tx[seq(s + 1, length(tx))])
      if (!length(rest)) next
      e <- s + rest[1]
      hdr <- tx[s]
      body <- tx[seq(s + 1, e - 1)]
      # an unevaluated chunk cannot do anything
      if (grepl("eval\\s*=\\s*FALSE", hdr)) next
      if (any(grepl("cpt_install_engines\\(", body))) {
        checked <- checked + 1L
        # every evaluated call must be a dry run
        calls <- grep("cpt_install_engines\\(", body, value = TRUE)
        if (!all(grepl("dry_run\\s*=\\s*TRUE", calls))) {
          offenders <- c(offenders, sprintf(
            "%s:%d installs packages in an evaluated chunk",
            basename(f), s))
        }
      }
      if (any(grepl("cpt_load_tcpd\\(", body))) {
        checked <- checked + 1L
        offenders <- c(offenders, sprintf(
          "%s:%d downloads in an evaluated chunk", basename(f), s))
      }
    }
  }
  expect_equal(offenders, character(0))

  # and the same for the help pages: both functions' examples must be
  # \dontrun{} or dry-run only
  mdir <- file.path(root, "man")
  if (dir.exists(mdir)) {
    tcpd <- file.path(mdir, "cpt_load_tcpd.Rd")
    if (file.exists(tcpd)) {
      rd <- paste(readLines(tcpd, warn = FALSE), collapse = "\n")
      ex <- regmatches(rd, regexpr("\\\\examples\\{.*", rd))
      expect_true(grepl("dontrun", ex, fixed = TRUE),
                  info = "cpt_load_tcpd examples must be \\dontrun{}")
    }
    inst <- file.path(mdir, "cpt_install_engines.Rd")
    if (file.exists(inst)) {
      rd <- paste(readLines(inst, warn = FALSE), collapse = "\n")
      ex <- regmatches(rd, regexpr("\\\\examples\\{[^}]*\\}", rd))
      if (length(ex) && !grepl("dontrun", ex, fixed = TRUE)) {
        expect_match(ex, "dry_run\\s*=\\s*TRUE",
                     info = "cpt_install_engines examples must be a dry run")
      }
    }
  }
})

test_that("the registry's `fitted` flag matches which engines augment() shows", {
  # augment()'s help enumerated the engines whose own fitted signal replaces
  # the segment means in `.fitted`, and the list had gone stale: it named
  # six (SMUCE, DeCAFS, cpop, segmented, bcp, beast) where the registry
  # marks nine, omitting `hsmuce`, `mcp` and `bfast`. The registry column is
  # the source of truth, so the behaviour is checked against it and the help
  # now points at cpt_methods() rather than repeating a list that drifts.
  skip_on_cran()
  set.seed(7)
  x <- c(stats::rnorm(70), stats::rnorm(70, 4), stats::rnorm(60, 1))
  reg <- ggchangepoint:::builtin_registry()
  flagged <- reg$method[reg$status == "available" & reg$univariate &
                          as.logical(reg$fitted)]
  expect_gt(length(flagged), 5L)

  available <- tested <- 0L
  for (m in flagged) {
    r <- tryCatch(
      suppressWarnings(suppressMessages(cpt_detect(x, method = m))),
      error = function(e) NULL)
    if (is.null(r)) next                     # engine absent
    available <- available + 1L
    a <- augment(r)
    # the flag says this engine supplies a signal, so the column is there ...
    expect_true("fitted" %in% names(a), info = m)
    # ... and `.fitted` is that signal rather than the segment means
    expect_equal(a$.fitted, as.numeric(a$fitted), info = m)
    tested <- tested + 1L
  }
  expect_equal(tested, available)

  # and an engine NOT flagged must not acquire one
  for (m in intersect(c("pelt", "binseg", "amoc"),
                      reg$method[!as.logical(reg$fitted)])) {
    a <- augment(suppressWarnings(suppressMessages(
      cpt_detect(x, method = m))))
    expect_false("fitted" %in% names(a), info = m)
  }

  # the help's enumeration must still name exactly the flagged engines
  rd <- file.path(normalizePath(file.path("..", ".."), mustWork = FALSE),
                  "man", "augment.ggcpt.Rd")
  if (!file.exists(rd)) skip("man/ not available (installed package)")
  txt <- paste(readLines(rd, warn = FALSE), collapse = " ")
  claim <- sub(".*\\\\code\\{fitted\\} column:", "", txt)
  claim <- sub("\\}\\s*$", "", claim)
  named <- unique(unlist(regmatches(
    claim, gregexpr("(?<=\\\\code\\{)[a-z][a-z0-9.]*(?=\\})", claim, perl = TRUE))))
  expect_setequal(named,
                  reg$method[reg$status == "available" &
                               as.logical(reg$fitted)])
})

test_that("every capability list in the help matches the registry column", {
  # Part XLVIII fixed augment()'s stale enumeration of the engines whose
  # fitted signal replaces `.fitted` -- six named where the registry marks
  # nine -- and added a test for that page. The identical list lived in
  # autoplot()'s `show_fit` and survived, and `show_ci` named two engines
  # where seven supply intervals, and cpt_confint()'s "native" item named
  # four. One page fixed, three still wrong: the lesson of the input sweeps
  # applies to prose too, so this checks every page rather than the one that
  # was caught.
  #
  # The convention the pages now share is the phrase
  #   ... marks in its \code{<column>} column: \code{a}, \code{b}, ...
  # so any future page written that way is covered without editing this test.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  man <- file.path(root, "man")
  if (!dir.exists(man)) skip("man/ not available (installed package)")
  reg <- ggchangepoint:::builtin_registry()

  pages <- 0L
  for (f in list.files(man, "\\.Rd$", full.names = TRUE)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    hits <- gregexpr(
      "marks in its \\\\code\\{([a-z_]+)\\} column:((?:\\s*(?:and\\s*)?\\\\code\\{[a-z0-9_.]+\\},?)+)",
      txt, perl = TRUE)
    if (hits[[1]][1] == -1L) next
    for (h in regmatches(txt, hits)[[1]]) {
      col <- sub(".*marks in its \\\\code\\{([a-z_]+)\\} column:.*", "\\1", h)
      expect_true(col %in% names(reg),
                  info = paste(basename(f), "names a column the registry",
                               "does not have:", col))
      named <- unlist(regmatches(h, gregexpr(
        "(?<=\\\\code\\{)[a-z0-9_.]+(?=\\})", h, perl = TRUE)))
      named <- setdiff(named, col)
      expect_gt(length(named), 0L)
      marked <- reg$method[as.logical(reg[[col]])]

      # 1. nothing named that the registry does not mark
      expect_length(setdiff(named, marked), 0L)
      # 2. nothing marked that the page never mentions at all. A page may
      #    deliberately leave one out of the list -- cpt_confint() excludes
      #    `nsp` from "native" because its regions get their own provenance
      #    -- but it must then say so somewhere on the page.
      omitted <- setdiff(marked, named)
      for (m in omitted) {
        expect_match(txt, paste0("\\\\code\\{", m, "\\}"),
                     info = paste(basename(f), "omits", m, "from its", col,
                                  "list without mentioning it"))
      }
      pages <- pages + 1L
    }
  }
  # The same stale lists had been copied into the vignettes -- three of the
  # four there were wrong, which is why the guard covers them too. The
  # vignettes write engine names as prose (SMUCE, DeCAFS, CPOP) rather than
  # in \code{}, so the comparison is case-insensitive.
  vigs <- list.files(file.path(root, "vignettes"), "\\.Rmd$",
                     full.names = TRUE)
  vig_lists <- 0L
  for (f in vigs) {
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    hits <- gregexpr("marks in its `([a-z_]+)` column:([^)]*)", txt, perl = TRUE)
    if (hits[[1]][1] == -1L) next
    for (h in regmatches(txt, hits)[[1]]) {
      col <- sub(".*marks in its `([a-z_]+)` column:.*", "\\1", h)
      expect_true(col %in% names(reg), info = paste(basename(f), col))
      tail_txt <- sub(".*column:", "", h)
      named <- tolower(unlist(regmatches(
        tail_txt, gregexpr("[A-Za-z][A-Za-z0-9_.]+", tail_txt))))
      marked <- reg$method[as.logical(reg[[col]])]
      # nothing named that the registry does not mark ... allowing the prose
      # words that sit between the names
      expect_length(setdiff(intersect(named, reg$method), marked), 0L)
      # ... and every marked engine named, or else named elsewhere nearby
      omitted <- setdiff(marked, named)
      for (m in omitted) {
        expect_match(tolower(txt), m, fixed = TRUE,
                     info = paste(basename(f), "omits", m, "from its", col,
                                  "list"))
      }
      vig_lists <- vig_lists + 1L
    }
  }

  # the convention must actually be in use, or this test proves nothing
  expect_gte(pages, 3L)
  expect_gte(vig_lists, 1L)
})

test_that("every documented change_in enumeration matches the registry", {
  # Both vignettes listed five accepted `change_in` values -- mean, var,
  # meanvar, slope, distribution -- when the registry routes nine. The four
  # missing ones (covariance, network, regression, seasonality) are exactly
  # the ones NEWS.md credits to the wave-2 engines, so a reader of either
  # vignette would not have known they could ask for a covariance or
  # network change at all. cpt_detect()'s own @param was already complete,
  # which is the same split Part XLIX found: the function doc right, the
  # vignette stale.
  skip_on_cran()
  reg <- ggchangepoint:::builtin_registry()
  routable <- sort(unique(unlist(reg$supports)))
  expect_gte(length(routable), 9L)

  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  files <- c(list.files(file.path(root, "vignettes"), "\\.Rmd$",
                        full.names = TRUE),
             file.path(root, "man", "cpt_detect.Rd"))
  files <- files[file.exists(files)]
  if (length(files) == 0L) skip("sources not available (installed package)")

  checked <- 0L
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    # a run of >=4 quoted change_in values is an enumeration of the argument
    for (pat in c('`"[a-z]+"`(?:[^`]{0,40}`"[a-z]+"`){3,}',
                  '\\\\code\\{"[a-z]+"\\}(?:[^{]{0,40}\\\\code\\{"[a-z]+"\\}){3,}')) {
      hits <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
      for (h in hits) {
        named <- unlist(regmatches(h, gregexpr('(?<=")[a-z]+(?=")', h, perl = TRUE)))
        named <- unique(named)
        # only judge runs that are actually change_in values, not e.g. a
        # list of method names or penalty types
        if (length(intersect(named, routable)) < 4L) next
        expect_length(setdiff(named, routable), 0L)
        expect_setequal(named, routable)
        checked <- checked + 1L
      }
    }
  }
  # the enumerations must actually have been found, or this proves nothing
  expect_gte(checked, 3L)
})

test_that("the `online` flag and cpt_monitor()'s methods are distinct sets", {
  # cpt_methods() defines every capability flag except the one that needed
  # it most. A reader seeing `bocpd` marked `online = TRUE` would try
  # cpt_monitor("bocpd") and get a bare match.arg error naming three other
  # methods; and `edetector`, which IS one of those three, has no row in
  # cpt_methods() at all because it is native to this package rather than a
  # wrapped engine. Both directions are now documented, so both directions
  # are pinned here: if either set changes, that text has to be revisited.
  reg <- ggchangepoint:::builtin_registry()
  online <- sort(reg$method[as.logical(reg$online)])
  monitor <- sort(eval(formals(cpt_monitor)$method))

  expect_setequal(online, c("bocpd", "cpm", "ocd"))
  expect_setequal(monitor, c("cpm", "edetector", "ocd"))
  expect_setequal(eval(formals(cpt_replay)$method), monitor)

  # the two facts the help now asserts
  expect_false("edetector" %in% reg$method)
  expect_false("bocpd" %in% monitor)

  # ... and that they are really unreachable that way, rather than merely
  # absent from a default
  expect_error(cpt_monitor("bocpd", baseline = stats::rnorm(60)), "should be one of")
  expect_error(cpt_detect(stats::rnorm(60), method = "edetector"), "should be one of")

  # an online engine is still usable in batch, which is what the flag means
  skip_if_not_installed("ocp")
  set.seed(7)
  x <- c(stats::rnorm(60), stats::rnorm(60, 3))
  expect_s3_class(suppressWarnings(suppressMessages(
    cpt_detect(x, method = "bocpd"))), "ggcpt")
})

test_that("no condition expectation accepts any error at all", {
  # `expect_error(f(x))` with no pattern passes on ANY error -- including
  # the cryptic upstream ones this package has spent several releases
  # replacing with messages that name the argument and the method. Two
  # examples of why it matters: the envcpt short-series expectation would
  # have passed on the engine's own "Minimum segment legnth is too large"
  # just as happily as on the translation that replaced it, and
  # cpt_penalty("Manual") would have passed on a typo in the function name.
  #
  # Capturing the condition and asserting on it afterwards counts as
  # patterned and is stronger than a regexp, because it can check several
  # substrings:
  #     e <- expect_error(cpt_batch(...))
  #     expect_match(conditionMessage(e), "`short`", fixed = TRUE)
  # so that idiom is recognised rather than reported.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  dir <- file.path(root, "tests", "testthat")
  if (!dir.exists(dir)) skip("test sources not available (installed package)")

  weak <- character(0)
  checked <- 0L
  for (f in list.files(dir, "^test.*\\.R$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      m <- regmatches(lines[i], regexec(
        "(?:([A-Za-z._][A-Za-z0-9._]*)\\s*<-\\s*)?expect_(error|warning|message)\\(",
        lines[i], perl = TRUE))[[1]]
      if (!length(m)) next
      if (grepl("^\\s*#", lines[i])) next          # a comment, not a call
      # take the whole call, balancing parentheses across lines
      j <- i; chunk <- ""; depth <- 0
      repeat {
        chunk <- paste0(chunk, lines[j])
        depth <- depth + lengths(regmatches(lines[j], gregexpr("(", lines[j], fixed = TRUE))) -
                          lengths(regmatches(lines[j], gregexpr(")", lines[j], fixed = TRUE)))
        if (depth <= 0 || j >= length(lines)) break
        j <- j + 1
      }
      checked <- checked + 1L
      inner <- substring(chunk, regexpr("expect_", chunk))
      args <- substring(inner, regexpr("(", inner, fixed = TRUE) + 1L)
      # split the top-level arguments
      d <- 0L; cur <- ""; top <- character(0)
      for (ch in strsplit(args, "")[[1]]) {
        if (ch %in% c("(", "[", "{")) d <- d + 1L
        else if (ch %in% c(")", "]", "}")) { if (d == 0L) break; d <- d - 1L }
        if (ch == "," && d == 0L) { top <- c(top, cur); cur <- ""; next }
        cur <- paste0(cur, ch)
      }
      top <- c(top, cur)
      if (length(top) >= 2L && nzchar(trimws(top[2]))) next     # patterned
      var <- m[2]
      if (nzchar(var)) {
        after <- paste(lines[seq(min(j + 1L, length(lines)),
                                min(j + 5L, length(lines)))], collapse = "\n")
        if (grepl(paste0("conditionMessage\\(\\s*", var, "\\s*\\)"), after)) next
      }
      weak <- c(weak, paste0(basename(f), ":", i))
    }
  }
  expect_gt(checked, 100L)          # the scan must have found the calls
  expect_equal(weak, character(0))
})

test_that("no help page contains literal markdown", {
  # This package does not enable roxygen markdown -- DESCRIPTION has no
  # `Roxygen: list(markdown = TRUE)` -- so `**bold**` written in a roxygen
  # block passes straight through into the Rd and renders as asterisks on
  # the rendered help page. Three pages had it, including one this loop
  # wrote itself in Part XLVIII ("the **mean edge weight** at each time
  # point"). Backticks and asterisks inside \examples{} are ordinary
  # characters in R code, so those blocks are excluded.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  man <- file.path(root, "man")
  if (!dir.exists(man)) skip("man/ not available (installed package)")

  offenders <- character(0)
  pages <- 0L
  for (f in list.files(man, "\\.Rd$", full.names = TRUE)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    body <- gsub("\\\\examples\\{.*?\\n\\}", "", txt)
    pages <- pages + 1L
    for (pat in c("\\*\\*[A-Za-z][^*\n]{0,60}\\*\\*",   # **bold**
                  "\\[[^]\n]{1,40}\\]\\([^)\n]{1,60}\\)")) {  # [text](link)
      hit <- regmatches(body, gregexpr(pat, body))[[1]]
      if (length(hit))
        offenders <- c(offenders,
                       paste0(basename(f), ": ", substr(hit[1], 1, 50)))
    }
  }
  expect_gt(pages, 100L)                 # the scan must have read the pages
  expect_equal(offenders, character(0))

  # and the reason it matters: markdown really is off
  desc <- readLines(file.path(root, "DESCRIPTION"), warn = FALSE)
  expect_false(any(grepl("^Roxygen:.*markdown\\s*=\\s*TRUE", desc)))

  # The mirror image is just as wrong and was also present: Rd markup
  # inside a Markdown vignette renders literally, so `\pkg{penaltyLearning}`
  # in supervised.Rmd reached the reader as those exact characters. The
  # vignettes write a package name in bold.
  vig <- c(list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE),
           list.files(file.path(root, "vignettes", "articles"), "\\.Rmd$",
                      full.names = TRUE))
  vig <- vig[file.exists(vig)]
  expect_gt(length(vig), 5L)
  rd_in_vig <- character(0)
  for (f in vig) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    hit <- regmatches(txt, gregexpr(
      "\\\\(code|link|emph|strong|pkg|item|insertRef)\\{", txt))[[1]]
    if (length(hit))
      rd_in_vig <- c(rd_in_vig, paste0(basename(f), ": ", hit[1]))
  }
  expect_equal(rd_in_vig, character(0))
})

test_that("no example calls a Suggests engine without a guard", {
  # R CMD check runs examples with every Suggests installed, so it can
  # never see this: a user with only the three Imports engines who copies
  # an example calling, say, wbs_wrapper() gets an error from a help page.
  # Every such example must be wrapped -- \examplesIf, \dontrun, or an
  # explicit requireNamespace() -- and 117 of them are. This pins that.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  man <- file.path(root, "man")
  if (!dir.exists(man)) skip("man/ not available (installed package)")
  reg <- ggchangepoint:::builtin_registry()
  imports <- c("changepoint", "changepoint.np", "ecp")
  engines <- setdiff(unique(unlist(strsplit(paste(reg$engine, collapse = ","),
                                            "[,;] *"))), c(imports, NA, ""))

  unguarded <- character(0)
  with_examples <- 0L
  for (f in list.files(man, "\\.Rd$", full.names = TRUE)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    if (!grepl("\\\\examples", txt)) next
    with_examples <- with_examples + 1L
    ex <- sub(".*\\\\examples[If]*\\{", "", txt)
    guarded <- grepl("\\\\examplesIf|\\\\dontrun|\\\\donttest|requireNamespace",
                     txt)
    if (guarded) next
    used <- engines[vapply(engines, function(p) {
      ms <- reg$method[grepl(p, reg$engine, fixed = TRUE)]
      ws <- unique(reg$wrapper[reg$method %in% ms])
      ws <- ws[!is.na(ws) & nzchar(ws)]
      any(vapply(ws, function(w) grepl(paste0(w, "\\("), ex), logical(1))) ||
        any(vapply(ms, function(m)
          grepl(paste0("method *= *\"", m, "\""), ex), logical(1)))
    }, logical(1))]
    if (length(used))
      unguarded <- c(unguarded,
                     paste0(basename(f), " uses ", paste(used, collapse = ", ")))
  }
  expect_gt(with_examples, 100L)      # the scan must have read the pages
  expect_equal(unguarded, character(0))
})

test_that("every shipped figure is referenced by something", {
  # man/figures/ is installed into help/figures/ and counts toward the
  # installed size, which for this package flickers either side of CRAN's
  # 5.0Mb line. The README's plots live there under knitr's default names
  # (README-unnamed-chunk-NN-1.png), and those names RENUMBER whenever a
  # chunk is added or removed above them -- so a re-knit leaves the old
  # numbers behind as files nothing points at. Four such orphans had
  # accumulated, 179K shipped to every user and referenced by nothing.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  figdir <- file.path(root, "man", "figures")
  if (!dir.exists(figdir)) skip("man/figures not available")

  have <- list.files(figdir)
  expect_gt(length(have), 5L)          # the check must have found the figures

  # anything referenced from any source the package ships or builds from
  srcs <- c(list.files(root, "\\.(md|Rmd)$", full.names = TRUE),
            list.files(file.path(root, "man"), "\\.Rd$", full.names = TRUE),
            list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE),
            file.path(root, c("DESCRIPTION", "_pkgdown.yml")))
  srcs <- srcs[file.exists(srcs)]
  blob <- paste(unlist(lapply(srcs, readLines, warn = FALSE)), collapse = "\n")

  orphans <- have[!vapply(have, function(f) grepl(f, blob, fixed = TRUE),
                          logical(1))]
  expect_equal(orphans, character(0))
})

test_that("no vignette chunk depends on a variable only a Suggests chunk defines", {
  # The vignettes must build on a machine with only the three Imports
  # engines -- which is what CRAN's check machines are, and what the 4.6.0
  # run here exercises. Conditional chunks (`eval = has_cpop`) make that
  # work, but they create a dependency the eye does not catch: if a chunk
  # USES a variable that only a differently-gated chunk assigns, the build
  # fails wherever that engine is absent.
  #
  # The property is about use, not definition. `res_cpop` being defined
  # only under `has_cpop` is perfectly correct -- it is used only in that
  # chunk. What would be wrong is an ungated chunk consuming it. The one
  # real cross-gate dependency, `y_slope`, is already handled by a paired
  # fallback: `cpop` defines it under `has_cpop` and `cpop-fallback` under
  # `!has_cpop`, so the segmented example downstream always has its data.
  skip_on_cran()
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  vigs <- list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE)
  if (!length(vigs)) skip("vignettes not available (installed package)")

  uncovered <- character(0)
  scanned <- 0L
  for (f in vigs) {
    lines <- readLines(f, warn = FALSE)
    opens <- grep("^```\\{r", lines)
    closes <- grep("^```\\s*$", lines)
    if (!length(opens)) next
    scanned <- scanned + 1L

    chunks <- lapply(opens, function(s) {
      e <- closes[closes > s]
      e <- if (length(e)) e[1] else length(lines)
      m <- regmatches(lines[s], regexpr("eval\\s*=\\s*[^,}]+", lines[s]))
      list(line = s,
           cond = if (length(m)) trimws(sub("eval\\s*=\\s*", "", m)) else NA_character_,
           body = lines[seq(s + 1L, max(s + 1L, e - 1L))])
    })

    # where is each variable assigned, and under what condition
    defs <- list()
    for (ch in chunks) {
      hits <- unlist(regmatches(ch$body, gregexpr(
        "^\\s*[A-Za-z._][A-Za-z0-9._]*\\s*(<-|=[^=])", ch$body)))
      for (v in unique(trimws(sub("\\s*(<-|=).*$", "", hits))))
        defs[[v]] <- c(defs[[v]], list(list(line = ch$line, cond = ch$cond)))
    }

    for (ch in chunks) {
      used <- unique(unlist(regmatches(ch$body, gregexpr(
        "[A-Za-z._][A-Za-z0-9._]*", ch$body))))
      # drop this chunk's own assignments: defining is not depending
      own <- unlist(regmatches(ch$body, gregexpr(
        "^\\s*[A-Za-z._][A-Za-z0-9._]*\\s*(<-|=[^=])", ch$body)))
      own <- trimws(sub("\\s*(<-|=).*$", "", own))
      for (v in setdiff(used, own)) {
        d <- defs[[v]]
        if (is.null(d)) next
        earlier <- Filter(function(z) z$line < ch$line, d)
        if (!length(earlier)) next
        conds <- vapply(earlier, function(z) z$cond %||% NA_character_,
                        character(1))
        if (any(is.na(conds))) next               # an ungated definition exists
        bare <- unique(sub("^!", "", conds))
        if (length(bare) == 1L && length(unique(conds)) >= 2L) next  # paired fallback
        if (!is.na(ch$cond) && ch$cond %in% conds) next              # same gate
        uncovered <- c(uncovered,
                       paste0(basename(f), ":", ch$line, " uses `", v,
                              "` defined only under ",
                              paste(unique(conds), collapse = " / ")))
      }
    }
  }
  expect_gt(scanned, 5L)
  expect_equal(uncovered, character(0))
})
