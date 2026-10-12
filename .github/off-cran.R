# Declared dependencies that CRAN no longer serves, for every workflow.
#
# Thirty-odd engines are in Suggests, and CRAN archives packages as a matter
# of routine: fpop on 2026-09-14, ocp on 2026-10-12. pak resolves Suggests as
# hard requirements when it builds a lockfile, so each archival used to stop
# every workflow at dependency setup until someone added the archived tarball
# to three workflow files by hand. Each workflow now sources this file and
# asks CRAN instead:
#
#   archive_refs()        pak refs for the last CRAN release of every
#                         declared dependency CRAN no longer serves, from the
#                         CRAN archive, which is permanent;
#   publish_repo(dir)     the same tarballs as a CRAN-like repository, which
#                         the pkgdown workflow deploys to the package website
#                         and DESCRIPTION's Additional_repositories names;
#   check_availability()  the weekly check: CRAN's own rule (a dependency
#                         outside CRAN must be served by a repository in
#                         Additional_repositories) and the package's advice
#                         (engine_repos() must say where users get it).
#
# Nothing here names an engine, so the next archival needs no workflow edit:
# the installs and the repository follow on their own, and the one decision
# left to a person, the advice in engine_repos(), is what the weekly check
# reports.

cran <- "https://cloud.r-project.org"
site_repo <- "https://pursuitofdatascience.github.io/ggchangepoint/repo"

# A download that survives one bad response from a CDN.
fetch <- function(url, dest = tempfile(), tries = 3) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      utils::download.file(url, dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) return(dest)
    Sys.sleep(5 * i)
  }
  stop("Could not download ", url, call. = FALSE)
}

# The packages DESCRIPTION declares, without R itself and base packages.
declared <- function(fields = c("Depends", "Imports", "LinkingTo",
                                "Suggests")) {
  desc <- read.dcf("DESCRIPTION")
  fields <- intersect(fields, colnames(desc))
  deps <- trimws(unlist(strsplit(gsub("\\([^)]*\\)", "", desc[, fields]),
                                 ",")))
  base <- rownames(utils::installed.packages(priority = "base"))
  setdiff(deps[nzchar(deps)], c("R", base))
}

additional_repositories <- function() {
  desc <- read.dcf("DESCRIPTION")
  if (!"Additional_repositories" %in% colnames(desc)) return(character())
  repos <- trimws(strsplit(desc[, "Additional_repositories"], ",")[[1]])
  repos[nzchar(repos)]
}

# The source index of a repository. Source, because the binary index of a
# Windows or macOS runner lacks any package whose binary failed to build,
# and that is not CRAN no longer serving it.
repo_index <- function(repo) {
  failed <- NULL
  index <- withCallingHandlers(
    utils::available.packages(
      contriburl = utils::contrib.url(repo, "source"),
      filters = "duplicates", ignore_repo_cache = TRUE),
    # An unreachable index is a warning and an empty table, which would
    # read as "serves nothing". Only that warning: available.packages()
    # tries PACKAGES.rds, .gz and plain in turn, and the misses it expects
    # warn too.
    warning = function(w) {
      if (grepl("unable to access index", conditionMessage(w))) {
        failed <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    })
  if (!is.null(failed)) stop(failed, call. = FALSE)
  index
}

served_by <- function(repo) rownames(repo_index(repo))

cran_versions <- function() {
  index <- repo_index(cran)
  # A truncated index would make every dependency look archived and pin
  # all of them to old releases.
  if (nrow(index) < 10000) {
    stop("CRAN's package index has only ", nrow(index), " entries.",
         call. = FALSE)
  }
  index[, "Version"]
}

off_cran <- function(on_cran = names(cran_versions())) {
  setdiff(declared(), on_cran)
}

# CRAN's record of every archived tarball, as tools:::CRAN_archive_db()
# reads it: one data frame per package, rows named "pkg/pkg_version.tar.gz".
archive_db <- function() {
  readRDS(fetch(paste0(cran, "/src/contrib/Meta/archive.rds")))
}

# The last CRAN release of `pkg`, as a path under src/contrib/Archive/.
last_release <- function(pkg, db) {
  files <- rownames(db[[pkg]])
  if (!length(files)) {
    stop(pkg, " is declared in DESCRIPTION but is neither on CRAN nor in ",
         "its archive.", call. = FALSE)
  }
  v <- package_version(sub("^.*_(.*)[.]tar[.]gz$", "\\1", files))
  files[order(v, decreasing = TRUE)[1]]
}

archive_url <- function(file) paste0(cran, "/src/contrib/Archive/", file)

archive_refs <- function() {
  gone <- off_cran()
  if (!length(gone)) return(character())
  db <- archive_db()
  paste0("url::", archive_url(vapply(gone, last_release, "", db = db)))
}

# An older release of `pkg`, for as long as CRAN's current versions are
# exactly `while_cran` (package = version). It steps around one upstream
# break and lapses by itself when either side ships a fix, so it cannot
# outlive the break it was written for; if the fix does not work, the job
# that used the pin fails again and says so.
pin_while <- function(pkg, version, while_cran) {
  now <- cran_versions()[names(while_cran)]
  if (anyNA(now) || any(now != while_cran)) {
    cat("::notice::The pin on ", pkg, " ", version, " has lapsed: CRAN now ",
        "has ", paste(names(while_cran), now, collapse = ", "),
        ". Delete it from the workflow.\n", sep = "")
    return(character())
  }
  paste0("url::", archive_url(sprintf("%s/%s_%s.tar.gz", pkg, pkg, version)))
}

# A step output for setup-r-dependencies' extra-packages.
write_output <- function(name, values) {
  line <- paste0(name, "=", paste(values, collapse = " "))
  out <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(out)) {
    cat(line, "\n", file = out, append = TRUE, sep = "")
  }
  cat(if (length(values)) paste(values, collapse = "\n")
      else "Every declared dependency is on CRAN.", "\n")
}

publish_repo <- function(dir) {
  contrib <- file.path(dir, "src", "contrib")
  dir.create(contrib, recursive = TRUE, showWarnings = FALSE)
  gone <- off_cran()
  if (length(gone)) {
    db <- archive_db()
    for (f in vapply(gone, last_release, "", db = db)) {
      fetch(archive_url(f), file.path(contrib, basename(f)))
    }
  }
  tools::write_PACKAGES(contrib, type = "source")
  # A page at the repository's own URL, which the package documentation
  # links to: without one GitHub Pages answers 404 there, and R CMD check
  # --as-cran reports the link as invalid.
  notes <- archival_notes(gone)
  rows <- if (length(gone)) {
    sprintf("<li><code>%s</code>: %s <code>install.packages(\"%s\", repos = \"%s\")</code></li>",
            gone, notes[gone], gone, site_repo)
  } else {
    "<li>None: CRAN serves every dependency of ggchangepoint.</li>"
  }
  writeLines(c(
    "<!DOCTYPE html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<title>ggchangepoint: engines CRAN no longer serves</title></head>",
    "<body>",
    "<h1>Engines CRAN no longer serves</h1>",
    paste0("<p>A CRAN-like repository with the last CRAN release of each ",
           "package that <a href=\"..\">ggchangepoint</a> suggests and CRAN ",
           "has archived, unmodified, from the ",
           "<a href=\"https://cran.r-project.org/src/contrib/Archive/\">CRAN ",
           "archive</a>. Rebuilt on every deploy of the website.</p>"),
    "<ul>", rows, "</ul>",
    "</body></html>"), file.path(dir, "index.html"))
  cat("Published to ", dir, ": ",
      if (length(gone)) paste(gone, collapse = ", ") else "nothing", "\n",
      sep = "")
}

# engine_repos() as the package defines it, read from the source without
# running anything else in R/.
engine_repos_in_source <- function() {
  for (f in list.files("R", "[.][Rr]$", full.names = TRUE)) {
    for (e in parse(f, keep.source = FALSE)) {
      if (is.call(e) && identical(e[[1]], as.name("<-")) &&
          identical(e[[2]], as.name("engine_repos"))) {
        return(eval(e[[3]], baseenv())())
      }
    }
  }
  stop("engine_repos() is not defined in R/.", call. = FALSE)
}

# Why and when CRAN archived each package, from its own record.
archival_notes <- function(pkgs) {
  notes <- tryCatch({
    db <- read.dcf(fetch(paste0(cran, "/src/contrib/PACKAGES.in")),
                   fields = c("Package", "X-CRAN-Comment"))
    db[match(pkgs, db[, "Package"]), "X-CRAN-Comment"]
  }, error = function(e) rep(NA_character_, length(pkgs)))
  notes <- gsub("\\s+", " ", trimws(notes))
  stats::setNames(ifelse(is.na(notes), "Not on CRAN.", notes), pkgs)
}

check_availability <- function() {
  on_cran <- names(cran_versions())
  deps <- declared()
  gone <- off_cran(on_cran)
  repos <- additional_repositories()
  served <- lapply(repos, function(r) {
    tryCatch(served_by(r), error = function(e) {
      cat("::warning::", r, " did not answer: ",
          gsub("\\s+", " ", conditionMessage(e)), "\n", sep = "")
      NULL
    })
  })
  names(served) <- repos
  serving <- function(p) {
    repos[vapply(served, function(s) p %in% s, logical(1))]
  }
  advice <- engine_repos_in_source()
  notes <- archival_notes(gone)
  hard <- declared(c("Depends", "Imports", "LinkingTo"))

  problems <- character()
  for (p in gone) {
    if (p %in% hard) {
      problems <- c(problems, paste0(
        p, ": ", notes[[p]], " It is a hard dependency, so CRAN will ",
        "archive this package too unless it stops depending on it."))
    }
    if (!length(serving(p))) {
      problems <- c(problems, paste0(
        p, ": ", notes[[p]], " No repository in Additional_repositories ",
        "serves it, so CRAN's incoming check will return the package. The ",
        "pkgdown workflow publishes every archived dependency to ",
        site_repo, ": run it, then this check again."))
    }
    if (is.na(advice[p])) {
      problems <- c(problems, paste0(
        p, ": ", notes[[p]], " engine_repos() in R/ does not say where to ",
        "install it from, so cpt_detect() and cpt_install_engines() still ",
        "send users to install.packages('", p, "'), which no longer finds ",
        "it. Add ", p, " = \"", site_repo, "\" there and say so in NEWS."))
    } else if (!advice[[p]] %in% repos) {
      problems <- c(problems, paste0(
        p, ": engine_repos() sends users to ", advice[[p]], ", which ",
        "Additional_repositories does not declare."))
    } else if (!is.null(served[[advice[[p]]]]) &&
               !p %in% served[[advice[[p]]]]) {
      problems <- c(problems, paste0(
        p, ": engine_repos() sends users to ", advice[[p]], ", which ",
        "answers but does not serve it."))
    }
  }

  back <- intersect(names(advice), on_cran)
  if (length(back)) {
    cat("::warning::Back on CRAN, so drop them from engine_repos(): ",
        paste(back, collapse = ", "), "\n", sep = "")
  }
  idle <- repos[vapply(served, function(s) {
    !is.null(s) && !any(gone %in% s)
  }, logical(1))]
  if (length(idle)) {
    cat("::warning::Additional_repositories that serve no declared ",
        "dependency (CRAN notes these): ", paste(idle, collapse = ", "), "\n",
        sep = "")
  }

  if (length(problems)) {
    for (msg in problems) cat("::error::", msg, "\n", sep = "")
    stop(length(problems), " problem(s) with dependencies outside CRAN; ",
         "see the errors above.", call. = FALSE)
  }
  cat(length(deps) - length(gone), " of ", length(deps),
      " declared dependencies are on CRAN.\n", sep = "")
  for (p in gone) {
    cat(p, ": ", notes[[p]], " Served by ",
        paste(serving(p), collapse = " and "), "; users are sent to ",
        advice[[p]], ".\n", sep = "")
  }
}
