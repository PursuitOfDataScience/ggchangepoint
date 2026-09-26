# Measure what each engine does with missing values: the `na_handling`
# column of the registry (see R/vocabulary.R). Run from the package root
# with a pinned R, e.g.
#   module load R/4.4.1 && Rscript data-raw/na_handling.R
#
# Probe (roadmap §171): 200 observations with changes at 70 and 140, and
# NAs at positions 30, 31, 32, 95 and 150. Each method runs through
# cpt_detect() with missing values allowed through validate_data(), which
# is exactly the route `na_action = "engine"` takes, and its answer is
# compared with the same method on the series without the NAs:
#
#   reject       the call errors
#   native       every changepoint matches the clean fit's within 3
#                positions: the engine handled the gaps in place
#   compacts     the matches hold only after each clean changepoint is
#                shifted left by the NAs before it: the engine dropped the
#                gaps and reported positions in the shortened series
#   silent_loss  anything else that returned without an error: an empty
#                answer, or changepoints unrelated to the clean fit's
#
# A method whose clean fit finds nothing cannot be classified and is
# reported as such; the table then records it as "reject", the safe value.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(rgl.useNULL = TRUE, warn = 1)
set.seed(171)
n <- 200
truth <- c(70, 140)
clean <- c(stats::rnorm(70, 0), stats::rnorm(70, 4), stats::rnorm(60, -1))
gaps <- c(30, 31, 32, 95, 150)
with_na <- clean
with_na[gaps] <- NA
Xclean <- cbind(clean, c(stats::rnorm(70, 0), stats::rnorm(70, 3),
                         stats::rnorm(60, 0)), stats::rnorm(200))
Xna <- Xclean
Xna[gaps, ] <- NA

reg <- ggchangepoint:::builtin_registry_core()
skip <- c("fabisearch", "mcp")
match_all <- function(a, b, tol = 3) {
  length(a) == length(b) && length(a) > 0 &&
    all(vapply(a, function(v) any(abs(b - v) <= tol), logical(1)))
}
classify <- function(cp_clean, cp_na) {
  if (inherits(cp_na, "error")) return("reject")
  if (length(cp_clean) == 0) return("unclassified")
  if (match_all(cp_clean, cp_na)) return("native")
  shifted <- vapply(cp_clean, function(v) v - sum(gaps <= v), numeric(1))
  if (match_all(shifted, cp_na)) return("compacts")
  "silent_loss"
}
run <- function(x, m) {
  wrapper <- get(reg$wrapper[reg$method == m], asNamespace("ggchangepoint"))
  args <- list(x, method = m)
  if ("seed" %in% names(formals(wrapper))) args$seed <- 1
  tryCatch({
    res <- suppressWarnings(suppressMessages(ggchangepoint:::with_na_allowed(
      do.call(cpt_detect, args))))
    res$changepoints$cp
  }, error = function(e) e)
}
out <- list()
for (i in seq_len(nrow(reg))) {
  m <- reg$method[i]
  if (m %in% skip || !ggchangepoint:::engine_installed(reg$engine[i])) next
  mv_only <- !reg$univariate[i]
  xc <- if (mv_only) Xclean else clean
  xn <- if (mv_only) Xna else with_na
  t0 <- proc.time()[["elapsed"]]
  cp_clean <- run(xc, m)
  cp_na <- run(xn, m)
  cls <- if (inherits(cp_clean, "error")) "clean_fit_failed" else
    classify(cp_clean, cp_na)
  msg <- if (inherits(cp_na, "error")) conditionMessage(cp_na) else ""
  out[[m]] <- data.frame(
    method = m, na_handling = cls,
    clean = paste(if (inherits(cp_clean, "error")) "ERR" else cp_clean,
                  collapse = ","),
    with_na = paste(if (inherits(cp_na, "error")) "ERR" else cp_na,
                    collapse = ","),
    error = substr(gsub("\\s+", " ", msg), 1, 80),
    seconds = round(proc.time()[["elapsed"]] - t0, 1))
  message(sprintf("%-12s %-14s clean=%s na=%s", m, cls, out[[m]]$clean,
                  out[[m]]$with_na))
}
tab <- do.call(rbind, out)
rownames(tab) <- NULL
print(tab, right = FALSE)
utils::write.csv(tab, file.path("data-raw", "na_handling.csv"),
                 row.names = FALSE)
