# The calibration suite (roadmap §95): what the package promises, against
# what it delivers. Each cell is one guarantee the documentation states, a
# simulation that checks it, and the realised value with its Monte Carlo
# standard error. The e-detector's run-length bound was wrong by a factor
# of two in 0.4.0 while every call succeeded, and it was found only because
# someone measured it; this is that measurement, for every guarantee that
# has one, regenerated each release.
#
# Run from the package root with a pinned R:
#   module load R/4.4.1
#   MEASURE_CORES=16 Rscript data-raw/calibration.R
# then `Rscript data-raw/build_measured_data.R`, which turns
# data-raw/measurements/calibration.csv into the cpt_calibration data set.
# CALIBRATION_REPS (default 2000) sets the replicates per cell; the
# bootstrap cell uses half as many and the two monitor cells a fifth, each
# replicate there being a long computation.
suppressMessages(library(parallel))
cores <- as.integer(Sys.getenv("MEASURE_CORES", "1"))
reps <- as.integer(Sys.getenv("CALIBRATION_REPS", "2000"))
root <- normalizePath(".")
out_dir <- file.path(root, "data-raw", "measurements")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
options(rgl.useNULL = TRUE, warn = -1)
suppressMessages(pkgload::load_all(root, quiet = TRUE))
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# ---- location intervals ------------------------------------------------------
# One mean change of 1.5 noise standard deviations after observation 100 of
# 200. Coverage is conditional on detection: a replicate counts when some
# changepoint lands within 20 of the truth, and the interval of the nearest
# one is scored. `interval(x)` returns the changepoints with ci_lower and
# ci_upper.
coverage_cell <- function(interval) {
  function(r) {
    x <- c(stats::rnorm(100), stats::rnorm(100, 1.5))
    ci <- tryCatch(quiet(interval(x, r)), error = function(e) NULL)
    if (is.null(ci) || !nrow(ci)) return(NA)
    j <- which.min(abs(ci$cp - 100))
    if (abs(ci$cp[j] - 100) > 20 || is.na(ci$ci_lower[j])) return(NA)
    ci$ci_lower[j] <= 100 && 100 <= ci$ci_upper[j]
  }
}

# ---- tests under their null hypothesis -----------------------------------------
# The share of replicates rejecting at 5% on data with no change.
size_cell <- function(p_value) {
  function(r) {
    p <- tryCatch(quiet(p_value(r)), error = function(e) NA_real_)
    if (!is.finite(p)) return(NA)
    p < 0.05
  }
}

# ---- monitors on in-control streams --------------------------------------------
# The run length to the first alarm, censored at the stream's length (eight
# times the nominal value for cpm, twenty for the e-detector, so the
# censoring barely moves the mean).
run_length_cell <- function(make, stream) {
  function(r) {
    mon <- quiet(make())
    mon <- quiet(cpt_update(mon, stats::rnorm(stream)))
    a <- alarms(mon)$time
    if (length(a)) a[1] else stream
  }
}

cells <- list(
  list(guarantee = "cpt_confint() location interval",
       call = "cpt_detect(x, method = \"strucchange\")",
       setting = "native 95% interval, one change of 1.5 sd, n = 200",
       nominal = 0.95, kind = "coverage",
       fun = coverage_cell(function(x, r) {
         cpt_detect(x, method = "strucchange")$changepoints
       })),
  list(guarantee = "cpt_confint() location interval",
       call = "cpt_detect(x, method = \"smuce\", alpha = 0.05)",
       setting = "stepR jump interval at 1 - alpha, one change of 1.5 sd, n = 200",
       nominal = 0.95, kind = "coverage",
       fun = coverage_cell(function(x, r) {
         cpt_detect(x, method = "smuce", alpha = 0.05)$changepoints
       })),
  list(guarantee = "cpt_confint() location interval",
       call = "cpt_confint(cpt_detect(x), method = \"bootstrap\")",
       setting = "residual bootstrap, B = 199, one change of 1.5 sd, n = 200",
       nominal = 0.95, kind = "coverage", reps = ceiling(reps / 2),
       fun = coverage_cell(function(x, r) {
         cpt_confint(cpt_detect(x, method = "pelt"), method = "bootstrap",
                     B = 199, seed = r)
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51)",
       setting = "Welch t, Gaussian noise, n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rnorm(100), when = 51)$p_value
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51, family = \"poisson\")",
       setting = "exact test of two rates, Poisson(4) counts, n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rpois(100, 4), when = 51,
                     family = "poisson")$p_value
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51, family = \"binomial\")",
       setting = "Fisher's exact test, Bernoulli(0.3), n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rbinom(100, 1, 0.3), when = 51,
                     family = "binomial")$p_value
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51, family = \"exponential\")",
       setting = "exact F test of two rates, exponential waits, n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rexp(100), when = 51,
                     family = "exponential")$p_value
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51, family = \"l1\")",
       setting = "Wilcoxon rank-sum, t(3) noise, n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rt(100, 3), when = 51, family = "l1")$p_value
       })),
  list(guarantee = "cpt_test_at() size",
       call = "cpt_test_at(x, when = 51, window = 5)",
       setting = "largest statistic over +/-5, permutation, B = 199, n = 100",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_at(stats::rnorm(100), when = 51, window = 5, B = 199,
                     seed = r)$p_value
       })),
  list(guarantee = "cpt_test_null() size",
       call = "cpt_test_null(x)",
       setting = "CUSUM, Kolmogorov limit, Gaussian noise, n = 200",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) cpt_test_null(stats::rnorm(200))$p_value)),
  list(guarantee = "cpt_test_null() size",
       call = "cpt_test_null(x, method = \"pettitt\")",
       setting = "Pettitt's approximation, Gaussian noise, n = 200",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_null(stats::rnorm(200), method = "pettitt")$p_value
       })),
  list(guarantee = "cpt_test_null() size",
       call = "cpt_test_null(x, method = \"supF\")",
       setting = "sup-F, 15% trimming, Gaussian noise, n = 200",
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         cpt_test_null(stats::rnorm(200), method = "supF")$p_value
       })),
  list(guarantee = "cpt_test() at a location the data chose",
       call = "cpt_test(cpt_detect(x, method = \"amoc\", penalty = \"None\"))",
       setting = paste("Welch t at the one changepoint amoc always returns,",
                       "Gaussian noise, n = 200 (selection_adjusted = FALSE)"),
       nominal = 0.05, kind = "size",
       fun = size_cell(function(r) {
         f <- cpt_detect(stats::rnorm(200), method = "amoc", penalty = "None")
         if (!nrow(f$changepoints)) return(NA_real_)
         cpt_test(f)$p_value[1]
       })),
  list(guarantee = "cpt_monitor() average run length",
       call = "cpt_monitor(\"cpm\", arl0 = 500)",
       setting = "Mann-Whitney statistic, in-control N(0, 1), stream of 4,000",
       nominal = 500, kind = "run_length", reps = ceiling(reps / 5),
       fun = run_length_cell(function() cpt_monitor("cpm", arl0 = 500),
                             4000)),
  list(guarantee = "cpt_monitor() average run length (lower bound)",
       call = "cpt_monitor(\"edetector\", baseline = b, alpha = 0.01)",
       setting = paste("in-control N(0, 1) after a baseline of 100, stream",
                       "of 2,000; the guarantee is at least 1 / alpha"),
       nominal = 100, kind = "run_length", reps = ceiling(reps / 5),
       fun = run_length_cell(function() {
         cpt_monitor("edetector", baseline = stats::rnorm(100), alpha = 0.01)
       }, 2000))
)

# Each cell draws from its own stream (seed 10000 * cell + replicate), so
# the rows are independent measurements, and the replicates are spread over
# the workers in chunks: six cells take minutes per hundred replicates and
# the rest seconds.
t_start <- Sys.time()
chunk <- 50L
tasks <- do.call(rbind, lapply(seq_along(cells), function(i) {
  n_rep <- cells[[i]]$reps %||% reps
  starts <- seq(1L, n_rep, by = chunk)
  data.frame(cell = i, from = starts, to = pmin(starts + chunk - 1L, n_rep))
}))
values <- mclapply(seq_len(nrow(tasks)), function(k) {
  cl <- cells[[tasks$cell[k]]]
  vapply(tasks$from[k]:tasks$to[k], function(r) {
    set.seed(10000L * tasks$cell[k] + r)
    as.numeric(cl$fun(r))
  }, numeric(1))
}, mc.cores = cores, mc.preschedule = FALSE)
rows <- lapply(seq_along(cells), function(i) {
  cl <- cells[[i]]
  v <- unlist(values[tasks$cell == i])
  ok <- v[is.finite(v)]
  realised <- if (length(ok)) mean(ok) else NA_real_
  mcse <- if (length(ok) < 2) NA_real_ else if (cl$kind == "run_length") {
    stats::sd(ok) / sqrt(length(ok))
  } else {
    sqrt(realised * (1 - realised) / length(ok))
  }
  data.frame(guarantee = cl$guarantee, call = cl$call, setting = cl$setting,
             kind = cl$kind, nominal = cl$nominal, realised = realised,
             mcse = mcse, reps = length(ok), attempted = length(v))
})
tab <- do.call(rbind, rows)
utils::write.csv(tab, file.path(out_dir, "calibration.csv"), row.names = FALSE)
message("wrote ", nrow(tab), " calibration cells in ",
        format(round(difftime(Sys.time(), t_start, units = "mins"), 1)))
