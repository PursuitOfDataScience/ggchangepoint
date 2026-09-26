# Turn the measurements in data-raw/measurements/ (written by
# data-raw/measure_engines.R) into the package's data sets. Run from the
# package root:
#   module load R/4.4.1 && Rscript data-raw/build_measured_data.R
m_dir <- file.path("data-raw", "measurements")
read <- function(f) utils::read.csv(file.path(m_dir, f),
                                    stringsAsFactors = FALSE)
dir.create("data", showWarnings = FALSE)

# ---- runtimes (§208) --------------------------------------------------------
rt <- read("runtime.csv")
# A cell that returned no time did not finish, whatever its status says.
rt$status[rt$status == "ok" & !is.finite(rt$seconds)] <- "error"
cpt_runtimes <- tibble::tibble(method = rt$method, n = as.integer(rt$n),
                               seconds = round(rt$seconds, 3),
                               k = as.integer(rt$k), status = rt$status)
save(cpt_runtimes, file = "data/cpt_runtimes.rda", compress = "xz")

# ---- invariances (§179, §184) ---------------------------------------------
inv <- read("invariance.csv")
inv <- inv[!is.na(inv$k), , drop = FALSE]
by_m <- split(inv, inv$method)
cpt_invariances <- do.call(rbind, lapply(names(by_m), function(m) {
  d <- by_m[[m]]
  tibble::tibble(
    method = m, reps = nrow(d),
    k = stats::median(d$k),
    scale_invariant = mean(d$scale, na.rm = TRUE) >= 0.8,
    shift_invariant = mean(d$shift, na.rm = TRUE) >= 0.8,
    reversal_rate = round(mean(d$reversal, na.rm = TRUE), 2),
    sequential = mean(d$reversal, na.rm = TRUE) < 0.5,
    concat_k = stats::median(d$concat_k, na.rm = TRUE),
    dup_k = stats::median(d$dup_k, na.rm = TRUE))
}))
save(cpt_invariances, file = "data/cpt_invariances.rda", compress = "xz")

# ---- noise regimes (§187, §189) ---------------------------------------------
nz <- read("noise.csv")
nz <- nz[!is.na(nz$hits), , drop = FALSE]
agg <- stats::aggregate(cbind(hits, fp) ~ method + setting + regime, nz,
                        function(v) mean(v))
reps <- stats::aggregate(hits ~ method + setting + regime, nz, length)
agg$reps <- reps$hits[match(paste(agg$method, agg$setting, agg$regime),
                            paste(reps$method, reps$setting, reps$regime))]
agg$call <- ifelse(agg$setting == "default",
                   paste0("cpt_detect(x, method = \"", agg$method, "\")"),
                   paste0("cpt_detect(x, method = \"", agg$method, "\", ",
                          agg$setting, ")"))
cpt_noise_benchmark <- tibble::tibble(
  method = agg$method, setting = agg$setting, call = agg$call,
  regime = agg$regime, reps = as.integer(agg$reps),
  hits = round(agg$hits, 2), fp = round(agg$fp, 2))
cpt_noise_benchmark <- cpt_noise_benchmark[
  order(cpt_noise_benchmark$regime, cpt_noise_benchmark$method,
        cpt_noise_benchmark$setting != "default"), ]
save(cpt_noise_benchmark, file = "data/cpt_noise_benchmark.rda",
     compress = "xz")

# ---- data types (§246) --------------------------------------------------------
dt <- read("datatype.csv")
dt <- dt[!is.na(dt$hit), , drop = FALSE]
dagg <- stats::aggregate(cbind(hit, fp) ~ method + data_type, dt, mean)
dreps <- stats::aggregate(hit ~ method + data_type, dt, length)
cpt_data_types <- tibble::tibble(
  method = dagg$method, data_type = dagg$data_type,
  reps = as.integer(dreps$hit[match(paste(dagg$method, dagg$data_type),
                                    paste(dreps$method, dreps$data_type))]),
  hit_rate = round(dagg$hit, 2), fp = round(dagg$fp, 2))
save(cpt_data_types, file = "data/cpt_data_types.rda", compress = "xz")

# ---- null size at scale (§211) ----------------------------------------------
if (file.exists(file.path(m_dir, "null.csv"))) {
  nl <- read("null.csv")
  nl <- nl[!is.na(nl$fp), , drop = FALSE]
  nagg <- stats::aggregate(fp ~ method + n, nl, function(v) {
    c(reps = length(v), size = mean(v > 0), mean_fp = mean(v))
  })
  cpt_null_sizes <- tibble::tibble(
    method = nagg$method, n = as.integer(nagg$n),
    reps = as.integer(nagg$fp[, "reps"]),
    size = round(nagg$fp[, "size"], 3),
    mean_fp = round(nagg$fp[, "mean_fp"], 3))
  cpt_null_sizes <- cpt_null_sizes[order(cpt_null_sizes$method,
                                         cpt_null_sizes$n), ]
  save(cpt_null_sizes, file = "data/cpt_null_sizes.rda", compress = "xz")
}
# ---- calibration (§95) --------------------------------------------------------
if (file.exists(file.path(m_dir, "calibration.csv"))) {
  cb <- read("calibration.csv")
  cpt_calibration <- tibble::tibble(
    guarantee = cb$guarantee, call = cb$call, setting = cb$setting,
    kind = cb$kind, nominal = cb$nominal,
    realised = signif(cb$realised, 3), mcse = signif(cb$mcse, 2),
    reps = as.integer(cb$reps))
  save(cpt_calibration, file = "data/cpt_calibration.rda", compress = "xz")
}
message("data sets written to data/")
