# Regression tests for the pre-submission polish pass (R26-R31). Each guards
# a bug found by exercising the public surface with degenerate or
# contract-violating input.

test_that("R26: a wrong-length `index` names the argument at fault", {
  set.seed(26)
  x <- c(rnorm(50), rnorm(50, 5))

  # Before: dplyr surfaced "`x` must be size 100 or 1, not 10", which never
  # mentions `index`.
  expect_error(ggcptplot(x, index = 1:10), "`index` must have one value")
  expect_error(ggecpplot(x, index = 1:10), "`index` must have one value")
  res <- cpt_detect(x, method = "pelt")
  expect_error(ggplot2::autoplot(res, index = 1:10),
               "`index` must have one value")

  # a correct-length index still works, including a non-numeric one
  expect_s3_class(ggcptplot(x, index = seq_along(x)), "ggplot")
  expect_s3_class(
    ggplot2::autoplot(res, index = as.Date("2020-01-01") + seq_along(x)),
    "ggplot")

  # and the multivariate path is guarded too
  X <- cbind(a = x, b = rev(x))
  res_mv <- cpt_detect(X, method = "ecp")
  expect_error(ggplot2::autoplot(res_mv, index = 1:10),
               "`index` must have one value")
  expect_s3_class(ggplot2::autoplot(res_mv, index = seq_len(nrow(X))),
                  "ggplot")
})

test_that("R27: changepoint.np refuses non-PELT methods by name", {
  set.seed(27)
  x <- c(rnorm(60), rnorm(60, 5))

  # Before: BinSeg/SegNeigh died on the internal Q clamp with "unused
  # argument (Q = 5)" and AMOC surfaced the engine's "Invalid Method".
  for (m in c("AMOC", "BinSeg", "SegNeigh")) {
    expect_error(cpt_wrapper(x, change_in = "np", cp_method = m),
                 "implements `cp_method = \"PELT\"` only", fixed = TRUE)
    expect_error(cpt_wrapper(x, change_in = "cpt_np", cp_method = m),
                 "implements `cp_method = \"PELT\"` only", fixed = TRUE)
  }
  expect_s3_class(cpt_wrapper(x, change_in = "np", cp_method = "PELT"),
                  "tbl_df")
  # the dispatcher only ever asks for PELT, so it is unaffected
  expect_s3_class(cpt_detect(x, method = "np"), "ggcpt")
})

test_that("R28: cpt_penalty enforces its documented argument ranges", {
  # sSIC is the *strengthened* SIC: alpha <= 1 makes it weaker than BIC.
  expect_error(cpt_penalty("sSIC", n = 100, alpha = 1), "greater than 1")
  expect_error(cpt_penalty("sSIC", n = 100, alpha = 0.5), "greater than 1")
  expect_gt(cpt_penalty("sSIC", n = 100), cpt_penalty("BIC", n = 100))

  # log(n) / log(log(n)) stop being penalties below n = 3
  expect_error(cpt_penalty("BIC", n = 1), "at least 3")
  expect_error(cpt_penalty("Hannan-Quinn", n = 2), "at least 3")
  expect_true(is.finite(cpt_penalty("Hannan-Quinn", n = 3)))
  # AIC does not involve n, so it is exempt
  expect_equal(cpt_penalty("AIC", n = 1, k = 2), 4)

  # MBIC's lchoose(n, k) term is -Inf once k > n
  expect_error(cpt_penalty("MBIC", n = 10, k = 20), "between 0 and `n`")
  expect_true(is.finite(cpt_penalty("MBIC", n = 100, k = 2)))

  expect_error(cpt_penalty("BIC", n = NA), "single finite number")
  expect_equal(cpt_penalty("None"), 0)
  expect_equal(cpt_penalty("Manual", value = 5), 5)
})

test_that("R29: mosum's automatic bandwidth is never 1", {
  skip_if_not_installed("mosum")
  set.seed(29)
  # n / 10 rounds to 1 for every n < 20, and a one-observation window makes
  # the engine's studentised statistic NaN: it warned "NaNs produced" and
  # returned spurious changepoints instead of failing.
  x <- c(rnorm(10), rnorm(10, 6))
  expect_no_warning(res <- mosum_wrapper(x))
  expect_s3_class(res, "ggcpt")
  expect_true(all(is.finite(res$changepoints$cp)))

  # too short for any valid window: an actionable message, not NaNs
  expect_error(mosum_wrapper(c(1, 5, 2, 6)), "moving-sum window")

  # a long series keeps the documented min(n / 10, 100) bandwidth
  y <- c(rnorm(200), rnorm(200, 5))
  expect_equal(nrow(mosum_wrapper(y)$changepoints),
               nrow(mosum_wrapper(y, G = 40)$changepoints))
})

test_that("R30: npmojo's default bandwidth works on short series", {
  skip_if_not_installed("CptNonPar")
  set.seed(30)
  # Before: the default G = max(20, 0.1 * n) exceeded the engine's n / 2
  # limit for every series shorter than 40, so the default always errored.
  for (n in c(10, 20, 30)) {
    x <- c(rnorm(n %/% 2), rnorm(n - n %/% 2, 6))
    expect_s3_class(npmojo_wrapper(x), "ggcpt")
  }
  # the cap only binds below n = 40; longer series are unchanged
  z <- c(rnorm(100), rnorm(100, 5))
  expect_equal(npmojo_wrapper(z)$changepoints$cp,
               npmojo_wrapper(z, G = 20)$changepoints$cp)
})

test_that("R31: a perfect evaluation plots without a ggplot scale warning", {
  set.seed(31)
  x <- c(rnorm(50), rnorm(50, 5))
  # No prediction and no truth: nothing is mapped to `type`, so the manual
  # scales warned "No shared levels found ...".
  expect_no_warning(
    p <- ggcpt_eval(integer(0), integer(0), x))
  expect_s3_class(p, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p))
  # the populated case still carries both scales
  p2 <- ggcpt_eval(c(48, 80), c(50), x)
  expect_s3_class(p2, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p2))
})

test_that("R32: cpt_simulate defaults every change type and warns about
           recycled parameters", {
  # `meanvar` was the one change type with no parameter default, so it died
  # with "replacement has length zero" instead of simulating anything.
  for (ci in c("mean", "var", "meanvar", "slope")) {
    d <- cpt_simulate(100, changepoints = 50, change_in = ci, seed = 32)
    expect_s3_class(d, "tbl_df")
    expect_equal(nrow(d), 100)
    expect_true(all(is.finite(d$value)))
    expect_equal(attr(d, "true_changepoints"), 50L)
  }

  # Three segments but two parameters recycles the last one, so the second
  # declared changepoint has no change behind it. That used to warn for
  # "mean" only, silently corrupting ground truth for the other types.
  two_of_three <- list(
    mean    = c(0, 5),
    var     = c(1, 5),
    meanvar = list(list(mean = 0, sd = 1), list(mean = 5, sd = 2)),
    slope   = list(list(intercept = 0, slope = 0.1),
                   list(intercept = 5, slope = -0.1))
  )
  for (ci in names(two_of_three)) {
    expect_warning(
      cpt_simulate(120, changepoints = c(40, 80), change_in = ci,
                   params = two_of_three[[ci]], seed = 32),
      "the last value is reused", info = ci)
  }
  # a full set of parameters warns about nothing
  expect_no_warning(
    cpt_simulate(120, changepoints = c(40, 80), change_in = "var",
                 params = c(1, 4, 9), seed = 32))
})

test_that("R33: cpt_detect() records its own call, not an internal helper", {
  set.seed(33)
  x <- c(rnorm(60), rnorm(60, 5))

  # Before: `$call` held wrap_cpt_to_ggcpt(x = data_vec, change_in = ci, ...)
  # -- an unexported helper, with the dispatcher's local symbol names.
  res <- cpt_detect(x, method = "pelt")
  expect_identical(as.character(res$call[[1]]), "cpt_detect")
  expect_identical(res$call$method, "pelt")

  # the ecp branch and the generic wrapper branch too
  X <- cbind(a = x, b = rev(x))
  expect_identical(as.character(cpt_detect(X, method = "ecp")$call[[1]]),
                   "cpt_detect")
  skip_if_not_installed("fpop")
  res_f <- cpt_detect(x, method = "fpop")
  expect_identical(as.character(res_f$call[[1]]), "cpt_detect")
  # and the series is still stored as a symbol, not inlined
  expect_lt(as.numeric(utils::object.size(res_f$call)), 2000)

  # a wrapper called directly still records itself
  skip_if_not_installed("wbs")
  expect_identical(as.character(wbs_wrapper(x)$call[[1]]), "wbs_wrapper")
})

test_that("R34: duplicate multivariate coordinate names are made unique", {
  set.seed(34)
  X <- cbind(c(rnorm(60), rnorm(60, 5)), c(rnorm(60), rnorm(60, -4)))
  # A matrix may legally carry duplicate colnames; add_column() then rejected
  # the wide frame with "must have unique names as of tibble 3.0.0".
  colnames(X) <- c("dup", "dup")
  res <- cpt_detect(X, method = "ecp")
  expect_s3_class(res, "ggcpt")
  expect_identical(names(res$data_wide), c("index", "dup", "dup.1"))
  expect_no_error(augment(res))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(res)))

  # the "index" collision fixed in R18 still behaves
  Xi <- X; colnames(Xi) <- c("index", "b")
  expect_identical(names(cpt_detect(Xi, method = "ecp")$data_wide),
                   c("index", "index.1", "b"))

  # and both at once
  Xb <- X; colnames(Xb) <- c("index", "index")
  expect_identical(names(cpt_detect(Xb, method = "ecp")$data_wide),
                   c("index", "index.1", "index.2"))

  # unnamed coordinates keep the V1/V2 fallback
  Xu <- X; colnames(Xu) <- NULL
  expect_identical(names(cpt_detect(Xu, method = "ecp")$data_wide),
                   c("index", "V1", "V2"))
})

test_that("R35: the changepoint engines keep their upstream fit and report a
           cost where one is well defined", {
  set.seed(35)
  x <- c(rnorm(120), rnorm(120, 5))

  # `$fit` is documented as "the raw upstream object" and every other engine
  # stores one; the changepoint family stored NULL, which also left the
  # `inherits(fit, "cpt")` branch of glance() unreachable.
  for (m in c("pelt", "binseg", "segneigh", "amoc", "np")) {
    res <- suppressWarnings(cpt_detect(x, method = m))
    expect_s4_class(res$fit, "cpt")
    expect_equal(as.integer(changepoint::cpts(res$fit)),
                 res$changepoints$cp, info = m)
    # glance() always returns exactly one row, whatever the engine exposes
    g <- glance(res)
    expect_equal(nrow(g), 1L, info = m)
    expect_length(g$total_cost, 1L)
  }

  # cpt_wrapper()'s documented return value is unchanged: a two-column tibble
  w <- cpt_wrapper(x)
  expect_s3_class(w, "tbl_df")
  expect_identical(names(w), c("cp", "cp_value"))

  # The unpenalised -2logLik is reported where changepoint exposes it, and it
  # rises as the penalty forces a coarser segmentation.
  costs <- vapply(c(2, 10, 1000), function(p)
    glance(cpt_detect(x, method = "pelt", penalty = p))$total_cost, numeric(1))
  expect_true(all(is.finite(costs)))
  expect_false(is.unsorted(costs))
  for (ci in c("meanvar", "var")) {
    expect_true(is.finite(glance(cpt_detect(x, method = "pelt",
                                            change_in = ci))$total_cost),
                info = ci)
  }

  # BinSeg/SegNeigh report a cost on a different scale, so it is deliberately
  # not pooled into the same column -- and asking must not leak the engine's
  # "Not changed to be -2*logLik" warning.
  for (m in c("binseg", "segneigh")) {
    res <- suppressWarnings(cpt_detect(x, method = m))
    expect_no_warning(g <- glance(res))
    expect_true(is.na(g$total_cost), info = m)
  }
})

test_that("R36: every plot the package produces survives ggcpt_interactive()", {
  skip_if_not_installed("plotly")
  set.seed(36)
  x <- c(rnorm(120), rnorm(120, 5))
  X <- cbind(a = x, b = rev(x))

  # The multivariate facet column used to be called `variable`, which is the
  # name plotly::ggplotly() gives a column of its own when it melts the built
  # plot -- so every multivariate result failed with "Names must be unique".
  mv <- cpt_detect(X, method = "ecp")
  p_mv <- ggplot2::autoplot(mv)
  expect_true("coordinate" %in% names(p_mv$data))
  expect_false("variable" %in% names(p_mv$data))
  expect_s3_class(ggcpt_interactive(mv), "plotly")

  # the facets still label themselves with the coordinate names
  expect_identical(levels(p_mv$data$coordinate), c("a", "b"))

  plots <- list(
    univariate = ggplot2::autoplot(cpt_detect(x, method = "pelt")),
    ggcptplot  = ggcptplot(x),
    ggecpplot  = ggecpplot(x),
    compare    = ggcpt_compare(x, methods = c("pelt", "binseg")),
    overlay    = ggcpt_compare(x, methods = c("pelt", "binseg"),
                               layout = "overlay"),
    eval       = ggcpt_eval(c(118, 200), c(120), x),
    crops      = ggplot2::autoplot(cpt_crops(x)),
    crops_segs = ggplot2::autoplot(cpt_crops(x), type = "segmentations"),
    batch      = ggplot2::autoplot(cpt_batch(X)),
    stability  = ggplot2::autoplot(cpt_stability(x, B = 5, seed = 1))
  )
  for (nm in names(plots)) {
    expect_s3_class(ggcpt_interactive(plots[[nm]]), "plotly")
  }

  expect_error(ggcpt_interactive(42), "must be a ggcpt object or a ggplot")
})

test_that("R37: glance() always returns one row and cpt_cite() explains an
           unusable method name", {
  set.seed(37)
  x <- c(rnorm(60), rnorm(60, 5))

  # new_ggcpt() defaulted `method`/`change_in` to character(0), so tibble()
  # recycled every other column of glance()'s row down to zero rows -- an
  # empty summary where the documentation promises exactly one.
  for (obj in list(
        new_ggcpt(),
        new_ggcpt(changepoints = tibble::tibble(cp = 30L, cp_value = 1),
                  data = tibble::tibble(index = 1:60, value = rnorm(60)),
                  method = "manual"),
        cpt_detect(x, method = "pelt"))) {
    g <- glance(obj)
    expect_equal(nrow(g), 1L)
    expect_length(g$method, 1L)
    expect_length(g$change_in, 1L)
    expect_length(g$penalty_type, 1L)
  }
  expect_true(is.na(glance(new_ggcpt())$method))
  expect_identical(glance(cpt_detect(x, method = "pelt"))$method, "pelt")

  # an object carrying no method name gets a message saying so, instead of
  # tibble's "Can't subset rows with `refs$method == method`"
  expect_error(cpt_cite(new_ggcpt()), "must be a method name")
  expect_error(cpt_cite(character(0)), "must be a method name")
  expect_error(cpt_cite(NA), "must be a method name")
  expect_error(cpt_cite(""), "must be a method name")
  # an unknown-but-usable name keeps its own message
  expect_error(cpt_cite("nope"), "No reference recorded")
  # and the working paths are untouched
  expect_output(cpt_cite("PELT"), "Killick")
  expect_output(cpt_cite(cpt_detect(x, method = "pelt")), "Killick")
  # the no-argument form still returns the whole table (it prints, so keep
  # that output out of the test log)
  invisible(capture.output(all_refs <- cpt_cite()))
  expect_gt(nrow(all_refs), 30L)
})

test_that("R38: cpt_stability() reports the proportion of replicates, not a
           capped count of detections", {
  # `freq` is documented as "the proportion of replicates detecting a
  # changepoint within `margin` of that index". The loop incremented once per
  # *changepoint*, so a replicate whose detections had overlapping +/-margin
  # windows counted twice there, and `pmin(hits / B, 1)` hid the overflow by
  # clipping -- reporting 1.00 ("re-detected every time") for indices only
  # half the replicates covered.
  reference <- function(x, B, margin, seed) {
    set.seed(seed)
    orig <- cpt_detect(x, method = "pelt")
    seg <- orig$segments
    step <- rep(seg$param_estimate, times = seg$n)
    resid <- x - step
    seg_id <- rep(seq_len(nrow(seg)), times = seg$n)
    n <- length(x)
    hits <- numeric(n)
    for (b in seq_len(B)) {
      rs <- resid
      for (s in seq_len(nrow(seg))) {
        i <- which(seg_id == s)
        rs[i] <- sample(resid[i], length(i), replace = TRUE)
      }
      cp <- tryCatch(cpt_detect(step + rs, method = "pelt")$changepoints$cp,
                     error = function(e) integer(0))
      covered <- logical(n)
      for (k in cp) covered[max(1, k - margin):min(n, k + margin)] <- TRUE
      hits <- hits + covered            # once per replicate, never twice
    }
    hits / B
  }

  # Configurations where detections cluster, so the old count-based version
  # overstated (index 1 came out at 1.00 against a true 0.50).
  for (cfg in list(list(sd = 1.6, margin = 15, seed = 5),
                   list(sd = 2.0, margin = 20, seed = 9))) {
    set.seed(cfg$seed)
    x <- c(rnorm(50, 0, cfg$sd), rnorm(30, 3, cfg$sd), rnorm(50, 0, cfg$sd))
    st <- cpt_stability(x, method = "pelt", B = 40, margin = cfg$margin,
                        seed = 1)
    expect_equal(st$frequency$freq, reference(x, 40, cfg$margin, 1),
                 info = paste("sd", cfg$sd))
  }

  # shape invariants that hold whatever the data
  set.seed(38)
  y <- c(rnorm(100), rnorm(100, 4))
  st <- cpt_stability(y, method = "pelt", B = 20, seed = 1)
  expect_equal(nrow(st$frequency), length(y))
  expect_true(all(st$frequency$freq >= 0 & st$frequency$freq <= 1))
  # a proportion out of B replicates is always a whole number of B-ths
  expect_equal(st$frequency$freq * 20, round(st$frequency$freq * 20))
  # a clean changepoint is still re-detected every time
  expect_equal(st$frequency$freq[100], 1)
})

test_that("R39: the scale-sensitivity note in ?cpt_detect is accurate", {
  # `pelt`/`binseg`/`segneigh`/`fpop` weigh the penalty against a raw segment
  # cost for a change in mean, so widening the noise shatters the
  # segmentation. Nothing in the package documented that until now; this test
  # pins both halves of the note -- which engines break, and that each
  # documented remedy works.
  make <- function(sigma, seed = 1) {
    set.seed(seed)
    c(rnorm(100, 0, sigma), rnorm(100, 5 * sigma, sigma))  # one cp, 5-sigma jump
  }
  ncp <- function(...) nrow(suppressWarnings(cpt_detect(...))$changepoints)

  # the trap itself: same signal-to-noise, wildly different answers
  expect_equal(ncp(make(1), method = "pelt"), 1L)
  expect_gt(ncp(make(10), method = "pelt"), 20L)

  wide <- make(10)
  # fpop lives in Suggests, so its half of the note is asserted only when it
  # is installed; the pelt half runs everywhere.
  fpop_ok <- requireNamespace("fpop", quietly = TRUE)
  # remedy 1 -- standardise
  expect_equal(ncp(scale(wide)[, 1], method = "pelt"), 1L)
  if (fpop_ok) expect_equal(ncp(scale(wide)[, 1], method = "fpop"), 1L)
  # remedy 2 -- a penalty on the data's own scale
  pen <- 2 * log(length(wide)) * stats::var(diff(wide)) / 2
  if (fpop_ok) expect_equal(ncp(wide, method = "fpop", penalty = pen), 1L)
  # remedy 3 -- estimate a variance per segment
  expect_equal(ncp(wide, method = "pelt", change_in = "meanvar"), 1L)

  # and the engines the note calls unaffected really are: same segmentation
  # whatever the units
  stable <- c("wbs", "wbs2", "not", "mosum", "idetect", "tguh", "smuce",
              "decafs", "cpop")
  for (m in stable) {
    skip_if_not_installed(switch(m,
      wbs = "wbs", wbs2 = "breakfast", not = "not", mosum = "mosum",
      idetect = "IDetect", tguh = "breakfast", smuce = "stepR",
      decafs = "DeCAFS", cpop = "cpop"))
    a <- suppressWarnings(cpt_detect(make(1), method = m))$changepoints$cp
    b <- suppressWarnings(cpt_detect(make(1) * 1000, method = m))$changepoints$cp
    expect_identical(a, b, info = m)
  }
})

test_that("R40: the documented dispatcher-vs-wrapper penalty difference is
           real, and an explicit penalty reconciles the two", {
  # `cpt_detect()` resolves its "MBIC" default to a numeric value stronger
  # than the numeric-penalty wrappers' own 2 * log(n), so the two entry
  # points can disagree on the same series. Both defaults were documented;
  # the fact that they differ was not.
  n <- 360
  set.seed(3)
  v <- as.numeric(rep(c(0, 3, -2, 5, 1, -4), each = 60)) + rnorm(n, 0, 1)

  disp_pen <- cpt_penalty("MBIC", n = n, k = 2)
  wrap_pen <- 2 * log(n)
  expect_gt(disp_pen, wrap_pen)

  # Pin the mechanism rather than the engine's output: the dispatcher's
  # default must behave exactly as if the stronger penalty had been passed,
  # and the wrapper's default exactly as if the weaker one had. That states
  # the documented claim without hard-coding changepoint counts that a
  # DeCAFS update could legitimately move.
  skip_if_not_installed("DeCAFS")
  expect_identical(cpt_detect(v, method = "decafs")$changepoints$cp,
                   decafs_wrapper(v, penalty = disp_pen)$changepoints$cp)
  expect_identical(decafs_wrapper(v)$changepoints$cp,
                   decafs_wrapper(v, penalty = wrap_pen)$changepoints$cp)

  # passing the penalty explicitly makes them agree, by either route
  expect_identical(
    cpt_detect(v, method = "decafs", penalty = wrap_pen)$changepoints$cp,
    decafs_wrapper(v, penalty = wrap_pen)$changepoints$cp)
  expect_identical(
    cpt_detect(v, method = "decafs", penalty = disp_pen)$changepoints$cp,
    decafs_wrapper(v, penalty = disp_pen)$changepoints$cp)

  # same story for fpop and cpop: an explicit penalty reconciles them
  skip_if_not_installed("fpop")
  expect_identical(
    cpt_detect(v, method = "fpop", penalty = wrap_pen)$changepoints$cp,
    fpop_wrapper(v, penalty = wrap_pen)$changepoints$cp)
  skip_if_not_installed("cpop")
  expect_identical(
    cpt_detect(v, method = "cpop", penalty = wrap_pen)$changepoints$cp,
    cpop_wrapper(v, penalty = wrap_pen)$changepoints$cp)
})

test_that("R41: every S3 method dispatches from NAMESPACE alone", {
  # The package used to re-register print/plot/summary/tidy/glance/augment/
  # autoplot from .onLoad(), writing into base's and generics' methods tables
  # for no effect and wrapping the lot in suppressWarnings() so a real
  # failure would have been invisible. The NAMESPACE declarations already do
  # the job; this guards that they keep doing it.
  expect_false(".onLoad" %in% ls(asNamespace("ggchangepoint"), all.names = TRUE))

  set.seed(41)
  x <- c(rnorm(60), rnorm(60, 5))
  res <- cpt_detect(x, method = "pelt")

  # the base generics
  expect_match(paste(capture.output(print(res)), collapse = " "),
               "ggcpt \\(changepoint")
  expect_match(paste(capture.output(res), collapse = " "),   # auto-printing
               "ggcpt \\(changepoint")
  expect_s3_class(summary(res), "summary.ggcpt")
  expect_match(paste(capture.output(print(summary(res))), collapse = " "),
               "ggcpt Summary")
  expect_s3_class(plot(res), "ggplot")
  expect_match(format(res), "^ggcpt \\[pelt\\]")
  expect_s3_class(as.data.frame(res), "data.frame")

  # the generics owned by other packages
  expect_s3_class(ggplot2::autoplot(res), "ggplot")
  expect_s3_class(generics::tidy(res), "tbl_df")
  expect_s3_class(generics::glance(res), "tbl_df")
  expect_s3_class(generics::augment(res), "tbl_df")
  expect_s3_class(tibble::as_tibble(res), "tbl_df")

  # and the methods on the other result classes
  X <- cbind(a = x, b = rev(x))
  expect_s3_class(ggplot2::autoplot(cpt_batch(X)), "ggplot")
  expect_s3_class(ggplot2::autoplot(cpt_crops(x)), "ggplot")
  expect_s3_class(ggplot2::autoplot(cpt_stability(x, B = 3, seed = 1)),
                  "ggplot")
  expect_s3_class(generics::tidy(cpt_batch(X)), "tbl_df")
  expect_s3_class(generics::tidy(cpt_crops(x)), "tbl_df")

  # every generic/class pair NAMESPACE declares resolves through the public
  # lookup, including the two registered onto base generics
  pairs <- list(c("tidy", "ggcpt"), c("glance", "ggcpt"), c("augment", "ggcpt"),
                c("autoplot", "ggcpt"), c("print", "ggcpt"),
                c("format", "ggcpt"), c("as_tibble", "ggcpt"),
                c("as.data.frame", "ggcpt"), c("plot", "ggcpt"),
                c("summary", "ggcpt"), c("print", "summary.ggcpt"),
                c("autoplot", "ggcpt_batch"), c("autoplot", "ggcpt_path"),
                c("autoplot", "ggcpt_stability"), c("tidy", "ggcpt_batch"),
                c("tidy", "ggcpt_path"), c("print", "ggcpt_batch"),
                c("print", "ggcpt_path"), c("print", "ggcpt_stability"))
  for (pr in pairs) {
    m <- utils::getS3method(pr[1], pr[2], optional = TRUE)
    expect_true(is.function(m), info = paste(pr, collapse = "."))
  }
})

test_that("R42: the covering metric is unchanged by the linear sweep", {
  # calc_covering() used to compare every truth segment against every
  # prediction segment, which is quadratic: 7.5 s for 3000 changepoints. It
  # now scans only the overlapping prediction segments, found with two
  # findInterval() lookups. This pins the numbers against an independent
  # set-based statement of the definition (van den Burg & Williams 2020),
  # not against either implementation.
  ref_cov <- function(pred, truth, n) {
    segs <- function(cp) {
      b <- sort(unique(c(0, cp, n)))
      lapply(seq_len(length(b) - 1), function(i) (b[i] + 1):b[i + 1])
    }
    A <- segs(truth); B <- segs(pred)
    sum(vapply(A, function(a) length(a) * max(vapply(B, function(bb)
      length(intersect(a, bb)) / length(union(a, bb)), 0)), 0)) / n
  }

  set.seed(42)
  worst <- 0
  for (trial in seq_len(150)) {
    n <- sample(3:200, 1)
    kp <- sample(0:min(10, n - 1), 1)
    kt <- sample(0:min(10, n - 1), 1)
    p <- if (kp) sort(sample(seq_len(n - 1), kp)) else integer(0)
    t <- if (kt) sort(sample(seq_len(n - 1), kt)) else integer(0)
    worst <- max(worst, abs(cpt_metrics(p, t, n = n)$covering -
                            ref_cov(p, t, n)))
  }
  expect_equal(worst, 0)

  # adversarial partitions: nothing, everything, interleaved, and the
  # boundaries
  edge <- list(list(integer(0), integer(0), 3), list(1, 1, 3), list(1, 2, 3),
               list(seq_len(49), integer(0), 50),
               list(integer(0), seq_len(49), 50),
               list(seq_len(49), seq_len(49), 50),
               list(c(1, 99), 50, 100), list(50, c(1, 99), 100),
               list(seq(2, 98, by = 2), seq(3, 97, by = 2), 100))
  for (e in edge) {
    expect_equal(cpt_metrics(e[[1]], e[[2]], n = e[[3]])$covering,
                 ref_cov(e[[1]], e[[2]], e[[3]]),
                 info = paste(length(e[[1]]), length(e[[2]]), e[[3]]))
  }

  # a perfect and a maximally wrong segmentation still bracket at 1 and > 0
  expect_equal(cpt_metrics(c(50, 150), c(50, 150), n = 300)$covering, 1)
  expect_gt(cpt_metrics(integer(0), c(50, 150), n = 300)$covering, 0)
})

test_that("R42b: cpt_metrics stays fast with many changepoints", {
  skip_on_cran()
  set.seed(43)
  n <- 200000
  k <- 3000
  p <- sort(sample(seq_len(n - 1), k))
  t <- sort(sample(seq_len(n - 1), k))
  # the quadratic version took ~7.5 s here; allow generous headroom for a
  # slow machine while still catching a return to O(segs^2)
  elapsed <- system.time(cpt_metrics(p, t, n = n))[["elapsed"]]
  expect_lt(elapsed, 5)
})

test_that("R43: geom_cpt_ci needs y/xmin/xmax, and x is optional", {
  # The help said `x` was required alongside xmin/xmax/y, but the layer is a
  # horizontal error bar: `x` is accepted and ignored, and neither of the
  # package's own two call sites supplies it.
  d <- data.frame(i = 1:100, v = rnorm(100))
  ci <- data.frame(x = 50, xmin = 45, xmax = 55, y = -2)
  base <- ggplot2::ggplot(d, ggplot2::aes(i, v)) + ggplot2::geom_line()

  expect_no_error(ggplot2::ggplot_build(
    base + geom_cpt_ci(data = ci, ggplot2::aes(y = y, xmin = xmin,
                                               xmax = xmax),
                       inherit.aes = FALSE)))
  expect_no_error(ggplot2::ggplot_build(
    base + geom_cpt_ci(data = ci, ggplot2::aes(x = x, y = y, xmin = xmin,
                                               xmax = xmax),
                       inherit.aes = FALSE)))
  # y really is required
  expect_error(ggplot2::ggplot_build(
    base + geom_cpt_ci(data = ci, ggplot2::aes(xmin = xmin, xmax = xmax),
                       inherit.aes = FALSE)))

  # and the other two geoms take exactly the aesthetics they document
  segs <- data.frame(start = c(1, 51), end = c(50, 100), param = c(0, 2))
  expect_no_error(ggplot2::ggplot_build(
    base + geom_cpt_segment(data = segs,
      ggplot2::aes(x = start, xend = end, y = param, yend = param),
      inherit.aes = FALSE)))
  expect_no_error(ggplot2::ggplot_build(
    base + geom_changepoint(data = data.frame(cp = 50),
                            ggplot2::aes(xintercept = cp))))
})

test_that("R44: the three citation sources agree with each other", {
  skip_on_cran()
  # The package ships per-method citations three ways -- cpt_cite(), the
  # \insertRef keys in the help pages, and the vignette bibliography -- and
  # they had drifted apart: the TGUH paper was dated 2018 in cpt_cite() and
  # 2022 in the vignette bib, and ecp's help cited the arXiv preprint of a
  # paper the vignettes cite in its published JSS form.
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  inst_bib <- file.path(root, "inst", "REFERENCES.bib")
  vig_bib <- file.path(root, "vignettes", "vignette_reference.bib")
  if (!file.exists(inst_bib) || !file.exists(vig_bib)) {
    skip("bibliography sources not available (installed package)")
  }

  parse_bib <- function(f) {
    lines <- readLines(f, warn = FALSE)
    starts <- grep("^@", lines)
    keys <- sub(",$", "", sub("^@[a-zA-Z]+\\{", "", lines[starts]))
    ends <- c(starts[-1] - 1, length(lines))
    stats::setNames(lapply(seq_along(starts), function(i) {
      b <- lines[starts[i]:ends[i]]
      field <- function(nm) {
        j <- grep(paste0("^\\s*", nm, "\\s*="), b)
        if (!length(j)) return(NA_character_)
        trimws(gsub("[{}\",]+$|^[{\"]+", "", sub("^[^=]*=\\s*", "", b[j[1]])))
      }
      list(year = field("year"), journal = field("journal"))
    }), keys)
  }
  inst <- parse_bib(inst_bib)
  vig <- parse_bib(vig_bib)

  # a key defined in both files must describe the same publication
  for (k in intersect(names(inst), names(vig))) {
    expect_identical(inst[[k]]$year, vig[[k]]$year, info = k)
    expect_identical(tolower(inst[[k]]$journal), tolower(vig[[k]]$journal),
                     info = k)
  }

  # every \insertRef key in the sources resolves in inst/REFERENCES.bib
  r_files <- list.files(file.path(root, "R"), "\\.R$", full.names = TRUE)
  used <- unlist(lapply(r_files, function(f) {
    l <- readLines(f, warn = FALSE)
    m <- regmatches(l, regexpr("insertRef\\{[^}]+\\}", l))
    gsub("insertRef\\{|\\}", "", m)
  }))
  expect_true(length(used) > 20)
  expect_true(all(used %in% names(inst)),
              info = paste(setdiff(used, names(inst)), collapse = ", "))

  # every @key cited in a vignette resolves in the vignette bibliography
  rmds <- list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE)
  cited <- unlist(lapply(rmds, function(f) {
    l <- readLines(f, warn = FALSE)
    m <- unlist(regmatches(l, gregexpr("@[A-Za-z][A-Za-z0-9]*[0-9]{4}[a-z0-9]*", l)))
    sub("^@", "", m)
  }))
  expect_true(all(unique(cited) %in% names(vig)),
              info = paste(setdiff(unique(cited), names(vig)), collapse = ", "))

  # and the TGUH / ecp entries specifically, since those were the two wrong
  expect_identical(vig[["fryzlewicz2018tail"]]$year, "2018")
  expect_identical(inst[["james2014ecp"]]$year, "2014")
  expect_match(inst[["james2014ecp"]]$journal, "Journal of Statistical Software")
})

test_that("R45: exported surface that the suite never exercised", {
  # Coverage measurement found two exported functions with no test at all,
  # and a number of documented modes and arguments that nothing called. They
  # work -- but nothing was guarding them against a future refactor.
  set.seed(45)
  x <- c(rnorm(80), rnorm(80, 5))
  X2 <- cbind(a = x, b = c(rnorm(80), rnorm(80, -4)))

  ## ggcpt_compare_table() -- previously 0% covered
  tb <- ggcpt_compare_table(x, methods = c("pelt", "binseg"))
  expect_s3_class(tb, "tbl_df")
  expect_identical(names(tb), c("method", "cp", "cp_value"))
  expect_setequal(unique(tb$method), c("pelt", "binseg"))
  # a method that finds nothing still gets a row, with NA
  flat <- rnorm(80)
  tb0 <- ggcpt_compare_table(flat, methods = "pelt")
  expect_equal(nrow(tb0), 1L)
  if (is.na(tb0$cp)) expect_true(is.na(tb0$cp_value))

  ## cpt_metrics_annotated() -- previously 0% covered
  ma <- cpt_metrics_annotated(c(100), list(c(100), c(102), c(98)), n = 300,
                              margin = 5)
  expect_equal(nrow(ma), 1L)
  expect_equal(ma$n_annotators, 3L)
  expect_equal(ma$f1, 1)
  # a bare vector is promoted to a single annotation set
  mb <- cpt_metrics_annotated(100, 100, n = 300)
  expect_equal(mb$n_annotators, 1L)
  expect_equal(mb$covering, 1)

  ## autoplot(show_segments = TRUE) -- the segment-level overlay
  res <- cpt_detect(x, method = "pelt")
  p <- ggplot2::autoplot(res, show_segments = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_gt(length(p$layers), length(ggplot2::autoplot(res)$layers))

  ## the no-changepoint print paths
  empty <- cpt_detect(flat, method = "pelt")
  expect_output(print(empty), "No changepoints detected")
  expect_output(print(cpt_stability(flat, B = 3, seed = 1)),
                "No changepoints detected in the original fit")

  ## format_penalty()'s numeric branch (a manual penalty prints its value)
  expect_output(print(cpt_detect(x, method = "pelt", penalty = 17.845951)),
                "Manual = 17\\.846")

  ## ecp's second algorithm
  expect_s3_class(ecp_wrapper(x, algorithm = "agglo"), "tbl_df")

  ## cpt_simulate()'s other two noise models, and the one untested signal
  for (nz in c("ar1", "rw")) {
    d <- cpt_simulate(120, changepoints = 60, params = c(0, 5),
                      noise = nz, rho = 0.4, seed = 1)
    expect_equal(nrow(d), 120L)
    expect_true(all(is.finite(d$value)))
  }
  sm <- signal_mix(n = 400, seed = 1)
  expect_equal(nrow(sm), 400L)
  expect_true(all(attr(sm, "true_changepoints") %in% seq_len(399)))
  expect_error(signal_mix(n = 10), "at least 40")
})

test_that("R45b: documented wrapper arguments that nothing called", {
  set.seed(46)
  x <- c(rnorm(80), rnorm(80, 5))
  X2 <- cbind(a = x, b = c(rnorm(80), rnorm(80, -4)))
  y_slope <- cumsum(c(rep(0.4, 80), rep(-0.3, 80))) + rnorm(160)

  skip_if_not_installed("mosum")
  # multiscale with an explicit bandwidth grid
  expect_s3_class(mosum_wrapper(x, G = c(20, 40), multiscale = TRUE), "ggcpt")

  skip_if_not_installed("cpop")
  expect_s3_class(cpop_wrapper(y_slope, sd = 1), "ggcpt")

  skip_if_not_installed("strucchange")
  r <- strucchange_wrapper(x, breaks = 2)
  expect_s3_class(r, "ggcpt")
  expect_lte(nrow(r$changepoints), 2)

  skip_if_not_installed("DeCAFS")
  expect_s3_class(
    decafs_wrapper(x, model_param = list(sdEta = 0, sdNu = 1, phi = 0)),
    "ggcpt")

  skip_if_not_installed("SNSeg")
  # the bivariate-correlation mode, and its two input guards
  Xc <- cbind(a = c(rnorm(100), rnorm(100)),
              b = c(rnorm(100), rnorm(100)))
  expect_s3_class(sn_wrapper(Xc, parameter = "bivcor"), "ggcpt")
  expect_error(sn_wrapper(x, parameter = "bivcor"), "two-column matrix")
  expect_error(sn_wrapper(X2, parameter = "mean"), "requires a numeric vector")

  skip_if_not_installed("InspectChangepoint")
  # an explicit lambda and threshold skip the Monte Carlo calibration
  expect_s3_class(inspect_wrapper(X2, lambda = 1, threshold = 1), "ggcpt")

  skip_if_not_installed("kcpRS")
  expect_error(kcp_wrapper(x, running_stat = "corr", nperm = 20),
               "at least two columns")
})

test_that("R46: glance() has no unreachable class branch, and every engine's
           $fit matches what the documentation promises", {
  set.seed(47)
  x <- c(rnorm(120), rnorm(120, 5))
  X <- cbind(a = x, b = rev(x), c = rnorm(240))

  # glance() used to carry a branch on class "cptrange". The changepoint
  # package's class is "cpt.range" -- with a dot -- so the branch could never
  # fire, and its body used `$` on an S4 object, which would have errored had
  # it ever been reached. Only the changepoint family and ecp are needed to
  # pin that; the one-row glance() contract for the rest is covered by
  # expect_ggcpt_contract() in test-040-wrappers.R and by R37.
  fit_classes <- character()
  for (m in c("pelt", "binseg", "segneigh", "amoc", "np", "ecp")) {
    dat <- if (m == "ecp") X else x
    r <- suppressWarnings(cpt_detect(dat, method = m))
    fit_classes <- c(fit_classes, class(r$fit))
    g <- glance(r)
    expect_equal(nrow(g), 1L, info = m)
    expect_length(g$total_cost, 1L)
  }
  expect_false("cptrange" %in% fit_classes)
  expect_true("cpt.range" %in% fit_classes)   # BinSeg / SegNeigh really do

  # ecp is the documented exception: no fit is retained, because e.agglo()'s
  # progression matrix is quadratic in the series length
  expect_null(cpt_detect(X, method = "ecp")$fit)
  # and every other engine does store one
  for (m in c("pelt", "binseg", "amoc", "np")) {
    expect_false(is.null(suppressWarnings(cpt_detect(x, method = m))$fit),
                 info = m)
  }
})

test_that("R47: the Bayesian displays' remaining documented paths", {
  # `?ggcpt_posterior` says it works with bcp_wrapper() and beast_wrapper()
  # results, but only the bcp branch of posterior_prob_profile() was ever
  # exercised. The guards on ggcpt_runlength() had no test either.
  set.seed(48)
  x <- c(rnorm(80), rnorm(80, 5))

  # not a ggcpt at all
  expect_error(ggcpt_posterior(x), "must be a ggcpt object")
  expect_error(ggcpt_runlength(x), "produced by bocpd_wrapper")
  # a ggcpt from an engine with no posterior
  res <- cpt_detect(x, method = "pelt")
  expect_error(ggcpt_posterior(res), "No posterior probability profile")
  expect_error(ggcpt_runlength(res), "produced by bocpd_wrapper")

  skip_if_not_installed("ocp")
  ro <- bocpd_wrapper(x)
  expect_s3_class(ggcpt_runlength(ro), "ggplot")
  # a prob_floor above 1 leaves nothing to draw and says so
  expect_error(ggcpt_runlength(ro, prob_floor = 2), "prob_floor")

  # Rbeast is gated off on Windows for the same reason its examples are
  skip_if_not_installed("Rbeast")
  skip_on_os("windows")
  rb <- beast_wrapper(x, seed = 1)
  p <- ggcpt_posterior(rb)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # the probability panel is length-n, in range, and agrees with the
  # posterior_prob column at every reported changepoint
  bottom <- p$data[p$data$panel == levels(p$data$panel)[2], ]
  expect_equal(nrow(bottom), length(x))
  expect_true(all(bottom$y >= 0 & bottom$y <= 1))
  if (nrow(rb$changepoints)) {
    expect_equal(bottom$y[rb$changepoints$cp], rb$changepoints$posterior_prob)
  }
})

test_that("R48: the greedy one-to-one matching really is a maximum matching", {
  # ?cpt_metrics claims the greedy scan "yields a maximum matching for
  # interval-structured problems". Precision and recall are derived from it,
  # so if it ever fell short they would be silently understated. Check it
  # against an exact maximum bipartite matching.
  max_matching <- function(pred, truth, margin) {
    np <- length(pred); nt <- length(truth)
    if (np == 0L || nt == 0L) return(0L)
    adj <- lapply(seq_len(np), function(i) which(abs(pred[i] - truth) <= margin))
    e <- new.env(parent = emptyenv())
    e$matchT <- rep(NA_integer_, nt)
    aug <- function(u) {
      for (v in adj[[u]]) {
        if (!e$seen[v]) {
          e$seen[v] <- TRUE
          w <- e$matchT[v]
          if (is.na(w) || aug(w)) { e$matchT[v] <- u; return(TRUE) }
        }
      }
      FALSE
    }
    res <- 0L
    for (u in seq_len(np)) {
      e$seen <- rep(FALSE, nt)
      if (aug(u)) res <- res + 1L
    }
    res
  }
  # the matcher itself must be right before it can be used as an oracle
  expect_equal(max_matching(c(10, 11), c(10, 11), 0), 2L)
  expect_equal(max_matching(c(10, 11), 10, 5), 1L)

  set.seed(49)
  for (i in seq_len(300)) {
    n <- sample(20:120, 1)
    margin <- sample(0:12, 1)
    kp <- sample(0:8, 1); kt <- sample(0:8, 1)
    p <- if (kp) sort(sample(seq_len(n - 1), kp)) else integer(0)
    tr <- if (kt) sort(sample(seq_len(n - 1), kt)) else integer(0)
    expect_equal(nrow(ggchangepoint:::match_changepoints(p, tr, margin)),
                 max_matching(p, tr, margin),
                 info = sprintf("margin=%d pred=[%s] truth=[%s]", margin,
                                paste(p, collapse = ","),
                                paste(tr, collapse = ",")))
  }

  # clustered and interleaved patterns, where a greedy rule is most at risk
  adv <- list(list(c(10, 11, 12), c(10, 20), 5), list(c(10, 20), c(10, 11, 12), 5),
              list(c(1, 6, 11), c(5, 10), 5),    list(c(5, 10), c(1, 6, 11), 5),
              list(c(10, 12, 14, 16), c(11, 15), 2),
              list(seq(10, 50, by = 5), seq(12, 52, by = 10), 3),
              list(3, c(1, 2, 3, 4, 5), 2))
  for (a in adv) {
    expect_equal(nrow(ggchangepoint:::match_changepoints(a[[1]], a[[2]], a[[3]])),
                 max_matching(a[[1]], a[[2]], a[[3]]),
                 info = paste(a[[3]]))
  }

  # and the consequence: recall can never exceed 1, however clustered
  m <- cpt_metrics(c(99, 100, 101), 100, n = 300, margin = 5)
  expect_lte(m$recall, 1)
  expect_equal(m$recall, 1)
  expect_equal(m$precision, 1 / 3)
})

test_that("R49: cpt_batch() names the series that failed", {
  # cpt_batch() exists for panels of hundreds of series; "`x` must have at
  # least 3 observations" on its own left the user to bisect the list to
  # find which one it meant.
  set.seed(50)
  x <- c(rnorm(80), rnorm(80, 5))

  e <- expect_error(
    cpt_batch(list(good = x, short = c(1, 2), also_good = rev(x))))
  expect_match(conditionMessage(e), "`short`", fixed = TRUE)
  expect_match(conditionMessage(e), "2 of 3", fixed = TRUE)
  expect_match(conditionMessage(e), "at least 3 observations")

  # matrix columns are named the same way
  e2 <- expect_error(cpt_batch(cbind(ok = x, bad = c(rnorm(159), NA))))
  expect_match(conditionMessage(e2), "`bad`", fixed = TRUE)
  expect_match(conditionMessage(e2), "must be finite")

  # and the healthy path is untouched: same names, same results, list named
  X <- cbind(a = x, b = rev(x), c = rnorm(160))
  b <- cpt_batch(X, method = "pelt", seed = 1)
  expect_identical(b$series, c("a", "b", "c"))
  expect_identical(names(b$result), c("a", "b", "c"))
  expect_true(all(vapply(b$result, is_ggcpt, logical(1))))
  # n_changepoints carries the series names, as it did before -- vapply over
  # a named list keeps them. Harmless inside a tibble, so left alone.
  expect_equal(unname(b$n_changepoints),
               vapply(b$result, function(r) nrow(r$changepoints),
                      integer(1), USE.NAMES = FALSE))
  expect_s3_class(ggplot2::autoplot(b), "ggplot")
})

test_that("R50: idetect returns the empty result on a constant series", {
  skip_if_not_installed("IDetect")
  # IDetect::ID() is erratic on flat input -- its statistics go to 0/0 and
  # what comes back depends on the value and the length. rep(3, 200) came
  # back with 126 changepoints at 1, 3, 4, 6, 7, ...; rep(0, 100) errors;
  # rep(-2.5, 60) returns the sentinel 0. The 0.4.0 audit caught this class
  # of bug for segmented/sn/kcp/npmojo/inspect but missed idetect.
  for (v in list(rep(3, 200), rep(0, 100), rep(-2.5, 60), rep(1e6, 300))) {
    r <- suppressWarnings(idetect_wrapper(v))
    expect_equal(nrow(r$changepoints), 0L, info = paste(v[1], length(v)))
    expect_equal(nrow(r$segments), 1L, info = paste(v[1], length(v)))
    expect_s3_class(r, "ggcpt")
  }
  # and through the dispatcher, with the usual contract intact
  r <- suppressWarnings(cpt_detect(rep(3, 200), method = "idetect"))
  expect_equal(nrow(r$changepoints), 0L)
  expect_equal(nrow(glance(r)), 1L)

  # a genuine signal is untouched, and so is one with tiny but real variation
  set.seed(51)
  x <- c(rnorm(150), rnorm(150, 4))
  expect_true(any(abs(suppressWarnings(idetect_wrapper(x))$changepoints$cp -
                        150) <= 5))
  tiny <- rep(3, 200) + rnorm(200, 0, 1e-9)
  expect_s3_class(suppressWarnings(idetect_wrapper(tiny)), "ggcpt")

  # every search engine now agrees that a flat series has no changepoint
  for (m in c("wbs", "not", "wbs2", "tguh", "idetect", "mosum")) {
    skip_if_not_installed(switch(m, wbs = "wbs", not = "not",
                                 wbs2 = "breakfast", tguh = "breakfast",
                                 idetect = "IDetect", mosum = "mosum"))
    expect_equal(
      nrow(suppressWarnings(cpt_detect(rep(3, 200), method = m))$changepoints),
      0L, info = m)
  }
})

test_that("R51: the dispatcher's change_in translations reach the engine", {
  # cpt_detect() derives an engine-specific argument from `change_in` for
  # not/cpm/kcp/sn/fastcpd. Tests covered *overriding* those through `...`,
  # but never the derivation itself -- so a wrong translation would have
  # silently run the wrong analysis.
  set.seed(52)
  x <- c(rnorm(150), rnorm(150, 4))
  X <- cbind(a = x, b = rev(x), c = rnorm(300))

  skip_if_not_installed("SNSeg")
  expect_identical(
    suppressWarnings(cpt_detect(x, method = "sn",
                                change_in = "var"))$changepoints$cp,
    suppressWarnings(sn_wrapper(x, parameter = "variance"))$changepoints$cp)

  skip_if_not_installed("cpm")
  expect_identical(cpt_detect(x, method = "cpm", change_in = "var")$changepoints$cp,
                   cpm_wrapper(x, cpm_type = "Mood")$changepoints$cp)
  expect_identical(
    cpt_detect(x, method = "cpm", change_in = "distribution")$changepoints$cp,
    cpm_wrapper(x, cpm_type = "Kolmogorov-Smirnov")$changepoints$cp)

  skip_if_not_installed("kcpRS")
  expect_identical(
    suppressWarnings(cpt_detect(x, method = "kcp", change_in = "var",
                                nperm = 20, seed = 1))$change_in,
    "running var")

  skip_if_not_installed("fastcpd")
  expect_identical(cpt_detect(X, method = "fastcpd", change_in = "var")$change_in,
                   "var")
  expect_identical(cpt_detect(X, method = "fastcpd",
                              change_in = "meanvar")$change_in, "meanvar")

  skip_if_not_installed("not")
  # not's contrast: "var" maps to the mean-and-variance contrast
  expect_identical(cpt_detect(x, method = "not", change_in = "var",
                              seed = 1)$change_in, "meanvar")
})

test_that("R52: envcpt does not print upstream try() failures as if it had
           failed", {
  skip_if_not_installed("EnvCpt")
  # EnvCpt fits up to twelve models with try(), and a non-silent try() prints
  # its error straight to stderr. On a degenerate series several AR fits fail
  # that way, so the call succeeded but first printed "Error in arima(...):
  # non-stationary AR part from CSS" -- six lines that read as a failure.
  quiet <- function(expr) {
    out <- capture.output(res <- force(expr), type = "message")
    list(stderr = out[nzchar(trimws(out))], res = res)
  }
  flat <- quiet(envcpt_wrapper(rep(3, 200)))
  expect_length(flat$stderr, 0L)
  expect_s3_class(flat$res, "ggcpt")
  expect_equal(nrow(flat$res$changepoints), 0L)

  # ordinary data is unaffected: still silent, still finds the change
  set.seed(53)
  x <- c(rnorm(150), rnorm(150, 4))
  ok <- quiet(envcpt_wrapper(x))
  expect_length(ok$stderr, 0L)
  expect_true(any(abs(ok$res$changepoints$cp - 150) <= 5))

  # The risk of diverting the message stream is hiding a real failure, so
  # check that one still gets through: a minimum segment length longer than
  # the series leaves the engine nothing to fit.
  expect_error(envcpt_wrapper(rnorm(10), minseglen = 400))

  # (whether a given series also triggers an upstream convergence *warning*
  # is data-dependent, so it is not asserted here; warnings are deferred past
  # the diversion by construction and reach the user unchanged.)
})

test_that("R53: hsmuce refuses input that would abort the R session", {
  skip_if_not_installed("stepR")
  # stepR's heterogeneous variance estimator does not raise an R error when
  # the data carry essentially no noise at the per-segment scale -- it
  # terminates the session, so nothing can catch it and the user loses their
  # work. Two regimes, both measured: a globally flat series, and (more
  # dangerous, because it looks ordinary) a clean step whose segments are
  # numerically constant -- which is what cpt_simulate(sd = 0) yields once
  # any rounding is added.
  fatal <- list(
    list(base = 4, jump = 0, sd = 1e-12), list(base = 4, jump = 0, sd = 1e-9),
    list(base = 4, jump = 0, sd = 2e-7), list(base = 1000, jump = 0, sd = 1e-9),
    list(base = 0, jump = 5, sd = 1e-9), list(base = 0, jump = 5, sd = 5e-8),
    list(base = 0, jump = 1, sd = 1e-9), list(base = 0, jump = 1000, sd = 1e-7)
  )
  for (f in fatal) {
    set.seed(300)
    v <- f$base + c(rep(0, 150), rep(f$jump, 150)) + rnorm(300, 0, f$sd)
    lbl <- sprintf("base=%g jump=%g sd=%g", f$base, f$jump, f$sd)
    expect_error(smuce_wrapper(v, family = "hsmuce"),
                 "orders of magnitude below", info = lbl)
    expect_error(cpt_detect(v, method = "hsmuce"),
                 "orders of magnitude below", info = lbl)
    # the message names family = "gauss", and that really does cope
    expect_s3_class(smuce_wrapper(v, family = "gauss"), "ggcpt")
  }

  # an exactly noiseless series is safe upstream and must keep working --
  # both the flat one and the clean step
  expect_equal(nrow(smuce_wrapper(rep(4, 300), family = "hsmuce")$changepoints),
               0L)
  expect_equal(
    nrow(smuce_wrapper(c(rep(0, 150), rep(5, 150)),
                       family = "hsmuce")$changepoints), 1L)

  # ordinary data is untouched
  set.seed(53)
  x <- c(rnorm(150), rnorm(150, 3))
  expect_true(any(abs(smuce_wrapper(x, family = "hsmuce")$changepoints$cp -
                        150) <= 5))
  expect_true(any(abs(cpt_detect(x, method = "hsmuce")$changepoints$cp -
                        150) <= 5))
  # and so is data whose noise is small but not degenerate
  set.seed(300)
  ok <- rep(4, 300) + rnorm(300, 0, 1e-6)
  expect_s3_class(smuce_wrapper(ok, family = "hsmuce"), "ggcpt")

  # the whole pipeline survives the series that first exposed this
  d0 <- cpt_simulate(300, changepoints = 150, params = c(0, 5), sd = 0,
                     seed = 1)
  set.seed(2)
  expect_error(cpt_detect(d0$value + rnorm(300, 0, 1e-9), method = "hsmuce"),
               "orders of magnitude below")
})

test_that("R54: the package's own simulator never feeds hsmuce fatal input", {
  skip_if_not_installed("stepR")
  # cpt_simulate() is how a user generates ground truth to benchmark against,
  # so its output must not be able to terminate the session. Two of its
  # configurations land in the degenerate band: sd = 0 for a change in mean
  # once any rounding is added, and -- found by auditing every generator
  # setting -- change_in = "slope" with sd = 0, whose consecutive differences
  # are a constant slope, so mad(diff) is pure floating-point residue
  # (about 1e-14) rather than zero.
  slope_pars <- list(list(intercept = 0, slope = 0.2),
                     list(intercept = 30, slope = -0.2))
  degenerate <- list(
    cpt_simulate(300, 150, "slope", params = slope_pars, sd = 0, seed = 1),
    cpt_simulate(300, 150, "slope", params = slope_pars, sd = 1e-9, seed = 1),
    cpt_simulate(300, 150, "mean", params = c(0, 5), sd = 1e-9, seed = 1)
  )
  for (d in degenerate) {
    expect_lt(stats::mad(diff(d$value)), 1e-7 * max(abs(d$value)))
    expect_error(smuce_wrapper(d$value, family = "hsmuce"),
                 "orders of magnitude below")
    # gauss copes with all of them
    expect_s3_class(smuce_wrapper(d$value, family = "gauss"), "ggcpt")
  }

  # the configurations a user actually reaches for are not degenerate and
  # run normally
  healthy <- list(
    cpt_simulate(300, 150, "mean", params = c(0, 5), seed = 1),
    cpt_simulate(300, 150, "var", params = c(1, 6), seed = 1),
    cpt_simulate(300, 150, "meanvar",
                 params = list(list(mean = 0, sd = 1), list(mean = 5, sd = 3)),
                 seed = 1),
    cpt_simulate(300, 150, "slope", params = slope_pars, seed = 1)
  )
  for (d in healthy) {
    expect_gt(stats::mad(diff(d$value)), 1e-7 * max(abs(d$value)))
    expect_s3_class(smuce_wrapper(d$value, family = "hsmuce"), "ggcpt")
  }

  # and neither are the canonical signals
  for (f in c("signal_blocks", "signal_fms", "signal_mix", "signal_teeth",
              "signal_stairs")) {
    v <- do.call(f, list(n = 600, seed = 1))$value
    expect_gt(stats::mad(diff(v)), 1e-7 * max(abs(v)), label = f)
  }
})

test_that("R55: the documented simulate-detect-evaluate-plot workflow holds
           end to end", {
  # Each piece of the README/vignette workflow is tested on its own, but not
  # the chain: a result's changepoints feeding cpt_metrics() and
  # ggcpt_eval(), its segments feeding geom_cpt_segment(), the object itself
  # feeding cpt_cite(). Six methods spanning the structural variety --
  # extra CI columns, a fitted signal, a posterior column, a multivariate
  # data_wide, and a slope engine -- rather than all 31, to keep the suite
  # quick.
  d <- cpt_simulate(300, changepoints = c(100, 200), change_in = "mean",
                    params = c(0, 6, -3), sd = 1, seed = 11)
  truth <- attr(d, "true_changepoints")
  v <- d$value
  n <- length(v)
  X <- cbind(a = v, b = rev(v))
  df <- data.frame(index = seq_len(n), value = v)

  for (m in c("pelt", "fpop", "smuce", "bcp", "ecp", "segmented")) {
    skip_if_not_installed(switch(m, pelt = "changepoint", fpop = "fpop",
                                 smuce = "stepR", bcp = "bcp", ecp = "ecp",
                                 segmented = "segmented"))
    dat <- if (m == "ecp") X else v
    res <- suppressWarnings(cpt_detect(dat, method = m))
    cp <- tidy(res)$cp

    mm <- suppressWarnings(cpt_metrics(cp, truth, n = n))
    expect_equal(nrow(mm), 1L, info = m)
    expect_true(all(vapply(mm[c("precision", "recall", "f1", "covering")],
                           function(z) z >= 0 && z <= 1, logical(1))), info = m)

    expect_no_error(ggplot2::ggplot_build(
      suppressWarnings(ggcpt_eval(cp, truth, v))))
    expect_no_error(ggplot2::ggplot_build(
      ggplot2::ggplot(df, ggplot2::aes(index, value)) + ggplot2::geom_line() +
        annotate_segments(cp, n)))
    expect_no_error(ggplot2::ggplot_build(
      ggplot2::ggplot(df, ggplot2::aes(index, value)) + ggplot2::geom_line() +
        geom_cpt_segment(data = res$segments,
                         ggplot2::aes(x = start, xend = end,
                                      y = param_estimate, yend = param_estimate),
                         inherit.aes = FALSE)))
    expect_output(cpt_cite(res))
  }

  # the step engines really do recover both planted changepoints
  for (m in c("pelt", "fpop", "smuce")) {
    skip_if_not_installed(switch(m, pelt = "changepoint", fpop = "fpop",
                                 smuce = "stepR"))
    mm <- suppressWarnings(
      cpt_metrics(tidy(cpt_detect(v, method = m))$cp, truth, n = n))
    expect_equal(mm$f1, 1, info = m)
  }
})

test_that("R56: ocd accepts an explicit threshold, skipping calibration", {
  skip_if_not_installed("ocd")
  # Almost all of ocd's cost is Monte Carlo threshold calibration -- about
  # 55 s at p = 50 with only mc_reps = 5, and the default is 100. `thresh`
  # takes the three thresholds directly and bypasses it, but nothing
  # exercised that path.
  set.seed(400)
  n <- 120
  p <- 6
  X <- matrix(rnorm(n * p), n, p)
  X[61:120, 1:2] <- X[61:120, 1:2] + 4
  colnames(X) <- paste0("v", seq_len(p))

  res <- ocd_wrapper(X, thresh = c(diag = 10, off_d = 10, off_s = 10))
  expect_s3_class(res, "ggcpt")
  expect_identical(res$method, "ocd")
  expect_true("declared_at" %in% names(res$changepoints))
  expect_true(all(res$changepoints$cp >= 1 &
                    res$changepoints$cp < nrow(res$data)))
  expect_equal(nrow(glance(res)), 1L)
  # and it reaches the dispatcher the same way
  expect_s3_class(
    cpt_detect(X, method = "ocd",
               thresh = c(diag = 10, off_d = 10, off_s = 10)), "ggcpt")
})

test_that("R57: the package's own arguments enforce their documented ranges", {
  # The engines validate their own arguments -- stepR refuses an alpha
  # outside (0, 1), SNSeg an unlisted confidence -- but this package's own
  # arguments were taken on trust, and out-of-range values there returned
  # answers rather than errors. Measured before the fix: margin = -3 scored a
  # *perfect* segmentation as precision 0 and recall 0; B = 0 produced a
  # stability profile of NaN; n = -10 a covering metric of -1; and
  # prob_threshold = 0 reported 239 changepoints in a 240-point series.
  set.seed(57)
  x <- c(rnorm(120), rnorm(120, 5))

  expect_error(cpt_metrics(integer(0), integer(0), n = 0), "`n` must be")
  expect_error(cpt_metrics(integer(0), integer(0), n = -10), "`n` must be")
  expect_error(cpt_metrics(integer(0), integer(0), n = NA), "`n` must be")
  expect_error(cpt_metrics(120, 120, n = 240, margin = -3), "`margin` must be")
  expect_error(cpt_stability(x, B = 0), "`B` must be")
  expect_error(cpt_stability(x, B = 3, margin = -3), "`margin` must be")
  expect_error(cpt_crops(x, pen_min = -5, pen_max = 10), "`pen_min` must be")

  skip_if_not_installed("bcp")
  expect_error(bcp_wrapper(x, prob_threshold = 0), "`prob_threshold` must be")
  expect_error(bcp_wrapper(x, prob_threshold = 2), "`prob_threshold` must be")
  expect_error(bcp_wrapper(x, prob_threshold = -0.5),
               "`prob_threshold` must be")

  skip_if_not_installed("kcpRS")
  expect_error(kcp_wrapper(x, alpha = 0, nperm = 20), "`alpha` must be")
  expect_error(kcp_wrapper(x, alpha = 2, nperm = 20), "`alpha` must be")

  # the boundaries that are legitimate still are
  expect_equal(nrow(cpt_metrics(integer(0), integer(0), n = 1)), 1L)
  expect_equal(cpt_metrics(120, 120, n = 240, margin = 0)$f1, 1)
  expect_s3_class(cpt_stability(x, B = 1, seed = 1), "ggcpt_stability")
  expect_s3_class(cpt_stability(x, B = 3, margin = 0, seed = 1),
                  "ggcpt_stability")
  expect_s3_class(bcp_wrapper(x, prob_threshold = 1, seed = 1), "ggcpt")
  expect_s3_class(suppressWarnings(kcp_wrapper(x, alpha = 0.05, nperm = 20,
                                               seed = 1)), "ggcpt")
  expect_s3_class(cpt_crops(x), "ggcpt_path")
})

test_that("R58: the logical switches refuse non-logical values instead of
           silently doing the opposite", {
  # These are all documented as "Logical", but they were read with isTRUE(),
  # which treats every non-TRUE value as FALSE. Measured before the fix:
  # show_segments = 1, "yes", "TRUE" or NA all silently drew nothing, and --
  # worse -- show_line = 1 silently *removed* the line the user was asking to
  # keep, taking the plot from three layers to one.
  set.seed(58)
  x <- c(rnorm(120), rnorm(120, 5))
  skip_if_not_installed("stepR")
  res <- suppressWarnings(smuce_wrapper(x))
  nlayers <- function(p) length(ggplot2::ggplot_build(p)$plot$layers)

  for (v in list(1, 0, "yes", "TRUE", NA, c(TRUE, TRUE))) {
    lbl <- paste(format(v), collapse = ",")
    expect_error(ggplot2::autoplot(res, show_segments = v),
                 "`show_segments` must be TRUE or FALSE", info = lbl)
    expect_error(ggplot2::autoplot(res, show_fit = v),
                 "`show_fit` must be TRUE or FALSE", info = lbl)
    expect_error(ggplot2::autoplot(res, show_ci = v),
                 "`show_ci` must be TRUE or FALSE", info = lbl)
    expect_error(ggplot2::autoplot(res, show_line = v),
                 "`show_line` must be TRUE or FALSE", info = lbl)
  }
  expect_error(ggcptplot(x, show_line = "y"), "`show_line` must be")
  expect_error(ggecpplot(x, show_points = 1), "`show_points` must be")
  skip_if_not_installed("mosum")
  expect_error(mosum_wrapper(x, multiscale = "yes"), "`multiscale` must be")

  # the real values still behave, and each overlay still adds its layer
  base <- nlayers(ggplot2::autoplot(res))
  expect_gt(nlayers(ggplot2::autoplot(res, show_segments = TRUE)), base)
  expect_gt(nlayers(ggplot2::autoplot(res, show_ci = TRUE, show_fit = TRUE)),
            base)
  expect_lt(nlayers(ggplot2::autoplot(res, show_line = FALSE)), base)
  # show_points = NULL is the documented "decide from the series length"
  expect_equal(nlayers(ggplot2::autoplot(res, show_points = NULL)), base)
  expect_s3_class(ggcptplot(x), "ggplot")
  expect_s3_class(ggecpplot(x, show_points = FALSE), "ggplot")
  expect_s3_class(mosum_wrapper(x, multiscale = TRUE), "ggcpt")
})

test_that("R59: cpt_simulate() refuses parameters that make it emit NaN", {
  # The simulator is where ground truth for every benchmark comes from, so
  # silently returning a series of NaN is the worst thing it can do.
  # Measured before the fix: sd = -1, sd = NA, and |rho| >= 1 under the AR(1)
  # model each produced a tibble whose `value` column was entirely NaN, with
  # no error and no warning.
  expect_error(cpt_simulate(100, 50, params = c(0, 5), sd = -1), "`sd` must be")
  expect_error(cpt_simulate(100, 50, params = c(0, 5), sd = NA), "`sd` must be")
  for (r in c(1, -1, 1.5, -2)) {
    expect_error(
      cpt_simulate(100, 50, params = c(0, 5), noise = "ar1", rho = r),
      "`rho` must be", info = paste("rho =", r))
  }
  expect_error(cpt_simulate(0, params = 0), "`n` must be")
  expect_error(cpt_simulate(-10, params = 0), "`n` must be")

  # everything legitimate still runs, and returns finite values
  ok <- list(
    cpt_simulate(100, 50, params = c(0, 5)),
    cpt_simulate(100, 50, params = c(0, 5), sd = 0),
    cpt_simulate(100, 50, params = c(0, 5), noise = "ar1", rho = 0.9),
    cpt_simulate(100, 50, params = c(0, 5), noise = "ar1", rho = -0.9),
    cpt_simulate(100, 50, params = c(0, 5), noise = "rw"),
    cpt_simulate(100, 50, params = c(0, 5), noise = "t", df = 5),
    rcpt(100, 50, params = c(0, 5))
  )
  for (d in ok) {
    expect_equal(nrow(d), 100L)
    expect_false(anyNA(d$value))
  }
  # rho is only used by the AR(1) model, so an out-of-range value that the
  # chosen model ignores is not an error
  expect_false(anyNA(
    cpt_simulate(100, 50, params = c(0, 5), noise = "gauss", rho = 2)$value))
})

test_that("R60: an out-of-range conf_level no longer hangs strucchange", {
  set.seed(60)
  x <- c(rnorm(120), rnorm(120, 5))

  skip_if_not_installed("strucchange")
  # `stats::confint()` on a breakpoints fit at level = 2 never returns. The
  # wrapper already wrapped that call in tryCatch(), but a tryCatch cannot
  # rescue a call that does not terminate -- it has to be refused up front.
  # This test would hang, not fail, if the guard were removed.
  expect_error(strucchange_wrapper(x, conf_level = 2), "`conf_level` must be")
  expect_error(strucchange_wrapper(x, conf_level = -1), "`conf_level` must be")
  expect_error(strucchange_wrapper(x, conf_level = 0), "`conf_level` must be")
  expect_error(strucchange_wrapper(x, conf_level = 1), "`conf_level` must be")
  expect_s3_class(strucchange_wrapper(x, conf_level = 0.95), "ggcpt")
  expect_s3_class(strucchange_wrapper(x, conf_level = 0.5), "ggcpt")

  skip_if_not_installed("segmented")
  expect_error(segmented_wrapper(x, conf_level = 2, seed = 1),
               "`conf_level` must be")
  expect_s3_class(segmented_wrapper(x, conf_level = 0.9, seed = 1), "ggcpt")

  skip_if_not_installed("ocp")
  # a hazard rate is a positive quantity
  expect_error(bocpd_wrapper(x, hazard = -100), "`hazard` must be")
  expect_error(bocpd_wrapper(x, hazard = 0), "`hazard` must be")
  expect_s3_class(bocpd_wrapper(x, hazard = 100), "ggcpt")

  skip_if_not_installed("cpop")
  # so is a noise standard deviation, when one is supplied at all
  expect_error(cpop_wrapper(x, sd = -1), "`sd` must be")
  expect_error(cpop_wrapper(x, sd = 0), "`sd` must be")
  expect_s3_class(cpop_wrapper(x, sd = 1), "ggcpt")
  expect_s3_class(cpop_wrapper(x), "ggcpt")          # NULL means "estimate it"

  skip_if_not_installed("wbs")
  expect_error(wbs_wrapper(x, n_intervals = 0), "`n_intervals` must be")
  expect_s3_class(wbs_wrapper(x, n_intervals = 500), "ggcpt")
})

test_that("R61: cpm and kcp no longer report 'no changepoints' when the
           analysis never ran", {
  set.seed(61)
  x <- c(rnorm(120), rnorm(120, 5))

  skip_if_not_installed("cpm")
  # cpm ships thresholds for a fixed set of average run lengths. For any
  # other value processStream() *prints* "Error: No thresholds available for
  # selected ARL0" and returns an empty result instead of raising a
  # condition, so tryCatch() never saw it and the wrapper reported zero
  # changepoints on a series with an obvious one. Exactly the trap the 0.4.0
  # audit found for cpm_type = "GLRAdjusted", on a different argument.
  for (a in c(0, -5, 333, 20, 50)) {
    expect_error(cpm_wrapper(x, arl0 = a), "average run length",
                 info = paste("arl0 =", a))
  }
  expect_error(cpt_detect(x, method = "cpm", arl0 = 0), "average run length")
  # supported values still work, and nothing the engine prints leaks out
  for (a in c(100, 370, 500, 1000, 20000)) {
    out <- capture.output(res <- cpm_wrapper(x, arl0 = a))
    expect_s3_class(res, "ggcpt")
    expect_length(out, 0L)
  }
  expect_true(any(abs(cpm_wrapper(x, arl0 = 500)$changepoints$cp - 120) <= 5))

  skip_if_not_installed("kcpRS")
  # kcp's permutation test needs a permutation distribution: nperm = 0 or
  # negative reported no changepoints at all, and nperm = 1 died inside the
  # engine with "'row.names' is not a character or integer vector of
  # length 11".
  for (np in c(0, 1, -1)) {
    expect_error(kcp_wrapper(x, nperm = np), "`nperm` must be",
                 info = paste("nperm =", np))
  }
  expect_s3_class(suppressWarnings(kcp_wrapper(x, nperm = 2, seed = 1)),
                  "ggcpt")
})

test_that("R63: the comparison functions refuse wide input instead of
           flattening it", {
  # `ggcpt_compare()` and `ggcpt_compare_table()` run univariate detectors
  # but took `as.numeric(x)` on trust, so a 160x2 matrix was unrolled column
  # after column and the join between the columns read as a level shift: the
  # table came back with changepoints at 80 AND 160, and 160 is the seam, not
  # anything in either series. Every wrapper already refused wide input; only
  # these two did not.
  set.seed(63)
  X <- cbind(a = c(rnorm(80), rnorm(80, 5)), b = rnorm(160))
  expect_error(ggcpt_compare(X, methods = "pelt"), "2 columns")
  expect_error(ggcpt_compare_table(X, methods = "pelt"), "2 columns")
  # the message points at the function that does take a panel
  expect_error(ggcpt_compare_table(X, methods = "pelt"), "cpt_batch")

  # a single series still works, however it is shaped
  tb <- ggcpt_compare_table(X[, 1], methods = "pelt")
  expect_equal(tb$cp, 80L)
  expect_equal(ggcpt_compare_table(as.matrix(X[, 1]), methods = "pelt")$cp, 80L)
  expect_equal(
    ggcpt_compare_table(data.frame(a = X[, 1]), methods = "pelt")$cp, 80L)
  expect_s3_class(ggcpt_compare(X[, 1, drop = FALSE], methods = "pelt"),
                  "ggplot")

  # and non-numeric input names the argument rather than failing inside
  # as.numeric() with "cannot coerce type 'object' to vector of type
  # 'double'"
  expect_error(ggcpt_compare_table(ggplot2::ggplot()), "must be a numeric")
  expect_error(ggcpt_compare(letters), "must be a numeric")
})

test_that("R64: ggcpt_compare asks future.apply for a documented seed value", {
  # `future.seed` is documented as a logical, an integer, or a list of
  # pre-generated seeds. `ggcpt_compare()` passed `seed` straight through and
  # `seed` defaults to NULL, so every parallel run without an explicit seed
  # handed future.apply a value outside its contract. `cpt_batch()` already
  # sent TRUE there. This pins the two on the same behaviour without needing
  # a worker: read it off the source of the branch that runs.
  body_txt <- paste(deparse(body(ggcpt_compare)), collapse = " ")
  expect_match(body_txt, "future.seed = seed %||% TRUE", fixed = TRUE)
  expect_match(paste(deparse(body(cpt_batch)), collapse = " "),
               "future.seed = seed %||% TRUE", fixed = TRUE)
})

test_that("R65: the strucchange result-size note is accurate", {
  skip_if_not_installed("strucchange")
  skip_on_cran()
  # `?strucchange_wrapper` now warns that `$fit` is quadratic in n, because
  # `breakpoints()` keeps the triangular RSS table. Pin both halves of that
  # claim: the growth rate, and that the table is what dominates.
  sizes <- vapply(c(200L, 400L), function(n) {
    set.seed(5)
    x <- c(stats::rnorm(n / 2), stats::rnorm(n / 2, 4))
    as.numeric(utils::object.size(suppressWarnings(strucchange_wrapper(x))))
  }, numeric(1))
  # doubling n costs far more than doubling the object: superlinear, and in
  # practice close to 4x
  expect_gt(sizes[2] / sizes[1], 3)
  set.seed(5)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  fit <- suppressWarnings(strucchange_wrapper(x))$fit
  # The exact share moves with the R version's object accounting (0.85 here,
  # 0.79 on R-devel), so pin the structural claim instead: the table outweighs
  # everything else in the fit put together, and no other component is close.
  parts <- vapply(fit, function(e) as.numeric(utils::object.size(e)), numeric(1))
  expect_identical(names(which.max(parts)), "RSS.triang")
  expect_gt(parts[["RSS.triang"]], sum(parts[names(parts) != "RSS.triang"]))
  # and the rest of the package is not like this: a pelt result stays small
  expect_lt(as.numeric(utils::object.size(cpt_detect(x, method = "pelt"))),
            as.numeric(utils::object.size(x)) * 20)
})

test_that("R66: a planned method is named as planned, not denied", {
  # `cpt_methods()` lists four engines with status "planned". Asking the
  # dispatcher for one used to hit match.arg(), whose message enumerates the
  # 31 wired methods and therefore does not contain the name the user just
  # read out of the table -- the table said it exists, the dispatcher said it
  # does not.
  tb <- as.data.frame(cpt_methods())
  planned <- tb$method[tb$status == "planned"]
  expect_setequal(planned, c("gfpop", "robust", "focus", "sbs"))
  for (m in planned) {
    expect_error(cpt_detect(rnorm(50), method = m),
                 "planned but not wired", info = m)
    # and the message says what it is waiting on and what it will be built on
    expect_error(cpt_detect(rnorm(50), method = m),
                 tb$engine[tb$method == m], fixed = TRUE, info = m)
  }
  # an outright unknown name still gets the ordinary match.arg list
  expect_error(cpt_detect(rnorm(50), method = "nosuchmethod"), "should be one of")

  # planned rows carry no installed flag and every wired row does
  expect_true(all(is.na(tb$installed[tb$status == "planned"])))
  expect_false(any(is.na(tb$installed[tb$status == "available"])))
  # `sbs` waits on the wrapper, not on CRAN: hdbinseg is back on CRAN (1.0.3,
  # September 2025), while gfpop was removed and robseg/FOCuS never appeared
  expect_identical(tb$target_release[tb$method == "sbs"], "next release")
  expect_true(all(tb$target_release[tb$method %in% c("gfpop", "robust", "focus")] ==
                    "when on CRAN"))
})
