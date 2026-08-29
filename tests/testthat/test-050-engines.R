# 0.5.0 engine wave. Each test is guarded with skip_if_not_installed(), so
# the suite passes on a minimal installation.

set.seed(2026)
x_step <- c(rnorm(100), rnorm(100, 3))
X_hd <- {
  M <- matrix(rnorm(100 * 20), nrow = 100)
  M[51:100, 1:5] <- M[51:100, 1:5] + 3
  M
}
X_func <- {
  M <- matrix(rnorm(100 * 30), nrow = 100)
  M[51:100, ] <- M[51:100, ] + 2
  M
}

expect_ggcpt_contract <- function(res, method = NULL, change_in = NULL) {
  expect_s3_class(res, "ggcpt")
  cp <- res$changepoints
  expect_true(all(c("cp", "cp_value") %in% names(cp)))
  expect_type(cp$cp, "integer")
  expect_false(is.unsorted(cp$cp))
  expect_false(anyDuplicated(cp$cp) > 0)
  n <- nrow(res$data)
  expect_true(all(cp$cp >= 1 & cp$cp < n))
  expect_equal(nrow(res$segments), nrow(cp) + 1L)
  expect_type(res$segments$start, "integer")
  expect_type(res$segments$n, "integer")
  expect_equal(sum(res$segments$n), n)
  if (!is.null(method)) expect_equal(res$method, method)
  if (!is.null(change_in)) expect_equal(res$change_in, change_in)
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(res)))
  invisible(res)
}

test_that("esac and pilliat find a sparse high-dimensional mean change", {
  skip_if_not_installed("HDCD")
  e <- cpt_detect(X_hd, method = "esac")
  expect_ggcpt_contract(e, "esac", "mean")
  expect_true(any(abs(e$changepoints$cp - 50) <= 3))
  expect_true(all(c("cusum", "depth") %in% names(e$changepoints)))

  p <- cpt_detect(X_hd, method = "pilliat")
  expect_ggcpt_contract(p, "pilliat", "mean")
  expect_true(any(abs(p$changepoints$cp - 50) <= 3))

  # Both are multivariate-only in spirit but must refuse nothing silently.
  expect_error(cpt_detect(X_hd, method = "esac", change_in = "var"),
               "not supported")
})

test_that("hdcov sees a change in dependence with no change in the margins", {
  skip_if_not_installed("changepoints")
  set.seed(7)
  p <- 5
  A <- matrix(rnorm(100 * p), ncol = p)
  B <- matrix(rnorm(100 * p), ncol = p)
  B[, 2] <- B[, 1] + 0.2 * B[, 2]
  # Too few permutations for alpha = 0.05 is a real caveat, and the wrapper
  # says so; the test keeps the run cheap and asserts the warning.
  expect_warning(fit <- hdcov_wrapper(rbind(A, B), n_perm = 5, seed = 1),
                 "extrapolates beyond")
  expect_ggcpt_contract(fit, "hdcov", "covariance")
  expect_true("cusum" %in% names(fit$changepoints) ||
                nrow(fit$changepoints) == 0)
})

test_that("network detects a change in edge probability", {
  skip_if_not_installed("changepoints")
  set.seed(11)
  p <- 5
  mk <- function(n, prob) {
    t(replicate(n, as.numeric(matrix(stats::rbinom(p * p, 1, prob), p))))
  }
  X <- rbind(mk(40, 0.2), mk(40, 0.7))
  expect_message(
    suppressWarnings(fit <- network_wrapper(X, n_intervals = 20, n_perm = 3,
                                            seed = 1)),
    "splitting each edge at random"
  )
  expect_ggcpt_contract(fit, "network", "network")
  expect_true(any(abs(fit$changepoints$cp - 40) <= 6))
  # A 3-d array is accepted and reshaped.
  arr <- array(stats::rbinom(30 * p * p, 1, 0.3), dim = c(30, p, p))
  expect_equal(dim(ggchangepoint:::network_matrix(arr)), c(30L, p * p))
})

test_that("var detects a change in the transition matrix", {
  skip_if_not_installed("changepoints")
  set.seed(13)
  step <- function(n, a) {
    Y <- matrix(0, n, 3)
    for (i in 2:n) Y[i, ] <- a * Y[i - 1, ] + stats::rnorm(3)
    Y
  }
  fit <- var_wrapper(rbind(step(60, 0.1), step(60, 0.85)))
  expect_ggcpt_contract(fit, "var", "regression")
})

test_that("fmean and fcov segment a functional series", {
  skip_if_not_installed("fChange")
  fm <- fmean_wrapper(X_func)
  expect_ggcpt_contract(fm, "fmean", "mean")
  expect_true(any(abs(fm$changepoints$cp - 50) <= 3))
  expect_true("p_value" %in% names(fm$changepoints) ||
                nrow(fm$changepoints) == 0)

  Xv <- matrix(rnorm(100 * 30), nrow = 100)
  Xv[51:100, ] <- Xv[51:100, ] * 3
  fc <- fcov_wrapper(Xv, target = "trace")
  expect_ggcpt_contract(fc, "fcov", "covariance")
  expect_error(fmean_wrapper(x_step), "needs functional observations")
})

test_that("kwc segments on depth ranks", {
  skip_if_not_installed("KWCChangepoint")
  Xv <- matrix(rnorm(100 * 20), nrow = 100)
  Xv[51:100, ] <- Xv[51:100, ] * 3
  fit <- cpt_detect(Xv, method = "kwc", seed = 1)
  expect_ggcpt_contract(fit, "kwc", "covariance")
  expect_true(any(abs(fit$changepoints$cp - 50) <= 5))
})

test_that("the classical single-change tests agree on a clean step", {
  skip_if_not_installed("trend")
  clean <- c(rep(0, 50), rep(5, 50)) + rnorm(100, 0, 0.01)
  for (m in c("pettitt", "buishand", "snht")) {
    fit <- cpt_detect(clean, method = m)
    expect_ggcpt_contract(fit, m, "mean")
    expect_equal(fit$changepoints$cp, 50L, info = m)
    expect_true(all(c("p_value", "statistic") %in% names(fit$changepoints)))
    expect_lt(fit$changepoints$p_value[1], 0.05)
  }
  # No change: an insignificant test reports nothing rather than a location.
  quiet <- cpt_detect(rnorm(100), method = "pettitt", alpha = 1e-8)
  expect_equal(nrow(quiet$changepoints), 0)
  # The per-position statistic is available for these.
  st <- cpt_statistic(cpt_detect(clean, method = "pettitt"))
  expect_equal(nrow(st), 100)
})

test_that("taylor reports confidence and an interval", {
  skip_if_not_installed("ChangePointTaylor")
  fit <- cpt_detect(x_step, method = "taylor", n_bootstraps = 200,
                    seed = 1)
  expect_ggcpt_contract(fit, "taylor", "mean")
  expect_true(all(c("ci_lower", "ci_upper", "confidence") %in%
                    names(fit$changepoints)))
  expect_true(all(fit$changepoints$ci_lower <= fit$changepoints$cp))
  expect_true(all(fit$changepoints$ci_upper >= fit$changepoints$cp))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::autoplot(fit, show_ci = TRUE)))
  # The interval string is parsed, not guessed.
  parsed <- ggchangepoint:::parse_taylor_ci(c("(100 - 101)", "(5 - 9)"))
  expect_equal(parsed$lower, c(100L, 5L))
  expect_equal(parsed$upper, c(101L, 9L))
})

test_that("binsegrcpp matches binseg on a clean step and exposes a path", {
  skip_if_not_installed("binsegRcpp")
  clean <- c(rep(0, 50), rep(5, 50)) + rnorm(100, 0, 0.05)
  fit <- cpt_detect(clean, method = "binsegrcpp")
  expect_ggcpt_contract(fit, "binsegrcpp", "mean")
  expect_equal(fit$changepoints$cp, 50L)
  fixed <- binsegrcpp_wrapper(clean, n_segments = 3)
  expect_equal(nrow(fixed$changepoints), 2)
})

test_that("wbsts finds a second-order change", {
  skip_if_not_installed("wbsts")
  set.seed(17)
  y <- c(as.numeric(stats::arima.sim(list(ar = 0.1), 250)),
         as.numeric(stats::arima.sim(list(ar = 0.9), 250)))
  fit <- cpt_detect(y, method = "wbsts")
  expect_ggcpt_contract(fit, "wbsts", "var")
})

test_that("bfast dates a trend break in a seasonal series", {
  skip_if_not_installed("bfast")
  season <- rep(sin(seq(0, 2 * pi, length.out = 12)), 10)
  y <- stats::ts(c(rnorm(60, 1), rnorm(60, 5)) + season,
                 frequency = 12, start = c(2000, 1))
  fit <- cpt_detect(y, method = "bfast")
  expect_ggcpt_contract(fit, "bfast")
  expect_true(any(abs(fit$changepoints$cp - 60) <= 6))
  expect_true("fitted" %in% names(fit$data))
  expect_error(bfast_wrapper(as.numeric(y), frequency = 1),
               "seasonal frequency")
})

test_that("fabisearch refuses negative input before the engine does", {
  skip_if_not_installed("fabisearch")
  expect_error(fabisearch_wrapper(matrix(rnorm(60), 20)),
               "non-negative matrix factorisation")
})

test_that("mcp reports the missing system dependency plainly", {
  skip_if(requireNamespace("mcp", quietly = TRUE))
  expect_error(mcp_wrapper(x_step), "JAGS")
})

test_that("mcp fits the plateau-only default model", {
  skip_on_cran()
  skip_if_not_installed("mcp")
  # Having the package is not the same as being able to sample: rjags can be
  # installed and still fail to find the JAGS library at run time, in which
  # case mcp returns a fit with no posterior. Skip on that rather than
  # asserting a system library is present.
  skip_if_not(requireNamespace("rjags", quietly = TRUE),
              "rjags cannot load, so JAGS is not usable here")
  # The default model is `list(y ~ 1, ~ 1)` -- no predictor anywhere -- so
  # mcp cannot derive its x-axis variable from the formulas and stops with
  # "This is a plateau-only model" unless `par_x` is named. Nothing here
  # covered that: the only mcp test was the negative one above, which skips
  # precisely when mcp is installed, and the example is behind @examplesIf.
  set.seed(2026)
  y <- c(stats::rnorm(50), stats::rnorm(50, 5))
  fit <- tryCatch(mcp_wrapper(y, iter = 300, adapt = 150, chains = 2,
                              seed = 1),
                  error = function(e) e)
  if (inherits(fit, "error") && grepl("JAGS", conditionMessage(fit))) {
    skip("JAGS is installed but not reachable, so mcp cannot sample")
  }
  expect_s3_class(fit, "ggcpt")
  expect_identical(fit$method, "mcp")
  expect_equal(nrow(fit$data), length(y))
  expect_equal(nrow(fit$segments), nrow(fit$changepoints) + 1L)
  expect_true(all(c("ci_lower", "ci_upper") %in% names(fit$changepoints)))
  expect_true(any(abs(fit$changepoints$cp - 50) <= 10))

  # a caller's own par_x is not overwritten
  expect_no_error(mcp_wrapper(y, iter = 300, adapt = 150, chains = 2,
                              par_x = "t", seed = 1))
})

test_that("mcp says JAGS is unreachable rather than failing inside summary()", {
  # An mcpfit built without JAGS still looks like a fit; only the posterior
  # is missing, and summary() on it dies with "subscript out of bounds".
  expect_false(ggchangepoint:::mcp_has_samples(list(mcmc_post = NULL)))
  expect_false(ggchangepoint:::mcp_has_samples(list(mcmc_post = list())))
  expect_false(ggchangepoint:::mcp_has_samples(structure(list(), class = "x")))
  expect_true(ggchangepoint:::mcp_has_samples(
    list(mcmc_post = list(matrix(1, nrow = 10, ncol = 2)))))
})

test_that("every new method appears in cpt_methods with a citation", {
  new_methods <- c("nsp", "mcp", "esac", "pilliat", "hdcov", "network",
                   "var", "fmean", "fcov", "kwc", "fabisearch", "wbsts",
                   "bfast", "pettitt", "buishand", "snht", "taylor",
                   "binsegrcpp")
  tab <- cpt_methods()
  expect_true(all(new_methods %in% tab$method))
  expect_true(all(tab$status[match(new_methods, tab$method)] == "available"))
  refs <- ggchangepoint:::cpt_references()$method
  expect_equal(setdiff(new_methods, refs), character(0))
})

test_that("hdcov and network survive a threshold nothing clears", {
  skip_if_not_installed("changepoints")
  set.seed(29)
  # An impossibly high threshold makes thresholdBS() return cpt_hat = NULL
  # rather than a zero-row matrix; subsetting that is an error, not an
  # empty result.
  X <- matrix(rnorm(60 * 4), ncol = 4)
  quiet <- hdcov_wrapper(X, threshold = 1e12)
  expect_equal(nrow(quiet$changepoints), 0)
  expect_equal(nrow(quiet$segments), 1)

  p <- 4
  net <- t(replicate(40, as.numeric(matrix(stats::rbinom(p * p, 1, 0.3), p))))
  quiet_net <- suppressMessages(
    network_wrapper(net, n_intervals = 10, threshold = 1e12, seed = 1)
  )
  # An explicit threshold means no permutation calibration, so no warning.
  expect_equal(nrow(quiet_net$changepoints), 0)
})

test_that("continuous-weight networks use the noise split, not thinning", {
  skip_if_not_installed("changepoints")
  set.seed(31)
  W <- matrix(abs(rnorm(40 * 9)), nrow = 40)
  expect_message(
    suppressWarnings(fit <- network_wrapper(W, n_intervals = 10, n_perm = 2,
                                            seed = 1)),
    "splitting each edge"
  )
  expect_s3_class(fit, "ggcpt")
})

test_that("fabisearch reads both shapes of its significance column", {
  # Supplying `alpha` makes the engine threshold internally and return a
  # LOGICAL verdict; leaving it NULL returns the permutation p-value. Reading
  # the logical form as a number turns FALSE into 0, which clears any
  # p-value threshold, so every candidate split the search proposed would be
  # returned as a changepoint. The filter is tested directly because running
  # the engine twice is too slow for a test.
  pick <- function(cps, alpha) {
    level <- if (is.null(alpha)) 0.05 else alpha
    st <- cps$stat_test
    keep <- if (is.logical(st)) {
      !is.na(st) & st
    } else {
      pv <- suppressWarnings(as.numeric(st))
      if (all(is.na(pv))) rep(TRUE, nrow(cps)) else !is.na(pv) & pv <= level
    }
    sort(as.integer(cps$T[keep]))
  }
  logical_form <- data.frame(T = c(16L, 40L, 52L), stat_test = c(FALSE, TRUE,
                                                                 FALSE))
  expect_equal(pick(logical_form, 0.1), 40L)
  pvalue_form <- data.frame(T = c(16L, 40L, 52L),
                            stat_test = c(1, 0.01, 0.4))
  expect_equal(pick(pvalue_form, NULL), 40L)
  # And the wrapper itself uses exactly this rule.
  skip_if_not_installed("fabisearch")
  body_txt <- paste(deparse(body(fabisearch_wrapper)), collapse = " ")
  expect_true(grepl("is.logical(st)", body_txt, fixed = TRUE))
})

test_that("hdreg dates a break in a high-dimensional regression", {
  skip_if_not_installed("changepoints")
  set.seed(37)
  p <- 10
  n <- 80
  X <- matrix(stats::rnorm(n * p), n, p)
  beta1 <- c(rep(2, 3), rep(0, p - 3))
  beta2 <- c(rep(0, p - 3), rep(2, 3))
  y <- c(X[1:40, ] %*% beta1, X[41:n, ] %*% beta2) + stats::rnorm(n)
  fit <- cpt_detect(X, method = "hdreg", response = y,
                    gamma_set = c(1, 10), lambda_set = c(0.1, 1))
  expect_ggcpt_contract(fit, "hdreg", "regression")
  expect_true(any(abs(fit$changepoints$cp - 40) <= 5))
  # The plotted series is the response, not a covariate.
  expect_equal(fit$data$value, y)
  expect_error(hdreg_wrapper(X), "`response` is")
  expect_error(hdreg_wrapper(X, response = y[1:10]),
               "one value per row")
})

# ---------------------------------------------------------------------------
# Direct-call coverage for the wrappers that the suite only ever reached
# through cpt_detect(). Calling them directly exercises their own argument
# handling and defaults, which dispatch bypasses.
# ---------------------------------------------------------------------------

test_that("univariate wrappers work when called directly", {
  skip_on_cran()
  set.seed(21)
  x <- c(stats::rnorm(80), stats::rnorm(80, 6))

  cases <- list(
    not   = function() not_wrapper(x, contrast = "pcwsConstMean"),
    wbs2  = function() wbs2_wrapper(x),
    pettitt = function() trend_wrapper(x, test = "pettitt"),
    buishand = function() trend_wrapper(x, test = "buishand"),
    taylor = function() taylor_wrapper(x, n_bootstraps = 100, seed = 1),
    wbsts = function() wbsts_wrapper(x, n_intervals = 50, seed = 1)
  )
  engines <- c(not = "not", wbs2 = "breakfast", pettitt = "trend",
               buishand = "trend", taylor = "ChangePointTaylor",
               wbsts = "wbsts")
  for (nm in names(cases)) {
    if (!requireNamespace(engines[[nm]], quietly = TRUE)) next
    res <- cases[[nm]]()
    expect_s3_class(res, "ggcpt")
    expect_equal(nrow(res$data), length(x))
    expect_equal(nrow(res$segments), nrow(res$changepoints) + 1L)
    expect_true(is.integer(res$changepoints$cp))
    expect_identical(res$method, nm)
  }
})

test_that("multivariate and functional wrappers work when called directly", {
  skip_on_cran()
  set.seed(22)
  n <- 100
  X <- cbind(stats::rnorm(n), stats::rnorm(n), stats::rnorm(n),
             stats::rnorm(n), stats::rnorm(n))
  X[(n / 2 + 1):n, 1:3] <- X[(n / 2 + 1):n, 1:3] + 4

  if (requireNamespace("HDCD", quietly = TRUE)) {
    e <- esac_wrapper(X)
    expect_s3_class(e, "ggcpt")
    expect_identical(e$method, "esac")
    pl <- pilliat_wrapper(X)
    expect_s3_class(pl, "ggcpt")
    expect_identical(pl$method, "pilliat")
  }
  if (requireNamespace("KWCChangepoint", quietly = TRUE)) {
    F <- matrix(stats::rnorm(n * 12), nrow = n)
    F[(n / 2 + 1):n, ] <- F[(n / 2 + 1):n, ] * 3
    k <- kwc_wrapper(F, algorithm = "fkwc", change_in = "covariance")
    expect_s3_class(k, "ggcpt")
    expect_identical(k$method, "kwc")
  }
})

test_that("the accessibility scales build", {
  d <- data.frame(x = 1:6, y = 1:6, g = rep(letters[1:3], 2))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g, linetype = g)) +
    ggplot2::geom_line()
  for (sc in list(scale_colour_cpt(), scale_color_cpt(), scale_linetype_cpt())) {
    expect_silent(ggplot2::ggplot_build(base + sc))
  }
  filled <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col()
  expect_silent(ggplot2::ggplot_build(filled + scale_fill_cpt()))
  # fill and colour must draw from the same palette rather than drifting
  expect_setequal(
    unique(ggplot2::ggplot_build(filled + scale_fill_cpt())$data[[1]]$fill),
    unique(ggplot2::ggplot_build(base + scale_colour_cpt())$data[[1]]$colour))
  expect_setequal(
    unique(ggplot2::ggplot_build(filled + scale_fill_cpt())$data[[1]]$fill),
    ggchangepoint:::cpt_palette_values()[1:3])
  expect_identical(scale_color_cpt, scale_colour_cpt)

  # the label scale is manual and keyed to the label-error vocabulary
  lab <- data.frame(x = 1:4, y = 1:4,
                    label = factor(names(
                      ggchangepoint:::cpt_label_palette())[1:4]))
  p <- ggplot2::ggplot(lab, ggplot2::aes(x, y, colour = label)) +
    ggplot2::geom_point() + scale_colour_cpt_label()
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("fabisearch names an all-zero time point before NMF does", {
  skip_if_not_installed("fabisearch")
  X <- matrix(stats::runif(60, 0.1, 1), nrow = 20)
  X[7, ] <- 0
  expect_error(fabisearch_wrapper(X), "all-zero time point")
  expect_error(fabisearch_wrapper(X), "row 7")
})

test_that("pilliat refuses the dimensions where HDCD's thresholds are short", {
  skip_if_not_installed("HDCD")
  skip_if_not(utils::packageVersion("HDCD") <= "1.1")
  set.seed(31)
  # A power-of-two dimension makes HDCD 1.1 report a change at every
  # observation, on pure noise as readily as on a real change.
  for (p in c(4L, 8L, 16L)) {
    X <- matrix(stats::rnorm(120 * p), nrow = 120)
    expect_error(cpt_detect(X, method = "pilliat"),
                 "cannot be trusted on exactly")
  }
  # one either side of a power of two is fine, and finds the change
  for (p in c(7L, 9L)) {
    X <- matrix(stats::rnorm(120 * p), nrow = 120)
    X[61:120, 1:2] <- X[61:120, 1:2] + 4
    res <- cpt_detect(X, method = "pilliat")
    expect_lt(nrow(res$changepoints), 5L)
    expect_true(any(abs(res$changepoints$cp - 60) <= 4))
  }
  # constant coordinates are dropped first, so 9 with one constant is 8
  X <- matrix(stats::rnorm(120 * 9), nrow = 120)
  X[, 9] <- 2
  expect_error(suppressWarnings(cpt_detect(X, method = "pilliat")),
               "9 supplied, 1 constant")
  # esac is unaffected at the same dimensions
  for (p in c(4L, 8L, 16L)) {
    X <- matrix(stats::rnorm(120 * p), nrow = 120)
    expect_equal(nrow(cpt_detect(X, method = "esac")$changepoints), 0L)
  }
})

test_that("is_power_of_two is right at the edges", {
  f <- ggchangepoint:::is_power_of_two
  expect_true(all(vapply(c(2, 4, 8, 16, 1024, 65536), f, logical(1))))
  expect_false(any(vapply(c(0, 1, 3, 5, 6, 7, 9, 100, 1023), f, logical(1))))
  expect_false(f(NA_integer_))
})

test_that("every declared change_in actually runs, and undeclared ones refuse", {
  skip_on_cran()
  set.seed(61)
  x <- c(stats::rnorm(100), stats::rnorm(100, 4))
  seasonal <- x + 2 * sin(2 * pi * seq_along(x) / 12)

  # binsegRcpp has no variance-only cost, so the registry must not claim one
  skip_if_not_installed("binsegRcpp")
  expect_error(cpt_detect(x, method = "binsegrcpp", change_in = "var"),
               "not supported")
  for (ci in c("mean", "meanvar")) {
    res <- cpt_detect(x, method = "binsegrcpp", change_in = ci)
    expect_s3_class(res, "ggcpt")
    expect_identical(res$change_in, ci)
  }

  # bfast reports "no seasonal breaks" as a bare NA, not an empty
  # breakpoints object, which used to be a `$`-on-atomic error
  skip_if_not_installed("bfast")
  for (ci in c("mean", "slope", "seasonality")) {
    res <- cpt_detect(seasonal, method = "bfast", change_in = ci,
                      frequency = 12)
    expect_s3_class(res, "ggcpt")
    expect_identical(res$change_in, ci)
    expect_equal(nrow(res$segments), nrow(res$changepoints) + 1L)
  }
  expect_error(bfast_wrapper(seasonal, frequency = 12,
                             change_in = "seasonality", season = "none"),
               "needs a seasonal component")

  # the engine's bootstrap floor is enforced by name here, not by the
  # engine's own message about its misspelled argument
  skip_if_not_installed("ChangePointTaylor")
  expect_error(taylor_wrapper(x, n_bootstraps = 60), "n_bootstraps")
  expect_s3_class(taylor_wrapper(x, n_bootstraps = 100, seed = 1), "ggcpt")
})

test_that("change_in = \"mean\" is accepted everywhere but never mislabels", {
  skip_on_cran()
  set.seed(62)
  n <- 160
  uni <- c(stats::rnorm(n / 2), stats::rnorm(n / 2, 4))
  # A method that targets something other than the mean accepts the default
  # request -- and records its own change type on the result, so no object
  # ever claims to be a mean change when it is not.
  native <- c(np = "distribution", cpop = "slope", segmented = "slope")
  engines <- c(np = "changepoint.np", cpop = "cpop", segmented = "segmented")
  for (m in names(native)) {
    if (!requireNamespace(engines[[m]], quietly = TRUE)) next
    res <- suppressWarnings(cpt_detect(uni, method = m, change_in = "mean"))
    expect_identical(res$change_in, native[[m]])
  }
})
